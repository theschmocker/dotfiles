;;; ../Documents/dotfiles/emacs/.doom.d/lisp/prlctl.el -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'map)

(defun prlctl--run-command (&rest args)
  (with-temp-buffer
    (apply #'call-process (append '("prlctl" nil t nil) args))
    (buffer-string)))

(defun prlctl-list ()
  (interactive)
  (let ((buffer (get-buffer-create "*prlctl list*" t)))
    (with-current-buffer buffer
      (setq-local require-final-newline nil)
      (read-only-mode -1)
      (fundamental-mode)
      (erase-buffer)
      (save-excursion
        (insert (prlctl--run-command "list" "--all" "--full" "--json"))
        (prlctl-mode)))
    (pop-to-buffer buffer)))

(defun prlctl--virtual-machines ()
  (json-read-from-string (prlctl--run-command "list" "--all" "--full" "--json")))

(defun prlctl-get-virtual-machine (name-or-uuid)
  (cl-find-if (lambda (vm)
                (or (string-equal (map-elt vm 'uuid) name-or-uuid)
                    (string-equal (map-elt vm 'name) name-or-uuid)))
              (prlctl--virtual-machines)))

(defun prlctl--virtual-machine-completing-read (&optional predicate)
  (let ((options (mapcar (lambda (vm)
                           (cons (concat (map-elt vm 'name)
                                         " ("
                                         (map-elt vm 'uuid)
                                         ")")
                                 vm))
                         (prlctl--virtual-machines))))
    (map-elt options
             (completing-read "Choose a VM: "
                              options
                              (lambda (option)
                                (let ((vm (cdr option)))
                                  (if predicate
                                      (funcall predicate vm)
                                    t)))
                              t))))

(defun prlctl--vm-info-string (vm)
  (prlctl--run-command "list" "-i" (map-elt vm 'uuid)))

(defun prlctl--vm-info-alist (vm)
  (let* ((cmd-output (prlctl--vm-info-string vm))
         (lines (thread-last cmd-output
                             (string-trim)
                             (string-lines)
                             cdr))
         (chunked (mapcar (lambda (line)
                            (split-string line ":" t))
                          lines)))
    (let ((child-item-p (lambda (item)
                          (string-prefix-p " " (car item)))))
      (named-let iter ((results '())
                       (items chunked))
        (if (null items)
            (reverse results)
          (let ((current (car items))
                (children (seq-take-while child-item-p
                                          (cdr items))))
            (iter (cons (if (null children)
                            (cons (car current) (string-trim (apply #'concat (cdr current))))
                          (cons (car current)
                                (mapcar (lambda (child)
                                          (cl-destructuring-bind (name &rest val) child
                                            (cons (string-trim name) (string-trim (apply #'concat val)))))
                                        children)))
                        results)
                  (seq-drop-while child-item-p
                                  (cdr items)))))))))

(defun prlctl-windows-vm-p (vm)
  (string-prefix-p "win" (map-elt (prlctl--vm-info-alist vm) "OS")))

(defun prlctl-exec (&optional vm-or-name-or-uuid cmd)
  (interactive)
  (let ((vm (cond ((null vm-or-name-or-uuid) (prlctl--virtual-machine-completing-read
                                              (lambda (vm)
                                                (string-equal "running"
                                                              (map-elt vm 'status)))))
                  ((listp vm-or-name-or-uuid) vm-or-name-or-uuid)
                  ((stringp vm-or-name-or-uuid ) (cl-find-if (lambda (vm)
                                                               (and (string-equal "running" (map-elt vm 'status))
                                                                    (or (string-equal (map-elt vm 'uuid) vm-or-name-or-uuid)
                                                                        (string-equal (map-elt vm 'name) vm-or-name-or-uuid))))
                                                             (prlctl--virtual-machines)))
                  (t (user-error "Wrong type of VM-OR-NAME-OR-UUID"))))
        (cmd (or cmd (read-string "Enter a command: "))))
    (if (not vm)
        (user-error "VM \"%s\" either not running or does not exist" vm-or-name-or-uuid)
      (if (prlctl-windows-vm-p vm)
          (prlctl-exec--run-windows vm cmd)
        (prlctl-exec--run vm cmd)))))

(defun prlctl-exec--run-windows (vm cmd)
  (compile (format "prlctl exec %s %s cmd /S /C \"%s\""
                   (map-elt vm 'uuid)
                   (if prlctl-run-with-current-user
                       "--current-user"
                     "")
                   (string-replace "\"" "\\\"" cmd))
           t))

(defun prlctl-exec--run (vm cmd)
  (compile (format "prlctl exec %s %s %s"
                   (map-elt vm 'uuid)
                   (if prlctl-run-with-current-user
                       "--current-user"
                     "")
                   cmd)
           t))

(defun prlctl-mode--table-format (vms)
  (let ((column-name-alist '((name . "Name")
                             (status . "Status")
                             (uuid . "UUID")
                             (ip_configured . "IP Address"))))
    (apply #'vector (mapcar (lambda (col)
                              (let ((key (car col))
                                    (name (cdr col)))
                                (let ((longest-value (apply #'max (mapcar (lambda (item)
                                                                            (length
                                                                             (map-elt item key)))
                                                                          vms))))
                                  (list name (+ 4 longest-value) t))))
                            column-name-alist))))

(defun prlctl-mode--table-entries (vms)
  (mapcar (lambda (vm)
            (let ((uuid (map-elt vm 'uuid)))
              (list uuid (vector
                          (map-elt vm 'name)
                          (map-elt vm 'status)
                          uuid
                          (map-elt vm 'ip_configured)))))
          vms))

(define-derived-mode prlctl-mode tabulated-list-mode "Parallels Control"
  (let ((data (json-read-from-string (buffer-substring-no-properties (point-min) (point-max)))))
    (setq tabulated-list-format (prlctl-mode--table-format data))
    (setq tabulated-list-entries (prlctl-mode--table-entries data))
    (tabulated-list-init-header)
    (tabulated-list-print t)))

(defun prlctl-mode--vm-uuid-at-point ()
  (get-text-property (point) 'tabulated-list-id))

(defun prlctl-mode--exec ()
  (interactive)
  (prlctl-exec (prlctl-mode--vm-uuid-at-point)))

(defun prlctl-mode--enter ()
  (interactive)
  (let* ((vm (prlctl-get-virtual-machine (prlctl-mode--vm-uuid-at-point)))
         (uuid (map-elt vm 'uuid))
         (name (map-elt vm 'name))
         (flags `("enter" ,uuid ,@(if prlctl-run-with-current-user
                                      (list "--current-user")
                                    nil))))
    (pop-to-buffer
     (apply #'make-comint (append `(,(format "prlctl shell (%s)" name)
                                    "prlctl"
                                    nil)
                                  flags)))))

(defun prlctl-mode--statistics ()
  (interactive)
  (compile (concat "prlctl statistics " (prlctl-mode--vm-uuid-at-point))))

(defun prlctl-mode--info ()
  (interactive)
  (let* ((vm (prlctl-get-virtual-machine (prlctl-mode--vm-uuid-at-point)))
         (uuid (map-elt vm 'uuid))
         (buffer (get-buffer-create (format "*prlctl info (%s)*" uuid) t)))
    (with-current-buffer buffer
      (setq-local require-final-newline nil)
      (read-only-mode -1)
      (fundamental-mode)
      (erase-buffer)
      (save-excursion
        (insert (prlctl--run-command "list" "--info" uuid))
        ;; TODO prlctl-related name for vm uuids instead of using tabulated-list-mode default
        (set-text-properties (point-min) (point-max) `(tabulated-list-id ,uuid))
        (delete-trailing-whitespace))
      (prlctl-info-mode))
    (pop-to-buffer buffer prlctl-info-display-buffer-func)))

(defvar prlctl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "e" #'prlctl-mode--exec)
    (define-key map "E" #'prlctl-mode--enter)
    (define-key map "i" #'prlctl-mode--info)
    map)
  "Local keymap for `prlctl-mode' buffers.")

(defvar prlctl-info-display-buffer-func #'display-buffer-same-window
  "")

(define-derived-mode prlctl-info-mode special-mode "Parallels VM Info"
  "")

(defvar prlctl-run-with-current-user t
  "Whether commands sent or shells started in virtual machines should be run
  under the currrent VM user.")

(provide 'prlctl)
;;; prlctl.el ends here

;;; code I was tinkering with that might come in handy when adding functionality to `prlctl-mode'
;; (define-derived-mode echo-line-mode special-mode "Echo Line"
;;   :syntax-table nil)

;; (defun echo-line-mode-print-line (line index)
;;   (let ((beg (point)))
;;     (insert line)
;;     (insert ?\n)
;;     (add-text-properties beg (point)
;;                          `(echo-line-index ,index
;;                            echo-line-content ,line))))

;; (defun echo-line-buffer-from-lines (lines)
;;   (let ((buf (generate-new-buffer "*echo-line*")))
;;     (with-current-buffer buf
;;       (mapc (lambda (item)
;;               (cl-destructuring-bind (index . line) item
;;                 (echo-line-mode-print-line line index)))
;;             (-zip (-iota (length lines))
;;                   lines))
;;       (echo-line-mode)
;;       buf)))


;; (defun echo-line-do-echo ()
;;   (interactive)
;;   (let ((index (get-text-property (point) 'echo-line-index))
;;         (line (get-text-property (point) 'echo-line-content)))
;;     (message (concat (number-to-string index) ": " line))))

;; (map! :map 'echo-line-mode-map
;;       :n "RET" #'echo-line-do-echo)
