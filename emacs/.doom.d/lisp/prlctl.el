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
      (insert (prlctl--run-command "list" "-a"))
      (prlctl-mode))
    (pop-to-buffer buffer)))

(defun prlctl--virtual-machines ()
  (map-elt
   (prlctl-mode--parse-table (prlctl--run-command "list" "-a"))
   :entries))

(defun prlctl-get-virtual-machine (name-or-uuid)
  (cl-find-if (lambda (vm)
                (or (string-equal (map-elt vm :uuid) name-or-uuid)
                    (string-equal (map-elt vm :name) name-or-uuid)))
              (prlctl--virtual-machines)))

(defun prlctl--virtual-machine-completing-read (&optional predicate)
  (let ((options (mapcar (lambda (vm)
                           (cons (concat (map-elt vm :name)
                                         " ("
                                         (map-elt vm :uuid)
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
  (prlctl--run-command "list" "-i" (map-elt vm :uuid)))

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

(defun prlctl-exec (&optional vm-or-name-or-uuid cmd)
  (interactive)
  (let ((vm (cond ((null vm-or-name-or-uuid) (prlctl--virtual-machine-completing-read
                                              (lambda (vm)
                                                (string-equal "running"
                                                              (map-elt vm :status)))))
                  ((listp vm-or-name-or-uuid) vm-or-name-or-uuid)
                  ((stringp vm-or-name-or-uuid ) (cl-find-if (lambda (vm)
                                                               (and (string-equal "running" (map-elt vm :status))
                                                                    (or (string-equal (map-elt vm :uuid) vm-or-name-or-uuid)
                                                                        (string-equal (map-elt vm :name) vm-or-name-or-uuid))))
                                                             (prlctl--virtual-machines)))
                  (t (user-error "Wrong type of VM-OR-NAME-OR-UUID"))))
        (cmd (or cmd (read-string "Enter a command: "))))
    (if (not vm)
        (user-error "VM \"%s\" either not running or does not exist" vm-or-name-or-uuid)
      (if (string-prefix-p "win" (map-elt (prlctl--vm-info-alist vm) "OS"))
          (compile (format "prlctl exec %s cmd /S /C \"%s\"" (map-elt vm :uuid) (string-replace "\"" "\\\"" cmd)) t)
        (compile (format "prlctl exec %s %s" (map-elt vm :uuid) cmd) t)))))

(defun prlctl-mode--parse-table-format (header-line longest-line-length)
  (let* ((uuid-start 0)
         (uuid-end (string-match "STATUS" header-line))
         (status-start uuid-end)
         (status-end (string-match "IP_ADDR" header-line))
         (ip-start status-end)
         (ip-end (string-match "NAME" header-line))
         (name-start ip-end))
    (vector (list (string-trim (substring header-line name-start)) (+ 4 (- longest-line-length name-start)) t)
            (list (string-trim (substring header-line status-start status-end)) (- status-end status-start) t)
            (list (string-trim (substring header-line uuid-start uuid-end)) uuid-end nil)
            (list (string-trim (substring header-line ip-start ip-end)) -1 t))))

(defun prlctl-mode--parse-table (input)
  (let* ((lines (string-lines (string-trim input)))
         (header-line (car lines))
         (entry-lines (cdr lines)))
    (let* ((uuid-start 0)
           (uuid-end (string-match "STATUS" header-line))
           (status-start uuid-end)
           (status-end (string-match "IP_ADDR" header-line))
           (ip-start status-end)
           (ip-end (string-match "NAME" header-line))
           (name-start ip-end)
           (entries (mapcar (lambda (line)
                              (let ((uuid (thread-last (substring line uuid-start uuid-end)
                                                       (string-trim)
                                                       (string-replace "{" "")
                                                       (string-replace "}" "")))
                                    (status (string-trim (substring line status-start status-end)))
                                    (ip (string-trim (substring line ip-start ip-end)))
                                    (name (string-trim (substring line name-start))))
                                (list :name name
                                      :status status
                                      :uuid uuid
                                      :ip ip)))
                            entry-lines)))
      (list :table-format (prlctl-mode--parse-table-format header-line (apply #'max (mapcar #'length entry-lines)))
            :entries entries))))

(define-derived-mode prlctl-mode tabulated-list-mode "Parallels Control"
  (let ((data (prlctl-mode--parse-table (buffer-string))))
    (setq tabulated-list-format (map-elt data :table-format))
    (setq tabulated-list-entries (mapcar (lambda (entry)
                                           (let ((uuid (map-elt entry :uuid)))
                                             (list uuid (apply #'vector (map-values entry)))))
                                         (map-elt data :entries)))
    (tabulated-list-init-header)
    (tabulated-list-print t)))

(defun prlctl-mode--vm-uuid-at-point ()
  (get-text-property (point) 'tabulated-list-id))

(defun prlctl-mode--exec ()
  (interactive)
  (prlctl-exec (prlctl-mode--vm-uuid-at-point)))

(defun prlctl-mode--statistics ()
  (interactive)
  (compile (concat "prlctl statistics " (prlctl-mode--vm-uuid-at-point))))

(defun prlctl-mode--info ()
  (interactive)
  (let* ((vm (prlctl-get-virtual-machine (prlctl-mode--vm-uuid-at-point)))
         (uuid (map-elt vm :uuid))
         (buffer (get-buffer-create (format "*prlctl info (%s)*" uuid) t)))
    (with-current-buffer buffer
      (setq-local require-final-newline nil)
      (read-only-mode -1)
      (fundamental-mode)
      (erase-buffer)
      (insert (prlctl--run-command "list" "--info" uuid))
      ;; TODO prlctl-related name for vm uuids instead of using tabulated-list-mode default
      (set-text-properties (point-min) (point-max) `(tabulated-list-id ,uuid))
      (delete-trailing-whitespace)
      (goto-line 1)
      (prlctl-info-mode))
    (pop-to-buffer buffer prlctl-info-display-buffer-func)))

(defvar prlctl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "e" #'prlctl-mode--exec)
    (define-key map "i" #'prlctl-mode--info)
    map)
  "Local keymap for `prlctl-mode' buffers.")

(defvar prlctl-info-display-buffer-func #'display-buffer-same-window
  "")

(define-derived-mode prlctl-info-mode special-mode "Parallels VM Info"
  "")

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
