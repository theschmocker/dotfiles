;;; personal/my-go/autoload.el -*- lexical-binding: t; -*-


;;;###autoload
(defvar schmo/last-go-cover-file nil)

;;;###autoload
(defmacro run-once-after-hook (hook function)
  (let ((defun-sym (gensym))
        (args-sym (gensym))
        (function-sym (gensym)))
    `(let ((,function-sym ,function))
       (defun ,defun-sym (&rest ,args-sym)
         (remove-hook ,hook ',defun-sym)
         (fmakunbound ',defun-sym)
         (apply (append (list 'funcall ,function-sym) ,args-sym)))
       (add-hook ,hook ',defun-sym))))

;;;###autoload
(defun schmo/get-all-go-files-in-project ()
  (let* ((root (projectile-acquire-root))
         (files (projectile-project-files root)))
    (thread-last files
                 (seq-map (apply-partially #'concat root))
                 (seq-filter (apply-partially #'string-match-p ".go$")))))

;;;###autoload
(defun schmo/go-mtime (file-name)
  (thread-first file-name
                file-attributes
                file-attribute-modification-time
                float-time))

;;;###autoload
(defun schmo/go-coverage (buffer coverage-file same-window)
  (dlet ((go-coverage-display-buffer-func (lambda (&rest args)
                                            (message "what")
                                            (apply #'display-buffer-same-window args))))
    (with-current-buffer buffer
      (go-coverage coverage-file))
    (let ((cov-buffer-name (concat (buffer-name buffer) "<gocov>")))
      (with-current-buffer (get-buffer cov-buffer-name)
        (setq schmo/go-coverage-opened-in-same-window same-window)
        (go-coverage-minor-mode 1)
        (read-only-mode 1)))))

;;;###autoload
(defun schmo/go-cover-tests (arg)
  (interactive "P")
  (let ((same-window t))
    (when (not (null arg))
      (setq same-window (not (schmo/consult-non-test-go-file-in-project))))
    (let* ((buf (current-buffer))
           (rerun-tests (or (null schmo/last-go-cover-file)
                            (let ((cov-mtime (schmo/go-mtime schmo/last-go-cover-file)))
                              (thread-last (schmo/get-all-go-files-in-project)
                                           (seq-map #'schmo/go-mtime)
                                           (seq-some (apply-partially #'< cov-mtime))))))
           (coverage-file (if rerun-tests
                              (concat (file-name-as-directory (temporary-file-directory))
                                      (make-temp-name "cover-")
                                      ".out")
                            schmo/last-go-cover-file)))
      (if rerun-tests
          (progn
            (run-once-after-hook 'compilation-finish-functions
                                 (lambda (&rest _)
                                   (schmo/go-coverage buf coverage-file same-window)
                                   (setq schmo/last-go-cover-file coverage-file)))
            (save-selected-window
              (compile (concat "go test -test.v -coverprofile " coverage-file))))
        (schmo/go-coverage buf coverage-file same-window)))))

(defun schmo/go-coverage-buffer-list ()
  (seq-filter (lambda (buf)
                (string-match-p "<gocov>" (buffer-name buf)))
              (buffer-list)))

;;;###autoload
(defun schmo/go-kill-coverage-buffers ()
  (interactive)
  (seq-do #'kill-buffer (schmo/go-coverage-buffer-list)))

;;;###autoload
(defun schmo/consult-non-test-go-file-in-project ()
  (interactive)
  (let ((selected-file
         (consult--read
          (or (mapcar #'abbreviate-file-name (seq-filter (lambda (file-name)
                                                           (not (string-match-p "_test.go$" file-name)))
                                                         (schmo/get-all-go-files-in-project)))
              (user-error "No .go files in current project"))
          :prompt "Find go file: "
          :sort nil
          :require-match t
          :category 'file
          :state (consult--file-preview)
          :history 'file-name-history)))
    (if (string-equal (buffer-file-name) (file-truename selected-file))
        (progn
          (find-file selected-file)
          nil)
      (progn
       (find-file-other-window selected-file)
       t))))

(defvar-local schmo/go-coverage-opened-in-same-window nil)

(define-minor-mode go-coverage-minor-mode
  "Minor mode for go-coverage buffers"
  :interactive nil
  :keymap (list (cons (kbd "q") (lambda ()
                                  (interactive)
                                  (go-coverage-minor-mode 0)
                                  (let ((should-kill-window (and (null (cdr (window-prev-buffers)))
                                                                 (not schmo/go-coverage-opened-in-same-window))))
                                    (if should-kill-window
                                        (kill-buffer-and-window)
                                      (let ((buf (current-buffer))
                                            (prev-buf (caar (cdr (window-prev-buffers)))))
                                        (when (not schmo/go-coverage-opened-in-same-window)
                                          (switch-to-buffer prev-buf))
                                        (kill-buffer buf))))
                                  (setq schmo/go-coverage-opened-in-same-window nil)))))

