;;; personal/my-go/autoload.el -*- lexical-binding: t; -*-


;;;###autoload
(defvar schmo/last-go-cover-file-by-project (make-hash-table)
  "Path to the most-recently generated coverage profile file")

(projectile-current-project-files)

;;;###autoload
(defmacro run-once-after-hook (hook function)
  "Add `function' to `hook' and remove it after it's run"
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
  "Returns a list of absolute paths to every .go file in the current project"
  (let* ((root (projectile-acquire-root))
         (files (projectile-project-files root)))
    (thread-last files
                 (seq-map (apply-partially #'concat root))
                 (seq-filter (apply-partially #'string-match-p ".go$")))))

;;;###autoload
(defun schmo/go-mtime (file-path)
  "Returns the mtime of the file at `file-path' as a float timestamp"
  (thread-first file-path
                file-attributes
                file-attribute-modification-time
                float-time))

;;;###autoload
(defun schmo/go-coverage (buffer coverage-file same-window)
  "Runs `go-coverage' using `coverage-file' and sets up the state necessary
   for `go-coverage-minor-mode'

   `buffer' is the buffer of the file `go-coverage' should apply to

   `coverage-file' is the location of file containing the coverage profile
   output by go test

   `same-window' should be nil if a new window was open to display coverage"
  (dlet ((go-coverage-display-buffer-func #'display-buffer-same-window))
    (with-current-buffer buffer
      (go-coverage coverage-file))
    (let ((cov-buffer-name (concat (buffer-name buffer) "<gocov>")))
      (with-current-buffer (get-buffer cov-buffer-name)
        (setq schmo/go-coverage-opened-in-same-window same-window)
        (go-coverage-minor-mode 1)))))

;;;###autoload
(defun schmo/go-cover-tests (arg)
  "Runs a go project's tests and displays the test coverage of the file in the
   current buffer. If `arg' is non-nil, prompts selection of another go file
   and opens it in a new window (if it's not the file in the current buffer)"
  (interactive "P")
  (let ((same-window t))
    (when (not (null arg))
      (setq same-window (not (schmo/consult-non-test-go-file-in-project))))
    (let* ((buf (current-buffer))
           (last-go-cover-file (gethash (projectile-project-root) schmo/last-go-cover-file-by-project))
           (rerun-tests (or (null last-go-cover-file)
                            (let ((cov-mtime (schmo/go-mtime last-go-cover-file)))
                              (thread-last (schmo/get-all-go-files-in-project)
                                           (seq-map #'schmo/go-mtime)
                                           (seq-some (apply-partially #'< cov-mtime))))))
           (coverage-file (if rerun-tests
                              (concat (file-name-as-directory (temporary-file-directory))
                                      (make-temp-name "cover-")
                                      ".out")
                            last-go-cover-file)))
      (if rerun-tests
          (progn
            (run-once-after-hook 'compilation-finish-functions
                                 (lambda (&rest _)
                                   (schmo/go-coverage buf coverage-file same-window)
                                   (puthash (projectile-project-root) coverage-file schmo/last-go-cover-file-by-project)))
            (save-selected-window
              (compile (concat "go test -test.v -coverprofile " coverage-file))))
        (schmo/go-coverage buf coverage-file same-window)))))

(defun schmo/go-coverage-buffer-list ()
  "Returns a list of go-coverage buffers"
  (seq-filter (lambda (buf)
                (string-match-p "<gocov>" (buffer-name buf)))
              (buffer-list)))

;;;###autoload
(defun schmo/go-kill-coverage-buffers ()
  "Kills all open go-coverage buffers"
  (interactive)
  (seq-do #'kill-buffer (schmo/go-coverage-buffer-list)))

;;;###autoload
(defun schmo/consult-non-test-go-file-in-project ()
  "Prompts for a non-test go file and opens it. Returns `t' if the selected
   file is different than the file in the current buffer."
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

(defvar-local schmo/go-coverage-opened-in-same-window nil
  "`t' when the current coverage buffer was opened in the same window as the
   file it targets, `nil' otherwise")

(define-minor-mode go-coverage-minor-mode
  "Minor mode for go-coverage buffers"
  :interactive nil
  :keymap (list (cons (kbd "q") (lambda ()
                                  (interactive)
                                  (go-coverage-minor-mode 0))))
  (if go-coverage-minor-mode
      (setq-local buffer-read-only t)
    (progn
      (let ((should-kill-window (and (null (cdr (window-prev-buffers)))
                                     (not schmo/go-coverage-opened-in-same-window))))
        (if should-kill-window
            (kill-buffer-and-window)
          (let ((buf (current-buffer))
                (prev-buf (caar (cdr (window-prev-buffers)))))
            (when (not schmo/go-coverage-opened-in-same-window)
              (switch-to-buffer prev-buf))
            (kill-buffer buf))))
      (setq schmo/go-coverage-opened-in-same-window nil))))
