;;; personal/eshell/autoloads.el -*- lexical-binding: t; -*-

;;;###autoload
(defun schmo/eshell-toggle (arg &optional command)
  "Toggle eshell popup window.

Copy-pasta of `+eshell/toggle' which doesn't kill the buffer when turning the
popup off."
  (interactive "P")
  (let ((eshell-buffer
         (get-buffer-create
          (format "*doom:eshell-popup:%s*"
                  (if (bound-and-true-p persp-mode)
                      (safe-persp-name (get-current-persp))
                    "main"))))
        confirm-kill-processes
        current-prefix-arg)
    (when arg
      (when-let (win (get-buffer-window eshell-buffer))
        (delete-window win))
      (when (buffer-live-p eshell-buffer)
        (with-current-buffer eshell-buffer
          (fundamental-mode)
          (erase-buffer))))
    (if-let (win (get-buffer-window eshell-buffer))
        (let (confirm-kill-processes)
          (delete-window win))
      (with-current-buffer eshell-buffer
        (doom-mark-buffer-as-real-h)
        (if (eq major-mode 'eshell-mode)
            (run-hooks 'eshell-mode-hook)
          (eshell-mode))
        (when command
          (+eshell-run-command command eshell-buffer)))
      (pop-to-buffer eshell-buffer))))

;;;###autoload
(defun schmo/eshell-here (&optional command)
  "Open eshell in the current window.

Calls `+eshell/here' without reusing an existing eshell buffer."
  (interactive)
  (let ((orig-+eshell--unused-buffer (symbol-function '+eshell--unused-buffer)))
    (cl-letf (((symbol-function '+eshell--unused-buffer)
               (lambda (_)
                 (funcall orig-+eshell--unused-buffer 'new))))
      (+eshell/here command))))
