;;; personal/treesit-helpers/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun schmo/insert-with-current-buffer-in-scratch (buf)
  (interactive "b")
  (doom/open-scratch-buffer)
  (goto-char (point-max))
  (newline 2)
  (insert (format "(with-current-buffer \"%s\")" buf))
  (backward-char)
  (newline-and-indent))

(defvar schmo/ts-overlays nil)

;;;###autoload
(defun schmo/ts-highlight-clear-overlays ()
  (interactive)
  (dolist (o schmo/ts-overlays)
    (delete-overlay o)))

;;;###autoload
(defun schmo/ts-highlight-query-result (results)
  (schmo/ts-highlight-clear-overlays)
  (dolist (match results)
    (let ((face 'highlight)
          start
          end)
      (if (symbolp (car match))
          (progn
            (cond ((facep (car match))
                   (setq face (car match)))
                  ((string-prefix-p "_" (symbol-name (car match))) ;; ignore captures starting with underscore
                   (setq face nil)))
            (setq start (treesit-node-start (cdr match)))
            (setq end (treesit-node-end (cdr match))))
        (setq start (car match))
        (setq end (cdr match)))
      (let ((o (make-overlay start end)))
        (overlay-put o 'face face)
        (push o schmo/ts-overlays)))))

;;;###autoload
(defun schmo/create-ts-query-in-scratch-buffer ()
  (interactive)
  (call-interactively #'schmo/insert-with-current-buffer-in-scratch)
  (let ((start (point)))
    (insert (string-trim-right (pp
                                '(schmo/ts-highlight-query-result
                                  (treesit-query-capture
                                   (treesit-buffer-root-node)
                                   )))))
    (indent-region start (point)))
  (backward-char 2)
  (newline-and-indent))
