;;; ../dotfiles/emacs/.doom.d/snippets/emacs-lisp-mode/.yas-setup.el -*- lexical-binding: t; -*-

(defun schmo/snippet/guess-package-name ()
  (save-excursion
    (goto-char (point-min))
    (cond ((re-search-forward "(provide '\\(.*?\\))" nil t)
           (let ((name (match-string 1)))
             (set-text-properties 0 (length name) nil name)
             name))
          ((buffer-file-name) (file-name-base (buffer-file-name)))
          (t "package"))))
