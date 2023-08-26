;;; treesit-additional-font-lock-rules.el --- Apply additional font-lock rules to ts modes -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Jacob
;;
;; Author: Jacob <>
;; Maintainer: Jacob <>
;; Created: August 25, 2023
;; Modified: August 25, 2023
;; Version: 0.0.1
;; Keywords: languages
;; Homepage: https://github.com/theschmocker/dotfiles
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Apply additional font-lock rules to ts modes
;;
;;; Code:

(require 'treesit)

(defvar treesit-additional-font-lock-rules-alist nil)
(defvar-local treesit-additional-font-lock-rules--original-rules nil)

(defun treesit-additional-font-lock-rules-apply ()
  (when-let ((additional-rules (cdr (assoc major-mode treesit-additional-font-lock-rules-alist))))
    (let ((base-rules
           (if treesit-additional-font-lock-rules--original-rules
               treesit-additional-font-lock-rules--original-rules
             (setq treesit-additional-font-lock-rules--original-rules treesit-font-lock-settings)
             treesit-font-lock-settings)))
      (setq treesit-font-lock-settings (append base-rules
                                               (apply #'treesit-font-lock-rules additional-rules))))
    (treesit-font-lock-recompute-features)
    (font-lock-update)))

(defun treesit-additional-font-lock-rules--applier-fn-name (sym)
  (intern (format "treesit-additional-font-lock-rules--apply-%s-rules"
                                (symbol-name sym))))

(defun treesit-additional-font-lock-rules--define-rule-applier (mode)
  (let ((fn-sym (treesit-additional-font-lock-rules--applier-fn-name mode)))
    (eval
     `(if (fboundp ',fn-sym)
          (function ,fn-sym)
        (defun ,fn-sym ()
          (let ((major-mode ',mode))
            (treesit-additional-font-lock-rules-apply)))))))

;;;###autoload
(defun treesit-additional-font-lock-rules (mode &rest rules)
  "Apply additional tree-sitter font-lock rules in MODE.

MODE should be a tree-sitter mode (e.g. `typescript-ts-mode', `csharp-ts-mode')
RULES are the same as the arguments to `treesit-font-lock-rules'."
  (declare (indent defun))
  (unless (symbolp mode)
    (error "MODE should be a mode symbol"))
  (setf (alist-get mode treesit-additional-font-lock-rules-alist nil 'remove) rules)

  (let ((rule-applier (treesit-additional-font-lock-rules--define-rule-applier mode)))

    (let ((hook (intern (concat (symbol-name mode) "-hook"))))
      (add-hook hook rule-applier))
    ;; Update existing buffers
    (treesit-additional-font-lock-rules--reapply-in-mode-buffers mode)))

(defun treesit-additional-font-lock-rules--reapply-in-mode-buffers (mode)
  (let ((applier (treesit-additional-font-lock-rules--define-rule-applier mode)))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (derived-mode-p mode)
          (funcall applier))))))

(provide 'treesit-additional-font-lock-rules)
;;; treesit-additional-font-lock-rules.el ends here
