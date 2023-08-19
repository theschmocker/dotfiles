;;; web-mode-language-triggers.el --- Trigger functions when point enters language in web mode -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Jacob Schmocker
;;
;; Author: Jacob Schmocker
;; Maintainer: Jacob Schmocker
;; Homepage: https://github.com/theschmocker
;; Created: Augest 17, 2023
;; Version: 0.1.0
;; Keywords: tools convenience
;; Package-Requires: ((emacs "28"))

;;; Commentary:
;; WIP
;;
;; Setup: (add-hook 'web-mode-hook 'web-mode-language-triggers-mode)
;;
;;; Code:

(require 'cl-lib)

;;; Core

(defvar web-mode-language-triggers-functions nil)

(defvar-local web-mode-language-triggers--language-at-pos nil)

(autoload 'web-mode-language-at-pos "web-mode")
(defun web-mode-language-triggers--set-language ()
  (when (derived-mode-p 'web-mode)
    (let ((lang (web-mode-language-at-pos)))
      (when (not (string= lang web-mode-language-triggers--language-at-pos))
        (setq web-mode-language-triggers--language-at-pos lang)
        (dolist (fn web-mode-language-triggers-functions)
          (funcall fn lang))))))

;;;###autoload
(define-minor-mode web-mode-language-triggers-mode
  ""
  :global nil
  (when (not (derived-mode-p 'web-mode))
    (error "%s does nothing outside of web-mode or a derived mode." 'web-mode-language-triggers-mode))
  (if web-mode-language-triggers-mode
      (progn
        (web-mode-language-triggers--set-language)
        (add-hook 'post-command-hook #'web-mode-language-triggers--set-language nil t))
    (setq web-mode-language-triggers--language-at-pos nil)
    (remove-hook 'post-command-hook #'web-mode-language-triggers--set-language t)))

;;; yasnippet

(defvar web-mode-language-triggers-yas-extra-mode-alist
  '(("javascript" js-mode)
    ("typescript" typescript-mode)
    ("css" css-mode)))

(autoload 'yas-activate-extra-mode "yasnippet")
(autoload 'yas-deactivate-extra-mode "yasnippet")

(defun web-mode-language-triggers-setup-yas-extra-langs (current-lang)
  (when (and (featurep 'yasnippet) (bound-and-true-p yas-minor-mode))
    (cl-loop for (lang . modes) in web-mode-language-triggers-yas-extra-mode-alist
             do (dolist (mode modes)
                  (if (string= lang current-lang)
                      (yas-activate-extra-mode mode)
                    (yas-deactivate-extra-mode mode))))))

(with-eval-after-load 'yasnippet
  (add-to-list 'web-mode-language-triggers-functions #'web-mode-language-triggers-setup-yas-extra-langs))

(provide 'web-mode-language-triggers)

;;; web-mode-language-triggers.el ends here
