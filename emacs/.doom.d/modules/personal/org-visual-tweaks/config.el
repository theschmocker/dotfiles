;;; personal/org-visual-tweaks/config.el -*- lexical-binding: t; -*-

(defvar-local +org-visual-tweaks/face-cookies nil)
(defvar-local +org-visual-tweaks/re-enable-superstar-mode-p nil)

(setq org-superstar-headline-bullets-list '(9673))

(defun +org-visual-tweaks/map-faces ()
  (dolist (face '(org-code
                  org-block
                  org-table
                  org-property-value
                  org-formula
                  org-tag
                  org-verbatim
                  org-date
                  company-tooltip
                  org-special-keyword
                  org-block-begin-line
                  org-block-end-line
                  org-meta-line
                  org-document-info-keyword
                  org-indent
                  org-todo))
    (push (face-remap-add-relative face :inherit 'fixed-pitch)
          +org-visual-tweaks/face-cookies))
  (dolist (face '(org-level-8
                  org-level-7
                  org-level-6
                  org-level-5
                  org-level-4
                  org-level-3
                  org-level-2
                  org-level-1))
    (push (face-remap-add-relative face :inherit 'default :height 1.5 :weight 'bold)
          +org-visual-tweaks/face-cookies)))

(defun +org-visual-tweaks/reset-faces ()
  (mapc #'face-remap-remove-relative +org-visual-tweaks/face-cookies)
  (setq +org-visual-tweaks/face-cookies nil))

(defun +org-visual-tweaks/refresh-faces (&rest _)
  (if +org-visual-tweaks-mode
      (progn
        (+org-visual-tweaks/reset-faces)
        (+org-visual-tweaks/map-faces))
    (+org-visual-tweaks/reset-faces))
  (font-lock-update))

(define-minor-mode +org-visual-tweaks-mode
  "Minor mode that applies some visual tweaks to org."
  :lighter nil
  (when (not (equal major-mode #'org-mode))
    (user-error "+org-visual-tweaks-mode is only available in org-mode"))
  (+org-visual-tweaks/refresh-faces)
  (if +org-visual-tweaks-mode
      (progn
        (require 'org-appear)
        (setq +org-visual-tweaks/re-enable-superstar-mode-p org-superstar-mode)
        ;; (+org-visual-tweaks/map-faces)
        (setq-local org-hide-emphasis-markers t)
        (org-appear-mode 1)
        (setq-local org-appear-trigger 'manual)
        (add-hook! 'evil-insert-state-entry-hook :local #'org-appear-manual-start)
        (add-hook! 'evil-insert-state-exit-hook :local #'org-appear-manual-stop)
        (variable-pitch-mode 1)
        (font-lock-update))
    (progn
      (setq-local org-hide-emphasis-markers nil)
      (remove-hook! 'evil-insert-state-entry-hook :local #'org-appear-manual-start)
      (remove-hook! 'evil-insert-state-exit-hook :local #'org-appear-manual-stop)
      (org-appear-mode -1)
      (setq-local org-appear-trigger 'always)
      ;; (+org-visual-tweaks/reset-faces)
      (variable-pitch-mode -1)
      (font-lock-update))))

(defadvice! +org-visual-tweaks/refresh-faces-in-all-buffers (&rest _)
  :after '(enable-theme)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when +org-visual-tweaks-mode
        (+org-visual-tweaks/refresh-faces)))))
