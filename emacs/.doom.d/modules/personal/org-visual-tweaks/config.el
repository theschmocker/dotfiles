;;; personal/org-visual-tweaks/config.el -*- lexical-binding: t; -*-

(defvar-local +org-visual-tweaks/face-cookies nil)

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
                  org-document-info-keyword))
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

(define-minor-mode +org-visual-tweaks-mode
  ""
  :lighter nil
  (when (not (equal major-mode #'org-mode))
    (user-error "+org-visual-tweaks-mode is only available in org-mode"))
  (if +org-visual-tweaks-mode
      (progn
        (variable-pitch-mode 1)
        (require 'org-appear)
        (setq-local org-appear-trigger 'manual)
        (org-appear-mode 1)
        (+org-visual-tweaks/map-faces)
        (add-hook! 'evil-insert-state-entry-hook :local #'org-appear-manual-start)
        (add-hook! 'evil-insert-state-exit-hook :local #'org-appear-manual-stop)
        (setq-local org-hide-emphasis-markers t))
    (progn
      (setq-local org-hide-emphasis-markers nil)
      (remove-hook! 'evil-insert-state-entry-hook :local #'org-appear-manual-start)
      (remove-hook! 'evil-insert-state-exit-hook :local #'org-appear-manual-stop)
      (+org-visual-tweaks/reset-faces)
      (variable-pitch-mode -1)
      (org-appear-mode -1))))
