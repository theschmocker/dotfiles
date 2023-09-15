;;; personal/vue/config.el -*- lexical-binding: t; -*-

(use-package! vue-ts-mode
  :defer t
  :commands (vue-ts-mode)
  :mode "\\.vue\\'"
  :config

  (map!
   :mode vue-ts-mode
   (:localleader
    :desc "Go to matching element tag" "m" #'vue-ts-mode-element-match
    (:prefix ("e" . "element")
     :desc "Go to next element" "n" #'vue-ts-mode-element-next
     :desc "Go to previous element" "p" #'vue-ts-mode-element-previous)))

  (when (featurep! :tools lsp)
    (setq-hook! 'vue-ts-mode-hook lsp-enable-imenu nil) ;; use vue-ts-mode's imenu index instead
    (add-hook! 'vue-ts-mode-local-vars-hook :append #'lsp!))

  (when (featurep 'emmet-mode)
    (add-hook! 'vue-ts-mode-hook #'emmet-mode))

  (add-to-list 'vue-ts-mode-language-at-point-functions #'+vue/setup-yas-extra-langs)

  (add-hook! 'vue-ts-mode-hook
    ;; TODO: put this back into vue-ts-mode
    (modify-syntax-entry ?= "." vue-ts-mode-syntax-table)
    (+vue/setup-yas-extra-langs (treesit-language-at (point))))

  (advice-add 'vue-ts-mode-element-match :around #'doom-set-jump-maybe-a)
  (advice-add 'vue-ts-mode-element-previous :around #'doom-set-jump-maybe-a)
  (advice-add 'vue-ts-mode-element-next :around #'doom-set-jump-maybe-a))

(defun +vue/setup-yas-extra-langs (current-lang)
  (when (and (featurep 'yasnippet) (bound-and-true-p yas-minor-mode))
    (cl-loop for (lang . modes) in '(("javascript" js-mode)
                                     ("typescript" typescript-mode)
                                     ("typescript" vue-script-mode)
                                     ("css" css-mode))
             do (dolist (mode modes)
                  (if (string= lang current-lang)
                      (yas-activate-extra-mode mode)
                    (yas-deactivate-extra-mode mode))))))
