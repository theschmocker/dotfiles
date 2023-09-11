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

  (advice-add 'vue-ts-mode-element-match :around #'doom-set-jump-maybe-a)
  (advice-add 'vue-ts-mode-element-previous :around #'doom-set-jump-maybe-a)
  (advice-add 'vue-ts-mode-element-next :around #'doom-set-jump-maybe-a))
