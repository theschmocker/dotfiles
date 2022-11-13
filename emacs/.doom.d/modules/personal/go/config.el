;;; personal/my-go/config.el -*- lexical-binding: t; -*-

(after! go-mode
  (map! :map go-mode-map
        :localleader
        (:prefix ("c" . "coverage")
                  "c" #'schmo/go-cover-tests
                  "k" #'schmo/go-kill-coverage-buffers)))

(add-hook 'go-coverage-minor-mode-hook (lambda (&rest _)
                                         (evil-make-overriding-map go-coverage-minor-mode-map 'normal t)
                                         (evil-normalize-keymaps)))
