;;; $DOOMDIR/modules/personal/tempel/config.el -*- lexical-binding: t; -*-

(use-package tempel
  ;; Require trigger prefix before template name when completing.
  :custom
  (tempel-trigger-prefix ">")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))

  :init
  (setq tempel-path (concat doom-user-dir "templates/templates.eld"))


  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions)))
  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)


  (defun +tempel/expand ()
    "handles expand call in normal mode"
    (interactive)
    (when (evil-normal-state-p)
      (goto-char (1+ (point)))
      (call-interactively #'evil-insert))
    (call-interactively #'tempel-expand))

  (defun +tempel/one-of-modes (modes)
    (cl-some #'derived-mode-p modes))

  (defun +tempel/web-mode-typescript-p ()
    (and (derived-mode-p 'web-mode)
         (let ((lang (web-mode-language-at-pos)))
           (cl-some (apply-partially #'string= lang)
                    '("typescript" "tsx")))))

  (defun +tempel/web-mode-javascript-p ()
    (and (derived-mode-p 'web-mode)
         (let ((lang (web-mode-language-at-pos)))
           (cl-some (apply-partially #'string= lang)
                    '("javascript" "jsx")))))

  (defun +tempel/javascript-p ()
    (or (+tempel/web-mode-javascript-p)
        (+tempel/one-of-modes '(js-mode
                                js-ts-mode
                                js2-mode
                                rjsx-mode))))

  (defun +tempel/typescript-p ()
    (or (+tempel/web-mode-typescript-p)
        (+tempel/one-of-modes '(typescript-ts-base-mode
                                typescript-mode
                                typescript-tsx-mode))))

  (defun +tempel/javascript-or-typescript-p ()
    (or (+tempel/javascript-p)
        (+tempel/typescript-p)))

  (add-hook! 'emmet-mode-hook
    (map! :map emmet-mode-keymap
          "<tab>" nil
          "C-h ," #'emmet-expand-line))

  (add-hook! 'evil-insert-state-exit-hook
             (when (bound-and-true-p tempel--active)
               (tempel-done)))

  (defadvice! +temple/insert-mode-after-tempel-insert (&rest _)
    :after #'tempel-insert
    (when (evil-normal-state-p)
      (call-interactively #'evil-insert)))

  (map!
   (:ni "C-<tab>" #'+tempel/expand)
   (:v "C-<tab>" #'tempel-insert)
   (:map tempel-map
         "TAB" #'tempel-next
         "<backtab>" #'tempel-previous))


  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )
