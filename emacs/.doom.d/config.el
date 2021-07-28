;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Jacob"
      user-mail-address "")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(setq doom-font (font-spec :family "MonoLisa" :size 12))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'spacemacs-light)
(setq doom-theme 'doom-gruvbox)
(setq doom-gruvbox-dark-variant "soft")
;; (setq doom-gruvbox-light-variant "hard")
;; (load-theme 'monokai-pro-spectrum t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Editor
(setq scroll-margin 8)

;; Normal mode mappings
(map! (:n "gh" 'lsp-glance-or-lookup))

(use-package! lisp-mode
  :init
  (map! (:map lisp-mode-map
         :n "gh" 'sly-describe-symbol)))

(use-package! sly
  :init
  (map! (:map sly-mrepl-mode-map
         :n "gh" 'sly-describe-symbol)))

(defun lsp-glance-or-lookup (identifier &optional arg)
  "If lsp-mode is enabled, then call lsp-ui-doc-glance. Fall back to +lookup/documentation"
  (interactive (list (doom-thing-at-point-or-region)
                     current-prefix-arg))
  (if (bound-and-true-p lsp-mode)
      (lsp-ui-doc-glance)
    (+lookup/documentation identifier arg)))

;; LSP
(after! lsp-mode
  (setq lsp-ui-doc-position 'top)
  (setq lsp-ui-doc-max-width 200)
  (add-to-list 'lsp-language-id-configuration '(".*\\.twig$" . "html")))

;; Vue
(setq lsp-vetur-experimental-template-interpolation-service t)

;; TS
(setenv "TSSERVER_LOG_FILE" "/tmp/tsserver.log") ;; prevents ts server from polluting project dir with log files

;; Which key
(setq which-key-idle-delay 0.25)
(setq doom-leader-alt-key "M-RET")

;; Completion
;; (add-to-list 'completion-styles 'flex) ;; fuzzy completion
;; (setq completion-styles (remove 'flex completion-styles))


;; Org
;; (set-company-backend! 'org-mode
;;   '(:separate company-capf company-yasnippet company-dabbrev))
