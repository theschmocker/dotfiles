;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

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

(defmacro doom-font! (&rest specs)
  "Sets `doom-font' to the first available font in `SPECS'"
  (let* ((specs (mapcar (lambda (spec-form)
                          (if (and (listp spec-form)
                                   (not (eql 'font-spec (car spec-form))))
                              (cons #'font-spec spec-form)
                            spec-form))
                        specs))
         (spec (cl-find-if (lambda (spec-form)
                             (doom-font-exists-p (eval spec-form)))
                           specs)))
    `(setq doom-font ,spec)))

(doom-font!
 (:family "MonoLisa Nerd Font" :size 13)
 (:family "MonoLisa" :size 13)
 (:family "Dank Mono" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'spacemacs-light)
(setq doom-theme 'doom-rose-pine-moon)
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

;;; Editor
(setq scroll-margin 8)

;;; Normal mode mappings
(map! (:n "gh" 'schmo/lsp-glance-or-lookup))
(defun schmo/lsp-glance-or-lookup ()
  "If lsp-mode is enabled, then show LSP documentation. Fall back to +lookup/documentation"
  (interactive)
  (if (bound-and-true-p lsp-mode)
      ;; if hover doc is already visible, then open and focus a help buffer. similar to how
      ;; vim.lsp.buf.hover neovim will focus the hover popup when called twice
      (if (lsp-ui-doc--frame-visible-p)
          (lsp-describe-thing-at-point)
        (lsp-ui-doc-glance))
    (call-interactively #'+lookup/documentation)))

;; disable evil-snipe
(remove-hook 'doom-first-input-hook #'evil-snipe-mode)
(remove-hook 'doom-first-input-hook #'evil-snipe-override-mode) ; fixes stuff like df<Space>

;; Remap s and S to avy-goto-char-2
(map! (:n "s" 'avy-goto-char-2))
(map! (:n "S" (lambda ()
                (interactive)
                (let ((avy-all-windows 't))
                  (call-interactively #'avy-goto-char-2)))))

(map! :leader
      (:prefix "o"
       (:desc "Open link at point" "l" #'link-hint-open-link-at-point))
      (:prefix "c"
       :desc "Jump to references" "f" #'+lookup/references
       :desc "Format buffer/region" "F" #'+format/region-or-buffer
       "D" nil)
      (:prefix "s"
       (:desc "Fuzzy search buffer" "f" 'schmo/swiper-fuzzy)
       (:desc "Locate file" "F" 'locate))
      (:prefix "w"
       (:desc "ace-window" "w" 'ace-window))
      (:prefix "g"
       ("B" nil)
       ("b" 'magit-blame-addition)
       (:prefix "c"
        ("o" 'magit-branch-checkout)))
      (:prefix "b"
       ("b" 'switch-to-buffer)
       ("B" nil)
       ("w" '+ivy/switch-workspace-buffer)))

(map! :after company
      :map company-active-map
      ("C-y" #'company-complete-selection)
      ("TAB" #'company-complete-selection)
      ("<tab>" #'company-complete-selection)
      ("RET" nil)
      ("<return>" nil))

(map!
 :map sly-mrepl-mode-map
 :n "gh" 'sly-describe-symbol)

(map!
 :map lisp-mode-map
 :n "gh" 'sly-describe-symbol
 (:localleader
  (:prefix "e"
   :desc "Evaluate defun" "d" 'sly-eval-defun)))

(defun schmo/swiper-fuzzy ()
  (interactive)
  (let ((ivy-re-builders-alist (cons `(swiper . ,#'ivy--regex-fuzzy) ivy-re-builders-alist)))
    (call-interactively 'swiper)))

;;; Cleverparens
(setq evil-cleverparens-use-s-and-S nil)
(add-hook! '(emacs-lisp-mode-hook
             lisp-mode-hook
             clojure-mode-hook
             scheme-mode-hook)
           #'evil-cleverparens-mode)

;;; LSP
(after! lsp-mode
  (add-to-list 'lsp-language-id-configuration '(".*\\.twig$" . "html")))

(after! lsp-ui
  (setq lsp-ui-doc-max-width 100)
  (setq lsp-ui-doc-max-height 13)
  (setq lsp-ui-doc-delay 0)
  (setq lsp-signature-render-documentation nil))

;;; TS
(setenv "TSSERVER_LOG_FILE" "/tmp/tsserver.log") ;; prevents ts server from polluting project dir with log files

;;; Which key
(setq which-key-idle-delay 0.25)
(setq doom-leader-alt-key "M-RET")

;;; Completion
;; fuzzy completion
(add-to-list 'completion-styles 'flex)

(setq completion-ignore-case t)

;; Current company selection will get put into to the buffer
(add-hook 'after-init-hook 'company-tng-mode)

(setq company-idle-delay 0.3)
(setq company-selection-wrap-around t)

;;; Web Mode
(after! web-mode
  (setq web-mode-part-padding 0)
  (setq web-mode-script-padding 0)
  (setq web-mode-style-padding 0)
  ;; doom modifies web-mode's autopairs to avoid conflict with smart parens,
  ;; but it doesn't remove whitespace from the closing portion. In Vue (for example),
  ;; this leads to this being inserted: {{ |  }}, when this is desired: {{ | }}
  ;; just disabling web-modes auto-pairing altogether for now; smartparens covers
  ;; most of my needs
  (setq web-mode-enable-auto-pairing nil))

(setq emmet-indent-after-insert nil)
;; Something about emmet-expand-yas breaks undo... just use emmet-expand-line instead
(advice-add 'emmet-expand-yas :override 'emmet-expand-line)

;;; Misc
(defmacro modify-syntax! (mode char newentry)
  (let* ((m-name (concat (symbol-name mode) "-mode"))
         (hook (intern (concat m-name "-hook")))
         (syntax-table (intern (concat m-name "-syntax-table"))))
    `(add-hook ',hook (lambda ()
                       (modify-syntax-entry ,char ,newentry ,syntax-table)))))

;; Treat symbols-with-hypens as whole words
(modify-syntax! emacs-lisp ?- "w")
(modify-syntax! lisp ?- "w")
(modify-syntax! clojure ?- "w")
(modify-syntax! scheme ?- "w")

;; Maximize window on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; Popups
;; I have a habit of pressing escape too many times... meaning I keep losing popup buffers.
;; This should make "q" the primary way to close some of them
(set-popup-rules!
  '(("^\\*info" :quit nil :modeline t :size 0.35)
    ("^\\*helpful" :quit nil :modeline t :size 0.35)))

(plist-put +popup-defaults :modeline t)

;;; org
(add-to-list 'org-modules 'ol-info)

;;; org-roam
(setq org-roam-directory (file-truename "~/notes"))
(org-roam-db-autosync-mode)
