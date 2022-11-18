;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(require 's)

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
(setq hscroll-margin 16)

;;; Normal mode mappings
;; disable evil-snipe
(remove-hook 'doom-first-input-hook #'evil-snipe-mode)
(remove-hook 'doom-first-input-hook #'evil-snipe-override-mode) ; fixes stuff like df<Space>

;; Remap s and S to avy-goto-char-2
(map! (:n "s" 'avy-goto-char-2))
(map! (:n "S" (lambda ()
                (interactive)
                (dlet ((avy-all-windows 't))
                  (call-interactively #'avy-goto-char-2)))))

(map! :leader
      ;; Swapped from defaults
      :desc "M-x" ";" 'execute-extended-command
      :desc "Eval expression" ":" 'pp-eval-expression
      :desc "Search History" "\"" (lambda ()
                                    (interactive)
                                    (vertico-repeat t))

      (:prefix "o"
               (:desc "Open link at point" "l" #'link-hint-open-link-at-point))

      (:prefix "c"
       :desc "Jump to references" "f" #'+lookup/references
       :desc "Format buffer/region" "F" #'+format/region-or-buffer
       "D" nil)

      (:prefix "w"
               ("w" 'ace-window)
               ("-" #'schmo/window-resize/body "Interactive Resize")

               ;; swap default o bindings
               ("o" 'delete-other-windows)
               ("C-o" 'doom/window-enlargen)

               ;; swap default split bindings
               ("v" '+evil/window-vsplit-and-follow)
               ("V" 'evil-window-vsplit)
               ("s" '+evil/window-split-and-follow)
               ("S" 'evil-window-split))

      (:prefix "g"
               ("B" nil)
               (:desc "Blame" "b" 'magit-blame-addition)
               (:prefix ("c" . "create/checkout")
                        ("o" 'magit-branch-checkout)))

      (:prefix "b"
               ("b" 'switch-to-buffer)
               ("B" nil)
               ("w" '+vertico/switch-workspace-buffer)))

(map! :after company
      :map company-active-map
      ("C-y" #'company-complete-selection)
      ("TAB" nil)
      ("<tab>" nil)
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

(map!
 :map emacs-lisp-mode-map
 :localleader
 (:prefix "e"
          ("p" 'eval-print-last-sexp)))

;;; Cleverparens
(setq evil-cleverparens-use-s-and-S nil)
(add-hook! '(emacs-lisp-mode-hook
             lisp-mode-hook
             clojure-mode-hook
             scheme-mode-hook)
           #'evil-cleverparens-mode)

;;; LSP
(after! lsp-mode
  (add-to-list 'lsp-language-id-configuration '(".*\\.twig$" . "html"))
  (setq lsp-eldoc-enable-hover nil))

(after! lsp-ui
  (setq lsp-ui-doc-max-width 100
        lsp-ui-doc-max-height 13
        lsp-ui-doc-delay 0
        lsp-signature-render-documentation nil
        lsp-ui-doc-include-signature t))

;; Not sure why lsp-mode doesn't respect this variable
(setq lsp-eslint-auto-fix-on-save t)
(advice-add 'lsp--before-save :before (lambda ()
                                        (when lsp-eslint-auto-fix-on-save
                                          (lsp-eslint-fix-all))))

;; Copied from lsp-mode docs to fix some workspace folder weirdness
(advice-add 'lsp :before (lambda (&rest _args) (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))

(defun schmo/lsp-volar-vue-project-p (workspace-root)
  "Check if the 'vue' package is present in the package.json file
in the WORKSPACE-ROOT. Checks dependencies and devDependencies."
  (if-let ((package-json (f-join workspace-root "package.json"))
           (exist (f-file-p package-json))
           (config (json-read-file package-json)))
      (let ((dependencies (alist-get 'dependencies config))
            (dev-dependencies (alist-get 'devDependencies config)))
        (or (alist-get 'vue dependencies)
            (alist-get 'vue dev-dependencies)))
    nil))

;; I work on a project that has vue in the devDependencies... for reasons. This ensures that volar gets initialized in that workspace
(advice-add 'lsp-volar--vue-project-p :override 'schmo/lsp-volar-vue-project-p)

(map! (:n "gh" 'schmo/lsp-glance-or-lookup))
(defun schmo/lsp-glance-or-lookup ()
  "If lsp-mode is enabled, then show LSP documentation. Fall back to
+lookup/documentation"
  (interactive)
  (if (bound-and-true-p lsp-mode)
      ;; if hover doc is already visible, then open and focus a help buffer. similar to how
      ;; vim.lsp.buf.hover neovim will focus the hover popup when called twice
      (if (lsp-ui-doc--frame-visible-p)
          (lsp-describe-thing-at-point)
        (lsp-ui-doc-glance))
    (call-interactively #'+lookup/documentation)))

;;; Which key
(setq which-key-idle-delay 0.25)
(setq doom-leader-alt-key "M-RET")

;;; Completion
;; fuzzy completion
(add-to-list 'completion-styles 'flex)

(setq completion-ignore-case t)

(use-package! fussy
  :config
  (push 'fussy completion-styles)
  (setq fussy-filter-fn 'fussy-filter-flex))

(after! orderless
  (let ((styles '(fussy flex basic partial-completion emacs22)))
    (setq completion-styles styles
          +vertico-company-completion-styles styles))
  (pushnew! orderless-matching-styles 'orderless-flex))

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
(setq whitespace-global-modes nil)

(defmacro modify-syntax! (modes &rest entries)
  (declare (indent defun))
  `(progn ,@(mapcar (lambda (mode)
                      (let* ((m-name (symbol-name mode))
                             (hook (intern (concat m-name "-hook")))
                             (syntax-table (intern (concat m-name "-syntax-table"))))
                        `(add-hook ',hook (lambda ()
                                            ,@(mapcar (lambda (entry)
                                                        (cl-destructuring-bind (char newentry) entry
                                                          `(modify-syntax-entry ,char ,newentry ,syntax-table)))
                                                      entries)))))
                    (if (listp modes)
                        modes
                      (list modes)))))

(modify-syntax! (emacs-lisp-mode lisp-mode clojure-mode scheme-mode)
  ;; consider these symbols to be parts of words
  (?- "w")
  (?: "w")
  (?? "w")
  (?/ "w")
  (?+ "w"))

;; Maximize window on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; Popups
;; I have a habit of pressing escape too many times... meaning I keep losing popup buffers.
;; This should make "q" the primary way to close some of them
(set-popup-rules!
  '(("^\\*info" :quit nil :modeline t :size 0.35)
    ("^\\*helpful" :quit nil :modeline t :size 0.35)))

(plist-put +popup-defaults :modeline t)

;; CSS
(defun schmo/print-float-with-max-places (num max-places)
  "Truncates decimal places of `num' to `max-places' without trailing 0s"
  (number-to-string
   (string-to-number
    (format (concat "%0." (number-to-string max-places) "f")
            num))))

(defun schmo/insert-relative-units (val &optional arg)
  "Inserts `val' relative to some base number for use with relative units in
CSS. If `arg' is non-nil, then prompts for a base. Base defaults to 16."
  (interactive (list (read-number "Value: ")
                     current-prefix-arg))
  (let ((base (float (if arg
                         (read-number "Base: ")
                       16))))
    (when (<= base 0)
      (error "Base must be greater than 0"))
    (let ((res (schmo/print-float-with-max-places (/ val base) 4)))
      (insert (if (s-ends-with? ".0" res)
                  (s-replace ".0" "" res)
                res)))))

(map! :leader
      (:prefix "i"
               (:prefix ("n" . "number")
                        (:desc "Relative units" "r" 'schmo/insert-relative-units))))

;;; org
(add-to-list 'org-modules 'ol-info)

(defalias 'run-geiser 'geiser
  "org-babel tries to execute scheme blocks with `run-geiser', which
no longer exists")

(defun schmo/org-insert-week ()
  (interactive)
  (let ((monday-label (let ((date (calendar-current-date)))
                        (cl-destructuring-bind (month day _year)
                            (calendar-gregorian-from-absolute
                             (1+ (- (calendar-absolute-from-gregorian date)
                                    (calendar-day-of-week date))))
                          (format "%02d/%02d" month day)))))
    (newline)
    (insert (concat "* " monday-label))
    (dolist (day '("Mon" "Tue" "Wed" "Thu" "Fri"))
      (newline)
      (insert (concat "** " day))
      (newline)
      (insert "- "))))

(defhydra schmo/window-resize nil
  "Window Resize"
  ("h" #'evil-window-decrease-width "Decrease Width")
  ("j" #'evil-window-increase-height "Increase Height")
  ("k" #'evil-window-decrease-height "Decrease Height")
  ("l" #'evil-window-increase-width "Increase Width")
  ("=" #'balance-windows "Balance"))
