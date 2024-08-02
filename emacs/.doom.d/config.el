;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(require 's)
(require 'f)

;; Personal lisp packages outside of doom's module system
(require 'schmo-lib)
(require 'prlctl)
(require 'string-edit)
(require 'msbuild)
(require 'web-mode-language-triggers)
(require 'misc-tools)

(setq user-full-name "Jacob"
      user-mail-address "")

(doom-font!
 (:family "MonoLisa Nerd Font" :size 13)
 (:family "MonoLisa" :size (if IS-WINDOWS 28 13))
 (:family "Dank Mono" :size 14))

(doom-variable-pitch-font!
 (:family "ETBembo" :size 16)
 (:family "Optima" :size 16)
 (:family "Futura" :size 16)
 (:family "Helvetica" :size 16))

;; prevent janky line numbers in variable-pitch-mode
(custom-set-faces!
  '(line-number-current-line :inherit fixed-pitch)
  '(line-number :inherit fixed-pitch))

(defadvice! schmo/text-scale-mode-face-remap (fn face)
  :around #'face-remap--remap-face
  (when (eq face 'default)
    (dolist (additional-face '(fixed-pitch fixed-pitch-serif variable-pitch))
      (funcall fn additional-face)))
  (funcall fn face))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-rose-pine-moon)

(setq doom-themes-treemacs-enable-variable-pitch nil)


;;; Tree-sitter

(setq treesit-font-lock-level 4)

(setq treesit-language-source-alist
      '((css "https://github.com/tree-sitter/tree-sitter-css")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (vue "https://github.com/ikatyang/tree-sitter-vue")
        (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp")))

(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

;;; Popups
;;;
;; I have a habit of pressing escape too many times... meaning I keep losing popup buffers.
;; This should make "q" the primary way to close some of them
(set-popup-rules!
  '(("^\\*info" :quit nil :modeline t :size 0.35)
    ("^\\*helpful" :quit nil :modeline t :size 0.35 :ttl 300)))

(plist-put +popup-defaults :modeline t)


;;; Editor

(setq scroll-margin 4)
(setq hscroll-margin 16)
(setq display-line-numbers-type 'relative)

;;; Evil

;; disable evil-snipe
(remove-hook 'doom-first-input-hook #'evil-snipe-mode)
(remove-hook 'doom-first-input-hook #'evil-snipe-override-mode) ; fixes stuff like df<Space>

;; Cleverparens
(setq evil-cleverparens-use-s-and-S nil)
(add-hook! '(emacs-lisp-mode-hook
             lisp-mode-hook
             clojure-mode-hook
             scheme-mode-hook
             fennel-mode-hook)
           #'evil-cleverparens-mode)

;; movement with f and t not restricted to current line.
;; I prefer the normal behavior for other commands affected by
;; `evil-cross-lines', so this advice only overrides it for `evil-find-char' and
;; its variants
(defadvice! schmo/with-evil-cross-lines (fun &rest args)
  "Call FUN with `evil-cross-lines' bound to `t'"
  :around #'evil-find-char
  (dlet ((evil-cross-lines t))
    (apply fun args)))


;;; Normal mode / leader mappings

;; Remap s and S to avy-goto-char-2
(map! (:n "s" (lambda ()
                (interactive)
                (dlet ((avy-all-windows 't))
                  (call-interactively #'avy-goto-char-2)))))

(setq avy-keys '(?j ?k ?f ?d ?l ?h ?g ?s ?a))

(defun schmo/insert-uuid ()
  "Shell out to uuidgen and insert the result"
  (interactive)
  (let ((command (if IS-WINDOWS
                     "powershell -Command \"[guid]::NewGuid().ToString()\""
                   "uuidgen")))
    (thread-last (shell-command-to-string command)
                 (string-replace "\n" "" )
                 insert)))

(map! :leader
      ;; Swapped from defaults
      :desc "M-x" ";" 'execute-extended-command
      :desc "Eval expression" ":" (lambda (arg)
                                    (interactive (list current-prefix-arg))
                                    (if arg
                                        (eval-expression (read--expression "Eval: ") t)
                                      (call-interactively #'pp-eval-expression)))
      :desc "Search History" "\"" (lambda ()
                                    (interactive)
                                    (vertico-repeat t))

      (:prefix "o"
               (:desc "Open link at point" "l" #'link-hint-open-link-at-point))

      (:prefix "c"
       :desc "Jump to references" "f" #'+lookup/references
       :desc "Format buffer/region" "F" #'+format/region-or-buffer
       "D" nil
       "o" #'schmo/try-find-alt-type)

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
               ("w" '+vertico/switch-workspace-buffer))

      (:prefix "i"
               (:desc "UUID" "U" #'schmo/insert-uuid)
               (:prefix ("n" . "number")
                        (:desc "Relative units" "r" 'schmo/insert-relative-units)))
      (:prefix "p"
               "D" #'projectile-remove-known-project
               "d" #'project-find-dir))

(map!
 (:map ctl-x-map
       (:desc "project" "p" project-prefix-map)))

;; Version Control Mappings
(map!
 (:leader
  (:desc "vc" "v" vc-prefix-map))
 (:map vc-prefix-map
       "=" #'vc-dir
       "d" #'vc-diff)
 (:localleader
  :mode (diff-mode magit-diff-mode)
  :desc "Show side-by-side diff" "d" #'diffview-current))

;; Make maps like q and l work in diffview-mode
(add-hook 'diffview-mode-hook (lambda (&rest _)
                                (evil-make-overriding-map diffview--mode-map 'normal t)
                                (evil-normalize-keymaps)))

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

(map! :after cycle-quotes
      :n "gq" 'cycle-quotes)

;;; LSP
(after! lsp-mode
  (setq lsp-warn-no-matched-clients nil)
  (add-to-list 'lsp-language-id-configuration '(".*\\.twig$" . "html"))

  ;; On macOS, the filenotify backend (kqueue) doesn't emit a change event
  ;; when a file inside of a watched directory changes (unless some other
  ;; change happens like create or delte). This caused some issues for me
  ;; when working in Svelte projects: changes in TS files weren't reflected
  ;; in Svelte components. svelte-language-server spins up its own watcher
  ;; if the client doesn't claim the workspace.didChangeWatchedFiles capability,
  ;; (which isn't sent to the server when this var is nil) and fixes my problem.
  ;;
  ;; Note to future self: revisit this if you run into servers that don't watch
  ;; files themselves
  ;; (setq lsp-enable-file-watchers (not IS-MAC))

  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-auto-execute-action nil)
  (setq lsp-enable-indentation nil)
  (lsp-register-custom-settings '(("eslint.rules.customizations" (vector (list :rule "prettier*"
                                                                               :severity "off"))))))

;; Register plugins with lsp-ts-plugin-manager
(after! lsp-mode
  (require 'lsp-javascript)
  (require 'lsp-ts-plugin-manager)
  (lsp-ts-plugin-manager-register
   (lsp-ts-plugin-manager-plugin
    :name 'typescript-styled-plugin
    :activation-fn #'schmo/project-has-styled-components-p
    :package "typescript-styled-plugin"))

  (lsp-ts-plugin-manager-register
   (lsp-ts-plugin-manager-plugin
    :name 'typescript-svelte-plugin
    :activation-fn #'schmo/svelte-project-p
    :package "typescript-svelte-plugin"
    :dependency-of 'svelte-language-server))

  (when IS-WINDOWS
    (setq lsp-volar-take-over-mode nil)
    (lsp-ts-plugin-manager-register
     (lsp-ts-plugin-manager-plugin
      :name 'typescript-vue-plugin
      :activation-fn (lambda (&rest _)
                       (thread-last (schmo/get-project-package-json-files)
                                    (mapcar #'file-name-directory)
                                    (cl-some #'schmo/vue-project-p)))
      :package "typescript-vue-plugin"))))

(after! lsp-ui
  (setq lsp-ui-doc-max-width 100
        lsp-ui-doc-max-height 13
        lsp-ui-doc-delay 0
        lsp-signature-render-documentation nil
        lsp-ui-doc-include-signature t)
  (when IS-WINDOWS
    ;; too slow on win with C#
    (setq lsp-lens-enable nil)))

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

;; Not sure why lsp-mode doesn't respect this variable
(setq lsp-eslint-auto-fix-on-save t)
(advice-add 'lsp--before-save :before
            (lambda ()
              (when lsp-eslint-auto-fix-on-save
                (dlet ((lsp-auto-execute-action t))
                  (lsp-eslint-fix-all)))))


;; Copied from lsp-mode docs to fix some workspace folder weirdness
(advice-add 'lsp :before (lambda (&rest _args) (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))

;; I work on a project that has vue in the devDependencies... for reasons. This ensures that volar gets initialized in that workspace
(advice-add 'lsp-volar--vue-project-p :override #'schmo/vue-project-p)

(defadvice! schmo/lsp-volar--vue-project-p (orig &rest args)
  :around 'lsp-volar--activate-p
  (and (schmo/vue-project-p (lsp-workspace-root))
       (apply orig args)))

;; Workarounds for language servers (specifically volar) that include bytes in their JSON responses.
;; The libjansson version in Emacs isn't configured to handle them and it barfs
;; https://github.com/typescript-language-server/typescript-language-server/issues/559#issuecomment-1259470791
;; This might be fixed in Emacs 29
(advice-add 'json-parse-string :around
            (lambda (orig string &rest rest)
              (apply orig (s-replace "\\u0000" "" string)
                     rest)))
(advice-add 'json-parse-buffer :around
            (lambda (oldfn &rest args)
              (save-excursion
                (while (search-forward "\\u0000" nil t)
                  (replace-match "" nil t)))
              (apply oldfn args)))

(add-hook 'csharp-tree-sitter-mode-local-vars-hook #'lsp! 'append)
(add-hook 'csharp-mode-hook #'lsp!)

(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-ts-mode))

(add-hook 'csharp-ts-mode-local-vars-hook #'lsp! 'append)

(setq typescript-ts-mode-indent-offset 4)
(add-hook 'typescript-ts-base-mode-hook (lambda ()
                                          (setq-local comment-multi-line t)
                                          (when indent-tabs-mode
                                            (setq-local typescript-ts-mode-indent-offset tab-width))))

;; The built in directive queries cause errors
(after! csharp-mode
  (setq csharp-ts-mode--font-lock-settings (cl-remove-if (lambda (r)
                                                           (eq (nth 2 r) 'directives))
                                                         csharp-ts-mode--font-lock-settings)))

(defface font-lock-constructor-face
  '((t . (:inherit font-lock-function-name-face
          :slant italic)))
  "Face for html tags.")

(autoload 'treesit-additional-font-lock-rules "treesit-additional-font-lock-rules")
(after! treesit
  (treesit-additional-font-lock-rules 'csharp-ts-mode
    :language 'c-sharp
    :feature 'definition
    :override t
    '((using_directive (identifier) @font-lock-variable-name-face)
      (qualified_name (identifier) @font-lock-variable-name-face)
      (qualified_name "." @font-lock-punctuation-face)

      (property_declaration
       type: (generic_name (identifier) @font-lock-type-face))

      (constructor_declaration
       name: (_) @font-lock-constructor-face)

      (expression_statement
       (assignment_expression left: (identifier) @font-lock-variable-name-face))

      (object_creation_expression
       type: (identifier) @font-lock-constructor-face)
      (object_creation_expression
       type: (generic_name (identifier) @font-lock-constructor-face)))

    :language 'c-sharp
    :feature 'type
    :override t
    '((predefined_type) @font-lock-builtin-face
      (void_keyword) @font-lock-builtin-face
      (implicit_type "var" @font-lock-keyword-face)))

  (cl-loop for (mode . feature) in '((typescript-ts-mode . identifier)
                                     (vue-ts-mode . vue-typescript-identifier))
           do (apply #'treesit-additional-font-lock-rules
                     mode
                     `(:language typescript
                       :feature ,feature
                       :override t
                       ((undefined) @font-lock-builtin-face
                        (predefined_type) @font-lock-builtin-face
                        (new_expression
                         constructor: (identifier) @font-lock-constructor-face)
                        (assignment_expression
                         left: (identifier) @font-lock-variable-name-face)
                        (member_expression
                         object: (identifier) @font-lock-type-face
                         (:match "^[[:upper:]].*" @font-lock-type-face))
                        (type_parameters
                         ["<" ">"] @font-lock-bracket-face)
                        (type_arguments
                         ["<" ">"] @font-lock-bracket-face)))))

  (treesit-additional-font-lock-rules 'json-ts-mode
    :language 'json
    :feature 'pair
    :override t
    '((object
       (pair
        key: (string "\"" @font-lock-bracket-face)))
      ;; special coloring for top level object keys
      (document
       (object
        (pair
         key: (string
               (string_content) @font-lock-keyword-face)))))))

(add-hook 'typescript-ts-mode-local-vars-hook #'lsp! 'append)
(add-hook 'tsx-ts-mode-local-vars-hook #'lsp! 'append)
(add-hook 'js-ts-mode-local-vars-hook #'lsp! 'append)

(autoload 'cs-ts-extras-convert-to-typescript-at-point-dwim "cs-ts-extras" nil t)

(autoload 'jsx-ts-extras-auto-close-mode "jsx-ts-extras")
(add-hook 'tsx-ts-mode-hook #'jsx-ts-extras-auto-close-mode)
(autoload 'jsx-ts-extras-auto-open-tag-mode "jsx-ts-extras")
(add-hook 'tsx-ts-mode-hook #'jsx-ts-extras-auto-open-tag-mode)
(add-hook 'tsx-ts-mode-hook
            (lambda ()
              (when (require 'emmet-mode nil t)
                (emmet-mode 1))))

(autoload 'jsx-ts-extras-element-match "jsx-ts-extras")
(advice-add 'jsx-ts-extras-element-match :around #'doom-set-jump-maybe-a)
(map!
   :mode tsx-ts-mode
   (:localleader
    :desc "Go to matching element tag" "m" #'jsx-ts-extras-element-match))

(map!
 :mode csharp-ts-mode
 :localleader
 :desc "Convert type def to TypeScript" "c" #'cs-ts-extras-convert-to-typescript-at-point-dwim)

(msbuild-auto-project-mode 1)

(defadvice! schmo/lsp-find-session-folder (orig-lsp-find-session-folder session file-name)
  "Advice around `lsp-find-session-folder'.

Use the solution root for C# files instead of the closest project folder.
Prevents spawning additional omnisharp processes inside of solution projects,
especially those which DO need their own language server processes for frontend
code"
  :around 'lsp-find-session-folder
  (if (and (string-match-p "\\.cs\\'" file-name)
           lsp-csharp-solution-file)
      (file-name-directory lsp-csharp-solution-file)
    (funcall orig-lsp-find-session-folder session file-name)))

;;; Which key
(setq which-key-idle-delay 0.25)
(setq doom-leader-alt-key "M-RET")

;;; Completion
(setq completion-ignore-case t)

(use-package! fussy
  :config
  (setq fussy-filter-fn 'fussy-filter-flex))

(defun schmo/+vertico-orderless-dispatch (pattern _index _total)
  "Orderless dispatch which swaps % and ~ from DOOM's
`+vertico-orderless-dispatch'. I'm more likely to use `orderless-flex', so I
want it on a key that's easier to hit"
  (cond
   ;; Character folding
   ((string-prefix-p "~" pattern) `(char-fold-to-regexp . ,(substring pattern 1)))
   ((string-suffix-p "~" pattern) `(char-fold-to-regexp . ,(substring pattern 0 -1)))
   ;; Flex matching
   ((string-prefix-p "%" pattern) `(orderless-flex . ,(substring pattern 1)))
   ((string-suffix-p "%" pattern) `(orderless-flex . ,(substring pattern 0 -1)))))

(after! orderless
  (add-to-list 'orderless-style-dispatchers #'schmo/+vertico-orderless-dispatch)
  ;; fall back on fussy for automatic fuzzy matching if I get lazy using orderless
  (setq completion-styles '(orderless fussy basic)))

;; I have no use for the default C-o mapping
(map! :i "C-o" #'completion-at-point)

(map! :after corfu
      :map corfu-map
      (:gi "C-y" #'corfu-insert)
      (:gi "C-g" #'corfu-quit)
      (:gi "TAB" nil)
      (:gi "<tab>" nil)
      (:gi "<backtab>" nil)
      (:gi "S-TAB" nil))

(setq +corfu-want-ret-to-confirm 'minibuffer)
(add-hook! 'corfu-mode-hook (setq-local completion-styles '(fussy orderless basic)))

;;; Web Mode

(def-project-mode! vue-minor-mode
  :match "\\.vue$")

(def-project-mode! vue-script-mode
  :match "\\.\\(js\\|ts\\)$"
  :when (schmo/vue-project-p (locate-dominating-file "." "package.json")))

(set-yas-minor-mode! 'vue-script-mode)
(set-yas-minor-mode! 'vue-minor-mode)

(add-hook 'vue-minor-mode-hook (lambda ()
                                 (treesit-parser-create 'vue)))

(add-to-list 'web-mode-language-triggers-yas-extra-mode-alist '("typescript" vue-script-mode))

(defun schmo/unset-vue-web-mode-padding ()
  (setq-local web-mode-part-padding nil)
  (setq-local web-mode-script-padding nil)
  (setq-local web-mode-style-padding nil))

(add-hook 'vue-minor-mode-hook #'schmo/unset-vue-web-mode-padding)

(defadvice! schmo/after-editorconfig-set-indentation (&rest _)
  "Override "
  :after #'editorconfig-set-indentation
  (when vue-minor-mode
    (schmo/unset-vue-web-mode-padding)))

;; WIP regexp for highlighting compilation errors for a work Vue project
;; "^\\(\\[vue-tsc\\] ?\\)? \\(.*?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\) - error"

(add-hook 'web-mode-hook 'web-mode-language-triggers-mode)

(after! web-mode
  ;; doom modifies web-mode's autopairs to avoid conflict with smart parens,
  ;; but it doesn't remove whitespace from the closing portion. In Vue (for example),
  ;; this leads to this being inserted: {{ |  }}, when this is desired: {{ | }}
  ;; just disabling web-modes auto-pairing altogether for now; smartparens covers
  ;; most of my needs
  (setq web-mode-enable-auto-pairing nil))
(add-to-list 'auto-mode-alist '("\\.cshtml\\'" . web-mode))

;; Fix newline behavior in comments in web-mode. Seems like a new problem after upgrading
;; to Emacs 29.
(setq-hook! 'web-mode-hook comment-line-break-function #'web-mode-comment-indent-new-line)

(defadvice! schmo/web-mode-yasnippet-exit-hook (fn)
  "Workaround an LSP timeout issue when completing css snippets in vue files"
  :around #'web-mode-yasnippet-exit-hook
  (when (not (string= "css" (web-mode-language-at-pos)))
    (funcall fn)))

(when IS-WINDOWS
  (defmacro schmo/fix-windows-float-conversion-fn (fn)
    `(defadvice! ,(intern (concat "schmo/fix-" (symbol-name (cadr fn)))) (orig-fn &rest args)
       :around ,fn
       (if (= 0.0 (car args))
           0
         (apply orig-fn args))))

  (schmo/fix-windows-float-conversion-fn #'floor)
  (schmo/fix-windows-float-conversion-fn #'round)
  (schmo/fix-windows-float-conversion-fn #'ceiling)
  (schmo/fix-windows-float-conversion-fn #'truncate))

(setq emmet-indent-after-insert nil)
;; Something about emmet-expand-yas breaks undo... just use emmet-expand-line instead
(advice-add 'emmet-expand-yas :override 'emmet-expand-line)

(add-hook! 'emmet-mode-hook
  (map! :map emmet-mode-keymap
        "<tab>" nil
        "C-h ," #'emmet-expand-line))

(after! yasnippet
  ;; breaks undo
  (setq yas-snippet-revival nil)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;; I wish there were a better way to disable doom's smart tab binding,
  ;; but this works.
  (map! :i [tab] nil))

(map! ;; expand snippets with C-l instead
      :gni "C-l" (cmds! (and (modulep! :editor snippets)
                             (yas-maybe-expand-abbrev-key-filter 'yas-expand))
                        #'yas-expand))
;;; Misc

;; TODO DOOM adds a function to `after-change-major-mode-hook' that tries to detect whitespace mismatches.
;; It doesn't take editorconfig changes into account, so tabs always get highlighted. Need to change that.
(setq whitespace-global-modes nil)

(map! :leader
      :prefix "t"
      "w" #'whitespace-mode
      "W" #'visual-line-mode)

(setq show-paren-context-when-offscreen 'overlay)

(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))

;; consider these symbols to be parts of words in lisp modes
(let ((word-characters (list ?- ?: ?? ?/ ?+))
      (modes '(emacs-lisp-mode lisp-mode clojure-mode scheme-mode)))
  (dolist (mode modes)
    (let* ((mode-name (symbol-name mode))
           (hook (intern (concat mode-name "-hook")))
           (syntax-table-sym (intern (concat mode-name "-syntax-table"))))
      (add-hook hook (lambda ()
                       (dolist (char word-characters)
                         (modify-syntax-entry char "w" (symbol-value syntax-table-sym))))))))

;; Maximize window on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(defhydra schmo/window-resize nil
  "Window Resize"
  ("h" #'evil-window-decrease-width "Decrease Width")
  ("j" #'evil-window-increase-height "Increase Height")
  ("k" #'evil-window-decrease-height "Decrease Height")
  ("l" #'evil-window-increase-width "Increase Width")
  ("=" #'balance-windows "Balance"))

(defun schmo/restart-windows-parallels-network-device ()
  "Restarts the Parallels network adapter over in the Windows VM.
Sometimes upload speeds get borked and local dev is slow; restarting the network
device helps"
  (interactive)
  ;; TODO: check if running on Windows and run command directly
  (dlet ((prlctl-run-with-current-user nil))
    (prlctl-exec-shell-command
     (prlctl-find-vm-by-name-or-uuid "Windows 11")
     "netsh interface set interface Ethernet disable && netsh interface set interface Ethernet enable"
     t)))


;;; CSS

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
      ;; If in normal mode and the point visual covers a space, I want the
      ;; relative units to be inserted at the next position.
      ;; Typical CSS example:
      ;; line-height:❚;
      ;; should become
      ;; line-height: 0.75❚ (where cursor covers the semicolon)
      ;; But if I'm in insert mode, I still want this:
      ;; line-height: |;
      ;; to become
      ;; line-height: 0.75|;
      (when (and evil-normal-state-minor-mode
                 (= 32 (char-after))
                 (not (= (point) (point-at-eol))))
        (goto-char (+ 1 (point))))
      (insert (if (s-ends-with? ".0" res)
                  (s-replace ".0" "" res)
                res)))))

;;; org
(setq org-directory "~/org/")
(add-to-list 'org-modules 'ol-info)

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

(defun schmo/parse-minutes-org (org-line)
  (let* ((parts (thread-first org-line
                              (split-string " ")
                              (last)
                              (car)
                              (split-string ":")))
         (hours-in-minutes (if (string-empty-p (car parts))
                               0
                             (floor (* 60 (string-to-number (car parts))))))
         (minutes (if (or (null (cdr parts))
                          (string-empty-p (cadr parts)))
                      0
                    (string-to-number (cadr parts)))))
    (+ hours-in-minutes minutes)))

(defun schmo/tally-region-org (start end)
  (interactive "r")
  (let ((time-in-mins (apply #'+ (mapcar 'schmo/parse-minutes-org
                                         (split-string (buffer-substring-no-properties start end)
                                                       "\n")))))
    (deactivate-mark)
    (goto-line (line-number-at-pos (if (> start end)
                                       start
                                     end)))
    (open-line 1)
    (newline)
    (insert "- TOTAL: ")
    (let ((hours (/ time-in-mins 60))
          (mins (% time-in-mins 60)))
      (when (< 0 hours)
        (insert (number-to-string hours)))
      (when (< 0 mins)
        (insert ":")
        (insert (s-pad-left 2 "0" (number-to-string mins)))))))


;;; JSON utils

(cl-defmacro schmo/define-json-region-format-command (name docstring &key js buffer-name (major-mode 'json-ts-mode))
  (declare (indent defun))
  (let* ((basedoc "Arguments:
START:   region start or `point-min'.
END:     region end or `point-max'.
REPLACE: when called interactively with a universal argument or called
         non-interactively with a non-nil value, replace the contents of the
         region or buffer with the modified value. Otherwise, open a dedicated
         buffer.")
         (doc (if (stringp docstring)
                  (format "%s\n\n%s" docstring basedoc)
                basedoc)))
    `(defun ,name (start end &optional replace)
       ,doc
       (interactive (if (region-active-p)
                        (list (region-beginning) (region-end) current-prefix-arg)
                      (list (point-min) (point-max) current-prefix-arg)))
       (let ((command (schmo/format-node-command ,js))
             (buffer-name ,buffer-name))
         (if replace
             (shell-command-on-region start end command nil t)
           (shell-command-on-region start end command buffer-name)
           (with-current-buffer buffer-name
             (,major-mode)
             (set-buffer-modified-p nil)
             (setq buffer-undo-list nil))
           (pop-to-buffer buffer-name))))))

(schmo/define-json-region-format-command schmo/stringify-json
  "Stringify JSON in region such that it can used as a string value in a JSON
object."
  :js "console.log(JSON.stringify(JSON.stringify(JSON.parse(<<region>>))))"
  :buffer-name "*stringified json*"
  :major-mode fundamental-mode)

(schmo/define-json-region-format-command schmo/prettify-json
  "Prettify JSON in region or buffer.
Indented with tabs."
  :js "console.log(JSON.stringify(JSON.parse(<<region>>), null, '\\t'))"
  :buffer-name "*formatted json*")

(schmo/define-json-region-format-command schmo/minify-json
  "Minify JSON in region or buffer."
  :js "console.log(JSON.stringify(JSON.parse(<<region>>)))"
  :buffer-name "*minified json*")

(defun schmo/edit-json-string-at-point ()
  "Edit the string at point in a `json-mode' buffer and handle reformatting."
  (interactive)
  (call-interactively #'edit-string-at-point)
  (schmo/prettify-json (point-min) (point-max) t)
  (setq buffer-undo-list nil)
  (set-buffer-modified-p nil)
  (json-ts-mode)
  (deactivate-mark)
  (setq-local string-edit-mode-reformat-string-function
              (lambda (_ new-json)
                (with-temp-buffer
                  (insert new-json)
                  (schmo/minify-json (point-min) (point-max) t)
                  (schmo/stringify-json (point-min) (point-max) t)
                  (goto-char (point-min))
                  (cl-destructuring-bind (start . end) (thing-at-point-bounds-of-string-at-point)
                    (buffer-substring-no-properties (1+ start) (1- end)))))))


;;; better-jumper

(advice-add 'avy-action-goto :around #'doom-set-jump-maybe-a)

(defun schmo/reapply-csharp-ts-mode-font-lock-settings ()
  "Fixes csharp-ts-mode font lock with latest version of parser"
  (interactive)
  (setq csharp-ts-mode--keywords
        '("this" "add" "alias" "as" "base" "break" "case" "catch" "checked" "class" "continue"
          "default" "delegate" "do" "else" "enum" "event" "explicit" "extern" "finally"
          "for" "foreach" "global" "goto" "if" "implicit" "interface" "is" "lock"
          "namespace" "notnull" "operator" "params" "return" "remove" "sizeof"
          "stackalloc" "static" "struct" "switch" "throw" "try" "typeof" "unchecked"
          "using" "while" "new" "await" "in" "yield" "get" "set" "when" "out" "ref" "from"
          "where" "select" "record" "init" "with" "let"))

  (let ((ops '("--" "-" "-=" "&" "&=" "&&" "+" "++" "+=" "<" "<=" "<<" "<<=" "="
               "==" "!" "!=" "=>" ">" ">=" ">>" ">>=" ">>>" ">>>=" "|" "|=" "||"
               "?" "??" "??=" "^" "^=" "~" "*" "*=" "/" "/=" "%" "%=" ":")))
    (setq csharp-ts-mode--font-lock-settings
          (treesit-font-lock-rules
           :language 'c-sharp
           :feature 'bracket
           '((["(" ")" "[" "]" "{" "}" (interpolation_brace)]) @font-lock-bracket-face)

           :language 'c-sharp
           :feature 'delimiter
           `((["," ":" ";"]) @font-lock-delimiter-face
             ([,@ops]) @font-lock-operator-face
             )

           :language 'c-sharp
           :override t
           :feature 'comment
           '((comment) @font-lock-comment-face)

           :language 'c-sharp
           :override t
           :feature 'keyword
           `([,@csharp-ts-mode--keywords] @font-lock-keyword-face
             (modifier) @font-lock-keyword-face
             (implicit_type) @font-lock-keyword-face)

           :language 'c-sharp
           :override t
           :feature 'property
           `((attribute name: (identifier) @font-lock-property-use-face))

           :language 'c-sharp
           :override t
           :feature 'literal
           `((integer_literal) @font-lock-number-face
             (real_literal) @font-lock-number-face
             (null_literal) @font-lock-constant-face
             (boolean_literal) @font-lock-constant-face)

           :language 'c-sharp
           :override t
           :feature 'string
           `([(character_literal)
              (string_literal)
              (raw_string_literal)
              (verbatim_string_literal)
              ;; (interpolated_string_expression)
              (string_content)
              (interpolation_start)
              (interpolation_quote)] @font-lock-string-face)

           :language 'c-sharp
           :override t
           :feature 'escape-sequence
           '((escape_sequence) @font-lock-escape-face)

           :language 'c-sharp
           :feature 'type
           :override t
           '((generic_name (identifier) @font-lock-type-face)
             (type_parameter (identifier) @font-lock-type-face)
             (parameter type: (identifier) @font-lock-type-face)
             (type_argument_list (identifier) @font-lock-type-face)
             (as_expression right: (identifier) @font-lock-type-face)
             (is_expression right: (identifier) @font-lock-type-face)
             (_ type: (identifier) @font-lock-type-face)
             (predefined_type) @font-lock-builtin-face
             )

           :language 'c-sharp
           :feature 'definition
           :override t
           '((interface_declaration name: (identifier) @font-lock-type-face)
             (class_declaration name: (identifier) @font-lock-type-face)
             (enum_declaration name: (identifier) @font-lock-type-face)
             (struct_declaration (identifier) @font-lock-type-face)
             (record_declaration (identifier) @font-lock-type-face)
             (namespace_declaration name: (identifier) @font-lock-type-face)
             (constructor_declaration name: (identifier) @font-lock-constructor-face)
             (destructor_declaration name: (identifier) @font-lock-constructor-face)
             (base_list (identifier) @font-lock-type-face)
             (enum_member_declaration (identifier) @font-lock-variable-name-face)
             (parameter name: (identifier) @font-lock-variable-name-face)
             (implicit_parameter) @font-lock-variable-name-face
             )

           :language 'c-sharp
           :feature 'function
           '((method_declaration name: (identifier) @font-lock-function-name-face)
             (local_function_statement name: (identifier) @font-lock-function-name-face)
             (invocation_expression
              function: (member_access_expression
                         name: (identifier) @font-lock-function-call-face))
             (invocation_expression
              function: (identifier) @font-lock-function-call-face)
             (invocation_expression
              function: (member_access_expression
                         name: (generic_name (identifier) @font-lock-function-call-face)))
             (invocation_expression
              function: (generic_name (identifier) @font-lock-function-call-face)))

           :language 'c-sharp
           :feature 'expression
           '((identifier) @font-lock-variable-use-face)

           :language 'c-sharp
           :feature 'directives
           :override t
           '((preproc_if
              "#if" @font-lock-preprocessor-face)
             (preproc_if
              "#endif" @font-lock-preprocessor-face)
             (preproc_elif
              "#elif" @font-lock-preprocessor-face)
             (preproc_else
              "#else" @font-lock-preprocessor-face)
             ;; (preproc_endif) @font-lock-preprocessor-face
             (preproc_define
              "#define" @font-lock-preprocessor-face
              (preproc_arg) @font-lock-constant-face)
             (preproc_undef
              "#undef" @font-lock-preprocessor-face
              (preproc_arg) @font-lock-constant-face)

             (preproc_nullable) @font-lock-preprocessor-face
             (preproc_pragma) @font-lock-preprocessor-face
             (preproc_region
              "#region" @font-lock-preprocessor-face
              (preproc_arg) @font-lock-comment-face)
             (preproc_endregion) @font-lock-preprocessor-face)))))

(after! csharp-mode
  (schmo/reapply-csharp-ts-mode-font-lock-settings))
