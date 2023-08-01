;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(require 's)
(require 'f)

;; Adds custom non-DOOM-module lisp files to load path
(add-to-list 'load-path (f-join doom-user-dir "lisp"))

(require 'schmo-lib)
(require 'prlctl)
(require 'string-edit)
(require 'msbuild)

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

(setq custom-file (concat doom-user-dir "custom-hidden.el"))

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


;;; Editor
(setq scroll-margin 8)
(setq hscroll-margin 16)
(setq display-line-numbers-type 'relative)

;;; Evil
;; disable evil-snipe
(remove-hook 'doom-first-input-hook #'evil-snipe-mode)
(remove-hook 'doom-first-input-hook #'evil-snipe-override-mode) ; fixes stuff like df<Space>

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
                  (call-interactively #'avy-goto-char-timer)))))

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
               ("w" '+vertico/switch-workspace-buffer))

      (:prefix "i"
               (:desc "UUID" "U" #'schmo/insert-uuid)
               (:prefix ("n" . "number")
                        (:desc "Relative units" "r" 'schmo/insert-relative-units))))

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

(map! :after company
      :map company-active-map
      ("C-y" #'company-complete-selection)
      ("RET" nil)
      ("<return>" nil)
      :map (company-active-map company-tng-map)
      ("TAB" nil)
      ("<tab>" nil)
      ("<backtab>" nil)
      ("S-TAB" nil))

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

;;; Cleverparens
(setq evil-cleverparens-use-s-and-S nil)
(add-hook! '(emacs-lisp-mode-hook
             lisp-mode-hook
             clojure-mode-hook
             scheme-mode-hook
             fennel-mode-hook)
           #'evil-cleverparens-mode)

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
  (setq lsp-enable-file-watchers nil)

  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-auto-execute-action nil))

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
    :dependency-of 'svelte-language-server)))

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

(defadvice! schmo/company-capf-candidates (fn &rest args)
  "Wraps `company-capf--candidates' to prioritize fuzzy matching over orderless
for filtering company completion candidates"
  :around #'company-capf--candidates
  (let ((completion-styles '(fussy orderless basic partial-completion emacs22)))
    (apply fn args)))

(defadvice! schmo/company-calculate-candidates (c)
  "Limits the number of candidates that show up in the company completion UI."
  :filter-return #'company-calculate-candidates
  (take 20 c))

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

;; Current company selection will get put into to the buffer
(add-hook 'after-init-hook 'company-tng-mode)

(add-hook! 'org-mode-hook
  ;; dictionary completion was causing some lag when typing. Bumping up the
  ;; delay here makes things feel a bit more smooth for how I type.
  (setq-local company-idle-delay 0.3))

(setq company-selection-wrap-around t
      company-tooltip-width-grow-only t)

;; Prevents company yasnippet completion when there's no symbol prefix
(defadvice! schmo/company-yas (fn &rest args)
  :around #'company-yasnippet
  (cl-letf ((original-company-grab-symbol (symbol-function 'company-grab-symbol)))
    (cl-letf (((symbol-function 'company-grab-symbol) (lambda ()
                                                        (let ((res (funcall original-company-grab-symbol)))
                                                          (if (string-empty-p res)
                                                              nil
                                                            res)))))
      (apply fn args))))

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
  (setq web-mode-enable-auto-pairing nil)
  ;; move company-web-html to the end so it doesn't shadow other backends
  ;; (specifically company-yasnippet)
  (set-company-backend! 'web-mode
    '(:separate company-yasnippet company-web-html))

  (add-to-list 'auto-mode-alist '("\\.cshtml\\'" . web-mode)))

(setq emmet-indent-after-insert nil)
;; Something about emmet-expand-yas breaks undo... just use emmet-expand-line instead
(advice-add 'emmet-expand-yas :override 'emmet-expand-line)

;;; Misc
(setq whitespace-global-modes nil)

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

;;; Popups
;; I have a habit of pressing escape too many times... meaning I keep losing popup buffers.
;; This should make "q" the primary way to close some of them
(set-popup-rules!
  '(("^\\*info" :quit nil :modeline t :size 0.35)
    ("^\\*helpful" :quit nil :modeline t :size 0.35 :ttl 30)))

(plist-put +popup-defaults :modeline t)

;; CSS
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

(defun schmo/restart-windows-parallels-network-device ()
  "Restarts the Parallels network adapter over in the Windows VM.
Sometimes upload speeds get borked and local dev is slow; restarting the network
device helps"
  (interactive)
  (dlet ((prlctl-run-with-current-user nil))
    (prlctl-exec-shell-command
     (prlctl-find-vm-by-name-or-uuid "Windows 11")
     "netsh interface set interface Ethernet disable && netsh interface set interface Ethernet enable"
     t)))

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

(map!
 (:localleader
  :mode (csharp-mode sln-mode)
  (:prefix ("b" . "build")
   :desc "Build All" "a" #'msbuild-build-all
   :desc "Build Project" "b" #'msbuild-build-project)))

(defun schmo/format-node-command (js)
  "Turn JS string into a one-liner node shell command.

Replaces <<stdin>> or <<region>> in JS with a JavaScript expression that reads
from stdin as a string, making this convenient for use with
`shell-command-on-region'."
  (let ((with-stdin-str (replace-regexp-in-string "<<\\(stdin\\|region\\)>>"
                                                  "require('fs').readFileSync(process.stdin.fd).toString()"
                                                  js)))
    (format "node -e %s" (shell-quote-argument with-stdin-str))))

(cl-defmacro schmo/define-json-region-format-command (name docstring &key js buffer-name (major-mode 'json-mode))
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
  (interactive)
  (call-interactively #'edit-string-at-point)
  (schmo/prettify-json (point-min) (point-max) t)
  (setq buffer-undo-list nil)
  (set-buffer-modified-p nil)
  (json-mode)
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

;; "^\\(\\[vue-tsc\\] ?\\)? \\(.*?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\) - error"

(defadvice! schmo/+eshell-default-prompt-fn (prompt)
  :filter-return #'+eshell-default-prompt-fn
  (setq eshell-prompt-regexp "^.* » ")
  (string-replace "λ"
                  (propertize "»" 'face (get-text-property (- (length prompt) 2) 'face prompt))
                  prompt))
