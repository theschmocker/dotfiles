;;; vue-ts-mode.el --- Vue major mode based on tree-sitter -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Jacob
;;
;; Author: Jacob <>
;; Maintainer: Jacob <>
;; Created: September 01, 2023
;; Modified: September 01, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jacob/vue-ts-mode
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Tree-sitter
;;
;;; Code:

(require 'treesit)
(require 'typescript-ts-mode)
(require 'js)

(defcustom vue-ts-mode-indent-offset 4
  "Number of spaces for each indentation step in `vue-ts-mode'.

Will be overridden by `tab-width' When `indent-tabs-mode' is non-nil.")

(defvar vue-ts-mode--indent-rules
  `((vue
     ((parent-is "component") column-0 0)
     ((node-is "element") parent-bol vue-ts-mode-indent-offset)
     ((node-is "start_tag") parent-bol 0)
     ((node-is "end_tag") parent-bol 0)
     ((node-is ">") parent-bol 0)
     ((node-is "/>") parent-bol 0)
     ((parent-is "start_tag") parent-bol vue-ts-mode-indent-offset)
     ((parent-is "self_closing_tag") parent-bol vue-ts-mode-indent-offset)
     ((parent-is "element") parent-bol vue-ts-mode-indent-offset)
     ((parent-is "text") parent-bol vue-ts-mode-indent-offset)
     ((node-is "attribute") parent-bol vue-ts-mode-indent-offset)
     ((node-is "directive_attribute") parent-bol vue-ts-mode-indent-offset)
     ;; ((node-is "quoted_attribute_value") parent-bol vue-ts-mode-indent-offset)
     ((parent-is "quoted_attribute_value") parent-bol 0)
     ((parent-is "attribute_value") parent-bol 0)
     ((node-is "interpolation") parent-bol vue-ts-mode-indent-offset)
     )))

(defface vue-ts-mode-html-tag-face
  '((t . (:inherit font-lock-function-name-face)))

  "Face for html tags."
  ;; :group 'web-mode-faces
  )

(defface vue-ts-mode-builtin-tag-face
  '((t . (:inherit font-lock-keyword-face)))

  "Face for Vue builtin tags, like template, script, and style."
  )

(defface vue-ts-mode-attribute-face
  '((t . (:inherit font-lock-constant-face)))

  "Face for html tags."
  )

(defface vue-ts-mode-attribute-value-face
  '((t . (:inherit font-lock-string-face)))
  "Face for attribute values")

(defface vue-ts-mode-builtin-directive-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for built-in directive names")

(defface vue-ts-mode-dynamic-directive-argument-face
  '((t . (:inherit font-lock-variable-name-face)))
  "Face for dynamic directive arguments, e.g. :[arg]")

(defvar vue-ts-mode-builtin-directives
  '("v-text" "v-html" "v-show" "v-if" "v-else" "v-else-if" "v-for" "v-on"
    "v-bind" "v-model" "v-slot" "v-pre" "v-once" "v-memo" "v-cloak"))

(defvar vue-ts-mode-sfc-tags
  '("template" "script" "style"))

(defvar vue-ts-mode--font-lock-settings
  (append
   (treesit-font-lock-rules
    :language 'vue
    :feature 'html
    `(((tag_name) @vue-ts-mode-html-tag-face)
      (quoted_attribute_value "\"" @vue-ts-mode-attribute-value-face)
      ((attribute
        (attribute_name) @vue-ts-mode-attribute-face
        ("=") :? @vue-ts-mode-attribute-face
        (quoted_attribute_value
         (attribute_value) @vue-ts-mode-attribute-value-face) :?))
      (directive_name) @font-lock-keyword-face
      (directive_argument) @font-lock-keyword-face
      (directive_dynamic_argument
       "[" @font-lock-bracket-face
       (directive_dynamic_argument_value) @vue-ts-mode-dynamic-directive-argument-face
       "]" @font-lock-bracket-face)
      (directive_attribute
       "=" @font-lock-keyword-face)
      ["<" ">" "</" "/>"] @font-lock-bracket-face
      (interpolation
       ["{{" "}}"] @font-lock-bracket-face ))

    :language 'vue
    :feature 'html
    :override t
    `(((tag_name) @vue-ts-mode-builtin-tag-face
       (:match ,(regexp-opt vue-ts-mode-sfc-tags) @vue-ts-mode-builtin-tag-face))

      ((directive_attribute
        (directive_name) @vue-ts-mode-builtin-directive-face
        (:match ,(regexp-opt vue-ts-mode-builtin-directives)
                @vue-ts-mode-builtin-directive-face)))))
   (typescript-ts-mode--font-lock-settings 'typescript)
   ;; js--treesit-font-lock-settings
   ))



(defmacro vue-ts-mode--define-lang-attr-predicate (lang)
  (let ((lang (if (symbolp lang)
                  (symbol-name lang)
                lang))
        (fun-sym (intern (concat "vue-ts-mode--start-tag-lang-" lang "-p"))))
    `(defun ,fun-sym (raw-text-node)
       (treesit-query-capture
         (treesit-node-parent raw-text-node)
         '((attribute
            (attribute_name) @attr
            (quoted_attribute_value
             (attribute_value) @lang)
            (:equal "lang" @attr)
            (:equal ,lang @lang)))))))

(vue-ts-mode--define-lang-attr-predicate "ts")
(vue-ts-mode--define-lang-attr-predicate "js")

(defun vue-ts-mode--script-no-lang-p (raw-text-node)
  (null
    (treesit-query-capture
     (treesit-node-parent raw-text-node)
     '((attribute
        (attribute_name) @attr
        (:equal "lang" @attr))))))

(defun vue-ts-mode--js-tag-p (raw-text-node)
  (or (vue-ts-mode--script-no-lang-p raw-text-node)
      (vue-ts-mode--start-tag-lang-js-p raw-text-node)))
(defun vue-ts-mode--point-in-range-p (point range)
  (when range
    (<= (car range) point (cdr range))))

(defun vue-ts-mode--treesit-language-at-point (point)
  "Return the language at POINT."
  (let* ((parser-range-alist
          (mapcar (lambda (p)
                    (cons (treesit-parser-language p) (treesit-parser-included-ranges p))) (treesit-parser-list)))
         (match (cl-find-if (lambda (pair)
                              (let ((lang (car pair))
                                    (ranges (cdr pair)))
                                (cl-some (lambda (range)
                                           (and range
                                                (vue-ts-mode--point-in-range-p point range)))
                                         ranges)))
                            parser-range-alist)))
    (if match
        (car match)
      'vue)))

(define-derived-mode vue-ts-mode prog-mode "Vue"
  "Tree-sitter mode for Vue."
  (when (treesit-ready-p 'vue)
    (treesit-parser-create 'vue))

  (setq-local electric-indent-chars
              (append "{}():;,<>/=" electric-indent-chars))

  (setq-local treesit-font-lock-feature-list
              '((html comment declaration interpolation)
                (keyword string escape-sequence)
                (constant expression identifier number pattern property)
                (function bracket delimiter)))


  (setq-local treesit-language-at-point-function #'vue-ts-mode--treesit-language-at-point)

  (setq-local treesit-font-lock-settings
              vue-ts-mode--font-lock-settings)

  (when indent-tabs-mode
    (setq-local vue-ts-mode-indent-offset tab-width))

  (setq-local typescript-ts-mode-indent-offset vue-ts-mode-indent-offset)

  (setq-local treesit-simple-indent-rules
              (append
               vue-ts-mode--indent-rules
               (typescript-ts-mode--indent-rules 'typescript)))

  (setq treesit-range-settings
        (treesit-range-rules
         ;; #'vue-ts-mode--setup-interpolation-parsers
         ;; :embed 'javascript
         ;; :host 'vue
         ;; '((interpolation
         ;;    (raw_text) @capture))

         ;; :embed 'javascript
         ;; :host 'vue
         ;; `((directive_attribute
         ;;    (quoted_attribute_value
         ;;     (attribute_value) @capture))
         ;;   (script_element
         ;;    (raw_text) @capture
         ;;    (:pred vue-ts-mode--script-no-lang-p @capture))
         ;;   )
         ;; '((directive_attribute
         ;;    (quoted_attribute_value
         ;;     (attribute_value) @attr_js)))

         ;; :embed 'javascript
         ;; :host 'vue
         ;; `(((script_element
         ;;     (raw_text) @javascript
         ;;     (:pred vue-ts-mode--js-tag-p @javascript))))

         :embed 'typescript
         :host 'vue
         '((script_element
            (raw_text) @typescript
            (:pred vue-ts-mode--start-tag-lang-ts-p @typescript)))

         ;; :embed 'css
         ;; :host 'vue
         ;; '((style_element (raw_text) @css))
         ))

  (treesit-major-mode-setup))

(defvar-local vue-ts-mode--interpolation-parsers nil)

(defun vue-ts-mode--setup-interpolation-parsers (_beg _end)
  (mapc #'treesit-parser-delete vue-ts-mode--interpolation-parsers)
  (setq vue-ts-mode--interpolation-parsers nil)
  (let ((ranges (treesit-query-range
                 (treesit-buffer-root-node 'vue)
                 '((directive_attribute
                    (quoted_attribute_value
                     (attribute_value) @attr_js))))))
    (dolist (range ranges)
      (let ((parser (treesit-parser-create 'javascript nil t)))
        (treesit-parser-set-included-ranges parser (list range))
        (push parser vue-ts-mode--interpolation-parsers)))))

(provide 'vue-ts-mode)
;;; vue-ts-mode.el ends here
