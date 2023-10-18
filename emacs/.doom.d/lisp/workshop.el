;;; workshop.el -- WIP/misc elisp that I'm tinkering with that doesn't have a better home yet. -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; Stuff that I feel may be worth keeping, but doesn't have a home yet. Avoids
;;; cluttering config.el and the potential for losing it in a scratch buffer
;;;
;;; Code:

(require 'cl-lib)
(require 'f)
(require 'project)
(require 'subr-x)
(require 'json)
(require 'doom-lib)

;; Beginnings of a multi-package-root npm command runner

(defun schmo/make-project-package-json-file-lookup-by-name ()
  "Create an alist of name->package.json-path.
Name is read from the package.json file."
  (mapcar (lambda (pj-filename)
            (cons (thread-last pj-filename
                               (json-read-file)
                               (alist-get 'name))
                  pj-filename))
          (schmo/get-project-package-json-files)))

(defun schmo/project-package-json-completing-read ()
  (let* ((name-to-filename-alist (schmo/make-project-package-json-file-lookup-by-name))
         (annotations (schmo/make-project-package-json-annotation-lookup name-to-filename-alist))
         (annotation-function (lambda (s)
                                (propertize (concat " " (alist-get s annotations "" nil #'string=))
                                            'face 'completions-annotations)))
         (selected (completing-read
                    "Choose project: "
                    (lambda (s pred action)
                      (cond
                       ((eq 'metadata action) `(metadata (annotation-function . ,annotation-function)))
                       (t (all-completions s (mapcar #'car name-to-filename-alist) pred)))))))
    (alist-get selected name-to-filename-alist nil nil #'string=)))



(defun schmo/make-project-package-json-annotation-lookup (package-jsons)
  (let* ((files (mapcar #'cdr package-jsons))
         (common-prefix (f-common-parent files)))
    (mapcar (lambda (pj)
              (cons (car pj)
                    (thread-last (cdr pj)
                                 (string-replace common-prefix "")
                                 (file-name-directory)
                                 (replace-regexp-in-string "\\(/\\|\\\\\\)\\'" ""))))
            package-jsons)))

(defadvice! schmo/npm-mode-run (orig-fn)
  :around #'npm-mode-npm-run
  (interactive)
  (dlet ((default-directory (file-name-directory (schmo/project-package-json-completing-read))))
    (call-interactively orig-fn)))

(defadvice! schmo/npm-mode-npm-install (orig-fn)
  :around #'npm-mode-npm-install
  (interactive)
  (dlet ((default-directory (file-name-directory (schmo/project-package-json-completing-read))))
    (call-interactively orig-fn)))

;; Can use npm-mode-npm-run. It uses `locate-dominating-file' at
;; `default-directory' to search for package.json. I work on a project that has
;; multiple nested package.json files, and right now, I must be visiting a file
;; within one of those projects to be able to run an npm script. By overriding
;; default directory, I can make it use a specific package.json file anywhere.
;; The goal with the code above is to support selecting such a nested
;; package.json file and running one of its npm scripts from anywhere in the
;; greater project
;;
;; I may try and abstract some of this or just copy the approach for my msbuild
;; package to support multi-solution repos. I just found `project-files', so the
;; approach I use here to find package.json files would let me provide a good
;; default for setting the sln file even in single-solution repos
;;
;; (let ()
;;   (dlet ((default-directory (file-name-directory (schmo/project-package-json-completing-read))))
;;     (call-interactively #'npm-mode-npm-run)))

(defvar-local ts-query-builder-mode-target-buffer nil)
(defvar-local ts-query-builder-mode-parser-language nil)
(defvar-local ts-query-builder-mode-overlays nil)

(define-derived-mode ts-query-builder-mode scheme-mode "TS Query Builder"
  ""
  (make-local-variable 'after-change-functions)
  (add-to-list 'after-change-functions (lambda (&rest _)
                                         (unwind-protect
                                             (schmo/ts-query--replace-overlays)))))

(defun schmo/ts-query-builder ()
  (interactive)
  (let* ((target-buffer (current-buffer))
         (parsers (treesit-parser-list target-buffer)))
    (when (null parsers)
      (error "No active tree-sitter parsers in the current buffer"))
    (let ((query-buffer (get-buffer-create (format "*ts-query-builder: %s*" (buffer-name target-buffer)))))
      (with-current-buffer query-buffer
        (ts-query-builder-mode)
        (setq ts-query-builder-mode-target-buffer target-buffer)
        ;; TODO
        (setq ts-query-builder-mode-parser-language 'c-sharp))
      (display-buffer query-buffer))))

(defun ts-query--overlay-from-range (buf range)
  (let ((o (make-overlay (car range) (cdr range) buf)))
    (overlay-put o 'face 'highlight)
    o))

(defun schmo/ts-query--replace-overlays ()
  (when (not ts-query-builder-mode-target-buffer)
    (error "No target buffer"))
  (condition-case nil
      (let ((query (read (buffer-substring-no-properties (point-min) (point-max)))))
        (dolist (o (car (with-current-buffer ts-query-builder-mode-target-buffer (overlay-lists))))
          (delete-overlay o))
        (let ((lang ts-query-builder-mode-parser-language))
          (let ((ranges (with-current-buffer ts-query-builder-mode-target-buffer
                          (treesit-query-range lang query))))
            (when ranges
              (dolist (range ranges)
                (ts-query--overlay-from-range ts-query-builder-mode-target-buffer range))))))
    ((debug error) nil)))


(defvar schmo/ts-overlays nil)

(defun schmo/ts-highlight-clear-overlays ()
  (interactive)
  (dolist (o schmo/ts-overlays)
    (delete-overlay o)))

(defun schmo/ts-highlight-query-result (results)
  (schmo/ts-highlight-clear-overlays)
  (dolist (match results)
    (let ((face 'highlight)
          start
          end)
      (if (symbolp (car match))
          (progn
            (when (facep (car match))
              (setq face (car match)))
            (setq start (treesit-node-start (cdr match)))
            (setq end (treesit-node-end (cdr match))))
        (setq start (car match))
        (setq end (cdr match)))
      (let ((o (make-overlay start end)))
        (overlay-put o 'face face)
        (push o schmo/ts-overlays)))))

(require 'treesit)

(defvar-local schmo/npm-dep-info-cache nil)

(defun schmo/retrieve-dependency-info (dep-name callback)
  (url-retrieve (format "https://registry.npmjs.org/%s" dep-name)
                (lambda (status)
                  (goto-char (point-min))
                  ;; skip the HTTP headers
                  (while (not (looking-at "\n")) (forward-line))
                  (delete-region (point-min) (+ 1 (point)))
                  (let ((res (json-parse-buffer
                              :object-type 'plist
                              :array-type 'list)))
                    (funcall callback
                             (mapcan (lambda (key)
                                       (list key (plist-get res key)))
                                     '(:description :homepage)))))
                nil
                'silent
                'inhibit-cookies))

(schmo/retrieve-dependency-info "react" #'print)

(defun schmo/get-json-pair-key (node)
  (treesit-node-text
   (car
    (treesit-query-capture
     (treesit-node-child-by-field-name node "key")
     '((string_content) @c)
     nil nil t))
   t))

(defun schmo/get-json-dep-name-at-pos (&optional pos)
  (let ((pos (or pos (point))))
    (let* ((node (treesit-node-at pos))
           (pair (treesit-parent-until node (lambda (n) (equal (treesit-node-type n) "pair"))))
           (pair-parent (treesit-parent-until pair (lambda (n) (equal (treesit-node-type n) "pair")))))
      (when (and pair
                 pair-parent
                 (string-match-p
                  (regexp-opt '("dependencies" "devDependencies" "peerDependencies"))
                  (schmo/get-json-pair-key pair-parent)))
        (schmo/get-json-pair-key pair)))))

(with-current-buffer "package.json"
  (schmo/get-json-dep-name-at-pos))

(defun schmo/insert-with-current-buffer-in-scratch (buf)
  (interactive "b")
  (doom/open-scratch-buffer)
  (goto-char (point-max))
  (newline 2)
  (insert (format "(with-current-buffer \"%s\")" buf))
  (backward-char)
  (newline-and-indent))

(defvar schmo/ts-overlays nil)

(defun schmo/ts-highlight-clear-overlays ()
  (interactive)
  (dolist (o schmo/ts-overlays)
    (delete-overlay o)))

(defun schmo/ts-highlight-query-result (results)
  (schmo/ts-highlight-clear-overlays)
  (dolist (match results)
    (let ((face 'highlight)
          start
          end)
      (if (symbolp (car match))
          (progn
            (cond ((facep (car match))
                   (setq face (car match)))
                  ((string-prefix-p "_" (symbol-name (car match))) ;; ignore captures starting with underscore
                   (setq face nil)))
            (setq start (treesit-node-start (cdr match)))
            (setq end (treesit-node-end (cdr match))))
        (setq start (car match))
        (setq end (cdr match)))
      (let ((o (make-overlay start end)))
        (overlay-put o 'face face)
        (push o schmo/ts-overlays)))))

(defun schmo/create-ts-query-in-scratch-buffer ()
  (interactive)
  (call-interactively #'schmo/insert-with-current-buffer-in-scratch)
  (let ((start (point)))
    (insert (string-trim-right (pp
                                '(schmo/ts-highlight-query-result
                                  (treesit-query-capture
                                   (treesit-buffer-root-node)
                                   )))))
    (indent-region start (point)))
  (backward-char 2)
  (newline-and-indent))

(defun schmo/treesit-selected-immediate-descendant (node path)
  (let ((node-types (string-split path "\\.")))
    (let ((selected node))
      (cl-loop for node-type in node-types
               do (setq selected (if (string-prefix-p "#" node-type)
                                     (let* ((field-parts (string-split node-type "#\\|:\\|\\." t))
                                            (field-name (car field-parts))
                                            (field-node-type (cadr field-parts))
                                            (field (treesit-node-child-by-field-name selected field-name)))
                                       (when (and field (equal field-node-type (treesit-node-type field)))
                                         field))
                                   (let ((child (treesit-node-child selected 0 t)))
                                     (when (equal node-type (treesit-node-type child))
                                       child))))
               unless selected
               return nil)
      selected)))

(defun schmo/ts-if-else-statement-at-point ()
  (let (res
        (node (treesit-node-at (point) 'typescript)))
    (while node
      (if-let* ((ifp (equal "if_statement" (treesit-node-type node)))
                (conseq-ret (or (schmo/treesit-selected-immediate-descendant node "#consequence:return_statement")
                                (schmo/treesit-selected-immediate-descendant node "#consequence:statement_block.return_statement")))
                (true (treesit-node-child conseq-ret 0 t))
                (alt-ret (or (schmo/treesit-selected-immediate-descendant node "#alternative:else_clause.return_statement")
                             (schmo/treesit-selected-immediate-descendant node "#alternative:else_clause.statement_block.return_statement")))
                (false (treesit-node-child alt-ret 0 t)))
          (progn
            (setq res (list :node node
                            :true (treesit-node-text true t)
                            :false (treesit-node-text false t)
                            :condition (treesit-node-text (treesit-node-child-by-field-name node "condition") t)))
            (setq node nil))
        (setq node (treesit-node-parent node))))
    res))

(defun schmo/ts-if-else-to-ternary (cond-plist)
  (when cond-plist
    (format "return %s \n? %s \n: %s;"
            (plist-get cond-plist :condition)
            (plist-get cond-plist :true)
            (plist-get cond-plist :false))))

(defun schmo/ts-convert-if-else-at-point-to-ternary ()
  (interactive)
  (if-let* ((condition (schmo/ts-if-else-statement-at-point))
            (ternary (schmo/ts-if-else-to-ternary condition)))
      (let* ((node (plist-get condition :node))
             (start (treesit-node-start node))
             (end (treesit-node-end node)))
        (goto-char start)
        (delete-region start end)
        (insert ternary)
        (indent-region start (point)))
    (error "No if-else at point that can be converted into a ternary")))

(defun schmo/ts-convert-ternary-return-expression-at-point-to-statements ()
  (interactive)
  (if-let* ((return-node (treesit-parent-until (treesit-node-at (point) 'typescript)
                                               (lambda (n)
                                                 (and (equal "return_statement" (treesit-node-type n))
                                                      (schmo/treesit-selected-immediate-descendant n "ternary_expression")))))
            (ternary (schmo/treesit-selected-immediate-descendant return-node "ternary_expression"))
            (condition (when-let ((field (treesit-node-child-by-field-name ternary "condition")))
                         (treesit-node-text
                          (if (equal "parenthesized_expression" (treesit-node-type field))
                              (treesit-node-child field 0 t)
                            field)
                          t)))
            (true (treesit-node-text (treesit-node-child-by-field-name ternary "consequence") t))
            (false (treesit-node-text (treesit-node-child-by-field-name ternary "alternative") t)))
      (let* ((start (treesit-node-start return-node))
             (end (treesit-node-end return-node)))
        (goto-char start)
        (delete-region start end)
        (insert (format "if (%s) {" condition))
        (newline)
        (insert (format "return %s;" true))
        (newline)
        (insert "} else {")
        (newline)
        (insert (format "return %s;" false))
        (newline)
        (insert "}")
        (indent-region start (point)))
    (error "No ternary at point that can be converted into conditional statements")))

(provide 'workshop)

;;; workshop.el ends here
