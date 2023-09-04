;;; ts-refactor.el --- Tree-sitter based refactorings -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Jacob
;;
;; Author: Jacob <>
;; Maintainer: Jacob <>
;; Created: September 01, 2023
;; Modified: September 01, 2023
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/theschmocker/dotfiles
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Tree-sitter based refactorings
;;
;;; Code:

(require 'cl-lib)
(require 'treesit)
(require 'subr-x)

(cl-defstruct ts-refactor--definition
  ""
  name
  description
  language
  find-node
  refactor)

(defvar ts-refactor-definition-alist
  nil
  "name->definition")

(setq ts-refactor-definition-alist nil)

(cl-defun ts-refactor-define (&key name description language find-node refactor)
  (declare (indent defun))
  ;; (when (assq name ts-refactor-definition-alist)
  ;;   (warn "Redefining refactoring \"%s\"" name))
  (add-to-list 'ts-refactor-definition-alist
               (cons name
                     (make-ts-refactor--definition
                      :name name
                      :description description
                      :language language
                      :find-node find-node
                      :refactor refactor))))

(defvar ts-refactor--csharp-access-level-modifier
  '("public" "protected" "private" "internal"))

(defun ts-refactor--csharp-find-access-modifier (language &optional pos)
  ""
  (let* ((pos (or pos (point)))
         (access-modifier-p (lambda (node)
                              (and
                               (equal "modifier" (treesit-node-type node))
                               (string-match-p (regexp-opt ts-refactor--csharp-access-level-modifier)
                                               (treesit-node-text node)))))
         (first-access-modifier-child (lambda (node)
                                        (car (treesit-filter-child node access-modifier-p)))))
    (funcall first-access-modifier-child
             (treesit-parent-until
              (treesit-node-at pos language)
              first-access-modifier-child))))

(ts-refactor-define
  :name 'csharp-change-access-level
  :description "Change Access Level"
  :language 'c-sharp
  :find-node #'ts-refactor--csharp-find-access-modifier
  :refactor (lambda (node)
              (let ((start (treesit-node-start node))
                    (end (treesit-node-end node))
                    (new-modifier (completing-read "Select modifier: "
                                                   (cl-remove-if (lambda (m)
                                                                   (equal (treesit-node-text node) m))
                                                                 ts-refactor--csharp-access-level-modifier)
                                                   nil
                                                   t)))
                (save-excursion
                  (goto-char start)
                  (delete-region start end)
                  (insert new-modifier)))))

(defvar ts-refactor--vue-conditional-directives
  '("v-if" "v-else-if" "v-else"))

(defun ts-refactor--vue-get-element-at-point (point)
  (treesit-parent-until
   (treesit-node-at point 'vue)
   (lambda (node)
     (equal "element" (treesit-node-type node)))))

(defun ts-refactor--vue-tag-p (kind node)
  ""
  (equal (format "%s_tag" kind) (treesit-node-type node)))

(defun ts-refactor-vue--get-element-tag (el-node kind)
  (let ((name (if (eq kind 'self-closing)
                  "self_closing"
                (symbol-name kind))))
    (car
     (treesit-filter-child
      el-node
      (lambda (node)
        (equal (format "%s_tag" name) (treesit-node-type node)))))))

(defun ts-refactor--vue-get-element-start-or-self-closing-tag (el-node)
  ""
  (car (treesit-filter-child
        el-node
        (lambda (node)
          (or (ts-refactor--vue-tag-p "start" node)
              (ts-refactor--vue-tag-p "self_closing" node))))))

(defun ts-refactor--vue-empty-text-node-p (node)
  ""
  (and (equal "text" (treesit-node-type node))
       (string-empty-p (string-trim (treesit-node-text node t)))))

(defun ts-refactor-vue--get-children-of-element (el-node &optional include-empty)
  (treesit-filter-child
   el-node
   (lambda (child)
     (not (or (ts-refactor--vue-tag-p "start" child)
              (ts-refactor--vue-tag-p "end" child)
              (unless include-empty
                (ts-refactor--vue-empty-text-node-p child)))))))

(defun ts-refactor--vue-get-element-last-child (el-node)
  (thread-last (treesit-filter-child
                el-node
                (lambda (child)
                  (not (or (ts-refactor--vue-tag-p "start" child)
                           (ts-refactor--vue-tag-p "end" child)
                           (ts-refactor--vue-empty-text-node-p child)))))
               (last)
               (car)))

(ts-refactor-define
  :name 'vue-make-element-conditional
  :description "Make Element Conditional"
  :language 'vue
  :find-node (lambda (language &optional pos)
               (when-let* ((el (ts-refactor--vue-get-element-at-point (or pos (point))))
                           (start-tag (ts-refactor--vue-get-element-start-or-self-closing-tag el)))
                 (unless (treesit-query-capture
                          start-tag
                          `((directive_attribute
                             (directive_name) @name
                             (:match ,(regexp-opt ts-refactor--vue-conditional-directives) @name))))
                   el)))
  :refactor (lambda (node)
              (let* ((tag (ts-refactor--vue-get-element-start-or-self-closing-tag node))
                     (tag-name (car (treesit-filter-child
                                     tag
                                     (lambda (n)
                                       (equal "tag_name" (treesit-node-type n))))))
                     (first-attr (car (treesit-filter-child
                                       tag
                                       (lambda (node)
                                         (or (equal "attribute" (treesit-node-type node))
                                             (equal "directive_attribute" (treesit-node-type node)))))))
                     (newlinep (and first-attr
                                    (not (eq (line-number-at-pos (treesit-node-end tag-name))
                                             (line-number-at-pos (treesit-node-start first-attr)))))))
                (goto-char (treesit-node-end tag-name))
                (if newlinep
                  (newline-and-indent)
                  (insert " "))
                (insert "v-if=\"\"")
                (goto-char (1- (point))))))

;; incomplete
(ts-refactor-define
  :name 'vue-barf
  :description "Barf"
  :language 'vue
  :find-node (lambda (language &optional pos)
               (ts-refactor--vue-get-element-last-child
                (ts-refactor--vue-get-element-at-point pos)))
  :refactor (lambda (node)
              (let* ((el (ts-refactor--vue-get-element-at-point (point))))
                (save-excursion
                  (goto-char (treesit-node-start node))
                  (let ((keep-newline-p (looking-at "\n")))
                    (delete-region (+ (treesit-node-start node)
                                      (if keep-newline-p
                                          1
                                        0))
                                   (treesit-node-end node))
                    (let ((line-start (line-beginning-position))
                          (line-end (line-end-position)))
                      (when (or (eql line-start line-end)
                                (string-empty-p (string-trim (buffer-substring line-start line-end))))
                        (delete-line)))))
                ;; TODO use markers
                (setq el (ts-refactor--vue-get-element-at-point (point)))

                (let ((children (ts-refactor-vue--get-children-of-element el t)))
                  (if (cl-every #'ts-refactor--vue-empty-text-node-p children)
                      (save-excursion
                        (let ((c (car children)))
                          (delete-region (treesit-node-start c) (treesit-node-end c))))


                    (save-excursion
                      (goto-char (treesit-node-start (ts-refactor-vue--get-element-tag el 'end)))
                      (indent-according-to-mode))))

                ;; (let ((el ))
                ;;   ;; (let (fill-prefix indent-region-function)
                ;;   ;;   (indent-region (treesit-node-start el) (treesit-node-end el)))

                ;;   )
                )))

(defun ts-refactor--completing-read ()
  ""
  (let ((available-refactors (cl-remove-if-not (lambda (entry)
                                                 (let* ((def (cdr entry))
                                                        (lang (ts-refactor--definition-language def))
                                                        (find-node (ts-refactor--definition-find-node def)))
                                                   (funcall find-node lang (point))))
                                               ts-refactor-definition-alist)))
    (when (null available-refactors)
      (error "No available refactors"))
    (intern
     (completing-read "Choose a refactor: " available-refactors nil t))))

(defun ts-refactor (refactor-name)
  ""
  (interactive (list (ts-refactor--completing-read)))
  (let ((refactor (assoc-default refactor-name ts-refactor-definition-alist)))
    (when (not refactor)
      (error "Refactor \"%s\" not registered" refactor-name))
    (let ((lang (ts-refactor--definition-language refactor))
          (find-node (ts-refactor--definition-find-node refactor))
          (do-refactor (ts-refactor--definition-refactor refactor)))
      (funcall do-refactor (funcall find-node lang (point))))))

(provide 'ts-refactor)
;;; ts-refactor.el ends here
