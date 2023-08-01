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

(defun schmo/cleanup-types-for-translation (start end)
  (interactive (if (region-active-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (replace-regexp "public enum" "export enum" nil start end)
  (replace-regexp "public\\( abstract\\)? class" "export interface" nil start end)
  (replace-regexp "\\(\\W+\\)public ?\\(override\\|abstract\\)? " "\\1" nil start end)
  (replace-regexp "{\\(.*?\\(get;\\|set;\\)+.*?\\)+}" "" nil start end)
  (replace-regexp "bool" "boolean" t start end)
  (replace-regexp "\\(int\\|decimal\\|double\\)" "number" t start end)
  (replace-regexp "DateTime" "string" t start end)
  (replace-regexp "Dictionary" "Record" t start end)
  (replace-regexp "{ ?get; ?set; ?}.*$" "" nil start end)
  (delete-trailing-whitespace))

(defun schmo/convert-normal-property ()
  (interactive)
  (end-of-line)
  (backward-kill-word 1)
  (back-to-indentation)
  (yank)
  (insert ": ")
  (set-mark (point))
  (end-of-line)
  (delete-trailing-whitespace)
  (deactivate-mark)
  (insert ";")
  (schmo/downcase-first-char)
  (forward-line 1))

(defun schmo/downcase-first-char ()
  (save-excursion
    (back-to-indentation)
    (downcase-region (point) (1+ (point)))))

(defun schmo/convert-discriminant-property ()
  (interactive)
  (let ((start (save-excursion
                 (back-to-indentation)
                 (point)))
        (end (save-excursion
               (end-of-line)
               (point))))
    (replace-regexp "\\(\\w+?\\) \\(\\w+?\\) => \\(.+?\\);" "\\2: \\3;" nil start end)
    (schmo/downcase-first-char)
    (goto-char start)
    (forward-line 1)))
(provide 'workshop)

;;; workshop.el ends here
