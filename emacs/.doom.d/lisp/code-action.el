;;; code-action.el -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar code-action-alist nil
  "An alist of code-actions, keyed by id")

(cl-defun code-action-define (&key id name (mode nil) (predicate #'always) action)
  (when (assq id code-action-alist)
    (message "Redefining code action with id %s" id))
  (setf (alist-get id code-action-alist)
        (list :id id
              :name name
              :mode mode
              :predicate predicate
              :action action)))

(autoload 'treesit-language-at "treesit")
(defun code-action-treesit-language-predicate (&rest lang-symbols)
  (lambda ()
    (let ((lang-at-point (treesit-language-at (point))))
      (memq lang-at-point lang-symbols))))

;; (code-action-define
;;  :id 'typescript-if-else-to-ternary-return
;;  :name "Convert to ternary"
;;  :predicate (lambda ()
;;               (and (funcall (code-action-treesit-language-predicate 'typescript))
;;                    (schmo/ts-if-else-statement-at-point)))
;;  :action #'schmo/ts-convert-if-else-at-point-to-ternary)

;; (code-action-define
;;  :id 'typescript-ternary-return-to-statements
;;  :name "Convert ternary return expression to statements"
;;  :predicate (lambda ()
;;               (and (funcall (code-action-treesit-language-predicate 'typescript))
;;                    (schmo/ternary-return-statement-at-point-p)))
;;  :action #'schmo/ts-convert-ternary-return-expression-at-point-to-statements)

;; (code-action-define
;;  :id 'elisp-test
;;  :name "elisp test"
;;  :mode 'emacs-lisp-mode
;;  :predicate (lambda () t)
;;  :action (lambda ()
;;            (message "Hooray!")))

;; (code-action-define
;;  :id 'elisp-test-2
;;  :name "Another elisp test"
;;  :mode 'emacs-lisp-mode
;;  :predicate (lambda () t)
;;  :action (lambda (pt)
;;            (interactive "d")
;;            (message "Point: %d" pt)))

(defun code-action--mode-symbol-active-p (mode-sym)
  (or (ignore-errors (symbol-value mode-sym))
      (derived-mode-p mode-sym)))

(defun code-action--get-available-actions ()
  (cl-remove-if-not (lambda (action-cell)
                      (let* ((def (cdr action-cell))
                             (mode (plist-get def :mode))
                             (mode-match-p (cond ((null mode) t)
                                                 ((symbolp mode) (code-action--mode-symbol-active-p mode))
                                                 ((proper-list-p mode) (cl-some #'code-action--mode-symbol-active-p mode))))
                             (predicate (plist-get def :predicate)))
                        (and mode-match-p
                             (or (not (functionp predicate))
                                 (funcall predicate)))))
                    code-action-alist))

(cl-defun code-action--completing-read (&optional (code-actions (code-action--get-available-actions)))
  (when (null code-actions)
    (error "No available code actions"))
  (when-let ((completion
              (completing-read
               "Code Action: "
               (lambda (s pred action)
                 (cond
                  ((eq 'metadata action) `(metadata (affixation-function . ,(lambda (c)
                                                                              (mapcar (lambda (item)
                                                                                        (list "" (plist-get (alist-get (intern item) code-actions) :name) ""))
                                                                                      c)))))
                  (t (all-completions s code-actions pred)))))))
    (alist-get (intern completion) code-actions)))

(defun code-action-execute (code-action)
  (interactive (list (code-action--completing-read)))
  (let ((action (plist-get code-action :action)))
    (if (commandp action)
        (call-interactively action)
      (funcall action))))

(provide 'code-action)
