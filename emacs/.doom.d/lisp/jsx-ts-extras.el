;;; jsx-ts-extras.el --- Tree-sitter extras for JSX/TSX -*- lexical-binding: t; -*-

(require 'cl-lib)

;; Auto close tags

;;;###autoload
(define-minor-mode jsx-ts-extras-auto-close-mode
  ""
  :global nil
  (when (not (cl-some #'derived-mode-p '(tsx-ts-mode js-ts-mode)))
    (error "%s does nothing outside of treesitter modes supporting JSX" 'jsx-ts-extras-auto-close-mode))
  (if jsx-ts-extras-auto-close-mode
      (add-hook 'post-command-hook #'jsx-ts-extras--auto-close-tag-post-command-h nil t)
    (remove-hook 'post-command-hook #'jsx-ts-extras--auto-close-tag-post-command-h t)))

(defun jsx-ts-extras--auto-close-tag-post-command-h ()
  "Run `jsx-ts-extras--close-tag' after \"</\" inserted in `post-command-hook'."
  (when (memq this-command
              (list 'self-insert-command
                    (key-binding [remap self-insert-command])))
    (jsx-ts-extras--close-tag)))

(defun jsx-ts-extras--close-tag ()
  (when (and (looking-back "</" (- (point) 2))
             (string= "</" (treesit-node-type (treesit-node-at (point)))))
    (when-let* ((open-tag (jsx-ts-extras--get-open-tag-of-element-at-point))
                (tag-name (jsx-ts-extras--get-open-tag-name open-tag))
                (open-tag-end-pos (treesit-node-end open-tag))
                (before-close-tag (- (point) 2)))
      (insert tag-name)
      (insert ?\>)
      (when (eql open-tag-end-pos before-close-tag)
        (goto-char open-tag-end-pos))
      (indent-according-to-mode))))

(defun jsx-ts-extras--get-open-tag-name (open-tag)
  (if (string= "<>" (treesit-node-text open-tag))
      ""
    (treesit-node-text
     (treesit-node-child-by-field-name open-tag "name")
     t)))

;; Auto open tags

;;;###autoload
(define-minor-mode jsx-ts-extras-auto-open-tag-mode
  ""
  :global nil
  (when (not (cl-some #'derived-mode-p '(tsx-ts-mode js-ts-mode)))
    (error "%s does nothing outside of treesitter modes supporting JSX" 'jsx-ts-extras-auto-close-mode))
  (if jsx-ts-extras-auto-open-tag-mode
      (add-hook 'pre-command-hook #'jsx-ts-extras--auto-open-tag-pre-command-h nil t)
    (remove-hook 'pre-command-hook #'jsx-ts-extras--auto-open-tag-pre-command-h t)))

(defun jsx-ts-extras--auto-open-tag-pre-command-h ()
  "Run `jsx-ts-extras--open-tag' before `newline-and-indent' in `pre-command-hook'."
  (when (memq this-command
              (list 'newline-and-indent
                    (key-binding [remap newline-and-indent])))
    (jsx-ts-extras--open-tag)))

(defun jsx-ts-extras--open-tag ()
  "Insert an extra newline and indent when point is between open/close tags.

When run before `newline-and-indent' in `pre-command-hook', it behaves like


<div>|</div>

becomes

<div>
  |
</div>"
  (when-let* ((open-tag (jsx-ts-extras--get-open-tag-of-element-at-point))
              (close-tag (jsx-ts-extras--get-close-tag-of-element-at-point)))
    (when (jsx-ts-extras--pos-directly-between-tags-p open-tag close-tag (point))
      (save-excursion
        (newline-and-indent)))))

(defun jsx-ts-extras--pos-directly-between-tags-p (open-tag close-tag pos)
  "Return t if POS is directly between OPEN-TAG and CLOSE-TAG.

For example: <tag>|</tag>, but not <tag> |</tag>."
  (and (eql pos (treesit-node-end open-tag))
       (eql pos (treesit-node-start close-tag))))

;; Movement Commands

;;;###autoload
(defun jsx-ts-extras-element-match ()
  ""
  (interactive)
  (when-let* ((open-tag (jsx-ts-extras--get-open-tag-of-element-at-point))
              (close-tag (jsx-ts-extras--get-close-tag-of-element-at-point)))
    (if (jsx-ts-extra--point-in-node-p open-tag)
        (goto-char (treesit-node-start close-tag))
      (goto-char (treesit-node-start open-tag)))))

(defun jsx-ts-extra--point-in-node-p (node)
  (<= (treesit-node-start node) (point) (treesit-node-end node)))

;; Shared Helpers

(defun jsx-ts-extras--get-node-at-point (node-type)
  ""
  (treesit-parent-until (treesit-node-at (point))
                        (lambda (n)
                          (string= node-type
                                   (treesit-node-type n)))))

(defun jsx-ts-extras--get-element-at-point ()
  ""
  (jsx-ts-extras--get-node-at-point "jsx_element"))

(defun jsx-ts-extras--get-self-closing-element-at-point ()
  ""
  (jsx-ts-extras--get-node-at-point "jsx_self_closing_element"))

(defun jsx-ts-extras--get-open-tag-of-element-at-point ()
  (when-let ((element (jsx-ts-extras--get-element-at-point)))
    (treesit-node-child-by-field-name element "open_tag")))

(defun jsx-ts-extras--get-close-tag-of-element-at-point ()
  (when-let ((element (jsx-ts-extras--get-element-at-point)))
    (treesit-node-child-by-field-name element "close_tag")))
