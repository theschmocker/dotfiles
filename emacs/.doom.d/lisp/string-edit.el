;;; string-edit.el -- Edit the string at point in another buffer -*- lexical-binding: t -*-
;;; Commentary:
;;; Inspired by https://github.com/magnars/string-edit.el.
;;;
;;; I wrote this 1) because I wanted to write some elisp, and it seemed like a
;;; cool and doable thing and 2) I wanted to add more customization options
;;;
;;; Code:

(require 'thingatpt)
(require 'subr-x)
(require 'cl-lib)

(defvar-keymap string-edit-mode-map
  "C-c C-c" #'string-edit-mode-save
  "C-c C-k" #'string-edit-mode-quit)

(defvar-local string-edit-mode--target nil
  "The `string-edit-mode--target-data' for the string which the current
  string-edit buffer is editing")
(put 'string-edit-mode--target 'permanent-local t) ;; make sure the target data sticks around when the major mode changes

(define-minor-mode string-edit-mode
  "Minor mode for buffers created by `edit-string-at-point'."
  :interactive nil
  :keymap string-edit-mode-map)
(put 'string-edit-mode 'permanent-local t) ;; prevent mode from being disabled when changing major modes

(define-minor-mode string-edit-target-mode
  "Minor mode for buffers which have an associated string-edit buffer."
  :interactive nil
  (if string-edit-target-mode
      (setq buffer-read-only t)
    ;; should I kill the edit buffers?
    (setq buffer-read-only nil)))

(cl-defstruct (string-edit-mode--target-data)
  "Information about a string & buffer used by string-edit-mode"
  delimeter
  inner-bounds
  contents
  buffer)

(cl-defmacro string-edit-mode--make-marker (&key position buffer)
  (let ((m (gensym))
        (p (gensym))
        (b (gensym)))
    `(let ((,m (make-marker))
           (,p ,position)
           (,b ,buffer))
       (set-marker ,m ,p ,b))))

(defun string-edit-mode--get-target-data-at-point (&optional point)
  (let ((point (or point (point))))
    (save-excursion
      (goto-char point)
      (when-let ((bounds (thing-at-point-bounds-of-string-at-point)))
        (cl-destructuring-bind (start . end) bounds
          (let* ((inner-start (1+ start))
                 (inner-end (1- end))
                 (m-start (string-edit-mode--make-marker :position inner-start))
                 (m-end (string-edit-mode--make-marker :position inner-end)))
            (make-string-edit-mode--target-data
             :delimeter (string (char-after start))
             :inner-bounds (cons m-start m-end)
             :contents (buffer-substring-no-properties m-start m-end)
             :buffer (current-buffer))))))))

(defun edit-string-buffer-name ()
  "Create the name for a string-edit buffer targeting the current buffer."
  (concat "*string-edit: " (buffer-name (current-buffer)) "*"))

;;;###autoload
(defun edit-string-at-point (point)
  "Open a new buffer to edit the string at POINT."
  (interactive "d")

  (when (bound-and-true-p string-edit-target-mode)
    (when-let ((existing (get-buffer (edit-string-buffer-name))))
      (switch-to-buffer-other-window existing))
    (error "Already editing string in this buffer"))

  (if-let* ((data (string-edit-mode--get-target-data-at-point))
            (bounds (string-edit-mode--target-data-inner-bounds data))
            (delim (string-edit-mode--target-data-delimeter data))
            (contents (string-edit-mode--target-data-contents data)))
      (let ((edit-buf (generate-new-buffer (edit-string-buffer-name))))
        (with-current-buffer edit-buf
          (when (= (point-min) (point-max))
            (insert (string-replace (concat "\\" delim) delim contents))
            (set-buffer-modified-p nil))
          ;; Moves point to same location in string in the string-edit buffer
          (let ((string-relative-point (- point (car bounds) 1))) ;; TODO account for replacements
            (goto-char string-relative-point))
          (string-edit-mode t)
          (setq-local string-edit-mode--target data))

        (string-edit-target-mode)
        (switch-to-buffer-other-window edit-buf))
    (error "Point not inside of a string")))

(defun string-edit-mode-save ()
  "Commit the changes to the string to the target buffer, kill the string-edit
buffer and close its window if one was created."
  (interactive)

  (unless (bound-and-true-p string-edit-mode)
    (error "Not in a string-edit-mode buffer"))

  (unless (and (bound-and-true-p string-edit-mode--target)
               (buffer-live-p (string-edit-mode--target-data-buffer string-edit-mode--target)))
    (error "Target buffer doesn't exist"))

  (let ((value (buffer-substring-no-properties (point-min) (point-max)))
        (delim (string-edit-mode--target-data-delimeter string-edit-mode--target))
        (target-buffer (string-edit-mode--target-data-buffer string-edit-mode--target))
        (bounds (string-edit-mode--target-data-inner-bounds string-edit-mode--target)))
    (cl-destructuring-bind (start . end) bounds
      (with-current-buffer target-buffer
        (dlet ((inhibit-read-only t))
          (replace-region-contents start end
                                   (lambda (&rest _)
                                     (string-replace delim
                                                     (concat "\\" delim)
                                                     value))))
        (string-edit-target-mode -1)))

    (when (and (not (window-prev-buffers)) (not (one-window-p)))
      (delete-window))

    (string-edit-mode--switch-to-target-buffer target-buffer))

  (kill-buffer (edit-string-buffer-name)))

(defun string-edit-mode-quit ()
  "Kill the current string-edit buffer and close its window if one was created."
  (interactive)
  (unless (bound-and-true-p string-edit-mode)
    (error "Not in a string-edit-mode buffer"))
  (let ((delete-window-p (and (not (window-prev-buffers)) (not (one-window-p))))
        (target-buffer (string-edit-mode--target-data-buffer string-edit-mode--target))
        (killedp (if (buffer-modified-p)
                     (kill-buffer-ask (current-buffer))
                   (kill-buffer)
                   t)))
    (when killedp
      (when delete-window-p
        (delete-window))
      (with-current-buffer target-buffer
        (string-edit-target-mode -1))
      (string-edit-mode--switch-to-target-buffer target-buffer))))

(defun string-edit-mode--switch-to-target-buffer (target-buffer)
  (if-let (win (get-buffer-window target-buffer))
      (select-window win)
    (switch-to-buffer target-buffer)))

(provide 'string-edit)
;;; string-edit.el ends here
