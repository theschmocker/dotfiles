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
(require 'compat)

(defvar-keymap string-edit-mode-map
  "C-c C-c" #'string-edit-mode-save
  "C-c C-k" #'string-edit-mode-quit)

(defvar-local string-edit-mode--current-target nil
  "The target data for the string the current string-edit buffer is editing.")
(put 'string-edit-mode--current-target 'permanent-local t) ;; make sure the target data sticks around when the major mode changes

(define-minor-mode string-edit-mode
  "Minor mode for buffers created by `edit-string-at-point'."
  :interactive nil
  :keymap string-edit-mode-map
  (if string-edit-mode
      (let* ((buf (current-buffer))
             (cleanup (lambda ()
                        (when (eq buf (current-buffer))
                          (string-edit-mode--cleanup-target-buffer (string-edit-mode-target-buffer string-edit-mode--current-target))))))
        ;; clean up target buffer when the string-edit buffer is killed
        (add-hook 'kill-buffer-hook cleanup nil t))
    ;; clean up target buffer when string-edit-mode is disabled in the string-edit buffer
    (string-edit-mode--cleanup-target-buffer (string-edit-mode-target-buffer string-edit-mode--current-target))))
(put 'string-edit-mode 'permanent-local t) ;; prevent mode from being disabled when changing major modes

(define-minor-mode string-edit-target-mode
  "Minor mode for buffers which have an associated string-edit buffer."
  :interactive nil
  (if string-edit-target-mode
      (setq buffer-read-only t)
    ;; should I kill the edit buffers?
    (setq buffer-read-only nil)))

;; Not a huge fan of this. Other options:
;; - Transient menu for toggling various options. multiline, escape chars, escaping escape chars, etc.
;; - set up an alist of modes -> these functions
(defun string-edit-mode-reformat-string-default (target new-string)
  (thread-last new-string
               (string-replace "\\" "\\\\")
               (string-replace (string-edit-mode-target-delimeter target)
                               (concat "\\" (string-edit-mode-target-delimeter target)))))

(defvar string-edit-mode-reformat-string-function 'string-edit-mode-reformat-string-default
  "Determines how the contents of the string edit buffer should be formatted
when reinserted into the target buffer. Use this to escape string delimeters,
newlines, and other characters which need to be escaped.")

(cl-defstruct (string-edit-mode-target)
  "Information about a string & buffer used by string-edit-mode"
  delimeter
  inner-bounds
  contents
  buffer)

(cl-defun string-edit-mode--make-marker (&key position buffer)
  "Create a marker with the given POSITION and BUFFER."
  (let ((m (make-marker)))
    (set-marker m position buffer)))

(defun string-edit-mode--get-target-data-at-point (&optional point)
  ""
  (let ((point (or point (point))))
    (save-excursion
      (goto-char point)
      (when-let ((bounds (thing-at-point-bounds-of-string-at-point)))
        (cl-destructuring-bind (start . end) bounds
          (let* ((inner-start (1+ start))
                 (inner-end (1- end))
                 (m-start (string-edit-mode--make-marker :position inner-start))
                 (m-end (string-edit-mode--make-marker :position inner-end)))
            (make-string-edit-mode-target
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
            (bounds (string-edit-mode-target-inner-bounds data))
            (delim (string-edit-mode-target-delimeter data))
            (contents (string-edit-mode-target-contents data)))
      (let ((edit-buf (generate-new-buffer (edit-string-buffer-name))))
        (with-current-buffer edit-buf
          (when (= (point-min) (point-max))
            (insert (string-replace (concat "\\" delim) delim contents))
            (setq buffer-undo-list nil)
            (set-buffer-modified-p nil))
          ;; Moves point to same location in string in the string-edit buffer
          (let ((string-relative-point (- point (car bounds) 1))) ;; TODO account for replacements
            (goto-char string-relative-point))
          (string-edit-mode t)
          (setq-local string-edit-mode--current-target data))

        (string-edit-target-mode)
        (switch-to-buffer-other-window edit-buf))
    (error "Point not inside of a string")))

(defun string-edit-mode-save ()
  "Commit the changed string to the target buffer.
Kills the string-edit buffer and closes its window if one was created."
  (interactive)

  (unless (bound-and-true-p string-edit-mode)
    (error "Not in a string-edit-mode buffer"))

  (unless (and (bound-and-true-p string-edit-mode--current-target)
               (buffer-live-p (string-edit-mode-target-buffer string-edit-mode--current-target)))
    (error "Target buffer doesn't exist"))

  (let* ((value (buffer-substring-no-properties (point-min) (point-max)))
         (target string-edit-mode--current-target)
         (reformatted (funcall string-edit-mode-reformat-string-function
                               target
                               value))
         (target-buffer (string-edit-mode-target-buffer target))
         (bounds (string-edit-mode-target-inner-bounds target)))
    (cl-destructuring-bind (start . end) bounds
      (with-current-buffer target-buffer
        (dlet ((inhibit-read-only t))
          (replace-region-contents start end
                                   (lambda (&rest _)
                                     reformatted)))))

    (set-buffer-modified-p nil)
    (string-edit-mode-quit)))

(defun string-edit-mode-quit ()
  "Kill the current string-edit buffer and close its window if one was created."
  (interactive)
  (unless (bound-and-true-p string-edit-mode)
    (error "Not in a string-edit-mode buffer"))
  (let ((delete-window-p (and (not (window-prev-buffers)) (not (one-window-p))))
        (target-buffer (string-edit-mode-target-buffer string-edit-mode--current-target))
        (killedp (if (buffer-modified-p)
                     (kill-buffer-ask (current-buffer))
                   (kill-buffer)
                   t)))
    (when killedp
      (when delete-window-p
        (delete-window))
      (string-edit-mode--switch-to-target-buffer target-buffer))))

(defun string-edit-mode--switch-to-target-buffer (target-buffer)
  ""
  (if-let (win (get-buffer-window target-buffer))
      (select-window win)
    (switch-to-buffer target-buffer)))

(defun string-edit-mode--cleanup-target-buffer (target-buffer)
  ""
  (with-current-buffer target-buffer
    (string-edit-target-mode -1)))

(provide 'string-edit)
;;; string-edit.el ends here
