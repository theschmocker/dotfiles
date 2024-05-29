;;; ../dotfiles/emacs/.doom.d/lisp/llama-pilot.el -*- lexical-binding: t; -*-
;;;  -*- mode: emacs-lisp; lexical-binding: t; -*-

(defvar llama-pilot-url "http://localhost:8080")

(defun llama-pilot--deepseek-prompt (prefix suffix)
  (format "<｜fim▁begin｜>%s<｜fim▁hole｜>%s<｜fim▁end｜>" prefix suffix))

(defun llama-pilot--codeqwen-prompt (prefix suffix)
  (format "<fim_prefix>%s<fim_suffix>%s<fim_middle>" prefix suffix))

(setq llama-pilot-prompt-fn 'llama-pilot--codeqwen-prompt)

(defvar llama-pilot-prompt-fn 'llama-pilot--deepseek-prompt)

(defconst llama-pilot-stop-words--codeqwen (list "<fim_prefix>" "<fim_suffix>" "<fim_middle>"))
(defconst llama-pilot-stop-words--deepseek (list "<｜fim▁begin｜>" "<｜fim▁hole｜>" "<｜fim▁end｜>"))

(defvar llama-pilot-stop-words llama-pilot-stop-words--codeqwen)

(defvar llama-pilot-process-completion #'identity)

(defun llama-pilot-process-completion--strip-single-leading-space (completion)
  (replace-regexp-in-string "^[[:space:]]" "" completion))

(llama-pilot-process-completion--strip-single-leading-space "  test")

(defvar llama-pilot-complete--current-request-buffer nil)
(defvar llama-pilot-complete--current-request nil)

(defun llama-pilot-complete ()
  ""
  (interactive)
  (let* ((prefix (buffer-substring-no-properties (point-min) (point)))
         (suffix (buffer-substring-no-properties (point) (point-max)))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (encode-coding-string (json-encode (list
                                                               :prompt (funcall llama-pilot-prompt-fn
                                                                                (replace-regexp-in-string (regexp-opt llama-pilot-stop-words) "STOPWORD" prefix)
                                                                                (replace-regexp-in-string (regexp-opt llama-pilot-stop-words) "STOPWORD" suffix))
                                                               ;; :temperature 0.5
                                                               ;; :repeat_penality 1.18
                                                               :n_predict 200
                                                               ;; :stop llama-pilot-stop-words
                                                               )) 'utf-8)))

    (print (concat prefix suffix))
    (setq llama-pilot-complete--current-request-buffer
          (url-retrieve "http://localhost:8080/completions"
                        (lambda (status buf point)
                          (setq llama-pilot-complete--current-request-buffer nil)
                          (goto-char url-http-end-of-headers)
                          ;; (display-buffer (current-buffer))
                          (let ((res (json-read)))
                            (with-current-buffer buf
                              (goto-char point)
                              (print (type-of (alist-get 'content res)))
                              (insert (funcall llama-pilot-process-completion (alist-get 'content res))))))
                        (list (current-buffer)
                              (point))
                        t
                        t))))

(require 'request)

(defun llama-pilot-abort ()
  (interactive)
  (when llama-pilot-complete--current-request-buffer
    (let ((proc (get-buffer-process llama-pilot-complete--current-request-buffer)))
      (when (process-live-p proc)
        (delete-process proc)
        (message "Request cancelled"))
      (print proc))))
