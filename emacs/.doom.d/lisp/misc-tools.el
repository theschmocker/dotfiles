;;; misc-tools.el -*- lexical-binding: t; -*-

(require 'cl-lib)

(defun schmo/get-net-web-app-config-files ()
  (cl-remove-if-not (lambda (p)
                      (let ((case-fold-search nil))
                        (string-match-p "Web Apps/[^/]+?/Web.config\\'" p)))
                    (project-files (project-current))))

(defun schmo/net-web-app-config-completing-read (&optional files)
  (completing-read "Choose Web.config: " (or files (schmo/get-net-web-app-config-files))))

(defun schmo/get-net-web-config-values (config-path)
  (let ((buf (find-file-noselect config-path)))
    (when buf
      (with-current-buffer buf
        (save-excursion
          (goto-char (point-min))
          (let (config-alist)
            (while (ignore-errors (re-search-forward "<add key=\"\\(.+?\\)\" value=\"\\(.+?\\)\""))
              (push (cons (match-string-no-properties 1)
                          (list (match-string-no-properties 2)
                                (match-beginning 2)
                                (match-end 2)))
                    config-alist))
            (nreverse config-alist)))))))

(defun schmo/update-web-config-value ()
  (interactive)
  (let* ((config-paths (schmo/get-net-web-app-config-files))
         (selected (if (member (buffer-file-name) config-paths)
                       (buffer-file-name)
                     (schmo/net-web-app-config-completing-read config-paths))))
    (when selected
      (let ((config-alist (schmo/get-net-web-config-values selected)))
        (print
         (pcase-let ((`(,cur-val ,start ,end)
                      (alist-get (completing-read "Config value: " config-alist nil t)
                                 config-alist
                                 nil
                                 nil
                                 #'equal)))
           (save-excursion
             (with-current-buffer (find-file-noselect selected)
               (atomic-change-group
                 (goto-char start)
                 (delete-region start end)
                 (insert (read-string "New Value: " cur-val)))
               (save-buffer)))))))))

(defun schmo/net-web-app-toggle-use-dev-server-assets ()
  (interactive)
  (let* ((config-paths (schmo/get-net-web-app-config-files))
         (selected (if (member (buffer-file-name) config-paths)
                       (current-buffer)
                     (find-file-noselect (schmo/net-web-app-config-completing-read config-paths)))))
    (when selected
      (with-current-buffer selected
        (save-excursion
          (goto-char (point-min))
          (let (new-value)
            (while (ignore-errors
                     (re-search-forward "<add key=\"\\(Debug\\|UseWebpackDevServer\\)\" value=\"\\(true\\|false\\)\""))
              (let ((start (match-beginning 2))
                    (end (match-end 2)))
                (when (null new-value)
                  (setq new-value
                        (if (equal "true" (match-string 2))
                            "false"
                          "true")))
                (goto-char start)
                (delete-region start end)
                (insert new-value)))))
        (save-buffer)))))

(defun schmo/closest-csproj-file ()
  (let (csproj-files)
    (locate-dominating-file (buffer-file-name)
                            (lambda (d)
                              (setq csproj-files
                                    (directory-files d t ".csproj\\'"))))
    (car csproj-files)))

(defun schmo/try-find-alt-type ()
  (interactive)
  (if-let ((sym (thing-at-point 'symbol)))
      (let ((search-type
             (cond ((string-match-p "\\.\\(vue\\|ts\\)\\'" (buffer-file-name)) 'cs)
                   ((string-match-p "\\.cs\\'" (buffer-file-name)) 'client))))
        (+vertico/project-search nil (format "%s\\ %s\\> -- -g=*.{%s}"
                                             (if (eq search-type 'cs)
                                                 "\\(class\\|interface\\|enum\\)"
                                               "\\(class\\|interface\\|enum\\|type\\)")
                                             (replace-regexp-in-string "^\\(I?\\)\\([A-Z]\\).*"
                                                                       "I?"
                                                                       sym
                                                                       t
                                                                       t
                                                                       1)
                                             (if (eq search-type 'cs)
                                                 "cs"
                                               "vue,ts"))))
    (error "Nothing at point")))

(provide 'misc-tools)
