;;; personal/sql/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +sql/connect (&optional product)
  (interactive)
  (if-let (sqli-buf (and sql-buffer (get-buffer sql-buffer)))
      (progn
        (sql-show-sqli-buffer)
        (switch-to-buffer-other-window sqli-buf))
    (let* ((prod (or product
                     (intern (completing-read "Choose a database type: " (mapcar #'car +sql/products)
                                              #'always
                                              t))))
           (entry (assoc prod +sql/products)))
      (if (not entry)
          (user-error "Invalid connection type: %s" prod)
        (progn
          (let ((buf (current-buffer)))
            (funcall (cdr entry))
            (with-current-buffer buf
              (sql-set-product prod))))))))
