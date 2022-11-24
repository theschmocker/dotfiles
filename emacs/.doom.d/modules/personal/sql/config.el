;;; personal/sql/config.el -*- lexical-binding: t; -*-

(map!
 :mode sql-mode
 :localleader
 (:prefix ("e" . "eval")
          "b" #'sql-send-buffer
          "d" #'sql-send-paragraph
          "p" #'sql-send-paragraph
          "n" #'sql-send-line-and-next
          "e" #'sql-send-line-and-next
          "r" #'sql-send-region)
 (:prefix ("l" . "list")
          "t" #'sql-list-table
          "a" #'sql-list-all)
 :desc "Connect to SQL Process" "'" #'+sql/connect)

(defvar +sql/products '((mysql . sql-mysql)
                        (ms . sql-ms)
                        (db2 . sql-db2)
                        (solid . sql-solid)
                        (ingres . sql-ingres)
                        (linter . sql-linter)
                        (oracle . sql-oracle)
                        (sqlite . sql-sqlite)
                        (sybase . sql-sybase)
                        (mariadb . sql-mariadb)
                        (vertica . sql-vertica)
                        (informix . sql-informix)
                        (postgres . sql-postgres)
                        (interbase . sql-interbase)))
