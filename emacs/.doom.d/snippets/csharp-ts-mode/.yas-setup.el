;;; -*- lexical-binding: t; -*-

(require 'treesit)
(require 'cs-ts-extras)

(defun schmo/snippet/csharp-union-from-enum (_)
  (let* ((pos (1+ yas-snippet-beg))
         (enum (cs-ts-extras-enum-at-pos pos)))
    (cs-ts-extras-create-simulated-union-from-enum enum)))
