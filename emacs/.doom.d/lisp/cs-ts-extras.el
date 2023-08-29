;;; cs-ts-extras.el --- Tree-sitter utils/commands for C# -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Jacob
;;
;; Author: Jacob
;; Maintainer: Jacob
;; Created: August 24, 2023
;; Modified: August 24, 2023
;; Version: 0.0.1
;; Keywords: convenience languages tools
;; Homepage: https://github.com/theschmocker/dotfiles
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  TODO
;;
;;; Code:

(require 'treesit)
(require 'subr-x)

(defconst cs-ts-extras--typescript-aggregate-buffer-name "*csharp-to-ts*")

(defvar cs-ts-extras-csharp-type-to-typescript-type-string-alist
  `(("int" . "number")
    ("byte" . "number")
    ("double" . "number")
    ("float" . "number")
    ("decimal" . "number")
    ("DateTime" . "string")
    ("String" . "string")
    ("short" . "number")
    ("bool" . "boolean")
    ("Guid" . "string")
    ("List" . ,#'cs-ts-extras-csharp-collection-type-to-typescript-array-string)
    ("ICollection" . ,#'cs-ts-extras-csharp-collection-type-to-typescript-array-string)
    ("IEnumerable" . ,#'cs-ts-extras-csharp-collection-type-to-typescript-array-string)
    ("HashSet" . ,#'cs-ts-extras-csharp-collection-type-to-typescript-array-string)
    ("Dictionary" . ,(apply-partially #'cs-ts-extras-csharp-map-generic-to-typescript-string "Record")))
  "Alist of C# types to equivalend TypeScript types.

The cdr of an entry may be one of
1. a string, in which case the type is replaced verbatim with the string
2. a function receiving a treesit-node and returning the TypeScript conversion.")

;;;###autoload
(defun cs-ts-extras-convert-to-typescript-at-point-dwim (&optional arg)
  "Convert the closest C# type declaration around point to TypeScript.

Adds the TypeScript version to a special buffer. Subsequent calls will add to
the same buffer unless ARG is non-nil."
  (interactive "P")
  (let ((type (cs-ts-extras-convertible-type-at-pos)))
    (cond
     ;; TODO: functions to pass in nodes to avoid additional query
     ((cs-ts-extras-class-declaration-p type) (cs-ts-extras-convert-class-at-point-to-typescript-interface arg))
     ((cs-ts-extras-enum-declaration-p type) (cs-ts-extras-convert-enum-at-point-to-typescript-enum arg))
     ((cs-ts-extras-interface-declaration-p type) (cs-ts-extras-convert-interface-at-point-to-typescript-interface arg))
     (t (error "Nothing to convert")))))

;;;###autoload
(defun cs-ts-extras-convert-class-at-point-to-typescript-interface (&optional arg)
  "Convert the closest C# class declaration around point to TypeScript.

Adds the TypeScript version to a special buffer. Subsequent calls will add to
the same buffer unless ARG is non-nil."
  (interactive "P")
  (when-let* ((class (cs-ts-extras-class-at-pos))
              (interface (cs-ts-extras-class-to-typescript-interface-string class)))
    (cs-ts-extras--show-typescript-in-buffer
     :name (format "*csharp-class-to-ts: %s*" (cs-ts-extras-class-name class))
     :contents interface
     :dedicated-buffer arg)))

;;;###autoload
(defun cs-ts-extras-convert-enum-at-point-to-typescript-enum (&optional arg)
  "Convert the closest C# enum definition around point to TypeScript.

Adds the TypeScript version to a special buffer. Subsequent calls will add to
the same buffer unless ARG is non-nil."
  (interactive "P")
  (when-let* ((enum (cs-ts-extras-enum-at-pos))
              (ts-enum (cs-ts-extras-enum-to-typescript-enum-string enum)))
    (cs-ts-extras--show-typescript-in-buffer
     :name (format "*csharp-enum-to-ts: %s*" (cs-ts-extras-enum-name enum))
     :contents ts-enum
     :dedicated-buffer arg)))

;;;###autoload
(defun cs-ts-extras-convert-interface-at-point-to-typescript-interface (&optional arg)
  "Convert the closest C# interface definition around point to TypeScript.

Adds the TypeScript version to a special buffer. Subsequent calls will add to
the same buffer unless ARG is non-nil."
  (interactive "P")
  (when-let* ((interface-node (cs-ts-extras-interface-at-pos))
              (interface (cs-ts-extras-interface-to-typescript-interface-string interface-node)))
    (cs-ts-extras--show-typescript-in-buffer
     :name (format "*csharp-interface-to-ts %s*" (cs-ts-extras-interface-name interface-node))
     :contents interface
     :dedicated-buffer arg)))

;;;###autoload
(defun cs-ts-extras-switch-to-csharp-to-ts-aggregate-buffer ()
  "Switch to the special buffer where TypeScript conversions get aggregated."
  (interactive)
  (switch-to-buffer-other-window cs-ts-extras--typescript-aggregate-buffer-name))

(defun cs-ts-extras--naive-pascal-case-to-camel-case (str)
  "Convert STR from PascalCase to camelCase.

Intended for use in C# -> TypeScript conversion property naming."
  (let (case-fold-search)
    (let ((with-capitalized-all-caps (replace-regexp-in-string "\\([A-Z]\\{2,\\}\\)\\(\\'\\|[a-z]\\)"
                                                               (lambda (m &rest _)
                                                                 (if (string-match-p "[a-z]\\'" m)
                                                                     (concat (capitalize (substring m 0 (- (length m) 2)))
                                                                             (substring m (- (length m) 2)))
                                                                   (capitalize m)))
                                                               str
                                                               t)))
      (replace-regexp-in-string "^\\([A-Z]\\)" (lambda (m)
                                                 (downcase m))
                                with-capitalized-all-caps t))))

(defun cs-ts-extras-class-at-pos (&optional pos)
  "Find the nearest class declaration up the tree from POS."
  (let ((pos (or pos (point))))
    (treesit-parent-until (treesit-node-at pos)
                          (lambda (node)
                            (string= "class_declaration" (treesit-node-type node))))))

(defun cs-ts-extras-interface-at-pos (&optional pos)
  "Find the nearest interface declaration up the tree from POS."
  (let ((pos (or pos (point))))
    (treesit-parent-until (treesit-node-at pos) #'cs-ts-extras-interface-declaration-p)))

(defun cs-ts-extras-enum-at-pos (&optional pos)
  "Find the nearest enum declaration up the tree from POS."
  (let ((pos (or pos (point))))
    (treesit-parent-until (treesit-node-at pos) #'cs-ts-extras-enum-declaration-p)))

(defun cs-ts-extras-convertible-type-at-pos (&optional pos)
  "Find the nearest TypeScript-convertible type declaration up the tree from POS."
  (let ((pos (or pos (point))))
    (treesit-parent-until (treesit-node-at pos)
                          (lambda (node)
                            (or (cs-ts-extras-class-declaration-p node)
                                (cs-ts-extras-interface-declaration-p node)
                                (cs-ts-extras-enum-declaration-p node))))))


(defun cs-ts-extras-class-declaration-p (node)
  "Return t if NODE is a class declaration."
  (string= "class_declaration" (treesit-node-type node)))

(defun cs-ts-extras-interface-declaration-p (node)
  "Return t if NODE is an interface declaration."
  (string= "interface_declaration" (treesit-node-type node)))

(defun cs-ts-extras-enum-declaration-p (node)
  "Return t if NODE is an enum declaration."
  (string= "enum_declaration" (treesit-node-type node)))

(defun cs-ts-extras-class-to-typescript-interface-string (class-decl-node)
  "Return CLASS-DECL-NODE as a string containing TypeScript."
  (concat
   (format "%s {\n" (cs-ts-extras-type-definition-info-to-typescript-interface-line
                     (cs-ts-extras-type-definition-info class-decl-node)))
   (string-join (mapcar (lambda (prop)
                          (let ((prop-name (cs-ts-extras--naive-pascal-case-to-camel-case
                                            (cs-ts-extras-property-declaration-name prop)))
                                (type-str (cs-ts-extras-csharp-type-to-typescript-string
                                           (cs-ts-extras-property-declaration-type-node prop))))
                            (format "\t%s: %s;\n" prop-name type-str)))
                        (cl-remove-if-not #'cs-ts-extras-public-property-declaration-p
                                          (cs-ts-extras-class-property-declarations class-decl-node))))
   "}"))

(defun cs-ts-extras-enum-to-typescript-enum-string (enum-node)
  "Return ENUM-NODE as a string containing TypeScript."
  (let ((name (cs-ts-extras-enum-name enum-node)))
    (concat
     (format "enum %s {\n" name)
     (string-join (mapcar (lambda (enum-mem)
                            (format "\t%s,\n" (treesit-node-text enum-mem t)))
                          (cs-ts-extras-enum-member-declarations enum-node)))
     "}")))

(defun cs-ts-extras-interface-to-typescript-interface-string (interface-decl-node)
  "Return INTERFACE-DECL-NODE as a string containing TypeScript."
  (concat
   (format "%s {\n" (cs-ts-extras-type-definition-info-to-typescript-interface-line
                     (cs-ts-extras-type-definition-info interface-decl-node)))
   (string-join (mapcar (lambda (prop)
                          (let ((prop-name (cs-ts-extras--naive-pascal-case-to-camel-case
                                            (cs-ts-extras-property-declaration-name prop)))
                                (type-str (cs-ts-extras-csharp-type-to-typescript-string
                                           (cs-ts-extras-property-declaration-type-node prop))))
                            (format "\t%s: %s;\n" prop-name type-str)))
                        (cs-ts-extras-interface-property-declarations interface-decl-node)))
   "}"))

(defun cs-ts-extras-class-name (class-decl-node)
  "Get the \"name\" field as a string from CLASS-DECL-NODE."
  (treesit-node-text (treesit-node-child-by-field-name class-decl-node "name") t))

(cl-defun cs-ts-extras--query (node query &key include-capture-names begin end)
  "Call `treesit-query-capture' with common defaults.

NODE: root treesit-node to query. The function returns nil if this is nil,
      unlike `treesit-query-capture' which signals an error

QUERY: same as QUERY argument to `treesit-query-capture'
BEGIN: same as BEG argument to `treesit-query-capture'
END: same as END argument to `treesit-query-capture'
INCLUDE-CAPTURE-NAMES: opposite of NODE-ONLY argument to `treesit-query-capture'"
  (when node
    (treesit-query-capture
     node
     query
     begin
     end
     (not include-capture-names))))

(defun cs-ts-extras-type-definition-info (type-decl-node)
  "Extract info from TYPE-DECL-NODE for conversion to TypeScript.

If TYPE-DECL-NODE isn't an interface or class declaration, return nil.

Returns a plist with keys:
:name - the string name of the class or interface
:type-params - a list of the class/interface's type param names as strings.
               maybe nil
:bases - a list of type nodes from which the class/interface inherits. maybe nil
:constraints - a plist of info about type parameter constraints:
               :target - name (string) of the target of the constraint.
               :types - nodes of types that the target extends.
                        constraints like new() are omitted.
                        maybe nil"
  (when (or (cs-ts-extras-class-declaration-p type-decl-node)
            (cs-ts-extras-interface-declaration-p type-decl-node))
    (let ((name (cs-ts-extras-class-name type-decl-node))
          (type-params (mapcar (lambda (tp)
                                 (treesit-node-text tp t))
                               (cs-ts-extras--query
                                (treesit-node-child-by-field-name type-decl-node "type_parameters")
                                '((type_parameter
                                   (identifier) @tp)))))
          (bases (cs-ts-extras--query
                  (treesit-node-child-by-field-name type-decl-node "bases")
                  '((base_list
                     (_) @b))))
          (constraints (when-let ((constraints-clause (car
                                                       (cl-remove-if-not (lambda (n)
                                                                           (treesit-node-eq (treesit-node-parent n) type-decl-node))
                                                                         (cs-ts-extras--query
                                                                          type-decl-node
                                                                          '((type_parameter_constraints_clause) @c))))))
                         (let ((target (treesit-node-child-by-field-name constraints-clause "target")))
                           (list :target (treesit-node-text target t)
                                 :types (cs-ts-extras--query
                                         constraints-clause
                                         '((type_constraint
                                            type: (_) @t))))))))
      (list :name name
            :type-params type-params
            :bases bases
            :constraints constraints))))

(defun cs-ts-extras-type-definition-info-to-typescript-interface-line (info)
  "Create the first line of a TypeScript type (minus {).

INFO is the return type of `cs-ts-extras-type-definition-info'."
  (when info
    (let ((generics
           (let* ((constraints (plist-get info :constraints))
                  (constraint-types (and constraints (plist-get constraints :types)))
                  (constraint-target (and constraints (plist-get constraints :target))))
             (mapcar (lambda (param-name)
                       (if (and constraint-types (string= param-name constraint-target))
                           (format "%s extends %s"
                                   param-name
                                   (string-join
                                    (mapcar #'cs-ts-extras-csharp-type-to-typescript-string constraint-types)
                                    " & "))
                         param-name))
                     (plist-get info :type-params))))
          (impls (mapcar #'cs-ts-extras-csharp-type-to-typescript-string
                         (plist-get info :bases)))
          (builder (list (format "interface %s" (plist-get info :name)))))
      (when generics
        (push (format "<%s>" (string-join generics ", ")) builder))
      (when impls
        (push (format " extends %s" (string-join impls ", ")) builder))
      (string-join (reverse builder)))))

(defalias 'cs-ts-extras-interface-name #'cs-ts-extras-class-name "Get the \"name\" field from the interface declaration node.")

(cl-defun cs-ts-extras--show-typescript-in-buffer (&key name contents (dedicated-buffer nil))
  "Show CONTENTS in a special TypeScript buffer.

NAME: name of the buffer to write to if DEDICATED-BUFFER is non-nil, otherwise
gets appended to `cs-ts-extras--typescript-aggregate-buffer-name'"
  (let ((buf (get-buffer-create (if dedicated-buffer name cs-ts-extras--typescript-aggregate-buffer-name))))
    (with-current-buffer buf
      (if dedicated-buffer
          (erase-buffer)
        (goto-char (point-max))
        (open-line 2))
      (save-excursion
        (cs-ts-extras--insert-flash contents))
      (display-buffer buf)
      (when-let ((win (get-buffer-window)))
        (set-window-point win (point)))
      ;; typescript-ts-mode adds itself to auto-mode-alist. I'm still using
      ;; typescript-mode for day-to-day ts dev, so I don't want that to
      ;; happen globally
      (make-local-variable 'auto-mode-alist)
      (when dedicated-buffer
        (setq buffer-undo-list nil)
        (set-buffer-modified-p nil))
      (typescript-ts-mode))))

(defvar cs-ts-extras-insert-flash-time 0.5
  "")

(defun cs-ts-extras--insert-flash (&rest args)
  (let ((start (point)))
    (apply #'insert args)
    (let* ((end (point))
           (o (make-overlay start end)))
      (overlay-put o 'face 'region)
      (run-with-timer
       cs-ts-extras-insert-flash-time
       nil
       (lambda (o)
         (delete-overlay o))
       o))))

(defun cs-ts-extras-class-property-declarations (class-decl-node)
  "Return CLASS-DECL-NODE's property declarations."
  (treesit-filter-child
   (treesit-node-child-by-field-name class-decl-node "body")
   #'cs-ts-extras-property-declaration-p))

(defalias 'cs-ts-extras-interface-property-declarations #'cs-ts-extras-class-property-declarations)

(defun cs-ts-extras-property-declaration-p (node)
  "Return t if NODE is a property declaration."
  (string= (treesit-node-type node) "property_declaration"))

(defun cs-ts-extras-node-modifier (node)
  "Return NODE's modifier as a string, if it has one."
  (when-let ((mod (car (treesit-filter-child node (lambda (child-node)
                                                    (string= "modifier"
                                                             (treesit-node-type child-node)))))))
    (treesit-node-text mod t)))

(defun cs-ts-extras-public-property-declaration-p (node)
  "Return t if NODE is a property declaration with a public modifier."
  (and (cs-ts-extras-property-declaration-p node)
       (string= (cs-ts-extras-node-modifier node) "public")))

(defun cs-ts-extras-property-declaration-name (node)
  "Return the name of property declaration NODE."
  (when (cs-ts-extras-property-declaration-p node)
    (treesit-node-text
     (treesit-node-child-by-field-name node "name")
     t)))

(defun cs-ts-extras-property-declaration-type-node (decl)
  "Return the type node of DECL."
  (treesit-node-child-by-field-name decl "type"))

(defun cs-ts-extras-type-nullable-p (type)
  "Return t if TYPE is a nullable type node."
  (string= "nullable_type" (treesit-node-type type)))

(defun cs-ts-extras-type-nullable-unwrap (type)
  "Return the inner type of the nullable_type node TYPE."
  (when (cs-ts-extras-type-nullable-p type)
    (treesit-node-child type 0)))

(defun cs-ts-extras-type-generic-p (type)
  "Return t if TYPE is a generic_name node.

generic_name being a generic type annotation."
  (string= "generic_name" (treesit-node-type type)))

(defun cs-ts-extras-type-name (type)
  "Return the name of TYPE as a string.

Normalizes nullable and generic types to return the name only."
  (cond ((cs-ts-extras-type-nullable-p type)
         (cs-ts-extras-type-name (cs-ts-extras-type-nullable-unwrap type)))
        ((cs-ts-extras-type-generic-p type) (treesit-node-text (treesit-node-child type 0) t))
        (t (treesit-node-text type t))))

(defun cs-ts-extras-generic-arguments (gen-type)
  "Return the generic type argument nodes if GEN-TYPE."
  (let ((arg-list (car (treesit-filter-child gen-type (lambda (node)
                                                        (string= "type_argument_list" (treesit-node-type node)))))))
    (treesit-node-children arg-list t)))

(defun cs-ts-extras-csharp-collection-type-to-typescript-array-string (type-node)
  "Convert a collection TYPE-NODE into a TypeScript array.

e.g. List<Something> node becomes \"Something[]\".
If the type argument is nullable: List<int?> node becomes (number | null)[]

Applies a transformation like the above to any generic type -- only the first
type argument is used."
  (let ((type (car (cs-ts-extras-generic-arguments type-node))))
    (format (if (cs-ts-extras-type-nullable-p type)
                "(%s)[]"
              "%s[]")
            (cs-ts-extras-csharp-type-to-typescript-string type))))

(defun cs-ts-extras-csharp-type-to-typescript-string (type-node)
  "Convert TYPE-NODE into a string of TypeScript recursively.

Handles nullable types and generic types. Special handlers are defined in
`cs-ts-extras-csharp-type-to-typescript-type-string-alist'"
  (if (cs-ts-extras-type-nullable-p type-node)
      (format "%s | null" (cs-ts-extras-csharp-type-to-typescript-string (cs-ts-extras-type-nullable-unwrap type-node)))
    (let ((name (cs-ts-extras-type-name type-node)))
      (let ((mapping (alist-get name cs-ts-extras-csharp-type-to-typescript-type-string-alist nil nil #'string=)))
        (cond ((stringp mapping) mapping)
              ((functionp mapping) (funcall mapping type-node))
              ((cs-ts-extras-type-generic-p type-node) (cs-ts-extras-csharp-map-generic-to-typescript-string nil type-node))
              ;; TODO ((cs-ts-extras-type-tuple-p type-node) (cs-ts-extras-csharp-map-tuple-to-typescript-string type-node))
              (t (treesit-node-text type-node t)))))))

(defun cs-ts-extras-csharp-map-generic-to-typescript-string (name-rep type)
  "Convert generic TYPE to a string of TypeScript recursively.

If NAME-REP is non-nil, then the name of TYPE is replaced. Otherwise, it's kept for the TypeScript version.

Ex: (cs-ts-extras-csharp-map-generic-to-typescript-string nil <node of Dictionary<string, int>>) returns \"Dictionary<string, number>\"
Ex: (cs-ts-extras-csharp-map-generic-to-typescript-string \"Record\" <node of Dictionary<string, int>>) returns \"Record<string, number>\""
  (let ((name (or name-rep (cs-ts-extras-type-name type))))
    (format "%s<%s>" name (string-join (mapcar #'cs-ts-extras-csharp-type-to-typescript-string
                                               (cs-ts-extras-generic-arguments type))
                                       ", "))))



(defun cs-ts-extras-enum-name (enum-node)
  "Return the name of enum ENUM-NODE."
  (treesit-node-text (treesit-node-child-by-field-name enum-node "name") t))

(defun cs-ts-extras-enum-member-declarations (enum-node)
  "Return the members of enum ENUM-NODE."
  (treesit-query-capture
   enum-node
   '(((enum_member_declaration) @d))
   nil nil t))

(provide 'cs-ts-extras)

;;; cs-ts-extras.el ends here
