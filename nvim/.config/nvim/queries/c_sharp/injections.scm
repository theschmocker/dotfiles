(object_creation_expression
  type: (identifier) @_class-name
  arguments: (argument_list
			   (argument
				 (interpolated_string_expression
				   (string_content) @injection.content)
				 (#set! injection.language "sql")))
  (#eq? @_class-name "SqlCommand"))

(object_creation_expression
  type: (identifier) @_class-name
  arguments: (argument_list
			   (argument
				 (string_literal
				   (string_literal_content) @injection.content
				   (#set! injection.language "sql"))))
  (#eq? @_class-name "SqlCommand"))

(object_creation_expression
  type: (identifier) @_class-name
  arguments: (argument_list
			   (argument
				 ((verbatim_string_literal) @injection.content
											(#offset! @injection.content 0 2 0 -1)
											(#set! injection.language "sql"))))
  (#eq? @_class-name "SqlCommand"))
