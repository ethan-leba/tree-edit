;;; tree-edit-c-grammar.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Ethan Leba
;; Author: Ethan Leba <ethanleba5@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.0"))
;; Homepage: https://github.com/ethan-leba/tree-edit
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This file contains key bindings and other configuration for `tree-edit' to
;; work with c.
;;
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:
(require 'mode-local)
(require 'tree-edit)

(setq-mode-local
 c-mode

 tree-edit-whitespace-rules
 '((nil ("{" nil (:indent :newline))
        ("}" (:dedent :newline) nil)
        (comment nil (:newline))
        ;; TODO: should dig inwards for ; rules
        (type_definition nil (:newline))
        (field_declaration nil (:newline))
        (_statement nil (:newline))
        (preproc_def nil (:newline))
        (declaration nil (:newline))
        (_non_case_statement nil (:newline))))

 tree-edit--parser-name
 'c

 tree-edit-significant-node-types
 '()

 tree-edit-placeholder-node-type
 'identifier

 tree-edit-indentation-level
 2

 tree-edit-node-deletion-override
 nil

 tree-edit-node-replacement-override
 nil

 tree-edit-node-insertion-override
 nil

 tree-edit-dwim-node-alist
 '((_expression . expression_statement)
   (struct_specifier . type_descriptor))

 tree-edit-nodes
 '(;; Statements
   (:type if_statement
    :key "i")
   (:type union_specifier
    :key "u")

   (:type while_statement
    :key "w")
   (:type return_statement
    :key "r"
    :wrap-override '((return_statement . ("return" identifier))))
   (:type delete_statement
    :key "x")
   (:type raise_statement
    :key "R")
   (:type break_statement
    :key "b")
   (:type compound_statement
    :key "B")
   (:type expression_statement
    :key "e")
   (:type enum_specifier
    :key "E")
   (:type for_statement
    :key "f")
   (:type function_definition
    :key "F")
   (:type cast_expression
    :key "C")
   (:type assert_statement
    :key "A")
   (:type assignment_expression
    :key "v")
   (:type declaration
    :key "d"
    :name "variable init"
    :node-override '((declaration primitive_type init_declarator ";")))
   (:type declaration
    :key "D"
    :name "variable declaration")
   ;; *shrug*
   (:type preproc_include
    :key "q")

   ;; Types prefix
   (:type struct_specifier
    :key "ts"
    :node-override '((struct_specifier "struct" identifier)))


   ;; Expressions
   (:type field_expression
    :key ".")
   (:type field_expression
    :key ">"
    :node-override '((field_expression identifier "->" identifier)))
   (:type parenthesized_expression
    :key "(")
   (:type pointer_expression
    :key "*")
   (:type pointer_expression
    :key "&"
    :node-override
    '((pointer_expression "&" identifier)))
   (:type pointer_declarator
    :key "8")
   (:type parameter_declaration
    :key "A"
    :name "function argument")
   (:type list
    :key "l")
   (:type list_comprehension
    :key "L")
   (:type struct_specifier
    :key "s")
   (:type switch_statement
    :key "S")
   (:type identifier
    :key "a")
   (:type not_operator
    :key "n")
   (:type call_expression
    :key "c"
    :wrap-override '((argument_list . ("(" identifier ")"))))
   (:type conditional_expression
    :key "?"
    :name "ternary conditional")
   (:type pair
    :key ":")
   (:type keyword_argument
    :key "K")
   (:type unary_expression
    :key "-"
    :name "-TREE"
    :node-override '((unary_expression . ("-" identifier))))
   (:type unary_expression
    :key "!"
    :name "!TREE"
    :node-override '((unary_expression . ("!" identifier))))
   (:type pattern_list
    :key ",")
   ;; Uncommon nodes and nodes that are only valid in specific contexts
   (:type preproc_def
    :key "mv")
   (:type case_statement
    :key "mc")
   (:type case_statement
    :key "md"
    :node-override '((case_statement "default" ":" expression_statement))
    :name "default case")
   (:type field_declaration
    :key "mf")
   (:type update_expression
    :key "m+"
    :node-override '((update_expression identifier "++"))
    :name "TREE++")
   (:type update_expression
    :key "m="
    :node-override '((update_expression "++" identifier))
    :name "++TREE")
   (:type update_expression
    :key "m-"
    :node-override '((update_expression identifier "--"))
    :name "TREE--")
   (:type update_expression
    :key "m_"
    :node-override '((update_expression "--" identifier))
    :name "--TREE")
   (:type elif_clause
    :key "ml")
   (:type else_clause
    :key "me")
   (:type except_clause
    :key "mx")


   (:type if_clause
    :key "mi")
   (:type argument_list
    :key "ma")
   (:type decorated_definition
    :key "md")
   (:type subscript_expression
    :key "ms")
   (:type lambda
    :key "mL")
   (:type true
    :key "mt")
   (:type false
    :key "mn")
   (:type null
    :key "mN")
   (:type call
    :key "mm"
    :name "method call"
    :node-override '((call . (attribute argument_list))))

   ;; Augmented assignment
   (:type augmented_assignment
    :key "=-"
    :name "-"
    :node-override '((augmented_assignment . (identifier "-=" identifier))))
   (:type augmented_assignment
    :key "=+"
    :name "+"
    :node-override '((augmented_assignment . (identifier "+=" identifier))))
   (:type augmented_assignment
    :key "=*"
    :name "*"
    :node-override '((augmented_assignment . (identifier "*=" identifier))))
   (:type augmented_assignment
    :key "=@"
    :name "@"
    :node-override '((augmented_assignment . (identifier "@=" identifier))))
   (:type augmented_assignment
    :key "=/"
    :name "/"
    :node-override '((augmented_assignment . (identifier "/=" identifier))))
   (:type augmented_assignment
    :key "=\\"
    :name "//"
    :node-override '((augmented_assignment . (identifier "//=" identifier))))
   (:type augmented_assignment
    :key "=%"
    :name "%"
    :node-override '((augmented_assignment . (identifier "%=" identifier))))
   (:type augmented_assignment
    :key "=e"
    :name "**"
    :node-override '((augmented_assignment . (identifier "**=" identifier))))
   (:type augmented_assignment
    :key "=|"
    :name "|"
    :node-override '((augmented_assignment . (identifier "|=" identifier))))
   (:type augmented_assignment
    :key "=&"
    :name "&"
    :node-override '((augmented_assignment . (identifier "&=" identifier))))
   ;; Operators
   (:type binary_expression
    :key "o-"
    :name "-"
    :node-override '((binary_expression . (identifier "-" identifier))))
   (:type binary_expression
    :key "o+"
    :name "+"
    :node-override '((binary_expression . (identifier "+" identifier))))
   (:type binary_expression
    :key "o*"
    :name "*"
    :node-override '((binary_expression . (identifier "*" identifier))))
   (:type binary_expression
    :key "o@"
    :name "@"
    :node-override '((binary_expression . (identifier "@" identifier))))
   (:type binary_expression
    :key "o/"
    :name "/"
    :node-override '((binary_expression . (identifier "/" identifier))))
   (:type binary_expression
    :key "o\\"
    :name "//"
    :node-override '((binary_expression . (identifier "//" identifier))))
   (:type binary_expression
    :key "o%"
    :name "%"
    :node-override '((binary_expression . (identifier "%" identifier))))
   (:type binary_expression
    :key "oe"
    :name "**"
    :node-override '((binary_expression . (identifier "**" identifier))))
   (:type binary_expression
    :key "o|"
    :name "|"
    :node-override '((binary_expression . (identifier "|" identifier))))
   (:type binary_expression
    :key "o&"
    :name "&"
    :node-override '((binary_expression . (identifier "&" identifier))))
   (:type binary_expression
    :key "o^"
    :name "^"
    :node-override '((binary_expression . (identifier "^" identifier))))
   (:type binary_expression
    :key "ol"
    :name "<<"
    :node-override '((binary_expression . (identifier "<<" identifier))))
   (:type binary_expression
    :key "or"
    :name ">>"
    :node-override '((binary_expression . (identifier ">>" identifier))))
   (:type binary_expression
    :key "o="
    :name "=="
    :node-override '((binary_expression . (identifier "==" identifier))))
   (:type binary_expression
    :key "o!"
    :name "!="
    :node-override '((binary_expression . (identifier "!=" identifier))))
   (:type binary_expression
    :key "o>"
    :name ">"
    :node-override '((binary_expression . (identifier ">" identifier))))
   (:type binary_expression
    :key "o<"
    :name "<"
    :node-override '((binary_expression . (identifier "<" identifier))))
   (:type binary_expression
    :key "o,"
    :name "<="
    :node-override '((binary_expression . (identifier "<=" identifier))))
   (:type binary_expression
    :key "o."
    :name ">="
    :node-override '((binary_expression . (identifier ">=" identifier))))
   (:type binary_expression
    :key "oo"
    :name "or"
    :node-override '((binary_expression . (identifier "||" identifier))))
   (:type binary_expression
    :key "oa"
    :name "and"
    :node-override '((binary_expression . (identifier "&&" identifier)))))

 tree-edit-query-nodes
 '((:type (struct_specifier
           union_specifier
           enum_specifier
           macro_type_specifier
           sized_type_specifier
           primitive_type
           type_identifier)
    :name "types"
    :key "t")
   (:type (identifier
           field_identifier
           type_identifier)
    :name "identifiers"
    :key "a")
   (:type (number_literal
           string_literal
           true
           false
           null
           concatenated_string
           char_literal)
    :name "values"
    :key "V"))

 ;; TODO: Check me!
 tree-edit-syntax-snippets
 '((translation_unit)
   (_top_level_item function_definition)
   (preproc_include "#include" string_literal)
   (preproc_def "#define" identifier identifier)
   (preproc_function_def \#define identifier preproc_params "
")
   (preproc_params "(" ")")
   (preproc_call preproc_directive "
")
   (preproc_if \#if identifier "
" \#endif)
   (preproc_ifdef \#ifdef identifier \#endif)
   (preproc_else \#else)
   (preproc_elif \#elif identifier "
")
   (preproc_if_in_field_declaration_list \#if identifier "
" \#endif)
   (preproc_ifdef_in_field_declaration_list \#ifdef identifier \#endif)
   (preproc_else_in_field_declaration_list \#else)
   (preproc_elif_in_field_declaration_list \#elif identifier "
")
   (preproc_directive :regex)
   (preproc_arg :regex)
   (_preproc_expression identifier)
   (preproc_parenthesized_expression "(" identifier ")")
   (preproc_defined "defined" identifier)
   (preproc_unary_expression "!" identifier)
   (preproc_call_expression identifier argument_list)
   (preproc_argument_list "(" ")")
   (preproc_binary_expression identifier "+" identifier)
   (function_definition primitive_type identifier parameter_list compound_statement)
   (declaration primitive_type identifier ";")
   (type_definition "typedef" struct_specifier pointer_declarator ";")
   (_declaration_specifiers struct_specifier)
   (linkage_specification "extern" string_literal function_definition)
   (attribute_specifier "__attribute__" "(" argument_list ")")
   (ms_declspec_modifier "__declspec" "(" identifier ")")
   (ms_based_modifier "__based" argument_list)
   (ms_call_modifier "__cdecl")
   (ms_restrict_modifier "__restrict")
   (ms_unsigned_ptr_modifier "__uptr")
   (ms_signed_ptr_modifier "__sptr")
   (ms_unaligned_ptr_modifier "_unaligned")
   (ms_pointer_modifier ms_unaligned_ptr_modifier)
   (declaration_list "{" "}")
   (_declarator pointer_declarator)
   (_field_declarator pointer_declarator)
   (_type_declarator pointer_declarator)
   (_abstract_declarator abstract_pointer_declarator)
   (parenthesized_declarator "(" pointer_declarator ")")
   (parenthesized_field_declarator "(" pointer_declarator ")")
   (parenthesized_type_declarator "(" pointer_declarator ")")
   (abstract_parenthesized_declarator "(" abstract_pointer_declarator ")")
   (pointer_declarator "*" identifier)
   (pointer_field_declarator "*" pointer_declarator)
   (pointer_type_declarator "*" pointer_declarator)
   (abstract_pointer_declarator "*")
   (function_declarator pointer_declarator parameter_list)
   (function_field_declarator pointer_declarator parameter_list)
   (function_type_declarator pointer_declarator parameter_list)
   (abstract_function_declarator parameter_list)
   (array_declarator pointer_declarator "[" "]")
   (array_field_declarator pointer_declarator "[" "]")
   (array_type_declarator pointer_declarator "[" "]")
   (abstract_array_declarator "[" "]")
   (init_declarator identifier "=" identifier)
   (compound_statement "{" "}")
   (storage_class_specifier "extern")
   (type_qualifier "const")
   (_type_specifier struct_specifier)
   (sized_type_specifier "signed")
   (primitive_type "void")
   (enum_specifier "enum" identifier enumerator_list)
   (enumerator_list "{" "}")
   (struct_specifier "struct" identifier field_declaration_list)
   (union_specifier "union" field_declaration_list)
   (field_declaration_list "{" "}")
   (_field_declaration_list_item field_declaration)
   (field_declaration primitive_type identifier ";")
   (bitfield_clause ":" identifier)
   (enumerator identifier)
   (parameter_list "(" ")")
   (parameter_declaration primitive_type identifier)
   (_statement compound_statement)
   (_non_case_statement labeled_statement)
   (labeled_statement statement_identifier ":" compound_statement)
   (expression_statement identifier ";")
   (if_statement "if" parenthesized_expression compound_statement)
   (switch_statement "switch" parenthesized_expression compound_statement)
   (case_statement "case" identifier ":" expression_statement)
   (while_statement "while" parenthesized_expression compound_statement)
   (do_statement "do" compound_statement "while" parenthesized_expression ";")
   (for_statement "for" "(" declaration ";" ")" compound_statement)
   (return_statement "return" ";")
   (break_statement "break" ";")
   (continue_statement "continue" ";")
   (goto_statement "goto" statement_identifier ";")
   (_expression identifier)
   (comma_expression identifier "," identifier)
   (conditional_expression identifier
                           "?"
                           identifier
                           ":"
                           identifier)
   (_assignment_left_expression identifier)
   (assignment_expression identifier "=" identifier)
   (pointer_expression "*" identifier)
   (unary_expression "!" identifier)
   (binary_expression identifier "+" identifier)
   (update_expression "--" identifier)
   (cast_expression "(" type_descriptor ")" identifier)
   (type_descriptor identifier)
   (sizeof_expression "sizeof" identifier)
   (subscript_expression identifier "[" identifier "]")
   (call_expression identifier argument_list)
   (argument_list "(" ")")
   (field_expression identifier "." identifier)
   (compound_literal_expression "(" type_descriptor ")" initializer_list)
   (parenthesized_expression "(" identifier ")")
   (initializer_list "{" "}")
   (initializer_pair subscript_designator "=" initializer_list)
   (subscript_designator "[" identifier "]")
   (field_designator "." field_identifier)
   (number_literal "." :regex)
   (char_literal "L'" escape_sequence "'")
   (concatenated_string string_literal string_literal)
   (string_literal "\"TREE\"")
   (escape_sequence "\\" :regex)
   (system_lib_string "<" ">")
   (true "TRUE")
   (false "FALSE")
   (null "NULL")
   (identifier "TREE")
   (_type_identifier type_identifier)
   (_field_identifier field_identifier)
   (_statement_identifier statement_identifier)
   (_empty_declaration struct_specifier ";")
   (macro_type_specifier identifier "(" type_descriptor ")")
   (comment "//" :regex)))

(provide 'tree-edit-c)
;;; tree-edit-c.el ends here
