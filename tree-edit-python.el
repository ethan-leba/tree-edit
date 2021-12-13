;;; tree-edit-python.el --- Description -*- lexical-binding: t; -*-
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
;; work with Python.
;;
;;
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:
(require 'mode-local)
(require 'tree-edit-python-grammar)
(require 'tree-edit)

(defun tree-edit-python-block-deletion-override (node)
  "Allow deletion of NODE in block, unless it's the only node."
  (-let [(tokens . index) (tree-edit--get-parent-tokens node)]
    (if (> (length tokens) 1) `(,index ,index nil))))

(setq-mode-local
 python-mode

 tree-edit-syntax-snippets
 '(;; Statements
   (if_statement . ("if" expression ":" block))
   (with_statement . ("with" expression ":" block))
   (else_clause . ("else" ":" block))
   (elif_clause . ("elif" expression ":" block))
   (for_statement . ("for" identifier "in" expression ":" block))
   (try_statement . ("try" ":" block except_clause))
   (except_clause . ("except" identifier ":" block))
   (finally_clause . ("finally" ":" block))
   (return_statement . ("return"))
   (raise_statement . ("raise" expression))
   (break_statement . ("break"))
   (continue_statement . ("continue"))
   (pass_statement . ("pass"))
   (global_statement . ("global" identifier))
   (nonlocal_statement . ("nonlocal" identifier))
   (import_statement . ("import" identifier))
   (import_from_statement . ("from" identifier "import" identifier))
   (delete_statement . ("del" expression))
   (assert_statement . ("assert" expression))
   (function_definition . ("def" identifier parameters ":" block))
   (class_definition . ("class" identifier ":" block))
   (parameters . ("(" ")"))
   (decorated_definition . (decorator function_definition))
   (assignment . (identifier "=" expression))
   (block . (expression_statement))
   (expression_statement . (expression))
   (decorator . ("@" primary_expression))

   ;; Expressions & values
   (list . ("[" "]"))
   (list_splat . ("*" expression))
   (list_comprehension . ("[" expression for_in_clause "]"))
   (dictionary . ("{" "}"))
   (dictionary_comprehension . ("{" pair for_in_clause "}"))
   (dictionary_splat . ("**" expression))
   (not_operator . ("not" expression))
   (pair . (identifier ":" identifier))
   (keyword_argument . (identifier "=" identifier))
   (pattern_list . (identifier "," identifier))
   ;; Set can't be empty, otherwise it's a dict
   (set . ("{" identifier "}"))
   (set_comprehension . ("{" expression for_in_clause "}"))
   (conditional_expression . (expression "if" expression "else" expression))
   (for_in_clause . ("for" identifier "in" identifier))
   (if_clause . ("if" identifier))
   (named_expression . (identifier ":=" expression))
   (lambda . ("lambda" lambda_parameters ":" expression))
   (lambda_parameters . (identifier))
   (call . (primary_expression argument_list))
   (argument_list . ("(" ")"))
   (true . ("True"))
   (false . ("False"))
   (none . ("None"))
   (attribute . (primary_expression "." identifier))
   (primary_expression . (identifier))
   (subscript . (identifier "[" expression "]"))
   (slice . (expression ":" expression))
   (expression . (identifier))
   (identifier . ("TREE")))

 ;; WARNING: Python is whitespace dependent, so messing with these parameters
 ;; may produce unparseable text
 tree-edit-whitespace-rules
 '((block . ((:newline :indent) . (:dedent :newline)))
   (expression . (nil . nil))
   (comment . (nil . (:newline)))
   (decorator . (nil . (:newline)))
   (_simple_statement . (nil . (:newline)))
   (except_clause . (nil . (:newline)))
   (finally_clause . (nil . (:newline)))
   (elif_clause . (nil . (:newline)))
   (else_clause . (nil . (:newline)))
   (_compound_statement . (nil . (:newline))))

 tree-edit-significant-node-types
 '(decorated_definition function_definition class_definition)

 tree-edit-placeholder-node-type
 'identifier

 tree-edit-node-deletion-override
 '((block . tree-edit-python-block-deletion-override)
   (module . tree-edit-simple-delete-override))

 tree-edit-node-replacement-override
 '((block . tree-edit-simple-replacement-override)
   (module . tree-edit-simple-replacement-override))

 tree-edit-node-insertion-override
 '((block . tree-edit-simple-insertion-override)
   (module . tree-edit-simple-insertion-override))

 ;; TODO: this should be auto-generated in the grammar file
 tree-edit--hidden-node-types
 '(_newline _indent _dedent)

 tree-edit-nodes
 '(;; Statements
   (:type if_statement
    :key "i")
   (:type with_statement
    :key "W")
   (:type return_statement
    :key "r"
    :wrap-override '((return_statement . ("return" expression))))
   (:type delete_statement
    :key "x")
   (:type raise_statement
    :key "R")
   (:type break_statement
    :key "b")
   (:type try_statement
    :key "t")
   (:type expression_statement
    :key "e")
   (:type for_statement
    :key "f")
   (:type function_definition
    :key "F")
   (:type class_definition
    :key "C")
   (:type assert_statement
    :key "A")
   (:type assignment
    :key "v"
    :name "variable declaration")
   ;; *shrug*
   (:type import_statement
    :key "q")
   (:type import_from_statement
    :key "Q")

   ;; Expressions
   (:type attribute
    :key ".")
   (:type named_expression
    :key "w"
    :name "walrus operator")
   (:type list
    :key "l")
   (:type list_comprehension
    :key "L")
   (:type set
    :key "s")
   (:type set_comprehension
    :key "S")
   (:type dictionary
    :key "d")
   (:type dictionary_comprehension
    :key "D")
   (:type identifier
    :key "a")
   (:type not_operator
    :key "n")
   (:type call
    :key "c"
    :wrap-override '((argument_list . ("(" expression ")"))))
   (:type conditional_expression
    :key "T"
    :name "ternary conditional")
   (:type pair
    :key ":")
   (:type keyword_argument
    :key "K")
   (:type unary_operator
    :key "-"
    :name "negation"
    :node-override '((unary_operator . ("-" expression))))
   (:type pattern_list
    :key ",")
   ;; Uncommon nodes and nodes that are only valid in specific contexts
   (:type list_splat
    :key "m*")
   (:type dictionary_splat
    :key "m8")
   (:type elif_clause
    :key "ml")
   (:type else_clause
    :key "me")
   (:type except_clause
    :key "mx")
   (:type finally_clause
    :key "mF")
   (:type for_in_clause
    :key "mf")
   (:type if_clause
    :key "mi")
   (:type argument_list
    :key "ma")
   (:type decorated_definition
    :key "md")
   (:type subscript
    :key "ms")
   (:type subscript
    :key "mS"
    :name "subscript slice"
    :node-override '((subscript . (identifier "[" slice "]"))))
   (:type lambda
    :key "mL")
   (:type true
    :key "mt")
   (:type false
    :key "mn")
   (:type none
    :key "mN")

   ;; Augmented assignment
   (:type augmented_assignment
    :key "=-"
    :name "-"
    :node-override '((augmented_assignment . (expression "-=" expression))))
   (:type augmented_assignment
    :key "=+"
    :name "+"
    :node-override '((augmented_assignment . (expression "+=" expression))))
   (:type augmented_assignment
    :key "=*"
    :name "*"
    :node-override '((augmented_assignment . (expression "*=" expression))))
   (:type augmented_assignment
    :key "=@"
    :name "@"
    :node-override '((augmented_assignment . (expression "@=" expression))))
   (:type augmented_assignment
    :key "=/"
    :name "/"
    :node-override '((augmented_assignment . (expression "/=" expression))))
   (:type augmented_assignment
    :key "=\\"
    :name "//"
    :node-override '((augmented_assignment . (expression "//=" expression))))
   (:type augmented_assignment
    :key "=%"
    :name "%"
    :node-override '((augmented_assignment . (expression "%=" expression))))
   (:type augmented_assignment
    :key "=e"
    :name "**"
    :node-override '((augmented_assignment . (expression "**=" expression))))
   (:type augmented_assignment
    :key "=|"
    :name "|"
    :node-override '((augmented_assignment . (expression "|=" expression))))
   (:type augmented_assignment
    :key "=&"
    :name "&"
    :node-override '((augmented_assignment . (expression "&=" expression))))
   ;; Operators
   (:type binary_operator
    :key "o-"
    :name "-"
    :node-override '((binary_operator . (expression "-" expression))))
   (:type binary_operator
    :key "o+"
    :name "+"
    :node-override '((binary_operator . (expression "+" expression))))
   (:type binary_operator
    :key "o*"
    :name "*"
    :node-override '((binary_operator . (expression "*" expression))))
   (:type binary_operator
    :key "o@"
    :name "@"
    :node-override '((binary_operator . (expression "@" expression))))
   (:type binary_operator
    :key "o/"
    :name "/"
    :node-override '((binary_operator . (expression "/" expression))))
   (:type binary_operator
    :key "o\\"
    :name "//"
    :node-override '((binary_operator . (expression "//" expression))))
   (:type binary_operator
    :key "o%"
    :name "%"
    :node-override '((binary_operator . (expression "%" expression))))
   (:type binary_operator
    :key "oe"
    :name "**"
    :node-override '((binary_operator . (expression "**" expression))))
   (:type binary_operator
    :key "o|"
    :name "|"
    :node-override '((binary_operator . (expression "|" expression))))
   (:type binary_operator
    :key "o&"
    :name "&"
    :node-override '((binary_operator . (expression "&" expression))))
   (:type binary_operator
    :key "o^"
    :name "^"
    :node-override '((binary_operator . (expression "^" expression))))
   (:type binary_operator
    :key "ol"
    :name "<<"
    :node-override '((binary_operator . (expression "<<" expression))))
   (:type binary_operator
    :key "or"
    :name ">>"
    :node-override '((binary_operator . (expression ">>" expression))))
   (:type comparison_operator
    :key "o="
    :name "=="
    :node-override '((comparison_operator . (expression "==" expression))))
   (:type comparison_operator
    :key "o!"
    :name "!="
    :node-override '((comparison_operator . (expression "==" expression))))
   (:type comparison_operator
    :key "on"
    :name "in"
    :node-override '((comparison_operator . (expression "in" expression))))
   (:type comparison_operator
    :key "oN"
    :name "not in"
    :node-override '((comparison_operator . (expression "not" "in" expression))))
   (:type comparison_operator
    :key "oi"
    :name "is"
    :node-override '((comparison_operator . (expression "is" expression))))
   (:type comparison_operator
    :key "oI"
    :name "is not"
    :node-override '((comparison_operator . (expression "is" "not" expression))))
   (:type comparison_operator
    :key "o>"
    :name ">"
    :node-override '((comparison_operator . (expression ">" expression))))
   (:type comparison_operator
    :key "o<"
    :name "<"
    :node-override '((comparison_operator . (expression "<" expression))))
   (:type comparison_operator
    :key "o,"
    :name "<="
    :node-override '((comparison_operator . (expression "<=" expression))))
   (:type comparison_operator
    :key "o."
    :name ">="
    :node-override '((comparison_operator . (expression ">=" expression))))
   (:type boolean_operator
    :key "oa"
    :name "and"
    :node-override '((boolean_operator . (expression "and" expression))))
   (:type boolean_operator
    :key "oo"
    :name "or"
    :node-override '((boolean_operator . (expression "or" expression)))))

 tree-edit-query-nodes
 '((:type (integer float string true false none)
    :name "values"
    :key "V")
   (:type (set list tuple dictionary)
    :name "containers"
    :key "j")
   (:type (decorated_definition
           class_definition
           function_definition
           with_statement
           try_statement
           while_statement
           for_statement
           if_statement
           future_import_statement
           import_statement
           import_from_statement
           print_statement
           assert_statement
           expression_statement
           return_statement
           delete_statement
           raise_statement
           pass_statement
           break_statement
           continue_statement
           global_statement
           nonlocal_statement
           exec_statement)
    :name "statement"
    :key "k")))


(provide 'tree-edit-python)
;;; tree-edit-python.el ends here
