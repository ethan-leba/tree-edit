;;; tree-edit-java.el --- Description -*- lexical-binding: t; -*-
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
;; work with Java.
;;
;;
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:
(require 'mode-local)
(require 'tree-edit-java-grammar)
(require 'tree-edit)

(setq-mode-local
 java-mode

 tree-edit-syntax-snippets
 '((class_declaration . ("class" identifier "{" "}"))
   (method_declaration . ("void" identifier "(" ")" "{" "}"))
   (method_declaration . ("void" identifier "(" ")" "{" "}"))
   (object_creation_expression . ("new" method_invocation))
   (if_statement . ("if" parenthesized_expression block))
   (while_statement . ("while" parenthesized_expression block))
   (method_invocation . (identifier argument_list))
   (try_statement . ("try" block catch_clause))
   (catch_clause . ("catch" "(" catch_formal_parameter ")" block))
   (finally_clause . ("finally" block))
   (string_literal . ("\"\""))
   (catch_formal_parameter . (catch_type "e"))
   (catch_type . ("Exception"))
   (local_variable_declaration . ("void" identifier "=" identifier ";"))
   (argument_list . ("(" ")"))
   (continue_statement . ("continue" ";"))
   (break_statement . ("break" ";"))
   (return_statement . ("return" ";"))
   (null_literal . ("null"))
   ;; I would use an identifier here, but java LSP won't format when 'TREE;' is present...
   (expression_statement . (method_invocation ";"))
   (block . ("{" "}"))
   (parenthesized_expression . ("(" identifier ")"))
   (expression . (identifier))
   (identifier . ("TREE"))
   (formal_parameter . ("int" "TREE"))
   (import_declaration . ("import" "TREE" ";")))

 tree-edit-significant-node-types
 '(block class_body)

 tree-edit-nodes
 '((:type if_statement
    :key "i"
    :wrap-override '((block . ("{" expression_statement "}"))))
   (:type return_statement
    :key "r")
   (:type while_statement
    :key "w"
    :wrap-override '((block . ("{" expression_statement "}"))))
   (:type expression_statement
    :key "e")
   (:type class_declaration
    :key "C")
   (:type identifier
    :key "a")
   (:type break_statement
    :key "B")
   (:type block
    :key "{")
   (:type string_literal
    :key "\"")
   (:type continue_statement
    :key "c")
   (:type local_variable_declaration
    :key "v")
   (:type method_invocation
    :name "static call"
    :key "c"
    :wrap-override '((argument_list . ("(" expression ")"))))
   (:type method_declaration
    :key "m")
   (:type object_creation_expression
    :key "n"
    :name "new object")
   (:type binary_expression
    :key "o+"
    :name "+ operator"
    :node-override '((binary_expression . (expression "-" expression))))
   (:type binary_expression
    :key "o-"
    :name "- operator"
    :node-override '((binary_expression . (expression "-" expression))))
   (:type binary_expression
    :key "o&"
    :name "and operator"
    :node-override '((binary_expression . (expression "&&" expression))))
   (:type binary_expression
    :key "o|"
    :name "or operator"
    :node-override '((binary_expression . (expression "||" expression))))
   (:type binary_expression
    :key "o="
    :name "== operator"
    :node-override '((binary_expression . (expression "==" expression))))
   (:type update_expression
    :key "+"
    :name "value++"
    :node-override '((update_expression . (expression "++"))))
   (:type modifiers
    :key "tp"
    :name "private"
    :node-override '((modifiers . ("private"))))
   (:type formal_parameter
    :key "P")
   (:type import_declaration
    :key "I")
   (:type try_statement
    :key "T")
   (:type catch_clause
    :key "C")
   (:type finally_clause
    :key "f")
   (:type null_literal
    :key "n")))

(provide 'tree-edit-java)
;;; tree-edit-java.el ends here
