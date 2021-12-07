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

(setq-mode-local
 python-mode

 tree-edit-syntax-snippets
 '((if_statement . ("if" expression ":" block))
   (for_statement . ("for" identifier "in" expression ":" block))
   (elif_clause . ("elif" expression ":" block))
   (return_statement . ("return"))
   (assignment . (identifier "=" expression))
   (list_comprehension . ("[" expression "for" identifier "in" identifier "]"))
   (else_clause . ("else" ":" block))
   (assert_statement . ("assert" expression))
   (function_definition . ("def" identifier parameters ":" block))
   (decorated_definition . (decorator function_definition))
   (call . (primary_expression argument_list))
   (decorator . ("@" primary_expression))
   (primary_expression . (identifier))
   (argument_list . ("(" ")"))
   (parameters . ("(" ")"))
   (block . (expression_statement))
   (expression_statement . (expression))
   (expression . (identifier))
   (identifier . ("TREE")))

 tree-edit-whitespace-rules
 '((block . ((:newline :indent) . (:dedent :newline)))
   (expression . (nil . nil))
   (comment . (nil . (:newline)))
   (decorator . (nil . (:newline)))
   (_simple_statement . (nil . (:newline)))
   (elif_clause . (nil . (:newline)))
   (_compound_statement . (nil . (:newline))))

 tree-edit-significant-node-types
 '(block decorated_definition function_definition class_definition)

 tree-edit-placeholder-node-type
 'identifier

 ;; TODO: this should be auto-generated in the grammar file
 tree-edit--hidden-node-types
 '(_newline _indent _dedent)

 tree-edit-nodes
 '((:type if_statement
    :key "i")
   (:type list_comprehension
    :key "l")
   (:type identifier
    :key "a")
   (:type return_statement
    :key "r"
    :wrap-override '((return_statement . ("return" expression))))
   (:type elif_clause
    :key "I")
   (:type else_clause
    :key "E")
   (:type expression_statement
    :key "e")
   (:type for_statement
    :key "f")
   (:type function_definition
    :key "d")
   (:type assignment
    :key "v"
    :name "variable declaration")
   (:type call
    :key "c"
    :wrap-override '((argument_list . ("(" expression ")"))))
   (:type decorated_definition
    :key "D")
   (:type assert_statement
    :key "A")
   (:type binary_operator
    :key "o="
    :name "== operator"
    :node-override '((binary_operator . (expression "==" expression))))))


(provide 'tree-edit-python)
;;; tree-edit-python.el ends here
