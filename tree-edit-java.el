;;; tree-edit-java.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Ethan Leba
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:
(require 'mode-local)
(require 'tree-edit-java-grammar)

(setq-mode-local
 java-mode

 tree-edit-semantic-snippets
 '((if_statement . ("if" parenthesized_expression expression_statement))
   (method_invocation . (identifier argument_list))
   (argument_list . ("(" ")"))
   (continue_statement . ("continue" ";\n"))
   (break_statement . ("break" ";"))
   (expression_statement . (identifier ";\n"))
   (block . ("{}"))
   (parenthesized_expression . ("(" identifier ")"))
   (identifier . ("TREE")))

 tree-edit-significant-node-types
 '(block class_body)

 tree-edit-nodes
 '((:type if_statement
    :key "i")
   (:type break_statement
    :key "b")
   (:type continue_statement
    :key "c")
   (:type local_variable_declaration
    :key "v")
   (:type method_invocation
    :name "static call"
    :key "c"))

 evil-tree-state-map (make-sparse-keymap))

(provide 'tree-edit-java)
;;; tree-edit-java.el ends here
