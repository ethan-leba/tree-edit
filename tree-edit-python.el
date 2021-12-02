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
   (elif_clause . ("elif" expression ":" block))
   (else_clause . ("else" expression ":" block))
   (block . (expression_statement))
   (expression_statement . (expression))
   (expression . (identifier))
   (identifier . ("TREE")))

 tree-edit-whitespace-rules
 '((block . ((:newline :indent) . (:dedent :newline)))
   (expression . (nil . nil))
   (_simple_statement . (nil . (:newline)))
   (elif_clause . (nil . (:newline)))
   (_compound_statement . (nil . (:newline))))

 tree-edit-significant-node-types
 '(block class_body)

 ;; TODO: this should be auto-generated in the grammar file
 tree-edit--hidden-node-types
 '(_newline _indent _dedent)

 tree-edit-nodes
 '((:type if_statement
    :key "i")
   (:type elif_clause
    :key "I")
   (:type else_clause
    :key "e")))


(provide 'tree-edit-python)
;;; tree-edit-python.el ends here
