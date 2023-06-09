;;; tree-edit-elisp.el --- Description -*- lexical-binding: t; -*-
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
;; work with Elisp.
;;
;;
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:
(require 'tree-edit)

(tree-edit--set-parser-local-vars
 'elisp

 tree-edit-placeholder-node-type
 'symbol

 tree-edit-syntax-snippets
 ((list . ("(" symbol ")"))
  (vector . ("[" "]"))
  (quote . ("'" symbol))
  (unquote . ("," symbol))
  (symbol . ("TREE")))

 tree-edit-whitespace-rules
 ((nil (comment nil (:newline))))

 tree-edit-node-deletion-override
 ((source_file . tree-edit-simple-delete-override))

 tree-edit-node-replacement-override
 ((source_file . tree-edit-simple-insertion-replacement-override))

 tree-edit-node-insertion-override
 ((source_file . tree-edit-simple-insertion-replacement-override))

 tree-edit-nodes
 ((:type list
   :key "l"
   :wrap-override '((list . ("(" symbol ")"))))
  (:type symbol
   :key "s")
  (:type vector
   :key "v")
  (:type hash_table
   :key "h")
  (:type quote
   :key "q")
  (:type unquote
   :key "u")
  (:type unquote_splice
   :key "U")))


(provide 'tree-edit-elisp)
;;; tree-edit-elisp.el ends here
