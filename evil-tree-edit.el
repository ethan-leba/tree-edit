;;; evil-tree-edit.el --- Evil structural editing for any language! -*- lexical-binding: t; -*-
;;
;; Copyright (C) Ethan Leba <https://github.com/ethan-leba>
;;
;; Author: Ethan Leba <ethanleba5@gmail.com>
;; Version: 0.1.0
;; Homepage: https://github.com/ethan-leba/tree-edit
;; Package-Requires: ((emacs "27.0") (tree-edit "0.1.0") (tree-sitter "0.15.0") (evil "1.0.0") (avy "0.5.0") (s "0.0.0"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Provides a 'tree' evil state that allows structural navigation and
;;  modification of the buffer.
;;
;;  See https://github.com/ethan-leba/tree-edit for usage.
;;
;;; Code:
(require 'avy)
(require 'evil)
(require 'mode-local)
(require 'tree-edit)
(require 's)

;;* Variables
(defvar evil-tree-edit-mode-map (make-sparse-keymap))
(defvar-local evil-tree-edit--node-overlay nil
  "The display overlay to show the current node.")
(defvar-local evil-tree-edit--return-to-tree-state nil
  "Whether tree state should be returned to after exiting insert mode.")

;;* Evil-specific tree-edit functions
(defun evil-tree-edit-change-node ()
  "Change the current node."
  (interactive)
  (setq evil-tree-edit--return-to-tree-state (tree-edit--save-location tree-edit--current-node))
  (delete-region (tsc-node-start-position tree-edit--current-node)
                 (tsc-node-end-position tree-edit--current-node))
  (evil-change-state 'insert))

(defun evil-tree-edit-avy-jump (node-type &optional pred)
  "Avy jump to a node with the NODE-TYPE and filter the node with PRED.

PRED will receive a pair of (position . node).
NODE-TYPE can be a symbol or a list of symbol."
  (interactive)
  (let* ((node-type (if (listp node-type) node-type `(,node-type)))
         ;; Querying needs a @name for unknown reasons
         (query-string
          (format "[%s] @foo"
                  (-as-> node-type %
                         (-mapcat (lambda (x) (alist-get x tree-edit--subtypes)) %)
                         (-uniq %)
                         (--remove (string-prefix-p "_" (symbol-name it)) %)
                         (--map (format "(%s)" it) %)
                         (string-join % " "))))
         (position->node
          (-filter (or pred (-const t))
                   (-remove-first (-lambda ((pos . _))
                                    (equal pos (tsc-node-start-position tree-edit--current-node)))
                                  (tree-edit-query query-string))))
         ;; avy-action declares what should be done with the result of avy-process
         (avy-action (lambda (pos)
                       (setq tree-edit--current-node (alist-get pos position->node))
                       (run-hooks 'tree-edit-movement-hook))))
    (cond ((not position->node) (user-error "Nothing to jump to!"))
          ((equal (length position->node) 1) (funcall avy-action (caar position->node)))
          (t (avy-process (-map #'car position->node))))))

;; TODO: Moved wrap here since it relies on avy, but it should be part of
;; `tree-edit.el` proper.
(defun evil-tree-edit-wrap-node (type)
  "Wrap the current node in a node of selected TYPE."
  (tree-edit--preserve-location tree-edit--current-node 0
    (let ((node-text (tsc-node-text tree-edit--current-node)))
      (tree-edit-exchange-node type)
      (unwind-protect
          (evil-tree-edit-avy-jump (alist-get type tree-edit--supertypes)
                                   (-lambda ((_ . node)) (tree-edit--valid-replacement-p type node)))
        (tree-edit-exchange-node node-text)))))

;;* Evil state definition and keybindings
(defun evil-tree-edit--update-overlay ()
  "Update the display of the current selected node, and move the cursor."
  (move-overlay evil-tree-edit--node-overlay
                (tsc-node-start-position tree-edit--current-node)
                (tsc-node-end-position tree-edit--current-node))
  (goto-char (tsc-node-start-position tree-edit--current-node)))

(defun evil-tree-edit--enter-tree-state ()
  "Activate tree-edit state."
  (unless evil-tree-edit--node-overlay
    (setq evil-tree-edit--node-overlay (make-overlay 0 0)))
  (let ((node (tsc-get-descendant-for-position-range
               (tsc-root-node tree-sitter-tree) (point) (point))))
    (setq tree-edit--current-node
          (if (tree-edit--boring-nodep node)
              (tree-edit--apply-until-interesting #'tsc-get-parent node)
            node)))
  (overlay-put evil-tree-edit--node-overlay 'face 'region)
  (evil-tree-edit--update-overlay))

(defun evil-tree-edit--exit-tree-state ()
  "De-activate tree-edit state."
  (when evil-tree-edit--node-overlay
    (overlay-put evil-tree-edit--node-overlay 'face '())))

(defun evil-tree-edit--re-enter-tree-state ()
  "Change the current node."
  (when evil-tree-edit--return-to-tree-state
    (evil-tree-state)
    (tree-edit--restore-location evil-tree-edit--return-to-tree-state 0)
    (setq evil-tree-edit--return-to-tree-state nil)))

(defun evil-tree-edit-teardown ()
  "De-activate tree-edit state."
  (when evil-tree-edit--node-overlay
    (delete-overlay evil-tree-edit--node-overlay)))

(evil-define-state tree
  "evil-tree-edit state"
  :tag " <T>"
  :entry-hook (evil-tree-edit--enter-tree-state)
  :exit-hook (evil-tree-edit--exit-tree-state)
  :suppress-keymap t)

;;;###autoload
(define-minor-mode evil-tree-edit-mode
  "Structural editing for any* language."
  :init-value nil
  :group 'tree-edit
  :keymap evil-tree-edit-mode-map
  :lighter " TE"
  (cond
   (evil-tree-edit-mode
    (let ((language-file (alist-get major-mode tree-edit-language-alist)))
      (unless language-file
        (evil-tree-edit-mode -1)
        (user-error "Tree-edit does not support %s!" (symbol-name major-mode)))
      (unless (featurep language-file)
        (require language-file)
        (evil-tree-edit--set-state-bindings major-mode)))
    (tree-sitter-mode)
    ;; HACK: Above mode binding won't come into effect until the state is changed.
    (evil-normal-state)
    (add-hook 'before-revert-hook #'evil-tree-edit-teardown nil 'local)
    (add-hook 'tree-edit-movement-hook #'evil-tree-edit--update-overlay nil 'local)
    (add-hook 'evil-normal-state-entry-hook #'evil-tree-edit--re-enter-tree-state nil 'local))
   (t
    (remove-hook 'before-revert-hook #'evil-tree-edit-teardown 'local)
    (remove-hook 'tree-edit-movement-hook #'evil-tree-edit--update-overlay 'local)
    (remove-hook 'evil-normal-state-entry-hook #'evil-tree-edit--re-enter-tree-state 'local))))

(defun define-evil-tree-edit-verb (key func &optional wrap)
  "Define a key command prefixed by KEY, calling FUNC.

FUNC must take two arguments, a symbol of the node type.
If WRAP is t, include :wrap-override."
  (dolist (node tree-edit-nodes)
    (define-key
      evil-tree-state-map
      (string-join (list key (plist-get node :key)))
      (cons
       ;; emacs-which-key integration
       (or (plist-get node :name) (s-replace "_" " " (symbol-name (plist-get node :type))))
       `(lambda ()
          (interactive)
          (let ((tree-edit-syntax-snippets
                 (append ,(plist-get node :node-override)
                         ,(if wrap (plist-get node :wrap-override))
                         tree-edit-syntax-snippets)))
            (,func ',(plist-get node :type)))))))
  ;; Can this be integrated into the loop?
  (define-key
    evil-tree-state-map
    (string-join (list key "p"))
    (cons
     "kill-ring"
     `(lambda ()
        (interactive)
        (,func (car kill-ring))))))

(defun evil-tree-edit--make-suppressed-keymap ()
  "Create a sparse keymap where keys default to undefined."
  (make-composed-keymap (make-sparse-keymap) evil-suppress-map))

(defun evil-tree-edit--set-state-bindings (mode)
  "Set keybindings for MODE in `evil-tree-state'.

Should only be used in the context of mode-local bindings, as
each language will have it's own set of nouns."
  (with-mode-local-symbol mode
    (mode-local-bind `((evil-tree-state-map . ,(evil-tree-edit--make-suppressed-keymap))) nil mode)
    (define-evil-tree-edit-verb "i" #'tree-edit-insert-sibling-before)
    (define-evil-tree-edit-verb "a" #'tree-edit-insert-sibling)
    (define-evil-tree-edit-verb "I" #'tree-edit-insert-child)
    (define-evil-tree-edit-verb "s" #'evil-tree-edit-avy-jump)
    (define-evil-tree-edit-verb "e" #'tree-edit-exchange-node)
    (define-evil-tree-edit-verb "w" #'tree-edit-wrap-node t)
    (define-key evil-tree-state-map [escape] 'evil-normal-state)
    (define-key evil-tree-state-map ">" #'tree-edit-slurp)
    (define-key evil-tree-state-map "<" #'tree-edit-barf)
    (define-key evil-tree-state-map "j" #'tree-edit-goto-next-sibling)
    (define-key evil-tree-state-map "k" #'tree-edit-goto-prev-sibling)
    (define-key evil-tree-state-map "h" #'tree-edit-goto-parent)
    (define-key evil-tree-state-map "f" #'tree-edit-goto-child)
    (define-key evil-tree-state-map "c" #'evil-tree-edit-change-node)
    (define-key evil-tree-state-map "d" #'tree-edit-delete-node)
    (define-key evil-tree-state-map "r" #'tree-edit-raise)
    (define-key evil-tree-state-map "y" #'tree-edit-copy)
    (define-key evil-tree-state-map "A" #'tree-edit-goto-sig-parent)))

;; TODO: allow configuring/disabling this
(evil-define-key 'normal evil-tree-edit-mode-map "Q" #'evil-tree-state)

(provide 'evil-tree-edit)
;;; evil-tree-edit.el ends here
