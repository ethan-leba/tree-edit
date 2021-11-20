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
(defvar-local evil-tree-edit-current-node nil
  "The current node to apply editing commands to.")
(defvar-local evil-tree-edit--node-overlay nil
  "The display overlay to show the current node.")
(defvar-local evil-tree-edit--return-to-tree-state nil
  "Whether tree state should be returned to after exiting insert mode.")

(defgroup evil-tree-edit nil
  "Evil structural editing for tree-sitter languages."
  :group 'bindings
  :prefix "evil-tree-edit-")
(defcustom evil-tree-edit-movement-hook nil
  "Functions to call after a tree-edit movement command has been issued."
  :type 'hook
  :group 'evil-tree-edit)
(defcustom evil-tree-edit-after-change-hook nil
  "Functions to call after a tree-edit command modifies the buffer."
  :type 'hook
  :group 'evil-tree-edit)

;;* Navigation
(defmacro evil-tree-edit--preserve-location (&rest body)
  "Preserves the location of NODE during the execution of the BODY.

Optionally applies a MOVEMENT to the node after restoration,
moving the sibling index by the provided value."
  (declare (debug t))
  (let ((location-sym (gensym "location")))
    `(let ((,location-sym (tree-edit--save-location evil-tree-edit-current-node)))
       ,@body
       (run-hooks 'evil-tree-edit-after-change-hook)
       (setq evil-tree-edit-current-node
             (tree-edit--restore-location ,location-sym))
       (evil-tree-edit--update-overlay))))

(defun evil-tree-edit--apply-movement (fun)
  "Apply movement FUN, and then update the node position and display."
  (when-let ((new-pos (tree-edit--apply-until-interesting fun evil-tree-edit-current-node)))
    (setq evil-tree-edit-current-node new-pos)
    (run-hooks 'evil-tree-edit-movement-hook)))

(defun evil-tree-edit--goto-sig-parent (node)
  "Move NODE to the next (interesting) named sibling."
  (let ((parent (tsc-get-parent node)))
    (cond
     ((not parent) (user-error "No significant node past the current!"))
     ((member (tsc-node-type parent) tree-edit-significant-node-types) parent)
     (t (evil-tree-edit--goto-sig-parent parent)))))

;;* Globals: navigation
(defun evil-tree-edit-goto-next-sibling ()
  "Move to the next (interesting) named sibling."
  (interactive)
  (evil-tree-edit--apply-movement #'tsc-get-next-named-sibling))

(defun evil-tree-edit-goto-prev-sibling ()
  "Move to the previous (interesting) named sibling."
  (interactive)
  (evil-tree-edit--apply-movement #'tsc-get-prev-named-sibling))

(defun evil-tree-edit-goto-parent ()
  "Move up to the next interesting parent."
  (interactive)
  (evil-tree-edit--apply-movement #'tsc-get-parent))

(defun evil-tree-edit-goto-child ()
  "Move to the first child, unless it's an only child."
  (interactive)
  (evil-tree-edit--apply-movement (lambda (node) (tsc-get-nth-named-child node 0))))

(defun evil-tree-edit-goto-sig-parent ()
  "Move to the next (interesting) named sibling."
  (interactive)
  (evil-tree-edit--apply-movement #'evil-tree-edit--goto-sig-parent))

;;* Evil tree-edit functions
(defun evil-tree-edit-change ()
  "Change the current node."
  (interactive)
  (setq evil-tree-edit--return-to-tree-state (tree-edit--save-location evil-tree-edit-current-node))
  (delete-region (tsc-node-start-position evil-tree-edit-current-node)
                 (tsc-node-end-position evil-tree-edit-current-node))
  (evil-change-state 'insert))

(defun evil-tree-edit-copy ()
  "Copy the current node."
  (interactive)
  (kill-ring-save (tsc-node-start-position evil-tree-edit-current-node)
                  (tsc-node-end-position evil-tree-edit-current-node)))

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
                                    (equal pos (tsc-node-start-position evil-tree-edit-current-node)))
                                  (tree-edit-query query-string evil-tree-edit-current-node))))
         ;; avy-action declares what should be done with the result of avy-process
         (avy-action (lambda (pos)
                       (setq evil-tree-edit-current-node (alist-get pos position->node))
                       (run-hooks 'evil-tree-edit-movement-hook))))
    (cond ((not position->node) (user-error "Nothing to jump to!"))
          ((equal (length position->node) 1) (funcall avy-action (caar position->node)))
          (t (avy-process (-map #'car position->node))))))

(defun evil-tree-edit-wrap-node (type)
  "Wrap the current node in a node of selected TYPE."
  (evil-tree-edit--preserve-location
    (let ((node-text (tsc-node-text evil-tree-edit-current-node)))
      (evil-tree-edit-exchange type)
      (unwind-protect
          (evil-tree-edit-avy-jump (alist-get type tree-edit--supertypes)
                                   (-lambda ((_ . node)) (tree-edit--valid-replacement-p type node)))
        ;; If avy fails, replace the old node back
        (evil-tree-edit-exchange node-text)))))

(defun evil-tree-edit-exchange (type-or-text)
  "Exchange current node for TYPE-OR-TEXT.

See `tree-edit-exchange'."

  (interactive)
  (evil-tree-edit--preserve-location
    (tree-edit-exchange type-or-text evil-tree-edit-current-node)))

(defun evil-tree-edit-delete ()
  "Delete the current node.

See `tree-edit-delete'."
  (interactive)
  (evil-tree-edit--preserve-location
    (tree-edit-delete evil-tree-edit-current-node)))

(defun evil-tree-edit-raise ()
  "Move the current node up the syntax tree until a valid replacement is found.

See `tree-edit-raise'."
  (interactive)
  (let ((raised-node (tree-edit-raise evil-tree-edit-current-node)))
    (setq evil-tree-edit-current-node raised-node)))

(defun evil-tree-edit-insert-sibling (type-or-text &optional before)
  "Insert a node of the given TYPE-OR-TEXT next to the current node.

if BEFORE is t, the sibling node will be inserted before the
current, otherwise after.

See `tree-edit-insert-sibling'."
  (interactive)
  (evil-tree-edit--preserve-location
    (tree-edit-insert-sibling type-or-text evil-tree-edit-current-node before))
  (unless before
    (evil-tree-edit-goto-next-sibling)))

(defun evil-tree-edit-insert-sibling-before (type)
  "Insert a node of the given TYPE before the current."
  (evil-tree-edit-insert-sibling type t))

(defun evil-tree-edit-insert-child (type-or-text)
  "Insert a node of the given TYPE-OR-TEXT inside of the current node."
  (interactive)
  (evil-tree-edit--preserve-location
    (tree-edit-insert-child type-or-text evil-tree-edit-current-node))
  (evil-tree-edit-goto-child))

(defun evil-tree-edit-slurp ()
  "Transform current node's next sibling into it's leftmost child, if possible."
  (interactive)
  (evil-tree-edit--preserve-location
    (tree-edit-slurp evil-tree-edit-current-node)))

(defun evil-tree-edit-barf ()
  "Transform current node's leftmost child into it's next sibling, if possible."
  (interactive)
  (evil-tree-edit--preserve-location
      (tree-edit-barf evil-tree-edit-current-node)))

;;* Mode and evil state definitions
(defun evil-tree-edit--update-overlay ()
  "Update the display of the current selected node, and move the cursor."
  (move-overlay evil-tree-edit--node-overlay
                (tsc-node-start-position evil-tree-edit-current-node)
                (tsc-node-end-position evil-tree-edit-current-node))
  (goto-char (tsc-node-start-position evil-tree-edit-current-node)))

(defun evil-tree-edit--enter-tree-state ()
  "Activate tree-edit state."
  (unless evil-tree-edit--node-overlay
    (setq evil-tree-edit--node-overlay (make-overlay 0 0)))
  (let ((node (tsc-get-descendant-for-position-range
               (tsc-root-node tree-sitter-tree) (point) (point))))
    (setq evil-tree-edit-current-node
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
    (tree-edit--restore-location evil-tree-edit--return-to-tree-state)
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
    ;; TODO: can we just run these on load?
    (add-hook 'evil-tree-edit-movement-hook #'evil-tree-edit--update-overlay nil 'local)
    (add-hook 'evil-normal-state-entry-hook #'evil-tree-edit--re-enter-tree-state nil 'local))
   (t
    (remove-hook 'before-revert-hook #'evil-tree-edit-teardown 'local)
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
    (define-evil-tree-edit-verb "i" #'evil-tree-edit-insert-sibling-before)
    (define-evil-tree-edit-verb "a" #'evil-tree-edit-insert-sibling)
    (define-evil-tree-edit-verb "I" #'evil-tree-edit-insert-child)
    (define-evil-tree-edit-verb "s" #'evil-tree-edit-avy-jump)
    (define-evil-tree-edit-verb "e" #'evil-tree-edit-exchange)
    (define-evil-tree-edit-verb "w" #'evil-tree-edit-wrap-node t)
    (define-key evil-tree-state-map [escape] 'evil-normal-state)
    (define-key evil-tree-state-map ">" #'evil-tree-edit-slurp)
    (define-key evil-tree-state-map "<" #'evil-tree-edit-barf)
    (define-key evil-tree-state-map "j" #'evil-tree-edit-goto-next-sibling)
    (define-key evil-tree-state-map "k" #'evil-tree-edit-goto-prev-sibling)
    (define-key evil-tree-state-map "h" #'evil-tree-edit-goto-parent)
    (define-key evil-tree-state-map "f" #'evil-tree-edit-goto-child)
    (define-key evil-tree-state-map "c" #'evil-tree-edit-change)
    (define-key evil-tree-state-map "d" #'evil-tree-edit-delete)
    (define-key evil-tree-state-map "r" #'evil-tree-edit-raise)
    (define-key evil-tree-state-map "y" #'evil-tree-edit-copy)
    (define-key evil-tree-state-map "A" #'evil-tree-edit-goto-sig-parent)))

;; TODO: allow configuring/disabling this
(evil-define-key 'normal evil-tree-edit-mode-map "Q" #'evil-tree-state)

(provide 'evil-tree-edit)
;;; evil-tree-edit.el ends here
