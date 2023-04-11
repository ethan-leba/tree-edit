;;; evil-tree-edit.el --- Evil structural editing for any language! -*- lexical-binding: t; -*-
;;
;; Copyright (C) Ethan Leba <https://github.com/ethan-leba>
;;
;; Author: Ethan Leba <ethanleba5@gmail.com>
;; Version: 0.1.0
;; Homepage: https://github.com/ethan-leba/tree-edit
;; Package-Requires: ((emacs "27.1") (tree-edit "0.1.0") (tree-sitter "0.15.0") (evil "1.0.0") (avy "0.5.0") (s "0.0.0"))
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
(defvar-local evil-tree-edit--current-node nil
  "The current node to apply editing commands to.")
(defvar-local evil-tree-edit--node-overlay nil
  "The display overlay to show the current node.")
(defvar-local evil-tree-edit--return-position nil
  "The location that the cursor should be returned to after exiting insert mode, if set.")

(defvar evil-tree-edit-pos-ring (make-ring 100)
  "Ring for node position history.")
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
(defcustom evil-tree-edit-disable-nontree-bindings nil
  "If non-nil, don't set the default normal and insert state bindings."
  :type 'boolean
  :group 'evil-tree-edit)

;;* Misc
(defun evil-tree-edit-current-node ()
  (when (and evil-tree-edit--current-node
             (treesit-node-check evil-tree-edit--current-node 'outdated))
    (ignore-errors
      (evil-tree-edit-set-current-node
       (tree-edit--node-from-steps evil-tree-edit--return-position))
      (run-hooks 'evil-tree-edit-after-change-hook)
      (evil-tree-edit--update-overlay)))
  evil-tree-edit--current-node)

(defun evil-tree-edit-set-current-node (val)
  (setq evil-tree-edit--current-node val))

;;* Navigation
(defmacro evil-tree-edit--preserve-location (&rest body)
  "Preserves the location of NODE during the execution of the BODY.

Optionally applies a MOVEMENT to the node after restoration,
moving the sibling index by the provided value."
  (declare (debug t))
  (let ((location-sym (gensym "location")))
    `(let ((,location-sym (tree-edit--node-steps (evil-tree-edit-current-node))))
       ,@body
       (run-hooks 'evil-tree-edit-after-change-hook)
       (evil-tree-edit-set-current-node
             (tree-edit--node-from-steps ,location-sym))
       (evil-tree-edit--update-overlay))))

(defun evil-tree-edit--preserve-current-node-before (_ __)
  "Save the location of the current node before the buffer is re-parsed."
  (when (evil-tree-state-p)
    (setq evil-tree-edit--return-position (tree-edit--node-steps (evil-tree-edit-current-node)))))

(defun evil-tree-edit--preserve-current-node-after ()
  "Restore the location of the current node after the buffer is re-parsed.

`tree-sitter-after-change-functions' provides an old-tree arg,
but it seems to not work reliably with `tree-edit--node-from-steps'."
  (when (evil-tree-state-p)
    (evil-tree-edit-current-node)))

(defun evil-tree-edit--apply-movement (fun)
  "Apply movement FUN, and then update the node position and display."
  (evil-tree-edit-ensure-current-node)
  (when-let ((new-pos (tree-edit--apply-until-interesting fun (evil-tree-edit-current-node))))
    (evil-tree-edit--goto-node new-pos)))

(defun evil-tree-edit--get-sig-parent (node)
  "Move NODE to the next (interesting) named sibling."
  (let ((parent (treesit-node-parent node)))
    (cond
     ((or (not parent)
          (not (treesit-node-parent parent))) node)
     ((treesit-node-eq parent (treesit-buffer-root-node)) node)
     ((--any (member (treesit-node-type parent)
                     (cons it (alist-get it tree-edit--subtypes '())))
             tree-edit-significant-node-types)
      parent)
     (t (evil-tree-edit--get-sig-parent parent)))))

(defun evil-tree-edit--remember ()
  "Store the current point and mark in history."
  (let* ((emptyp (zerop (ring-length evil-tree-edit-pos-ring)))
         (top (unless emptyp (ring-ref evil-tree-edit-pos-ring 0)))
         (pos (tree-edit--node-steps (evil-tree-edit-current-node))))
    (when (or emptyp (not (equal top pos)))
      (ring-insert evil-tree-edit-pos-ring pos))))

(defun evil-tree-edit--read-from-kill-ring (prompt)
  "Prompt for kill ring string with PROMPT in backwards compatible manner."
  (if (>= emacs-major-version 28)
      (read-from-kill-ring prompt)
    (completing-read prompt kill-ring)))

(defun evil-tree-edit-ensure-current-node ()
  "Error if `evil-tree-edit-current-node' is nil."
  (unless (evil-tree-edit-current-node)
    (user-error "`evil-tree-edit-current-node' is nil, are you in `evil-tree-state'?")))

;;* Globals: navigation
(defun evil-tree-edit-goto-next-sibling ()
  "Move to the next (interesting) named sibling."
  (interactive)
  (evil-tree-edit-ensure-current-node)
  (evil-tree-edit--apply-movement (lambda (node) (treesit-node-next-sibling node :named))))

(defun evil-tree-edit-goto-prev-sibling ()
  "Move to the previous (interesting) named sibling."
  (interactive)
  (evil-tree-edit-ensure-current-node)
  (evil-tree-edit--apply-movement (lambda (node) (treesit-node-prev-sibling node :named))))

(defun evil-tree-edit-goto-parent ()
  "Move up to the next interesting parent."
  (interactive)
  (evil-tree-edit-ensure-current-node)
  (evil-tree-edit--apply-movement #'treesit-node-parent))

(defun evil-tree-edit-goto-child ()
  "Move to the first child, unless it's an only child."
  (interactive)
  (evil-tree-edit-ensure-current-node)
  (evil-tree-edit--apply-movement (lambda (node) (treesit-node-child node 0 :named))))

(defun evil-tree-edit-goto-sig-parent ()
  "Move to the next (interesting) named sibling."
  (interactive)
  (evil-tree-edit--remember)
  (evil-tree-edit-ensure-current-node)
  (evil-tree-edit--apply-movement #'evil-tree-edit--get-sig-parent))

(defun evil-tree-edit-back ()
  "Set current node to the last remembered position."
  (interactive)
  (unless (zerop (ring-length evil-tree-edit-pos-ring))
    (-> (ring-remove evil-tree-edit-pos-ring 0)
        (tree-edit--node-from-steps)
        (evil-tree-edit--goto-node))))

;;* Evil tree-edit functions
(defun evil-tree-edit-change (&optional return-location)
  "Change the current node, and return to RETURN-LOCATION on exit.

If RETURN-NODE is unset, `evil-tree-edit-current-node' is used."
  (interactive)
  (evil-tree-edit-ensure-current-node)
  (setq evil-tree-edit--return-position
        (or return-location (tree-edit--node-steps (evil-tree-edit-current-node))))
  (evil-change-state 'insert)
  (delete-region (treesit-node-start (evil-tree-edit-current-node))
                 (treesit-node-end (evil-tree-edit-current-node))))

(defun evil-tree-edit-copy ()
  "Copy the current node."
  (interactive)
  (evil-tree-edit-ensure-current-node)
  (tree-edit-copy (evil-tree-edit-current-node)))

(defun evil-tree-edit-undo (count)
  "Undo COUNT actions while saving the cursor position."
  (interactive "*p")
  (evil-tree-edit-ensure-current-node)
  (evil-undo count))

(defun evil-tree-edit-append-sibling-placeholder-and-change ()
  "Add a placeholder node and then change it."
  (interactive)
  (evil-tree-edit-insert-sibling tree-edit-placeholder-node-type)
  (evil-tree-edit-change))

(defun evil-tree-edit-insert-child-placeholder-and-change ()
  "Add a placeholder node and then change it."
  (interactive)
  (evil-tree-edit-insert-child tree-edit-placeholder-node-type)
  (evil-tree-edit-change))

(defun evil-tree-edit-sig-avy-jump (node-type)
  "Avy jump to a node with the NODE-TYPE within scope of the nearest sig node.

NODE-TYPE can be a symbol or a list of symbol."
  (interactive)
  (evil-tree-edit-ensure-current-node)
  (evil-tree-edit--remember)
  (let ((query-node
         (if (member (treesit-node-type (evil-tree-edit-current-node))
                     tree-edit-significant-node-types)
             (evil-tree-edit-current-node)
           (evil-tree-edit--get-sig-parent (evil-tree-edit-current-node)))))
    (-> node-type
        (tree-edit--format-query-string)
        (tree-edit-query query-node)
        (evil-tree-edit--avy-jump))))

(defun evil-tree-edit-avy-jump (node-type)
  "Avy jump to a node with NODE-TYPE under the current node.

NODE-TYPE can be a symbol or a list of symbol."
  (interactive)
  (evil-tree-edit-ensure-current-node)
  (evil-tree-edit--remember)
  (-> node-type
      (tree-edit--format-query-string)
      (tree-edit-query (evil-tree-edit-current-node))
      (evil-tree-edit--avy-jump)))

(defun evil-tree-edit--goto-node (node)
  "Set current node to NODE and run hooks."
  (evil-tree-edit-set-current-node node)
  (run-hooks 'evil-tree-edit-movement-hook))

(defun evil-tree-edit-out (node-type/s)
  "Move outwards until a node of NODE-TYPE/S has been hit."
  (interactive)
  (evil-tree-edit-ensure-current-node)
  (evil-tree-edit--remember)
  (let ((new-node (evil-tree-edit-current-node))
        (node-types (if (listp node-type/s) node-type/s `(,node-type/s))))
    (while (and (treesit-node-parent new-node)
                (not (member (treesit-node-type new-node) node-types)))
      (setq new-node (treesit-node-parent new-node)))
    (if (not (treesit-node-parent new-node))
        (user-error "Current node has no parent of type %s" node-type/s)
      (evil-tree-edit--goto-node new-node))))

(defun evil-tree-edit--avy-jump (nodes)
  "Avy jump to one of NODES."
  (interactive)
  (evil-tree-edit-ensure-current-node)
  (let* ((window-top-boundary (window-start))
         (window-bottom-boundary (window-end))
         (position->node
          (--keep
           (let ((position (treesit-node-start it)))
             (if (>= window-bottom-boundary position window-top-boundary)
                 (cons position it)))
           nodes))
         ;; avy-action declares what should be done with the result of avy-process
         (avy-action (lambda (pos) (evil-tree-edit--goto-node (alist-get pos position->node)))))
    (cond ((not position->node) (user-error "Nothing to jump to!"))
          ((equal (length position->node) 1) (funcall avy-action (caar position->node)))
          (t (avy-process (-map #'car position->node))))))

(defun evil-tree-edit-wrap-node (type)
  "Wrap the current node in a node of selected TYPE."
  (evil-tree-edit-ensure-current-node)
  (evil-tree-edit--preserve-location
   (let ((node-text (treesit-node-text (evil-tree-edit-current-node)))
         (node-type (tree-edit--node-type (evil-tree-edit-current-node))))
     (tree-edit-cache-node (evil-tree-edit-current-node))
     (atomic-change-group
       (evil-tree-edit-exchange type)
       (evil-tree-edit--avy-jump
        (--filter (tree-edit--valid-replacement-p node-type it)
                  (tree-edit--all-named-descendants (evil-tree-edit-current-node))))
       (evil-tree-edit-exchange node-text)))))

(defun evil-tree-edit-exchange (type-or-text)
  "Exchange current node for TYPE-OR-TEXT.

See `tree-edit-exchange'."

  (interactive)
  (evil-tree-edit-ensure-current-node)
  (tree-edit-exchange type-or-text (evil-tree-edit-current-node)))

(defun evil-tree-edit-delete ()
  "Delete the current node.

See `tree-edit-delete'."
  (interactive)
  (evil-tree-edit-ensure-current-node)
  (tree-edit-delete (evil-tree-edit-current-node)))

(defun evil-tree-edit-move ()
  "Copy then delete the current node.

See `tree-edit-delete'."
  (interactive)
  (evil-tree-edit-ensure-current-node)
  (tree-edit-copy (evil-tree-edit-current-node))
  (tree-edit-delete (evil-tree-edit-current-node)))

(defun evil-tree-edit-raise ()
  "Move the current node up the syntax tree until a valid replacement is found.

See `tree-edit-raise'."
  (interactive)
  (evil-tree-edit-ensure-current-node)
  (let ((raised-node (tree-edit-raise (evil-tree-edit-current-node))))
    (evil-tree-edit-set-current-node raised-node))
  (evil-tree-edit--update-overlay))

(defun evil-tree-edit-insert-sibling (type-or-text &optional before)
  "Insert a node of the given TYPE-OR-TEXT next to the current node.

if BEFORE is t, the sibling node will be inserted before the
current, otherwise after.

See `tree-edit-insert-sibling'."
  (interactive)
  (evil-tree-edit-ensure-current-node)
  (tree-edit-insert-sibling type-or-text (evil-tree-edit-current-node) before)
  (unless before
    (evil-tree-edit-goto-next-sibling)))

(defun evil-tree-edit-insert-sibling-before (type)
  "Insert a node of the given TYPE before the current."
  (evil-tree-edit-insert-sibling type :before))

(defun evil-tree-edit-insert-child (type-or-text)
  "Insert a node of the given TYPE-OR-TEXT inside of the current node."
  (interactive)
  (evil-tree-edit-ensure-current-node)
  (tree-edit-insert-child type-or-text (evil-tree-edit-current-node))
  (evil-tree-edit-goto-child))

(defun evil-tree-edit-slurp ()
  "Transform current node's next sibling into it's leftmost child, if possible."
  (interactive)
  (evil-tree-edit-ensure-current-node)
  (tree-edit-slurp (evil-tree-edit-current-node)))

(defun evil-tree-edit-barf ()
  "Transform current node's leftmost child into it's next sibling, if possible."
  (interactive)
  (evil-tree-edit-ensure-current-node)
  (tree-edit-barf (evil-tree-edit-current-node)))

(defun evil-tree-edit-goto-next-placeholder ()
  "Move cursor to the next placeholder node.

Placeholder is defined by `tree-edit-placeholder-node-type'."
  (interactive)
  (evil-tree-edit-ensure-current-node)
  (unless tree-edit-placeholder-node-type
    (user-error "`tree-edit-placeholder-node-type' not set!"))
  (pcase (tree-edit-query
          (format "((%s) (#equal @node %s))"
                  (tree-edit--format-query-string
                   (cons tree-edit-placeholder-node-type
                         (tree-edit-all-aliases-for-type tree-edit-placeholder-node-type)))
                  ;; XXX: Assuming the placeholder type is a singleton list containing a string
                  (car (alist-get tree-edit-placeholder-node-type tree-edit-syntax-snippets)))
          (evil-tree-edit-current-node)
          :want-text t)
    (`(,first . ,_) (evil-tree-edit--goto-node first))
    (_ (user-error "No placeholders contained in the current!"))))

(defun evil-tree-edit-change-next-placeholder ()
  "Move cursor to the next placeholder node and change it."
  (interactive)
  (evil-tree-edit-ensure-current-node)
  (let ((current-location (tree-edit--node-steps (evil-tree-edit-current-node))))
    (evil-tree-edit-goto-next-placeholder)
    (evil-tree-edit-change current-location)))

(defun evil-tree-edit-change-next-placeholder-from-insert ()
  "Complete edit of current node and change the next placeholder node."
  (interactive)
  (evil-tree-edit-normal-or-tree-state)
  (evil-tree-edit-change-next-placeholder))

(defun evil-tree-edit-node-info ()
  "Preview the different variations of the current node."
  (interactive)
  (cond
   (current-prefix-arg (evil-tree-edit-preview-node))
   (t (evil-tree-edit-ensure-current-node)
      (message "Current node type is '%s', bound to key '%s'."
               (treesit-node-type (evil-tree-edit-current-node))
               (--any
                (when (member (treesit-node-type (evil-tree-edit-current-node))
                              (if (listp (plist-get it :type))
                                  (plist-get it :type)
                                `(,(plist-get it :type))))
                  (plist-get it :key))
                tree-edit-nodes)))))

(defun evil-tree-edit-preview-node ()
  "Preview the different variations of the current node."
  (interactive)
  (evil-tree-edit-ensure-current-node)
  (let ((reazon-occurs-check nil)
        (reazon-timeout 0.1)
        (tree-edit-parse-comments nil))
    (--> (evil-tree-edit-current-node)
         (treesit-node-type it)
         (alist-get it tree-edit-grammar)
         ;; TODO: Parametrize
         (reazon-run 10 q (tree-edit-parseo it q '()))
         ;; Sometime parser returns repeats, not sure if that's expected
         (-uniq it)
         ;; TODO: Prettier print
         (message (string-join (-map #'prin1-to-string it) "\n")))))

(defun evil-tree-edit-yank ()
  "Exchange the current node with the top of the kill ring."
  (interactive)
  (evil-tree-edit-exchange (car kill-ring)))

(defun evil-tree-edit-yank-pop ()
  "Exchange the current node with a selection from the kill-ring."
  (interactive)
  (evil-tree-edit-exchange (evil-tree-edit--read-from-kill-ring "Exchange node: ")))

(defun evil-tree-edit-clone ()
  "Insert a copy of the current node."
  (interactive)
  (let ((node-text (treesit-node-text (evil-tree-edit-current-node))))
    (tree-edit-cache-node (evil-tree-edit-current-node))
    (evil-tree-edit-insert-sibling node-text)))

(defun evil-tree-edit--treesit--explorer-refresh-advice (func &rest args)
  "If in `evil-tree-state', set the region to the range of the current node.

This is so that the current node will be properly highlighted in explorer mode."
  (if (not (eq evil-state 'tree)) (apply func args)
    (save-mark-and-excursion
      (goto-char (treesit-node-start (evil-tree-edit-current-node)))
      (set-mark (treesit-node-end (evil-tree-edit-current-node)))
      (apply func args))))

(advice-add 'treesit--explorer-refresh :around
            #'evil-tree-edit--treesit--explorer-refresh-advice)

(defun evil-tree-edit-toggle-tree-view ()
  "Toggle `evil-tree-edit-view-mode'."
  (interactive)
  (evil-tree-edit-ensure-current-node)
  (treesit-explore-mode))

(defun evil-tree-edit-select-in-visual-state ()
  "Move to visual state with the current node as the selection."
  (interactive)
  (evil-tree-edit-ensure-current-node)
  (evil-visual-select (treesit-node-start (evil-tree-edit-current-node))
                      (1- (treesit-node-end (evil-tree-edit-current-node)))))


(defun evil-tree-edit--ambiguous-node-range-p (node-a node-b)
  "Do NODE-A and NODE-B share the same range?"
  (and node-a node-b
       (equal (treesit-node-start node-a)
              (treesit-node-start node-b))
       (equal (treesit-node-end node-a)
              (treesit-node-end node-b))))

;;* Mode and evil state definitions
(defun evil-tree-edit--update-overlay ()
  "Update the display of the current selected node, and move the cursor."
  (move-overlay evil-tree-edit--node-overlay
                (or (treesit-node-start (evil-tree-edit-current-node)) (point-min))
                (or (treesit-node-end (evil-tree-edit-current-node)) (point-max)))
  (goto-char (treesit-node-start (evil-tree-edit-current-node)))
  (if (or (evil-tree-edit--ambiguous-node-range-p
           (treesit-node-parent (evil-tree-edit-current-node))
           (evil-tree-edit-current-node))
          (evil-tree-edit--ambiguous-node-range-p
           (treesit-node-child (evil-tree-edit-current-node) 0)
           (evil-tree-edit-current-node)))
      (overlay-put evil-tree-edit--node-overlay 'after-string
                   (propertize
                    (let ((type (treesit-node-type (evil-tree-edit-current-node))))
                      (s-concat " " (if (stringp type) type (symbol-name type))))
                    ;; TODO: Abstract into var
                    'face '(italic :foreground "dark gray")))
    (overlay-put evil-tree-edit--node-overlay 'after-string "")))

(defun evil-tree-edit--enter-tree-state ()
  "Activate tree-edit state."
  (unless evil-tree-edit--node-overlay
    (setq evil-tree-edit--node-overlay (make-overlay 0 0)))
  (let ((node (treesit-node-descendant-for-range
               (treesit-buffer-root-node) (point) (point))))
    (evil-tree-edit-set-current-node
          (if (tree-edit--boring-nodep node)
              (tree-edit--apply-until-interesting #'treesit-node-parent node)
            node)))
  (overlay-put evil-tree-edit--node-overlay 'face 'region)
  (evil-tree-edit--update-overlay))

(defun evil-tree-edit--exit-tree-state ()
  "De-activate tree-edit state."
  (unless (eq evil-next-state 'insert)
    (setq evil-tree-edit--return-position nil))
  (when evil-tree-edit--node-overlay
    (overlay-put evil-tree-edit--node-overlay 'after-string "")
    (overlay-put evil-tree-edit--node-overlay 'face '())))

(defun evil-tree-edit-normal-or-tree-state ()
  "Enter normal or tree state contextually."
  (interactive)
  (if evil-tree-edit--return-position
      (progn
        (evil-tree-state)
        (evil-tree-edit--goto-node
         (or (tree-edit--node-from-steps-strict evil-tree-edit--return-position)
             (progn
               (message "Could not restore node position, selecting at point.")
               (treesit-node-descendant-for-range
                ;; Entering normal state will put the point before the selected
                ;; text, so we increment it by one.
                (treesit-buffer-root-node) (1+ (point)) (1+ (point))))))
        (setq evil-tree-edit--return-position nil))
    (evil-normal-state)))

(defun evil-tree-edit--teardown ()
  "De-activate tree-edit state."
  (when evil-tree-edit--node-overlay
    (delete-overlay evil-tree-edit--node-overlay)))

(evil-define-state tree
  "evil-tree-edit state"
  :tag " <T>"
  :cursor 'hollow
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
    (tree-edit-load-grammar-for-major-mode)
    (evil-tree-edit-set-state-bindings major-mode)
    ;; HACK: Above mode binding won't come into effect until the state is changed.
    (evil-normal-state)
    (add-hook 'before-revert-hook #'evil-tree-edit--teardown nil 'local)
    ;; TODO: can we just run these on load?
    (add-hook 'post-command-hook #'evil-tree-edit--preserve-current-node-after nil 'local)
    (add-hook 'before-change-functions #'evil-tree-edit--preserve-current-node-before nil 'local)
    (add-hook 'evil-tree-edit-movement-hook #'evil-tree-edit--update-overlay nil 'local))
   (t
    (remove-hook 'post-command-hook #'evil-tree-edit--preserve-current-node-after 'local)
    (remove-hook 'before-revert-hook #'evil-tree-edit--teardown 'local)
    (remove-hook 'before-change-functions #'evil-tree-edit--preserve-current-node-before 'local))))

(defun define-evil-tree-edit-avy-jump (keymap key func)
  "Define a key command in KEYMAP prefixed by KEY calling FUNC.

FUNC must take one argument, a symbol of the node type."
  (dolist (node (append tree-edit-nodes tree-edit-query-nodes))
    (define-key
      keymap
      (string-join (list key (plist-get node :key)))
      (cons
       ;; emacs-which-key integration
       (cond ((plist-get node :name) (plist-get node :name))
             ((listp (plist-get node :type)) (s-join "/" (--map (s-replace "_" " " (symbol-name it)) (plist-get node :type))))
             (t (s-replace "_" " " (symbol-name (plist-get node :type)))))
       `(lambda ()
          (interactive)
          (,func ',(plist-get node :type)))))))

(defun define-evil-tree-edit-verb (keymap key func &optional wrap)
  "Define a key command in KEYMAP prefixed by KEY calling FUNC.

FUNC must take one argument, a symbol of the node type.
If WRAP is t, include :wrap-override."
  (dolist (node tree-edit-nodes)
    (define-key
      keymap
      (string-join (list key (plist-get node :key)))
      (cons
       ;; emacs-which-key integration
       (cond ((plist-get node :name) (plist-get node :name))
             ((listp (plist-get node :type)) (s-join "/" (--map (s-replace "_" " " (symbol-name it)) (plist-get node :type))))
             (t (s-replace "_" " " (symbol-name (plist-get node :type)))))
       `(lambda ()
          (interactive)
          (let ((tree-edit-syntax-snippets
                 (append ,(plist-get node :node-override)
                         ,(if wrap (plist-get node :wrap-override))
                         tree-edit-syntax-snippets)))
            (,func ',(plist-get node :type)))))))
  ;; Can this be integrated into the loop?
  (define-key
    keymap
    (string-join (list key "p"))
    (cons
     "yank"
     `(lambda ()
        (interactive)
        (,func (car kill-ring)))))
  (define-key
    keymap
    (string-join (list key "P"))
    (cons
     "yank-pop"
     `(lambda ()
        (interactive)
        (,func (evil-tree-edit--read-from-kill-ring "Kill-ring: "))))))

(defun evil-tree-edit-set-state-bindings (mode)
  "Set keybindings for MODE in `evil-tree-state'.

Should only be used in the context of mode-local bindings, as
each language will have it's own set of nouns."
  (with-mode-local-symbol mode
    (let ((mode-local-keymap (make-composed-keymap (make-sparse-keymap) evil-suppress-map)))
      (define-evil-tree-edit-verb mode-local-keymap "i" #'evil-tree-edit-insert-sibling-before)
      (define-evil-tree-edit-verb mode-local-keymap "a" #'evil-tree-edit-insert-sibling)
      (define-evil-tree-edit-verb mode-local-keymap "I" #'evil-tree-edit-insert-child)
      (define-evil-tree-edit-verb mode-local-keymap "e" #'evil-tree-edit-exchange)
      (define-evil-tree-edit-verb mode-local-keymap "w" #'evil-tree-edit-wrap-node t)
      (define-evil-tree-edit-avy-jump mode-local-keymap "s" #'evil-tree-edit-avy-jump)
      (define-evil-tree-edit-avy-jump mode-local-keymap "q" #'evil-tree-edit-sig-avy-jump)
      (define-evil-tree-edit-avy-jump mode-local-keymap "o" #'evil-tree-edit-out)
      (define-key mode-local-keymap [escape] 'evil-normal-state)
      (define-key mode-local-keymap ">" #'evil-tree-edit-slurp)
      (define-key mode-local-keymap "<" #'evil-tree-edit-barf)
      (define-key mode-local-keymap "j" #'evil-tree-edit-goto-next-sibling)
      (define-key mode-local-keymap "k" #'evil-tree-edit-goto-prev-sibling)
      (define-key mode-local-keymap "h" #'evil-tree-edit-goto-parent)
      (define-key mode-local-keymap "f" #'evil-tree-edit-goto-child)
      (define-key mode-local-keymap "b" #'evil-tree-edit-back)
      (define-key mode-local-keymap "x" #'evil-tree-edit-append-sibling-placeholder-and-change)
      (define-key mode-local-keymap "X" #'evil-tree-edit-insert-child-placeholder-and-change)
      (define-key mode-local-keymap "n" #'evil-tree-edit-goto-next-placeholder)
      (define-key mode-local-keymap "N" #'evil-tree-edit-change-next-placeholder)
      (define-key mode-local-keymap "c" #'evil-tree-edit-change)
      (define-key mode-local-keymap "d" #'evil-tree-edit-delete)
      (define-key mode-local-keymap "m" #'evil-tree-edit-move)
      (define-key mode-local-keymap "r" #'evil-tree-edit-raise)
      (define-key mode-local-keymap "y" #'evil-tree-edit-copy)
      (define-key mode-local-keymap "p" #'evil-tree-edit-yank)
      (define-key mode-local-keymap "P" #'evil-tree-edit-yank-pop)
      (define-key mode-local-keymap "C" #'evil-tree-edit-clone)
      (define-key mode-local-keymap "u" #'evil-tree-edit-undo)
      (define-key mode-local-keymap "A" #'evil-tree-edit-goto-sig-parent)
      (define-key mode-local-keymap "?" #'evil-tree-edit-node-info)
      (define-key mode-local-keymap "v" #'evil-tree-edit-select-in-visual-state)
      (define-key mode-local-keymap "V" #'evil-tree-edit-toggle-tree-view)
      (define-key mode-local-keymap "zz" #'evil-scroll-line-to-center)
      ;; `setq-mode-local' macroexpanded, since it doesn't accept symbols
      (mode-local-bind `((evil-tree-state-map . ,mode-local-keymap)) '(mode-variable-flag t) mode)
      (mode-local-map-mode-buffers
       (lambda () (set (make-local-variable 'evil-tree-state-map) mode-local-keymap))
       mode))))

(unless evil-tree-edit-disable-nontree-bindings
  (evil-define-key 'normal evil-tree-edit-mode-map "Q" #'evil-tree-state)
  (evil-define-key 'insert evil-tree-edit-mode-map (kbd "<escape>") #'evil-tree-edit-normal-or-tree-state)
  (evil-define-key 'insert evil-tree-edit-mode-map (kbd "C-<return>") #'evil-tree-edit-change-next-placeholder-from-insert))

(provide 'evil-tree-edit)
;;; evil-tree-edit.el ends here
