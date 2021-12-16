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
(defvar-local evil-tree-edit-current-node nil
  "The current node to apply editing commands to.")
(defvar-local evil-tree-edit--node-overlay nil
  "The display overlay to show the current node.")
(defvar-local evil-tree-edit--return-position nil
  "The location that the cursor should be returned to after exiting insert mode, if set.")

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

(defun evil-tree-edit--preserve-current-node-before (_ __)
  "Save the location of the current node before the buffer is re-parsed."
  (when (evil-tree-state-p)
    (ignore-errors
      (setq evil-tree-edit--return-position (tree-edit--save-location evil-tree-edit-current-node)))))

(defun evil-tree-edit--preserve-current-node-after (_)
  "Restore the location of the current node after the buffer is re-parsed.

`tree-sitter-after-change-functions' provides an old-tree arg,
but it seems to not work reliably with `tree-edit--save-location'."
  (when (and evil-tree-edit-current-node evil-tree-edit--return-position (evil-tree-state-p))
    (ignore-errors
      (setq evil-tree-edit-current-node (tree-edit--restore-location evil-tree-edit--return-position))
      (run-hooks 'evil-tree-edit-after-change-hook)
      (evil-tree-edit--update-overlay))))

(defun evil-tree-edit--apply-movement (fun)
  "Apply movement FUN, and then update the node position and display."
  (when-let ((new-pos (tree-edit--apply-until-interesting fun evil-tree-edit-current-node)))
    (evil-tree-edit--goto-node new-pos)))

(defun evil-tree-edit--get-sig-parent (node)
  "Move NODE to the next (interesting) named sibling."
  (let ((parent (tsc-get-parent node)))
    (cond
     ((tsc-node-eq parent (tsc-root-node tree-sitter-tree)) node)
     ((--any (member (tsc-node-type parent)
                     (cons it (alist-get it tree-edit--subtypes '())))
             tree-edit-significant-node-types)
      parent)
     (t (evil-tree-edit--get-sig-parent parent)))))

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
  (evil-tree-edit--apply-movement #'evil-tree-edit--get-sig-parent))

;;* Evil tree-edit functions
(defun evil-tree-edit-change (&optional return-location)
  "Change the current node, and return to RETURN-LOCATION on exit.

If RETURN-NODE is unset, `evil-tree-edit-current-node' is used."
  (interactive)
  (setq evil-tree-edit--return-position
        (or return-location (tree-edit--save-location evil-tree-edit-current-node)))
  (evil-change-state 'insert)
  (delete-region (tsc-node-start-position evil-tree-edit-current-node)
                 (tsc-node-end-position evil-tree-edit-current-node)))

(defun evil-tree-edit-copy ()
  "Copy the current node."
  (interactive)
  (kill-ring-save (tsc-node-start-position evil-tree-edit-current-node)
                  (tsc-node-end-position evil-tree-edit-current-node)))

(defun evil-tree-edit-undo (count)
  "Undo COUNT actions while saving the cursor position."
  (interactive "*p")
  (evil-undo count))

(defun evil-tree-edit--format-query-string (node-type)
  "Format a query string for NODE-TYPE.

NODE-TYPE can be a symbol or a list of symbol."
  (interactive)
  (--> (if (listp node-type) node-type `(,node-type))
       (-map (lambda (type) (format "(%s)" type)) it)
       (string-join it " ")
       ;; Query string needs an @name here, or it won't return any results
       (format "[%s] @foo" it)))

(defun evil-tree-edit-sig-avy-jump (node-type)
  "Avy jump to a node with the NODE-TYPE within scope of the nearest sig node.

NODE-TYPE can be a symbol or a list of symbol."
  (interactive)
  (let ((query-node
         (if (member (tsc-node-type evil-tree-edit-current-node)
                     tree-edit-significant-node-types)
             evil-tree-edit-current-node
           (evil-tree-edit--get-sig-parent evil-tree-edit-current-node))))
    (-> node-type
        (evil-tree-edit--format-query-string)
        (tree-edit-query query-node)
        (evil-tree-edit--avy-jump))))

(defun evil-tree-edit-avy-jump (node-type)
  "Avy jump to a node with NODE-TYPE under the current node.

NODE-TYPE can be a symbol or a list of symbol."
  (interactive)
  (-> node-type
      (evil-tree-edit--format-query-string)
      (tree-edit-query evil-tree-edit-current-node)
      (evil-tree-edit--avy-jump)))

(defun evil-tree-edit--goto-node (node)
  "Set current node to NODE and run hooks."
  (setq evil-tree-edit-current-node node)
  (run-hooks 'evil-tree-edit-movement-hook))

(defun evil-tree-edit--avy-jump (nodes)
  "Avy jump to one of NODES."
  (interactive)
  (let* ((position->node (--map (cons (tsc-node-start-position it) it) nodes))
         ;; avy-action declares what should be done with the result of avy-process
         (avy-action (lambda (pos) (evil-tree-edit--goto-node (alist-get pos position->node)))))
    (cond ((not position->node) (user-error "Nothing to jump to!"))
          ((equal (length position->node) 1) (funcall avy-action (caar position->node)))
          (t (avy-process (-map #'car position->node))))))

(defun evil-tree-edit--all-named-descendants (node)
  "Retrieve all named descendants of NODE."
  ;; Use cursor for efficiency?
  (let (result (stack `(,node)))
    (while stack
      (let* ((item (pop stack))
             (children (tree-edit--get-all-children item)))
        (setq stack (append children stack))
        (setq result (append result children))))
    result))

(defun evil-tree-edit-wrap-node (type)
  "Wrap the current node in a node of selected TYPE."
  (evil-tree-edit--preserve-location
   (let* ((node-type (tsc-node-type evil-tree-edit-current-node))
          (node-text (tsc-node-text evil-tree-edit-current-node)))
     (evil-tree-edit-exchange type)
     (unwind-protect
         (evil-tree-edit--avy-jump
          (-filter (lambda (node) (--any (tree-edit--valid-replacement-p (tsc-node-type it) node)
                                    (tree-edit--parse-fragment node-text)))
                   (evil-tree-edit--all-named-descendants evil-tree-edit-current-node)))
       ;; If avy fails, replace the old node back
       (evil-tree-edit-exchange node-text)))))

(defun evil-tree-edit-exchange (type-or-text)
  "Exchange current node for TYPE-OR-TEXT.

See `tree-edit-exchange'."

  (interactive)
  (tree-edit-exchange type-or-text evil-tree-edit-current-node))

(defun evil-tree-edit-delete ()
  "Delete the current node.

See `tree-edit-delete'."
  (interactive)
  (tree-edit-delete evil-tree-edit-current-node))

(defun evil-tree-edit-raise ()
  "Move the current node up the syntax tree until a valid replacement is found.

See `tree-edit-raise'."
  (interactive)
  (let ((raised-node (tree-edit-raise evil-tree-edit-current-node)))
    (setq evil-tree-edit-current-node raised-node))
  (evil-tree-edit--update-overlay))

(defun evil-tree-edit-insert-sibling (type-or-text &optional before)
  "Insert a node of the given TYPE-OR-TEXT next to the current node.

if BEFORE is t, the sibling node will be inserted before the
current, otherwise after.

See `tree-edit-insert-sibling'."
  (interactive)
  (tree-edit-insert-sibling type-or-text evil-tree-edit-current-node before)
  (unless before
    (evil-tree-edit-goto-next-sibling)))

(defun evil-tree-edit-insert-sibling-before (type)
  "Insert a node of the given TYPE before the current."
  (evil-tree-edit-insert-sibling type t))

(defun evil-tree-edit-insert-child (type-or-text)
  "Insert a node of the given TYPE-OR-TEXT inside of the current node."
  (interactive)
  (tree-edit-insert-child type-or-text evil-tree-edit-current-node)
  (evil-tree-edit-goto-child))

(defun evil-tree-edit-slurp ()
  "Transform current node's next sibling into it's leftmost child, if possible."
  (interactive)
  (tree-edit-slurp evil-tree-edit-current-node))

(defun evil-tree-edit-barf ()
  "Transform current node's leftmost child into it's next sibling, if possible."
  (interactive)
  (tree-edit-barf evil-tree-edit-current-node))

(defun evil-tree-edit-goto-next-placeholder ()
  "Move cursor to the next placeholder node.

Placeholder is defined by `tree-edit-placeholder-node-type'."
  (interactive)
  (unless tree-edit-placeholder-node-type
    (user-error "`tree-edit-placeholder-node-type' not set!"))
  (pcase (tree-edit-query
          (format "((%s) @node (.eq? @node %s))"
                  tree-edit-placeholder-node-type
                  ;; XXX: Assuming the placeholder type is a singleton list containing a string
                  (car (alist-get tree-edit-placeholder-node-type tree-edit-syntax-snippets)))
          evil-tree-edit-current-node)
    (`(,first . ,_) (evil-tree-edit--goto-node first))
    (_ (user-error "No placeholders contained in the current!"))))

(defun evil-tree-edit-change-next-placeholder ()
  "Move cursor to the next placeholder node and change it."
  (interactive)
  (let ((current-location (tree-edit--save-location evil-tree-edit-current-node)))
    (evil-tree-edit-goto-next-placeholder)
    (evil-tree-edit-change current-location)))

(defun evil-tree-edit-preview-node ()
  "Preview the different variations of the current node."
  (interactive)
  (let ((reazon-occurs-check nil)
        (reazon-timeout 0.1)
        (tree-edit-parse-comments nil))
    (--> evil-tree-edit-current-node
         (tsc-node-type it)
         (alist-get it tree-edit-grammar)
         ;; TODO: Parametrize
         (reazon-run 10 q (tree-edit-parseo it q '()))
         ;; Sometime parser returns repeats, not sure if that's expected
         (-uniq it)
         ;; TODO: Prettier print
         (message (string-join (-map #'prin1-to-string it) "\n")))))


(defun evil-tree-edit--ambiguous-node-range-p (node-a node-b)
  "Do NODE-A and NODE-B share the same range?"
  (and node-a node-b
       (equal (tsc-node-start-position node-a)
              (tsc-node-start-position node-b))
       (equal (tsc-node-end-position node-a)
              (tsc-node-end-position node-b))))

;;* Mode and evil state definitions
(defun evil-tree-edit--update-overlay ()
  "Update the display of the current selected node, and move the cursor."
  (move-overlay evil-tree-edit--node-overlay
                (tsc-node-start-position evil-tree-edit-current-node)
                (tsc-node-end-position evil-tree-edit-current-node))
  (goto-char (tsc-node-start-position evil-tree-edit-current-node))
  (if (or (evil-tree-edit--ambiguous-node-range-p
           (tsc-get-parent evil-tree-edit-current-node)
           evil-tree-edit-current-node)
          (evil-tree-edit--ambiguous-node-range-p
           (tsc-get-nth-child evil-tree-edit-current-node 0)
           evil-tree-edit-current-node))
      (overlay-put evil-tree-edit--node-overlay 'after-string
                   (propertize
                    (let ((type (tsc-node-type evil-tree-edit-current-node)))
                      (s-concat " " (if (stringp type) type (symbol-name type))))
                    'face '(italic :foreground "dark gray")))
    (overlay-put evil-tree-edit--node-overlay 'after-string "")))

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
    (overlay-put evil-tree-edit--node-overlay 'after-string "")
    (overlay-put evil-tree-edit--node-overlay 'face '())))

(defun evil-tree-edit-normal-or-tree-state ()
  "Enter normal or tree state contextually."
  (interactive)
  (if evil-tree-edit--return-position
      (progn
        (evil-tree-state)
        (evil-tree-edit--goto-node
         (tree-edit--restore-location evil-tree-edit--return-position))
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
    (let ((language-file (alist-get major-mode tree-edit-language-alist)))
      (unless language-file
        (evil-tree-edit-mode -1)
        (user-error "Tree-edit does not support %s!" (symbol-name major-mode)))
      (unless (featurep language-file)
        (require language-file)
        (evil-tree-edit-set-state-bindings major-mode)))
    (tree-sitter-mode)
    ;; HACK: Above mode binding won't come into effect until the state is changed.
    (evil-normal-state)
    (add-hook 'before-revert-hook #'evil-tree-edit--teardown nil 'local)
    ;; TODO: can we just run these on load?
    (add-hook 'tree-sitter-after-change-functions #'evil-tree-edit--preserve-current-node-after nil 'local)
    (add-hook 'before-change-functions #'evil-tree-edit--preserve-current-node-before nil 'local)
    (add-hook 'evil-tree-edit-movement-hook #'evil-tree-edit--update-overlay nil 'local))
   (t
    (remove-hook 'before-revert-hook #'evil-tree-edit--teardown 'local)
    (remove-hook 'before-change-functions #'evil-tree-edit--preserve-current-node-before 'local)
    (remove-hook 'tree-sitter-after-change-functions #'evil-tree-edit--preserve-current-node-after 'local))))

(defun define-evil-tree-edit-avy-jump (keymap key func)
  "Define a key command in KEYMAP prefixed by KEY calling FUNC.

FUNC must take one argument, a symbol of the node type."
  (dolist (node (append tree-edit-nodes tree-edit-query-nodes))
    (define-key
      keymap
      (string-join (list key (plist-get node :key)))
      (cons
       ;; emacs-which-key integration
       (or (plist-get node :name) (s-replace "_" " " (symbol-name (plist-get node :type))))
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
    keymap
    (string-join (list key "p"))
    (cons
     "kill-ring"
     `(lambda ()
        (interactive)
        (,func (car kill-ring))))))

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
      (define-evil-tree-edit-avy-jump mode-local-keymap "S" #'evil-tree-edit-sig-avy-jump)
      (define-key mode-local-keymap [escape] 'evil-normal-state)
      (define-key mode-local-keymap ">" #'evil-tree-edit-slurp)
      (define-key mode-local-keymap "<" #'evil-tree-edit-barf)
      (define-key mode-local-keymap "j" #'evil-tree-edit-goto-next-sibling)
      (define-key mode-local-keymap "k" #'evil-tree-edit-goto-prev-sibling)
      (define-key mode-local-keymap "h" #'evil-tree-edit-goto-parent)
      (define-key mode-local-keymap "f" #'evil-tree-edit-goto-child)
      (define-key mode-local-keymap "n" #'evil-tree-edit-goto-next-placeholder)
      (define-key mode-local-keymap "N" #'evil-tree-edit-change-next-placeholder)
      (define-key mode-local-keymap "c" #'evil-tree-edit-change)
      (define-key mode-local-keymap "d" #'evil-tree-edit-delete)
      (define-key mode-local-keymap "r" #'evil-tree-edit-raise)
      (define-key mode-local-keymap "y" #'evil-tree-edit-copy)
      (define-key mode-local-keymap "u" #'evil-tree-edit-undo)
      (define-key mode-local-keymap "A" #'evil-tree-edit-goto-sig-parent)
      (define-key mode-local-keymap "?" #'evil-tree-edit-preview-node)
      (define-key mode-local-keymap "zz" #'evil-scroll-line-to-center)
      ;; `setq-mode-local' macroexpanded, since it doesn't accept symbols
      (mode-local-bind `((evil-tree-state-map . ,mode-local-keymap)) '(mode-variable-flag t) mode)
      (mode-local-map-mode-buffers
       (lambda () (set (make-local-variable 'evil-tree-state-map) mode-local-keymap))
       mode))))

(unless evil-tree-edit-disable-nontree-bindings
  (evil-define-key 'normal evil-tree-edit-mode-map "Q" #'evil-tree-state)
  (evil-define-key 'insert evil-tree-edit-mode-map (kbd "<escape>") #'evil-tree-edit-normal-or-tree-state))

(provide 'evil-tree-edit)
;;; evil-tree-edit.el ends here
