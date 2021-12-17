;;; tree-edit-mode.el --- A minor mode for structural refactoring and editing -*- lexical-binding: t; -*-
;;
;; Copyright (C) Ethan Leba <https://github.com/ethan-leba>
;;
;; Author: Felix Geller
;; Version: 0.1.0
;; Homepage: https://github.com/ethan-leba/tree-edit
;; Package-Requires: ((emacs "27.1") (tree-sitter "0.15.0") (tsc "0.15.0") (tree-sitter-langs "0.10.0") (dash "2.19") (reazon "0.4.0") (s "0.0.0"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
(require 'tree-edit)

(defcustom tree-edit-mode-map (make-sparse-keymap)
  "Keymap to hold bindings that become active with tree-edit-mode")

(defcustom tree-edit-movement-hook nil
  "Hook to run when a movement was applied")

(defcustom tree-edit-add-type-annotation-p nil
  "Whether type annotation should be added to current node's
overlay in case node's range is ambiguous")

(defcustom tree-edit-mark-current-node-p nil
  "Whether the current node should be marked")

(defvar-local tree-edit-desired-node-types nil
  "List of node types that movement should stop at, all other types are skipped")

(defface tree-edit-current-node
  '((t (:inherit 'default)))
  "Face that is used to highlight the current node."
  :group 'tree-edit)

(defface tree-edit-current-node-annotation
  '((t (:inherit 'font-lock-comment-face)))
  "Face that is used for overlay annotation in case the current
node's range is ambiguous"
  :group 'tree-edit)

(defcustom tree-edit--hide-current-node-overlay-delay 0.3
  "Seconds until current node overlay is removed when point and
current node are out of sync")

;; internal

(defvar-local tree-edit--current-node nil
  "The current node to apply editing commands to.")

(defvar-local tree-edit--current-node-overlay nil
  "The display overlay to show the current node.")

(defvar-local tree-edit--current-node-overlay-hide-timer nil
  "Timer that triggers hiding the current node overlay.")

(defun tree-edit--find-current-node ()
  "Checks if the current node matches with point and updates it if
they are out of sync."
  (unless (and tree-edit--current-node
	       (= (point) (tsc-node-start-position tree-edit--current-node)))
    (let ((node (tsc-get-descendant-for-position-range
		 (tsc-root-node tree-sitter-tree) (point) (point))))
      (setq tree-edit--current-node
            (if (tree-edit--boring-nodep node)
		(tree-edit--apply-until-interesting #'tsc-get-parent node)
              node))))
  tree-edit--current-node)

(defun tree-edit--hide-current-node-overlay ()
  (when (and tree-edit--current-node
	     tree-edit--current-node-overlay)
    (unless (eq (point) (tsc-node-start-position tree-edit--current-node))
      (move-overlay tree-edit--current-node-overlay 0 0))))

(defun tree-edit--schedule-current-node-overlay-hide-timer ()
  (unless tree-edit--current-node-overlay-hide-timer
    (setq tree-edit--current-node-overlay-hide-timer
          (run-with-idle-timer
           tree-edit--hide-current-node-overlay-delay
	   t
	   'tree-edit--hide-current-node-overlay))))

(defun tree-edit--ambiguous-node-range-p (node-a node-b)
  (and node-a node-b
       (equal (tsc-node-start-position node-a)
              (tsc-node-start-position node-b))
       (equal (tsc-node-end-position node-a)
              (tsc-node-end-position node-b))))

(defun tree-edit--update-mark ()
  "Update location of mark to end point current node"
  (when tree-edit-mark-current-node-p
    (save-excursion
      (goto-char (tsc-node-end-position tree-edit--current-node))
      (set-mark (point)))))

(defun tree-edit--update-overlay ()
  "Updates the overlay for the current node and optionally appends
info about the node in case the node's range is ambiguous"
  (move-overlay tree-edit--current-node-overlay
                (tsc-node-start-position tree-edit--current-node)
                (tsc-node-end-position tree-edit--current-node))
  (when tree-edit-add-type-annotation-p
    (tree-edit--update-annotation-overlay)))

(defun tree-edit--update-annotation-overlay ()
  "Adds type annotation to current node's overlay in case node's range is ambiguous"
  (if (or (tree-edit--ambiguous-node-range-p
            (tsc-get-parent tree-edit--current-node)
	    tree-edit--current-node)
           (tree-edit--ambiguous-node-range-p
            (tsc-get-nth-child tree-edit--current-node 0)
            tree-edit--current-node))
      (overlay-put tree-edit--current-node-overlay 'after-string
                   (propertize
                    (let ((type (tsc-node-type tree-edit--current-node)))
                      (s-concat " " (if (stringp type) type (symbol-name type))))
                    'face 'tree-edit-current-node-annotation))
    (overlay-put tree-edit--current-node-overlay 'after-string "")))

;;;###autoload
(define-minor-mode tree-edit-mode
  "Structural editing for any* language."
  :init-value nil
  :keymap tree-edit-mode-map
  :lighter " TE"
  (cond
   (tree-edit-mode
    (let ((language-file (alist-get major-mode tree-edit-language-alist)))
      (unless language-file
        (tree-edit-mode -1)
        (user-error "Tree-edit does not support %s!" (symbol-name major-mode)))
      (unless (featurep language-file)
        (require language-file)))
    (tree-sitter-mode)
    (require 'tree-sitter-langs)
    (tree-edit--find-current-node)
    (unless tree-edit--current-node-overlay
      (setq tree-edit--current-node-overlay (make-overlay 0 0))
      (overlay-put tree-edit--current-node-overlay 'face 'tree-edit-current-node))
    (tree-edit--schedule-current-node-overlay-hide-timer)
    (add-hook 'tree-edit-movement-hook #'tree-edit--update-overlay nil 'local)
    (add-hook 'tree-edit-movement-hook #'tree-edit--update-mark nil 'local)
    )))

(defun tree-edit--range-eq (a b)
  (and a b
       (eq (tsc-node-start-position a) (tsc-node-start-position b))
       (eq (tsc-node-end-position a) (tsc-node-end-position b))))

(defun tree-edit--apply-movement-until (fun pred node)
  "Apply FUN to NODE until PRED returns non-nil."
  (let* ((next-node (funcall fun node)))
    (if (tree-edit--range-eq node next-node)
	(tree-edit--apply-movement-until fun pred next-node)
      (if (funcall pred next-node)
	  next-node
	(if next-node
	    (tree-edit--apply-movement-until fun pred next-node))))))

(defun tree-edit--apply-movement (fun &rest desired)
  "Apply movement FUN, and then update the current node, point and run movement hooks."
  (let* ((pred (lambda (n)
		 (and n
		      (tsc-node-p n)
		      (tsc-node-named-p n)
		      (or (not desired)
			  (not (car desired))
			  (memq (tsc-node-type n) (car desired))))))
	 (old-pos (tree-edit--find-current-node))
	 (new-pos (tree-edit--apply-movement-until fun pred old-pos)))
    (unless new-pos
      (message "tree-edit: found no new position for movement %s" fun))
    (when new-pos
      (setq tree-edit--current-node new-pos)
      (goto-char (tsc-node-start-position tree-edit--current-node))
      (run-hooks 'tree-edit-movement-hook))))

(defun tree-edit-goto-next-sibling ()
  "Move to the next (interesting) named sibling."
  (interactive)
  (tree-edit--apply-movement #'tsc-get-next-named-sibling))

(defun tree-edit-goto-prev-sibling ()
  "Move to the prev (interesting) named sibling."
  (interactive)
  (tree-edit--apply-movement #'tsc-get-prev-named-sibling))

(defun tree-edit-goto-parent ()
  "Move to the parent node."
  (interactive)
  (tree-edit--apply-movement #'tsc-get-parent))

(defun tree-edit-goto-child ()
  "Move to the first child, unless it's an only child."
  (interactive)
  (tree-edit--apply-movement (lambda (node) (tsc-get-nth-named-child node 0))))

(defun tree-edit-goto-desired-parent ()
  "Move to the interesting parent node."
  (interactive)
  (tree-edit--apply-movement #'tsc-get-parent tree-edit-desired-node-types))

(defun tree-edit-mark-current-node ()
  "Marks the current node"
  (interactive)
  (when tree-edit--current-node
    (goto-char (tsc-node-start-position tree-edit--current-node))
    (set-mark (point))
    (goto-char (tsc-node-end-position tree-edit--current-node))))

(defun tree-edit-slurp-cmd ()
  (interactive)
  (tree-edit-slurp tree-edit--current-node))

(defun tree-edit-barf-cmd ()
  (interactive)
  (tree-edit-barf tree-edit--current-node))

(defun tree-edit-delete-cmd ()
  (interactive)
  (tree-edit-delete tree-edit--current-node))

(defun tree-edit-raise-cmd ()
  (interactive)
  (let ((raised-node (tree-edit-raise tree-edit--current-node)))
    (setq tree-edit--current-node raised-node)
    (run-hooks 'tree-edit-movement-hook)))

(provide 'tree-edit-mode)
