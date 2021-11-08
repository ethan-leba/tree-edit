;;; tree-edit-debug.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) Ethan Leba <https://github.com/ethan-leba>
;;
;; Author: Ethan Leba <ethanleba5@gmail.com>
;; Version: 0.1.0
;; Homepage: https://github.com/ethan-leba/tree-edit
;; Package-Requires: ((emacs "27.0"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Modified version of `tree-sitter-debug-mode' to provide highlighting for the
;;  selected node.
;;
;;; Code:

(require 'tree-edit)

(defvar-local tree-edit-debug--tree-buffer nil
  "Buffer used to display the syntax tree of this buffer.")

(defvar-local tree-edit-debug--source-code-buffer nil
  "Source buffer of the syntax tree displayed in this `tree-edit-debug' buffer.")

(defvar-local tree-edit-debug--current-node-line nil)

(defun tree-edit-debug--display-node (node depth child-of-current-node)
  "Display NODE that appears at the given DEPTH in the syntax tree.

NODE be displayed in bold if it matches `tree-edit--current-node', or in a
lighter color if CHILD-OF-CURRENT-NODE is non-nil."
  (insert (make-string (* 2 depth) ?\ ))
  (let* ((current-nodep (with-current-buffer tree-edit-debug--source-code-buffer
                          (and tree-edit--current-node
                               (equal (tsc-node-range node)
                                      (if (equal (tsc-count-named-children tree-edit--current-node) 0)
                                          (tsc-node-range (tsc-get-parent tree-edit--current-node))
                                        (tsc-node-range tree-edit--current-node))))))
         (node-text (format "%s%s\n"
                            (tsc-node-type node)
                            (if (> (tsc-count-named-children node) 0) ":" ""))))
    (let ((point-before (point)))
      (insert node-text)
      (when current-nodep
        (setq tree-edit-debug--current-node-line (point)))
      (when current-nodep
        (add-text-properties point-before (point) '(face (:weight bold))))
      (when (not (or current-nodep child-of-current-node))
        (add-text-properties point-before (point) '(face (:foreground "dim gray")))))
    (tsc-mapc-children (lambda (c)
                         (when (and (tsc-node-named-p c)
                                    (> (tsc-count-named-children c) 0))
                           (tree-edit-debug--display-node c (1+ depth) (or child-of-current-node current-nodep))))
                       node)))

(defun tree-edit-debug--display-tree (_old-tree)
  "Display the current `tree-sitter-tree'."
  ;; TODO: Re-render only affected nodes.
  (when-let ((tree tree-sitter-tree))
    (with-current-buffer tree-edit-debug--tree-buffer
      (let (buffer-read-only)
        (erase-buffer)
        (tree-edit-debug--display-node (tsc-root-node tree) 0 nil)
        (when (window-live-p (get-buffer-window tree-edit-debug--tree-buffer))
          (with-selected-window (get-buffer-window tree-edit-debug--tree-buffer)
            (goto-char tree-edit-debug--current-node-line)
            (recenter)))))))

(defun tree-edit-debug--display-tree-no-arg ()
  "Display the current `tree-edit-tree'."
  ;; TODO: Re-render only affected nodes.
  (tree-edit-debug--display-tree nil))

(defun tree-edit-debug--setup ()
  "Set up syntax tree debugging in the current buffer."
  (unless (buffer-live-p tree-edit-debug--tree-buffer)
    (setq tree-edit-debug--tree-buffer
          (get-buffer-create (format "tree-edit-tree: %s" (buffer-name)))))
  (let ((source-buffer (current-buffer)))
    (with-current-buffer tree-edit-debug--tree-buffer
      (buffer-disable-undo)
      (setq tree-edit-debug--source-code-buffer source-buffer
            buffer-read-only t)))
  (add-hook 'tree-sitter-after-change-functions #'tree-edit-debug--display-tree nil :local)
  (add-hook 'tree-edit-movement-hook #'tree-edit-debug--display-tree-no-arg nil :local)
  (add-hook 'kill-buffer-hook #'tree-edit-debug--teardown nil :local)
  (display-buffer tree-edit-debug--tree-buffer)
  (tree-edit-debug--display-tree nil))

(defun tree-edit-debug--teardown ()
  "Tear down syntax tree debugging in the current buffer."
  (remove-hook 'tree-sitter-after-change-functions #'tree-edit-debug--display-tree :local)
  (remove-hook 'tree-edit-movement-hook #'tree-edit-debug--display-tree-no-arg :local)
  (when (buffer-live-p tree-edit-debug--tree-buffer)
    (kill-buffer tree-edit-debug--tree-buffer)
    (setq tree-edit-debug--tree-buffer nil)))

;;;###autoload
(define-minor-mode tree-edit-debug-mode
  "Toggle syntax tree debugging for the current buffer.
This mode displays the syntax tree in another buffer, and keeps it up-to-date."
  :init-value nil
  :group 'tree-edit
  (tree-sitter--handle-dependent tree-edit-debug-mode
    #'tree-edit-debug--setup
    #'tree-edit-debug--teardown))

(provide 'tree-edit-debug)
;;; tree-edit-debug.el ends here
