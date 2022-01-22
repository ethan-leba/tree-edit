;;; evil-tree-edit-view.el --- Syntax tree visualization with tree-edit  -*- lexical-binding: t; -*-
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
;;  Modified version of `tree-sitter-debug-mode' to provide tree visualization
;;  for `evil-tree-edit-mode'.
;;
;;; Code:

(require 'evil-tree-edit)

(defvar-local evil-tree-edit-view--tree-buffer nil
  "Buffer used to display the syntax tree of this buffer.")

(defvar-local evil-tree-edit-view--source-code-buffer nil
  "Source buffer of the syntax tree displayed in this `evil-tree-edit-view' buffer.")

(defvar-local evil-tree-edit-view--current-node-line nil)

(defun evil-tree-edit-view--display-node (node depth child-of-current-node)
  "Display NODE that appears at the given DEPTH in the syntax tree.

NODE be displayed in bold if it matches `tree-edit--current-node', or in a
lighter color if CHILD-OF-CURRENT-NODE is non-nil."
  (insert (make-string (* 2 depth) ?\ ))
  (let* ((current-nodep (with-current-buffer evil-tree-edit-view--source-code-buffer
                          (and evil-tree-edit-current-node
                               (tsc-node-eq node evil-tree-edit-current-node))))
         (node-text (format "%s%s\n"
                            (tsc-node-type node)
                            (if (> (tsc-count-named-children node) 0) ":" ""))))
    (let ((point-before (point)))
      (insert node-text)
      (when current-nodep
        (setq evil-tree-edit-view--current-node-line (point)))
      (when current-nodep
        (add-text-properties point-before (point) '(face (:weight bold))))
      (when (not (or current-nodep child-of-current-node))
        (add-text-properties point-before (point) '(face (:foreground "dim gray")))))
    (tsc-mapc-children (lambda (c)
                         (when (tsc-node-named-p c)
                           (evil-tree-edit-view--display-node c (1+ depth) (or child-of-current-node current-nodep))))
                       node)))

(defun evil-tree-edit-view--display-tree (_old-tree)
  "Display the current `tree-sitter-tree'."
  ;; TODO: Re-render only affected nodes.
  (when-let ((tree tree-sitter-tree))
    (with-current-buffer evil-tree-edit-view--tree-buffer
      (let (buffer-read-only)
        (erase-buffer)
        (evil-tree-edit-view--display-node (tsc-root-node tree) 0 nil)
        (when (window-live-p (get-buffer-window evil-tree-edit-view--tree-buffer))
          (with-selected-window (get-buffer-window evil-tree-edit-view--tree-buffer)
            (goto-char evil-tree-edit-view--current-node-line)
            (recenter)))))))

(defun evil-tree-edit-view--display-tree-no-arg ()
  "Display the current `tree-edit-tree'."
  ;; TODO: Re-render only affected nodes.
  (evil-tree-edit-view--display-tree nil))

(defun evil-tree-edit-view--setup ()
  "Set up syntax tree viewging in the current buffer."
  (unless (buffer-live-p evil-tree-edit-view--tree-buffer)
    (setq evil-tree-edit-view--tree-buffer
          (get-buffer-create (format "tree-edit-tree: %s" (buffer-name)))))
  (let ((source-buffer (current-buffer)))
    (with-current-buffer evil-tree-edit-view--tree-buffer
      (buffer-disable-undo)
      (setq evil-tree-edit-view--source-code-buffer source-buffer
            buffer-read-only t)))
  (add-hook 'tree-sitter-after-change-functions #'evil-tree-edit-view--display-tree nil :local)
  (add-hook 'evil-tree-edit-movement-hook #'evil-tree-edit-view--display-tree-no-arg nil :local)
  (add-hook 'kill-buffer-hook #'evil-tree-edit-view--teardown nil :local)
  (display-buffer evil-tree-edit-view--tree-buffer)
  (evil-tree-edit-view--display-tree nil))

(defun evil-tree-edit-view--teardown ()
  "Tear down syntax tree viewging in the current buffer."
  (remove-hook 'tree-sitter-after-change-functions #'evil-tree-edit-view--display-tree :local)
  (remove-hook 'evil-tree-edit-movement-hook #'evil-tree-edit-view--display-tree-no-arg :local)
  (when (buffer-live-p evil-tree-edit-view--tree-buffer)
    (kill-buffer evil-tree-edit-view--tree-buffer)
    (setq evil-tree-edit-view--tree-buffer nil)))

;;;###autoload
(define-minor-mode evil-tree-edit-view-mode
  "Toggle syntax tree viewing for the current buffer.

This mode displays the syntax tree in another buffer, and keeps it up-to-date."
  :init-value nil
  :group 'tree-edit
  (tree-sitter--handle-dependent evil-tree-edit-view-mode
    #'evil-tree-edit-view--setup
    #'evil-tree-edit-view--teardown))

(provide 'evil-tree-edit-view)
;;; evil-tree-edit-view.el ends here
