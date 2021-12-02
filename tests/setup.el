;; -*- lexical-binding: t; -*-
(require 'dash)
(require 'mode-local)
(require 'buttercup)
(require 's)
(require 'tree-sitter-langs)
(require 'evil-tree-edit)

(defun buffer-status-as-string ()
  (if (equal evil-state 'tree)
      (progn
        (let ((start (tsc-node-start-position evil-tree-edit-current-node))
              (end (tsc-node-end-position evil-tree-edit-current-node)))
          (goto-char end)
          (insert "]")
          (goto-char start)
          (insert "[")))
    (let ((p (point))
          (m (when mark-active (mark))))
      (goto-char p)
      (insert "|")

      ;; show mark as well (other side of selection, if any)
      (when m
        (goto-char m)
        (insert "~"))))

  (buffer-substring-no-properties (point-min)
                                  (point-max)))

(defun select-node ()
  (when (search-forward "[")
    (backward-delete-char 1)
    (let ((start (point)))
      (when (search-forward "]")
        (backward-delete-char 1)
        (let ((temp-node (tsc-get-descendant-for-position-range
                          (tsc-root-node tree-sitter-tree) start (point))))
          (evil-tree-state)
          (setq evil-tree-edit-current-node temp-node)
          (evil-tree-edit--update-overlay))))))

(defmacro with-base-test-buffer (mode contents &rest test-forms)
  "This awesome macro is adapted (borrowed) from
  https://github.com/abo-abo/lispy/blob/master/lispy-test.el#L15"
  (declare (indent 2))
  `(progn
     (-when-let (b (get-buffer "tree-edit-test-buffer"))
       (kill-buffer b))
     (let ((temp-buffer (get-buffer-create "tree-edit-test-buffer"))
           evil-tree-edit-after-change-hook
           evil-tree-edit-movement-hook)
       (save-window-excursion
         (switch-to-buffer temp-buffer)
         (funcall ,mode)
         (evil-mode)
         (tree-sitter-mode)
         (evil-tree-edit-mode)
         (mode-local--activate-bindings)
         (insert ,contents)
         (goto-char 0)

         ,@test-forms

         temp-buffer))))

(defmacro with-test-buffer (mode contents &rest test-forms)
  "This awesome macro is adapted (borrowed) from
  https://github.com/abo-abo/lispy/blob/master/lispy-test.el#L15"
  (declare (indent 1))
  `(with-base-test-buffer ,mode ,contents
     (when (search-forward "|")
       (backward-delete-char 1))
     ,@test-forms))

(defmacro with-tree-test-buffer (mode contents &rest test-forms)
  "This awesome macro is adapted (borrowed) from
  https://github.com/abo-abo/lispy/blob/master/lispy-test.el#L15"
  (declare (indent 1))
  `(with-base-test-buffer ,mode ,contents
     (select-node)
     ,@test-forms))

(buttercup-define-matcher :to-have-buffer-contents (test-buffer
                                                    expected-contents)
  (setq test-buffer (funcall test-buffer))
  (setq expected-contents (funcall expected-contents))
  (with-current-buffer test-buffer
    (let ((contents (buffer-status-as-string)))
      (if (equal contents expected-contents)
          t
        `(nil . ,(format "Expected '%s' to equal '%s'."
                         contents
                         expected-contents))))))

(defmacro with-tree-test-buffer-avy (mode contents avy-index &rest test-forms)
  "If AVY-INDEX is out of bounds a 'user-ptr' error will appear."
  (declare (indent 3))
  `(with-tree-test-buffer ,mode ,contents
     (cl-letf (((symbol-function 'avy-process)
                (lambda (positions) (funcall avy-action (nth ,avy-index positions)))))
       ,@test-forms)))
