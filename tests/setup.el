;; -*- lexical-binding: t; -*-
(require 'dash)
(require 'mode-local)
(require 'buttercup)
(require 'tree-edit)
(require 'evil-tree-edit)
(require 'tree-edit-build)
(require 'url-parse)

(defvar tree-edit--test-grammar-location (expand-file-name ".test-grammars/"))
(defvar tree-edit--test-major-mode-mapping
  '((python-mode . python)
    (java-mode . java)
    (c-mode . c)))

(when (getenv "TESTING")

  ;; HACK!! bug#62704 introduces a call to `read-string` that breaks CI
  (defun treesit-install-language-grammar (lang)
    "Build and install the tree-sitter language grammar library for LANG.

Interactively, if `treesit-language-source-alist' doesn't already
have data for building the grammar for LANG, prompt for its
repository URL and the C/C++ compiler to use.

This command requires Git, a C compiler and (sometimes) a C++ compiler,
and the linker to be installed and on PATH.  It also requires that the
recipe for LANG exists in `treesit-language-source-alist'.

See `exec-path' for the current path where Emacs looks for
executable programs, such as the C/C++ compiler and linker."
    (interactive (list (intern
                        (completing-read
                         "Language: "
                         (mapcar #'car treesit-language-source-alist)))))
    (when-let ((recipe
                (or (assoc lang treesit-language-source-alist)
                    (treesit--install-language-grammar-build-recipe
                     lang))))
      (condition-case err
          (apply #'treesit--install-language-grammar-1
                 ;; The nil is OUT-DIR.
                 (cons nil recipe))
        (error
         (display-warning
          'treesit
          (format "Error encountered when installing language grammar: %s"
                  err)))))

    ;; Check that the installed language grammar is loadable.
    (pcase-let ((`(,available . ,err)
                 (treesit-language-available-p lang t)))
      (when (not available)
        (display-warning
         'treesit
         (format "The installed language grammar for %s cannot be located or has problems (%s): %s"
                 lang (nth 0 err)
                 (string-join
                  (mapcar (lambda (x) (format "%s" x))
                          (cdr err))
                  " "))))))

  (make-directory tree-edit--test-grammar-location 'parents)
  (setq user-emacs-directory tree-edit--test-grammar-location)
  (setq tree-edit-storage-dir tree-edit--test-grammar-location)

  (setq treesit-extra-load-path `(,(expand-file-name "tree-sitter" user-emacs-directory)))
  (tree-edit-install-grammars-wizard nil :accept-all))

(defun buffer-status-as-string ()
  (if (equal evil-state 'tree)
      (progn
        (let* ((current-node (or (evil-tree-edit-current-node)
                                 (buttercup-fail "Buffer in invalid state, no current node")))
               (start (treesit-node-start current-node))
               (end (treesit-node-end current-node)))
          (evil-tree-state -1)
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
    (delete-char -1)
    (let ((start (point)))
      (when (search-forward "]")
        (delete-char -1)
        (let ((temp-node (treesit-node-descendant-for-range
                          (treesit-buffer-root-node) start (point) :named)))
          (evil-tree-state)
          (evil-tree-edit-set-current-node temp-node)
          (evil-tree-edit--update-overlay))))))

(defun tree-edit--test-setup (mode)
  ;; FIXME, disable mode grammar activaiton?
  (funcall mode)

  (treesit-parser-create
   (or (alist-get mode tree-edit-language-alist)
       (user-error "[test harness] no grammar specified for %s" major-mode))
   temp-buffer)

  (evil-mode)
  (evil-tree-edit-mode))

(defmacro with-base-test-buffer (mode contents &rest test-forms)
  "This awesome macro is adapted (borrowed) from
  https://github.com/abo-abo/lispy/blob/master/lispy-test.el#L15"
  (declare (indent 2))
  `(progn
     (-when-let (b (get-buffer "tree-edit-test-buffer"))
       (kill-buffer b))
     (let ((temp-buffer (get-buffer-create "tree-edit-test-buffer"))
           evil-tree-edit-after-change-hook
           evil-tree-edit-movement-hook
           python-indent-guess-indent-offset-verbose)
       (save-window-excursion
         (switch-to-buffer temp-buffer)

         (tree-edit--test-setup ,mode)
         (mode-local--activate-bindings)
         (insert ,contents)
         (goto-char 0)

         ,@test-forms

         temp-buffer))))

(defmacro with-test-buffer (mode contents &rest test-forms)
  "This awesome macro is adapted (borrowed) from
  https://github.com/abo-abo/lispy/blob/master/lispy-test.el#L15"
  (declare (indent 2))
  `(with-base-test-buffer ,mode ,contents
     (when (search-forward "|")
       (backward-delete-char 1))
     ,@test-forms))

(defmacro with-type-cache (k v &rest body)
  (declare (indent 2))
  (let ((hashname (gensym)))
    `(let ((,hashname (make-hash-table :test #'equal)))
       (puthash ,k ,v ,hashname)
       (let ((tree-edit--type-cache ,hashname))
         ,@body))))

(defmacro with-tree-test-buffer (mode contents &rest test-forms)
  "This awesome macro is adapted (borrowed) from
  https://github.com/abo-abo/lispy/blob/master/lispy-test.el#L15"
  (declare (indent 2))
  `(with-base-test-buffer ,mode ,contents
     (evil-tree-state -1)
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
