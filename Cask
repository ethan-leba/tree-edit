(source gnu)
(source melpa)

;; read metadata from this file's package headers
(package-file "tree-edit.el")

(development
 (depends-on "tree-sitter-langs"
             :git "https://github.com/ethan-leba/tree-sitter-langs.git"
             :branch "local-dir")
 (depends-on "buttercup")
 ;; FIXME: can't reference evil-tree-edit's package-file directly, since it
 ;; relies on tree-edit (which causes a dependency error). manually declaring
 ;; the deps. instead.
 (depends-on "evil")
 (depends-on "avy"))
