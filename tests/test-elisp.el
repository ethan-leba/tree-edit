(require 'evil-tree-edit)
(ignore-errors (load-file "setup.el"))

(describe "elisp regressions"
  (it "properly spaces the cons ."
    ;; Fixed in commit 0393dc6
    (expect (with-tree-test-buffer #'emacs-lisp-mode "'([foo] . bar)"
              (evil-tree-edit-exchange 'symbol))
            :to-have-buffer-contents"'([TREE] . bar)")))
