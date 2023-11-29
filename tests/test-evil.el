(require 'evil-tree-edit)
(ignore-errors (load-file "setup.el"))

(describe "movement cmds work"
  (it "out"
    (expect (with-tree-test-buffer #'python-mode "
if foo:
    bar([foo()])"
              (evil-tree-edit-out 'if_statement))
            :to-have-buffer-contents "
[if foo:
    bar(foo())]"))
  (it "sig out"
    (expect (with-tree-test-buffer #'python-mode "
def foo():

    def bar():
        x = x + [5]"
              (evil-tree-edit-goto-sig-parent))
            :to-have-buffer-contents "
def foo():

    [def bar():
        x = x + 5]")
    (expect (with-tree-test-buffer #'python-mode "
def foo():

    def bar():
        x = x + [5]"
              (evil-tree-edit-goto-sig-parent))
            :to-have-buffer-contents "
def foo():

    [def bar():
        x = x + 5]"))
  (it "avy jumps"
    (expect (with-tree-test-buffer-avy #'python-mode "
[def foo():

    def bar():
        x = x + 5]" 2
        (evil-tree-edit-avy-jump 'identifier))
            :to-have-buffer-contents "
def foo():

    def bar():
        [x] = x + 5")))
