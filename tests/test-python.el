(require 'evil-tree-edit)
(ignore-errors (load-file "setup.el"))

(describe "slurp"
  (it "correctly indents"
    (expect (with-tree-test-buffer #'python-mode "
if foo:
    [foo()]
bar()
baz()"
              (evil-tree-edit-goto-parent)
              (evil-tree-edit-goto-parent)
              (evil-tree-edit-slurp))
            :to-have-buffer-contents "
if foo:
    [foo()
    bar()]
baz()")
    (expect (with-tree-test-buffer #'python-mode "
if foo:
    [foo()]
if nested:
    indentation(x)
    with difficulty():
        sadness()
    qwert()
baz()"
              (evil-tree-edit-goto-parent)
              (evil-tree-edit-goto-parent)
              (evil-tree-edit-slurp))
            :to-have-buffer-contents "
if foo:
    [foo()
    if nested:
        indentation(x)
        with difficulty():
            sadness()
        qwert()]
baz()")))

(describe "barf"
  (it "correctly indents"
    (expect (with-tree-test-buffer #'python-mode "
if foo:
    [foo()
    bar()]
baz()"
              (evil-tree-edit-barf))
            :to-have-buffer-contents "
if foo:
    [foo()]
bar()
baz()")))

(describe "raise"
  (it "correctly indents"
    (expect (with-tree-test-buffer #'python-mode "
if foo:
    [with gumbo:
        eat()]"
              (evil-tree-edit-raise))
            :to-have-buffer-contents "
[with gumbo:
    eat()]")))

(describe "copy/paste node"
  (xit "can copy blocks"
    (expect (with-tree-test-buffer #'python-mode
              "
if foo:
    [qwert()
    bert()]"
              (evil-tree-edit-copy)
              (evil-tree-edit-exchange (car kill-ring)))
            :to-have-buffer-contents "
if foo:
    [qwert()
    bert()]")))

(describe "insert sibling"
  (it "works"
    (expect (with-tree-test-buffer #'python-mode "
if foo:
    [foo()
    bar()]
baz()"
              (evil-tree-edit-insert-sibling 'elif_clause))
            :to-have-buffer-contents "
if foo:
    foo()
    bar()
[elif TREE:
    TREE]
baz()"))
  (it "can handle ambiguous fragments"
    ;; Could be identifier, or expression statement
    (expect (with-tree-test-buffer #'python-mode "foo([x])"
              (evil-tree-edit-insert-sibling "y"))
            :to-have-buffer-contents "foo(x,[y])")
    (expect (with-tree-test-buffer #'python-mode "
if TREE:
    [x]"
              (evil-tree-edit-goto-parent)
              (evil-tree-edit-insert-sibling "y"))
            :to-have-buffer-contents "
if TREE:
    x
    [y]")))

(describe "wrap"
  (it "works"
    (expect (with-tree-test-buffer #'python-mode "
[for FOO in BAR:
    hi]"
              (evil-tree-edit-wrap-node 'for_statement))
            :to-have-buffer-contents "
[for TREE in TREE:
    for FOO in BAR:
        hi]")
    (expect (with-tree-test-buffer #'python-mode "
for FOO in BAR:
    [hi]"
              (evil-tree-edit-goto-parent)
              (let ((tree-edit-syntax-snippets
                     `((return_statement . ("return" expression)) . ,tree-edit-syntax-snippets)))
                (evil-tree-edit-wrap-node 'return_statement)))
            :to-have-buffer-contents "
for FOO in BAR:
    [return hi]")))

(describe "delete node"
  (xit "doesn't allow empty blocks (manual grammar edit)"
    ;; Should select bounds of new named node
    (expect (with-tree-test-buffer #'python-mode "
if foo:
    [bar()]"
              (evil-tree-edit-goto-parent)
              (evil-tree-edit-delete))
            :to-throw 'user-error)))
