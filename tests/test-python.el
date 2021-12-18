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
baz()"))
  (it "can slurp multilevel"
    (expect (with-tree-test-buffer #'python-mode "
if TREE:
    FOO
else:
    [BAR]
TREE"
              (evil-tree-edit-goto-parent)
              (evil-tree-edit-goto-parent)
              (evil-tree-edit-slurp))
            :to-have-buffer-contents "
if TREE:
    FOO
else:
    [BAR
    TREE]")))

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
baz()"))
  (it "can barf multilevel"
    (expect (with-tree-test-buffer #'python-mode "
if TREE:
    FOO
else:
    [BAR
    TREE]"
              (evil-tree-edit-barf))
            :to-have-buffer-contents "
if TREE:
    FOO
else:
    [BAR]
TREE")
    (expect (with-tree-test-buffer #'python-mode "
if TREE:
    FOO
else:
    [BAR
    TREE]"
              (evil-tree-edit-barf))
            :to-have-buffer-contents "
if TREE:
    FOO
else:
    [BAR]
TREE")))

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
    bert()]"))
  (it "can handle context-specific types"
    (expect (with-tree-test-buffer #'python-mode "foo([*bar])"
              (evil-tree-edit-copy)
              (evil-tree-edit-insert-sibling (car kill-ring)))
            :to-have-buffer-contents "foo(*bar,[*bar])")
    (expect (with-tree-test-buffer #'python-mode "{[1:2]}"
              (evil-tree-edit-copy)
              (evil-tree-edit-insert-sibling (car kill-ring)))
            :to-have-buffer-contents "{1:2,[1:2]}")
    (expect (with-tree-test-buffer #'python-mode "{[1:2],3:4}"
              (evil-tree-edit-copy)
              (evil-tree-edit-goto-next-sibling)
              (evil-tree-edit-exchange (car kill-ring)))
            :to-have-buffer-contents "{1:2,[1:2]}")))

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
baz()")
    (expect (with-tree-test-buffer #'python-mode "
if foo:
    [foo()]
    bar()"
              (evil-tree-edit-goto-parent)
              (evil-tree-edit-insert-sibling 'call))
            :to-have-buffer-contents "
if foo:
    foo()
    [TREE()]
    bar()"))
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

(describe "exchange"
  (it "works"
    (expect (with-tree-test-buffer #'python-mode "
for [foo] in TREE:
    TREE
"
              (evil-tree-edit-exchange "bar"))
            :to-have-buffer-contents "
for [bar] in TREE:
    TREE
")))

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

(describe "goto next placeholder"
  (it "works"
    (expect (with-tree-test-buffer #'python-mode "
[for TREE in BAR:
    TREE]"
              (evil-tree-edit-goto-next-placeholder))
            :to-have-buffer-contents "
for [TREE] in BAR:
    TREE")
    (expect (with-tree-test-buffer #'python-mode "
[for qwer in BAR:
    TREE]"
              (evil-tree-edit-goto-next-placeholder))
            :to-have-buffer-contents "
for qwer in BAR:
    [TREE]")))

(describe "delete node"
  (it "doesn't allow empty blocks (manual grammar edit)"
    ;; Should select bounds of new named node
    (expect (with-tree-test-buffer #'python-mode "
if foo:
    [bar()]"
              (evil-tree-edit-goto-parent)
              (evil-tree-edit-delete))
            :to-throw 'user-error)))
