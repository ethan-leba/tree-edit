(require 'evil-tree-edit)
(require 'buttercup)
(ignore-errors (load-file "setup.el"))
(describe "C specific regressions"
  (xit "handles alias-related issues"
    (expect (with-tree-test-buffer #'c-mode "{struct Test {int [foo]};}"
              (evil-tree-edit-wrap-node 'pointer_declarator))
            :to-have-buffer-contents "{struct Test {int [*foo]};}")
    ;; pointer decl uses non-aliased name
    (expect (with-tree-test-buffer #'c-mode "
struct foo {
  char **[data];
};"
              (evil-tree-edit-raise))
            :to-have-buffer-contents "
struct foo {
  char *[data];
};")
    (expect (with-tree-test-buffer #'c-mode "char*[* foo](){};"
              (evil-tree-edit-raise))
            :to-have-buffer-contents "char[* foo()]{};"))
  (it "can handle non-named aliases"
    (expect (with-tree-test-buffer #'c-mode "
#if FOO
[hi();]
#endif"
              (evil-tree-edit-delete))
            :to-have-buffer-contents "
#if [FOO]
#endif"))
  ;; puts the same thing back, reduced number of nodes not true?
  (it "can delete node by replacing named node with syntax"
    (expect (with-tree-test-buffer #'c-mode "for([void TREE;] TREE;){}"
              (evil-tree-edit-delete))
            :to-have-buffer-contents "for(;[TREE];){}"))
  ;; delete surrounding syntax?
  (xit "can reorganize syntax"
    (expect (with-tree-test-buffer #'c-mode "for (; [TREE];) {}"
              (evil-tree-edit-insert-sibling 'identifier))
            :to-have-buffer-contents "for (; TREE; [TREE]) {}"))
  ;; needs to be wrapped in 'type specifier'
  (xit "has dwim nodes where needed"
    (expect (with-tree-test-buffer #'c-mode "{sizeof([foo]);}"
              (evil-tree-edit-exchange "struct Foo"))
            :to-have-buffer-contents "{sizeof([struct Foo]);}"))
  (it "?" (expect (with-tree-test-buffer #'c-mode "
{if (foo) [bar;]}"
             (evil-tree-edit-exchange "{bar;}"))
           :to-have-buffer-contents
           "
{if (foo)[{
    bar;
  }]}")))
