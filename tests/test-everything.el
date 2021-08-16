
(condition-case nil
    (tree-edit-load-grammar "tests/java-grammar")
  (error (tree-edit-load-grammar "java-grammar")))

(describe "test set up"
  (it "grammar properly loaded"
    (expect tree-edit-grammar :not :to-be nil))
  (it "inserts buffer contents and returns them"
    (expect (with-test-buffer '("hello world|"))
            :to-have-buffer-contents '("hello world|"))))

;; TODO: Rename navigation primitives
(describe "basic navigation"
  (it "can move between sibling nodes"
    (expect (with-tree-test-buffer '("if ([foo] == 3) {}")
              (tree-edit-up))
            :to-have-buffer-contents '("if (foo == [3]) {}"))
    (expect (with-tree-test-buffer '("if (foo == [3]) {}")
              (tree-edit-down))
            :to-have-buffer-contents '("if ([foo] == 3) {}")))
  (it "can move between parent and children nodes"
    (expect (with-tree-test-buffer '("if (foo == [3]) {}")
              (tree-edit-left))
            :to-have-buffer-contents '("if ([foo == 3]) {}"))
    (expect (with-tree-test-buffer '("if (foo == [3]) {}")
              (tree-edit-left)
              (tree-edit-left))
            :to-have-buffer-contents '("if [(foo == 3)] {}"))
    (expect (with-tree-test-buffer '("if (foo == [3]) {}")
              (tree-edit-left)
              (tree-edit-left)
              (tree-edit-left))
            :to-have-buffer-contents '("[if (foo == 3) {}]"))
    (expect (with-tree-test-buffer '("[if (foo == 3) {}]")
              (tree-edit-right))
            :to-have-buffer-contents '("if [(foo == 3)] {}"))
    (expect (with-tree-test-buffer '("if ([foo == 3]) {}")
              (tree-edit-right))
            :to-have-buffer-contents '("if ([foo] == 3) {}"))))

(describe "entering tree state"
  (it "will select the smallest node at point"
    (expect (with-test-buffer '("if (|foo == 3) {}")
              (evil-tree-state))
            :to-have-buffer-contents '("if ([foo] == 3) {}"))
    (expect (with-test-buffer '("if| (foo == 3) {}")
              (evil-tree-state))
            :to-have-buffer-contents '("[if (foo == 3) {}]"))
    (expect (with-test-buffer '("i|f (foo == 3) {}")
              (evil-tree-state))
            :to-have-buffer-contents '("[if (foo == 3) {}]"))
    (expect (with-test-buffer '("if (foo =|= 3) {}")
              (evil-tree-state))
            :to-have-buffer-contents '("if ([foo == 3]) {}")))
  (expect (with-test-buffer '("
if (foo == 3) {
   int x |= 3;
}")
            (evil-tree-state))
          :to-have-buffer-contents
          '("
if (foo == 3) {
   int [x = 3];
}")))

;; TODO: Test avy?
(describe "node creation"
  (it "works with trivial nodes"
    (expect (tree-edit-make-node 'break_statement '((break_statement . ("break" ";"))))
            :to-equal "break;")
    (expect (tree-edit-make-node 'continue_statement '((continue_statement . ("continue" ";"))))
            :to-equal "continue;"))
  (it "can select choices based on input"
    (expect (tree-edit-make-node
             'expression_statement
             '((expression_statement . (identifier ";")) ;; _expression = identifier
               (identifier . ("TREE_EDIT"))))
            :to-equal "TREE_EDIT;")
    (expect (tree-edit-make-node
             'argument_list
             '((expression_statement . (call_expression ";")) ;; _expression = identifier
               (call_expression . (identifier argument_list))
               (argument_list . ("(" identifier ")"))
               (identifier . ("TREE_EDIT"))))
            :to-equal "(TREE_EDIT)")
    (expect (tree-edit-make-node
             'expression_statement
             '((expression_statement . (call_expression ";")) ;; _expression = identifier
               (call_expression . (identifier argument_list))
               (argument_list . ("(" identifier ")"))
               (identifier . ("TREE_EDIT"))))
            :to-equal "TREE_EDIT(TREE_EDIT);")
    (expect (tree-edit-make-node
             'if_statement
             '((if_statement . ("if" parenthesized_expression expression_statement)) ;; _expression = identifier
               (expression_statement . (identifier ";"))
               (parenthesized_expression . ("(" identifier ")"))
               (identifier . ("TREE"))))
            :to-equal "if(TREE)TREE;"))
  (xit "can space words properly"
    ;; FIXME: private void, etc?
    ))

;;* Raise node
(describe "raise node"
  (it "replaces the parent node with selected child"
    (expect (with-tree-test-buffer '("{[foo] == 3;}")
              (tree-edit-raise))
            :to-have-buffer-contents '("{[foo];}"))
    (expect (with-tree-test-buffer '("{foo == [3];}")
              (tree-edit-raise))
            :to-have-buffer-contents '("{[3];}"))
    ;; XXX: how to multi-select?
    (expect (with-tree-test-buffer '("{if (foo) {[foo;]bar;baz;}}")
              (tree-edit-raise))
            :to-have-buffer-contents '("{if (foo) [foo;]}"))
    (expect (with-tree-test-buffer '("{if (foo) {[foo;]bar;baz;}}")
              (tree-edit-raise)
              (tree-edit-raise))
            :to-have-buffer-contents '("{[foo;]}")))
  ;; XXX: there should be a limit to this...
  (it "travels up the syntax tree until a valid construction is found"
    (expect (with-tree-test-buffer '("{foo([bar()]);}")
              (tree-edit-raise))
            :to-have-buffer-contents '("{[bar()];}"))
    (expect (with-tree-test-buffer '("{foo(bar([baz]));}")
              (tree-edit-raise))
            :to-have-buffer-contents '("{foo([baz]);}"))
    (expect (with-tree-test-buffer '("{foo(bar([baz], bomb));}")
              (tree-edit-raise))
            :to-have-buffer-contents '("{foo([baz]);}"))
    )
  (it "errors if a node cannot be raised"
    (expect (with-tree-test-buffer '("{[foo];}")
              (tree-edit-raise))
            :to-throw 'user-error)
    (expect (with-tree-test-buffer '("[{foo;}]")
              (tree-edit-raise))
            :to-throw 'user-error)))

(describe "exchange node"
  (it "correctly replaces valid transformations")
  (xit "does not allow invalid transformations"))

(describe "insert sibling"
  (it "correctly inserts sibling nodes"
    ;; Should select bounds of new named node
    (expect (with-tree-test-buffer '("{foo([x]);}")
              (tree-edit-insert-sibling 'identifier))
            :to-have-buffer-contents '("{foo(x,[TREE]);}"))
    (expect (with-tree-test-buffer '("{foo([x]);}")
              (tree-edit-insert-sibling 'method_invocation))
            :to-have-buffer-contents '("{foo(x,[TREE()]);}"))
    (expect (with-tree-test-buffer '("{[foo(x);]}")
              (tree-edit-insert-sibling 'break_statement))
            :to-have-buffer-contents '("{foo(x);[break;]}")))
  (xit "calling insert on a node with no named children will enter in"
    ;; Should select bounds of new named node
    (expect (with-tree-test-buffer '("{foo[()];}")
              (tree-edit-insert-sibling 'identifier))
            :to-have-buffer-contents '("{foo([TREE]);}")))
  (xit "can perform multi-node insertions"
    ;; Should select bounds of new named node
    (expect (with-tree-test-buffer '("{if(foo) [bar;]}")
              (tree-edit-insert-sibling '("else" 'block)))
            :to-have-buffer-contents '("{if(foo) bar;else{TREE;}}")))
  (it "does not allow invalid transformations"
    (expect (with-tree-test-buffer '("{foo([x]);}")
              (tree-edit-insert-sibling 'break_statement))
            :to-throw 'user-error)))

(describe "wrap node"
  (xit "correctly replaces valid transformations")
  (xit "does not allow invalid transformations"))

(describe "modify node"
  (xit "correctly replaces valid transformations")
  (xit "does not allow invalid transformations")
  (xit "provides an accurate list of possible replacements"))

(describe "delete node"
  (it "correctly replaces valid transformations"
    ;; Should select bounds of new named node
    (expect (with-tree-test-buffer '("{foo([x]);}")
              (tree-edit-delete-node))
            :to-have-buffer-contents '("{foo[()];}"))
    ;; FIXME: Class body decl. is an inline type
    ;; (expect (with-tree-test-buffer '("
;; class Main {
;;   [void foo() {}]
;;   void bar() {}
;; }")
;;               (tree-edit-delete-node))
;;             :to-have-buffer-contents '("
;; class Main {
;;   [void bar() {}]
;; }"))
    (expect (with-tree-test-buffer '("{foo([x],y);}")
              (tree-edit-delete-node))
            :to-have-buffer-contents '("{foo([y]);}"))
    (expect (with-tree-test-buffer '("{foo(x,[y]);}")
              (tree-edit-delete-node))
            :to-have-buffer-contents '("{foo([x]);}"))
    (expect (with-tree-test-buffer '("{foo(x);[break;]}")
              (tree-edit-delete-node))
            :to-have-buffer-contents '("{[foo(x);]}")))
  (it "does not allow invalid transformations"
    (expect (with-tree-test-buffer '("{[foo](x);}")
              (tree-edit-delete-node))
            :to-throw 'user-error)))
