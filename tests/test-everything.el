(require 'evil-tree-edit)
(ignore-errors (load-file "setup.el"))

(describe "test set up"
  (it "grammar properly loaded"
    (with-mode-local java-mode (expect tree-edit-grammar :not :to-be nil)))
  (it "inserts buffer contents and returns them"
    (expect (with-test-buffer "hello world|")
            :to-have-buffer-contents "hello world|")))

(describe "mode local settings"
  (it "sets keybindings"
    (expect (with-tree-test-buffer "if ([foo] == 3) {}"
              (execute-kbd-macro "j"))
            :to-have-buffer-contents "if (foo == [3]) {}")))

(describe "defining tree-edit verbs"
  :var (dummy-verb)

  (before-each
    (setf (symbol-function 'dummy-verb)
          (lambda (noun)))
    (spy-on 'dummy-verb))

  ;; TODO: Test which-key
  (it "sets keybindings"
    (with-base-test-buffer ""
      (let ((evil-tree-state-map (make-sparse-keymap))
            (tree-edit-nodes
             '((:type if_statement
                :key "i"))))
        (define-evil-tree-edit-verb "t" #'dummy-verb)
        (evil-tree-state)
        (expect (key-binding "ti"))
        (expect (not (key-binding "tq")))
        (execute-kbd-macro "ti")
        (expect 'dummy-verb :to-have-been-called-with 'if_statement))))

  ;; TODO
  (xit "uses node overrides"))

(describe "load grammar based on major mode"
  (it "loads grammar"
    (with-base-test-buffer ""
      (expect tree-edit-grammar)))
  (it "errors and disables tree-edit-mode on unknown mode"
    (let ((tree-edit-test-mode #'emacs-lisp-mode))
      (expect (with-base-test-buffer "") :to-throw 'error)))

  ;; TODO
  (xit "uses node overrides"))

(describe "basic navigation"
  (it "can move between sibling nodes"
    (expect (with-tree-test-buffer "if ([foo] == 3) {}"
              (evil-tree-edit-goto-next-sibling))
            :to-have-buffer-contents "if (foo == [3]) {}")
    (expect (with-tree-test-buffer "if (foo == [3]) {}"
              (evil-tree-edit-goto-prev-sibling))
            :to-have-buffer-contents "if ([foo] == 3) {}"))
  (it "can move between parent and children nodes"
    (expect (with-tree-test-buffer "if (foo == [3]) {}"
              (evil-tree-edit-goto-parent))
            :to-have-buffer-contents "if ([foo == 3]) {}")
    (expect (with-tree-test-buffer "if (foo == [3]) {}"
              (evil-tree-edit-goto-parent)
              (evil-tree-edit-goto-parent))
            :to-have-buffer-contents "if [(foo == 3)] {}")
    (expect (with-tree-test-buffer "if (foo == [3]) {}"
              (evil-tree-edit-goto-parent)
              (evil-tree-edit-goto-parent)
              (evil-tree-edit-goto-parent))
            :to-have-buffer-contents "[if (foo == 3) {}]")
    (expect (with-tree-test-buffer "[if (foo == 3) {}]"
              (evil-tree-edit-goto-child))
            :to-have-buffer-contents "if [(foo == 3)] {}")
    (expect (with-tree-test-buffer "if ([foo == 3]) {}"
              (evil-tree-edit-goto-child))
            :to-have-buffer-contents "if ([foo] == 3) {}"))
  (it "can move to sig node"
    (expect (with-tree-test-buffer "{if (foo == [3]) {}}"
              (let ((tree-edit-significant-node-types '(block)))
                (evil-tree-edit-goto-sig-parent)))
            :to-have-buffer-contents "[{if (foo == 3) {}}]")
    (expect (with-tree-test-buffer "{[{if (foo == 3) {}}]}"
              (let ((tree-edit-significant-node-types '(block)))
                (evil-tree-edit-goto-sig-parent)))
            :to-have-buffer-contents "[{{if (foo == 3) {}}}]"))
  (it "sig node gracefully fails"
    (expect (with-tree-test-buffer "{[{if (foo == 3) {}}]}"
              (let ((tree-edit-significant-node-types '()))
                (ignore-errors (evil-tree-edit-goto-sig-parent))))
            :to-have-buffer-contents "{[{if (foo == 3) {}}]}")))

(describe "entering tree state"
  (it "will select the smallest node at point"
    (expect (with-test-buffer "if (|foo == 3) {}"
              (evil-tree-state))
            :to-have-buffer-contents "if ([foo] == 3) {}")
    (expect (with-test-buffer "if| (foo == 3) {}"
              (evil-tree-state))
            :to-have-buffer-contents "[if (foo == 3) {}]")
    (expect (with-test-buffer "i|f (foo == 3) {}"
              (evil-tree-state))
            :to-have-buffer-contents "[if (foo == 3) {}]")
    (expect (with-test-buffer "if (foo =|= 3) {}"
              (evil-tree-state))
            :to-have-buffer-contents "if ([foo == 3]) {}"))
  (expect (with-test-buffer "
if (foo == 3) {
   int x |= 3;
}"
            (evil-tree-state))
          :to-have-buffer-contents "
if (foo == 3) {
   int [x = 3];
}"))

;; TODO: Test avy?
(describe "node creation"
  (it "works with trivial nodes"
    (expect (with-mode-local java-mode
              (tree-edit-make-node 'break_statement '((break_statement . ("break" ";")))))
            :to-equal "break;")
    (expect (with-mode-local java-mode
              (tree-edit-make-node 'continue_statement '((continue_statement . ("continue" ";")))))
            :to-equal "continue;"))
  (it "can select choices based on input"
    (expect (with-mode-local java-mode
              (tree-edit-make-node
               'expression_statement
               '((expression_statement . (identifier ";")) ;; _expression = identifier
                 (identifier . ("TREE_EDIT")))))
            :to-equal "TREE_EDIT;")
    (expect (with-mode-local java-mode
              (tree-edit-make-node
               'argument_list
               '((expression_statement . (call_expression ";")) ;; _expression = identifier
                 (call_expression . (identifier argument_list))
                 (argument_list . ("(" identifier ")"))
                 (identifier . ("TREE_EDIT")))))
            :to-equal "(TREE_EDIT)")
    (expect (with-mode-local java-mode
              (tree-edit-make-node
               'expression_statement
               '((expression_statement . (call_expression ";")) ;; _expression = identifier
                 (call_expression . (identifier argument_list))
                 (argument_list . ("(" identifier ")"))
                 (identifier . ("TREE_EDIT")))))
            :to-equal "TREE_EDIT(TREE_EDIT);")
    (expect (with-mode-local java-mode
              (tree-edit-make-node
               'if_statement
               '((if_statement . ("if" parenthesized_expression expression_statement)) ;; _expression = identifier
                 (expression_statement . (identifier ";"))
                 (parenthesized_expression . ("(" identifier ")"))
                 (identifier . ("TREE")))))
            :to-equal "if(TREE)TREE;"))
  (it "can space words properly"
    ;; FIXME: private void, etc?
    (expect (with-mode-local java-mode
              (tree-edit-make-node
               'modifiers
               '((modifiers . ("private" "void")))))
            :to-equal "private void")
    (expect (with-mode-local java-mode
              (tree-edit-make-node
               'if_statement
               '((if_statement . ("if" parenthesized_expression expression_statement "else" expression_statement)) ;; _expression = identifier
                 (expression_statement . (identifier ";"))
                 (parenthesized_expression . ("(" identifier ")"))
                 (identifier . ("TREE")))))
            :to-equal "if(TREE)TREE;else TREE;")))

;;* Raise node
(describe "raise node"
  (it "replaces the parent node with selected child"
    (expect (with-tree-test-buffer "{[foo] == 3;}"
              (evil-tree-edit-raise))
            :to-have-buffer-contents "{[foo];}")
    (expect (with-tree-test-buffer "{foo == [3];}"
              (evil-tree-edit-raise))
            :to-have-buffer-contents "{[3];}")
    ;; XXX: how to multi-select?
    (expect (with-tree-test-buffer "{if (foo) {[foo;]bar;baz;}}"
              (evil-tree-edit-raise))
            :to-have-buffer-contents "{if (foo) [foo;]}")
    (expect (with-tree-test-buffer "{if (foo) {[foo;]bar;baz;}}"
              (evil-tree-edit-raise)
              (evil-tree-edit-raise))
            :to-have-buffer-contents "{[foo;]}"))
  ;; XXX: there should be a limit to this...
  (it "travels up the syntax tree until a valid construction is found"
    (expect (with-tree-test-buffer "{foo([bar()]);}"
              (evil-tree-edit-raise))
            :to-have-buffer-contents "{[bar()];}")
    (expect (with-tree-test-buffer "{foo(bar([baz]));}"
              (evil-tree-edit-raise))
            :to-have-buffer-contents "{foo([baz]);}")
    (expect (with-tree-test-buffer "{foo(bar([baz], bomb));}"
              (evil-tree-edit-raise))
            :to-have-buffer-contents "{foo([baz]);}"))
  (it "errors if a node cannot be raised"
    (expect (with-tree-test-buffer "{[foo];}"
              (evil-tree-edit-raise))
            :to-throw 'user-error)
    (expect (with-tree-test-buffer "[{foo;}]"
              (evil-tree-edit-raise))
            :to-throw 'user-error)))

(describe "change node"
  (it "enters insert mode after deleting the current node"
    (expect (with-tree-test-buffer "{foo([x]);}"
              (evil-tree-edit-change-node))
            :to-have-buffer-contents "{foo(|);}"))
  (xit "re-enters tree mode on escape and reselects the node"
    (expect (with-tree-test-buffer "{foo([x]);}"
              (evil-tree-edit-change-node)
              (insert "foo")
              (evil-normal-state))
            :to-have-buffer-contents "{foo([foo]);}")
    (expect (with-tree-test-buffer "{foo(x + [y]);}"
              (evil-tree-edit-change-node)
              (insert "z")
              (evil-normal-state))
            :to-have-buffer-contents "{foo(x + [z]);}"))
  ;; TODO
  (xit "only allows string nodes to be changed"
    (expect (with-tree-test-buffer "[{foo;}]"
              (evil-tree-edit-change-node))
            :to-throw 'user-error)
    (expect (with-tree-test-buffer "{[3 + 5];}"
              (evil-tree-edit-change-node))
            :to-throw 'user-error)))

;; (describe "exchange node"
;;   (it "correctly replaces valid transformations")
;;   (xit "does not allow invalid transformations"))

(describe "copy/paste node"
  (it "correctly replaces valid transformations"
    (expect (with-tree-test-buffer "{foo([x]);}"
              (evil-tree-edit-copy)
              (evil-tree-edit-insert-sibling (car kill-ring)))
            :to-have-buffer-contents "{foo(x,[x]);}")
    ;; Regression: "foo.readl()" would parse as an expression_statement with a missing ";"
    (expect (with-tree-test-buffer "{foo([x]);}"
              (evil-tree-edit-copy)
              (evil-tree-edit-insert-sibling "foo.readl()"))
            :to-have-buffer-contents "{foo(x,[foo.readl()]);}")
    (expect (with-tree-test-buffer "{[foo;]bar;}"
              (evil-tree-edit-copy)
              (evil-tree-edit-goto-next-sibling)
              (evil-tree-edit-exchange-node (car kill-ring)))
            :to-have-buffer-contents "{foo;[foo;]}")
    (expect (with-tree-test-buffer "{foo([x]);}"
              (evil-tree-edit-copy)
              (evil-tree-edit-goto-parent)
              (evil-tree-edit-goto-prev-sibling)
              (evil-tree-edit-exchange-node (car kill-ring)))
            :to-have-buffer-contents "{[x](x);}")))

(describe "insert sibling"
  (it "correctly inserts sibling nodes"
    (expect (with-tree-test-buffer "{foo([x]);}"
              (evil-tree-edit-insert-sibling 'identifier))
            :to-have-buffer-contents "{foo(x,[TREE]);}")
    (expect (with-tree-test-buffer "{foo([x]);}"
              (evil-tree-edit-insert-sibling 'method_invocation))
            :to-have-buffer-contents "{foo(x,[TREE()]);}")
    (expect (with-tree-test-buffer "{[foo(x);]}"
              (evil-tree-edit-insert-sibling 'break_statement))
            :to-have-buffer-contents "{foo(x);[break;]}")
    (expect (with-tree-test-buffer "{if(TREE)[{}]}"
              (evil-tree-edit-insert-sibling 'if_statement))
            :to-have-buffer-contents "{if(TREE){}else [if(TREE){}]}")
    (expect (with-tree-test-buffer "{if(TREE)[{}]}"
              (evil-tree-edit-insert-sibling 'block))
            :to-have-buffer-contents "{if(TREE){}else[{}]}")
    (expect (with-tree-test-buffer "{try{}[catch(Exception e) {}]}"
              (evil-tree-edit-insert-sibling 'catch_clause))
            :to-have-buffer-contents "{try{}catch(Exception e) {}[catch(Exception e){}]}")
    (expect (with-tree-test-buffer "{try{}[catch(Exception e) {}]}"
              (evil-tree-edit-insert-sibling 'finally_clause))
            :to-have-buffer-contents "{try{}catch(Exception e) {}[finally{}]}")
    (expect (with-tree-test-buffer "
{
[foo();]// i'm a comment!
}"
              (evil-tree-edit-insert-sibling 'break_statement))
            :to-have-buffer-contents "
{
foo();[break;]// i'm a comment!
}"))
  (xit "can perform multi-node insertions"
    ;; Should select bounds of new named node
    (expect (with-tree-test-buffer "{if(foo) [bar;]}"
              (evil-tree-edit-insert-sibling '("else" 'block)))
            :to-have-buffer-contents "{if(foo) bar;else{TREE;}}"))
  (it "does not allow invalid transformations"
    (expect (with-tree-test-buffer "{foo([x]);}"
              (evil-tree-edit-insert-sibling 'break_statement))
            :to-throw 'user-error)
    ;; Only one else block
    (expect (with-tree-test-buffer "{if(TREE){}else[{}]}"
              (evil-tree-edit-insert-sibling 'block))
            :to-throw 'user-error)
    ;; Only one finally clause
    (expect (with-tree-test-buffer "{try{}catch(Exception e) {}[finally{}]}"
              (evil-tree-edit-insert-sibling 'finally_clause))
            :to-throw 'user-error)
    ;; Catch cannot go after finally
    (expect (with-tree-test-buffer "{try{}catch(Exception e) {}[finally{}]}"
              (evil-tree-edit-insert-sibling 'catch_clause))
            :to-throw 'user-error)))

(describe "insert child"
  (it "correctly inserts child nodes"
    ;; Should select bounds of new named node
    (expect (with-tree-test-buffer "{if (TREE) [{}]}"
              (evil-tree-edit-insert-child 'break_statement))
            :to-have-buffer-contents "{if (TREE) {[break;]}}")
    (expect (with-tree-test-buffer "{foo[()];}"
              (evil-tree-edit-insert-child 'identifier))
            :to-have-buffer-contents "{foo([TREE]);}"))
  (it "can insert text fragments"
    ;; Should select bounds of new named node
    (expect (with-tree-test-buffer "{if (TREE) [{}]}"
              (evil-tree-edit-insert-child "break;"))
            :to-have-buffer-contents "{if (TREE) {[break;]}}")
    (expect (with-tree-test-buffer "{foo[()];}"
              (evil-tree-edit-insert-child "3 + 4"))
            :to-have-buffer-contents "{foo([3 + 4]);}"))
  (it "does not allow invalid transformations"
    (expect (with-tree-test-buffer "{foo[()];}"
              (evil-tree-edit-insert-child 'break_statement))
            :to-throw 'user-error)
    (expect (with-tree-test-buffer "{foo[()];}"
              (evil-tree-edit-insert-child "break;"))
            :to-throw 'user-error)))

(describe "slurp"
  (it "correctly slurps nodes"
    (expect (with-tree-test-buffer "{if(foo)[{}]break;}"
              (evil-tree-edit-slurp))
            :to-have-buffer-contents "{if(foo)[{break;}]}")
    (expect (with-tree-test-buffer "{if(foo)[{break;}]break;}"
              (evil-tree-edit-slurp))
            :to-have-buffer-contents "{if(foo)[{break;break;}]}")
    (expect (with-tree-test-buffer "{if(foo)[{break;}] if(foobar){} else if (qwerty){}}"
              (evil-tree-edit-slurp))
            :to-have-buffer-contents "{if(foo)[{break;if(foobar){} else if (qwerty){}}] }")
    (expect (with-tree-test-buffer "{foo(bar[()], x, y, z)}"
              (evil-tree-edit-slurp))
            :to-have-buffer-contents "{foo(bar[(x)],y, z)}")
    (expect (with-tree-test-buffer "{foo(bar[(x)], y, z)}"
              (evil-tree-edit-slurp))
            :to-have-buffer-contents "{foo(bar[(x,y)],z)}"))
  (it "gracefully fails if slurp is impossible"
    (expect (with-tree-test-buffer "{foo(bar[()])}"
              (ignore-errors (evil-tree-edit-slurp)))
            :to-have-buffer-contents "{foo(bar[()])}")
    (expect (with-tree-test-buffer "{foo(bar[(x)])}"
              (ignore-errors (evil-tree-edit-slurp)))
            :to-have-buffer-contents "{foo(bar[(x)])}")
    (expect (with-tree-test-buffer "{if(foo)[{}]}"
              (ignore-errors (evil-tree-edit-slurp)))
            :to-have-buffer-contents "{if(foo)[{}]}")
    (expect (with-tree-test-buffer "{if(foo)[{break;}]}"
              (ignore-errors (evil-tree-edit-slurp)))
            :to-have-buffer-contents "{if(foo)[{break;}]}")))

(describe "barf"
  (it "correctly barfs nodes"
    (expect (with-tree-test-buffer "{if(foo)[{break;}]}"
              (evil-tree-edit-barf))
            :to-have-buffer-contents "{if(foo)[{}]break;}")
    (expect (with-tree-test-buffer "{if(foo)[{break;break;}]}"
              (evil-tree-edit-barf))
            :to-have-buffer-contents "{if(foo)[{break;}]break;}")
    (expect (with-tree-test-buffer "{if(foo)[{break;if(foobar){} else if (qwerty){}}] }"
              (evil-tree-edit-barf))
            :to-have-buffer-contents "{if(foo)[{break;}] if(foobar){} else if (qwerty){}}")
    (expect (with-tree-test-buffer "{foo(bar[(x)], y, z)}"
              (evil-tree-edit-barf))
            :to-have-buffer-contents "{foo(bar[()],x, y, z)}")
    (expect (with-tree-test-buffer "{foo(bar[(x,y)], z)}"
              (evil-tree-edit-barf))
            :to-have-buffer-contents "{foo(bar[(x)],y, z)}"))
  (it "gracefully fails if barf is impossible"
    (expect (with-tree-test-buffer "{foo(bar[()])}"
              (ignore-errors (evil-tree-edit-barf)))
            :to-have-buffer-contents "{foo(bar[()])}")
    (expect (with-tree-test-buffer "{if(foo)[{}]}"
              (ignore-errors (evil-tree-edit-barf)))
            :to-have-buffer-contents "{if(foo)[{}]}")))

(describe "wrap node"
  (it "correctly wraps nodes"
    (expect (with-tree-test-buffer-avy "{[break;]}" 0
              (let ((tree-edit-syntax-snippets `((block . ("{" expression_statement "}")) . ,tree-edit-syntax-snippets)))
                (evil-tree-edit-wrap-node 'if_statement)))
            :to-have-buffer-contents "{[if(TREE)break;]}")
    (expect (with-tree-test-buffer-avy "{[break;]}" 1
              (let ((tree-edit-syntax-snippets `((block . ("{" expression_statement "}")) . ,tree-edit-syntax-snippets)))
                (evil-tree-edit-wrap-node 'if_statement)))
            :to-have-buffer-contents "{[if(TREE){break;}]}")
    (expect (with-tree-test-buffer "{[3 + 3];}"
              (let ((tree-edit-syntax-snippets `((argument_list . ("(" expression ")")) . ,tree-edit-syntax-snippets)))
                (evil-tree-edit-wrap-node 'method_invocation)))
            :to-have-buffer-contents "{[TREE(3 + 3)];}"))
  (it "gracefully fails if node is unwrappable"
    (expect (with-tree-test-buffer "{[break;]}"
              (ignore-errors (evil-tree-edit-wrap-node 'method_invocation)))
            :to-have-buffer-contents "{[break;]}")
    (expect (with-tree-test-buffer "{[3 + 3];}"
              (ignore-errors (evil-tree-edit-wrap-node 'method_invocation)))
            :to-have-buffer-contents "{[3 + 3];}")))

;; (describe "modify node"
;;   (xit "correctly replaces valid transformations")
;;   (xit "does not allow invalid transformations")
;;   (xit "provides an accurate list of possible replacements"))

(describe "delete node"
  (it "correctly replaces valid transformations"
    ;; Should select bounds of new named node
    (expect (with-tree-test-buffer "{foo([x]);}"
              (evil-tree-edit-delete-node))
            :to-have-buffer-contents "{foo[()];}")
    ;; TODO: Check tree-equals, I don't care about formatting
    (expect (with-tree-test-buffer "
class Main {
  [void foo() {}]
  void bar() {}
}"
              (evil-tree-edit-delete-node))
            :to-have-buffer-contents "
class Main {[void bar() {}]
}")
    (expect (with-tree-test-buffer "
class Main {
  // should we delete this?
  [void foo() {}]
  void bar() {}
}"
              (evil-tree-edit-delete-node))
            :to-have-buffer-contents "
class Main {
  // should we delete this?
  [void bar() {}]
}")
    (expect (with-tree-test-buffer "
class Main {
  // should we delete this?
  // i'm a second comment in a row
  [void foo() {}]
  void bar() {}
  // here too
}"
              (evil-tree-edit-delete-node))
            :to-have-buffer-contents "
class Main {
  // should we delete this?
  // i'm a second comment in a row
  [void bar() {}]
  // here too
}")
    (expect (with-tree-test-buffer "
{
  break;
  [break;]
  break;
}"
              (evil-tree-edit-delete-node))
            :to-have-buffer-contents "
{
  break;
  [break;]
}")
    (expect (with-tree-test-buffer "
class Main {[public] void main() {}
}"
              ;; FIXME: Test selects anonymous keyword 'public', instead of 'modifiers
              (evil-tree-edit-goto-parent)
              (evil-tree-edit-delete-node))
            :to-have-buffer-contents "
class Main {[void] main() {}
}")
    (expect (with-tree-test-buffer "{foo([x],y);}"
              (evil-tree-edit-delete-node))
            :to-have-buffer-contents "{foo([y]);}")
    (expect (with-tree-test-buffer "{foo(x,[y]);}"
              (evil-tree-edit-delete-node))
            :to-have-buffer-contents "{foo([x]);}")

    (expect (with-tree-test-buffer "{foo(x);[break;]}"
              (evil-tree-edit-delete-node))
            :to-have-buffer-contents "{[foo(x);]}"))
  (it "does not allow invalid transformations"
    (expect (with-tree-test-buffer "{[foo](x);}"
              (evil-tree-edit-delete-node))
            :to-throw 'user-error)
    (expect (with-tree-test-buffer "
class Main {
  [void] main() {}
}"
              (evil-tree-edit-delete-node))
            :to-throw 'user-error))
  (xit "can deal with comments in between relevant syntax"
    (expect (with-tree-test-buffer "{
foo(x, // comment
    [y]);
}"
              (evil-tree-edit-delete-node))
            :to-have-buffer-contents "{
foo(x // comment
    );
}")))
