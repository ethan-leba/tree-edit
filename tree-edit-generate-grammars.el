;;; tree-edit-generate-grammars.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Ethan Leba
;;
;; Example usage: cask emacs --script dev/tree-edit-generate-grammars.el ~/grammar.json python python-mode
;;
(require 'dash)
(require 'json)
(require 'cl-extra)
(require 'tree-edit)

(defconst tree-edit--grammar-template
  ";;; tree-edit-%1$s-grammar.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Ethan Leba
;; Author: Ethan Leba <ethanleba5@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.1.0
;; Package-Requires: ((emacs \"27.0\"))
;; Homepage: https://github.com/ethan-leba/tree-edit
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This is an autogenerated file from 'tree-edit-generate-grammars.el' that contains the grammar
;; and other precalculations for the %1$s language. Please do not manually modify this!
;;
;;; Code:
(require 'mode-local)
(require 'tree-edit)

(setq-mode-local
 %s

 tree-edit-grammar
 %s

 tree-edit--identifier-regex
 %s

 tree-edit--supertypes
 %s

 tree-edit--subtypes
 %s

 tree-edit--alias-map
 %s

 tree-edit--containing-types
 %s)

(provide 'tree-edit-%1$s-grammar)
;;; tree-edit-%1$s-grammar.el ends here")

(defconst tree-edit--config-template
  ";;; tree-edit-%1$s-grammar.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Ethan Leba
;; Author: Ethan Leba <ethanleba5@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.1.0
;; Package-Requires: ((emacs \"27.0\"))
;; Homepage: https://github.com/ethan-leba/tree-edit
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This file contains key bindings and other configuration for `tree-edit' to
;; work with %1$s.
;;
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:
(require 'mode-local)
(require 'tree-edit-%1$s-grammar)
(require 'tree-edit)

(setq-mode-local
 %s

 tree-edit-whitespace-rules
 '()

 tree-edit-significant-node-types
 '()

 tree-edit-placeholder-node-type
 nil

 tree-edit-node-deletion-override
 nil

 tree-edit-node-replacement-override
 nil

 tree-edit-node-insertion-override
 nil

 tree-edit-dwim-node-alist
 nil

 tree-edit-nodes
 nil

 tree-edit-query-nodes
 nil

 ;; TODO: Check me!
 tree-edit-syntax-snippets
 %s)

(provide 'tree-edit-%1$s)
;;; tree-edit-%1$s.el ends here")

(defun tree-edit--invert-supertypes (supertypes)
  "Invert SUPERTYPES alist into subtypes."
  (let ((subtypes '()))
    (progn
      (mapc (-lambda ((sub . supers))
              (mapc
               (lambda (super)
                 (if-let ((record (assoc super subtypes)))
                     (setcdr record (cons sub (cdr record)))
                   (setq subtypes `((,super . (,sub)) . ,subtypes))))
               supers))
            supertypes)
      subtypes)))

(defun tree-edit--generate-supertype (type grammar possible-supertypes)
  "Return TYPE's supertypes (and itself) in GRAMMAR."
  (->> (-map #'car grammar)
       (--filter (and (or (s-starts-with-p "_" (symbol-name it))
                          (member it possible-supertypes))
                      (reazon-run 1 q (tree-edit-parseo (alist-get it grammar) `(,type) '()))))
       (--mapcat `(,it . ,(tree-edit--generate-supertype it grammar possible-supertypes)))
       (-uniq)
       (cons type)))

(defun tree-edit--generate-supertypes (grammar possible-supertypes)
  "Return an alist of a type to it's supertypes (and itself) in GRAMMAR."
  (--map
   (let ((type (car it)))
     `(,type . ,(tree-edit--generate-supertype type grammar possible-supertypes)))
   grammar))

(defun tree-edit--invert-supertypes (supertypes)
  "Invert SUPERTYPES alist into subtypes."
  (let ((subtypes '()))
    (progn
      (mapc (-lambda ((sub . supers))
              (mapc
               (lambda (super)
                 (if-let ((record (assoc super subtypes)))
                     (setcdr record (cons sub (cdr record)))
                   (setq subtypes `((,super . (,sub)) . ,subtypes))))
               supers))
            supertypes)
      subtypes)))

(defun tree-edit--generate-containing-types (grammar)
  "Return an alist of a type to all the types it contained in it's GRAMMAR."
  (--map `(,(car it) . ,(tree-edit--extract-types (cdr it))) grammar))

(defun tree-edit--extract-types (grammar)
  "Return a list of all the symbol types in GRAMMAR."
  (pcase grammar
    ((and `((type . ,type)
            (value . ,_)
            (content . ,content))
          (guard (string-prefix-p "PREC" type)))
     ;; Silence the foolish linter.
     (ignore type)
     (tree-edit--extract-types content))
    (`((type . "SEQ")
       (members . ,members))
     (-mapcat #'tree-edit--extract-types members))
    (`((type . "ALIAS")
       (content . ,_)
       (named . ,_)
       (value . ,alias-name))
     `(,alias-name))
    (`((type . "IMMEDIATE_TOKEN")
       (content . ,content))
     (tree-edit--extract-types content))
    (`((type . "REPEAT")
       (content . ,content))
     (tree-edit--extract-types content))
    (`((type . "REPEAT1")
       (content . ,content))
     (tree-edit--extract-types content))
    (`((type . "FIELD")
       (name . ,_)
       (content . ,content))
     (tree-edit--extract-types content))
    (`((type . "SYMBOL")
       (name . ,name))
     `(,name))
    (`((type . "CHOICE")
       (members . ,members))
     (-mapcat #'tree-edit--extract-types members))
    (_ '())))

(defun tree-edit--generate-alias-names (grammar)
  "Return an alist of a type to all the types it contained in it's GRAMMAR.

Assumes that each node has only one alias. This may not be true!"
  (--map `(,(car it) . ,(tree-edit--extract-aliases (cdr it))) grammar))

(defun tree-edit--extract-aliases (grammar)
  "Return a list of all the symbol types in GRAMMAR."
  (pcase grammar
    ((and `((type . ,type)
            (value . ,_)
            (content . ,content))
          (guard (string-prefix-p "PREC" type)))
     ;; Silence the foolish linter.
     (ignore type)
     (tree-edit--extract-aliases content))
    (`((type . "SEQ")
       (members . ,members))
     (-mapcat #'tree-edit--extract-aliases members))
    (`((type . "ALIAS")
       (content . ((type . "SYMBOL")
                   (name . ,name)))
       (named . ,_)
       (value . ,alias-name))
     `((,name . ,alias-name)))
    (`((type . "ALIAS")
       (content . ,_)
       (named . ,_)
       (value . ,_))
     '())
    (`((type . "REPEAT")
       (content . ,content))
     (tree-edit--extract-aliases content))
    (`((type . "REPEAT1")
       (content . ,content))
     (tree-edit--extract-aliases content))
    (`((type . "FIELD")
       (name . ,_)
       (content . ,content))
     (tree-edit--extract-aliases content))
    (`((type . "SYMBOL")
       (name . ,name))
     '())
    (`((type . "CHOICE")
       (members . ,members))
     (-mapcat #'tree-edit--extract-aliases members))
    (_ '())))

(defun tree-edit--inline-type (node grammar)
  "Inlines anonymous nodes for NODE in GRAMMAR.

https://tree-sitter.github.io/tree-sitter/using-parsers#named-vs-anonymous-nodes"
  (pcase node
    ((and `((type . ,type)
            (value . ,value)
            (content . ,content))
          (guard (string-prefix-p "PREC" type)))
     `((type . ,type)
       (value . ,value)
       (content . ,(tree-edit--inline-type content grammar))))
    (`((type . "SEQ")
       (members . ,members))
     `((type . "SEQ")
       (members . ,(--map (tree-edit--inline-type it grammar) members))))
    (`((type . "ALIAS")
       (content . ,content)
       (named . ,named)
       (value . ,value))
     `((type . "ALIAS")
       (content . ,(tree-edit--inline-type content grammar))
       (named . ,named)
       (value . ,value)))
    (`((type . "IMMEDIATE_TOKEN")
       (content . ,content))
     `((type . "IMMEDIATE_TOKEN")
       (content . ,(tree-edit--inline-type content grammar))))
    (`((type . "REPEAT")
       (content . ,content))
     `((type . "REPEAT")
       (content . ,(tree-edit--inline-type content grammar))))
    (`((type . "REPEAT1")
       (content . ,content))
     `((type . "REPEAT1")
       (content . ,(tree-edit--inline-type content grammar))))
    (`((type . "FIELD")
       (name . ,name)
       (content . ,content))
     `((type . "FIELD")
       (name . ,name)
       (content . ,(tree-edit--inline-type content grammar))))
    ((and `((type . "SYMBOL")
            (name . ,name))
          (guard (and (string-prefix-p "_" (if (symbolp name) (symbol-name name) name))
                      (alist-get (if (symbolp name) name (intern name)) grammar))))
     (tree-edit--inline-type (alist-get (if (symbolp name) name (intern name)) grammar) grammar))
    (`((type . "CHOICE")
       (members . ,members))
     `((type . "CHOICE")
       (members . ,(--map (tree-edit--inline-type it grammar) members))))
    (other other)))

(defun tree-edit--process-grammar (node)
  "Convert node types to symbols in NODE."
  (pcase node
    ((and `((type . ,type)
            (value . ,value)
            (content . ,content))
          (guard (string-prefix-p "PREC" type)))
     `((type . ,type)
       (value . ,value)
       (content . ,(tree-edit--process-grammar content))))
    (`((type . "SEQ")
       (members . ,members))
     `((type . "SEQ")
       (members . ,(-map #'tree-edit--process-grammar members))))
    (`((type . "ALIAS")
       (content . ,content)
       (named . ,named)
       (value . ,value))
     `((type . "ALIAS")
       (content . ,(tree-edit--process-grammar content))
       (named . ,named)
       (value . ,(intern value))))
    (`((type . "REPEAT")
       (content . ,content))
     `((type . "REPEAT")
       (content . ,(tree-edit--process-grammar content))))
    (`((type . "REPEAT1")
       (content . ,content))
     `((type . "REPEAT1")
       (content . ,(tree-edit--process-grammar content))))
    (`((type . "FIELD")
       (name . ,name)
       (content . ,content))
     `((type . "FIELD")
       (name . ,name)
       (content . ,(tree-edit--process-grammar content))))
    (`((type . "SYMBOL")
       (name . ,name))
     `((type . "SYMBOL")
       (name . ,(intern name))))
    (`((type . "CHOICE")
       (members . ,members))
     `((type . "CHOICE")
       (members . ,(-map #'tree-edit--process-grammar members))))
    (other other)))

(defun tree-edit--extract-word-regex (grammar)
  "Extract regex for words in GRAMMAR."
  (let ((identifier-node (alist-get (intern (alist-get 'word grammar)) (alist-get 'rules grammar))))
    (pcase identifier-node
      (`((type . "PATTERN")
         (value . ,regex)) regex)
      (_ (error "Expected regex node, found %s" identifier-node)))))


(defun tree-edit--extract-grammar (grammar)
  "Retrieve and format grammar rules for GRAMMAR."
  (map-apply (lambda (node node-grammar) (cons node (tree-edit--inline-type node-grammar grammar))) grammar))

(defun tree-edit--pretty-print (sexp)
  "Prettyprint SEXP to a string."
  (with-temp-buffer
    (cl-prettyprint sexp)
    (buffer-string)))

(defun tree-edit--generate-grammar-file (path name mode)
  "Generate file contents based on the grammar at PATH for NAME and MODE."
  (let* ((raw-grammar
          (let ((json-array-type 'list))
            (json-read-file path)))
         (grammar
          (map-apply (lambda (type node) (cons type (tree-edit--process-grammar node)))
                     (alist-get 'rules raw-grammar)))
         (grammar (tree-edit--extract-grammar grammar))
         (supertypes (tree-edit--generate-supertypes grammar (-map #'intern (alist-get 'supertypes raw-grammar)))))
    (format tree-edit--grammar-template
            name
            mode
            `',(tree-edit--pretty-print grammar)
            nil ;; TODO: Use grammar defined regex
            `',(tree-edit--pretty-print supertypes)
            `',(tree-edit--pretty-print (tree-edit--invert-supertypes supertypes))
            `',(tree-edit--pretty-print (tree-edit--generate-alias-names grammar))
            `',(tree-edit--pretty-print (tree-edit--generate-containing-types grammar)))))

(defun tree-edit-generate-grammars (path name mode)
  (message (format "Parsing %s grammar at %s." name path))
  (let ((time-start (float-time))
        (templated-string (tree-edit--generate-grammar-file path name mode)))
    (with-temp-file (format "tree-edit-%s-grammar.el" name)
      (insert templated-string))
    (message
     (format "Completed after %s seconds."
             (round (- (float-time) time-start))))))

(defvar tree-edit--store
  (expand-file-name "store" (file-name-directory (locate-library "tree-edit.el"))))

(make-directory tree-edit--store t)
(defvar tree-edit--hashes ())

(defun tree-edit--needs-recompilation (lang hash &optional force)
  "Return non-nil when the stored hash for LANG does not match HASH."
  (let* ((hashfile (expand-file-name ".filehashes.el" tree-edit--store))
         (contents
          (when (file-exists-p hashfile)
            (with-temp-buffer
              (insert-file-contents-literally hashfile)
              (read (buffer-string)))))
         (old-hash (alist-get lang contents nil nil #'equal)))
    (if (and (not force) (string= old-hash hash)) nil
      (delete-file hashfile)
      (append-to-file
       (prin1-to-string (if old-hash
                            (--map-first (equal (car it) lang) `(,lang . ,hash) contents)
                          (cons `(,lang . ,hash) contents)))
       nil
       hashfile)
      t)))

(defun tree-edit--lang-compile (path mode &optional force)
  "Compile grammar at PATH if necessary."
  (let* ((default-directory path)
         (grammar-path (expand-file-name "./src/grammar.json"))
         (parser-name (s-replace "_" "-" (alist-get 'name (json-read-file grammar-path))))
         (grammar-hash
          (with-temp-buffer
            (insert-file-contents grammar-path)
            (secure-hash 'sha1 (current-buffer))))
         (shared-lib-name
          (format
           "%s.%s"
           parser-name
           (pcase system-type
             ('darwin "dylib")
             ('gnu/linux "so")
             (_ (error "Unsupported system-type %s" system-type))))))
    (when (tree-edit--needs-recompilation parser-name grammar-hash force)
      (message "Compiling grammar at %s" path)
      (with-temp-buffer
        (unless (zerop (apply #'call-process "gcc" nil t nil
                              "./src/parser.c" "-I./src/" "--shared" "-O3" "-o"
                              shared-lib-name (if (file-exists-p "./src/scanner.cc")
                                                  '("./src/scanner.cc" "-lstdc++"))))
          (error "Unable to compile grammar, please file a bug report\n%s" (buffer-string))))
      (rename-file shared-lib-name
                   (expand-file-name "bin/" tree-sitter-langs-grammar-dir)
                   :ok-if-already-exists)
      (with-temp-file (expand-file-name (format "tree-edit-%s-grammar.el" parser-name) tree-edit--store)
        (insert (tree-edit--generate-grammar-file grammar-path parser-name mode)))
      (message "Completed compilation"))))

(provide 'tree-edit-generate-grammars)
;;; tree-edit-generate-grammars.el ends here
