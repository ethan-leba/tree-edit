;;; tree-edit-build.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Ethan Leba
;;
;; Author: Ethan Leba <ethanleba5@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Utilities for compiling tree-sitter grammars and preprocessing them for use
;; with `tree-edit'.
;;
;;; Code:
(require 'dash)
(require 'json)
(require 'cl-extra)
(require 'tree-edit)

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

(defun tree-edit--generate-grammar-file (path)
  "Generate file contents based on the grammar at PATH."
  (let* ((raw-grammar
          (let ((json-array-type 'list))
            (json-read-file path)))
         (grammar
          (map-apply (lambda (type node) (cons type (tree-edit--process-grammar node)))
                     (alist-get 'rules raw-grammar)))
         (grammar (tree-edit--extract-grammar grammar))
         ;; FIXME, I broke the reazon queries?
         (supertypes (tree-edit--generate-supertypes grammar (-map #'intern (alist-get 'supertypes raw-grammar)))))
    (prin1-to-string
     `((tree-edit-grammar . ,grammar)
       (tree-edit--supertypes . ,supertypes)
       (tree-edit--subtypes . ,(tree-edit--invert-supertypes supertypes))
       (tree-edit--alias-map . ,(tree-edit--generate-alias-names grammar))
       (tree-edit--containing-types . ,(tree-edit--generate-containing-types grammar))))))

;;;###autoload
(cl-defun tree-edit-install-language-grammar (lang)
  "Compile grammar at PATH, and place the resulting shared library in DESTINATION."
  (treesit-install-language-grammar lang)

  (unless (treesit-language-available-p lang)
    (princ (string-join (directory-files (expand-file-name ".test-grammars/tree-sitter")) "\n"))
    (error "%S grammar did not compile properly!" lang))

  ;; XXX: We're cloning the repo again since `treesit-install-language-grammar'
  ;;      deletes the repo after using it, sadly.

  (-let* ((default-directory (make-temp-file "tree-edit-workdir" t))
          (workdir (expand-file-name "repo"))
          ((url revision source-dir) (alist-get lang treesit-language-source-alist))
          (grammar
           (expand-file-name "grammar.json" (expand-file-name (or source-dir "src") workdir))))
    (if revision
        (treesit--call-process-signal
         "git" nil t nil "clone" url "--depth" "1" "--quiet"
         "-b" revision workdir)
      (treesit--call-process-signal
       "git" nil t nil "clone" url "--depth" "1" "--quiet"
       workdir))
    (make-directory tree-edit-storage-dir 'parents)
    (with-temp-file (expand-file-name (format "tree-edit-%s-grammar.el"
                                              (thread-last grammar
                                                           (json-read-file)
                                                           (alist-get 'name)))
                                      tree-edit-storage-dir)
      (message "Processing grammar file... this may take a minute or two")
      ;; TODO: This is quite slow... we should speed this up or cache it
      (-> grammar (tree-edit--generate-grammar-file) (insert)))

    (message "Completed compilation")))

;;;###autoload
(defun tree-edit-install-grammars-wizard (force-rebuild &optional accept-all)
  "Install all available tree-sitter grammars compatible with tree-edit.

FORCE-REBUILD, if non-nil or C-u prefix is used, forces a rebuild of the grammars even if cached.
ACCEPT-ALL, if non-nil, installs all grammars without confirmation."
  (interactive "P")
  (pcase-dolist
      (`(,lang . ,url)
       '((python "https://github.com/tree-edit/tree-sitter-python")
         (c "https://github.com/tree-edit/tree-sitter-c")
         (elisp "https://github.com/tree-edit/tree-sitter-elisp")
         (java "https://github.com/tree-edit/tree-sitter-java")))
    (when
        ;; TODO: Some way to detect if the grammar is out of date
        (and (or (not (treesit-language-available-p lang)) force-rebuild)
             (or accept-all
                 (y-or-n-p (format "Would you like to install the %S grammar? " lang))))

      (add-to-list 'treesit-language-source-alist `(,lang . ,url))
      (message "Building %s grammar" lang)
      (tree-edit-install-language-grammar lang)))
  (message "Installations complete."))

(provide 'tree-edit-build)
;;; tree-edit-build.el ends here
