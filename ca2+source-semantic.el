;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.



(defun ca-source-semantic-find-local-tag (tag)
  (let* ((p (point))
	 (a (or ca-source-semantic-context-cntxt
		(semantic-analyze-current-context p)))
	 (ca-source-semantic-context-cntxt a)
	 (scope (oref a scope))
	 (localvar (oref scope localvar))
	 (type (semantic-tag-type tag))
	 (name (semantic-tag-name tag))
	 (ptag nil)
	 (tmp (nconc
	       ;; Argument list and local variables
	       (semantic-find-tags-by-type type localvar)
	       ;; The current scope
	       (semantic-find-tags-by-type type (oref scope fullscope))))
	 localtags)

    (dolist (ptag tmp)
      (if (or 
	   ;; XXX hack to find out whether var is from local buffer/class... just a guess
	   (not (arrayp (car (reverse ptag)))) 
	   ;; filter out local vars that were defined after point
	   (< (aref (car (reverse ptag)) 1) p))
	  (push ptag localtags)))

    (when localtags
      (setq case-fold-search t)
      (setq ptag (or
		  (find-if '(lambda (item) 
			      (string-equal name (semantic-tag-name item))) 
			   localtags)
		  (find-if '(lambda (item) 
			      (string-match name (semantic-tag-name item))) 
			   localtags)
		  (find-if '(lambda (item) 
			      (string-match (semantic-tag-name item) name)) 
			   localtags)
		  (car localtags)))
      (when ptag
	(cond ((and (semantic-tag-get-attribute tag :pointer)
		    (not (semantic-tag-get-attribute ptag :pointer))
		    (not (semantic-tag-get-attribute ptag :dereference)))
	       (concat "&" (semantic-tag-name ptag)))
	      ;; ((and (semantic-tag-get-attribute ptag :pointer)
	      ;; 	    (not (semantic-tag-get-attribute tag :pointer)))
	      ;;  (concat "*" (semantic-tag-name ptag)))
	      (t (semantic-tag-name ptag)))))))


;; modified from semantic-format.el, creates yas-template
(defun ca-source-semantic-format-tag-arguments (args formatter)
  "Format the argument list ARGS with FORMATTER.
FORMATTER is a function used to format a tag.
COLOR specifies if color should be used."

  (let ((template nil)
	(cnt 1))
    (when (and args (not (equal (semantic-tag-type (car args)) "void")))
      ;;(message "tag %s" (car args))
      (while args
	(push 
	 (concat "${" (number-to-string cnt) ":"
		 (or
		  ;; try find matchin candidate
		  (and (semantic-tag-p (car args))
		       (ca-source-semantic-find-local-tag (car args)))
		  ;; try tag formatter
		  (and (semantic-tag-name (car args))
		       (upcase (semantic-tag-name (car args))))
		  ;; (and formatter
		  ;;      (semantic-tag-p (car args))
		  ;;      (not (string= (semantic-tag-name (car args)) ""))
		  ;;      (funcall formatter (car args) nil nil))
		  ;; fallback
		  (semantic-format-tag-name-from-anything
		   (car args) nil nil 'variable))
		 "}")
	 template)
	(setq cnt (1+ cnt))
	(setq args (cdr args)))
      ;;semantic-function-argument-separator
      (mapconcat 'identity (nreverse template) ", "))))


(defun ca-source-semantic-continue-function (tag)
  (yas/expand-snippet 
   (concat "(" (ca-source-semantic-format-tag-arguments
		(semantic-tag-function-arguments tag)
		#'semantic-format-tag-prototype) 
	   ")"
	   (unless(looking-at "[[:space:]]*\\()\\|,\\)") ";")))
  ;; finish ca2+
  ;; XXX hack
  (setq ca-current-source nil)
  nil)

(defun ca-source-semantic-continue-operator (tag)
  
  (let ((args (semantic-tag-function-arguments tag))
	desired-type)
    (when args
      (setq desired-type (semantic-tag-type (car args)))
      (ca-semantic-completions nil desired-type))))

(defun ca-source-semantic-continue-variable (tag)
  (let* ((ttype (semantic-tag-type tag))
	 (refsign "")
	 type members)
    (while (and ttype (listp ttype) (not (semantic-tag-type-members ttype)))
      ;; find parent of variable
      (setq type (semantic-analyze-find-tag (car ttype)))

      (or (setq ttype (semantic-tag-get-attribute type :typedef))
	  (setq ttype (semantic-tag-get-attribute type :class))))

    (when (and ttype (listp ttype))
      (setq type ttype))

    (setq members (semantic-tag-type-members type))
    
    (when members
      ;;sort members with ca2+semantic
      (when (boundp 'ca-semantic-analyze-cache-tags)
	(let* ((a (or
		   ;; reuse context as desired-type stays the same 
		   ca-source-semantic-context-cntxt
		   ;; get current context
		   (semantic-analyze-current-context (point))))
	       ;; type that is expected in current context
	       (desired-type (semantic-analyze-type-constraint a)))
	  ;; members of candidate sorted by reachability
	  (setq members 
		(if (not ca-semantic-analyze-cache-tags)
		    (ca-semantic-analyze-possible-completions a)
		  (ca-semantic-completions type desired-type members)))))
    
      ;; remove public/private labels
      (setq members (delete-if 
		     '(lambda (tag) 
			(or (not (semantic-tag-type tag)) ;; filter labels: have no type
			    (semantic-tag-get-attribute tag :constructor-flag)
			    (semantic-tag-get-attribute tag :destructor-flag)))
		     members))

      (insert (if (semantic-tag-get-attribute tag :pointer) "->" "."))

      (mapcar '(lambda(tag) (cons (semantic-tag-name tag) tag)) members))))

(defun ca-source-semantic-continue (candidate)
  (let ((tag (cdr-safe candidate))
	(cands nil))
    (when (semantic-tag-p tag)
      (setq ca-prefix "")
      (setq ca-initial-prefix "")
      (cond 
       ((semantic-tag-get-attribute tag :operator-flag)
	(setq cands (ca-source-semantic-continue-operator tag)))
       ((eq (semantic-tag-class tag) 'function)
	(setq cands (ca-source-semantic-continue-function tag)))
       ((eq (semantic-tag-class tag) 'variable)
	(setq cands (ca-source-semantic-continue-variable tag)))))
    cands))

;; semantic context completion
(defvar ca-source-semantic-context-completions nil)
(defvar ca-source-semantic-context-cntxt nil)

(defun ca-source-semantic-context-decider-1 ()
  (unless (looking-at "\\w")
  (let* ((p (point))
	 (a (semantic-analyze-current-context p))
	 ;; XXX make this an option 
	 ;;(syms (if a (semantic-ia-get-completions a p)))
	 (syms (if a (ca-semantic-analyze-possible-completions a)))
	 (completions (mapcar 
		       '(lambda(tag) (cons (semantic-tag-name tag) tag))
		       syms)))
    (when completions
      (setq ca-source-semantic-context-cntxt a)
      (setq ca-source-semantic-context-completions completions)
      (or (car-safe (bounds-of-thing-at-point 'symbol))
	  p)))))

;; decider works also with function argument templates, inserted 
;; text needs to be deleted before semantic-analyze-current-context
;; can do its work
(defun ca-source-semantic-context-decider ()
  (let* ((overlays (overlays-at (point)))
	 (overlay (find-if '(lambda(ov) 
			      (overlay-get ov 'yas/snippet)) 
			   overlays)))
    (if overlay
	(let* ((start (overlay-start overlay))
	       (end (overlay-end overlay))
	       (reg (buffer-substring start end))
	       (foo (delete-region start end))
	       (prefix-start (ca-source-semantic-context-decider-1)))
	  (if prefix-start
	      prefix-start
	    (insert reg)
	    (goto-char start)
	    nil))
      (ca-source-semantic-context-decider-1))))

(defun ca-source-semantic-context-candidates (prefix)
  ca-source-semantic-context-completions)

(defun ca-source-semantic-action (candidate)
  (if (not candidate)
      (progn
	(when (looking-back "\\.") 
	  (delete-backward-char 1))
	(when (looking-back "->") 
	  (delete-backward-char 2)))

    (when (and (semantic-tag-p (cdr-safe candidate))
	       (semantic-tag-get-attribute (cdr candidate) :operator-flag))
      (delete-backward-char (length (car candidate)))
      (when (looking-back "\\.") 
	(delete-backward-char 1))
      (when (looking-back "->") 
	(delete-backward-char 2))
      (insert " ")
      (insert (car candidate)))))

(defun ca-semantic-show-doc (tag)
  (let ((doc (semantic-documentation-for-tag tag)))
    (if (or (null doc) (string= doc ""))
	(message "Doc unavailable for: %s"
		 (semantic-format-tag-uml-prototype tag t))
      (with-output-to-temp-buffer "*TAG DOCUMENTATION*"
	(princ "\n  ")
	(princ (ca-semantic-format-tag tag))
	(princ "\n\n")
	(cond 
	 (doc
	  (setq doc (semantic--format-colorize-text doc 'documentation))
	  (princ doc))
	 (t 
	  (princ "  Documentation unavailable.")))))))

(defun ca-source-semantic-describe (candidate)
  (cond ((and (consp candidate) 
	      (semantic-tag-p (cdr candidate)))
	 (ca-semantic-show-doc (cdr candidate)))
	(t
	 (message "no description"))))

(defun ca-source-semantic-tag-extend (tag)
  (cond ((semantic-tag-p tag)
	 (ca-semantic-format-args tag))
	(t
	 (message "no tag")
	 nil)))

(setq ca-source-semantic-context
  '((decider . ca-source-semantic-context-decider)
    (candidates . ca-source-semantic-context-candidates)
    (info . ca-source-semantic-tag-summary)
    (extend . ca-source-semantic-tag-extend)
    (continue . ca-source-semantic-continue)
    (describe  . ca-source-semantic-describe)
    (sorted . t)
    (action . ca-source-semantic-action)
    (name . "semantic-context")))
  ;; "ca2+ source for semantic context completion")


;; standard semantic tags completion
(defvar ca-source-semantic-tags-analysis nil)

(defun ca-source-semantic-tags-decider ()
  "Construct candidates from the list inside of tags.
   If candidates were found return the starting point of tag"
  (setq ca-source-semantic-context-cntxt nil)  
  (let ((list
	 (condition-case nil
	     (mapcar (lambda (tag)
		       (if (listp tag)
			   (let ((type (semantic-tag-type tag))
				 (class (semantic-tag-class tag))
				 (name (semantic-tag-name tag)))
			     (if (or (and (stringp type)
					  (string= type "class")) ;; compound-p?
				     (eq class 'function)
				     (eq class 'variable))
				 (cons name tag)))))
		     (semantic-fetch-tags)))))
    (when list
      (setq ca-source-semantic-tags-analysis (delq nil list))
      (or (car-safe (bounds-of-thing-at-point 'symbol))
	  (point)))))

(defun ca-source-semantic-tags-candidates (prefix)
  ca-source-semantic-tags-analysis)

(defun ca-source-semantic-tag-summary (tag)
  (if (semantic-tag-p tag)
      (if t
	  (ca-source-semantic-summary-and-doc tag)
	(semantic-format-tag-summarize-with-file tag nil t))))


(defun ca-semantic-format-args (tag &optional parent color)
  "Return a UML style prototype for TAG.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors."
  (let* ((class (semantic-tag-class tag))
	 (type (semantic--format-tag-uml-type tag color))
	 (argtext
	  (cond ((eq class 'function)
		 (concat
		  " ("
		  (semantic--format-tag-arguments
		   (semantic-tag-function-arguments tag)
		   #'semantic-format-tag-prototype
		   color)
		  ")"))
		((eq class 'type)
		 "{}")))
	 )
    (concat argtext type)
    ))

(defun ca-semantic-format-tag (tag &optional parent color)
  "Return a UML style prototype for TAG.
Optional argument PARENT is the parent type if TAG is a detail.
Optional argument COLOR means highlight the prototype with font-lock colors."
  (let* ((class (semantic-tag-class tag))
	 (cp (semantic-format-tag-name tag parent color))
	 (type (semantic--format-tag-uml-type tag color))
	 (prot (semantic-format-tag-uml-protection tag parent color))
	 (argtext
	  (cond ((eq class 'function)
		 (concat
		  " ("
		  (semantic--format-tag-arguments
		   (semantic-tag-function-arguments tag)
		   #'semantic-format-tag-prototype
		   color)
		  ")"))
		((eq class 'type)
		 "{}")))
	 (text nil))
    (setq text (concat prot cp argtext type))
    (if color
	(setq text (semantic--format-uml-post-colorize text tag parent)))
    text
    ))

;; copied from company-semantic.el
(defun ca-source-semantic-summary-and-doc (tag)
  ;; (setq ezimage-use-images nil)
  (let* (
	 (semantic-format-use-images-flag nil)
	 ;;(name (semantic-tag-name tag))
	 ;;(tag (semantic-analyze-find-tag name))
	 (doc (ignore-errors (semantic-documentation-for-tag tag)))
	 ;; (summary (semantic-format-tag-summarize-with-file tag nil t)))
	 (summary (ca-semantic-format-tag tag nil t)))
    (and (stringp doc)
         (string-match "\n*\\(.*\\)$" doc)
         (setq doc (match-string 1 doc)))
    (concat summary
            (when doc
	      (if (< (+ (length doc) (length summary) 4) (frame-width))
		  " -- "
		"\n"))
            doc)))

;; copied from company-semantic.el
(defun company-doc-buffer (&optional string)
  (with-current-buffer (get-buffer-create "*Company meta-data*")
    (erase-buffer)
    (current-buffer)))

(defun ca-source-semantic-doc-buffer (tag)
  (let ((doc (semantic-documentation-for-tag tag)))
    (when doc
      (with-curent-buffer (company-doc-buffer)
        (insert (funcall semantic-idle-summary-function tag nil t)
                "\n"
                doc)
        (current-buffer)))))


(defvar ca-source-semantic-tags
  '((decider . ca-source-semantic-tags-decider)
    (candidates . ca-source-semantic-tags-candidates)
    (continue . ca-source-semantic-continue)
    (limit . 1)
    (info . ca-source-semantic-tag-summary)
    (name . "semantic-tags"))
  "ca2+ semantic source for tag completion")

(provide 'ca2+source-semantic)


;; (defun company-ca-semantic (command &optional arg &rest ignored)
;;   "A `company-mode' completion back-end using CEDET Semantic."
;;   (case command
;;     ('prefix (and (memq major-mode '(c-mode c++-mode jde-mode java-mode))
;;                   (not (company-in-string-or-comment))
;;                   (ca-source-semantic-context-decider)))
;;     ('candidates (or (ca-source-semantic-context-candidates arg)
;;                      (mapcar 'semantic-tag-name
;;                              (semantic-analyze-find-tags-by-prefix arg))))
;;     ('meta (funcall company-semantic-metadata-function
;;                     (semantic-analyze-find-tag arg)))
;;     ('doc-buffer (company-semantic-doc-buffer (semantic-analyze-find-tag arg)))
;;     ;; because "" is an empty context and doesn't return local variables
;;     ('no-cache (equal arg ""))
;;     ('location (let ((tag (semantic-analyze-find-tag arg)))
;;                  (when (buffer-live-p (semantic-tag-buffer tag))
;;                    (cons (semantic-tag-buffer tag)
;;                          (semantic-tag-start tag)))))))


