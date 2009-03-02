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

    ;; filter out local vars that were defined after point
    (dolist (ptag tmp)
      (if (< (aref (car (reverse ptag)) 1) p)
	  (push ptag localtags)))

    (when localtags
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
	(if (and (semantic-tag-get-attribute tag :pointer)
		 (not (semantic-tag-get-attribute ptag :pointer)))
	    (concat "&" (semantic-tag-name ptag))
	  (semantic-tag-name ptag))))))


;; modified from semantic-format.el, creates yas-template
(defun ca-source-semantic-format-tag-arguments (args formatter)
  "Format the argument list ARGS with FORMATTER.
FORMATTER is a function used to format a tag.
COLOR specifies if color should be used."
  (let ((out nil)
	(cnt 1))
    (while args
      (unless (equal (semantic-tag-name (car args)) "")
	(push 
	 (concat "${" (number-to-string cnt) ":"
		 (or
		  ;; try find matchin candidate
		  (and (semantic-tag-p (car args))
		       (ca-source-semantic-find-local-tag (car args)))
		  ;; try tag formatter
		  (and formatter
		       (semantic-tag-p (car args))
		       (not (string= (semantic-tag-name (car args)) ""))
		       (funcall formatter (car args) nil nil))
		  ;; fallback
		  (semantic-format-tag-name-from-anything
		   (car args) nil nil 'variable))
		 "}")
	 out))
      (setq cnt (1+ cnt))
      (setq args (cdr args)))
    ;;semantic-function-argument-separator
    (mapconcat 'identity (nreverse out) ", ")))


(defun ca-source-semantic-continue-function (tag)
  (yas/expand-snippet 
   (point) (point) 
   (concat 
    "(" (ca-source-semantic-format-tag-arguments
	 (semantic-tag-function-arguments tag)
	 #'semantic-format-tag-prototype) ")"
	 (unless(looking-at "[[:space:]]*\\()\\|,\\)") ";")))
  ;; finish ca2+
  nil)


(defun ca-source-semantic-continue-variable (tag)
  (let* ((ttype (semantic-tag-type tag))
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
      (when (boundp 'semantic-analyze-cache-tags)
	(let* ((a (or
		   ;; reuse context
		   ca-source-semantic-context-cntxt
		   ;; get current context
		   (semantic-analyze-current-context (point))))
	       ;; type that is expected in current context
	       (desired-type (semantic-analyze-type-constraint a))
	       ;; members of candidate sorted by reachability
	       (members (if (not semantic-analyze-cache-tags)
			    (semantic-analyze-possible-completions a)
			  (ca-semantic-completions type desired-type))))))

      (if (semantic-tag-get-attribute tag :pointer)
	  (insert "->") (insert "."))
      
      (mapcar '(lambda(tag) (cons (semantic-tag-name tag) tag)) 
	      members))))

(defun ca-source-semantic-continue (candidate)
  (let ((tag (cdr-safe candidate)))
    (when (semantic-tag-p tag)
      (cond 
       ((eq (semantic-tag-class tag) 'function)
	(ca-source-semantic-continue-function tag))
       ((eq (semantic-tag-class tag) 'variable)
	(ca-source-semantic-continue-variable tag))))))



;; semantic context completion
(defvar ca-source-semantic-context-completions nil)
(defvar ca-source-semantic-context-cntxt nil)

(defun ca-source-semantic-context-decider-1 ()
  (let* ((p (point))
	 (a (semantic-analyze-current-context p))
	 ;; XXX make this an option 
	 ;; (syms (if a (semantic-ia-get-completions a p)))
	 (syms (if a (semantic-analyze-possible-completions a)))
	 (completions (mapcar 
		       '(lambda(tag) (cons (semantic-tag-name tag) tag))
		       syms)))
    (when completions
      (setq ca-source-semantic-context-cntxt a)
      (setq ca-source-semantic-context-completions completions)
      (or (car-safe (bounds-of-thing-at-point 'symbol))
	  p))))

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

(defvar ca-source-semantic-context
  '((decider . ca-source-semantic-context-decider)
    (candidates . ca-source-semantic-context-candidates)
    (info . ca-source-semantic-tag-summary)
    (continue . ca-source-semantic-continue)
    (sorted . t)
    (name . "semantic-context"))
  "ca2+ source for semantic context completion")



;; semanticdb tags source - seems to be fastest
(defun ca-source-semanticdb-tags-candidates (prefix)
  (setq ca-source-semantic-context-cntxt nil)
  (let ((cands nil))
    (mapc '(lambda (tag)
	     (unless (or (semantic-tag-get-attribute tag :faux)
			 (semantic-tag-of-class-p tag 'include))
	       (push (cons (semantic-tag-name tag) tag) cands)))
	  (semanticdb-fast-strip-find-results 
	   (semanticdb-find-tags-for-completion prefix)))
    cands))

(defvar ca-source-semanticdb-tags
  '((candidates . ca-source-semanticdb-tags-candidates)
    (continue . ca-source-semantic-continue)
    (limit . 1)
    (info . ca-source-semantic-tag-summary)
    (name . "semanticdb-tags"))
  "ca2+ semanticdb source for tag completion")



;; standard semantic tags completion
(defvar ca-source-semantic-tags-analysis nil)

(defun ca-source-semantic-tags-decider ()
  "Construct candidates from the list inside of tags.
   If candidates were found return the starting point of tag"
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

(defun ca-source-semantic-tag-summary (candidate)
  (semantic-format-tag-summarize-with-file candidate nil t))

(defvar ca-source-semantic-tags
  '((decider . ca-source-semantic-tags-decider)
    (candidates . ca-source-semantic-tags-candidates)
    (continue . ca-source-semantic-continue)
    (limit . 1)
    (info . ca-source-semantic-tag-summary)
    (name . "semantic-tags"))
  "ca2+ semantic source for tag completion")



;; find candidates with dabbrev but do semantic source action
;; if candidate is a tag name. 
(defun ca-source-semantic-dabbrev-continue (candidate)
  (let ((cands (ca-source-semanticdb-tags-candidates candidate)))
    (if cands
	(ca-source-semantic-continue (car-safe cands)))))

(defvar ca-source-semantic-with-dabbrev
  '((candidates . ca-source-dabbrev-candidates)
    (continue   . ca-source-semantic-dabbrev-continue)
    (limit      . 1)
    (sorted     . t)
    (name       . "dabbrev"))
  "ca2+ dabbrev source")


(provide 'ca2+source-semantic)