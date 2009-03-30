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
	(if (and (semantic-tag-get-attribute tag :pointer)
		 (not (semantic-tag-get-attribute ptag :pointer)))
	    (concat "&" (semantic-tag-name ptag))
	  (semantic-tag-name ptag))))))


;; modified from semantic-format.el, creates yas-template
(defun ca-source-semantic-format-tag-arguments (args formatter)
  "Format the argument list ARGS with FORMATTER.
FORMATTER is a function used to format a tag.
COLOR specifies if color should be used."

  (let ((template nil)
	(cnt 1))
    (while args
      (push 
       (concat "${" (number-to-string cnt) ":"
	       (or
		;; try find matchin candidate
		(and (semantic-tag-p (car args))
		     (ca-source-semantic-find-local-tag (car args)))
		;; try tag formatter
		(and (semantic-tag-name (car args))
		     (concat "_" (semantic-tag-name (car args)) "_"))
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
    (mapconcat 'identity (nreverse template) ", ")))


(defun ca-source-semantic-continue-function (tag)
  (yas/expand-snippet 
   (point) (point) 
   (concat "(" (ca-source-semantic-format-tag-arguments
		(semantic-tag-function-arguments tag)
		#'semantic-format-tag-prototype) 
	   ")"
	   (unless(looking-at "[[:space:]]*\\()\\|,\\)") ";")))
  ;; finish ca2+
  nil)

(defun ca-source-semantic-continue-operator (tag)
  
  (let ((args (semantic-tag-function-arguments tag))
	desired-type)
    (when args
      (setq desired-type (semantic-tag-type (car args)))
      (message "desired-type %s" desired-type)
      (ca-semantic-completions nil desired-type))))


(defun ca-source-semantic-continue-variable (tag)
  (let* ((ttype (semantic-tag-type tag))
	 (refsign "")
	 type members tmp)


    (while (and ttype (listp ttype) (not (semantic-tag-type-members ttype)))
      ;; find parent of variable
      (setq type (semantic-analyze-find-tag (car ttype)))

      (or (setq ttype (semantic-tag-get-attribute type :typedef))
	  (setq ttype (semantic-tag-get-attribute type :class))))

    (when (and ttype (listp ttype))
      (setq type ttype))

    (unless (equal (semantic-tag-type type) "enum")
      ;; XXX hack, somehow semantic looses all but first 
      ;; member after when completing
      ;; (semantic-clear-toplevel-cache)
      ;; (senator-parse)
      (setq tmp (copy-list (semantic-tag-type-members type))))

    (when tmp
      ;;sort members with ca2+semantic
      (when (boundp 'ca-semantic-analyze-cache-tags)
    	(let* ((a (or
    		   ;; reuse context as desired-type stays the 
		   ;; same 
    		   ca-source-semantic-context-cntxt
    		   ;; get current context
    		   (semantic-analyze-current-context (point))))
    	       ;; type that is expected in current context
    	       (desired-type (semantic-analyze-type-constraint a))
    	       ;; members of candidate sorted by reachability
    	       (tmp (if (not ca-semantic-analyze-cache-tags)
    			    (ca-semantic-analyze-possible-completions a)
    			  (ca-semantic-completions type desired-type))))))
    
      ;; remove public/private labels
      (dolist (member (nreverse tmp))
	(when (and (semantic-tag-type member) ;; filter labels: have no type
		   (not (semantic-tag-get-attribute member :constructor-flag))
		   (not (semantic-tag-get-attribute member :destructor-flag)))
	  (push member members)))
  
      (setq refsign (if (semantic-tag-get-attribute tag :pointer) "->" "."))
      (insert refsign)

      (mapcar '(lambda(tag) (cons (semantic-tag-name tag) tag)) members))))

(defun ca-source-semantic-continue (candidate)
  (let ((tag (cdr-safe candidate))
	(cands nil))
    (when (semantic-tag-p tag)
      (setq ca-prefix "")
      (setq ca-initial-prefix "")

      (cond 
       ((semantic-tag-get-attribute tag :operator-flag)
	;; do not complete operators for now 
	(setq cands nil)) ;;(ca-source-semantic-continue-operator tag)))
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
	 ;; (syms (if a (semantic-ia-get-completions a p)))
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

(defvar ca-source-semantic-context
  '((decider . ca-source-semantic-context-decider)
    (candidates . ca-source-semantic-context-candidates)
    (info . ca-source-semantic-tag-summary)
    (continue . ca-source-semantic-continue)
    (sorted . t)
    (name . "semantic-context"))
  "ca2+ source for semantic context completion")




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

(defun ca-source-semantic-tag-summary (candidate)
  (if (semantic-tag-p candidate)
      (semantic-format-tag-summarize-with-file candidate nil t)))

(defvar ca-source-semantic-tags
  '((decider . ca-source-semantic-tags-decider)
    (candidates . ca-source-semantic-tags-candidates)
    (continue . ca-source-semantic-continue)
    (limit . 1)
    (info . ca-source-semantic-tag-summary)
    (name . "semantic-tags"))
  "ca2+ semantic source for tag completion")





(provide 'ca2+source-semantic)