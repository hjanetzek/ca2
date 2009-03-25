
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

;; this code is based on semantic-analyze-complete


;;;###autoload
(define-overloadable-function semantic-analyze-possible-completions (context)
  "Return a list of semantic tags which are possible completions.
CONTEXT is either a position (such as point), or a precalculated
context.  Passing in a context is useful if the caller also needs
to access parts of the analysis.
Completions run through the following filters:
  * Elements currently in scope
  * Constants currently in scope
  * Elements match the :prefix in the CONTEXT.
  * Type of the completion matches the type of the context.
Context type matching can identify the following:
  * No specific type
  * Assignment into a variable of some type.
  * Argument to a function with type constraints.
When called interactively, displays the list of possible completions
in a buffer."
  (interactive "d")
  ;; In theory, we don't need the below since the context will
  ;; do it for us.
  ;;(semantic-refresh-tags-safe)
  (with-syntax-table semantic-lex-syntax-table
    (let* ((context 
	    (if (semantic-analyze-context-child-p context)
	       context
	    (semantic-analyze-current-context context))
	    )
	   (ans (if (not context)
		    (error "Nothing to Complete.")
		  (:override))))
      ;; If interactive, display them.
      (when (interactive-p)
	(with-output-to-temp-buffer "*Possible Completions*"
	  (semantic-analyze-princ-sequence ans "" (current-buffer)))
	(shrink-window-if-larger-than-buffer
	 (get-buffer-window "*Possible Completions*")))
      ans)))

(defun ca-semantic-clear-cache ()
  (interactive)
  (message "Clear Semantic Cache")
  (setq semantic-analyze-cache-tags nil)
  (setq semantic-analyze-cache-funcs/vars nil)
  (setq semantic-analyze-cache-mtype-alist nil))

(defsubst ca-semantic-strip-type (type)
  (if (listp type)
      (car type)
    type))

(defvar semantic-analyze-cache-funcs/vars nil)
(defvar semantic-analyze-cache-mtype-alist nil)
(defvar semantic-analyze-cache-tags nil)

(defun semantic-analyze-possible-completions-default (context)
  "Default method for producing smart completions.
Argument CONTEXT is an object specifying the locally derived context."
  (let* ((a context)
	 (desired-type (semantic-analyze-type-constraint a))
	 (desired-class (oref a prefixclass))
	 (prefix (oref a prefix))
	 (prefixtypes (oref a prefixtypes))
	 (completetext nil)
	 (completetexttype nil)
	 (scope (oref a scope))
	 (localvar (oref scope localvar))
	 (localc)
	 (c nil)
	 (can-complete t)
	 (func/var-alist nil)
	 (mtypes-alist)
	 (use-cache nil))

    ;;(message "semantic-analyze-possible-completions-default")
    
    ;; XXX is this the right way?
    (set (make-local-variable 'semantic-analyze-cache-tags) 
	 semantic-analyze-cache-tags)
    (set (make-local-variable 'semantic-analyze-cache-mtype-alist)
	 semantic-analyze-cache-mtype-alist)
    (set (make-local-variable 'semantic-analyze-cache-funcs/vars)
	 semantic-analyze-cache-funcs/vars)

    (setq debug nil)
    ;; Calculate what our prefix string is so that we can
    ;; find all our matching text.
    (setq completetext (car (reverse prefix)))
    (if (semantic-tag-p completetext)
	(setq completetext (semantic-tag-name completetext)))

    (when (and (not completetext) 
	       (not desired-type))
      (setq can-complete nil)
      (message "Nothing to complete"))

    (if (not completetext) (setq completetext ""))

    ;; This better be a reasonable type, or we should fry it.
    ;; The prefixtypes should always be at least 1 less than
    ;; the prefix since the type is never looked up for the last
    ;; item when calculating a sequence.
    (setq completetexttype (car (reverse prefixtypes)))
    ;; (message "completetexttype %s" completetexttype)
    (when (or (not completetexttype)
     	      (not (and (semantic-tag-p completetexttype)
    			(or (semantic-tag-prototype-p completetexttype)
     			(eq (semantic-tag-class completetexttype) 'type)))))

      ;; ;; What should I do here?  I think this is an error condition.
      ;; (setq completetexttype nil)
      ;; ;; If we had something that was a completetexttype but it wasn't
      ;; ;; valid, then express our dismay!
      (when (> (length prefix) 1)
      	(let* ((errprefix (car (cdr (reverse prefix)))))
      	  (setq can-complete nil)
      	  (message "Cannot find types for `%s'"
      		   (cond ((semantic-tag-p errprefix)
      			  (semantic-format-tag-prototype errprefix))
      			 (t
      			  (format "%S" errprefix)))))))

    (when can-complete
      (if semantic-analyze-cache-tags
	  (progn 
	    (when debug (message "use cached tags"))
	    (setq c semantic-analyze-cache-tags))

	(when debug (message "fetch tags"))

	(setq c (let ((cands nil))
		  (mapc '(lambda (tag)
			   (unless (or (semantic-tag-get-attribute tag :faux)
				       (semantic-tag-of-class-p tag 'include))
			     (push tag cands)))
			(semanticdb-fast-strip-find-results 
			 (semanticdb-find-tags-for-completion "")))
		  (semantic-unique-tag-table-by-name cands)))

	(unless c
	  (setq c (semantic-analyze-find-tags-by-prefix "")))
	(setq semantic-analyze-cache-tags c))

      (if completetexttype
	  ;; set local vars to be members of type to complete
	  (setq localc (semantic-tag-type-members completetexttype))
	(setq localc 
	      (nconc
	       ;; Argument list and local variables
	       (semantic-find-tags-for-completion completetext localvar)
	       ;; The current scope
	       (semantic-find-tags-for-completion completetext (oref scope fullscope)))))

      ;; (if completetexttype
      ;; 	  (setq c (append (semantic-analyze-scoped-type-parts 
      ;; 			   completetexttype (oref 
      ;; 					     scope 
      ;; 					     fullscope))
      ;; 			  c)))

      (if (and (not semantic-analyze-cache-mtype-alist)
	       (zerop (length completetext)))
	  (setq c (append c localc))	  

	(setq use-cache t)
	(when debug
	  (message "use types cache"))

	(setq mtypes-alist (copy-alist semantic-analyze-cache-mtype-alist))
	(setq func/var-alist (copy-alist semantic-analyze-cache-funcs/vars))
	(setq c localc))

      (when debug
	(message "test tags %d" (length c))
	(message "completetext -%s-" completetext)
	(message "completetexttype -%s-" completetexttype)
	(message "desired-type -%s-" desired-type))

      (let ((origc c))
	;; Reset c.
	(setq c nil)
	;; Loop over all the found matches, and catagorize them
	;; as being possible features.
	(while origc
	  (cond
	   ;; Strip operators.. hm why?
	   ((semantic-tag-get-attribute (car origc) :operator-flag)
	    nil)
	   
	   ;; create cache or update cache with local vars for desired-type
	   ((or desired-type (not use-cache))
	    (let* ((tag (car origc))
		   (members (semantic-tag-type-members tag))
		   (ttype (ca-semantic-strip-type (semantic-tag-type tag)))
		   (tname (semantic-tag-name tag))
		   (class (semantic-tag-class tag))
		   (typedef (car-safe (semantic-tag-get-attribute tag :typedef)))
		   (sup (semantic-tag-get-attribute tag :superclasses))
		   found mtype mtypes)
	       (if sup
		;; TODO (dolist (superclass sup) for multiple ingeritance?
		(let* ((m (assoc sup mtypes-alist)))
		  (unless m 
		    (setq m (cons sup nil))
		    (setq mtypes-alist (cons m mtypes-alist)))
		  (setcdr m (cons tname (cdr m)))))
	       
	       (if members
		(dolist (member members)
		  (let ((mtype (semantic-tag-type member)))
		    (if (listp mtype)
			(setq mtype (car mtype)))
		    (let ((m (assoc mtype mtypes-alist)))
		      (unless m
			(setq m (cons mtype nil))
			(setq mtypes-alist (cons m mtypes-alist)))
		      (setcdr m (cons tname (cdr m)))))))
	       (cond 
	       ((or (semantic-tag-of-class-p tag 'variable)
		    (semantic-tag-of-class-p tag 'function)) 
		(let ((m (assoc ttype func/var-alist)))
	          (unless m
		    (setq m (cons ttype nil))
		    (setq func/var-alist (cons m func/var-alist)))
		    (setcdr m (cons tag (cdr m))))))))
	   (t
	    (setq c (cons (car origc) c))))
	  (setq origc (cdr origc))))

      (unless use-cache
	(setq semantic-analyze-cache-mtype-alist mtypes-alist)
	(setq semantic-analyze-cache-funcs/vars func/var-alist))
  
      (dolist (func/vars func/var-alist)
	(setcdr func/vars (semantic-unique-tag-table-by-name (cdr func/vars))))
 
      ;; XXX check this again
      (when (or completetexttype 
		(zerop (length completetext)))
	(setq c localc))

      (if (or desired-type completetexttype)
	  (setq c (ca-semantic-completions-1 completetexttype desired-type 
					     desired-class mtypes-alist
					     func/var-alist c))
	;; no desired type or type to complete, sort local tags first
	(let ((local nil)
	      (cur-buffer nil)
	      (tags nil))
	  (dolist (tag c)
	    (cond
	     ;; put local tags first
	     ((member tag localc)
	      (setq local (cons tag local)))
	     ;; one way to figure out if tag is from current buffer. 
	     ;; just a guess..
	     ((not (arrayp (car (reverse tag))))
	      (setq cur-buffer (cons tag cur-buffer)))		 
	     ;; matches desired type
	     (t
	      (setq tags (cons tag tags)))))
	  (setq c (append local cur-buffer tags))))

      ;; TODO get some cpp to see what this does
      ;; (when desired-class
      ;; 	(setq c (semantic-analyze-tags-of-class-list c desired-class)))
      c)))


(defun ca-semantic-completions (completetexttype desired-type)
  ;;(message "ca-semantic-completions \n%s\n%s" completetexttype desired-type)
  (ca-semantic-completions-1 completetexttype desired-type 
			     nil ;;desired-class
			     semantic-analyze-cache-mtype-alist
			     semantic-analyze-cache-funcs/vars
			     nil))

(defun ca-semantic-completions-1 (completetexttype desired-type 
				  desired-class mtypes-alist
				  func/var-alist local-tags)
  (let* ((accept (list (ca-semantic-strip-type desired-type)))
	 (tmp accept)
	 (tags nil))
    (when (or (and desired-type (listp desired-type))
	      completetexttype)
      (while tmp
	(let* ((mtype (car tmp))
	       (bla (assoc mtype mtypes-alist)))
	  (unless (null mtype)
	    (dolist (compound (cdr bla))
	      (when (and compound (not (member compound accept)))
		(setq tmp (append tmp (list compound)))
		(setq accept (cons compound accept))))))
	(setq tmp (cdr tmp))))

    (setq accept (delete nil accept))
    
    (when (and (not completetexttype) desired-type)
      ;; check wheter we complete an enum type
      (let (tag members global local cur-buffer)
	(when (listp desired-type)
	  (setq tag (semantic-analyze-find-tag (semantic-tag-name desired-type)))
	  ;; FIXME look further than one typedef
	  (when (semantic-tag-get-attribute tag :typedef)
	    (setq tag (semantic-tag-get-attribute tag :typedef)))
	  (when (and (equal (semantic-tag-type tag) "enum")
		     (setq members (semantic-tag-type-members tag)))
	    (setq local members)))
	;; add all functions and variables that have a type from which 
	;; desired-type is reachable
	(dolist (cand accept)
	  (let ((funcs/vars (assoc cand func/var-alist)))
	    (dolist (tag (cdr-safe funcs/vars)) ;; safe?
	      (when tag
		(cond 
		 ;; put local tags first
		 ((member tag local-tags)
		  ;; (message "local %s" tag)
		  (setq local (cons tag local)))
		 ;; one way to figure out if tag is from current buffer. just a guess..
		 ((not (arrayp (car (reverse tag))))
		  ;; (message "buffer %s" tag)
		  (setq cur-buffer (cons tag cur-buffer)))		 
		 ;; matches desired type XXX first ????
		 ((let ((ttype (ca-semantic-strip-type (semantic-tag-type tag))))
		    (when (equal ttype (ca-semantic-strip-type desired-type))
		      ;; (message "match %s" tag)
		      (setq tags (cons tag tags)))))
		 (t ;; XXX used?
		  ;; (message "other %s" tag)
		  (setq global (cons tag global))))))))

	(setq tags (append local cur-buffer tags global))))
    
    (when completetexttype
      (message "completetexttype" )
      (setq tmp tags)
      (setq tags nil)
      (let ((members (semantic-tag-type-members completetexttype))
	    reachable unreachable)
	(when members
	  (dolist (member members)
	    (if (and member 
		     (or (semantic-tag-of-class-p member 'variable)
			 (semantic-tag-of-class-p member 'function))
		     (not (semantic-tag-get-attribute member :constructor-flag))
		     (not (semantic-tag-get-attribute member :destructor-flag)))
		(cond 
	     ;; matches desired type
	     ((let ((ttype (ca-semantic-strip-type (semantic-tag-type member))))
	       (when (equal ttype (car desired-type))
		 ;; (message "desired %s" member)
		 (setq tags (cons member tags)))))
	     ;; desired type is reachable
	     ((memql member tmp)
	      ;; (message "reach %s" member)
	      (setq reachable (cons member reachable)))
	     ;; desired type is not reachable
	     (t
	      ;; (message "unreach %s" member)
	      (setq unreachable (cons member unreachable))))))

	  (setq tags (append tags reachable unreachable)))))
    tags))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; semantic-analyze-fcn.el
;;; Tag Finding 
;;
;; Mechanism for lookup up tags by name.
;;
(defun semantic-analyze-find-tags-by-prefix (prefix)
  ;; @todo - only used in semantic-complete.  Find something better?
  "Attempt to find a tag with PREFIX.
is is a wrapper on top of semanticdb, and semantic search functions.
Almost all searches use the same arguments."
  (if (and (fboundp 'semanticdb-minor-mode-p)
           (semanticdb-minor-mode-p))
      ;; Search the database & concatenate all matches together.
      ;;(semanticdb-strip-find-results
       (semanticdb-fast-strip-find-results
       (semanticdb-find-tags-for-completion prefix))
    ;;'name)
    ;; Search just this file because there is no DB available.
    (semantic-find-tags-for-completion
     prefix (current-buffer))))

;; do not throw errors
(defun semantic-analyze-dereference-metatype-stack (type scope &optional type-declaration)
  "Dereference metatypes repeatedly until we hit a real TYPE.
Uses `semantic-analyze-dereference-metatype'.
Argument SCOPE is the scope object with additional items in which to search."
  (let ((lasttype type)
        (lasttypedeclaration type-declaration)
  	(nexttype (semantic-analyze-dereference-metatype 
		   type scope type-declaration))
  	(idx 0))
    (while (and nexttype (not (eq (car nexttype) lasttype)) (< idx 10))
      (setq lasttype (car nexttype) 
            lasttypedeclaration (cadr nexttype))
      (setq nexttype (semantic-analyze-dereference-metatype 
		      lasttype scope lasttypedeclaration))
      (setq idx (1+ idx))
      (when (> idx 20) (error "Possible metatype recursion for %S"
  			      (semantic-tag-name lasttype))))
    lasttype))


(provide 'ca2+semantic)