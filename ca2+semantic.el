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

(defun clear-cache ()
  (interactive)
  (setq semantic-analyze-cache-tags nil)
  (setq semantic-analyze-cache-funcs/vars nil)
  (setq semantic-analyze-cache-mtype-alist nil))


(defvar semantic-analyze-possible-completions-from-cache nil)
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

    (setq debug t)
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
	    (message "use cached tags")
	    (setq c semantic-analyze-cache-tags))
	(message "fetch tags for %s" completetext)
;;	(setq c (semantic-analyze-find-tags-by-prefix completetext)
	(setq c (semantic-fetch-tags))
	;; we fetched all global tags 
	(if (zerop (length completetext))
	    (setq semantic-analyze-cache-tags c)))
 
      (if completetexttype
	  (setq localc (semantic-tag-type-members completetexttype))
	(setq localc 
	      (nconc
	       ;; Argument list and local variables
	       (semantic-find-tags-for-completion completetext localvar)
	       ;; The current scope
	       (semantic-find-tags-for-completion completetext (oref scope fullscope)))))

      (if completetexttype
	  (setq c (append (semantic-find-tags-for-completion 
			   completetext
			   (semantic-analyze-scoped-type-parts 
			    completetexttype scope))
			  c)))

      (if (and (not semantic-analyze-cache-mtype-alist)
	       (zerop (length completetext)))
	  (setq c (append c localc))	  
	(setq use-cache t)
	(message "use mtypes cache !!!")
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
	  (message "test %s" (car origc))
	  (cond
	   ;; Strip operators
	   ((semantic-tag-get-attribute (car origc) :operator-flag)
	    nil)
	 
	   ;; If we are completing from within some prefix,
	   ;; then we want to exclude constructors and destructors
	   ((and completetexttype
		 (or (semantic-tag-get-attribute (car origc) :constructor-flag)
		     (semantic-tag-get-attribute (car origc) :destructor-flag)))
	    nil)

	   ;; If there is a desired type, we need a pair of restrictions
	   (desired-type
	    (let* ((tag (car origc))
		   (members (semantic-tag-type-members tag))
		   (ttype (semantic-tag-type tag))
		   (tname (semantic-tag-name tag))
		   (typedef (car-safe (semantic-tag-get-attribute tag :typedef)))
		   (dtype (car desired-type))
		   found mtype mtypes)
	      (cond 
	       (typedef
		(let* ((m (assoc typedef mtypes-alist)))
		  (unless m 
		    (setq m (cons typedef nil))
		    (setq mtypes-alist (cons m mtypes-alist)))
		  (setcdr m (cons tname (cdr m)))))

	       (members
		(dolist (member members)
		  (let ((mtype (semantic-tag-type member)))

		    (if (listp mtype)
			(setq mtype (car mtype)))
		    (let ((m (assoc mtype mtypes-alist)))
		      (unless m
			(setq m (cons mtype nil))
			(setq mtypes-alist (cons m mtypes-alist)))
		      (setcdr m (cons (caar origc) (cdr m)))))))

	       (t ;;(not (semantic-tag-variable-constant-p tag))
		(if (listp ttype)
		    (setq ttype (car ttype)))
	        (let ((m (assoc ttype func/var-alist)))
	          (unless m
		    (setq m (cons ttype nil))
		    (setq func/var-alist (cons m func/var-alist)))
		    (setcdr m (cons tag (cdr m))))))))

	   (t ;; XXX still needed?
	    ;; No desired type, no other restrictions.  Just add.
	    (setq c (cons (car origc) c)))) ;; cond

	  (setq origc (cdr origc))) ;; while
	) ;;let (origc c)

      (unless use-cache
	(message "add mtypes to cache!!!")
	(setq semantic-analyze-cache-mtype-alist mtypes-alist)
	(setq semantic-analyze-cache-funcs/vars func/var-alist))
   
      (when (or completetexttype 
		(zerop (length completetext)))
	(setq c localc))
      
      (setq c (ca-semantic-completions completetexttype 
				       desired-type 
				       desired-class
				       mtypes-alist
				       func/var-alist
				       c))
      ;; (when desired-class
      ;; 	(setq c (semantic-analyze-tags-of-class-list c desired-class)))

      ;; Pull out trash.
      ;; NOTE TO SELF: Is this too slow?
      ;; OTHER NOTE: Do we not want to strip duplicates by name and
      ;; only by position?  When are duplicate by name but not by tag
      ;; useful?
      (setq c (semantic-unique-tag-table-by-name c))

      ;; All done!
      c)))



(defun ca-semantic-completions (completetexttype 
				desired-type 
				desired-class
				mtypes-alist
				func/var-alist
				c)
  (let ((accept desired-type))
    (setq debug nil)
    (when (or desired-type completetexttype)
      (let ((tmp accept))
	(while tmp
	  (let* ((mtype (car tmp))
		 (bla (assoc mtype mtypes-alist)))
	    (dolist (blub (cdr bla))
	      (when (not (member blub accept))
		(setq tmp (append tmp (list blub)))
		(setq accept (cons blub accept)))))
	  (setq tmp (cdr tmp)))))

    (when (and desired-type (not completetexttype))
      ;; add all functions and variables that have a type from which 
      ;; desired-type is reachable
      (setq c nil)
      (dolist (cand accept)
	(let ((funcs/vars (assoc cand func/var-alist)))
	  (dolist (blub (cdr-safe funcs/vars)) ;; safe?
	    (when blub
	      (setq c (cons  blub c)))))))

   
    (when completetexttype 
      (setq tmp c)
      (setq c nil)
      (let ((members (semantic-tag-type-members completetexttype)))
	(when members
	  (dolist (member members) 
	    (if (memql member tmp)
		(setq c (cons member c))
	      ;;(setq c (append (list member) c))
	      )))))
    c))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; semantic-analyze-fcn.el
;;; Tag Finding 
;;
;; Mechanism for lookup up tags by name.
;;
(defun semantic-analyze-find-tags-by-prefix (prefix)
  ;; @todo - only used in semantic-complete.  Find something better?
  "Attempt to find a tag with PREFIX.
This is a wrapper on top of semanticdb, and semantic search functions.
Almost all searches use the same arguments."
  (if (and (fboundp 'semanticdb-minor-mode-p)
           (semanticdb-minor-mode-p))
      ;; Search the database & concatenate all matches together.
      (semanticdb-strip-find-results
       (semanticdb-find-tags-for-completion prefix)
       'name)
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
  	(nexttype (semantic-analyze-dereference-metatype type scope type-declaration))
  	(idx 0))
    (while (and nexttype (not (eq (car nexttype) lasttype)) (< idx 10))
      (setq lasttype (car nexttype) 
            lasttypedeclaration (cadr nexttype))
      (setq nexttype (semantic-analyze-dereference-metatype lasttype scope lasttypedeclaration))
      (setq idx (1+ idx))
      (when (> idx 20) (error "Possible metatype recursion for %S"
  			      (semantic-tag-name lasttype)))
      )
    lasttype))






(defun ca-semantic-completion-build-cache ()
  (let ((tags (semantic-analyze-find-tags-by-prefix "")))
    (setq semantic-analyze-cache-tags tags)
    (dolist (tag tags)
      (let* ((members (semantic-tag-type-members tag))
	     (ttype (semantic-tag-type tag))
	     (tname (semantic-tag-name tag))
	     (typedef (car-safe (plist-get (caddr tag) :typedef)))
	     (dtype (car desired-type))
	     found mtype mtypes)
	(cond 
	 (typedef
	  (let* ((m (assoc typedef typedef-alist)))
	    (unless m 
	      (setq m (cons typedef nil))
	      (setq typedef-alist (cons m typedef-alist)))
	    (setcdr m (cons tname (cdr m)))))

	 (members
	  ;;(message "members %s" (listp ttype))
	  (dolist (member members)
	    (let ((mtype (semantic-tag-type member)))

	      (if (listp mtype)
		  (setq mtype (car mtype)))
	      (let ((m (assoc mtype mtypes-alist)))
		(unless m
		  (setq m (cons mtype nil))
		  (setq mtypes-alist (cons m mtypes-alist)))
		(setcdr m (cons (car tag) (cdr m)))))))

	 (t ;;(not (semantic-tag-variable-constant-p tag))
	  (if (listp ttype)
	      (setq ttype (car ttype)))
	  (let ((m (assoc ttype func/var-alist)))
	    (unless m
	      (setq m (cons ttype nil))
	      (setq func/var-alist (cons m func/var-alist)))
	    (setcdr m (cons tag (cdr m))))))))
    ))
