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
    (let* ((context (if (semantic-analyze-context-child-p context)
                        context
                      (semantic-analyze-current-context context)))
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
	 (c nil))

    ;; Calculate what our prefix string is so that we can
    ;; find all our matching text.
    (setq completetext (car (reverse prefix)))
    (if (semantic-tag-p completetext)
	(setq completetext (semantic-tag-name completetext)))

    (if (and (not completetext) (not desired-type))
	(error "Nothing to complete"))

    (if (not completetext) (setq completetext ""))

    ;; This better be a reasonable type, or we should fry it.
    ;; The prefixtypes should always be at least 1 less than
    ;; the prefix since the type is never looked up for the last
    ;; item when calculating a sequence.
    (setq completetexttype (car (reverse prefixtypes)))
    (when (or (not completetexttype)
	      (not (and (semantic-tag-p completetexttype)
			(eq (semantic-tag-class completetexttype) 'type))))

      ;; What should I do here?  I think this is an error condition.
      (setq completetexttype nil)
      ;; If we had something that was a completetexttype but it wasn't
      ;; valid, then express our dismay!
      (when (> (length prefix) 1)
	(let* ((errprefix (car (cdr (reverse prefix)))))
	  (error "Cannot find types for `%s'"
		 (cond ((semantic-tag-p errprefix)
			(semantic-format-tag-prototype errprefix))
		       (t
			(format "%S" errprefix)))))))

    ;; There are many places to get our completion stream for.
    ;; Here we go.
    (if  completetexttype
	(setq c (semantic-find-tags-for-completion
		 completetext
		 (semantic-analyze-scoped-type-parts completetexttype scope)))
      
      ;; No type based on the completetext.  This is a free-range
      ;; var or function.  We need to expand our search beyond this
      ;; scope into semanticdb, etc.
      (setq c (nconc
	       ;; Argument list and local variables
	       (semantic-find-tags-for-completion completetext localvar)
	       ;; The current scope
	       (semantic-find-tags-for-completion completetext (oref scope fullscope))
	       ;; The world
	       (semantic-analyze-find-tags-by-prefix completetext))))

    ;;(message "_______ %d __ %s ___ %s ___________________" 
    ;;(length c) desired-type completetexttype)

    (let ((origc c)
	  (dtname (semantic-tag-name desired-type))
	  accept
	  func/var-alist
	  mtypes-alist)

      ;; Reset c.
      (setq c nil)
      ;; Loop over all the found matches, and catagorize them
      ;; as being possible features.
      (while origc
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
	  ;;	  (message "_____________________________________________\n%s" (car origc))

	  (let* ((tag (car origc))
		 (members (semantic-tag-type-members tag))
		 (ttype (semantic-tag-type tag))
		 (tname (semantic-tag-name tag))
		 (typedef (car-safe (plist-get (caddr tag) :typedef)))
		 (dtype (car desired-type))
		 found mtype mtypes)
	    ;;	    (message "1 %s" ttype)
	    ;;	    (message "2 %s" members)    
	    ;;	    (message "3 %s" typedef)    
	    ;;	    (message "4 %s" tname)    
	    ;;(message "5 %s" dtype)    
	    ;;	    (message "\n")
	    (cond 
	     (typedef
	      ;;	      (message "typedef")
	      (let* ((m (assoc typedef mtypes-alist)))
		(unless m 
		  (setq m (cons typedef nil))
		  (setq mtypes-alist (cons m mtypes-alist)))
		(setcdr m (cons tname (cdr m)))))

	     ((not (or (listp ttype) members))
	      ;;	      (message "simple type")
	      (if (equal ttype dtype)
	          (progn
		    ;; function or variable matches the desired-type
		    (setq c (cons tag c))
		    ;;		    (message "direct match")
		    )
		;;(setq accept (cons (semantic-tag-type (car origc)) accept)))
	        ;; remember return type for function or variable
	        (let ((m (assoc ttype func/var-alist)))
	          (unless m
		    (setq m (cons ttype nil))
		    (setq func/var-alist (cons m func/var-alist)))
	          (setcdr m (cons tag (cdr m))))))

	     (members
	      (while (and (not found) members) ;; -> all items to mtimes list?
	     	(let ((mtype (semantic-tag-type (car members))))
		  ;;	     	  (message "%s - %s" mtype dtype)
	     	  (if (listp mtype)
	     	      (setq mtype (car mtype)))
	     	  (when (equal mtype dtype) 
	     	    ;; compound has desired-type 
	     	    (setq found t)
	     	    ;;(message "2")
	     	    (setq accept (cons (caar origc) accept)))
	     	  (setq mtypes (cons mtype mtypes))
	     	  (setq members (cdr members))))
	      (when (not found)
	     	;; compound has not the desired type
	     	;; keep for next level search
	     	(dolist (mtype mtypes)
	     	  (let ((m (assoc mtype mtypes-alist)))
	     	    (unless m
	     	      (setq m (cons mtype nil))
	     	      (setq mtypes-alist (cons m mtypes-alist)))
	     	    (setcdr m (cons (caar origc) (cdr m)))))))
	     (t
	      ;;(message "simple type2 %s" ttype)
	      (if (listp ttype)
		  (setq ttype (car ttype)))
	      (if (equal ttype dtype)
	          (progn
		    ;; function or variable matches the desired-type
		    (setq c (cons tag c))
		    ;;(message "direct match")
		    )
		;;(setq accept (cons (semantic-tag-type (car origc)) accept)))
	        ;; remember return type for function or variable
	        (let ((m (assoc ttype func/var-alist)))
	          (unless m
		    (setq m (cons ttype nil))
		    (setq func/var-alist (cons m func/var-alist)))
	          (setcdr m (cons tag (cdr m))))))
	     

	     )
	    ))
	 ;; )

	 ;; No desired type, no other restrictions.  Just add.
	 (t
	  ;;	  (message "just add")
	  (setq c (cons (car origc) c)))) ;; cond

	(setq origc (cdr origc))) ;; while

      ;; (dolist (cand c)
      ;; 	(message "all  : %s" cand))
      ;; (dolist (cand accept)
      ;; 	(message "accept: %s" cand))
      ;; (dolist (cand mtypes-alist)
      ;; 	(message "mtypes: %s" cand))
      ;; (dolist (cand func/var-alist)
      ;; 	(message "funcvar: %s" cand))

      (when desired-type
	(let ((tmp accept))
	  (while tmp
	    (let* ((mtype (car tmp))
		   (bla (assoc mtype mtypes-alist)))
	      (dolist (blub (cdr bla))
		(when (not (member blub accept))
		  (setq tmp (append tmp (list blub)))
		  (setq accept (cons blub accept)))))
	    (setq tmp (cdr tmp))))

	;; (dolist (cand accept)
	;;   (message "accept: %s" cand))


	;; add all functions and variables that have a type from which 
	;; desired-type is reachable
	(dolist (cand accept)
	  ;;(message "accept %s"cand )
	  (let ((funcs/vars (assoc cand func/var-alist)))
	    ;;(message "append %s" funcs/vars )	    
	    (dolist (blub (cdr-safe funcs/vars)) ;; safe?
	      (when blub
		(setq c (cons  blub c))))))
	

	;; (dolist (cand c)
	;;   (message "all  : %s" cand))

	)

      (when desired-type
	;; Some types, like the enum in C, have special constant values that
	;; we could complete with.  Thus, if the target is an enum, we can
	;; find possible symbol values to fill in that value.
	(let ((constants
	       (semantic-analyze-type-constants desired-type)))
	  (if constants
	      (progn
		;; Filter
		(setq constants
		      (semantic-find-tags-for-completion
		       completetext constants))
		;; Add to the list
		(setq c (nconc c constants)))
	    )))
      )

    (when desired-class
      (setq c (semantic-analyze-tags-of-class-list c desired-class)))

    ;; Pull out trash.
    ;; NOTE TO SELF: Is this too slow?
    ;; OTHER NOTE: Do we not want to strip duplicates by name and
    ;; only by position?  When are duplicate by name but not by tag
    ;; useful?
    (setq c (semantic-unique-tag-table-by-name c))

    ;; All done!

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