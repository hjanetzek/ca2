
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

;; Author: Hannes Janetzek 


;;;###autoload
(defun ca-semantic-analyze-possible-completions (context)
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
	    (semantic-analyze-current-context context)))
	   (ans (if (not context)
		    (error "Nothing to Complete.")
		  (ca-semantic-analyze-possible-completions-default context))))
      ans)))

(defun ca-semantic-clear-cache (&optional taglist)
  (interactive)
  (message "Clear Semantic Cache")
  (setq ca-semantic-analyze-cache-tags nil)
  (setq ca-semantic-analyze-cache-funcs/vars nil)
  (setq ca-semantic-analyze-cache-mtype-alist nil))

(defsubst ca-semantic-strip-type (type)
  (if (listp type)
      (car type)
    type))

(defvar ca-semantic-analyze-cache-funcs/vars nil)
(make-variable-buffer-local 'ca-semantic-analyze-cache-funcs/vars)
(defvar ca-semantic-analyze-cache-mtype-alist nil)
(make-variable-buffer-local 'ca-semantic-analyze-cache-mtype-alist)
(defvar ca-semantic-analyze-cache-tags nil)
(make-variable-buffer-local 'ca-semantic-analyze-cache-tags)



(defun ca-semantic-analyze-possible-completions-default (context)
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
	 (can-complete t)
	 (func/var-alist nil)
	 (mtypes-alist)
	 (c ca-semantic-analyze-cache-tags)
	 (use-cache (if c t)))

    (add-hook 'semantic-after-toplevel-cache-change-hook 
	      'ca-semantic-clear-cache t t)

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
      (unless use-cache
	;; get taglist
	(setq c (let ((cands nil))
		  (mapc '(lambda (tag)
			   (unless (or (semantic-tag-get-attribute tag :faux)
				       (semantic-tag-of-class-p tag 'include))
			     (push tag cands)))
			;; (semanticdb-strip-find-results
			;;  (semanticdb-find-tags-for-completion "")
			;;  'name)
			(semanticdb-fast-strip-find-results 
			 (semanticdb-find-tags-for-completion "")))
		  (semantic-unique-tag-table-by-name cands)))
	;; XXX needed?
	(unless c (setq c (semantic-analyze-find-tags-by-prefix "")))
	;; add taglist to cache
	(setq ca-semantic-analyze-cache-tags c))

      (setq localc 
	    (if completetexttype 
		;; set local vars to be members of type to complete
		(semantic-tag-type-members completetexttype)

	      (nconc
	       ;; Argument list and local variables
	       (semantic-find-tags-for-completion completetext localvar)
	       ;; The current scope (e.g. class members)
	       (semantic-find-tags-for-completion completetext 
						  (oref scope fullscope)))))
	    
      (if (or (not use-cache)
	      (not (or completetexttype desired-type)))
	  (setq c (append c localc))

	(setq mtypes-alist   (copy-alist ca-semantic-analyze-cache-mtype-alist))
	(setq func/var-alist (copy-alist ca-semantic-analyze-cache-funcs/vars))
	(setq c localc))

      (when (or (not use-cache) desired-type completetexttype)
	(let ((origc c))
	  ;; Reset c.
	  ;;(setq c nil)
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
		    (setcdr m (cons tag (cdr m)))))))))
	    (setq origc (cdr origc))))

	(unless use-cache
	  (setq ca-semantic-analyze-cache-mtype-alist mtypes-alist)
	  (setq ca-semantic-analyze-cache-funcs/vars func/var-alist))

	(dolist (func/vars func/var-alist)
	  (setcdr func/vars (semantic-unique-tag-table-by-name (cdr func/vars)))))

  
      (if (not (or desired-type completetexttype))
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
	    (setq c (append local cur-buffer tags)))
	(setq c (ca-semantic-completions-1 completetexttype desired-type 
					     desired-class mtypes-alist
					     func/var-alist localc)))
      c)))


(defun ca-semantic-completions (completetexttype desired-type &optional local-tags)
  (ca-semantic-completions-1 completetexttype desired-type 
			     nil ;;desired-class
			     ca-semantic-analyze-cache-mtype-alist
			     ca-semantic-analyze-cache-funcs/vars
			     local-tags))

(defun ca-semantic-completions-1 (completetexttype desired-type 
				  desired-class mtypes-alist
				  func/var-alist local-tags)
  (let* ((accept (list (ca-semantic-strip-type desired-type)))
	 (tmp accept)
	 (tags nil))
    (when desired-type
    ;; (when (or (and desired-type (listp desired-type))
    ;; 	      completetexttype) ;;XXX completeext????
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

    (when desired-type
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
		  (setq cur-buffer (cons tag cur-buffer)))		 
		 ;; matches desired type XXX first ????
		 ((let ((ttype (ca-semantic-strip-type (semantic-tag-type tag))))
		    (when (equal ttype (ca-semantic-strip-type desired-type))
		      (setq tags (cons tag tags)))))
		 (t ;; XXX used?
		  (setq global (cons tag global))))))))

	(setq tags (append local cur-buffer tags global))))
    
    (when completetexttype
      (setq tmp tags)
      (setq tags nil)
      (let ((members local-tags)
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
		 (setq tags (cons member tags)))))
	     ;; desired type is reachable
	     ((memql member tmp)
	      (setq reachable (cons member reachable)))
	     ;; desired type is not reachable
	     (t
	      (setq unreachable (cons member unreachable))))))

	  (setq tags (append tags reachable unreachable)))))
    tags))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; semantic-analyze-fcn.el
;;; Tag Finding 
;;
;; Mechanism for lookup up tags by name.
;;
;; (defun semantic-analyze-find-tags-by-prefix (prefix)
;;   ;; @todo - only used in semantic-complete.  Find something better?
;;   "Attempt to find a tag with PREFIX.
;; is is a wrapper on top of semanticdb, and semantic search functions.
;; Almost all searches use the same arguments."
;;   (if (and (fboundp 'semanticdb-minor-mode-p)
;;            (semanticdb-minor-mode-p))
;;       ;; Search the database & concatenate all matches together.
;;       ;;(semanticdb-strip-find-results
;;        (semanticdb-fast-strip-find-results
;;        (semanticdb-find-tags-for-completion prefix))
;;     ;;'name)
;;     ;; Search just this file because there is no DB available.
;;     (semantic-find-tags-for-completion
;;      prefix (current-buffer))))

;; do not throw errors
;; (defun semantic-analyze-dereference-metatype-stack (type scope &optional type-declaration)
;;   "Dereference metatypes repeatedly until we hit a real TYPE.
;; Uses `semantic-analyze-dereference-metatype'.
;; Argument SCOPE is the scope object with additional items in which to search."
;;   (let ((lasttype type)
;;         (lasttypedeclaration type-declaration)
;;   	(nexttype (semantic-analyze-dereference-metatype 
;; 		   type scope type-declaration))
;;   	(idx 0))
;;     (while (and nexttype (not (eq (car nexttype) lasttype)) (< idx 10))
;;       (setq lasttype (car nexttype) 
;;             lasttypedeclaration (cadr nexttype))
;;       (setq nexttype (semantic-analyze-dereference-metatype 
;; 		      lasttype scope lasttypedeclaration))
;;       (setq idx (1+ idx))
;;       (when (> idx 20) (error "Possible metatype recursion for %S"
;;   			      (semantic-tag-name lasttype))))
;;     lasttype))


(provide 'ca2+semantic)