;;;;;;;;;;;;;;;;;;;; CompleteAnything^2+ sources ;;;;;;;;;;;;;;;;;;;;;;
;;
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


;; based on company-mode.el, auto-complete.el and completion methods
;; found on emacswiki


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; dabbrev ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ca-source-dabbrev-candidates-max 40)
;; taken from emacswiki CompletionUI
(defun ca-source-dabbrev-candidates (prefix)
  "A wrapper for dabbrev that returns a list of expansion of
  PREFIX ordered in the same way dabbrev-expand find expansions.
  First, expansions from the current point and up to the beginning
  of the buffer is listed. Second, the expansions from the current
  point and down to the bottom of the buffer is listed. Last,
  expansions in other buffer are listed top-down. The returned
  list has at most MAXNUM elements."
  (dabbrev--reset-global-variables)
  (let ((all-expansions nil)
	(i 0)
	(j 0)
	(ignore-case nil)
	expansion)
    ;; Search backward until we hit another buffer or reach max num
    (save-excursion
      (while (and (< i (/ ca-source-dabbrev-candidates-max 2))
		  (setq expansion 
			(dabbrev--find-expansion prefix 1 ignore-case))
		  (not dabbrev--last-buffer))
	(setq all-expansions (nconc all-expansions (list expansion)))
	(setq i (+ i 1))))
    ;; If last expansion was found in another buffer, remove of it from the
    ;; dabbrev-internal list of found expansions so we can find it when we
    ;; are supposed to search other buffers.
    (when (and expansion dabbrev--last-buffer)
      (setq dabbrev--last-table (delete expansion dabbrev--last-table)))
    ;; Reset to prepeare for a new search
    (let ((table dabbrev--last-table))
      (dabbrev--reset-global-variables)
      (setq dabbrev--last-table table))
    ;; Search forward in current buffer and after that in other buffers
    (save-excursion
      (while (and (< j (/ ca-source-dabbrev-candidates-max 2))
		  (setq expansion 
			(dabbrev--find-expansion prefix -1 ignore-case)))
	(setq all-expansions (nconc all-expansions (list expansion)))
	(setq j (+ i j))))
    all-expansions))

(defvar ca-source-dabbrev
  '((candidates . ca-source-dabbrev-candidates)
    (limit      . 1)
    (sorted     . t)
    (name       . "dabbrev"))
  "ca2+ dabbrev source")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; filename ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ca-source-filename-candidates (prefix)
  (let ((dir (file-name-directory prefix)))
    (ignore-errors
      (mapcar (lambda (file) (concat dir file))
              (remove-if (lambda (file) (or (equal file "../")
                                            (equal file "./")))
                         (file-name-all-completions
                          (file-name-nondirectory prefix) dir))))))


(defvar ca-source-filename
  '((candidates . ca-source-filename-candidates)
    (decider    . filename)
    (limit      . 1)   ;; minimum prefix length to find completion
    (separator  . "/") ;; truncate candidates shown in popup
                       ;; before last position of separator
    (continue   . t)   ;; find new completions after expansion
    ;;(sorted     . t)
    (name       . "filename"))
  "ca2+ filename source")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; lisp symbols ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ca-source-lisp-candidates (prefix)
  (all-completions prefix obarray))


(defun ca-source-lisp-describe (candidate)
  (let ((symbol (intern candidate)))
  (cond ((fboundp symbol)
	 (describe-function symbol))
	((symbolp symbol)
	 (describe-variable symbol))
	(t
	 (message "no description")))))


;;(defun ca-source-lisp-action (candidate))


(defvar ca-source-lisp
  '((name       . "elisp")
    (candidates . ca-source-lisp-candidates)
    (limit      . 1)
    (describe   . ca-source-lisp-describe)
    (sorted     . t)
    ;;(action     . ca-source-lisp-action)
    ;;(separator  . "-") ;; use this to strip common-prefix from tooltip
    (sort-by-occurence . t)
    (common-prefix . t)) ;; candidates have common prefixes,
                         ;; this is used to reduce the number 
                         ;; of visible candidates, instead
                         ;; the prefixes are shown.
  "ca2+ lisp symbol source")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; gtags ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; from auto-complete-extension.el, by Andy Stewart

(defun ca-source-gtags-candidates (prefix)
  (all-completions prefix
   (let ((option "-c")
	 all-expansions
	 expansion)
     (with-temp-buffer
       (call-process "global" nil t nil option prefix)
       (goto-char (point-min))
       (while (looking-at gtags-symbol-regexp)
	 (setq expansion (gtags-match-string 0))
	 (setq all-expansions (cons expansion all-expansions))
	 (forward-line)))
     all-expansions)))


(defvar ca-source-gtags
  '((candidates . ca-source-gtags-candidates)
    (limit      . 1)
    (sorted     . nil)
    (sort-by-occurence . t)
    (common-prefix . t)
    (name       . "gtags"))
  "ca2+ gtags source")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; yasnippet ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; taken from auto-complete.el

(defun ca-source-yasnippet-candidates-1 (table)

  (let ((hashtab (yas/snippet-table-hash table))
        (parent (yas/snippet-table-parent table))
	(regex (concat "^" prefix))
        cands)
    (maphash (lambda (key value)
	       (if (string-match regex key)
		   (push (cons key (yas/template-name (cdar value)))
			 cands)))
             hashtab)
    (if parent
	(append cands (ca-source-yasnippet-candidates-1 parent))
      cands)))


(defun ca-source-yasnippet-candidates (prefix)
  (let ((table (yas/snippet-table major-mode)))
    (if table
	(ca-source-yasnippet-candidates-1 table))))


(defvar ca-source-yasnippet
  '((candidates . ca-source-yasnippet-candidates)
    (action     . (lambda(candidate) (yas/expand)))
    (limit      . 1)
    (sorted     . t)
    (name       . "yasnippet"))
  "ca2+ yasnippet source")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; semantic ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; sematinc tags completion
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


;; modified from semantic-format.el, creates yas-template
(defun ca-source-semantic-format-tag-arguments (args formatter)
  "Format the argument list ARGS with FORMATTER.
FORMATTER is a function used to format a tag.
COLOR specifies if color should be used."
  (let ((out nil)
	(cnt 1))
    (while args
      (unless (equal (semantic-tag-name (car args)) "")
	  (push (concat "${" (number-to-string cnt) ":"
			(if (and formatter
				 (semantic-tag-p (car args))
				 (not (string= (semantic-tag-name (car args)) "")))
			    (funcall formatter (car args) nil nil)
			  (semantic-format-tag-name-from-anything
			   (car args) nil nil 'variable))
			"}")
		out))
      (setq cnt (1+ cnt))
      (setq args (cdr args)))
    ;;semantic-function-argument-separator
    (mapconcat 'identity (nreverse out) ", ")))


(defun ca-source-semantic-tags-action (candidate)
  (let ((tag (cdr-safe candidate)))
    (when (semantic-tag-p tag)
      (cond 
       ((eq (semantic-tag-class tag) 'function)
	(yas/expand-snippet 
	 (point) (point) 
	 (concat 
	  "(" (ca-source-semantic-format-tag-arguments
	       (semantic-tag-function-arguments tag)
	       #'semantic-format-tag-prototype) ")"
	  (unless(looking-at "[[:space:]]*\\()\\|,\\)") ";"))))
       ;; expansion was called for a complete variable name
       ((and (not (memq this-command (cdr ca-continue-commands)))
	     (eq (semantic-tag-class tag) 'variable))
	(let ((candidates (ca-source-semantic-continue candidate)))
	  (when candidates
	    (message "got cands")
	    (ca-begin candidates ca-source-semantic-context))))))))


(defvar ca-source-semantic-tags
  '((decider . ca-source-semantic-tags-decider)
    (candidates . ca-source-semantic-tags-candidates)
    (action . ca-source-semantic-tags-action)
    (limit . 1)
    (info . ca-source-semantic-tag-summary)
    (filter . t) ;; source provides all candidates: filter with prefix
    (name . "semantic-tags"))
  "ca2+ semantic source for tag completion")


;; semantic context completion
(defvar ca-source-semantic-context-completions nil)
(defvar ca-source-semantic-context-cntxt nil)


(defun ca-source-semantic-context-decider ()
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


(defun ca-source-semantic-continue (candidate)
  (let ((tag (cdr-safe candidate)))
    (when (and tag (eq (semantic-tag-class tag) 'variable))
      (let* ((ttype (semantic-tag-type tag))
	      type members)

	(while (and ttype (listp ttype))
	  ;; find parent of variable
	  (setq type (semantic-analyze-find-tag (car ttype)))
	  ;; check wheter the parent is typedef, TODO class, whatever
	  (setq ttype (semantic-tag-get-attribute type :typedef)))

	(when (and type (setq members (semantic-tag-type-members type)))	
	  ;; sort members by reachability
	  (if (and (boundp 'semantic-analyze-cache-tags)
		   semantic-analyze-cache-tags 
		   ca-source-semantic-context-cntxt)
	      (let* ((a ca-source-semantic-context-cntxt)
		    (desired-type (semantic-analyze-type-constraint a)))
		(setq members (ca-semantic-completions
			       type
			       desired-type))))

	  (if (semantic-tag-get-attribute tag :pointer)
	      (insert "->") (insert "."))
	    
	  (mapcar '(lambda(tag) (cons (semantic-tag-name tag) tag)) 
		  members))))))


(defun ca-source-semantic-context-candidates (prefix)
  ca-source-semantic-context-completions)


(defvar ca-source-semantic-context
  '((decider . ca-source-semantic-context-decider)
    (candidates . ca-source-semantic-context-candidates)
    (action . ca-source-semantic-tags-action)
    (info . ca-source-semantic-tag-summary)
    (continue . ca-source-semantic-continue)
    (sorted . t)
    (trigger . ".")
    ;;(filter . t)
    (name . "semantic-context"))
  "ca2+ source for semantic context completion")


;; semantic completion for function argument templates, inserted 
;; text needs to be deleted before semantic-analyze-current-context
;; can do its work
(defun ca-source-semantic-args-decider ()
  (let* ((overlays (overlays-at (point)))
	 (overlay (find-if '(lambda(ov) 
			      (overlay-get ov 'yas/snippet)) 
			   overlays)))
    (when overlay
      (let* ((start (overlay-start overlay))
	    (end (overlay-end overlay))
	    (reg (buffer-substring start end))
	    (foo (delete-region start end))
	    (prefix-start (ca-source-semantic-context-decider)))
	(if prefix-start
	    prefix-start
	  (insert reg)
	  (goto-char start)
	  nil)))))


(defvar ca-source-semantic-yas-arguments
  '((decider . ca-source-semantic-args-decider)
    (candidates . ca-source-semantic-context-candidates)
    (info . ca-source-semantic-tag-summary)
    (continue . ca-source-semantic-continue)
    (sorted . t)
    ;;(sort-by-occurence . t)
    (name . "semantic-arguments"))
  "ca2+ source for semantic argument completion")


(provide 'ca2+sources)

