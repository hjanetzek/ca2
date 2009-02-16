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
      (while (and (< i 20)
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
      (while (and (< j 20) 
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


;; (defun ca-dabbrev-completion-func (prefix)
;;   (require 'dabbrev)
;;   (let ((dabbrev-check-other-buffers))
;;     (dabbrev--reset-global-variables)
;;     (dabbrev--find-all-expansions prefix nil)))



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


(defvar ca-source-lisp
  '((candidates . ca-source-lisp-candidates)
    (limit . 1)
    (sorted . nil)
    ;;(separator  . "-") ;; use this to strip common-prefix from tooltip
    (sort-by-occurence . t)
    (common-prefix . t) ;; candidates have common prefixes,
                        ;; this is used to reduce the number 
                        ;; of visible candidates, instead
                        ;; the prefixes are shown.
    (name . "elisp"))
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
    (action     . yas/expand)
    (limit      . 1)
    (sorted     . t)
    (name       . "yasnippet"))
  "ca2+ yasnippet source")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; semantic ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; from http://www.emacswiki.org/emacs/AutoCompleteSources#toc3
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
					  (string= type "class"))
				     (eq class 'function)
				     (eq class 'variable))
				 (list (list name type class))))))
		     (semantic-fetch-tags)))))
    (when list
      (setq ca-source-semantic-tags-analysis (apply 'append list))
      (or (car-safe (bounds-of-thing-at-point 'symbol))
	  (point))))

(defun ca-source-semantic-tags-candidates (prefix)
  (mapcar 'car ca-source-semantic-analysis))

(defvar ca-source-semantic-tags-analysis nil)
(defvar ca-source-semantic-tags
  '((decider . ca-source-semantic-tags-decider)
    (candidates . ca-source-semantic-tags-candidates)
    (limit . 1)
    (name . "semantic-tags")))


(defvar ca-source-semantic-context-completions nil)
(defun ca-source-semantic-context-decider ()
  (let* ((p (point))
	 (a (semantic-analyze-current-context p))
	 (syms (if a (semantic-ia-get-completions a p)))
	 (completions (mapcar 'semantic-tag-name syms)))
    (message "updated %d completions" (length completions))

    (when completions
      (setq ca-source-semantic-context-completions completions)
      (or (car-safe (bounds-of-thing-at-point 'symbol))
	  p))))

(defvar ca-source-semantic-context
  '((decider . ca-source-semantic-context-decider)
    (candidates . (lambda(prefix) ca-source-semantic-context-completions))
    (name . "semantic-context")))




(provide 'ca2+sources)