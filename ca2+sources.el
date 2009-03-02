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

;; Simple Example Source  
;; (defvar ca-source-lisp
;;   '((name       . "elisp")
;;     (candidates . '(lambda (prefix)  
;; 		     (all-completions 
;; 		      prefix obarray)))))


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
;; added '+'
(setq thing-at-point-file-name-chars "-~/[:alnum:]_.${}#+%,:")

(defun ca-source-filename-candidates (prefix)
  (let (strip-dash)
  (unless (string-match "\\(\./\\|\.\./\\|~/\\)" prefix)
    (setq strip-dash t)
    (setq prefix (concat "./" prefix)))
  (let ((dir (file-name-directory prefix)))
    (ignore-errors
      (mapcar (lambda (file) 
		(concat (if strip-dash (substring dir 2) dir) file))
              (remove-if (lambda (file) (or (equal file "../")
                                            (equal file "./")))
                         (file-name-all-completions
                          (file-name-nondirectory prefix) dir)))))))

(defvar ca-source-filename
  '((candidates . ca-source-filename-candidates)
    (decider    . filename) ;; TODO use function that checks content of 
                            ;; current dir for name completion without ./
    (limit      . 1)   ;; minimum prefix length to find completion
    (separator  . "/") ;; truncate candidates shown in popup
                       ;; before last position of separator
    (continue   . t)   ;; find new completions after expansion
    ;;(sorted     . t)
    (name       . "filename"))
  "ca2+ filename source")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; lisp symbols ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ca-source-lisp-candidates (prefix)
  (delete-if-not 
   '(lambda (cand) 
      (or (boundp (intern cand))
	  (fboundp (intern cand))))
   (all-completions (regexp-quote prefix) obarray)))


(defun ca-source-lisp-describe (candidate)
  (let ((symbol (intern candidate)))
  (cond ((fboundp symbol)
	 (describe-function symbol))
	((symbolp symbol)
	 (describe-variable symbol))
	(t
	 (message "no description")))))

(defvar ca-source-lisp
  '((name       . "elisp")
    (candidates . ca-source-lisp-candidates)
    (limit      . 1)
    (describe   . ca-source-lisp-describe)
    (sorted     . t)
    ;;(separator  . "-") ;; use this to strip common-prefix from tooltip
    (sort-by-occurrence . t)
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
    (sort-by-occurrence . t)
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
    (decider    . word)
    (action     . (lambda(candidate) (yas/expand)))
    (limit      . 1)
    (sorted     . t)
    (name       . "yasnippet"))
  "ca2+ yasnippet source")


(provide 'ca2+sources)
