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
  "Ca2+ dabbrev source")



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
    (continue   . ca-source-filename-candidates) ;; find new completions
    ;;(sorted     . t)
    (name       . "filename"))
  "Ca2+ filename source")


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
  "Ca2+ lisp symbol source")



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
  "Ca2+ gtags source")



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
  "Ca2+ yasnippet source")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; python rope ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ca-source-rope-candidates (prefix)
  (let (cands)
    (dolist (cand (rope-completions))
      ;;(setq cands (cons (format "%s%s" prefix cand) cands)))
      (setq cands (cons (concat prefix cand) cands)))
    cands))

(defun ca-source-rope-decider ()
  (let ((symbol (car-safe (bounds-of-thing-at-point 'symbol))))
    (if (null symbol)
        (when (looking-back "\.")
	  (point))
      symbol)))

(defun ca-source-rope-continue (candidate)
  (setq ca-prefix "")
  (setq ca-initial-prefix "")
  (insert ".")
  (let ((cands (ca-source-rope-candidates "")))
    (or (nreverse cands)
	(delete-char -1))))

(defvar ca-source-python-rope
  '((candidates . ca-source-rope-candidates)
    (decider    . ca-source-rope-decider)
    (continue   . ca-source-rope-continue)
    (filter     . t)
    (sorted     . nil)
    (sort-by-occurrence . t)
    (common-prefix . t)
    (name       . "python"))
  "Ca2+ rope powered python source")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rust racer  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ca--source-racer-candidates(prefix)
  (racer--candidates))

(defun ca--source-racer-decider()
  (if (looking-back "[:.]")
      (point)
    (car-safe (bounds-of-thing-at-point 'word))
    ;;(re-search-backward "\\([a-zA-z1-9]\\)*" nil t)
    ))

(defun ca--source-racer-tag-extend (arg)
  (format "%10s : %s"
	  (get-text-property 0 'matchtype arg)
	  (get-text-property 0 'contextstr arg)))

(defvar ca-source-rust-racer
  '((candidates . ca--source-racer-candidates)
    (decider    . ca--source-racer-decider)
    ;;(continue   . ca-source-racer-continue)
    (extend . ca--source-racer-tag-extend)
    (filter     . t)
    (sorted     . nil)
    (sort-by-occurrence . t)
    (common-prefix . t)
    (name       . "rust"))
  "Ca2+ racer powered rust source")

(provide 'ca2+sources)

