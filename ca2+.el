;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CompleteAnything^2+ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; fork of company-mode.el, with code from auto-complete.el and completion
;; methods found on emacswiki ...pronounced 'c a 2 plus'

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

;; changes:
;; + tab cycles through sources 
;; + substring matching: type ca-substring-match-delimiter (default 'space')
;;   to start this mode. e.g. "a[tab] iso s[ret]" will insert:
;;   "add-log-iso8601-time-string". this feature also saves you from having to 
;;   type in not so easily reachable charachters like '-' or '_'
;; + expand-common expand the current selected candidate to next word boundary,
;;   if no common expansion is possible
;; 
;; + thing-at-point decider, see 'filename' source
;; + continue-after-insertion option, to get new completions after insertion, 
;;   see 'filename' source
;; + actions to execute after insertion, see yasnippet source
;; + candidates can be cons pairs, car is candidate, cdr is shown as minibuffer
;;   message, see yasnippet source
;; + sources can indicate whether their candidates have a common-prefix, this
;;   is used to reduce the number of visible candidates, as the prefix will
;;   be shown only once. after expansion of prefix  all candidates with that 
;;   prefix are shown. see gtags and elisp sources.
;;
;; TODO:
;; - add autoexpand
;;
;; BUGS:
;; - point jump around when cycling (remove current workaround)
;;

(require 'cl)
(require 'thingatpt)

(defgroup ca-mode nil
  "Interactive completion framework."
  :group 'convenience)

;; TODO
;; common - all
;; (defcustom ca-auto-expand nil
;;   "*Determines whether to automatically expand the first completion."
;;   :group 'ca-mode
;;   :type '(choice (const :tag "Off" nil)
;;                  (const :tag "On" t)))

(defcustom ca-verbose t
  "*Determines whether to explain user choices in the minibuffer."
  :group 'ca-mode
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom ca-display-style 'pseudo-tooltip
  "*Determines how to display possible candidates."
  :group 'ca-mode
  :type '(choice (const :tag "None" nil)
                 (const :tag "Pseudo Tooltip" pseudo-tooltip)))

(defface ca-common-face
  '((((class color) (background dark))
     (:foreground "DarkOrchid3"))
    (((class color) (background light))
     (:foreground "DarkOrchid4")))
  "*Face used for common parts during dynamic completion."
  :group 'ca-mode)

(defface ca-expand-face
  '((((class color) (background dark))
     (:foreground "DarkOrchid1"))
    (((class color) (background light))
     (:background "orange")))
  "*Face used for first complete match during dynamic completion."
  :group 'ca-mode)

(defface ca-pseudo-tooltip-face
  '((t :inherit default
       :background "gray20"
       :foreground "gray85"))
  "*Bla."
  :group 'ca-mode)

(defface ca-pseudo-tooltip-selection-face
  '((t :inherit ca-pseudo-tooltip-face
       :background "gray28"
       :foreground "white")
    )
  "*Bla."
  :group 'ca-mode)

;; (defcustom ca-tooltip-entire-names t
;;   "*Determines whether to show the entire name of candidates"
;;   :group 'ca-mode
;;   :type '(choice (const :tag "Off" nil)
;;                  (const :tag "On" t)))
(defvar  ca-tooltip-entire-names t)

(defcustom ca-how-many-candidates-to-show 15
  "*"
  :group 'ca-mode)

(defcustom ca-modes
  '(emacs-lisp-mode lisp-interaction-mode
                    c-mode cc-mode c++-mode java-mode
                    perl-mode cperl-mode python-mode ruby-mode
                    ecmascript-mode javascript-mode js2-mode php-mode css-mode
                    makefile-mode makefile-gmake-mode
		    sh-mode fortran-mode f90-mode ada-mode
                    xml-mode sgml-mode)
  "Major modes `ca-mode' can run on."
  :type '(list symbol)
  :group 'ca-mode)

(defvar ca-overlay nil)
(defvar ca-common-overlay nil)
(defvar ca-hide-overlay nil)
(defvar ca-pseudo-tooltip-overlays nil)
(defvar ca-source-alist nil)
(defvar ca-common nil)
(defvar ca-candidates nil)
(defvar ca-all-candidates nil)
(defvar ca-selection 0)
(defvar ca-prefix nil)
(defvar ca-current-source nil)
(defvar ca-last-command-change nil)
(defvar ca-complete-word-on nil)
(defvar ca-substring-match-delimiter " ")
(defvar ca-substring-match-on nil)
(defvar ca-initial-prefix nil)

(defvar ca-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'ca-expand-common)
    map)
  "Keymap used in by `ca-mode'.")

(defmacro ca-expand-number-macro (n)
  "Create in interactive function to select a completion by number N."
  `(lambda () (interactive) (ca-expand-number ,n)))

(defvar ca-active-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map ca-mode-map)
    (define-key map [down] 'ca-cycle)
    (define-key map "\M-n" 'ca-cycle)
    (define-key map [up] 'ca-cycle-backwards)
    (define-key map "\M-p" 'ca-cycle-backwards)
    (define-key map [return] 'ca-expand-top)
    (define-key map [left] 'ca-expand-common)
    (define-key map [right] 'ca-finish)
    (define-key map [tab] 'ca-next-source)
    (define-key map [(C left)] 'backward-char)
    (define-key map [(C right)] 'forward-char)
    (define-key map [tab] 'ca-next-source)
    (define-key map "\M-1" (ca-expand-number-macro 1))
    (define-key map "\M-2" (ca-expand-number-macro 2))
    (define-key map "\M-3" (ca-expand-number-macro 3))
    (define-key map "\M-4" (ca-expand-number-macro 4))
    (define-key map "\M-5" (ca-expand-number-macro 5))
    (define-key map "\M-6" (ca-expand-number-macro 6))
    (define-key map "\M-7" (ca-expand-number-macro 7))
    (define-key map "\M-8" (ca-expand-number-macro 8))
    (define-key map "\M-9" (ca-expand-number-macro 9))
    (define-key map "\M-0" (ca-expand-number-macro 10))
    map)
  "Keymap used in by `ca-mode' when candidates being completed.")


;; TODO: use property!
(defconst ca-continue-commands
  '(ca-expand-common ca-expand-top ca-expand-anything ca-cycle
    ca-cycle-backwards universal-argument
    ca-start-showing ca-match-abbrev
    ca-expand-abbrev ca-next-source)
  "Commands as given by `last-command' that don't end extending.")


;;;###autoload
(define-minor-mode ca-mode
  ""
  nil " ca2+" ca-mode-map
  (if ca-mode
      (progn
        (add-hook 'post-command-hook 'ca-post-command t t)
        (add-hook 'pre-command-hook 'ca-mode-pre-command nil t)
        (setq ca-common-overlay nil)
        (setq ca-overlay nil)
        (setq ca-hide-overlay nil)
        (setq ca-candidates nil)
        (setq ca-all-candidates nil)
        (setq ca-pseudo-tooltip-overlays nil)
        (setq ca-selection 0)
	(setq ca-complete-word-on nil)
	(setq ca-substring-match-on nil)
	(setq ca-current-source nil)
	(setq ca-initial-prefix nil))
    (ca-finish)
    (remove-hook 'post-command-hook 'ca-post-command t)
    (remove-hook 'pre-command-hook 'ca-mode-pre-command t)))


(defun ca-mode-maybe ()
  "What buffer `ca-mode' prefers."
  (if (and (not (minibufferp (current-buffer)))
           (memq major-mode ca-modes))
      (ca-mode 1)))


(define-global-minor-mode global-ca-mode
  ca-mode ca-mode-maybe
  :group 'ca-mode)


(defun ca-begin ()
  ;; workaround
  (when (looking-at "$") (insert-string " ") (backward-char))

  (ca-get-candidates)

  (if (null ca-candidates)
      (ca-finish)
    (ca-enable-active-keymap)
    (setq ca-common (try-completion "" ca-candidates))
    (setq ca-last-command-change (point))
    (setq ca-selection 0)
    ;; showing overlays is handled in post-command hook. this
    ;; here makes sure that company is not aborted.
    ;; (as it aborts on commands that are not in this list)
    (unless (memq this-command ca-continue-commands)
      (push this-command ca-continue-commands)))
  ca-candidates)


(defun ca-finish ()
  (interactive)
  ;;(message "finish")
  (ca-disable-active-keymap)
  (setq ca-candidates nil)
  (setq ca-all-candidates nil)
  (setq ca-substring-match-on nil)
  (ca-hide-overlay)
  (ca-hide-pseudo-tooltip)
  ;; workaround
  (when (looking-at " $") (delete-char 1))

  ;; TODO in which cases don not run hook?
  (ca-source-action)
  (setq ca-current-source nil))


(defun ca-filter-words-push (word candidate list)
  (let ((cands (assoc word list)))
    (unless cands
      (setq cands (cons word nil))
      (push cands list))
    (push candidate (cdr cands)))
  list)


(defun ca-filter-words ()
  ;;(message "ca-filter-words")
  (if (or (not (ca-source-has-common-prefix))
	  (<= (length ca-candidates)
	      ca-how-many-candidates-to-show))
      (setq ca-complete-word-on nil)
    (let ((cands nil)
	  (len (length ca-prefix))
	  cand end)
      (setq ca-complete-word-on t)
      (dolist (item ca-candidates)
	(setq cand (ca-candidate-string item))
	(setq end (string-match "\\W.+" cand len))
	(if (and end (= end len))
	    (setq end  (string-match "\\W" cand (1+ len)))
	  (setq end  (string-match "\\W" cand len)))
	(if end
	    (setq cands (ca-filter-words-push 
			 (substring cand 0 (1+ end))
			 cand cands))
	  (setq cands (ca-filter-words-push cand cand cands))))
      (setq ca-candidates nil)
      (dolist (item cands)
	(if (= (length (cdr item)) 1)
	    (push (cadr item) ca-candidates)
	  (push (car item) ca-candidates))))))


(defun ca-filter-candidates ()
  (let ((prefix (if ca-substring-match-on
		    (replace-regexp-in-string 
		     ca-substring-match-delimiter ".*"
		     ca-prefix)
		  ca-prefix)))
    ;;(message "filter: %s" prefix)
    (setq ca-candidates nil)
    (dolist (item ca-all-candidates)
      (if  (string-match (concat "^" prefix) (ca-candidate-string item))
	  (push item ca-candidates)))
    (setq ca-candidates (nreverse ca-candidates))
    (unless ca-substring-match-on
      (ca-filter-words))))


(defun ca-mode-pre-command ()
  (when ca-candidates
    (ca-hide-pseudo-tooltip)
    (ca-hide-overlay)
    (setq ca-last-command-change (point))))


(defun ca-post-command ()
  (when ca-candidates
    (cond
     ((eq this-command 'ca-expand-top)
      (when ca-complete-word-on
	(ca-grab-prefix)
	(unless (find-if '(lambda (item)
			    (string-equal ca-prefix (ca-candidate-string item)))
			 ca-all-candidates)
	  (ca-filter-candidates)
	  (setq ca-selection 0))))

     ;; expanded common substring
     ((eq this-command 'ca-expand-common)
      (setq cand (nth ca-selection ca-candidates))
      (ca-grab-prefix)
      (ca-filter-candidates)
      (setq ca-selection (or (position-if
			      '(lambda (c) (eq c cand))
			      ca-candidates) 0)))
     ;; char inserted
     ((eq (- (point) ca-last-command-change) 1)
      (when (looking-back ca-substring-match-delimiter)
	(setq ca-substring-match-on t))
      (if ca-substring-match-on
	  (setq ca-prefix (concat ca-prefix 
				  (buffer-substring-no-properties 
				   ca-last-command-change (point))))
	(ca-grab-prefix))
      (ca-filter-candidates)
      (setq ca-selection 0))

     ;; char deleted
     ((eq (- (point) ca-last-command-change) -1)
      (if ca-substring-match-on
	  (setq ca-prefix (substring 
			   ca-prefix 0
			   (1- (length ca-prefix))))
	(ca-grab-prefix))
      (if (>= (length ca-prefix) (length ca-initial-prefix))
	  (ca-filter-candidates)
	(ca-get-candidates))
      (setq ca-selection 0))

     ;; other command
     ((not (memq this-command ca-continue-commands))
      (setq ca-candidates nil)))

    (if (null ca-candidates)
	(ca-finish)

      ;; finish when only one candidate is left which
      ;; is equal prefix and no new candidates can be found
      (if (and (= (length ca-candidates) 1)
	       (string-equal ca-prefix
			     (ca-candidate-string-nth 0))
	       (not (ca-source-continue-after-expansion)))
	  (ca-finish)
	;; update overlays
	(if (>= ca-selection (length ca-candidates))
	    (setq ca-selection 0))
	(setq ca-common (try-completion "" ca-candidates))
	(ca-show-overlay)
	(ca-show-overlay-tips)))))


;; TODO pass candidate?
(defun ca-source-action ()
  (let ((action (cdr-safe (assq 'action ca-current-source))))
    (if action (funcall action))))


(defun ca-source-decider ()
  (let ((decider (cdr-safe (assq 'decider ca-current-source))))
    (if  decider
	(cond
	 ((functionp decider)
	  (funcall decider))
	 ((stringp decider) ;; regexp
	  (ca-grab-regexp decider))
	 ((consp decider)   ;; regexp w/ subexpression
	  (ca-grab-regexp (car decider) (cdr decider)))
	 ((symbolp decider) ;; thing-at-point thing
	  (car-safe (bounds-of-thing-at-point decider))))
      ;; use symbol thing by default
      (car-safe (bounds-of-thing-at-point 'symbol)))))


(defun ca-grab-regexp (regex &optional subexp)
  (save-excursion
    (let ((beg (re-search-backward regex nil t)))
      (if (and beg subexp)
	  (match-end subexp)
	beg))))


(defun ca-source-candidates ()
  (let ((c (cdr-safe (assq 'candidates ca-current-source))))
    (if c
	(funcall c ca-prefix))))


(defun ca-source-check-limit ()
  (let ((limit (cdr-safe (assq 'limit ca-current-source))))
    (not (and limit (< (length ca-prefix) limit)))))


(defun ca-source-is-sorted ()
  (cdr-safe (assq 'sorted ca-current-source)))


(defun ca-source-separator ()
  (cdr-safe (assq 'separator ca-current-source)))


(defun ca-source-has-common-prefix ()
  (cdr-safe (assq 'common-prefix ca-current-source)))


(defun ca-source-continue-after-expansion ()
  (if (not (cdr-safe (assq 'continue ca-current-source)))
      nil
    (let ((prefix ca-prefix))
      (ca-get-candidates)
      ;; test if new candidates were found
      (not (and (= (length ca-candidates) 1)
		(string-equal prefix (ca-candidate-string-nth 0)))))))


;; check wheter the candidate list is made of cons pairs
(defsubst ca-cons-candidates (cands)
  (consp (car-safe cands)))


(defun ca-sort-candidates (cands)
  (if (ca-cons-candidates cands)
      (sort* cands
	     '(lambda (c1 c2) (string< (car c1) (car c2))))
    (sort* cands 'string<)))


(defun ca-get-candidates (&optional next)
  (let* ((candidates nil)
	 (sources (append
		 (cdr (assoc major-mode ca-source-alist))
		 (cdr (assoc 'otherwise ca-source-alist))))
	 (sources (delq nil sources)))

    ;; cycle to next source
    (if (and next sources ca-current-source)
	(let ((tmp sources))
	  (while (and tmp (not (eq ca-current-source (car tmp))))
	    (setq tmp (cdr tmp)))
	  (if (and tmp (cdr tmp))
	      (setq sources (cdr tmp))))
      ;; or continue with the same source as before
      (if ca-current-source
	  (setq sources (list ca-current-source))))

    (while (and sources (null candidates))
      (let ((source (car sources)))
	(setq ca-current-source source)
	(ca-grab-prefix)
	(when (ca-source-check-limit)
	  (setq candidates (ca-source-candidates)))
	(setq sources (cdr sources))))

    (when candidates
      (message "%s candidates" (cdr-safe (assq 'name ca-current-source)))
      (unless (ca-source-is-sorted)
	(setq candidates (ca-sort-candidates candidates)))

      (if (cdr-safe (assq 'sort-by-occurence ca-current-source))
	  (let ((around (delete ca-prefix (ca-words-in-buffer)))
		(len (length candidates)))
	    (dolist (word around)
	      (setq candidates (delete word candidates))
	      (if (not (eq len (length candidates)))
		  (push word candidates)))))

      (setq ca-initial-prefix ca-prefix)
      (setq ca-substring-match-on nil)
      (setq ca-all-candidates candidates)
      (setq ca-candidates ca-all-candidates)
      (ca-filter-words))
    candidates))


;;FIXME set prefix nil if nothing is found
(defun ca-grab-prefix (&optional thing)
    (setq start (ca-source-decider))
    (setq ca-prefix
	  (if start (buffer-substring-no-properties start (point)) "")))


(defsubst ca-chop (candidate)
  (if (and candidate ca-prefix);; XXX
      (let ((len (length ca-prefix)))
	(substring (ca-candidate-string candidate) len))))


(defsubst ca-candidate-string (cand)
  (if (and cand (consp cand)) (car cand) cand))


(defsubst ca-candidate-string-nth (n)
  (let ((cand (nth n ca-candidates)))
    (if (and cand (consp cand)) (car cand) cand)))


(defun ca-insert-candidate (candidate)
  (delete-region (- (point) (length ca-prefix)) (point))
  (insert (ca-candidate-string candidate)))



;;; interactive commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ca-next-source ()
  (interactive)
  (setq ca-candidates nil)
  (setq ca-all-candidates nil)
  (ca-get-candidates :next))


;;; TODO: make this the Nth _visible_ completion?
(defun ca-expand-number (n &optional word)
  "Expand the Nth candidate."
  (interactive "P")
  (unless ca-mode (error "ca-mode not enabled"))
  (unless ca-candidates
    (ca-begin))
  (if (not ca-candidates)
      (message "No candidates found")
    (let* ((candidate (nth (1- n) ca-candidates))
	   (cand (if candidate (ca-candidate-string candidate)))
	   (tmp cand))
      (if (not cand)
	  (error "No such candidate")
	(if word
	    (ca-insert-candidate
	     (substring cand 0
			(or (and (string-match "^\\W" cand) 1)
			    (string-match "\\W" cand)
			    (length cand))))
	  (ca-insert-candidate cand)))
      candidate)))


;; XXX remove?
(defmacro ca-expand-then (command)
  `(lambda () (interactive) (ca-expand-top) (call-interactively ,command)))


(defun ca-start-showing ()
  (interactive)
  (unless ca-candidates (ca-begin)))


(defun ca-expand-or-cycle ()
  (interactive)
  (unless ca-mode (error "ca-mode not enabled"))
  (if ca-candidates
      (ca-cycle)
    (ca-begin)))


(defun ca-cycle (&optional n)
  (interactive)
  (unless ca-mode (error "ca-mode not enabled"))
  (unless ca-candidates
    (ca-begin))
  (if ca-candidates
      (setq ca-selection
            (min (max 0 (+ ca-selection (or n 1)))
                 (1- (length ca-candidates))))
    (message "No candidates found"))
  (if ca-candidates
      (let ((cand (nth ca-selection ca-candidates)))
	(if (consp cand)
	    (message "%s" (cdr cand))))))


(defun ca-cycle-backwards (&optional n)
  (interactive)
  (ca-cycle (- (or n 1)))
  (setq this-command 'ca-cycle))


(defun ca-expand-top ()
  (interactive)
  (setq ca-candidates (list (ca-expand-number (1+ ca-selection))))
  (setq ca-prefix (ca-candidate-string-nth 0)))


(defun ca-expand-anything ()
  (interactive)
  (when (equal (ca-expand-common) "")
    (ca-expand-top)))


(defun ca-expand-common ()
  (interactive)
  (unless ca-mode (error "ca-mode not enabled"))
  (unless ca-candidates
    (ca-begin))
  (if ca-complete-word-on
      (ca-expand-top)
    (if ca-candidates
	(let ((common ca-common))
	  (if (= (length ca-prefix) (length common))
	      (ca-expand-number (1+ ca-selection) t)
	    (ca-insert-candidate common))
	  common)
      (when (called-interactively-p)
	(error "No candidates found")))))


(defmacro ca-without-undo (&rest body)
  `(let ((buffer-undo-list nil)
         (inhibit-modification-hooks t))
     ,@body))


(defun ca-put-overlay (beg end &optional prop value prop2 value2)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'window t)
    (when prop
      (overlay-put ov prop value)
      (when prop2
        (overlay-put ov prop2 value2)))
    (when (eq prop 'keymap)
      (overlay-put ov 'face 'ca-common-face))
    ov))


;; TODO there must be a library function for this ....
(defun ca-sublist (list start length)
  (dotimes (i start)
    (pop list))
  (unless (or (= length 0) (null list))
    (let* ((new-list (cons (pop list) nil))
           (tail new-list))
      (while (and list (> (decf length) 0))
        (setq tail (setcdr tail (cons (pop list) nil))))
      new-list)))


(defsubst ca-get-selection-offset (page-size)
  (when ca-how-many-candidates-to-show
    (setq page-size (min page-size ca-how-many-candidates-to-show)))
  (let ((offset (* (/ ca-selection page-size) page-size)))
    offset))


(defsubst ca-pick-candidates (page-size)
  (when ca-how-many-candidates-to-show
    (setq page-size (min page-size ca-how-many-candidates-to-show)))
  (if t ;;(ca-tooltip-entire-names)
      (ca-sublist ca-candidates
		       (ca-get-selection-offset page-size) page-size)
    (mapcar 'ca-chop
	    (ca-sublist ca-candidates
			     (ca-get-selection-offset page-size) page-size))))


;;; keymap ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ca-original-keymap nil)
(make-variable-buffer-local 'ca-original-keymap)


(defun ca-enable-active-keymap ()
  (setcdr (assoc 'ca-mode minor-mode-map-alist)
          ca-active-map))


(defun ca-disable-active-keymap ()
  (setcdr (assoc 'ca-mode minor-mode-map-alist)
          ca-mode-map))


;;; overlays ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ca-show-overlay ()
  (if ca-overlay
      (ca-hide-overlay))
  (let* ((candidate (nth ca-selection ca-candidates))
	 (candidate (if ca-substring-match-on candidate (ca-chop candidate)))
	 (prefix-length (length ca-prefix))
	 (common-length (length ca-common))
	 (beg (point)))
    (ca-without-undo
     (save-excursion
       (if ca-substring-match-on (insert " ["))
       (insert candidate)
       (if ca-substring-match-on (insert "]"))
       (setq ca-overlay
	     (ca-put-overlay beg (point)
			     'face 'ca-expand-face))
       (if (= common-length 0)
	   (setq ca-common-overlay
		 (ca-put-overlay (- beg prefix-length) beg
				 'face 'ca-common-face))
	 (setq ca-common-overlay
	       (ca-put-overlay (- beg prefix-length)
			       (+ (- beg prefix-length) common-length)
			       'face 'ca-common-face)))))))


(defun ca-show-overlay-tips ()
  (when (cdr ca-candidates)
    (when (eq ca-display-style 'pseudo-tooltip)
      (ca-show-list-pseudo-tooltip))))


(defun ca-hide-overlay ()
  (ca-without-undo
   (when ca-overlay
     (delete-region (overlay-start ca-overlay)
                    (overlay-end ca-overlay))
     (delete-overlay ca-overlay)
     (setq ca-overlay nil))
   (when ca-common-overlay
     (delete-overlay ca-common-overlay)
     (setq ca-common-overlay nil))
   (when ca-hide-overlay
     (delete-overlay ca-hide-overlay)
     (setq ca-hide-overlay nil))))


;;; pseudo tooltip ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ca-show-list-pseudo-tooltip (&optional point)
  (let* ((max-lines (- (window-height) 2))
         (candidates (ca-pick-candidates max-lines)))
    (ca-show-pseudo-tooltip-at-point
     candidates
     (- ca-selection (ca-get-selection-offset max-lines)))))


(defun ca-hide-pseudo-tooltip ()
  (dolist (ov ca-pseudo-tooltip-overlays)
    (delete-overlay ov))
  (setq ca-pseudo-tooltip-overlays nil))


(defun ca-show-pseudo-tooltip-line (start replacement &optional no-insert)
  ;; start might be in the middle of a tab, which means we need to hide the
  ;; tab and add spaces
  (let ((end (+ start (length replacement)))
        beg-point end-point
        before-string after-string)
    (goto-char (point-at-eol))
    (if (< (current-column) start)
        (progn (setq before-string
                     (make-string (- start (current-column)) ? ))
               (setq beg-point (point)))
      (goto-char (point-at-bol)) ;; Emacs bug, move-to-column is wrong otherwise
      (move-to-column start)
      (setq beg-point (point))
      (when (> (current-column) start)
        (goto-char (1- (point)))
        (setq beg-point (point))
        (setq before-string (make-string (- start (current-column)) ? ))))
    (move-to-column end)
    (setq end-point (point))
    (let ((end-offset (- (current-column) end)))
      (when (> end-offset 0)
        (setq after-string (make-string end-offset ?b))))
    (when no-insert
      ;; prevent inheriting of faces
      (setq before-string (when before-string
                            (propertize before-string 'face 'default)))
      (setq after-string (when after-string
                           (propertize after-string 'face 'default))))
    (let ((string (concat before-string
                          replacement
                          after-string)))
      (if no-insert
          string
        (push (ca-put-overlay beg-point end-point
                                   'invisible t
                                   'after-string string)
              ca-pseudo-tooltip-overlays)))))


(defun ca-pseudo-tooltip-strip ()
  (let* ((sepstart nil)
	 (separator (ca-source-separator))
	 (start
	  (cond
	  ;; start at last separator
	  ((and ca-common separator)
	   (setq sepstart (string-match
			   (concat "\\("separator "\\)[^" separator "]*$")
			   ca-prefix))
	   (if sepstart (1+ sepstart) 0))
	  ;; start at beginning of prefix
	  (t ;;(ca-tooltip-entire-names)
	   0)
	  ;; start at current column
	  ;;(t
	  ;; (length ca-prefix))
	  )))
    start))


(defun ca-show-pseudo-tooltip-at-point (lines &optional highlight)
  (ca-hide-pseudo-tooltip)
  (let ((lines-to-bottom (- (window-height) (count-lines (point) (window-start)))))
    (if (< lines-to-bottom (length lines))
	(scroll-down (+ (- lines-to-bottom (length lines) 2)))))

  (let* ((strip (ca-pseudo-tooltip-strip))
	 (start (- (- (current-column)
		      (- (length ca-prefix) strip))
		   (window-hscroll)))
	 (lines (if (consp (car lines)) (mapcar 'car lines) lines))
	 ;; strip redundant prefix
	 (lines (if (> strip 0)
		    (mapcar '(lambda (s) (substring s strip))
			    lines)
		  lines))

	 (lengths (mapcar 'length lines))
	 (max-length (min (apply 'max lengths)
			  (- (+ (window-hscroll) (window-width)) start)))
	 (i -1)
	 (lines (mapcar*
                 '(lambda (line length)
                    (let ((diff (- max-length length)))
                      (propertize
                       ;; make the string the correct size
                       (if (> diff 0)
                           ;; pad line with spaces
                           (concat line (make-string diff ? ))
                         ;; we might be at the right end of the buffer
                         (substring line 0 max-length))
                       'face (if (equal (incf i) highlight)
                                 'ca-pseudo-tooltip-selection-face
                               'ca-pseudo-tooltip-face))))
                 lines lengths)))
    (save-excursion
      (let ((max (point-max)))
        (while (and lines (/= (vertical-motion 1) 0))
          (ca-show-pseudo-tooltip-line (+ (current-column) start)
                                            (pop lines)))
        (when lines
          ;; append to end of buffer in one giant
          (let* ((newline (propertize "\n" 'face 'default))
                 (append newline))
            (while lines
              (setq append
                    (concat append
                            (ca-show-pseudo-tooltip-line
                             (+ (window-hscroll) (+ (current-column) start))
			     (pop lines) t)
                             newline)))
            ;; Add the appended lines to the last overlay, unless we didn't
            ;; create any yet, or we aren't at point-max yet.  We have to
            ;; append, because otherwise two overlays, both at point-max, will
            ;; be in reversed order.
            (let ((ov (car-safe ca-pseudo-tooltip-overlays)))
              (unless (and ov
                           (= (overlay-end ov) (point-max)))
                (setq ov (make-overlay (point-max) (point-max)))
                (push ov ca-pseudo-tooltip-overlays))
              (overlay-put ov 'after-string
                           (concat (overlay-get ov 'after-string)
                                   append)))))))))


;;; Completion Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ca-add-completion-source-1 (major-mode source)
  "Add a completion source for `ca-mode'."
  (let ((sources (assoc major-mode ca-source-alist)))
    (unless sources
      (setq sources (cons major-mode nil))
      (push sources ca-source-alist))
    (push source (cdr sources))
    source))

(defun ca-add-completion-source (major-mode source)
  "Add a completion source."
  (if (consp major-mode)
      (dolist (mode major-mode)
	(ca-add-completion-source-1 mode source))
    (ca-add-completion-source-1 major-mode)))
  

(defun ca-clear-completion-sources ()
  (setq ca-source-alist))


;;; debug ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; don't debug
(dolist (mess '("^No candidates found$"
                "^No such completion$"
                "^ca-mode not enabled$"))
  (add-to-list 'debug-ignored-errors (regexp-quote mess)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; taken from auto-complete, find word near to point for sorting
;; candidates
(defun ca-words-in-buffer ()
  "Default implementation for `ac-candidate-function'."
  (if (> (length ca-prefix) 0)
      (let ((i 0)
	    (ac-point (point))
	    (ac-limit 15)
            candidate
            candidates
            (regexp (concat "\\b" (regexp-quote ca-prefix)
			    "\\(\\s_\\|\\sw\\)*\\b")))
        (save-excursion
          ;; search backward
          (goto-char (- ac-point (length ca-prefix)))
          (while (and (< i ac-limit)
                      (re-search-backward regexp nil t))
            (setq candidate (match-string-no-properties 0))
            (unless (member candidate candidates)
              (push candidate candidates)
              (setq i (1+ i))))
          ;; search forward
          (goto-char (+ ac-point (length ca-prefix)))
          (while (and (< i ac-limit)
                      (re-search-forward regexp nil t))
            (setq candidate (match-string-no-properties 0))
            (unless (member candidate candidates)
              (push candidate candidates)
              (setq i (1+ i))))
          (goto-char ac-point))
	candidates)))

(provide 'ca2+)
