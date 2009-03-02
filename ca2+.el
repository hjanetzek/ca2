;;;;;;;;;;;;;;;;;;;;;;;;;;;; CompleteAnything^2+ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; fork of company-mode.el, with code from auto-complete.el and completion
;; methods found on emacswiki
;;
;; plesase send Bug reports and suggestions to 'hannes dot janetzek at gmail dot com'

;; This file is NOT part of GNU Emacs
;;
;;
;;  License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.


;; either enable ca-mode per buffer via (ca-mode 1) or use (global-ca-mode 1)
;; to enable it in all buffer that match 'ca-modes'
;;
;; (require 'ca2+)
;; (require 'ca2+sources)
;; (require 'ca2+config) ;; change this to your needs
;;
;; if you use yasnippet: it must be loaded after ca2+config or you will
;; get infinite recursions. I'll have to look into yasnippet to see why 
;; it does.
;;
;; changes:
;; + tab cycles through sources
;; + substring matching: type ca-substring-match-delimiter (default 'space')
;;   to start this mode. e.g. "a[tab] 8 s[ret]" will insert:
;;   "add-log-iso8601-time-string". this feature also saves you from having to
;;   type in not so easily reachable charachters like '-' or '_'
;; + expand-common expand the current selected candidate to next word boundary,
;;   if no common expansion is possible
;; + candidates are sorted by words in current buffer
;; + ac-styled source description: so it's really easy to port sources for
;;   auto-complete
;;
;; + describe option: sources can provide a description function for candidates
;;   bound to C-h, see elisp source
;; + continue-after-insertion option, to get new completions after insertion,
;;   see 'filename' source
;; + actions to execute after insertion, see yasnippet source
;; + candidates can be cons pairs, car is candidate, cdr is shown as minibuffer
;;   message, see yasnippet source
;; + sources can indicate whether their candidates have a common-prefix, this
;;   is used to reduce the number of visible candidates, as the prefix will
;;   be shown only once. after expansion of prefix  all candidates with that
;;   prefix are shown. see gtags and elisp sources.
;; + thing-at-point decider can be used now, see 'filename' source

;; TODO:
;; - add autoexpand
;; - when there is only one candidate to expand move point to end
;;   so that one only need to press space?
;;
;; BUGS:
;; - expand-common does weird thing when word-filter is on
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

;;(setq  ca-tooltip-entire-names nil)

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
(defvar ca-current-candidate nil)
(defvar ca-description-window nil)
(defvar ca-highlight-parentheses-mode nil)


;; 1. do indent
;; 2. do yas expand if match exactly
;; 3. run ca2+ completion and enables it if not already active
(defun ca-smart-indent ()
  (interactive)
  ;; this shouldnt be started in minibuffer anyway, just in case
  (if (minibufferp)
      (minibuffer-complete)
    ;; workaround to make the loading order of ca2+ and yasnippet 
    ;; independent without the chance to get infinite recursion.
    (let ((tmp yas/fallback-behavior))
      (unless (if (not mark-active)
		  (let ((prev-point (point)))
		    (indent-for-tab-command)
		    (not (eql (point) prev-point)))
		(indent-region (region-beginning) (region-end)) t)
	(unless (and (fboundp 'yas/expand)
		     (setq yas/fallback-behavior 'return-nil)
		     (yas/expand))
	  (ca-begin)))
      (setq yas/fallback-behavior tmp))))

(defvar ca-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'ca-smart-indent)
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
    (define-key map [(C return)] 'ca-expand-and-continue)
    (define-key map [left] 'ca-expand-common)
    (define-key map "\M-h" 'ca-expand-common)
    (define-key map [right] 'ca-abort)
    (define-key map "\M-f" 'ca-abort)
    (define-key map [tab] 'ca-next-source)
    (define-key map [(C left)] 'backward-char)
    (define-key map [(C right)] 'forward-char)
    (define-key map [tab] 'ca-next-source)
    (define-key map [(C h)] 'ca-describe-candidate)
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



;; TODO: use property! ;;ca-expand-anything ca-start-showing ca-match-abbrev
(defconst ca-continue-commands
  '(ca-expand-common ca-expand-top ca-cycle
    ca-cycle-backwards universal-argument
    ca-expand-abbrev ca-next-source
    ca-expand-and-continue ca-describe-candidate)
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
	(setq ca-initial-prefix nil)
	(setq ca-current-candidate nil)
	(setq ca-description-window nil)
	(setq ca-highlight-parentheses-mode nil))
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


(defun ca-begin (&optional candidates source)
  (interactive)

  ;; workarounds
  (when (looking-at "$")
    (insert-string " ") (backward-char))
  (when (and (fboundp 'highlight-parentheses-mode)
	     highlight-parentheses-mode)
    (setq ca-highlight-parentheses-mode t)
    (highlight-parentheses-mode 0))


  (if (not candidates)
      (if (not source)
	  (ca-get-candidates)
	(setq ca-current-source source)
	(ca-get-candidates :current-source))
    (setq ca-current-source source)
    (setq ca-candidates candidates)
    (setq ca-all-candidates candidates)
    (setq ca-initial-prefix "")
    (setq ca-prefix ""))

  (if (null ca-candidates)
      (ca-abort)
    ;; TODO put all these in one alist. not sure if this is needed
    ;; but it seems to fix a bug with active completion and then 
    ;; changing buffers
    (set (make-local-variable 'ca-current-candidate)
	 ca-current-candidate)
    (set (make-local-variable 'ca-current-source)
	 ca-current-source)
    (set (make-local-variable 'ca-candidates)
	 ca-candidates)
    (set (make-local-variable 'ca-all-candidates)
	 ca-all-candidates)
    (set (make-local-variable 'ca-prefix)
	 ca-prefix)
    (set (make-local-variable 'ca-initial-prefix)
	 ca-initial-prefix)
    (set (make-local-variable 'ca-selection)
	 ca-selection)
    (set (make-local-variable 'ca-last-command-change)
	 ca-last-command-change)
    (set (make-local-variable 'ca-overlay)
	 ca-overlay)
    (set (make-local-variable 'ca-common-overlay)
	 ca-common-overlay)
    (set (make-local-variable 'ca-complete-word-on)
	 ca-complete-word-on)
    (set (make-local-variable 'ca-substring-match-on)
	 ca-substring-match-on)
    (set (make-local-variable 'ca-pseudo-tooltip-overlays)
	 ca-pseudo-tooltip-overlays)

    (ca-enable-active-keymap)
    (setq ca-common (try-completion "" ca-candidates))
    (setq ca-last-command-change (point))
    (setq ca-selection 0)
    ;; showing overlays is handled in post-command hook. this
    ;; here makes sure that company is not aborted.
    ;; (as it aborts on commands that are not in this list)
    (unless (memq this-command ca-continue-commands)
      (push this-command ca-continue-commands))
    (when candidates
      (ca-show-overlay)
      (ca-show-overlay-tips)))
  ca-candidates)


(defun ca-finish ()
  (interactive)
  (ca-disable-active-keymap)
  (setq ca-candidates nil)
  (setq ca-all-candidates nil)
  (setq ca-substring-match-on nil)
  (ca-hide-overlay)
  (ca-hide-pseudo-tooltip)
  (ca-kill-description-buffer)
  ;; workarounds
  (when (looking-at " $") (delete-char 1))
  (when ca-highlight-parentheses-mode
    (setq ca-highlight-parentheses-mode nil)
    (highlight-parentheses-mode 1))
  (let ((source ca-current-source))
    (setq ca-current-source nil)
    (ca-source-action source ca-current-candidate)))


(defun ca-abort ()
  (interactive)
  ;; strip prefix to first position of
  ;; ca-substring-match-delimiter
  (when (and ca-substring-match-on
	     (> (length ca-prefix) 0))
    (let ((end (string-match ca-substring-match-delimiter ca-prefix)))
      (when end
	(delete-region (1+ ca-last-command-change)
		       (1+ (- ca-last-command-change
			      (- (length ca-prefix)  end)))))))

  (setq ca-current-candidate nil)
  (setq ca-current-source nil)
  (ca-finish))


(defun ca-kill-description-buffer ()
  (when (and ca-description-window
	     (window-live-p ca-description-window))
    (let ((win (selected-window))
	  (point (point)))
      (select-window ca-description-window)
      (View-quit)
      (setq ca-description-window nil)
      (select-window win)
      (goto-char point))))


(defun ca-filter-words-push (word candidate list)
  (let ((cands (assoc word list)))
    (unless cands
      (setq cands (cons word nil))
      (push cands list))
    (push candidate (cdr cands)))
  list)


(defun ca-filter-words ()
  (if (or (not (ca-source-has-common-prefix))
	  (<= (length ca-candidates)
	      ca-how-many-candidates-to-show))
      (progn
	(setq ca-complete-word-on nil)
	candidates)
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


(defun ca-filter-candidates (&optional dont-filter-words)
  (let* ((candidates nil)
	 (prefix (concat "^" ca-prefix)))

    (if ca-substring-match-on 
	;; find substring matches, sort by min positions
	(let ((parts (split-string prefix ca-substring-match-delimiter))
	      cnt tmp it)
	  (dolist (cand ca-all-candidates)
	    (setq match nil)
	    (setq cnt 0)
	    (setq it parts)
	    (while it
	      (setq match (string-match (car it) 
					(ca-candidate-string cand)))
	      (if (not match)
		  (setq it nil)
		(setq cnt (+ cnt match))
		(setq it (cdr it))))
	    
	    (if match (push (cons cnt cand) tmp)))
	  (setq tmp (sort tmp '(lambda(a b) 
				 (< (car a) (car b)))))
	  (setq ca-candidates (mapcar 'cdr tmp)))
      ;; find matches for prefix
      (dolist (item ca-all-candidates)
	(when (string-match prefix (ca-candidate-string item))
	  (push item candidates)))
      (setq ca-candidates (nreverse candidates)))

    (ca-source-sort-by-occurrence)

    (unless (or dont-filter-words ca-substring-match-on)
      (ca-filter-words)))
  ca-candidates)


(defun ca-mode-pre-command ()
  (when ca-candidates
    (ca-hide-pseudo-tooltip)
    (ca-hide-overlay)
    (setq ca-last-command-change (point))))


(defun ca-post-command ()
  (when ca-candidates
    (cond
     ((memq this-command ca-continue-commands)
      (ca-continue))

     ;; char inserted
     ((eq (- (point) ca-last-command-change) 1)
      (when (looking-back ca-substring-match-delimiter)
	(setq ca-substring-match-on t))

      (setq ca-prefix (concat ca-prefix
		       (buffer-substring-no-properties
			ca-last-command-change (point))))

      (ca-filter-candidates)

      (cond
       ((null ca-candidates)
	(ca-abort))
       ((= (length ca-candidates) 1)
	(setq ca-current-candidate (car ca-candidates))
	(ca-continue))
       ;; XXX make this an option. i.e. automatically
       ;; expand only matching candidate
	;; (ca-insert-candidate (car ca-candidates))
	;; (if (ca-source-continue-after-expansion)
	;;     (ca-continue)
	;;   (ca-finish)))
       (t
	(setq ca-selection
	      (or (position-if
		   '(lambda (c) (equal c ca-current-candidate))
		   ca-candidates) 0))
	(ca-continue))))

     ;; char deleted
     ((and (> ca-last-command-change (point))
	   (>= (point) (- ca-last-command-change (length ca-prefix))))

      (setq ca-prefix (substring ca-prefix 0
		       (- (length ca-prefix)
			  (- ca-last-command-change (point)))))
      (cond
       ((or (>= (length ca-prefix)
		(length ca-initial-prefix))
	    (not (ca-source-is-filtered)))
	;; prefix is longer as the initial prefix for
	;; which ca-all-candidates were collected or
	;; source provided all possible candidates
	(ca-filter-candidates))
       
       ;; get new candidates
       ((> (length ca-prefix) 0)
	(ca-get-candidates)))
       
      (setq ca-selection 0)
      (ca-continue))

     ;; abort on other commands
     (t (ca-abort)))))


(defun ca-continue ()
  (if (>= ca-selection (length ca-candidates))
      (setq ca-selection 0))
  (setq ca-common (try-completion "" ca-candidates))
  (setq ca-current-candidate (nth ca-selection ca-candidates))
  (ca-show-overlay)
  (ca-show-overlay-tips))


;; TODO pass candidate?
(defun ca-source-action (source candidate)
  (let ((action (cdr-safe (assq 'action source))))
    (if action (funcall action candidate))))


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

(defun ca-source-init ()
  (let ((c (cdr-safe (assq 'init ca-current-source))))
    (if c
	(funcall c))))

(defun ca-source-check-limit ()
  (let ((limit (cdr-safe (assq 'limit ca-current-source))))
    (not (and limit (< (length ca-prefix) limit)))))


(defun ca-source-is-sorted ()
  (cdr-safe (assq 'sorted ca-current-source)))


(defun ca-source-is-filtered ()
  (not (cdr-safe (assq 'filter ca-current-source))))


(defun ca-source-separator ()
  (cdr-safe (assq 'separator ca-current-source)))


(defun ca-source-candidate-info (candidate)
  (let ((func (cdr-safe (assq 'info ca-current-source))))
    (if func
	(message "%s" (funcall func (cdr candidate)))
      (message "%s" (cdr candidate)))))


(defun ca-source-has-common-prefix ()
  (cdr-safe (assq 'common-prefix ca-current-source)))


(defun ca-source-continue-after-expansion ()
  (let ((cont (cdr-safe (assq 'continue ca-current-source))))
    (cond
     ((fboundp cont)
      (let ((candidates (funcall cont ca-current-candidate)))
	(when candidates
	  (setq ca-prefix "")
	  (setq ca-initial-prefix "")
	  (setq ca-common "")
	  (setq ca-selection 0)
	  (setq ca-candidates candidates)
	  (setq ca-all-candidates candidates))))

     (cont ;; TODO remove this!
      (let ((prefix ca-prefix))
	(ca-get-candidates)
	;; test if new candidates were found
	(not (and (= (length ca-candidates) 1)
		  (string-equal prefix (ca-candidate-string-nth 0)))))))))


;; check wheter the candidate list is made of cons pairs
(defsubst ca-cons-candidates (cands)
  (consp (car-safe cands)))


(defun ca-sort-candidates (cands)
  (if (ca-cons-candidates cands)
      (sort* cands
	     '(lambda (c1 c2) (string< (car c1) (car c2))))
    (sort* cands 'string<)))


(defun ca-source-sort-by-occurrence ()
  (if (cdr-safe (assq 'sort-by-occurrence ca-current-source))
      (let ((around (ca-words-in-buffer))
	    (len (length ca-candidates)))
	(dolist (word around)
	  (setq ca-candidates (delete word ca-candidates))
	  (if (not (eq len (length ca-candidates)))
	      (push word ca-candidates))))))


(defun ca-get-candidates (&optional next)
  (let* ((candidates nil)
	 (sources (append
		   (cdr (assoc major-mode ca-source-alist))
		   (cdr (assoc 'otherwise ca-source-alist))))
	 (sources (delq nil sources))) ;; XXX needed?

    ;; cycle to next source
    (if (and (eq next :next-source) sources ca-current-source)
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
	;;(message "try %s" (cdr-safe (assq 'name source)))
	(setq ca-current-source source)
        (ca-source-init)
	(when (ca-grab-prefix)
	  ;; check min prefix length
	  (when (ca-source-check-limit)
	    ;; get candidates
	    (setq candidates (ca-source-candidates))
	    ;; sort candidates
	    (unless (ca-source-is-sorted)
	      (setq candidates (ca-sort-candidates candidates)))

	    ;; keep a full list of candidates
	    (setq ca-all-candidates (copy-list candidates))

	    ;; filter candidates by prefix
	    (ca-filter-candidates)
	    (setq candidates ca-candidates)))
	(if (eq next :current-source)
	    (setq sources nil)
	  (setq sources (cdr sources)))))

    (when candidates
      (message "%s candidates" (cdr-safe (assq 'name ca-current-source)))

      (setq ca-initial-prefix ca-prefix)
      (setq ca-substring-match-on nil) ;; set this in 'cycle and 'begin?

      (setq ca-current-candidate (car ca-candidates)))

    candidates))


(defun ca-grab-prefix (&optional thing)
  (setq start (ca-source-decider))
  (setq ca-prefix
	(and start (buffer-substring-no-properties start (point)))))


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
  (ca-kill-description-buffer)
  (setq ca-candidates nil)
  (setq ca-all-candidates nil)
  (setq ca-current-candidate nil)
  (setq ca-selection 0)
  (ca-get-candidates :next-source))


(defun ca-expand-number (n)
  "Expand the Nth candidate."
  (interactive "P")
  (unless ca-mode (error "ca-mode not enabled"))
  (unless ca-candidates
    (ca-begin))
  (if (not ca-candidates)
      (message "No candidates found")
    (let* ((cand (nth (1- n) ca-candidates)))
      (if (not cand)
	  (error "No such candidate")
	(ca-insert-candidate cand))
      cand)))


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
	(when (consp cand)
	  (ca-source-candidate-info cand)))))


(defun ca-cycle-backwards (&optional n)
  (interactive)
  (ca-cycle (- (or n 1)))
  (setq this-command 'ca-cycle))


(defun ca-expand-top ()
  (interactive)
  (ca-insert-candidate ca-current-candidate)
  (setq ca-prefix (ca-candidate-string ca-current-candidate))
  (if (not ca-complete-word-on)
      (ca-finish)
    (ca-filter-candidates)
    (if (< (length ca-candidates) 2)
	(ca-finish))))


(defun ca-expand-and-continue ()
  (interactive)
  (ca-insert-candidate ca-current-candidate)
  (setq ca-prefix (ca-candidate-string ca-current-candidate))
  (if (not ca-complete-word-on)
      (or (ca-source-continue-after-expansion)
	  (ca-finish))
    (ca-filter-candidates)
    (if (< (length ca-candidates) 2)
	(or (ca-source-continue-after-expansion)
	    (ca-finish)))))


(defun ca-expand-common ()
  (interactive)
  (unless ca-mode (error "ca-mode not enabled"))
  (if (null ca-candidates)
      (ca-begin)
    (if ca-candidates
	(let ((common ca-common))
	  (if (= (length ca-prefix) (length common))
	      ;; if there is no common part to expand
	      (let* ((current (ca-candidate-string ca-current-candidate))
		     (delim (string-match "\\W" current (length ca-prefix))))
		(if delim
		    ;; expand to the next word delimiter
		    (setq common (substring  current 0 (1+ delim)))
		  ;; or expand the current candidate
		  (setq common current))))

	  (ca-insert-candidate common)
	  (setq ca-prefix common)
	  (ca-filter-candidates)

	  (if (> (length ca-candidates) 1)
	      ;; continue,
	      (setq ca-selection
		    (or (position-if
			 '(lambda (c) (equal c ca-current-candidate))
			 ca-candidates) 0))
	    ;; expand last left candidate  and finish
	    (ca-insert-candidate (car ca-candidates))
	    (ca-finish)))

      (when (called-interactively-p)
	(error "No candidates found")))))


(defun ca-describe-candidate ()
  (interactive)
  (unless ca-mode (error "ca-mode not enabled"))
  (if (and ca-current-candidate
	   ca-current-source)
      (let ((func (cdr-safe (assq 'describe ca-current-source)))
	    (win (selected-window)))
        (when func
	  (funcall func ca-current-candidate)
	  (when (not (eq win (selected-window)))
	    (setq ca-description-window (selected-window))
	    (select-window win))))))




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


(defun ca-show-overlay ()
  (if ca-overlay
      (ca-hide-overlay))
  (let* ((candidate (if ca-substring-match-on
			ca-current-candidate
		      (ca-chop ca-current-candidate)))
	 (prefix-length (length ca-prefix))
	 (common-length (length ca-common))
	 (beg (point)))
    (ca-without-undo
     (save-excursion
       (if ca-substring-match-on (insert " ["))
       (insert (ca-candidate-string candidate))
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
  (let* ((max-lines (- (window-height) 4))
	 (max-lines (if (< max-lines 0) 1 max-lines))
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
    (if (<= lines-to-bottom (length lines))
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

(defun ca-add-source-1 (source mode)
  "Add a completion source for `ca-mode'."
  (let ((sources (assoc mode ca-source-alist)))
    (unless sources
      (setq sources (cons mode nil))
      (push sources ca-source-alist))
    (push source (cdr sources))
    source))

(defun ca-add-source (source modes)
  "Add a completion source."
  (if (consp modes)
      (dolist (mode modes)
	(ca-add-source-1 source mode))
    (ca-add-source-1 source modes)))


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
      (let* ((i 0)
	    (ac-point (point))
	    (ac-limit 15)
            candidate
            candidates
	    (end (string-match ca-substring-match-delimiter ca-prefix))
	    (prefix (if end (substring ca-prefix 0 end) ca-prefix))
            (regexp (concat "\\b" (regexp-quote prefix)
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
	(delq (symbol-at-point) candidates))))

(provide 'ca2+)
