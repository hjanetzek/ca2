;; based on smart-tab


;; 1. do indent
;; 2. do yas expand if match exactly. this needs:
(setq yas/fallback-behaviour 'return-nil)
;; 3. run ca2+ completion and enables it if not already active 

(defun magic-tab (prefix)
  (interactive "P")
  (if (minibufferp)
      (minibuffer-complete)
    (or (smart-indent)
	(yas/expand)
	(if (and (boundp ca-mode)
		 (or ca-mode (ca-mode 1)))
	    (ca-begin)
	  (dabbrev-expand 1)))))


(defun smart-indent ()
  "Indents region if mark is active, or current line otherwise."
  (interactive)
  (if (not mark-active)
      (let ((prev-point (point)))
	(indent-for-tab-command)
	(not (eql (point) prev-point)))
    (indent-region (region-beginning) (region-end)) t))


(global-set-key [(tab)] 'magic-tab)
	       
(provide 'magic-tab)

