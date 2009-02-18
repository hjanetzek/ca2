;; based on smart-tab

(defun magic-tab (prefix)
  (interactive "P")
  (if (minibufferp)
      (minibuffer-complete)
    (or (smart-indent)
	(yas/expand)
	(if (and (boundp ca-mode) ca-mode)
	    (ca-begin)
	  (dabbrev-expand 1)))))


(defun smart-indent ()
  "Indents region if mark is active, or current line otherwise."
  (interactive)
  (cond (mark-active 
	 (indent-region (region-beginning) (region-end))
	 t)
	(t 
	 (let ((prev-point (point)))
	   (indent-for-tab-command)
	   (not (eql (point) prev-point))))))


(global-set-key [(tab)] 'magic-tab)
	       
(provide 'magic-tab)

