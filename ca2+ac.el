;; wrapper generators to use ac-sources in ca2+ and vice versa

;; TODO after invocation of funcs one might need to update 
;; state according ac- variables

(defmacro ca-ac-source-candidates-macro (func)
  ""
  `(let ((realfunc (cdr ,func)))
     `(lambda (prefix) 
	(let ((ac-prefix prefix)
	      (ac-target prefix) ;; old ac-sources
	      (ac-point (point)))
	  (,realfunc)))))

(defmacro ca-ac-source-action-macro (func)
  ""
  `(let ((realfunc (cdr ,func)))
     `(lambda (candidate) 
	(let ((ac-candidates ca-candidates)
	      (ac-selection ca-selection)
	      (ac-prefix ca-prefix)
	      (ac-target prefix)  ;; old ac-sources
	      (ac-point (point))) ;; XXX start of prefix?
	  (,realfunc)))))

(defmacro ca-ac-source-init-macro (func)
  ""
  `(let ((realfunc (cdr ,func)))
     `(lambda () 
	(,realfunc))))


(defun ca-ac-convert-source (ac-source)
  (let* ((ca-source nil)
	(source (symbol-value ac-source))
	(ac-candidates (assq 'candidates source))
	(ac-action (assq 'action source))
	(ac-limit (assq 'limit source))
	(ac-init (assq 'init source))
	(ac-name (documentation-property ac-source
				      'variable-documentation t)))
    (when ac-name
      (push (cons 'name ac-name) 
    	  ca-source))
    (when ac-limit
      (push (cons 'limit (cdr ac-limit))
	    ca-source))
    (when ac-candidates
      (push (cons 'candidates (ca-ac-source-candidates-macro ac-candidates))
    	    ca-source))
    (when ac-action
       (push (cons 'action (ca-ac-source-action-macro ac-action))
     	    ca-source))
    (when ac-init
      (push (cons 'init (ca-ac-source-init-macro ac-init))
    	    ca-source))
    
    ca-source))


(defun ca-add-ac-source (ac-source modes)
  "Add an auto-complete completion source."
  (let ((source (ca-ac-convert-source ac-source)))
    (if (consp modes)
	(dolist (mode modes)
	  (ca-add-source-1 source mode))
      (ca-add-source-1 source modes))))


(provide 'ca2+ac)


;; (defmacro ac-ca-source-candidates-macro (func)
;;   ""
;;   `(let ((realfunc (cdr ,func)))
;;      `(lambda () 
;; 	(,realfunc ac-prefix))))

;; (defmacro ac-ca-source-action-macro (func)
;;   ""
;;   `(let ((realfunc (cdr ,func)))
;;      `(lambda (candidate) 
;; 	(,realfunc (nth ac-selection ac-candidates)))))

;; (defmacro ac-ca-source-init-macro (func)
;;   ""
;;   `(let ((realfunc (cdr ,func)))
;;      `(lambda () 
;; 	(,realfunc))))


;; (defun ac-ca-convert-source (ca-source)
;;   (let* ((ac-source nil)
;; 	 (source (symbol-value ca-source))
;; 	 (ca-candidates (assq 'candidates source))
;; 	 (ca-action (assq 'continue source))
;; 	 (ca-limit (assq 'limit source))
;; 	 (ca-decider (assq 'decider source)))
;;     (when ca-limit
;;       (push (cons 'limit (cdr ca-limit))
;; 	    ac-source))
;;     (when ca-candidates
;;       (push (cons 'candidates (ac-ca-source-candidates-macro ca-candidates))
;; 	    ac-source))
;;     (when ca-action
;;       (push (cons 'action (ac-ca-source-action-macro ca-action))
;; 	    ac-source))
;;     ;; some ca-sources require the decider function to be run
;;     ;; maybe i should split this into init and decider
;;     (when ca-decider
;;       (push (cons 'init (ac-ca-source-init-macro ca-decider))
;; 	    ac-source))
;;     ac-source))

;; (ac-ca-convert-source 'ca-source-semantic-context)
;; (setq ac-ca-source-semantic (ac-ca-convert-source 'ca-source-semantic-context))
;; (dolist (hook '(c-mode-hook 
;; 		c++-mode-hook))
;;   (add-hook 
;;    hook 
;;    '(lambda()
;;       (auto-complete-mode 1)
;;       (set (make-local-variable 'ac-sources)
;; 	   '(ac-ca-source-semantic)))))

;; (ca-clear-completion-sources)
;; (ca-add-ac-source 'ac-source-yasnippet 'emacs-lisp-mode)
;; (ca-add-ac-source 'ac-source-symbols 'emacs-lisp-mode)
;; (ca-add-ac-source 'ac-source-files-in-current-dir 'emacs-lisp-mode)
;; (ca-add-ac-source 'ac-source-filename 'emacs-lisp-mode)

