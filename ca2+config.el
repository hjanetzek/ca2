
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; install sources ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ca-clear-completion-sources)

;; mode sources
;; sources are pushed on the list: load lower priority sources first
(ca-add-completion-source ca-gtags-source
			  '(c++-mode c-mode java-mode))

(ca-add-completion-source ca-lisp-source 
			  '(emacs-lisp-mode 
			    lisp-interaction-mode))

(ca-add-completion-source ca-yasnippet-source 
			  '(emacs-lisp-mode 
			    lisp-interaction-mode
			    c++-mode c-mode java-mode))

;; general sources
(ca-add-completion-source ca-filename-source
			  'otherwise)

(ca-add-completion-source ca-dabbrev-source
			  'otherwise)

(global-ca-mode 1)

(provide 'ca2+config)

