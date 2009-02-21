
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; install sources ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ca-clear-completion-sources)

;; mode sources
;; sources are pushed on the list: load lower priority sources first
(eval-after-load 'gtags
  '(progn 
     (ca-add-completion-source ca-source-gtags
			       '(c++-mode c-mode java-mode))))

(ca-add-completion-source ca-source-lisp
			  '(emacs-lisp-mode 
			    lisp-interaction-mode))

;; TODO how to get modes from yasnippet?
(ca-add-completion-source ca-source-yasnippet
			  '(emacs-lisp-mode 
			    lisp-interaction-mode
			    c++-mode c-mode java-mode))

(eval-after-load 'semantic
  '(progn 
     (require 'semantic-ia)
     (require 'ca2+semantic)
     (ca-add-completion-source ca-source-semantic-context
			       '(c++-mode c-mode java-mode))
     (ca-add-completion-source ca-source-semantic-tags
			       '(c++-mode c-mode java-mode))
     (ca-add-completion-source ca-source-semantic-yas-arguments
			       '(c++-mode c-mode java-mode))))

;; general sources
(ca-add-completion-source ca-source-filename
			  'otherwise)

(ca-add-completion-source ca-source-dabbrev
			  'otherwise)

;; tab cycles to next candidate
;;(define-key ca-active-map [tab] 'ca-cycle)

;; tab expands current candidate
;;(define-key ca-active-map [tab] 'ca-expand-top)

(global-ca-mode 1)

(provide 'ca2+config)

