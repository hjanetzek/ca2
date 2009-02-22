
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


(eval-after-load 'yasnippet
  '(progn
     ;; disable yas/trigger key - we call you!!!
     (setq yas/trigger-key (kbd "C-c <kp-multiply>"))
     ;; change this to your liking, but it tab would interfer 
     ;; with completion within yas templates.
     (setq yas/next-field-key (kbd "C-f"))
     (setq yas/prev-field-key (kbd "C-b"))

     ;; expand only if word matches template name,
     ;; otherwise ca2+ shows possible completions
     (setq yas/fallback-behaviour 'return-nil)

     (ca-add-completion-source ca-source-yasnippet
			       '(emacs-lisp-mode 
				 lisp-interaction-mode
				 c++-mode c-mode java-mode 
				 'otherwise))))

(eval-after-load 'semantic
  '(progn 
     (require 'semantic-ia)
     ;; experimental, but faster version. 
     ;; you need to run ca-semantic-clear-cache 
     ;; to update cached tags tables
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

