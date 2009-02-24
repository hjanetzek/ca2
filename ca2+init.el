
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; install sources ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ca2+)
(require 'ca2+sources)


;; Options what tab should preferably do.
;; just uncomment the preffered method:

;; default: tab cycles to next source
;;(define-key ca-active-map [tab] 'ca-next-source)

;; tab cycles to next candidate
;;(define-key ca-active-map [tab] 'ca-cycle)

;; tab expands current candidate
;;(define-key ca-active-map [tab] 'ca-expand-top)

;; tab expands common part, after that it expands the 
;; current candidate to the next word boundary 
;; when only one candidate is left tab will insert 
;; that candidate 
;; (define-key ca-active-map [tab] 'ca-expand-common)

;; when rebinding tab you'll need another binding for 
;; cycling sources
;; (define-key ca-active-map "\M-h" 'ca-next-source)


(ca-clear-completion-sources)

;; mode sources
;; sources are pushed on the list: load lower priority sources first

(ca-add-completion-source ca-source-lisp
			  '(emacs-lisp-mode 
			    lisp-interaction-mode))


(eval-after-load 'gtags
  '(progn 
     (ca-add-completion-source ca-source-gtags
			       '(c++-mode c-mode java-mode))))


;; it seems that this needs to be set before '(require 'yasnippet)'
;; change this to your liking, but tab would interfer with 
;; completion within yas templates.
(defvar yas/next-field-key (kbd "C-f"))
(defvar yas/prev-field-key (kbd "C-b"))

(eval-after-load 'yasnippet
  '(progn
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

     (ca-add-completion-source ca-source-semantic-tags
				'(c++-mode c-mode java-mode))
     (ca-add-completion-source ca-source-semantic-context
			       '(c++-mode c-mode java-mode))
     (ca-add-completion-source ca-source-semantic-yas-arguments
			       '(c++-mode c-mode java-mode))

     (defun ca-semantic-completion (arg)
       (interactive "p")
       (self-insert-command arg)
       (when (and (= arg 1))
	 (ca-begin nil ca-source-semantic-context)))

     (defun ca-semantic-c-hook ()
       (local-set-key "." 'ca-semantic-completion)
       (local-set-key ">" 'ca-semantic-completion))

     (add-hook 'c-mode-common-hook 'ca-semantic-c-hook)))



;; general sources
(ca-add-completion-source ca-source-filename
			  'otherwise)

(ca-add-completion-source ca-source-dabbrev
			  'otherwise)



(global-ca-mode 1)

(provide 'ca2+init) 

