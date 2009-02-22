(load-file "~/elisp/cedet/common/cedet.el")
(require 'semantic-gcc)
(require 'semantic-ia)
;;(require 'pulse)
(semantic-load-enable-minimum-features)
(global-ede-mode 1)
(global-semantic-mru-bookmark-mode 1)
(global-senator-minor-mode 1)

;;(semantic-load-enable-all-exuberent-ctags-support)

(provide 'my-semantic)