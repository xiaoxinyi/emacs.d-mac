;Load package.el
(require 'package)

;Add melpa to list of repositories
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") 
         t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

;Initialize package.el
(package-initialize)


;; (require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
;; (cask-initialize)
;; (require 'pallet)
