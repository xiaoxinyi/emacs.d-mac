;Load package.el
(require 'package)

;Add melpa to list of repositories
(setq package-archives
      '(("org"       . "http://orgmode.org/elpa/")
        ("gnu"       . "http://elpa.gnu.org/packages/")
        ("melpa"     . "http://melpa.milkbox.net/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")))


;Initialize package.el
(package-initialize)


;; (require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
;; (cask-initialize)
;; (require 'pallet)
