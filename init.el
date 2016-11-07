
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(load-file "~/.emacs.d/super-emacs/repositories.el")
(load-file "~/.emacs.d/super-emacs/packages.el")
(load-file "~/.emacs.d/super-emacs/interface.el")
(load-file "~/.emacs.d/super-emacs/misc.el")
(load-file "~/.emacs.d/super-emacs/key-bindings.el")


(setq org-agenda-files (list "~/Desktop/org-mode/"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(flymake-google-cpplint-command "/usr/local/bin/cpplint")
 '(hl-sexp-background-color "#1c1f26")
 '(package-selected-packages
   (quote
    (macrostep smart-mode-line solarized solarized-dark auto-complete helm ztree ws-butler use-package undo-tree theme-looper switch-window stickyfunc-enhance solarized-theme smex smartparens py-autopep8 powerline pallet ox-twbs ox-gfm org-pandoc org-bullets org ob-ipython myterminal-controls multiple-cursors multi-term monokai-theme meta-presenter material-theme markdown-mode magit lua-mode irony impatient-mode iedit ido-vertical-mode ido-ubiquitous helm-themes helm-projectile helm-ag graphviz-dot-mode google-c-style github-theme git-gutter+ ggtags flymake-google-cpplint flymake-cursor flycheck flx-isearch flx-ido exec-path-from-shell etags-select eproject elpy ein direx dired-open dired+ cuda-mode ctags-update command-log-mode color-theme-solarized color-theme-monokai clean-aindent-mode cask-mode camcorder buffer-move bookmark+ auto-complete-c-headers anaconda-mode ag ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
