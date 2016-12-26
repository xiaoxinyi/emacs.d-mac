;; (unbind-key "<tab>")
;; (unbind-key "<backtab>")
;; (bind-key "C-<return>" 'yas-expand yas-minor-mode-map)

;; Create repositories cache,if required
(when (not package-archive-contents)
  (package-refresh-contents))

;; Declare a list of required packages
(defvar super-emacs--required-packages
  '())

;; Install required packages
(mapc (lambda (p)
        (package-install p))
      super-emacs--required-packages)

;; yasnippet config
(use-package yasnippet
  :bind (("<backtab>" . yas-expand))
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs
    '("~/.emacs.d/snippets"))
  )

;; Load default auto-complete configs
(use-package auto-complete
  :config
  ;; auto complete mode
  ;; should be loaded after yasnippet so that they can work together
  (require 'auto-complete-config)
  ;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (ac-config-default)
  ;; set the trigger key so that it can work together with yasnippet on tab key,
  ;; if the word exists in yasnippet, pressing tab will cause yasnippet to
  ;; activate, otherwise, auto-complete will
  ;; (ac-set-trigger-key "TAB")
  ;; (ac-set-trigger-key "<tab>")

)


(use-package multiple-cursors
  :ensure t
  :bind*
  (("C-c m t" . mc/mark-all-like-this)
   ("C-c m m" . mc/mark-all-like-this-dwim)
   ("C-c m l" . mc/edit-lines)
   ("C-c m e" . mc/edit-ends-of-lines)
   ("C-c m a" . mc/edit-beginnings-of-lines)
   ("C-c m n" . mc/mark-next-like-this)
   ("C-c m p" . mc/mark-previous-like-this)
   ("C-c m s" . mc/mark-sgml-tag-pair)
   ("C-c m d" . mc/mark-all-like-this-in-defun)
   ("C-c m k" . mc/skip-to-next-like-this)
   ("C-c m j" . mc/skip-to-previous-like-this)))

(use-package visual-regexp-steroids
  :ensure t
  :init (use-package visual-regexp
          :ensure t)
  :bind* (
          ("C-c r" . vr/replace)
          ("C-c q" . vr/query-replace)
          ("C-c v m" . vr/mc-mark)
          ("C-M-s" . vr/isearch-forward)
          ("C-M-r" . vr/isearch-backward))

  )

;; used in multi-cursor for search
(use-package phi-search
  :ensure t
  :config (set-face-attribute 'phi-search-selection-face nil
                              :background "orange")
  )

(use-package phi-search-mc
  :disabled t
  :ensure t
  :config (phi-search-mc/setup-keys))

(use-package mc-extras
  :ensure t
  :config (define-key mc/keymap (kbd "C-. =") 'mc/compare-chars))

(use-package switch-window
  :ensure t
  :bind ("C-<tab>" . switch-window))

(use-package buffer-move
  :ensure t
  :bind
  ("C-s-<up>" . buf-move-up)
  ("C-s-<down>" . buf-move-down)
  ("C-s-<left>" . buf-move-left)
  ("C-s-<right>" . buf-move-right))

(use-package ztree
  :ensure t)


(use-package meta-presenter
  :ensure t)


;; Start undo-tree
(use-package undo-tree
  :bind (("M-/" . undo-tree-visualize))
  :config
  (global-undo-tree-mode))


(use-package ace-jump-mode
  :commands ace-jump-mode
  :bind* (("M-h" . ace-jump-mode))
  :config (setq ace-jump-mode-submode-list
                '(ace-jump-char-mode
                  ace-jump-word-mode
                  ace-jump-line-mode))
  )


;; Enable powerline
(use-package powerline
  :config
  (powerline-center-theme)
  ;; (setq powerline-default-separator 'utf-8)
  (setq powerline-default-separator 'wave))

(use-package color-theme
  :ensure t)

 (use-package color-theme-solarized
  :ensure t
  :if window-system
  :config
  (setq solarized-use-variable-pitch nil)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0)
  (setq solarized-use-less-bold t)
  (setq solarized-termcolors 256)
  (setq solarized-high-contrast-mode-line t)
  (load-theme 'solarized-dark t)
)

;; Configure theme-looper
(use-package theme-looper
  :bind (("C-\"" . theme-looper-enable-next-theme))
  :config
  (theme-looper-set-theme-set '(;;deeper-blue
                                darcula
                                zenburn
                                ;;wheatgrass
(use-pa ox-md)(use-pa ox-md)                                ;;monokai
                                solarized-dark))
  (theme-looper-set-customizations 'powerline-reset))


;; Configure myterminal-controls
(use-package myterminal-controls
  :commands myterminal-controls-open-controls
  :bind (("C-M-'" . myterminal-controls-open-controls))
  :config
  (myterminal-controls-set-controls-data
   '(("t" "Change color theme" theme-looper-enable-next-theme)
     ("r" "Reload file" super-emacs-reload-current-file))))

;; Set up helm-mode
;; (require 'helm)
;; (require 'helm-config)
;; (require 'helm-ag nil t)
(use-package helm
  :init
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-recentf)
         ("M-y" . helm-show-kill-ring)
         ("C-c h" . helm-command-prefix)
         :map helm-map
         ;;("<tab>" . helm-execute-persistent-action)
         ("C-j" .  helm-execute-persistent-action)
         ("C-z" . helm-select-action))
  :config
  (helm-mode 1)
  (helm-autoresize-mode t)
  (setq helm-split-window-in-side-p t
        ;;helm-move-to-line-cycle-in-source t
        helm-ff-file-name-history-use-recentf t
        helm-ff-search-library-in-sexp t)
  (use-package helm-ag
    :ensure t)
  (require 'helm-config))

;; switch window shortcut
(use-package   window-numbering
  :ensure t
  :bind (
         ("s-0" .  select-window-0)
         ("s-1" .  select-window-1)
         ("s-2" .  select-window-2)
         ("s-3" .  select-window-3)
         ("s-4" .  select-window-4)
         ("s-5" .  select-window-5)
         ("s-6" .  select-window-6)
         ("s-7" .  select-window-7)
         ("s-8" .  select-window-8)
         ("s-9" .  select-window-9)
         )
  :config
  (window-numbering-mode 1)
  (setq window-numbering-assign-func
        (lambda () (when (equal (buffer-name) "*Calculator*") 9)))
  )



;; multi term
(use-package multi-term
  :bind (("C-x t" . multi-term))
  :config
  (require 'multi-term)
  (setq multi-term-program "/bin/zsh")
  ;; set PS1
  (setq system-uses-terminfo nil)
  ;; tab complete
  (add-hook 'term-mode-hook
            (lambda()
              (setq yas-dont-activate t))))


(use-package org-bullets
  :ensure t)


;; execute in emacs
(setenv "PATH"
        (concat (getenv "PATH")
                ":" "/usr/local/bin"))
(setenv "PATH"
        (concat (getenv "PATH")
                ":" "/Library/TeX/texbin/"))

(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/Library/TeX/texbin/")))


;; org-mode
(use-package org
  :ensure t
  :init
  (defun my-org-mode-hook ()
    (progn
      (auto-fill-mode 1)))
  :bind (:map org-mode-map
         ("C-x n m"  . zl/org-narrow-forward)
         ("C-x n p"  . zl/org-narrow-backward)
         ("C-c C-x C-s" . mark-done-and-archive)
         ("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)

         )
  :config
  (add-hook 'org-mode-hook 'my-org-mode-hook)
  (add-hook 'org-mode-hook
          (lambda ()
            (org-bullets-mode t)))

  (setq org-ellipsis "⤵")

  ;; show utf-8
  (setq org-pretty-entities t)

  (setq org-src-fontify-natively t)

  (setq org-src-tab-acts-natively t)

  (setq org-src-window-setup 'current-window)

  ;; export html get rid of footer
  (setq org-html-postamble nil)

  (setq org-html-toc nil)

  ;; org-mode  export pdf
  ;; (setq org-latex-pdf-process
  ;;       '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
  ;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
  ;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (setq org-latex-to-pdf-process
        '("xelatex -shell-escape -file-line-error -interaction=nonstopmode  -synctex=1 -output-directory %o %f"
          "xelatex -shell-escape -file-line-error -interaction=nonstopmode  -synctex=1 -output-directory %o %f"
          "xelatex -shell-escape -file-line-error -interaction=nonstopmode  -synctex=1 -output-directory %o %f"))

  ;; add minted package highlight source code
  (require 'ox-latex)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-listings 'minted)

  ;; org latex preview inline
;;  eamer  (setq org-latex-create-formula-image-program 'imagemagick)
  ;; scale image inline default set to 1.5
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

  ;; org-narrow-forward
  (defun zl/org-narrow-forward ()
    "Move to the next subtree at same level, and narrow to it."
    (interactive)
    (widen)
    (org-forward-heading-same-level 1)
    (org-narrow-to-subtree))

  (defun zl/org-narrow-backward ()
    "Move to the previous subtree at same level, and narrow to it."
    (interactive)
    (widen)
    (org-backward-heading-same-level 1)
    (org-narrow-to-subtree))

  ;; Tex config
  (setq TeX-parse-self t)

  (setq TeX-PDF-mode t)

  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (LaTeX-math-mode)
              (setq TeX-master t)))

  (defun mark-done-and-archive ()
    "Mark the state of an org-mode item as DONE and archive it."
    (interactive)
    (org-todo 'done)
    (org-archive-subtree))

  (setq org-export-with-smart-quotes t)

  (setq org-log-done 'time)

  ;; babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (ruby . t)
     (python . t)
     (sh . t)
     (C . t)
     (lua . t )
     (dot . t)))

  (setq org-confirm-babel-evaluate nil)
  )


(require 'ox-md)
(require 'ox-beamer)
(use-package ox-twbs
  :ensure t)
(use-package ox-gfm
  :ensure t)

;; dired config
(require 'dired-x)
(require 'dired+)
(require 'dired-open)

(setq dired-open-extensions
      '(
        ("pdf" . "preview")
        ("md" . "emacs")
        )
      )

(setq-default dired-listing-switches "-lha")

(setq dired-clean-up-buffers-too t)

(setq dired-recursive-copies 'always)

(setq dired-recursive-deletes 'top)


;; ido
(use-package ido
  :ensure t
  :init  (setq ido-enable-flex-matching t
               ido-ignore-extensions t
               ido-use-virtual-buffers t
               ido-everywhere t)
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (add-to-list 'completion-ignored-extensions ".pyc")
  ;; don't confirm to create new buffers
  (setq ido-create-new-buffer 'always))

(use-package flx-ido
  :ensure t
  :init (setq ido-enable-flex-matching t)
  :config (flx-ido-mode 1))

(use-package ido-vertical-mode
  :ensure t
  :init               ;; I like up and down arrow keys:
  (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
  :config
  (ido-vertical-mode 1))

(use-package ido-ubiquitous
  :ensure t
  :config
  (ido-ubiquitous))

;; smex
;; (smex-initialize)

;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)

(use-package flycheck
  :ensure t
  :config
  ;; global flycheck enabled
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))

  (add-hook 'c++-mode-hook
            (lambda () (setq flycheck-clang-include-path
                         (list (expand-file-name "~/Desktop/DeepAR_Algorithm/include")))))
  (add-hook 'c-mode-hook
            (lambda () (setq flycheck-clang-include-path
                         (list (expand-file-name "~/Desktop/DeepAR_Algorithm/include")))))
  )


;; auto-complete-c-header
(use-package auto-complete-c-headers
  :ensure t
  :config
  ;; c header complete
  (defun zl/ac-complete-c-header-init ()
    (require 'auto-complete-c-headers)

    (add-to-list 'ac-sources 'ac-source-c-headers)

    (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.11.sdk/usr/include/c++/4.2.1")

    (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.11.sdk/usr/include"))

  (add-hook 'c++-mode-hook 'zl/ac-complete-c-header-init)
  (add-hook 'c-mode-hook 'zl/ac-complete-c-header-init))

;; iedit
(use-package iedit
  :ensure t
  :bind (("C-c o" . iedit-mode)))


;; flymake google cpp
(use-package flymake-google-cpplint
  :ensure t
  :config
  (defun zl/flymake-google-init ()
    (require 'flymake-google-cpplint)
    (custom-set-variables
     '(flymake-google-cpplint-command "/usr/local/bin/cpplint"))
    (flymake-google-cpplint-load))
  (add-hook 'c-mode-hook 'zl/flymake-google-init)
  (add-hook 'c++-mode-hook 'zl/flymake-google-init)
)


;; google-c-style
(use-package google-c-style
  :ensure t
  :config
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent)
  )


;; cedet
(use-package cedet)

;; stickfunc-enhance
(use-package stickyfunc-enhance
  :ensure t)

;; semantic
(use-package semantic
  :init
  ;; add senmatic as a suggestion backend to auto complete
  (defun zl/add-senmantic-to-autocompelte ()
    (add-to-list 'ac-sources 'ac-source-semantic)
    (add-to-list 'ac-sources 'ac-source-gtags)
    )
  :bind (:map
         semantic-mode-map
         ("\C-c d" . semantic-ia-show-doc)
         ("\C-c c" . semantic-ia-describe-class)
         ("\C-c s" . semantic-ia-show-summary)
         ("\C-c >" . semantic-ia-fast-jump)
         ("\C-c -" . senator-fold-tag)
         ("\C-c +" . senator-unfold-tag))

  :config
  (semantic-mode 1)
  (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)

  (add-hook 'c-mode-common-hook 'zl/add-senmantic-to-autocompelte)
  (add-hook 'c-mode-common-hook 'global-semantic-mru-bookmark-mode)

  (global-semantic-idle-scheduler-mode 1)
  
  (semantic-add-system-include "/usr/local/Cellar/gsl/1.16/include" 'c++-mode)
  
  (defun zl/semantic-hook ()
    (imenu-add-to-menubar "TAGS"))
  (add-hook 'semantic-init-hook 'zl/semantic-hook)
  )


;; ede-mode
(use-package ede
  :config
  (global-ede-mode 1)
  (ede-cpp-root-project "my project" :file "~/Desktop/DeepAR_Algorithm/include/SXAR/ARWrapper/Marker.h"
                      :include-path '("../../"))
  )

;; Package: smartparens
(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode +1)
  (smartparens-global-mode 1)

  ;; when you press RET, the curly braces automatically
  ;; add another newline
  (sp-with-modes '(c-mode c++-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                              ("* ||\n[i]" "RET"))))
  )


;; Package: clean-aindent-mode
(use-package clean-aindent-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'clean-aindent-mode))

;; Package: ws-butler
(use-package ws-butler
  :ensure t
  :config
  (add-hook 'c-mode-common-hook 'ws-butler-mode)
  (add-hook 'prog-mode-hook 'ws-butler-mode)
  )


;; elpy
(use-package elpy
  :ensure t
  :config
  (require 'elpy)
  (elpy-enable)
  ;; set indent for python
  (setq python-indent-offset 4)
  (setq elpy-rpc-python-command "/usr/local/bin/python")
  ;; remove indention hightlight
  (delq 'elpy-module-highlight-indentation elpy-modules)
  (elpy-use-ipython)

  ;;(setq elpy-rpc-backend "jedi")
  (setq elpy-rpc-backend "jedi")

  (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
  (defun zl/switch-to-python3 ()
    (interactive)
    (setq elpy-rpc-python-command "python3")
    (elpy-use-ipython "ipython3"))
  (defun zl/switch-to-python2 ()
    (interactive)
    (setq elpy-rpc-python-command "python")
    (elpy-use-ipython "ipython"))

  (require 'flycheck)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  )

;; clang-format
(use-package clang-format
  :ensure t
  :bind* (
         ("C-M-<tab>" . clang-format-buffer))
  :config
  (setq clang-format-style-option "google")
  )

;; ein for ipython-notebook
(use-package ein
  :ensure t
  :config
  (require 'ein)
  (setq ein:use-auto-complete-superpack t)
  (setq ein:use-smartrep t))

;; activate whitespace-mode to view all whitespace characters
(use-package whitespace
  :bind ("C-c w" . whitespace-mode))


;; windmove
(use-package windmove
  :ensure t
  :config
  (windmove-default-keybindings)
  )

;; winner-mode recover window
(use-package winner
  :config
  (winner-mode t))

;; projectile
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  )

(use-package  helm-projectile
  :ensure t
  :config
  (helm-projectile-on)
  ;; switch to project add call magit-status
  (setq projectile-switch-project-action 'projectile-vc)
  ;; (setq projectile-switch-project-action 'helm-projectile-find-file)
  (setq projectile-switch-project-action 'helm-projectile)
  (setq projectile-enable-caching t)
  )

;; ag
(use-package ag
  :ensure    t
  :commands  ag
  :init      (setq ag-highlight-search t)
  :config    (add-to-list 'ag-arguments "--word-regexp"))

;; exec-path-from-shell makes the command-line path with Emacs’s shell match the same one on OS X
(use-package exec-path-from-shell
  :ensure t
  :config (when (memq window-system '(mac ns))
            (exec-path-from-shell-initialize)))

;; git-gutter+
(use-package git-gutter+
  :ensure t
  :config (global-git-gutter+-mode))


;; graphviz-dot
(use-package graphviz-dot-mode
  :ensure t
  :config
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
  )

;; magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  )


;; markdown mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


;; lua mode
(use-package lua-mode
  :ensure t
  :interpreter "lua5.1"
  :config
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
  :bind
  (
   :map lua-mode-map
        ("C-<return>" . lua-send-current-line)
        ("C-c C-c" . lua-send-region)
        ("C-M-x" . lua-send-defun)
        )
  )

;; impatient-mode
;; useage: httpd start impatient-mode
;; localhost:8080/imp
(use-package impatient-mode
  :ensure t
  :config
  (require 'impatient-mode))


;; sh-mode bug org-mode fontification error
;; (add-to-list 'load-path "~/.emacs.d/elpa/sh-mode/")

;; (use-package neotree
;;   :ensure t)

;; eproject
(use-package eproject
  :ensure t)


;; tags
(require 'etags)

(use-package ctags-update
  :ensure t
  :bind (("C-c C-r" . ctags-update))
  :config
  ;;;###autoload
  (defun turn-on-ctags-auto-update-mode()
    "turn on `ctags-auto-update-mode'."
    (interactive)
    (ctags-update-minor-mode 1))
  (add-hook 'prog-mode-hook  'turn-on-ctags-auto-update-mode)
  (setq ctags-update-command "/usr/local/bin/ctags-zl")
  :diminish ctags-auto-update-mode)

;; etags-select
(use-package etags-select
  :ensure t
  :commands etags-select-find-tag-at-point
  :init
  (defun build-ctags ()
    (interactive)
    (message "building project tags")
    (let ((root (eproject-root)))
      (shell-command (concat "ctags-zl -e -R --extra=+fq --exclude=db --exclude=test --exclude=.git --exclude=public -f " root "TAGS " root))
      )
    (visit-project-tags)
    (message "tags built successfully"))

    (defun visit-project-tags ()
    (interactive)
    (let ((tags-file (concat (eproject-root) "TAGS")))
      (visit-tags-table tags-file)
      (message (concat "Loaded " tags-file))))

      (defun my-find-tag ()
    (interactive)
    (if (file-exists-p (concat (eproject-root) "TAGS"))
        (visit-project-tags)
      (build-ctags))
    (etags-select-find-tag-at-point))

      :bind* (("M-*" . pop-tag-mark)
              ("M-." . my-find-tag))
  ;;:config
  ;; (global-set-key (kbd "M-.") 'my-find-tag)
  )

;; hs-minor-mode
(use-package hideshow
  :ensure t
  :bind (("s-+" . toggle-hiding)
         ("C-\\" . toggle-selective-display))
  :init
  (defun toggle-selective-display (column)
    (interactive "P")
    (set-selective-display
     (or column
         (unless selective-display
           (1+ (current-column))))))

  (defun toggle-hiding (column)
    (interactive "P")
    (if hs-minor-mode
        (if (condition-case nil
                (hs-toggle-hiding)
              (error t))
            (hs-show-all))
      (toggle-selective-display column)))
  :config
  (add-hook 'c-mode-common-hook   'hs-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
  (add-hook 'elpy-mode-hook 'hs-minor-mode)
  (add-hook 'lua-mode-hook 'hs-minor-mode)
  )


;; bookmark+
 ; (use-package bookmark+
 ;   :ensure t
 ;   :config
 ;   (require 'bookmark+)
 ;   (setq bookmark-save-flag 1)
 ;   (setq inhibit-splash-screen t)
 ;   (require 'bookmark)
 ;   (bookmark-bmenu-list)
 ;   (switch-to-buffer "*Bookmark List*")
 ;   ;; (bookmark-load bookmark-default-file t)
 ;   )

(use-package smart-mode-line
  :ensure t)

(fset 'yes-or-no-p 'y-or-n-p)

(use-package macrostep
  :ensure t
  ;; :bind ("C-c m e" . macrostep-expand)
  )

(use-package smooth-scrolling
  :ensure t)


(use-package cmake-mode
  :ensure t
  :config
  ;; Add cmake listfile names to the mode list.
  (setq auto-mode-alist
        (append
         '(("CMakeLists\\.txt\\'" . cmake-mode))
         '(("\\.cmake\\'" . cmake-mode))
         auto-mode-alist)))

(use-package cmake-font-lock
  :ensure t)

;; yaml-mode
(use-package yaml-mode
  :ensure t)

(use-package objc-font-lock
  :ensure t
  :config
   (objc-font-lock-global-mode 1)
   ;; Don't highlight brackets.
   (setq objc-font-lock-bracket-face nil)
   ;; Use `secondary-selection` (a builtin face) as background.
   (setq objc-font-lock-background-face 'secondary-selection))

;; commented lines make org mode cannot convert to md.
(use-package cc-mode
  :init
  ;;   (defadvice ff-get-file-name (around ff-get-file-name-framework
  ;;                   (search-dirs
  ;;                    fname-stub
  ;;                    &optional suffix-list))
  ;; "Search for Mac framework headers as well as POSIX headers."
  ;;  (or
  ;;   (if (string-match "\\(.*?\\)/\\(.*\\)" fname-stub)
  ;;   (let* ((framework (match-string 1 fname-stub))
  ;;          (header (match-string 2 fname-stub))
  ;;          (fname-stub (concat framework ".framework/Headers/" header)))
  ;;     ad-do-it))
  ;;     ad-do-it))
  :config
  ;; (add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))
  ;; (add-to-list 'magic-mode-alist
  ;;               `(,(lambda ()
  ;;                    (and (string= (file-name-extension buffer-file-name) "h")
  ;;                         (re-search-forward "@\\<interface\\>"
  ;;                        magic-mode-regexp-match-limit t)))
  ;;                 . objc-mode))
  ;; (require 'find-file) ;; for the "cc-other-file-alist" variable
  ;; (nconc (cadr (assoc "\\.h\\'" cc-other-file-alist)) '(".m" ".mm"))

  ;; system header
  ;; (ad-enable-advice 'ff-get-file-name 'around 'ff-get-file-name-framework)
  ;; (ad-activate 'ff-get-file-name)

  (setq cc-search-directories '("." "../include" "/usr/include" "/usr/local/include/*"
                                "/System/Library/Frameworks" "/Library/Frameworks"))
  )

;; flycheck for objective c
(use-package flycheck-objc-clang
  :ensure t
  :config
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-objc-clang-setup))
  )

(use-package dummy-h-mode
  :ensure t)

