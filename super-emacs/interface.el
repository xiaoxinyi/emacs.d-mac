;;Change title-bar text
(setq frame-title-format
      "emacs")

;;Disable menu-bar
;; (menu-bar-mode -1)

;;Disable tool-bar
(tool-bar-mode -1)

;Disable scroll-bar
(scroll-bar-mode -1)

(defun zl/mac? ()
  (eq system-type 'darwin))


;; (use-package color-theme
;;   :ensure t)

;; (use-package color-theme-solarized
;;   :ensure t
;;   :config
;;   (when window-system
;;     (setq solarized-use-variable-pitch nil)
;;     (setq solarized-height-plus-1 1.0)
;;     (setq solarized-height-plus-2 1.0)
;;     (setq solarized-height-plus-3 1.0)
;;     (setq solarized-height-plus-4 1.0)
;;     (setq solarized-use-less-bold t)
;;     (setq solarized-termcolors 256)
;;     (setq solarized-high-contrast-mode-line t)
;;     (load-theme 'solarized-dark t)
;;     )
;;)


(use-package hl-line
  :if window-system
  :config
  (global-hl-line-mode))


(when (zl/mac?)
  (set-frame-parameter nil 'fullscreen 'fullboth))

(defun zl/is-window-system
    nil
    (when window-system t)
  )


;; ;Set font
;; (custom-set-faces
;;  '(default ((t (:height 120)))))

;; (add-to-list 'default-frame-alist '(font . "Fira Code-15" ))


;; font config

;; (setq zl/default-font "Inconsolata")
(setq zl/default-font "Source Code Pro")
(setq zl/default-font "Fira Code")

(setq zl/default-font-size 15)
(setq zl/current-font-size zl/default-font-size)

;; (if (zl/mac?)
;;     (setq zl/default-font-size 16)
;;     (setq zl/default-font-size 12))

(setq zl/font-change-increment 1.1)



(defun zl/set-font-size ()
  "Set the font to `zl/default-font' at `zl/current-font-size'."
  (set-frame-font
   (concat zl/default-font "-" (number-to-string zl/current-font-size)))

  (when (and (string= zl/default-font "Fira Code") (zl/is-window-system))
      (set-frame-font
   (concat zl/default-font "-" (number-to-string zl/current-font-size)))
      (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
                     (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
                     (36 . ".\\(?:>\\)")
                     (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
                     (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                     (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
                     (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
                     (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
                     (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
                     (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
                     (48 . ".\\(?:x[a-zA-Z]\\)")
                     (58 . ".\\(?:::\\|[:=]\\)")
                     (59 . ".\\(?:;;\\|;\\)")
                     (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
                     (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                     (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                     (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
                     (91 . ".\\(?:]\\)")
                     (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                     (94 . ".\\(?:=\\)")
                     (119 . ".\\(?:ww\\)")
                     (123 . ".\\(?:-\\)")
                     (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                     (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
                     )
                   ))
        (dolist (char-regexp alist)
          (set-char-table-range composition-function-table (car char-regexp)
                                `([,(cdr char-regexp) 0 font-shape-gstring])))
        )
      )
  )

(defun zl/reset-font-size ()
  "Change font size back to `zl/default-font-size'."
  (interactive)
  (setq zl/current-font-size zl/default-font-size)
  (zl/set-font-size))

(defun zl/increase-font-size ()
  "Increase current font size by a factor of `zl/font-change-increment'."
  (interactive)
  (setq zl/current-font-size
        (ceiling (* zl/current-font-size zl/font-change-increment)))
  (zl/set-font-size))

(defun zl/decrease-font-size ()
  "Decrease current font size by a factor of `zl/font-change-increment', down to a minimum size of 1."
  (interactive)
  (setq zl/current-font-size
        (max 1
             (floor (/ zl/current-font-size zl/font-change-increment))))
  (zl/set-font-size))

(zl/set-font-size)

(bind-key "C-)" 'zl/reset-font-size)
(bind-key "C-)" 'zl/reset-font-size)
(bind-key "C-+" 'zl/increase-font-size)
(bind-key "C-=" 'zl/increase-font-size)
(bind-key "C-_" 'zl/decrease-font-size)
(bind-key "C--" 'zl/decrease-font-size)


(global-prettify-symbols-mode t)
