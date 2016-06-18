;Change title-bar text
(setq frame-title-format
      "emacs")

;Disable menu-bar
(menu-bar-mode -1)

;Disable tool-bar
; (tool-bar-mode -1)
(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)))

;Disable scroll-bar
(scroll-bar-mode -1)

;Activate material theme
(load-theme 'material
            t)
;Set font
(custom-set-faces
 '(default ((t (:height 120)))))

(add-to-list 'default-frame-alist '(font . "Fira Code-15" ))

