(defun zl/split-window-horizontally ()
  (interactive)
  (split-window-horizontally)
  (other-window 1)
  (switch-to-next-buffer))

(defun zl/split-window-vertically ()
  (interactive)
  (split-window-vertically)
  (other-window 1)
  (switch-to-next-buffer))

(defun zl/select-current-line ()
  "Select the current line"
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))

(defvar super-emacs--my-keyboard-bindings 
  '(("C-x _" . zl/split-window-vertically)
    ("C-x |" . zl/split-window-horizontally)
    ("M-l" . zl/select-current-line)
    ;; ("\C-x a j" . ace-jump-mode)
    ;; ("\C-x a k " . ace-jump-mode-pop-mark)
    ;; ("M-/" . undo-tree-visualize)
    ;; ("C-\"" . theme-looper-enable-next-theme)
    ;; ("C-M-'" . myterminal-controls-open-controls)
    ("C-c M-x" . execute-extended-command)
    ;; ("M-x" . helm-M-x)
    ;; ("C-x b" . helm-mini)
    ;; ("C-x C-b" . helm-buffers-list)
    ;; ("C-x C-f" . helm-find-files)
    ;; ("C-x C-r" . helm-recentf)
    ;; ("M-y" . helm-show-kill-ring)
    ;; ("C-<tab>" . switch-window)
    ("s-/" . comment-or-uncomment-region)
    ("M-w" . kill-region)
    ("M-n" . company-select-next)
    ("M-p" . company-select-previous)
    ;; indention setup
    ;; automatically indent when press RET
    ("RET" . newline-and-indent)
    ("<f5>" . super-emacs-reload-current-file)))

(defun super-emacs-apply-keyboard-bindings (pair)
  "Apply keyboard-bindings for supplied list of key-pair values"
  (global-set-key (kbd (car pair))
                  (cdr pair)))





(mapc 'super-emacs-apply-keyboard-bindings
      super-emacs--my-keyboard-bindings)





