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
  '(("C-}" . mc/mark-next-like-this)
    ("C-{" . mc/mark-previous-like-this)
    ("C-|" . mc/mark-all-like-this)
    ("C-c C-}" . mc/skip-to-next-like-this)
    ("C-c C-{" . mc/skip-to-previous-like-this)
    ("C-x _" . zl/split-window-vertically)
    ("C-x |" . zl/split-window-horizontally)
    ("M-l" . zl/select-current-line)
    ("C->" . ace-jump-mode)
    ("C-<" . ace-jump-mode-pop-mark)
    ("M-/" . undo-tree-visualize)
    ("C-\"" . theme-looper-enable-next-theme)
    ("C-M-'" . myterminal-controls-open-controls)
    ("C-c M-x" . execute-extended-command)
;    ("M-x" . helm-M-x)
    ("C-x b" . helm-mini)
    ("C-x C-b" . helm-buffers-list)
    ("C-x C-f" . helm-find-files)
    ("C-x C-r" . helm-recentf)
    ("M-y" . helm-show-kill-ring)
    ("M-<tab>" . switch-window)
    ("C-s-<up>" . buf-move-up)
    ("C-s-<down>" . buf-move-down)
    ("C-s-<left>" . buf-move-left)
    ("C-s-<right>" . buf-move-right)
    ("s-/" . comment-or-uncomment-region)
    ("C-x t" . multi-term)
    ("M-w" . kill-region)
    ("<f5>" . super-emacs-reload-current-file)))

(defun super-emacs-apply-keyboard-bindings (pair)
  "Apply keyboard-bindings for supplied list of key-pair values"
  (global-set-key (kbd (car pair))
                  (cdr pair)))





(mapc 'super-emacs-apply-keyboard-bindings
      super-emacs--my-keyboard-bindings)





