(setq inhibit-splash-screen t)
(setq make-backup-files nil)
(menu-bar-mode -1)
(toggle-scroll-bar -1)

;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;;(package-initialize)
;;(package-refresh-contents)

;; Download use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Enable Evil
(use-package evil :ensure t)
(evil-mode 1)

(use-package vterm)

;; Try to move direction, which is supplied as arg
;; If cannot move that direction, send a tmux command to do appropriate move
(defun windmove-emacs-or-tmux(dir tmux-cmd)
  (interactive)
  (if (ignore-errors (funcall (intern (concat "windmove-" dir))))
      nil                       ;; Moving within emacs
    (shell-command tmux-cmd)) ;; At edges, send command to tmux
  )

;;Move between windows with custom keybindings
(global-set-key (kbd "C-k")
   '(lambda () (interactive) (windmove-emacs-or-tmux "up"  "tmux select-pane -U")))
(global-set-key (kbd "C-j")
   '(lambda () (interactive) (windmove-emacs-or-tmux "down"  "tmux select-pane -D")))
(global-set-key (kbd "C-l")
   '(lambda () (interactive) (windmove-emacs-or-tmux "right" "tmux select-pane -R")))
(global-set-key (kbd "C-h")
   '(lambda () (interactive) (windmove-emacs-or-tmux "left"  "tmux select-pane -L")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(vterm use-package evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
