(setq inhibit-splash-screen t)
(setq make-backup-files nil)
(menu-bar-mode nil)
(toggle-scroll-bar nil)

;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;;(package-initialize)
;;(package-refresh-contents)

;; Download use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(use-package evil
  :ensure t
  :config (evil-mode 1))

(use-package vterm
  :ensure t)

(use-package
  zenburn-theme
  :ensure t
  :config (load-theme 'zenburn t))

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
