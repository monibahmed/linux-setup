(setq inhibit-splash-screen t)
(setq make-backup-files nil)
(toggle-scroll-bar -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;; (package-initialize)
;; (package-refresh-contents)

;; Download use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(use-package evil
  :ensure t
  :config (evil-mode 1))

(use-package vterm
  :ensure t)

(use-package zenburn-theme :ensure t)
(use-package dracula-theme :ensure t)
(use-package zeno-theme :ensure t)

(if (display-graphic-p)
    (load-theme 'zeno t)
  nil)
