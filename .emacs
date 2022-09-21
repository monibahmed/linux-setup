;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook
 'emacs-startup-hook
 (lambda ()
   (message "*** Emacs loaded in %s seconds with %d garbage collections."
            (emacs-init-time "%.2f")
            gcs-done)))

;;Some initial package and environment setup
;;https://github.com/radian-software/straight.el
(setq straight-vc-git-default-clone-depth 1)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage)
  )

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package exec-path-from-shell)

;; Emacs options for different things
(setq inhibit-splash-screen t)
(setq make-backup-files nil)
(if (display-graphic-p)
    (progn
      (toggle-scroll-bar -1)
      (tool-bar-mode     -1)
      )
  )
(menu-bar-mode     -1)
(tooltip-mode      -1)
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq package-enable-at-startup nil)
(setq vc-follow-symlinks nil)
;;doesn't work as expected
;;(add-to-list 'default-frame-alist '(undecorated . t))
;;https://www.emacswiki.org/emacs/YesOrNoP
(defalias 'yes-or-no-p 'y-or-n-p)

(if (eq system-type 'darwin)
    (progn
      (message "Emacs running in Mac OS")
      (exec-path-from-shell-initialize)
      (setq frame-resize-pixelwise t)
      (setq mac-command-modifier 'meta)
      (setq vterm-shell "/bin/zsh")
      )
  )


;;some helper packages
;;Undo/Redo in Emacs
(use-package undo-tree
  :init (global-undo-tree-mode))
;;notified if the definition of a function you are customizing change
(use-package el-patch)
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init (savehist-mode))
;; a better window manager?
(use-package ace-window
  :bind ("C-c o" . 'ace-window)
  :init
  (setq aw-dispatch-always t)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))
;; divides search pattern into space separated components
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))
;;what key should you push next? not needed embark 
(use-package which-key
  :init (which-key-mode))
;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; enable evil mode
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-undo-system 'undo-tree)
  (setq evil-insert-state-cursor '(bar))
  (setq evil-normal-state-cursor '(box))
  ;;(setq evil-motion-state-modes (append evil-emacs-state-modes evil-motion-state-modes))
  ;;(setq evil-emacs-state-modes nil)
  ;;(setq evil-want-minibuffer t)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  ;;(evil-set-initial-state 'messages-buffer-mode 'normal)
  ;;(evil-set-initial-state 'dashboard-mode 'normal)
  )

(use-package evil-collection
  ;;:custom (evil-collection-setup-minibuffer t)
  :after evil
  :config
  (evil-collection-init))

(use-package ibuffer
  :straight nil
  :bind ("C-x C-b" . ibuffer))
;; (add-to-list 'ibuffer-never-show-predicates "^\\*")
(use-package ibuf-ext
  :straight nil)
(setq ibuffer-saved-filter-groups
      (quote (("default"
	       ("Dotfiles" (or (name . "^\\.")))
	       ("Messages" (or (name . "^\\*")))
	       ("Magit" (or (name . "^\\magit*")))
	       ))))

(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "default")))

;; Customize your keyboard shortcuts
(use-package hydra)
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(use-package general
  :config
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (rune/leader-keys
    "t"  '(:ignore t :which-key "Toggles")
    "tt" '(load-theme :which-key "Choose Theme")
    "ts" '(hydra-text-scale/body :which-key "Scale Text")
    "tl" '(lambda() (interactive)(load-theme 'doom-one-light t) :which-key "Light Theme")
    "td" '(lambda() (interactive)(load-theme 'doom-moonlight t) :which-key "Dark Theme") "xb" '(ibuffer :which-key "ibuffer")
    "xv" '(multi-vterm :which-key "vterm")
    "xn" '(treemac :which-key "Tree Browser")
    "fe" '(lambda() (interactive)(find-file "~/.emacs") :which-key ".emacs")
    "fz" '(lambda() (interactive)(find-file "~/.zshrc") :which-key ".zshrc")
    "fn" '(lambda() (interactive)(find-file "~/.notes") :which-key ".notes")
    )
  )
;;(global-set-key (kbd "C-e") 'end-of-line)

;; Completion frameworks and doing stuff 
(use-package vertico
  :bind (:map
	 vertico-map
	 ("C-j" . vertico-next)
	 ("C-k" . vertico-previous)
	 ("C-f" . vertico-exit)
	 :map minibuffer-local-map
	 ("M-h" . backward-kill-word))
  :custom (vertico-cycle t)
  :init (vertico-mode))

(use-package consult
  ;;:demand t
  :bind (("C-c s" . consult-line)
	 ("C-M-l" . consult-imenu)
	 ("C-r" . consult-history)
	 ))

;;Do commands and operatioms on buffers or synbols
(use-package embark
  :bind (("C-c e" . embark-act)
	 ("M-." . embark-dwim)
	 ("C-h B" . embark-bindings))
  :init (setq prefix-help-command #'embark-prefix-help-command))
(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Help you code
(if (not (eq system-type 'windows-nt))
    (progn
      (message "Emacs running in Windows")
      (use-package vterm)
      (use-package multi-vterm)
      ))


;; Quickly comment/uncomment code
(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;; (use-package lsp-mode
;;   :commands (lsp lsp-deffered)
;;   :init (setq lsp-keymap-prefix "C-c l"))

;; (use-package company
;;   :init (global-company-mode t))

;; Project management
(use-package magit)
(use-package perspective
  :bind
  ("C-x C-b" . persp-ibuffer)
  :custom
  (persp-mode-prefix-key (kbd "C-x C-x"))
  :init
  (persp-mode))

  
;; Organize your notes and maybe part of your life
(use-package org
  :config (setq org-confirm-babel-evaluate nil))

(use-package org-roam)

;; themes at the end
(if (display-graphic-p)
    (progn
      (use-package doom-themes
	:config
	;; Global settings (defaults)
	(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
              doom-themes-enable-italic t) ; if nil, italics is universally disabled
	(load-theme 'doom-moonlight t)
	;; Enable flashing mode-line on errors
	(doom-themes-visual-bell-config)
	;; Enable custom neotree theme (all-the-icons must be installed!)
	(doom-themes-neotree-config)
	;; or for treemacs users
	(setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
	(doom-themes-treemacs-config)
	;; Corrects (and improves) org-mode's native fontification.
	(doom-themes-org-config))
      ))
