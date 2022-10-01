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
      ))
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
      (setq frame-resize-pixelwise t)
      (setq mac-command-modifier 'meta)
      (setq vterm-shell "/bin/zsh")
      ))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

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
;; what key should you push next? not needed embark 
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
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))
(use-package evil-snipe
  :init (evil-snipe-mode 1))
(use-package evil-matchit
  :after evil
  :init
  (setq evilmi-shortcut "M-m")
  (global-evil-matchit-mode 1))
(use-package evil-lion
  :after evil
  :config
  (evil-lion-mode))
(use-package evil-numbers
  :after evil)
(use-package vimish-fold
  :after evil)
(use-package evil-vimish-fold
  :after vimish-fold
  :hook ((prog-mode conf-mode text-mode) . evil-vimish-fold-mode))

(define-key evil-normal-state-map (kbd "C-=") 'evil-numbers/inc-at-pt)
(define-key evil-visual-state-map (kbd "C-=") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C--") 'evil-numbers/dec-at-pt)
(define-key evil-visual-state-map (kbd "C--") 'evil-numbers/dec-at-pt)

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
    ;;"xn" '(treemac :which-key "Tree Browser")
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
      (use-package vterm)
      (use-package multi-vterm)

      (define-key vterm-mode-map (kbd "C-q") #'vterm-send-next-key)
      (push (list "find-file-below"
		  (lambda (path)
		    (if-let* ((buf (find-file-noselect path))
                             (window (display-buffer-below-selected buf nil)))
			(select-window window)
                      (message "Failed to open file: %s" path))))
	    vterm-eval-cmds)
      ))


;; Quickly comment/uncomment code
(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))



;; Scala
;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :interpreter ("scala" . scala-mode))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false")))

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :init (global-flycheck-mode))

(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in scala files
  ;; You could also swap out lsp for lsp-deffered in order to defer loading
  :hook  (scala-mode . lsp)
         (lsp-mode . lsp-lens-mode)
  :config
  ;; Uncomment following section if you would like to tune lsp-mode performance according to
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  ;; (setq gc-cons-threshold 100000000) ;; 100mb
  ;; (setq read-process-output-max (* 1024 1024)) ;; 1mb
  ;; (setq lsp-idle-delay 0.500)
  ;; (setq lsp-log-io nil)
  ;; (setq lsp-completion-provider :capf)
  (setq lsp-prefer-flymake nil))

;; Add metals backend for lsp-mode
(use-package lsp-metals)

;; Enable nice rendering of documentation on hover
;;   Warning: on some systems this package can reduce your emacs responsiveness significally.
;;   (See: https://emacs-lsp.github.io/lsp-mode/page/performance/)
;;   In that case you have to not only disable this but also remove from the packages since
;;   lsp-mode can activate it automatically.
(use-package lsp-ui)

;; lsp-mode supports snippets, but in order for them to work you need to use yasnippet
;; If you don't want to use snippets set lsp-enable-snippet to nil in your lsp-mode settings
;; to avoid odd behavior with snippets and indentation
(use-package yasnippet)

;; Use company-capf as a completion provider.
;;
;; To Company-lsp users:
;;   Company-lsp is no longer maintained and has been removed from MELPA.
;;   Please migrate to company-capf.
(use-package company
  :hook (scala-mode . company-mode)
  :config
  (setq lsp-completion-provider :capf))

;; Posframe is a pop-up tool that must be manually installed for dap-mode
(use-package posframe)

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

(use-package lsp-mode
  :commands (lsp lsp-deffered)
  :init (setq lsp-keymap-prefix "C-c l"))

(use-package company
  :init (global-company-mode t))

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
  :init
  (setq org-confirm-babel-evaluate nil)
  ;;(setq org-startup-folded 'content')
  (setq org-startup-indented  t)
  (setq org-startup-numerated t)
  :hook visual-line-mode)

(use-package org-roam)

;; themes at the end
(if (display-graphic-p)
    (progn
      (use-package all-the-icons)
      (use-package doom-themes
	:config
	;; Global settings (defaults)
	(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
              doom-themes-enable-italic t) ; if nil, italics is universally disabled
	(load-theme 'doom-moonlight t)
	;; Enable flashing mode-line on errors
	(doom-themes-visual-bell-config)
	;; Enable custom neotree theme (all-the-icons must be installed!)
	;;(doom-themes-neotree-config)
	;; or for treemacs users
	;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
	;;(doom-themes-treemacs-config)
	;; Corrects (and improves) org-mode's native fontification.
	(doom-themes-org-config))
      ))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "b9761a2e568bee658e0ff723dd620d844172943eb5ec4053e2b199c59e0bcc22" default))
 '(safe-local-variable-values
   '((diff-add-log-use-relative-names . t)
     (vc-git-annotate-switches . "-w"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
