;; -*- lexical-binding: t; -*-
;; This Emacs configuration file sets up packages for running efficiently. Emacs provides a framework for doing things quickly, so you can think more and type less.
;; Our Emacs philosophy:
;; 1. Looks at feels the way we like it
;; 2. Quickly create, open, close, search, compile/run projects
;; 3. Enable repository management and creation
;; 4. Enable code frameworks to streamline the process

;; Find a good emacs package manager
;; Emacs works in Modes, Major and Minor modes
;; Section 1 -- Look and feel
;; Centaur-tabs
;; Section 2 -- Project Management
;; Perspecttive, Persp-mode
;; Section 3 -- Prgramming Language Enhancements
;; yassnippets
;; Section 4 -- Organization of Task

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))


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
(setq vc-follow-symlinks nil)
(setq tramp-verbose 10)

;; Enable good retina support for Mac OS
;;(set-face-attribute 'default nil :font "Fira Code Retina" :height 280)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


(customize-set-variable 'tramp-encoding-shell "/bin/bash")
(if (eq system-type 'darwin)
    (progn
     (message "Emacs running in Mac OS")
     (setq mac-command-modifier 'meta)
     (setq vterm-shell "/bin/zsh")
     )
  )

(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))


(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;;(use-package command-log-mode)
;;(use-package rainbow-delimiters)
;;(use-package helpful)

(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Your own keybinding with Prefix
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
    "td" '(lambda() (interactive)(load-theme 'doom-moonlight t) :which-key "Dark Theme")
    "xb" '(ibuffer :which-key "ibuffer")
    "xv" '(multi-vterm :which-key "vterm")
    "xn" '(treemac :which-key "Tree Browser")
    "fe" '(lambda() (interactive)(find-file "~/.emacs") :which-key ".emacs")
    "fz" '(lambda() (interactive)(find-file "~/.zshrc") :which-key ".zshrc")
    "fn" '(lambda() (interactive)(find-file "~/.notes") :which-key ".notes")
    )
  )

(global-set-key (kbd "C-x C-b") 'ibuffer) 
(global-set-key (kbd "C-x C-n") 'treemacs)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)


;; Don't use evil binding in Vterm?
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)
  (setq evil-want-C-i-jump nil)
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

(use-package all-the-icons
  :if (display-graphic-p))
(use-package doom-modeline
  :init (doom-modeline-mode 1))


(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'vertico))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/")
    (setq projectile-project-search-path '("~/")))
  (setq projectile-switch-project-action #'projectile-dired))

;;(use-package counsel-projectile)
;;(use-package ivy)
;;(use-package diminish)
;;(use-package consult)
;;(use-package flymake)
;;(use-package flycheck)
;; Use ripgrep for checking variables inside
;;(use-package ripgrep)
;;(use-package rg)
;;(use-package org-ql)
;; Use custom for defining values rather the setq
;; (use-package org-wild...)
;; esc esc repeat complex command

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge)

(require 'org)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org-notes/gtd.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/org-notes/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))


(use-package vertico
  :init
  ;;  :bind (:map minibuffer-local-map
  ;;	      ("C-k" . vertico-previous)
  ;;	      ("C-j" . vertico-next)
  ;;	      )
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  ;; The :init configuration is always executed (Not lazy!)
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode)
)

;; A few more useful configurations... for vertico
;; Add prompt indicator to `completing-read-multiple'.
;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
(savehist-mode 1)

(defun crm-indicator (args)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr arGs)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Emacs 28: Hide commands in M-x which do not work in the current mode.
;; Vertico commands are hidden in normal buffer
(setq read-extended-command-predicate
      #'command-completion-default-include-p)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))  

;; This is default setup for Treemacs
(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :config (treemacs-set-scope-type 'Tabs))

;; optional if you want which-key integration
(use-package which-key
    :config
    (which-key-mode))




(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

;; THIS IS FOR CODE COMPLETION/CHECKING
;; ;; (use-package flycheck
;; ;;   :init (global-flycheck-mode))
;; ;; 
;; ;; (use-package lsp-mode
;; ;;   :hook ((python-mode c++-mode) . lsp-deferred) ; XYZ are to be replaced by python, c++, etc.
;; ;;   :commands lsp)
;; ;; 
;; ;; (use-package lsp-ui
;; ;;   :commands lsp-ui-mode
;; ;;   :config
;; ;;   (setq lsp-ui-doc-enable nil)
;; ;;   (setq lsp-ui-doc-header t)
;; ;;   (setq lsp-ui-doc-include-signature t)
;; ;;   (setq lsp-ui-doc-border (face-foreground 'default))
;; ;;   (setq lsp-ui-sideline-show-code-actions t)
;; ;;   (setq lsp-ui-sideline-delay 0.05))
;; ;; 
;; ;; (use-package lsp-pyright
;; ;;   :hook (python-mode . (lambda () (require 'lsp-pyright)))
;; ;;   :init (when (executable-find "python3")
;; ;;           (setq lsp-pyright-python-executable-cmd "python3")))
;; ;; 
;; ;; 
;; ;; (use-package lsp-mode
;; ;;   :hook ((c-mode          ; clangd
;; ;;           c++-mode        ; clangd
;; ;;           c-or-c++-mode   ; clangd
;; ;;           ;;java-mode       ; eclipse-jdtls
;; ;;           ;;js-mode         ; ts-ls (tsserver wrapper)
;; ;;           ;;js-jsx-mode     ; ts-ls (tsserver wrapper)
;; ;;           ;;typescript-mode ; ts-ls (tsserver wrapper)
;; ;;           python-mode     ; pyright
;; ;;           ;;web-mode        ; ts-ls/HTML/CSS
;; ;;           ;;haskell-mode    ; haskell-language-server
;; ;;           ) . lsp-deferred)
;; ;;   :commands lsp
;; ;;   :config
;; ;;   (setq lsp-auto-guess-root t)
;; ;;   (setq lsp-log-io nil)
;; ;;   (setq lsp-restart 'auto-restart)
;; ;;   (setq lsp-enable-symbol-highlighting nil)
;; ;;   (setq lsp-enable-on-type-formatting nil)
;; ;;   (setq lsp-signature-auto-activate nil)
;; ;;   (setq lsp-signature-render-documentation nil)
;; ;;   (setq lsp-eldoc-hook nil)
;; ;;   (setq lsp-modeline-code-actions-enable nil)
;; ;;   (setq lsp-modeline-diagnostics-enable nil)
;; ;;   (setq lsp-headerline-breadcrumb-enable nil)
;; ;;   (setq lsp-semantic-tokens-enable nil)
;; ;;   (setq lsp-enable-folding nil)
;; ;;   (setq lsp-enable-imenu nil)
;; ;;   (setq lsp-enable-snippet nil)
;; ;;   (setq read-process-output-max (* 1024 1024)) ;; 1MB
;; ;;   (setq lsp-idle-delay 0.5))
;; ;; 
;; ;; 
;; ;; 
;; ;; (use-package company
;; ;;   :init
;; ;;   (global-company-mode)
;; ;;   :config
;; ;;   (add-hook 'after-init-hook 'global-company-mode))
;; ;; 
;; ;; ;; (use-package company-irony
;; ;; ;;   :config
;; ;; ;;   (add-to-list 'company-backends 'company-irony))
;; ;; ;;   
;; ;; ;; (use-package irony
;; ;; ;;   :config
;; ;; ;;   (add-hook 'c++-mode-hook 'irony-mode)
;; ;; ;;   (add-hook 'c-mode-hook 'irony-mode)
;; ;; ;;   (add-hook 'objc-mode-hook 'irony-mode))
;; ;; 
;; ;; ;; (use-package lsp-mode
;; ;; ;;   :init
;; ;; ;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;; ;; ;;   (setq lsp-keymap-prefix "C-c l")
;; ;; ;;   :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;; ;; ;;          (XXX-mode . lsp)
;; ;; ;;          ;; if you want which-key integration
;; ;; ;;          (lsp-mode . lsp-enable-which-key-integration))
;; ;; ;;   :commands lsp)
;; ;; ;; 
;; ;; ;; ;; optionally
;; ;; (use-package lsp-ui
;; ;;   :commands lsp-ui-mode)
;; ;; (use-package lsp-treemacs
;; ;;   :commands lsp-treemacs-sync-mode)
;; ;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; ;; optionally if you want to use debugger
;; ;; (use-package dap-mode)
;; ;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; (use-package helm)
;; (require 'helm-config)
;; (global-set-key (kbd "C-c h") 'helm-mini)
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

(if (display-graphic-p)
    (progn
      (use-package doom-themes
	:ensure t
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
	(setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
	(doom-themes-treemacs-config)
	;; Corrects (and improves) org-mode's native fontification.
	(doom-themes-org-config))
      )
  )

(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tab-forward))

(require 'ibuf-ext)
;; (add-to-list 'ibuffer-never-show-predicates "^\\*")
(setq ibuffer-saved-filter-groups
      (quote (("default"
	       ("mesages" (or
			   (name . "^\\*")))
	       ))))
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef" "7b1ea77093c438aa5887b2649ca079c896cc8780afef946d3b6c53931081a726" default))
 '(ein:output-area-inlined-images t)
 '(org-agenda-files
   '("~/org-notes/gtd.org" "/Users/monibahmed/org-notes/org-mode-tutorial.org" "/Users/monibahmed/org-notes/latex_example.org"))
 '(package-selected-packages
   '(lsp-mode centaur-tabs forge evil-magit rg ripgrep general command-log-mode darkroom olivetti ibuf-ext org-mode multi-vterm conda ein vterm zeno-theme zenburn-theme which-key vertico use-package treemacs-tab-bar treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-evil orderless marginalia evil-collection dracula-theme doom-modeline all-the-icons))
 '(safe-local-variable-values
   '((eval progn
	   (turn-off-auto-fill)
	   (text-scale-set 1)
	   (load-theme 'doom-spacegray t))
     (load-theme 'doom-spacegray t)
     (if
	 (display-graphic-p)
	 (load-theme 'doom-spacegray t))
     (olivetti-body-width . 80)
     (visual-fill-column-width . 80)
     (eval progn
	   (turn-off-auto-fill)
	   (text-scale-set 1)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
