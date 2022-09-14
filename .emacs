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

(use-package exec-path-from-shell
  :straight t)

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

;;https://www.emacswiki.org/emacs/YesOrNoP
(defalias 'yes-or-no-p 'y-or-n-p)


(if (eq system-type 'darwin)
    (progn
     (message "Emacs running in Mac OS")
     (exec-path-from-shell-initialize)
     (setq mac-command-modifier 'meta)
     (setq vterm-shell "/bin/zsh")
     )
  )



;;notified if the definition of a function you are customizing change
(use-package el-patch
  :straight t)

;; enable evil mode
(use-package evil
  :straight t
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
  :straight t
  :after evil
  :config
  (evil-collection-init))


;; (add-to-list 'ibuffer-never-show-predicates "^\\*")
(require 'ibuf-ext)
(setq ibuffer-saved-filter-groups
      (quote (("default"
	 ("Dotfiles" (or (filename . "^\\.")))
	 ("Messages" (or (name     . "^\\*")))
	 )
	)
       )
      )

(add-hook
 'ibuffer-mode-hook
 (lambda ()
   (ibuffer-switch-to-saved-filter-groups "default")))


(use-package hydra
  :straight t)
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(use-package general
  :straight t
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
