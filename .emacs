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
:init (evil-mode 1))
