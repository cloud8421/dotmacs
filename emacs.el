(require 'package)
(package-initialize)

;; add sources
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))

;; add dependencies
(defvar my-packages '(evil
		      evil-leader
		      evil-surround
		      projectile
		      monokai-theme
		      powerline
		      powerline-evil
		      js2-mode
		      auto-complete
		      helm))

;; figure out what's missing
(defun my-missing-packages ()
  (let (missing-packages)
    (dolist (package my-packages (reverse missing-packages))
      (or (package-installed-p package)
          (push package missing-packages)))))

;; make sure that everything's installed
(defun ensure-my-packages ()
  (let ((missing (my-missing-packages)))
    (when missing
      ;; Check for new packages (package versions)
      (package-refresh-contents)
      ;; Install the missing packages
      (mapc (lambda (package)
              (when (not (package-installed-p package))
                (package-install package)))
            missing)
      ;; Close the compilation log.
      (let ((compile-window (get-buffer-window "*Compile-Log*")))
        (if compile-window
          (delete-window compile-window))))))

(ensure-my-packages)

;; start evil mode
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-mode t)
(global-evil-surround-mode 1)

;; hook helm, so that it's used everywhere
(helm-mode t)
(recentf-mode t)

;; hook projectile
(projectile-global-mode)

;; Hook autocomplete
(global-auto-complete-mode t)

;; disable backup and autosave
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Pound sign
(global-set-key (kbd "M-3") '(lambda()(interactive)(insert "#"))) 

;; Leader shortcuts
(evil-leader/set-key
  "1" 'projectile-find-file ;; find files
  "2" 'projectile-switch-project
  "i" 'package-install
  "b" 'eval-buffer ;; lisp eval entire buffer
  "e" 'eval-last-sexp ;; eval expression under cursor
  "m" 'helm-M-x ;; better replacement for M-x
  "/" 'helm-find-files) ;; generic file finder

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-show-menu t)
 '(ac-auto-start t)
 '(ac-show-menu-immediately-on-auto-complete t)
 '(custom-safe-themes
   (quote
    ("05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" default))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; fonts
(defvar ui-font "Source Code Pro-12")
(add-to-list 'default-frame-alist '(font . "Source Code Pro-12"))
(set-frame-font "Source Code Pro-12" nil t)

;; ui
(tool-bar-mode -1)
(powerline-evil-vim-color-theme)

;; theme
(load-theme 'monokai)
