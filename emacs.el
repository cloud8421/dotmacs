(require 'package)
(package-initialize)

;; add sources
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))

;; add dependencies
(defvar my-packages '(evil evil-leader
                      evil-surround evil-nerd-commenter
                      evil-tabs evil-paredit
                      projectile
                      monokai-theme gruvbox-theme
                      powerline powerline-evil
                      js2-mode
                      elm-mode
                      markdown-mode
                      scss-mode
                      rainbow-mode
                      company
                      elixir-mode alchemist
                      magit
                      cider nrepl-eval-sexp-fu
                      exec-path-from-shell
                      rainbow-delimiters highlight
                      paredit smartparens
                      recentf
                      dash-at-point
                      org-journal
                      helm helm-ag))

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

;; hook $PATH
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; start evil mode
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-mode t)
(global-evil-surround-mode 1)
(global-evil-tabs-mode t)

;; hook helm, so that it's used everywhere
(helm-mode t)
(recentf-mode t)

;; hook projectile
(projectile-global-mode)

;; Hook autocomplete
(add-hook 'after-init-hook 'global-company-mode)
(setq company-dabbrev-downcase nil
      company-idle-delay 0)

;; Disable evil with magit
(add-hook 'magit-mode-hook 'turn-off-evil-mode)
(add-hook 'magit-popup-mode-hook 'turn-off-evil-mode)

;; Disable evil with sql
(add-hook 'sql-mode 'turn-off-evil-mode)

;; disable backup and autosave
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Disable electric mode
(electric-indent-mode -1)

;; Remove trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Show matching parentheses
(show-paren-mode)

;; Soft tabs
(setq-default indent-tabs-mode nil)
(setq c-basic-offset 2)
(setq css-indent-offset 2)

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
  "w" 'whitespace-mode ;; toggles whitespace mode
  "s" 'helm-mini ;; switch between buffers
  "/" 'helm-find-files ;; generic file finder
  "c" 'evilnc-comment-or-uncomment-lines
  "d" 'dash-at-point
  "j" 'org-journal-new-entry
  "g" 'magit-status)

(evil-leader/set-key-for-mode 'clojure-mode "e" 'cider-eval-last-sexp)

(evil-leader/set-key-for-mode 'org-journal-mode "q" 'org-set-tags-command)
(evil-leader/set-key-for-mode 'org-journal-mode "h" 'org-journal-previous-entry)
(evil-leader/set-key-for-mode 'org-journal-mode "l" 'org-journal-next-entry)

(defadvice cider-last-sexp (around evil activate)
  "In normal-state or motion-state, last sexp ends at point."
  (if (or (evil-normal-state-p) (evil-motion-state-p))
      (save-excursion
        (unless (or (eobp) (eolp)) (forward-char))
        ad-do-it)
    ad-do-it))

;; Javascript
(add-to-list 'auto-mode-alist '("\\.jsx$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; CSS & SCSS
(defun all-css-modes() (css-mode) (rainbow-mode))
(defun all-scss-modes() (scss-mode) (rainbow-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . all-css-modes))
(add-to-list 'auto-mode-alist '("\\.scss$" . all-scss-modes))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("52706f54fd3e769a0895d1786796450081b994378901d9c3fb032d3094788337" "05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" default)))
 '(elm-indent-offset 2)
 '(inhibit-startup-screen t)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
 '(nrepl-hide-special-buffers t)
 '(nrepl-popup-stacktraces-in-repl t)
 '(recentf-max-saved-items 50))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.0))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.0))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.0))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.0))))
 '(org-level-1 ((t (:inherit variable-pitch :foreground "#FD971F" :height 1.0))))
 '(org-level-2 ((t (:inherit variable-pitch :foreground "#A6E22E" :height 1.0))))
 '(org-level-3 ((t (:inherit variable-pitch :foreground "#66D9EF" :height 1.0))))
 '(org-level-4 ((t (:inherit variable-pitch :foreground "#E6DB74" :height 1.0))))
 '(outline-1 ((t nil)))
 '(outline-2 ((t nil)))
 '(outline-3 ((t nil)))
 '(outline-4 ((t nil)))
 '(variable-pitch ((t nil))))

;; lisp
(defun standard-lisp-modes ()
  (require 'nrepl-eval-sexp-fu)
  (rainbow-delimiters-mode t)
  (require 'evil-paredit)
  (paredit-mode t)
  (evil-paredit-mode t)
  (local-set-key (kbd "RET") 'newline-and-indent))

;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (standard-lisp-modes)))

;; Elixir
(require 'elixir-mode)
(require 'alchemist)

(evil-leader/set-key-for-mode 'elixir-mode "]" 'alchemist-goto-definition-at-point)
(evil-leader/set-key-for-mode 'elixir-mode "h" 'alchemist-help-search-at-point)

(add-hook 'alchemist-help-minor-mode-hook 'turn-off-evil-mode)

;; Clojure
(add-hook 'clojure-mode-hook
          '(lambda ()
             (standard-lisp-modes)

             (mapc '(lambda (char)
                      (modify-syntax-entry char "w" clojure-mode-syntax-table))
                   '(?- ?_ ?/ ?< ?> ?: ?' ?.))
             (require 'cider-test)

             (add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)))

;; Journal
(setq org-journal-dir "~/Dropbox/Journal/")

;; fonts
(defvar ui-font "Source Code Pro-12")
(add-to-list 'default-frame-alist '(font . "Source Code Pro-12"))
(set-frame-font "Source Code Pro-12" nil t)

;; ui
(setq initial-frame-alist '((top . 0) (left . 0) (width . 140) (height . 40)))
(tool-bar-mode -1)
(powerline-evil-vim-color-theme)
(global-linum-mode)

;; battery
(setq battery-mode-line-format "%t")
(display-battery-mode)

;; theme
(load-theme 'gruvbox)
