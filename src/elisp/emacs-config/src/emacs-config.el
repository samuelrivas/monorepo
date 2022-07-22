;; Disable packages (we use nix for dependencies)
(defvar package-archives)
(setq package-archives nil)
(package-initialize)

;; Disable customize (to avoid conflicts with this configuration)
(setq custom-file "/dev/null")

;; Useful functions that are not there by default
(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name)
                 (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
        (revert-buffer t t t))))
  (message "Refreshed open files."))

;; Enable the solarised theme, with dark background We assume that
;; color-theme-solarized is in the path and use that info to set
;; custom-theme-load-path
(add-to-list
 'custom-theme-load-path
 (file-name-directory
  (locate-file "color-theme-solarized" load-path '(".el" ".elc"))))

(setq frame-background-mode 'dark)
(load-theme 'solarized t)
(enable-theme 'solarized)

;; Minor modes
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode t)
(global-font-lock-mode t)
(transient-mark-mode t)
(delete-selection-mode t)
(show-paren-mode t)
(blink-cursor-mode -1)
(auto-fill-mode t)

;; Key bindings
(global-set-key "\M-g" 'goto-line)

;; Global variables
(setq-default fill-column 80)

(setq inhibit-splash-screen t)

(setq-default indent-tabs-mode nil)

(defvar browse-url-generic-program)
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium-browser")

(setq backup-by-copying t)
(setq backup-directory-alist '(("." . "~/.emacs-backups")))
(setq delete-old-versions t)
(setq kept-new-versions 10)
(setq kept-old-versions 0)
(setq version-control t)

(setq require-final-newline 'ask)
(setq mode-require-final-newline t)

;; Assorted modes
(add-to-list 'auto-mode-alist '("Makefile\\..*\\'" . makefile-mode))

;; Whitespace mode
(global-whitespace-mode t)
(defvar whitespace-line-column)
(defvar whitespace-style)
(setq whitespace-line-column 80)
(setq whitespace-style '(face trailing empty tabs lines-tail))

;; helm, projectile, etc
(helm-mode)
(projectile-mode)

(global-set-key (kbd "C-x C-d") 'helm-browse-project)
(add-to-list 'helm-completing-read-handlers-alist
             '(org-capture . helm-org-completing-read-tags))
(add-to-list 'helm-completing-read-handlers-alist
             '(org-set-tags . helm-org-completing-read-tags))

;; LaTeX mode
(defun my-latex-mode-hook ()
  "LaTeX mode hook"
  (yas-minor-mode)
  (company-mode)
  (flyspell-mode t)
  (ispell-change-dictionary "british")
  (auto-fill-mode t)
  (define-key eglot-mode-map (kbd "C-c l") 'eglot-code-actions)
  (add-to-list 'eglot-ignored-server-capabilities :hoverProvider))

(add-hook 'latex-mode-hook 'my-latex-mode-hook)
(add-hook 'latex-mode-hook 'eglot-ensure)

;; C/C++ mode
(defun my-c-mode-hook ()
  (flyspell-prog-mode)
  (auto-fill-mode)
  (setq indent-tabs-mode nil)
  (setq comment-start "//")
  (setq comment-end ""))

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)

;; Javascript mode
(defun my-javascript-mode-hook ()
  (flyspell-prog-mode)
  (auto-fill-mode)
  (setq indent-tabs-mode nil))

(add-hook 'js-mode-hook 'my-javascript-mode-hook)

;; Text mode
(defun my-text-mode-hook ()
  (ispell-change-dictionary "british")
  (auto-fill-mode t)
  (flyspell-mode t))

(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG\\'" . text-mode))

(add-hook 'text-mode-hook 'my-text-mode-hook)


;; Erlang mode
(autoload 'erlang-mode "erlang-start" "erlang-mode" t)
(add-to-list 'auto-mode-alist '("\\.[he]rl\\'" . erlang-mode))

(defun my-erlang-mode-hook ()
  "Erlang mode hook"
  (setq indent-tabs-mode nil)
  (auto-fill-mode)
  (flyspell-prog-mode)
  (ispell-change-dictionary "british"))

(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)


;; Haskell mode
(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)
(add-hook 'haskell-mode-hook 'eglot-ensure)
(defun my-haskell-mode-hook ()
  (yas-minor-mode)
  (company-mode)
  (flycheck-mode)
  (flyspell-prog-mode)
  (setq haskell-stylish-on-save t)
  (define-key eglot-mode-map (kbd "C-c l") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key eglot-mode-map (kbd "M-p") 'flymake-goto-prev-error))

;; Nix mode
(autoload 'nix-mode "nix-mode" "nix-mode" t)
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))

;; Octave mode
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; Groovy mode
(defun my-groovy-mode-hook ()
  (auto-fill-mode)
  (flyspell-prog-mode))

(add-hook 'groovy-mode-hook 'my-groovy-mode-hook)
