;;; -*- lexical-binding: t -*-

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

;; Packages and major modes
;; ========================
(use-package emacs
  :config
  ;; Colour theme
  ;; ============
  (setq frame-background-mode 'dark)
  (load-theme 'solarized t)
  (enable-theme 'solarized)

  ; Minor modes
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (column-number-mode t)
  (global-font-lock-mode t)
  (transient-mark-mode t)
  (delete-selection-mode t)
  (show-paren-mode t)
  (blink-cursor-mode -1)
  (global-hl-line-mode t)

  ;; Global variables
  ;; ================
  (setq-default
   fill-column 80
   indent-tabs-mode nil
   inhibit-splash-screen t
   backup-by-copying t
   backup-directory-alist '(("." . "~/.emacs-backups"))
   delete-old-versions t
   kept-new-versions 10
   kept-old-versions 0
   version-control t
   require-final-newline 'ask
   mode-require-final-newline t)

  (if (string-equal system-type "darwin")
      nil
    (setq-default browse-url-browser-function 'browse-url-generic
                  browse-url-generic-program "xdg-open"))

  ;; Global hooks
  ;; ============
  :hook (text-mode . auto-fill-mode))

(use-package text-mode
  :mode ("COMMIT_EDITMSG\\'" . text-mode))

(use-package whitespace
  :config
  (global-whitespace-mode t)
  (setq-default
   whitespace-line-column 80
   whitespace-style '(face trailing empty tabs lines-tail missing-newline-at-eof)))

(use-package projectile
  :config projectile-mode)

(use-package helm
  :config (helm-mode)
  :bind ("C-x C-d" . helm-browse-project))

(use-package cc-mode
  :hook (c-mode . (lambda () (c-toggle-comment-style -1))))

(use-package flyspell
  :hook
  ((prog-mode . flyspell-prog-mode)
   (text-mode . flyspell-mode)))

(use-package eglot
  :bind (:map eglot-mode-map
              ("C-c l" . eglot-code-actions)
              ("M-n" . flymake-goto-next-error)
              ("M-p" . 'flymake-goto-prev-error)))

(use-package auctex
  :hook (LaTeX-mode . eglot-ensure))

(use-package copilot
  :bind (:map copilot-mode-map
              ("M-o" . copilot-accept-completion-by-word))
  :config
  (add-to-list 'warning-suppress-types '(copilot copilot-no-mode-indent))
  (add-to-list 'warning-suppress-types '(copilot copilot-exceeds-max-char))

  :hook (text-mode . copilot-mode))

(use-package ispell
  :config
  (setq-default ispell-dictionary "british"))

;; Legacy
;; ======

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
(defvar haskell-stylish-on-save)
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
(add-hook 'nix-mode-hook 'my-nix-mode-hook)
(add-hook 'nix-mode-hook 'eglot-ensure)
(defun my-nix-mode-hook ()
  (require 'copilot)
  (copilot-mode t)
  (define-key eglot-mode-map (kbd "C-c l") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key eglot-mode-map (kbd "M-p") 'flymake-goto-prev-error))

;; Terraform mode
(add-hook 'terraform-mode-hook 'eglot-ensure)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(terraform-mode . ("terraform-ls" "serve"))))

;; Go mode
;; (require 'project)

(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)
(add-hook 'go-mode-hook 'eglot-ensure)

;; Optional: install eglot-format-buffer as a save hook.
;; The depth of -10 places this before eglot's willSave notification,
;; so that that notification reports the actual contents that will be saved.
(defun eglot-format-buffer-before-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
(add-hook 'go-mode-hook #'eglot-format-buffer-before-save)

;; Python mode
(add-hook 'python-mode-hook 'eglot-ensure)
