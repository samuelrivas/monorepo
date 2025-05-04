;;; -*- lexical-binding: t -*-

;; Disable packages (we use nix for dependencies)
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
                  browse-url-generic-program "xdg-open")))


(use-package text-mode
  :mode ("COMMIT_EDITMSG\\'" . text-mode)
  :hook (text-mode . auto-fill-mode))

(use-package whitespace
  :config
  (global-whitespace-mode t)
  (setq-default
   whitespace-line-column 80

   whitespace-style
   '(face trailing empty tabs lines-char missing-newline-at-eof)))

(use-package marginalia
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :init (marginalia-mode))

(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ("M-g e" . consult-compile-error)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history)))

(use-package corfu
  :init
  (global-corfu-mode)
  :config
  (setq corfu-auto t))

(use-package corfu-history
  :config
  (corfu-history-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic)))

(use-package vertico
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

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

(use-package haskell-mode
  :hook (haskell-mode . eglot-ensure)
  :config
  (setq haskell-stylish-on-save t))

(use-package nix-mode
  :hook (nix-mode . eglot-ensure))

(use-package terraform-mode
  :hook (terraform-mode . eglot-ensure))

(use-package python-mode
  :hook (python-mode . eglot-ensure))
