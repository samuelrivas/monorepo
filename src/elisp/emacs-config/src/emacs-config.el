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
(if (string-equal system-type "linux")
    nil
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "chromium-browser"))

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

(defvar helm-completing-read-handlers-alist)
(global-set-key (kbd "C-x C-d") 'helm-browse-project)
(add-to-list 'helm-completing-read-handlers-alist
             '(org-capture . helm-org-completing-read-tags))
(add-to-list 'helm-completing-read-handlers-alist
             '(org-set-tags . helm-org-completing-read-tags))

;; LaTeX mode
(defvar eglot-mode-map)
(defvar eglot-ignored-server-capabilities)
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
  (require 'copilot)
  (copilot-mode t)
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

;; Octave mode
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; Groovy mode
(defun my-groovy-mode-hook ()
  (auto-fill-mode)
  (flyspell-prog-mode))

(add-hook 'groovy-mode-hook 'my-groovy-mode-hook)

;; Copilot mode
(defvar copilot-mode-map)
(defun my-copilot-mode-hook ()
  (add-to-list 'warning-suppress-types '(copilot copilot-no-mode-indent))
  (add-to-list 'warning-suppress-types '(copilot copilot-exceeds-max-char))
  (define-key copilot-mode-map (kbd "M-o") 'copilot-accept-completion)
  (define-key copilot-mode-map (kbd "M-n") 'copilot-next-completion)
  (define-key copilot-mode-map (kbd "M-p") 'copilot-previous-completion))

(add-hook 'copilot-mode-hook 'my-copilot-mode-hook)

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
