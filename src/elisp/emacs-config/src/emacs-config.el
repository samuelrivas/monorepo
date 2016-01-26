;; Disable packages (we use nix for dependencies)
(setq package-archives nil)
(package-initialize)

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
(global-set-key "\C-c\C-c" 'comment-region)
(global-set-key "\C-c\C-u" 'uncomment-region)
(global-set-key "\M-g" 'goto-line)

;; Global variables
(setq-default fill-column 80)

(setq inhibit-splash-screen t)

(setq-default indent-tabs-mode nil)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium-browser")

(setq backup-by-copying t)
(setq backup-directory-alist '(("." . "~/.emacs-backups")))
(setq delete-old-versions t)
(setq kept-new-versions 10)
(setq kept-old-versions 0)
(setq version-control t)

;; Whitespace mode
(global-whitespace-mode t)
(setq whitespace-line-column 80)
(setq whitespace-style '(face trailing empty tabs lines-tail))

;; LaTeX mode
(defun my-latex-mode-hook ()
  "LaTeX mode hook"
  (local-set-key "\C-c\C-c" 'comment-region)
  (local-set-key "\C-c\C-u" 'uncomment-region)
  (flyspell-mode t)
  (ispell-change-dictionary "british")
  (auto-fill-mode t))

(add-hook 'LaTeX-mode-hook 'my-latex-mode-hook)

;; C mode
(defun my-c-mode-hook ()
  (flyspell-prog-mode)
  (auto-fill-mode)
  (setq indent-tabs-mode nil)
  (setq comment-start "//")
  (setq comment-end "")
  (local-set-key "\C-c\C-u" 'uncomment-region))

(add-hook 'c-mode-hook 'my-c-mode-hook)

;; Javascript mode
(defun my-javascript-mode-hook ()
  (flyspell-prog-mode)
  (auto-fill-mode)
  (setq indent-tabs-mode nil)
  (local-set-key "\C-c\C-c" 'comment-region)
  (local-set-key "\C-c\C-u" 'uncomment-region))

(add-hook 'js-mode-hook 'my-javascript-mode-hook)

;; Text mode
(defun my-text-mode-hook ()
  (ispell-change-dictionary "british")
  (auto-fill-mode t)
  (flyspell-mode t))

(add-hook 'text-mode-hook 'my-text-mode-hook)

;; ORG
(setq org-catch-invisible-edits 'error)

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
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(custom-set-variables
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t))

(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

(defun my-haskell-mode-hook ()
  (turn-on-haskell-indentation)
  (flycheck-mode)

  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-c") 'comment-region)
  (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space))

;; Ocaml mode
(setq auto-mode-alist
      (append '(("\\.ml[i]?\'" . tuareg-mode)
                ("\\.topml\'" . tuareg-mode))
              auto-mode-alist))

(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
(add-hook 'tuareg-mode-hook 'my-tuareg-mode-hook)

(defun my-tuareg-mode-hook ()
  (require 'merlin)
  (require 'ocp-indent)
  (setq utop-command "utop -emacs")
  (utop-setup-ocaml-buffer)
  (merlin-mode)
  (flyspell-prog-mode)

  (setq merlin-use-auto-complete-mode t)

  (define-key merlin-mode-map
    (kbd "C-c C-n") 'merlin-error-next)
  (define-key merlin-mode-map
    (kbd "C-c <up>") 'merlin-type-enclosing-go-up)
  (define-key merlin-mode-map
    (kbd "C-c <down>") 'merlin-type-enclosing-go-down))

;; Nix mode
(autoload 'nix-mode "nix-mode" "nix-mode" t)
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))

;; Scala mode
(autoload 'scala-mode "scala-mode2" "scala-mode" t)
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))

(defun my-scala-mode-hook ()
  (setq whitespace-line-column 120))
(add-hook 'scala-mode-hook 'my-scala-mode-hook)

;; Octave mode
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
