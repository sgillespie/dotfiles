;;; Start the emacs server
(add-hook 'after-init-hook 'server-start)

;;; Global Settings
(require 'compile)
(require 'tramp)

(custom-set-variables
 '(ansi-color-names-vector ["black" "red" "green4" "yellow4" "blue3" 
			    "magenta4" "cyan4" "white"])
 '(auto-save-default nil)
 '(auto-save-mode nil)
 '(backup-directory-alist '(("." . "~/.emacs_backups")))
 '(compile-command "./gradle.sh build")
 '(fill-column 90)
 '(indent-tabs-mode t)
 '(js-indent-level 2)
 '(make-backup-files nil)
 '(nxml-child-indent 2)
 '(package-archives '(("melpa-stable" . "http://stable.melpa.org/packages/")
		      ("marmalade" . "http://marmalade-repo.org/packages/")
                      ("gnu" . "http://elpa.gnu.org/packages/")
                      ("melpa" . "http://melpa.org/packages/")))
 '(speedbar-frame-parameters
   '((width . 45)
     (minibuffer)
     (border-width . 0)
     (menu-bar-lines . 0)
     (tool-bar-lines . 0)
     (unsplittable . t)
     (left-fringe . 0)))
 '(speedbar-show-unknown-files t)
 '(speedbar-update-flag nil)
 '(truncate-lines t)
 '(x-select-enable-clipboard t))

;;; Mode-specific Settings
(add-to-list 'auto-mode-alist '("capfile" . ruby-mode))

;; Haskell mode
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'haskell-key-bindings)
(add-hook 'haskell-cabal-mode-hook 'haskell-key-bindings)

;; Make shell mode nicer
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;; Key Bindings
;; Disable these for now
; (global-set-key "\C-cr" 'recompile)
; (global-set-key "\C-cv" 'visual-line-mode)

; Haskell local bindings
(defun haskell-process-cabal-test ()
  "Test the Cabal project"
  (interactive)
  (haskell-process-do-cabal "build")
  (haskell-process-add-cabal-autogen))

(defun haskell-key-bindings ()
  (interactive)
  (local-set-key (kbd "C-c c") 'haskell-process-cabal)
  (local-set-key (kbd "C-c C-c") 'haskell-process-cabal-build)
  (local-set-key (kbd "C-c t") 'haskell-process-cabal-test))

(add-hook 'haskell-mode-hook 'haskell-key-bindings)
(add-hook 'haskell-cabal-mode-hook 'haskell-key-bindings)

;; Override disabled keys
(put 'downcase-region 'disabled nil)

;;; Load environment from shell
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

;;; Packages
(require 'package)
(package-initialize)

(defvar install-packages '(speedbar
			   color-theme
			   color-theme-solarized

                           ; languages
                           haskell-mode
			   ghc
                           groovy-mode
                           
                           ; clojure
                           starter-kit
                           starter-kit-lisp
                           starter-kit-bindings
                           starter-kit-eshell
                           clojure-mode
                           clojure-test-mode
                           cider))

(dolist (p install-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(setq site-el "/usr/share/emacs/site-lisp/")
(setq home-el "~/.emacs.d/")

;;; Colors
(require 'color-theme)
(color-theme-solarized-dark)

;;; Groovy
(autoload 'groovy-mode "groovy-mode" "Groovy editing mode." t)

(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

(add-hook 'java-mode-hook (lambda ()
			    (setq c-basic-offset 4
				  tab-width 4
				  indent-tabs-mode nil)))
			    

(add-hook 'groovy-mode-hook (lambda ()
			      (setq c-basic-offset 4
				    tab-width 4
				    indent-tabs-mode nil)))

;;; UI
(line-number-mode 1)
(column-number-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-hl-line-mode 1)
; (desktop-save-mode 1)

;;; magit
(global-set-key "\C-cg" 'magit-status)
;;; speedbar
(custom-set-variables
 '(speedbar-show-unknown-files t)
 '(speedbar-update-flag nil)
 '(speedbar-frame-parameters '((width . 45)
			       (minibuffer . nil)
			       (border-width . 0)
			       (menu-bar-lines . 0)
			       (tool-bar-lines . 0)
			       (unsplittable . t)
			       (left-fringe . 0))))

;; Disable bindings for now
(global-set-key "\C-cs" 'speedbar-get-focus)
(require 'speedbar)

;;; fonts
(add-to-list 'default-frame-alist '(font . "Source Code Pro-12"))

(put 'scroll-left 'disabled nil)
