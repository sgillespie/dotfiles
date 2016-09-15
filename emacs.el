;;; Start the emacs server
(add-hook 'after-init-hook 'server-start)

;;; Global Settings
(require 'compile)
(require 'tramp)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["black" "red" "green4" "yellow4" "blue3" "magenta4" "cyan4" "white"])
 '(auto-save-default nil)
 '(auto-save-mode nil)
 '(backup-directory-alist (quote (("." . "~/.emacs_backups"))))
 '(compilation-environment (quote ("TERM=xterm-256color")))
 '(compile-command "./gradle.sh build")
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(fill-column 90)
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(js2-allow-member-expr-as-function-name t)
 '(js2-basic-offset 2)
 '(js2-missing-semi-one-line-override t)
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(js2-strict-cond-assign-warning nil)
 '(js2-strict-inconsistent-return-warning nil)
 '(js2-strict-missing-semi-warning nil)
 '(js2-strict-trailing-comma-warning nil)
 '(js2-strict-var-hides-function-arg-warning nil)
 '(js2-strict-var-redeclaration-warning nil)
 '(make-backup-files nil)
 '(neo-show-hidden-files t)
 '(nxml-child-indent 2)
 '(package-archives
   (quote
    (("melpa" . "http://melpa.org/packages/")
     ("elpa" . "http://elpa.gnu.org/packages/"))))
 '(speedbar-frame-parameters
   (quote
    ((width . 50)
     (minibuffer)
     (border-width . 0)
     (menu-bar-lines . 0)
     (tool-bar-lines . 0)
     (unsplittable . t)
     (left-fringe . 0))))
 '(speedbar-show-unknown-files t)
 '(speedbar-update-flag nil)
 '(sr-speedbar-auto-refresh nil)
 '(truncate-lines t)
 '(web-mode-indent-offset 2)
 '(x-select-enable-clipboard t))

(add-to-list 'load-path "~/.emacs.d/elisp")

;;; Packages
(require 'package)
(package-initialize)

(defvar install-packages '(speedbar
                           sr-speedbar

                           ; Themes
			   color-theme
                           color-theme-sanityinc-tomorrow
			   color-theme-sanityinc-solarized

                           ; Vim
                           evil

                           ; languages
                           haskell-mode
			   ghc
                           groovy-mode

                           ; javascript
                           flycheck
                           js2-mode
                           json-mode
                           web-mode

                           ; Misc
                           exec-path-from-shell
                           find-file-in-repository
                           ido-ubiquitous
                           smex))



(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (p install-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;; Mode-specific Settings
(add-to-list 'auto-mode-alist '("capfile" . ruby-mode))

;; Haskell mode
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'haskell-key-bindings)
(add-hook 'haskell-cabal-mode-hook 'haskell-key-bindings)

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;; Make shell mode nicer
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

; Haskell local bindings
(defun haskell-process-cabal-test ()
  "Test the Cabal project"
  (interactive)
  (haskell-process-do-cabal "test --show-details=failures")
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
(when window-system
  (exec-path-from-shell-initialize))

;;; Key Bindings
(require 'evil)
(evil-mode 1)

(add-hook 'neotree-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") 'neotree-enter))) 

(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(global-set-key (kbd "C-c C-r") 'recompile)
(global-set-key (kbd "C-c C-l") 'visual-line-mode)

(setq site-el "/usr/share/emacs/site-lisp/")
(setq home-el "~/.emacs.d/")

;;; Colors
(require 'color-theme)
(color-theme-sanityinc-tomorrow-night)

;;; Flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

(flycheck-add-mode 'javascript-eslint 'js-mode)
(flycheck-add-mode 'javascript-eslint 'js2-mode)
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
	  '(javascript-jshint)))

;;; Compilation
(add-hook 'compilation-mode-hook
          (lambda () (font-lock-mode -1)))

;;; Groovy
(autoload 'groovy-mode "groovy-mode" "Groovy editing mode." t)
(add-to-list 'auto-mode-alist '("\\.groovy$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\\.gradle$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;;; Javascript
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("package\\.json" . json-mode))
(add-to-list 'auto-mode-alist '("\\.babelrc" . json-mode))
(add-hook 'js2-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") 'newline-and-indent)
            (set (make-local-variable 'electric-indent-functions)
                 (list (lambda (arg)
                         'no-indent)))))

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

;;; neotree
(require 'neotree)
(add-hook 'neotree-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
              (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

(global-set-key (kbd "C-c n") 'neotree-toggle)

;;; speedbar
(require 'speedbar)
(require 'sr-speedbar)

(defun sr-speedbar-get-focus ()
  (interactive)
  (progn
    (sr-speedbar-open)
    (sr-speedbar-select-window)))

(global-set-key "\C-c\C-s" 'sr-speedbar-toggle)
(global-set-key "\C-cs" 'sr-speedbar-get-focus)

(add-hook 'speedbar-visiting-file-hook 'sr-speedbar-close t)

;;; fonts
(add-to-list 'default-frame-alist '(font . "Source Code Pro-14"))
