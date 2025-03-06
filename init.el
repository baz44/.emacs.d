
;; =================================
;; Emacs settings for Basel Farah
;; =================================
;;
;;
;; Copyright 2015 Basel Farah
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; =================================
;; Start up
;; =================================

;; no need to see the start up screen
(setq inhibit-startup-screen t)

(setq image-types '(svg png gif tiff jpeg xpm xbm pbm))

;; disable the menu-bar
(menu-bar-mode 0)

(when (display-graphic-p)
  ;; enable the tool-bar in UI mode
  (setq tool-bar-mode -1)

  ;; disable the scroll-bar
  (setq scroll-bar-mode -1)

  ;; disable toolbar
  (tool-bar-mode -1))

;; set font size
(set-face-attribute
 'default nil
 :height (if (= (display-pixel-width) 1280)
             150
           250))

(display-pixel-width)


;; fix the # key
(fset 'insertPound "#")
(define-key global-map "\M-3" 'insertPound)

;; show column number
(column-number-mode)

;; show matching pairs of parentheses
(show-paren-mode)

;; remove whitespaces before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; set the default tab width
(setq default-tab-width 2)

;; maximum distance to scan for a matching parenthesis before giving up
(setq blink-matching-paren-distance nil)

;; no bell please
(setq visible-bell 1)

;; =================================
;; self installed packages
;; =================================
(add-to-list 'load-path "~/.emacs.d/self_installs/")


;; =================================
;; Package
;; =================================
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))


;; helper function to install a package and then install requiring it
(defun install-package-and-require (p)
  (unless (package-installed-p p)
    (package-install p))
    (require p))

;; =================================
;; use-package
;; =================================
(install-package-and-require 'use-package)

;; =================================
;; color-theme-sanityinc-solarized
;; =================================
(install-package-and-require 'color-theme-sanityinc-solarized)


;; =================================
;; fill-column-indicator
;; =================================
(install-package-and-require 'fill-column-indicator)

;; show fci after x chars
(setq fci-rule-column 80)

;; fci char
(setq fci-rule-character ?X)

;; auto enable fci for these major modes
(add-hook 'clojure-mode-hook 'fci-mode)
(add-hook 'ruby-mode-hook 'fci-mode)


;; =================================
;; undo-tree-mode
;; =================================
(install-package-and-require 'undo-tree)
(global-undo-tree-mode t)


;; =================================
;; rgrep
;; =================================
(global-set-key (kbd "C-c C-f") 'rgrep)



;; =================================
;; magit
;; =================================
(install-package-and-require 'magit)
(global-set-key (kbd "C-c C-g") 'magit-status)



;; =================================
;; golden ratio
;; =================================
(install-package-and-require 'golden-ratio)
(golden-ratio-mode 1)



;; =================================
;; ido
;; =================================
(install-package-and-require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

(install-package-and-require 'ido-vertical-mode)
(ido-vertical-mode 1)

(global-set-key
 "\M-x"
 (lambda ()
   (interactive) (call-interactively
                  (intern (ido-completing-read "M-x " (all-completions "" obarray 'commandp))))))

(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
            (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)))


;; =================================
;; expand region
;; =================================
(install-package-and-require 'expand-region)
(global-set-key (kbd "C-\\") 'er/expand-region)



;; =================================
;; comments
;; =================================
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)



;; =================================
;; auto revert
;; =================================
(global-auto-revert-mode t)

;; =================================
;; Cider
;; =================================
(install-package-and-require 'cider)
(setq cider-repl-wrap-history t)
(setq cider-repl-history-size 1000)
(setq cider-repl-history-file "~/.emacs.d/cider-history")
(setq nrepl-use-ssh-fallback-for-remote-hosts t)



;; =================================
;; eldoc
;; =================================
(add-hook 'cider-mode-hook 'eldoc-mode)
(add-hook 'cider-repl-mode-hook 'eldoc-mode)



;; =================================
;; prettify-symbols-mode
;; =================================
(add-hook 'clojure-mode-hook 'prettify-symbols-mode)
(add-hook 'lisp-mode-hook 'prettify-symbols-mode)



;; =================================
;; hl-sexp
;; =================================
(require 'hl-sexp)
(add-hook 'clojure-mode-hook 'hl-sexp-mode)
(add-hook 'lisp-mode-hook 'hl-sexp-mode)
(add-hook 'scheme-mode-hook 'hl-sexp-mode)
(add-hook 'emacs-lisp-mode-hook 'hl-sexp-mode)



;; =================================
;; idle-highlight-mode
;; =================================
(install-package-and-require 'idle-highlight-mode)
(add-hook 'clojure-mode-hook 'idle-highlight-mode)
(add-hook 'lisp-mode-hook 'idle-highlight-mode)
(add-hook 'scheme-mode-hook 'idle-highlight-mode)
(add-hook 'emacs-lisp-mode-hook 'idle-highlight-mode)



;; =================================
;; backup files
;; =================================
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))



;; =================================
;; save places
;; =================================
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name "places" user-emacs-directory))



;; =================================
;; Multiple cursors
;; =================================
(install-package-and-require 'multiple-cursors)
(global-set-key (kbd "C-c .") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c ,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c M-.") 'mc/mark-all-like-this)



;; =================================
;; smartparens
;; =================================
(install-package-and-require 'smartparens)
(smartparens-global-mode t)
(sp-use-paredit-bindings)
(sp-pair "'" nil :actions :rem)
(add-hook 'lisp-mode-hook 'smartparens-strict-mode)
(add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
(add-hook 'scheme-mode-hook 'smartparens-strict-mode)
(add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
(add-hook 'clojure-mode-hook 'smartparens-strict-mode)



;; =================================
;; run ruby rpecs
;; =================================
(require 'dash)

(defun rspec-procject-dir ()
  (-reduce-from (lambda (acc v) (concat acc v))
                "/"
                (-interpose "/" (-take 4 (-drop 1 (split-string (buffer-file-name) "/"))))))

(defun run-rspec ()
  (interactive)
  (cd-absolute (rspec-procject-dir))
  (let ((cmd (concat "bundle exec rspec "
                     (buffer-file-name)
                     ":"
                     (car (cdr (split-string (what-line) " "))))))
    (async-shell-command cmd)))

(global-set-key (kbd "C-c C-r")  'run-rspec)


;; =================================
;; clojure indentation settings
;; =================================
(define-clojure-indent
  (defroutes 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (context 2))


;; =================================
;; set js indent level
;; =================================
(setq js-indent-level 2)



;; =================================
;; no tabs ever
;; =================================
(setq-default indent-tabs-mode nil)


;; =================================
;; yasnippet
;; =================================
(install-package-and-require 'yasnippet)
(install-package-and-require 'clojure-snippets)
(install-package-and-require 'yasnippet-snippets)
(yas-global-mode 1)


;; =================================
;; multi-scratch
;; =================================
(require 'multi-scratch)



;; =================================
;; projectile
;; =================================
(setq projectile-keymap-prefix (kbd "C-c p"))
(install-package-and-require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)




;; ================================
;; sql-indent
;; =================================
(install-package-and-require 'sql-indent)
(eval-after-load "sql"
  (load-library "sql-indent"))


;; ================================
;; deft
;; =================================
(install-package-and-require 'deft)
(setq deft-directory "~/iCloud/notes/")
(setq deft-extensions '("org"))
(setq deft-default-extension "org")
(setq deft-use-filename-as-title t)
(setq deft-use-filter-string-for-filename t)
(setq deft-auto-save-interval 0)


;; =================================
;; focus-mode
;; =================================
(install-package-and-require 'focus)



;; =================================
;; tramp support
;; =================================
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")
(require 'tramp)
(setq tramp-default-method "sshx")
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
;; Backup (file~) disabled and auto-save (#file#) locally to prevent delays in editing remote files
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))
(setq tramp-auto-save-directory temporary-file-directory)
(setq tramp-verbose 10)

;; Open files in Docker containers like so: /docker:drunk_bardeen:/etc/passwd
(push
 (cons
  "docker"
  '((tramp-login-program "docker")
    (tramp-login-args (("exec" "-it") ("%h") ("/bin/bash")))
    (tramp-remote-shell "/bin/sh")
    (tramp-remote-shell-args ("-i") ("-c"))))
 tramp-methods)

(defadvice tramp-completion-handle-file-name-all-completions
  (around dotemacs-completion-docker activate)
  "(tramp-completion-handle-file-name-all-completions \"\" \"/docker:\" returns
    a list of active Docker container names, followed by colons."
  (if (equal (ad-get-arg 1) "/docker:")
      (let* ((dockernames-raw (shell-command-to-string "docker ps | awk '$NF != \"NAMES\" { print $NF \":\" }'"))
             (dockernames (cl-remove-if-not
                           #'(lambda (dockerline) (string-match ":$" dockerline))
                           (split-string dockernames-raw "\n"))))
        (setq ad-return-value dockernames))
    ad-do-it))


;; =================================
;; ag
;; =================================
(install-package-and-require 'ag)


;; =================================
;; markdown-mode
;; =================================
(install-package-and-require 'markdown-mode)

;; =================================
;; exec-path-from-shell
;; =================================
(install-package-and-require 'exec-path-from-shell)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


;; =================================
;; indivisual command to move to projects
;; =================================
(let ((location "~/.emacs.d/projects_configs.el"))
  (when (file-exists-p location)
    (load-file location)))


;; =================================
;; imenu and imenu-anywhere
;; =================================
(setq imenu-auto-rescan t)
(add-hook 'clojure-mode-hook 'imenu-add-menubar-index)
(install-package-and-require 'imenu-anywhere)
(global-set-key (kbd "C-.") #'ido-imenu-anywhere)

;; =================================
;; doc-view settings
;; =================================
(setq doc-view-ghostscript-program "/usr/local/bin/gs")

;; =================================
;; flycheck
;; =================================
(install-package-and-require 'flycheck)
(global-flycheck-mode 1)


;; =================================
;; go-mode
;; =================================
(install-package-and-require 'go-mode)
(add-hook 'go-mode-hook (lambda ()
                          (setq tab-width 4)))

;; =================================
;; eglot
;; =================================
(install-package-and-require 'eglot)
(add-hook 'clojure-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'js-mode-hook 'eglot-ensure)
(add-hook 'typescript-mode-hook 'eglot-ensure)
(add-hook 'ruby-mode-hook 'eglot-ensure)

(use-package company
  :ensure t
)

(add-hook 'js-mode-hook 'company-mode)
(add-hook 'clojure-mode-hook 'company-mode)
(add-hook 'python-mode-hook 'company-mode)
(add-hook 'typescript-mode-hook 'company-mode)
(add-hook 'ruby-mode-hook 'company-mode)


;; ;; maps C-n and C-p instead of M-n M-p when selecting an option in company-mode
(define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
(define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)


(defun company-mode/backend-with-yas (backend)
  (if (and (listp backend) (member 'company-yasnippet backend))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

;; =================================
;; org-mode settings
;; =================================
(setq org-log-done 'time)


;; =================================
;; deft
;; =================================
(install-package-and-require 'deft)
(setq deft-directory "~/icloud/notes")
(setq deft-recursive t)


;; =================================
;; rust-mode
;; =================================
(install-package-and-require 'rust-mode)
(add-hook 'rust-mode-hook 'lsp-deferred)


;; =================================
;; flyspell
;; =================================
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))


(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; =================================
;; vterm
;; =================================
(use-package vterm
    :ensure t)


;; =================================
;; ein
;; =================================
(use-package ein
    :ensure t)


;; =================================
;; copilot
;; =================================
(install-package-and-require 'copilot)
(install-package-and-require 'company-box)

(defun rk/no-copilot-mode ()
  "Helper for `rk/no-copilot-modes'."
  (copilot-mode -1))

(defvar rk/no-copilot-modes '(shell-mode
                              inferior-python-mode
                              eshell-mode
                              term-mode
                              vterm-mode
                              comint-mode
                              compilation-mode
                              debugger-mode
                              dired-mode-hook
                              compilation-mode-hook
                              flutter-mode-hook
                              minibuffer-mode-hook)
  "Modes in which copilot is inconvenient.")

(defun rk/copilot-disable-predicate ()
  "When copilot should not automatically show completions."
  (or rk/copilot-manual-mode
      (member major-mode rk/no-copilot-modes)
      (company--active-p)))

(add-to-list 'copilot-disable-predicates #'rk/copilot-disable-predicate)

(defvar rk/copilot-manual-mode nil
  "When `t' will only show completions when manually triggered, e.g. via M-C-<return>.")

(defun rk/copilot-change-activation ()
  "Switch between three activation modes:
- automatic: copilot will automatically overlay completions
- manual: you need to press a key (M-C-<return>) to trigger completions
- off: copilot is completely disabled."
  (interactive)
  (if (and copilot-mode rk/copilot-manual-mode)
      (progn
        (message "deactivating copilot")
        (global-copilot-mode -1)
        (setq rk/copilot-manual-mode nil))
    (if copilot-mode
        (progn
          (message "activating copilot manual mode")
          (setq rk/copilot-manual-mode t))
      (message "activating copilot mode")
      (global-copilot-mode))))

(define-key global-map (kbd "M-C-<escape>") #'rk/copilot-change-activation)

(defun rk/copilot-complete-or-accept ()
  "Command that either triggers a completion or accepts one if one
is available. Useful if you tend to hammer your keys like I do."
  (interactive)
  (if (copilot--overlay-visible)
      (progn
        (copilot-accept-completion)
        (open-line 1)
        (next-line))
    (copilot-complete)))

(define-key copilot-mode-map (kbd "M-C-<next>") #'copilot-next-completion)
(define-key copilot-mode-map (kbd "M-C-<prior>") #'copilot-previous-completion)
(define-key copilot-mode-map (kbd "M-C-<right>") #'copilot-accept-completion-by-word)
(define-key copilot-mode-map (kbd "M-C-<down>") #'copilot-accept-completion-by-line)
(define-key global-map (kbd "M-C-<return>") #'rk/copilot-complete-or-accept)


;; =================================
;; copilot-chat
;; =================================
(install-package-and-require 'copilot-chat)


;; =================================
;; tree-sitter
;; =================================
(install-package-and-require 'tree-sitter)
(install-package-and-require 'tree-sitter-langs)


;; =================================
;; typescript-mode
;; =================================
(install-package-and-require 'typescript-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))




;; ==== Everything else
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(sanityinc-solarized-dark))
 '(custom-safe-themes
   '("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default))
 '(package-selected-packages
   '(typescript-mode tree-sitter-langs tree-sitter copilot-chat company-box copilot quelpa f editorconfig ein yasnippet-snippets vterm undo-tree sql-indent smartparens rust-mode projectile multiple-cursors markdown-mode magit imenu-anywhere ido-vertical-mode idle-highlight-mode golden-ratio go-mode focus flycheck fill-column-indicator expand-region exec-path-from-shell deft company color-theme-sanityinc-solarized clojure-snippets cider ag)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
