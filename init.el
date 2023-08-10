
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
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))

(add-to-list 'package-archives
             '("melpa" . "https://stable.melpa.org/packages/") t)



(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; helper function to install a package and then install requiring it
(defun install-package-and-require (p)
  (unless (package-installed-p p)
    (package-install p))
    (require p))


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
;; git-gutter
;; =================================
(install-package-and-require 'git-gutter)
(global-git-gutter-mode t)



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
;; auto complete
;; =================================
(install-package-and-require 'auto-complete)
(ac-config-default)
(global-auto-complete-mode t)
(setq ac-auto-show-menu t)
(setq ac-dwim t)
(setq ac-use-menu-map t)
(setq ac-delay 0.3)
(setq ac-quick-help-delay 1)
(setq ac-quick-help-height 60)



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
;; ac-cider
;; =================================
(install-package-and-require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode)))



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
;; clj-refactor
;; =================================
(install-package-and-require 'clj-refactor)

(defun my-clojure-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)



;; =================================
;; clojure-cheatsheet
;; =================================
;(install-package-and-require 'clojure-cheatsheet)



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
;; ejc-sql
;; =================================
;; (install-package-and-require 'ejc-sql)

;; (setq clomacs-httpd-default-port 8090)

;; (require 'ejc-autocomplete)

;; (add-hook 'ejc-sql-minor-mode-hook
;;           (lambda ()
;;             (auto-complete-mode t)
;;             (ejc-ac-setup)))

;;(setq ejc-use-flx nil)


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
;; lsp-mode
;; =================================
(install-package-and-require 'lsp-mode)

(setq package-selected-packages '(lsp-mode yasnippet lsp-treemacs helm-lsp
    projectile hydra flycheck company avy which-key helm-xref dap-mode lsp-ui))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  ;;(package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(which-key-mode)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode))

;; show inline warnings
(setq lsp-ui-sideline-enable t)

(require 'company)
;; maps C-n and C-p instead of M-n M-p when selecting an option in company-mode
(define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
(define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)

;; enable it for these languages
(add-hook 'go-mode-hook #'lsp)
(add-hook 'clojure-mode-hook #'lsp)

(setq display-time-mode t)


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
;; auctex
;; =================================
;;(install-package-and-require 'auctex)

;; =================================
;; lsp-java
;; =================================
;; (condition-case nil
;;     (require 'use-package)
;;   (file-error
;;    (require 'package)
;;    (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;;    (package-initialize)
;;    (package-refresh-contents)
;;    (package-install 'use-package)
;;    (setq use-package-always-ensure t)
;;    (require 'use-package)))

;; (use-package projectile)
 ;; (use-package flycheck)
 ;; (use-package yasnippet :config (yas-global-mode))
 ;; (use-package lsp-mode :hook ((lsp-mode . lsp-enable-which-key-integration))
 ;;   :config (setq lsp-completion-enable-additional-text-edit nil))
;; (use-package hydra)
;; (use-package company)
;; (use-package lsp-ui)
;; (use-package which-key :config (which-key-mode))
;; (use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
;; (use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
;; (use-package dap-java :ensure nil)
;; (use-package helm-lsp)
;; (use-package helm
;;   :config (helm-mode))
;; (use-package lsp-treemacs)

(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'set-goal-column 'disabled nil)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#cccccc" "#f2777a" "#99cc99" "#ffcc66" "#6699cc" "#cc99cc" "#66cccc" "#515151"))
 '(custom-enabled-themes '(sanityinc-solarized-dark))
 '(custom-safe-themes
   '("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default))
 '(fci-rule-color "#515151")
 '(markdown-command "/usr/local/bin/markdown")
 '(org-agenda-files '("~/iCloud/Private/tasks.org"))
 '(package-selected-packages
   '(rust-mode delight lsp-mode yasnippet lsp-treemacs helm-lsp projectile hydra flycheck company avy which-key helm-xref dap-mode lsp-ui)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
