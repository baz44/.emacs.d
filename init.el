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

;; disable the menu-bar
(menu-bar-mode 1)


(when (display-graphic-p)
  ;; disable the tool-bar
  (setq tool-bar-mode -1)

  ;; disable the scroll-bar
  (setq scroll-bar-mode -1)

  ;; disable toolbar
  (tool-bar-mode -1))

;; set font size
(set-face-attribute 'default nil :height 150)

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
             '("melpa" . "https://stable.melpa.org/packages/") t)

(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(defun install-package-and-require (p)
  (unless (package-installed-p p)
    (package-install p))
    (require p))



;; =================================
;; theme
;; =================================
(install-package-and-require 'ample-theme)
;; (load-theme 'ample-flat t t)
;; (load-theme 'ample t t)
;; (load-theme 'ample-light t t)
;; (enable-theme 'ample-flat)
;; ;; change the highlight colour
(set-face-attribute 'region nil :background "#FFF")



;; =================================
;; color-theme-tomorrow
;; located in ~/.emacs.d/self_installs/
;; available options: night, day, night-eighties, night-blue and night-bright
;; =================================
(require 'color-theme-tomorrow)
(color-theme-tomorrow--define-theme night-bright)




;; =================================
;; Linum
;; =================================
(global-linum-mode t)

;; linum format
(setq linum-format "%d ")

;; list of modes that we don't want to show linum for
(setq linum-disabled-modes-list '(eshell-mode
				  wl-summary-mode
				  compilation-mode
				  cider-repl-mode))

(defun linum-on ()
  (unless (or (minibufferp)
	      (member major-mode linum-disabled-modes-list)
	      (string-match "*" (buffer-name)))
    (linum-mode 1)))



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
(install-package-and-require 'hl-sexp)
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
;; align-cljlet
;; =================================
(install-package-and-require 'align-cljlet)
(global-set-key (kbd "C-c C-a") 'align-cljlet)



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
(install-package-and-require 'clojure-cheatsheet)



;; =================================
;; set js indent level
;; =================================
(setq js-indent-level 2)



;; =================================
;; no tabs ever
;; =================================
(setq-default indent-tabs-mode nil)



;; =================================
;; git-blame
;; =================================
(install-package-and-require 'git-blame)



;; =================================
;; yasnippet
;; =================================
(install-package-and-require 'yasnippet)
(install-package-and-require 'clojure-snippets)
(yas-global-mode 1)


;; =================================
;; multi-scratch
;; =================================
(require 'multi-scratch)



;; =================================
;; projectile
;; =================================
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
(setq deft-extension "org")
(setq deft-text-mode 'org-mode)
(setq deft-directory "~/iCloud/notes/")
(setq deft-recursive t)
(setq deft-use-filename-as-title t)
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
;; nyan-mode
;; =================================
(install-package-and-require 'nyan-mode)
(nyan-mode t)
(setq nyan-animate-nyancat t)



;; =================================
;; zone-nyan
;; =================================
(install-package-and-require 'zone-nyan)
(setq zone-nyan-term-type 'ascii)
(setq zone-nyan-gui-type 'text)
(setq zone-programs [zone-nyan])
;; screen save timer
(require 'zone)
(zone-when-idle 1200)


;; =================================
;; markdown-mode
;; =================================
(install-package-and-require 'markdown-mode)
(custom-set-variables
 '(markdown-command "/usr/local/bin/markdown"))



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
;; go-mode
;; =================================
(install-package-and-require 'go-mode)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (focus focus-mode sql-indent projectile clj-refactor clojure-snippets yasnippet git-blame clojure-cheatsheet align-cljlet smartparens multiple-cursors idle-highlight-mode hl-sexp ac-cider cider auto-complete expand-region golden-ratio magit git-gutter undo-tree fill-column-indicator ample-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
