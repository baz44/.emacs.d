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
(menu-bar-mode -1)

;; disable the tool-bar
(setq tool-bar-mode -1)

;; disable the scroll-bar
(setq scroll-bar-mode -1)

;; set font size
(set-face-attribute 'default nil :height 200)

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

;; change the highlight colour
(set-face-attribute 'region nil :background "#666")

;; =================================
;; Package
;; =================================
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

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
;; self installed packages
;; =================================
(add-to-list 'load-path "~/.emacs.d/self_installs/")

;; =================================
;; theme
;; =================================
(install-package-and-require 'ample-theme)
(load-theme 'ample-flat t t)
(load-theme 'ample t t)
(enable-theme 'ample)



;; =================================
;; Linum
;; =================================
(global-linum-mode t)

;; linum format
(setq linum-format "%d  ")

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
(setq fci-rule-column 100)

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
(golden-ratio-mode t)



;; =================================
;; ido
;; =================================
(install-package-and-require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

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
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'cider-turn-on-eldoc-mode)



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
(setq deft-directory "~/Dropbox/shared-with-work/notes/")
(setq deft-recursive t)
(setq deft-use-filename-as-title t)
(setq deft-auto-save-interval 0)



;; ;; --- clear all buffers and run tests
;; (defun current-nrepl-server-buffer ()
;;   (let ((nrepl-server-buf (replace-regexp-in-string "connection" "server" (nrepl-current-connection-buffer))))
;;     (when nrepl-server-buf
;;       (get-buffer nrepl-server-buf))))

;; (defun clear-buffers ()
;;   (interactive)

;;   (cider-find-and-clear-repl-buffer)

;;   ;; (with-current-buffer "test.log"
;;   ;;   (kill-region (point-min) (point-max))
;;   ;;   (save-buffer))

;;   (with-current-buffer (current-nrepl-server-buffer)
;;     (kill-region (point-min) (point-max))))

;; (global-set-key (kbd "C-c :") '(lambda ()
;;                                  (interactive)
;;                                  (clear-buffers)
;;                                  (clojure-test-run-tests)
;;                                  (message "Winning")))



;; ;; hipchat
;; ;; (setq ssl-program-name "gnutls-cli"
;; ;;       ssl-program-arguments '("--insecure" "-p" service host)
;; ;;       ssl-certificate-verification-policy 1)

;; ;; Connect using jabber.el
;; ;; M-x jabber-connect <RET>

;; (condition-case nil
;; 		(load-file "~/.emacs.d/jabber-conf.el")
;; 	(error (message "no jabber conf loaded")))

;; ;; Join a room
;; (defun hipchat-join (room)
;;   (interactive "sRoom name: ")
;;   (jabber-groupchat-join
;;    (jabber-read-account)
;;    (concat hipchat-number "_" room "@conf.hipchat.com")
;;    hipchat-nickname
;;    t))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (sql-indent projectile clj-refactor clojure-snippets yasnippet git-blame clojure-cheatsheet align-cljlet smartparens multiple-cursors idle-highlight-mode hl-sexp ac-cider cider auto-complete expand-region golden-ratio magit git-gutter undo-tree fill-column-indicator ample-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
