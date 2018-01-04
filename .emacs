;;
;;
;; custom-set-variables was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
(custom-set-variables
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))
 '(cursor-type (quote bar))
 '(custom-enabled-themes (quote (tango-dark)))
 '(erc-nick "panji")
 '(global-linum-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(linum-format " %d ")
 '(make-backup-files t)
 '(org-agenda-files (quote ("~/org/agenda")))
 '(package-selected-packages
   (quote
    (auto-complete dash emmet-mode htmlize kotlin-mode lice move-dup multiple-cursors org ox-epub ox-twbs parinfer php-mode popup qzuma restclient shift-number web-mode)))
 '(ring-bell-function (quote ignore))
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(version-control t))
;;
;;
;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
(custom-set-faces)
;;
;;
;; melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)
;;
;;
;; auto-complete
(require 'auto-complete)
(global-auto-complete-mode t)
;;
;;
;; web-mode.el
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
;;
;;
;; emmet-mode.el
(require 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
;;
;;
;; multi-cursor
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "<f5>") 'mc/insert-numbers)
(global-set-key (kbd "<f6>") 'mc/insert-letters)
(global-set-key (kbd "<f7>") 'mc/edit-ends-of-lines)
;;
;;
;; move-dup.el
(require 'move-dup)
(global-move-dup-mode)
;;
;;
;; shift-number.el
(global-set-key (kbd "C-c +") 'shift-number-up)
(global-set-key (kbd "C-c -") 'shift-number-down)
;;
;;
;; erc
(setq erc-hide-list '("join" "part" "quit"))
;;
;;
;; epg epa
(require 'epa)
;;
;;
;; enabled
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
;;
;;
;; binding
(global-set-key (kbd "C-`") 'delete-trailing-whitespace)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "<f8>") 'delete-other-windows)
(global-set-key (kbd "<f9>") 'emacs-lock-mode)
;;
;;
;; org
(add-hook 'org-mode-hook '(lambda () (move-dup-mode -1)))
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-iswitchb)
;;
;;
;; slime
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(load (expand-file-name "~/quicklisp/clhs-use-local.el") t)
(setq inferior-lisp-program "/usr/bin/sbcl")
(add-hook 'lisp-mode-hook 'electric-pair-mode 1)
(add-hook 'lisp-mode-hook 'show-paren-mode 1)
(add-hook
 'comint-mode-hook
 '(lambda ()
    (local-set-key (kbd "<f8>") 'comint-clear-buffer)))
(add-hook
 'slime-repl-mode-hook
 '(lambda ()
    (show-paren-mode 1)
    (electric-pair-mode 1)
    (local-set-key (kbd "<f8>") 'slime-repl-clear-buffer)))
;;
;;
;; server
(require 'server)
(add-hook
 'window-setup-hook
 '(lambda ()
    (unless (server-running-p)
      (switch-to-buffer "*scratch*")
      (server-start)
      (emacs-lock-mode 1)
      (set-frame-parameter nil 'fullscreen 'maximized))))
;;
;;
;; parinfer
(add-hook
 'parinfer-mode-hook
 '(lambda ()
    (show-paren-mode 1)
    (electric-pair-mode 1)
    (local-set-key (kbd "<f12>") 'parinfer-toggle-mode)))
;;
;;
;; qzuma
(require 'qzuma)
(prefer-coding-system 'utf-8)
