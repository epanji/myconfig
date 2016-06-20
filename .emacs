;;
;;
;; auto
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector [default default default italic underline success warning error])
 '(ansi-color-names-vector ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))
 '(cursor-type (quote bar) t)
 '(custom-enabled-themes (quote (tango-dark)))
 '(erc-nick "panji")
 '(global-linum-mode t)
 '(inhibit-startup-screen t)
 '(linum-format " %d ")
 '(make-backup-files t)
 '(ring-bell-function (quote ignore) t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(version-control t))
;;
;;
;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;
;;
;; melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
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
(setq erc-hide-list '("JOIN" "PART" "QUIT"))
;;
;;
;; epg epa
(require 'epa)
(epa-file-enable)
;;
;;
;; enabled
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
;;
;;
;; binding
(global-set-key (kbd "C-`") 'delete-trailing-whitespace)
(global-set-key (kbd "C-<tab>") 'other-window)
;;
;;
;;
