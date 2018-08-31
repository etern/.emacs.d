(package-initialize)
(push '("Melpa Stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/") package-archives)
(push '("Melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/") package-archives)

(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file)) ;; touch file
(load custom-file)

(eval-when-compile
  (require 'use-package))
(require 'bind-key) ;; for use-package :bind
(require 'diminish) ;; for use-package :diminish
(setq use-package-verbose 1)

(setq ring-bell-function 'ignore)
(setq make-backup-files nil)
(setq inhibit-startup-screen t)
;;(desktop-save-mode 1)
(show-paren-mode)
(blink-cursor-mode -1)
(add-hook 'c-mode-common-hook (lambda () (abbrev-mode -1)))

(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (set-frame-size (selected-frame) 115 35) ;; better: add "-geometry 115x35" to Windows shortcut
  (set-fontset-font (frame-parameter nil 'font)
                  'han (font-spec :family "Microsoft Yahei")))

(when (eq system-type 'windows-nt)
  (setq gc-cons-threshold (* 512 1024 1024))
  (setq gc-cons-percentage 0.5)
  (run-with-idle-timer 5 t #'garbage-collect)
  ;; (setq garbage-collection-messages t)
  (setq w32-recognize-altgr nil))

;;(setq org-refile-use-outline-path nil)

;;(yas-global-mode 1)
(setq epa-file-select-keys nil)

;; 必须在 (require 'org) 之前
(setq org-emphasis-regexp-components
      ;; markup 记号前后允许中文
      (list (concat " \t('\"{"            "[:nonascii:]")
            (concat "- \t.,:!?;'\")}\\["  "[:nonascii:]")
            " \t\r\n,\"'"
            "."
            1))
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-image-actual-width nil) ;; to show resized image

(push "~/.emacs.d/lisp" load-path)

(load "setup-org-knowledge-project.el")

(use-package avy
  :ensure t
  :bind ("M-g M-g" . avy-goto-line))

(use-package magit
  :init
  (setq magit-diff-use-overlays nil)
  :bind ("C-x g" . magit-status))

(use-package ivy
  :init (ivy-mode 1)
  :diminish ivy-mode)

(use-package semantic
  :bind ("C-c , s" . semantic-ia-show-summary))

;;(use-package ede
;;  :init
;;  (global-ede-mode))

(use-package cmake-mode
  :commands cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'")) ;;add to auto-mode-alist

(use-package org-bullets
  :init
  (add-hook 'org-mode-hook #'org-bullets-mode))

(use-package yasnippet
  :commands (yas-minor-mode yas-reload-all)
  :init
  (add-hook 'org-mode-hook #'yas-minor-mode)
  :config
  (yas-reload-all))

(require 'wget)

(use-package imenu-list
  :bind (("C-<f8>" . imenu-list-smart-toggle)))

(use-package neotree
  :ensure t
  :bind (("<f8>" . neotree-toggle))
  :config
  (use-package all-the-icons)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(use-package nyan-mode
  :if window-system
  :ensure t
  :init (nyan-mode))

(use-package org-present
  :commands org-present
  :config
  (use-package hide-mode-line
    :ensure t)
  (add-hook 'org-present-mode-hook
            (lambda ()
              (org-present-big)
              (org-display-inline-images)
              (hide-mode-line-mode 1)
              (menu-bar-mode -1)))
  (add-hook 'org-present-mode-quit-hook
            (lambda ()
              (org-present-small)
              (org-remove-inline-images)
              (hide-mode-line-mode -1)
              (menu-bar-mode 1))))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("<mouse-2>" . mc/add-cursor-on-click)))
