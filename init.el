(package-initialize)
(push '("Melpa Stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/") package-archives)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(eval-when-compile
  (require 'use-package))
(require 'bind-key) ;; for use-package :bind
(setq use-package-verbose 1)

(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (set-frame-size (selected-frame) 115 35)) ;; better: add "-geometry 115x35" to Windows shortcut

(when (eq system-type 'windows-nt)
  (setq gc-cons-threshold (* 512 1024 1024))
  (setq gc-cons-percentage 0.5)
  (run-with-idle-timer 5 t #'garbage-collect)
  ;; (setq garbage-collection-messages t)
  (setq w32-recognize-altgr nil)
  )

(set-fontset-font (frame-parameter nil 'font)
                  'han (font-spec :family "Microsoft Yahei"))

(global-set-key (kbd "M-g M-g") 'avy-goto-line)
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

(push "~/.emacs.d/lisp" load-path)

(load "setup-org-knowledge-project.el")

(use-package magit
  :init
  (setq magit-diff-use-overlays nil))

(use-package ivy
  :init (ivy-mode 1))

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

(setq ring-bell-function 'ignore)
(setq make-backup-files nil)
(setq inhibit-startup-screen t)
