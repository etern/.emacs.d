(package-initialize)
(push '("Melpa Stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/") package-archives)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(eval-when-compile
  (require 'use-package))
(setq use-package-verbose 1)
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (setq initial-frame-alist '((tool-bar-lines . 0) (width . 115) (height . 35)))
  (setq default-frame-alist '((tool-bar-lines . 0) (width . 115) (height . 35))))

(when (eq system-type 'windows-nt)
  (setq gc-cons-threshold (* 512 1024 1024))
  (setq gc-cons-percentage 0.5)
  (run-with-idle-timer 5 t #'garbage-collect)
  ;; 显示垃圾回收信息，这个可以作为调试用
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
  :bind (("<f8>" . neotree-toggle))
  :config
  (use-package all-the-icons)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(use-package nyan-mode
  :init (nyan-mode))

(setq ring-bell-function 'ignore)
(setq make-backup-files nil)
(setq inhibit-startup-screen t)
