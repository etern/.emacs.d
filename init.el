(package-initialize)
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file)) ;; touch file
(load custom-file)

;; Bootstrap `use-package`
(setq package-pinned-packages
      '((bind-key    . "melpa-stable")
        (diminish    . "melpa-stable")
        (use-package . "melpa-stable")))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

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
                    'han (font-spec :family "Microsoft Yahei"))
  (set-face-attribute 'mode-line nil :box nil) ;; flat mode line
  (set-face-attribute 'mode-line-inactive nil :box nil)
  )

(when (eq system-type 'windows-nt)
  (setq gc-cons-threshold (* 512 1024 1024))
  (setq gc-cons-percentage 0.5)
  (run-with-idle-timer 5 t #'garbage-collect)
  ;; (setq garbage-collection-messages t)
  (setq w32-recognize-altgr nil)
  (require 'wget))

(when (eq system-type 'darwin)
  (defvar mac-current-keyboard 'usb)
  (defun mac-toggle-keyboard ()
    (interactive)
    (if (eq mac-current-keyboard 'usb)
        (progn
          (setq mac-current-keyboard 'embeded)
          (setq mac-option-modifier 'control)
          (setq mac-command-modifier 'meta)
          (message "Remap to Embeded keyboard"))
      (progn
        1       (setq mac-current-keyboard 'usb)
        (setq mac-option-modifier 'meta)
        (setq mac-command-modifier 'super)
        (message "Remap to USB keyboard"))))
  ;; not lose focus when execute `plantuml.jar`
  (setenv "JAVA_TOOL_OPTIONS" "-Djava.awt.headless=true")
  (setenv "PATH" (concat "/usr/local/bin" ":" (getenv "PATH")))
  (setq exec-path (append exec-path '("/usr/local/bin")))
  (global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen))

;;(yas-global-mode 1)
(setq epa-file-select-keys nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq scroll-conservatively 1000) ;; don't recenter point

;; Must set before (require 'org)
(setq org-emphasis-regexp-components
      ;; markup 记号前后允许中文
      (list (concat " \t('\"{"            "[:nonascii:]")
            (concat "- \t.,:!?;'\")}\\["  "[:nonascii:]")
            " \t\r\n,\"'"
            "."
            1))
(push "~/.emacs.d/lisp" load-path)

;;(load "setup-org-knowledge-project.el")
(load "my-functions.el")

(global-set-key (kbd "C-x |") #'toggle-window-split)
(global-set-key (kbd "C-c i") #'imenu)

(use-package org
  :defer t
  :config
  (use-package org-download
    :init
    (setq org-download-image-dir "./org-download-images") ;; this dir will be auto-created
    :config
    (add-hook 'org-mode-hook #'org-download-enable)
    (setq org-download-timestamp "")) ;; don't append time string to image name
  (use-package org-bullets
    :init
    (add-hook 'org-mode-hook #'org-bullets-mode))
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
  (use-package ob-plantuml)
  (when (display-graphic-p)
    (setq org-image-actual-width nil) ;; to show resized image
    (set-face-attribute 'org-table nil
                        :fontset (create-fontset-from-fontset-spec
                                  "-*-*-*-*-*--*-*-*-*-*-*-fontset-orgtable, han:宋体:size=18")))
  ;;(setq org-refile-use-outline-path nil)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda))
)

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

(use-package restclient
  :mode ("\\.restclient\\'" . restclient-mode))

(use-package yasnippet
  :commands (yas-minor-mode yas-reload-all)
  :diminish yas-minor-mode
  :init
  (add-hook 'org-mode-hook #'yas-minor-mode)
  :config
  (yas-reload-all))

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

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("<mouse-2>" . mc/add-cursor-on-click)))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode)

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
	 ("M-g b" . dumb-jump-back)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
  :ensure)
