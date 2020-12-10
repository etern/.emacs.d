(package-initialize)
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file)) ;; touch file
(load custom-file)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'diminish))

(eval-when-compile
  (require 'use-package))
(require 'bind-key) ;; for use-package :bind
(require 'diminish) ;; for use-package :diminish
(setq use-package-verbose 1)

(push "~/.emacs.d/lisp" load-path)
(set-language-environment "UTF-8")

(setq ring-bell-function 'ignore)
(setq make-backup-files nil)
(setq inhibit-startup-screen t)
;;(desktop-save-mode 1)
(show-paren-mode)
(blink-cursor-mode -1)
(column-number-mode)
(menu-bar-mode -1)
(add-hook 'c-mode-common-hook
          (lambda ()
            (abbrev-mode -1)
            (global-set-key (kbd "<f7>") #'compile)))
(add-hook 'python-mode-hook
          (lambda ()
	    (global-set-key (kbd "<f7>") #'compile)
	    (set (make-local-variable 'compile-command)
                 (concat "python " buffer-file-name))
	    (when (functionp 'anaconda-mode) (anaconda-mode 1))
	    (setenv "PYTHONIOENCODING" "utf-8")
            (setenv "IPY_TEST_SIMPLE_PROMPT" "1")))

(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (set-frame-size (selected-frame) 1000 600 t) ;; better: add "-geometry 115x35" to Windows shortcut
  (setq frame-title-format '(multiple-frames "%b" ""))
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
	(setq mac-current-keyboard 'usb)
        (setq mac-option-modifier 'meta)
        (setq mac-command-modifier 'super)
        (message "Remap to USB keyboard"))))

  (use-package exec-path-from-shell
    :ensure t
    :init (exec-path-from-shell-initialize)
    :config (exec-path-from-shell-copy-env "PS1"))

  ;; not lose focus when execute `plantuml.jar`
  (setenv "JAVA_TOOL_OPTIONS" "-Djava.awt.headless=true")
  (global-unset-key (kbd "s-x"))
  (global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen))

;;(yas-global-mode 1)
(setq epa-file-select-keys nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;;(setq scroll-conservatively 1000) ;; don't recenter point

;; Must set before (require 'org)
(setq org-emphasis-regexp-components
      ;; markup 记号前后允许中文
      (list (concat " \t('\"{"            "[:nonascii:]")
            (concat "- \t.,:!?;'\")}\\["  "[:nonascii:]")
            " \t\r\n,\"'"
            "."
            1))

;;(load "setup-org-knowledge-project.el")
(load "my-functions.el")

(global-set-key (kbd "C-x |") #'toggle-window-split)
(global-set-key (kbd "C-c i") #'imenu)

;; #auto-save-file# to /tmp
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(use-package org
  :init
  (add-hook 'org-mode-hook (lambda () (require 'org-tempo)))
  :defer t
  :config
  (use-package org-download
    :if (display-graphic-p)
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
                (hide-mode-line-mode 1)))
    (add-hook 'org-present-mode-quit-hook
              (lambda ()
                (org-present-small)
                (org-remove-inline-images)
                (hide-mode-line-mode -1))))
  (use-package ob-plantuml)
  (use-package ox-latex
    :config
    (setq org-latex-pdf-process '("xelatex -shell-escape %f"
                                  "xelatex -shell-escape %f"
                                  "xelatex -shell-escape %f")))
  (defun my/org-template ()
    (insert "#+STARTUP: showall\n")
    (insert "#+TITLE: ")
    (insert (file-name-base (buffer-name)))
    (insert "\n#+OPTIONS: num:nil ^:{} toc:nil\n"))
  (define-auto-insert "\\.org$" #'my/org-template)
  (when (display-graphic-p)
    (setq org-image-actual-width nil) ;; to show resized image
    (plist-put org-format-latex-options :scale 1.5) ;; LaTeX preview
    (set-face-attribute 'org-table nil
                        :fontset (create-fontset-from-fontset-spec
                                  "-*-*-*-*-*--*-*-*-*-*-*-fontset-orgtable, han:宋体:size=16")))
  ;;(setq org-refile-use-outline-path nil)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda))
)

(define-auto-insert "\\.py$"
  (lambda ()
    (insert "#! /usr/bin/env python3\n# -*- coding:utf-8 -*-\n")))

(use-package avy
  :ensure t
  :bind (("M-g M-g" . avy-goto-line)
	 ("M-g M-c" . avy-goto-char)))

(use-package ace-window
  :ensure t
  :custom (aw-scope 'frame)
  :bind ("M-o" . ace-window))

(use-package magit
  :init
  (setq magit-diff-use-overlays nil)
  :bind ("C-x g" . magit-status))

(use-package counsel
  ;; counsel depends on swiper depends on ivy
  :ensure t
  :init
  (counsel-mode 1)
  (ivy-mode 1)
  :config
  (use-package flx ;; fuzzy match better sorting
    :ensure t)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)))
  :diminish (counsel-mode ivy-mode))

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

(use-package imenu-list
  :bind (("C-<f8>" . imenu-list-smart-toggle)))

(use-package neotree
  :ensure t
  :bind (("<f8>" . neotree-toggle))
  :config
  (use-package all-the-icons
    :if (display-graphic-p))
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

(use-package dumb-jump
  :init (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :config (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
  :ensure)

(use-package dashboard
  :ensure t
  :diminish 'page-break-lines-mode
  :config
  (setq dashboard-banner-logo-title "hello world")
  (setq dashboard-startup-banner nil)
  (setq dashboard-items '((recents . 15)))
  (defun dashboard-insert-totd (list-size)
    (let* ((commands (seq-filter #'commandp obarray))
	   (command (nth (random (length commands)) commands)))
      (insert
       (format "** Tip of the day: ** \nCommand: %s\n\n%s\n\nInvoke with:\n\n"
	       (symbol-value 'command)
	       (documentation command)))
      (where-is command t)))
  (add-to-list 'dashboard-item-generators '(totd . dashboard-insert-totd))
  (add-to-list 'dashboard-items '(totd . 1) t)
  (dashboard-setup-startup-hook))

(use-package recentf
  :config
  (add-to-list 'recentf-exclude "\\.emacs\\.d/recentf$")
  (add-to-list 'recentf-exclude "\\.emacs\\.d/bookmarks$"))

(use-package pdf-tools
  :if (display-graphic-p)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :bind (:map pdf-view-mode-map ;; `GNU less` flavor
	      (("j" . pdf-view-next-line-or-next-page)
	       ("k" . pdf-view-previous-line-or-previous-page)
	       ("b" . pdf-view-scroll-down-or-previous-page)))
  :config
  (add-hook 'pdf-view-mode-hook
	    (lambda ()
	      (progn (pdf-isearch-minor-mode)
		     (pdf-annot-minor-mode)
		     (pdf-misc-context-menu-minor-mode)))))

(use-package expand-region
  :bind (("C-c =" . er/expand-region)))

(use-package which-key
  :ensure t
  :diminish 'which-key-mode
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 3.0)
  (setq which-key-idle-secondary-delay 0))

(use-package pyim
  :defer t
  :config
  (use-package pyim-basedict
    :config (pyim-basedict-enable))
  (setq default-input-method "pyim")
  (setq pyim-default-scheme 'quanpin)
  (pyim-isearch-mode 1))

(use-package hideshow
  :diminish hs-minor-mode
  :hook (prog-mode . hs-minor-mode))

(use-package subword ;; camelCase
  :hook (prog-mode . subword-mode))
