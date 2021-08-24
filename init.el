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
(setq use-package-keywords ;; precedence  :if > :ensure
      (append '(:if) (remove :if use-package-keywords)))

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
(add-hook 'prog-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)))
(add-hook 'c-mode-common-hook
          (lambda ()
            (abbrev-mode -1)
            (global-set-key (kbd "<f7>") #'compile)))
(add-hook 'python-mode-hook
          (lambda ()
            (setq tab-width 4)
            (global-set-key (kbd "<f7>") #'compile)
            (set (make-local-variable 'compile-command)
                 (concat "python " buffer-file-name))
            (setenv "PYTHONIOENCODING" "utf-8")
            (setenv "IPY_TEST_SIMPLE_PROMPT" "1")))

(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (set-frame-size (selected-frame) 1000 600 t) ;; better: add "-geometry 115x35" to Windows shortcut
  (setq frame-title-format '(multiple-frames "%e" (:eval (poem-get 'content))))
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
  (add-to-list 'process-coding-system-alist '("rg" utf-8 . gbk) t)
  (add-to-list 'process-coding-system-alist '("git" utf-8 . gbk) t)
  (add-to-list 'process-coding-system-alist '("" gbk . gbk) t)
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
(setq vc-handled-backends '(Git))
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
(global-set-key (kbd "C-x k") #'kill-this-buffer)

;; #auto-save-file# to /tmp
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
;; make (symbol-at-point) break at chinese punctuation
(mapc (lambda (ch) (modify-syntax-entry ch "." (standard-syntax-table)))
      "，。？！")

(use-package org
  :init
  :hook (org-mode . (lambda () (set-fill-column 80)
                      (electric-indent-local-mode -1)))
  :config
  (require 'org-tempo) ;; use `<s` to expand src_block
  (use-package display-fill-column-indicator
    :if (not (version< emacs-version "27"))
    :hook (org-mode . display-fill-column-indicator-mode))
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
  :bind (("C-c a" . org-agenda)
         :map org-mode-map
         ("C-c l" . org-store-link))
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

(use-package vertico
  :if (version<= "27.1" emacs-version)
  :ensure t
  :init (vertico-mode)
  :custom
  (completion-styles '(substring flex))
  (completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  :config
  (setq completion-in-region-function #'consult-completion-in-region)
  ;; https://github.com/minad/vertico/issues/10
  (unless (boundp 'minibuffer--require-match)
    (defvar minibuffer--require-match nil))
  (use-package marginalia
    :defer t
    :config (marginalia-mode))
  (require 'marginalia nil 'noerror))

(use-package consult
  :ensure t
  :bind
  (("C-x b" . consult-buffer)
   ("M-y" . consult-yank-pop)
   ("M-g i" . consult-imenu)
   ("M-g o" . consult-outline)
   ("M-s o" . consult-line-symbol-at-point)
   ("M-s g" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   :map prog-mode-map
   ("M-g f" . consult-flymake))
  :config
  (consult-customize
   consult-ripgrep consult-xref
   consult--source-file consult--source-project-file
   consult--source-bookmark
   :preview-key (kbd "M-."))
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
  (defun consult-line-symbol-at-point ()
    (interactive)
    (consult-line (thing-at-point 'symbol))))

(use-package embark
  :bind
  (([remap describe-bindings] . embark-bindings)
   :map minibuffer-local-map
   ("C-." . embark-act)
   :map embark-buffer-map
   ("C-k" . kill-buffer)) ;; so both k & C-k works
  :config
  (setq embark-quit-after-action nil)
  (delete #'kill-buffer embark-allow-edit-actions)
  (use-package embark-consult
    :after (embark consult)))
;; embark only used for minibuffer
;; stick with `C-.` for context menu, even without embark
(global-set-key (kbd "C-. s") #'my/bing-search)
(global-set-key (kbd "C-. w") #'whitespace-mode)
(global-set-key (kbd "C-. t") #'toggle-truncate-lines)

(use-package semantic
  :bind ("C-c , s" . semantic-ia-show-summary))

;;(use-package ede
;;  :init
;;  (global-ede-mode))

(use-package cmake-mode
  :commands cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'")) ;;add to auto-mode-alist

(use-package restclient
  :mode (("\\.rest\\'" . restclient-mode)
         ("\\.http\\'" . restclient-mode)))

(use-package imenu-list
  :bind ("C-<f8>" . imenu-list-smart-toggle))

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
  :ensure)

(use-package dashboard
  :ensure t
  :diminish 'page-break-lines-mode
  :config
  (setq dashboard-startup-banner nil)
  (setq dashboard-items '((recents . 20)))
  (defun dashboard-insert-totd (list-size)
    (let* ((commands (seq-filter #'commandp obarray))
           (command (nth (random (length commands)) commands)))
      (insert (propertize "Tip of the day:\n" 'face 'dashboard-heading))
      (insert (format "Command: %s\n\n%s\n\nInvoke with:\n\n"
                      (symbol-value 'command)
                      (documentation command)))
      (where-is command t)))
  (add-to-list 'dashboard-item-generators '(totd . dashboard-insert-totd))
  (add-to-list 'dashboard-items '(totd) t)
  (when (display-graphic-p)
      (advice-add #'dashboard-refresh-buffer :after #'poem-async-update)
      (defun dashboard-dev-tools (list-size)
        (insert (propertize "Dev tools:\n" 'face 'dashboard-heading))
        (insert-button "ASCII Table\n" 'follow-link t
                       'action (lambda (_btn) (list-charset-chars 'ascii)
                                 (other-window 1)))
        (insert-button "Calculator" 'follow-link t 'action (lambda (_) (calc))))
      (defun dashboard-poem (list-size)
        (insert (poem-get-formatted)))
        (add-to-list 'dashboard-item-generators '(devtools . dashboard-dev-tools))
        (add-to-list 'dashboard-items '(devtools) t)
        (add-to-list 'dashboard-item-generators '(poem . dashboard-poem))
        (add-to-list 'dashboard-items '(poem) t))
  (dashboard-setup-startup-hook))

(use-package recentf
  :config
  (add-to-list 'recentf-exclude "\\.emacs\\.d/recentf$")
  (add-to-list 'recentf-exclude "\\.emacs\\.d/bookmarks$"))

(use-package pdf-tools
  :if (display-graphic-p)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :bind (:map pdf-view-mode-map ;; `GNU less` flavor
              ("j" . pdf-view-next-line-or-next-page)
              ("k" . pdf-view-previous-line-or-previous-page)
              ("b" . pdf-view-scroll-down-or-previous-page))
  :config
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (progn (pdf-isearch-minor-mode)
                     (pdf-annot-minor-mode)
                     (pdf-outline-minor-mode)
                     (pdf-misc-context-menu-minor-mode)))))

(use-package expand-region
  :bind ("C-c =" . er/expand-region))

(use-package which-key
  :ensure t
  :diminish 'which-key-mode
  :config
  (which-key-mode 1)
  (setq which-key-show-early-on-C-h t)
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
  :diminish subword-mode
  :hook (prog-mode . subword-mode))

(use-package hl-line
  :hook ((prog-mode . hl-line-mode)
         (dired-mode . hl-line-mode)))

(use-package lsp-pyright
  :defer t
  :hook (python-mode . (lambda ()
                         (if (require 'lsp-pyright nil 'noerror)
                             (lsp)
                           (message "lsp-pyright not installed, ignore")))))

(use-package lsp-mode
  :defer t
  :init
  (setq lsp-keymap-prefix "C-c p")
  :diminish eldoc-mode
  :config
  (use-package flymake :diminish)
  (setq lsp-enable-snippet nil)
  ;; diminish signature message in minibuffer
  (setq lsp-signature-render-documentation nil)
  (setq lsp-signature-auto-activate nil)
  ;; Performance tuning
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-completion-provider :capf)
  (setq lsp-log-io nil))

(use-package company
  :ensure t
  :diminish
  :bind (:map company-mode-map
              ([remap completion-at-point] . company-complete))
  :hook ((prog-mode . company-mode)
         (shell-mode . company-mode)
         (inferior-python-mode . company-mode)
         (eshell-mode . company-mode)))

(use-package project
  :bind ("C-c p f" . project-find-file))

(use-package treemacs
  :defer t
  :bind (("C-c p p" . treemacs-select-window)
         ("<f8>" . treemacs)
         :map treemacs-mode-map
         ("j" . treemacs-next-line)
         ("k" . treemacs-previous-line)))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-graph-link-hidden-types '("http" "https" "info" "help"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

(use-package dired-x
  :custom
  (dired-dwim-target t))

(use-package helpful
  :if (require 'helpful nil 'noerror)
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key))
  :config
  (use-package elisp-demos
    :if (require 'elisp-demos nil 'noerror)
    :init
    (advice-add 'helpful-update
                :after #'elisp-demos-advice-helpful-update)))

;; Since vertico is there, ido/icomplete will not be used intentionally
;; In case they're loaded indirectly (ex: by lsp), use \n as separator
(use-package ido :defer t :config (setf (nth 2 ido-decorations) "\n"))
(use-package icomplete :defer t :custom (icomplete-separator "\n"))
