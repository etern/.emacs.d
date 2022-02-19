(package-initialize)
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file)) ;; touch file
(when (display-graphic-p)
  (defcustom my/netdisk-dir "c:/Users/Administrator/OneDrive" "Netdisk" :group 'my))
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

(blink-cursor-mode -1)
(column-number-mode)
(menu-bar-mode -1)
(save-place-mode)
(savehist-mode)
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
(add-hook 'inferior-python-mode-hook
          (lambda ()
            (setq comint-input-ring-file-name "~/.emacs.d/.python.hist")
            (comint-read-input-ring)
            (add-hook 'kill-buffer-hook #'comint-write-input-ring)))

(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (set-frame-size (selected-frame) 110 30) ;; better: add "-geometry 115x35" to Windows shortcut
  (setq frame-title-format '(multiple-frames "%e" (:eval (poem-get 'content))))
  (set-fontset-font t 'chinese-gbk (font-spec :family "Microsoft Yahei"))
  (set-face-attribute 'mode-line nil :box nil) ;; flat mode line
  (set-face-attribute 'mode-line-inactive nil :box nil)
  )

(when (eq system-type 'windows-nt)
  (setq default-directory "~/")
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

;; markup 记号前后允许中文, must set before (require 'org)
(setq org-emphasis-regexp-components
      '("-[:space:]('\"{[:nonascii:]"
        "-[:space:].,:!?;'\")}\\[[:nonascii:]"
        "[:space:]" ;; github render '" not supported
        "."
        1))

;;(load "setup-org-knowledge-project.el")
(load "my-functions.el")

(global-set-key (kbd "C-x |") #'toggle-window-split)
(global-set-key [remap kill-buffer] #'kill-this-buffer)
(global-set-key [remap just-one-space] #'cycle-spacing)
(global-set-key [remap upcase-word] #'upcase-dwim)
(global-set-key [remap downcase-word] #'downcase-dwim)
(global-set-key [remap capitalize-word] #'capitalize-dwim)

;; #auto-save-file# to /tmp
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
;; make (symbol-at-point) break at chinese punctuation
(mapc (lambda (ch) (modify-syntax-entry ch "." (standard-syntax-table)))
      "，。？！")

(use-package org
  :custom (org-imenu-depth 4)
  (org-src-preserve-indentation t)
  (org-use-speed-commands t)
  :hook (org-mode . (lambda () (set-fill-column 80)
                      (org-next-visible-heading 1)
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
         ("M-g w" . avy-goto-word-1)
         ("M-g c" . avy-goto-char-timer)))

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
   ("M-s l" . consult-line-multi-symbol-at-point)
   ("M-s g" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("M-s f" . consult-find) ;; `choco install findutils' on Win
   :map isearch-mode-map
   ("M-s l" . consult-line)
   :map prog-mode-map
   ("M-g f" . consult-flymake))
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-ripgrep consult-xref
   consult--source-recent-file consult--source-project-recent-file
   consult--source-bookmark
   :preview-key (kbd "M-."))
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
  (defun consult-line-multi-symbol-at-point ()
    (interactive)
    (consult-line-multi (thing-at-point 'symbol))))

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x f" . consult-dir-jump-file)))

(use-package embark
  :bind
  (([remap describe-bindings] . embark-bindings)
   :map minibuffer-local-map
   ("C-." . embark-act)
   :map embark-buffer-map
   ([remap kill-buffer] . kill-this-buffer)
   ("C-k" . kill-this-buffer)) ;; so both k & C-k works
  :config
  (setq embark-quit-after-action nil)
  (add-to-list 'embark-post-action-hooks '(kill-this-buffer embark--restart))
  (use-package embark-consult
    :after (embark consult)))
;; embark only used for minibuffer
;; stick with `C-.` for context menu, even without embark
(global-set-key (kbd "C-. s") #'my/bing-search)
(global-set-key (kbd "C-. w") #'whitespace-mode)
(global-set-key (kbd "C-. t") #'toggle-truncate-lines)
(global-set-key (kbd "C-. d") #'my/toggle-debug)
(global-set-key (kbd "C-. l") #'my/copy-line-ref)

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
  :init (add-hook 'xref-backend-functions #'dumb-jump-xref-activate 100)
  :defer t
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
      (advice-add #'dashboard-refresh-buffer :after #'poem-update)
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
  :custom (recentf-exclude '("\\.emacs\\.d/recentf$"
                             "\\.emacs\\.d/elpa/"
                             "\\.emacs\\.d/bookmarks$")))

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
  :bind ([remap mark-sexp] . er/expand-region)) ;; "C-M-SPC"

(use-package which-key
  :ensure t
  :diminish 'which-key-mode
  :config
  (which-key-mode 1)
  ;; hide verbose bindings
  (add-to-list 'which-key-replacement-alist
               '((nil . "digit-argument") . t))
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

(use-package paren
  :init (show-paren-mode)
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

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
  :custom
  (lsp-imenu-index-symbol-kinds '(Class Function Interface Enum Method Constuctor Operator TypeParameter Field Struct Property))
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
         (eshell-mode . company-mode))
  :custom
  (company-transformers '(company-sort-by-occurrence
                          company-sort-prefer-same-case-prefix)))

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
  :custom
  (org-roam-graph-link-hidden-types '("http" "https" "info" "help"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert))
  :config (org-roam-db-autosync-mode))

(use-package dired
  :custom
  (dired-dwim-target t)
  :bind (("C-x C-j" . dired-jump) ;; cmd from dired-x
         :map dired-mode-map
         ([remap dired-summary] . which-key-show-major-mode))
  :config
  (unbind-key "M-s f" dired-mode-map)) ;; "M-s f" is taken by consult-find

(use-package helpful
  :if (fboundp 'helpful-callable)
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h o" . helpful-symbol)
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

(use-package cc-mode
  :defer t
  :config
  (c-add-style "mine" '("gnu"  ;; inherit from gnu, with some customization
                        (c-basic-offset . 4)
                        (c-offsets-alist
                         (innamespace . 0)))) ;; namespace no indent
  (setf (alist-get 'c-mode c-default-style) "mine"
        (alist-get 'c++-mode c-default-style) "mine"))

(use-package elec-pair
  :hook (prog-mode . electric-pair-local-mode)
  :config
  (setq electric-pair-skip-whitespace nil
        electric-pair-inhibit-predicate #'electric-pair-conservative-inhibit))

(use-package view
  :init (setq view-read-only t) ;; C-x C-q
  :bind
  (:map view-mode-map
        ("j" . View-scroll-line-forward)
        ("k" . View-scroll-line-backward)
        ("b" . View-scroll-page-backward)
        ("f" . View-scroll-page-forward)))

(use-package org-capture
  :if (display-graphic-p)
  :bind
  ("C-c c" . org-capture)
  :config
  (setq my/journal-file (format "%s/Documents/%s" my/netdisk-dir (format-time-string "%Y.org")))
  (defun my/journal-goto-today ()
    (let ((heading (format-time-string "* [%Y-%m-%d %a]")))
      (goto-char (point-max))
      (unless (re-search-backward (regexp-quote heading) nil t)
        (goto-char (point-max))
        (or (bolp) (insert "\n"))
        (insert heading "\n")) ;; TODO don't change file on cancel (C-c C-k)
      (org-end-of-subtree)))
  :custom
  (org-capture-templates
   '(("j" "Write journal" plain (file+function my/journal-file my/journal-goto-today)
      nil :empty-lines-after 1))))

(use-package isearch
  :if (version<= "27.1" emacs-version)
  :bind (:map isearch-mode-map
         ([remap isearch-delete-char] . isearch-del-char))
  :custom (isearch-lazy-count t))

(use-package anzu
  :ensure t
  :bind ([remap query-replace] . anzu-query-replace))
