;;; init.el --- My Emacs Config  -*- lexical-binding: t; -*-

(package-initialize)
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(setq custom-file "~/.emacs.d/custom.el")
(when (display-graphic-p)
  (defcustom my/netdisk-dir "c:/Users/Administrator/OneDrive"
    "Netdisk" :type 'string :group 'my)
  (defcustom my/hide-title-bar nil "Hide title bar, restart emacs to take effect"
    :type 'boolean :group 'my))
(load custom-file t t)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-keywords ;; precedence  :if > :ensure
        (append '(:if) (remove :if use-package-keywords))))
(use-package diminish :ensure t) ;; for use-package :diminish
;;(setq use-package-verbose 1)

(push "~/.emacs.d/lisp" load-path)
(require 'my-functions)
(set-language-environment "UTF-8")

(setq ring-bell-function 'ignore)
(setq make-backup-files nil)
(setq inhibit-startup-screen t)
(setq completions-detailed t)
;;(setq completion-auto-help nil) ;; never show *Completions* buffer
;;(desktop-save-mode 1)

(blink-cursor-mode -1)
(column-number-mode)
(menu-bar-mode -1)
(save-place-mode)
(savehist-mode)
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local show-trailing-whitespace t)
            (setq eldoc-minor-mode-string nil)
            (setq indent-tabs-mode nil)))

(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (set-frame-size (selected-frame) 110 30) ;; better: add "-geometry 115x35" to Windows shortcut
  (if my/hide-title-bar
      (setq default-frame-alist '((undecorated . t) (drag-internal-border . 1)
                                  (internal-border-width . 5)))
    (setq frame-title-format '(multiple-frames "%e" (:eval (poetry-get 'content)))))
  (set-fontset-font t 'chinese-gbk (font-spec :family "Microsoft Yahei"))
  (when (member "Segoe UI Emoji" (font-family-list))
    (set-fontset-font t 'emoji (font-spec :family "Segoe UI Emoji") nil 'prepend))
  (set-face-attribute 'mode-line nil :box nil) ;; flat mode line
  (set-face-attribute 'mode-line-inactive nil :box nil))

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
          (setq mac-current-keyboard 'embeded
                mac-option-modifier 'control
                mac-command-modifier 'meta)
          (message "Remap to Embeded keyboard"))
      (setq mac-current-keyboard 'usb
            mac-option-modifier 'meta
            mac-command-modifier 'super)
      (message "Remap to USB keyboard")))

  (use-package exec-path-from-shell
    :ensure t
    :custom (exec-path-from-shell-arguments '("-l"))
    :init (exec-path-from-shell-initialize)
    :config (exec-path-from-shell-copy-env "PS1"))

  ;; not lose focus when execute `plantuml.jar`
  (setenv "JAVA_TOOL_OPTIONS" "-Djava.awt.headless=true")
  (global-unset-key (kbd "s-x"))
  (global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen))

(setq epa-file-select-keys nil)
(setq vc-handled-backends '(Git))
;;(setq scroll-conservatively 1000) ;; don't recenter point

;; markup 记号前后允许中文, must set before (require 'org)
(setq org-emphasis-regexp-components
      '("-[:space:]('\"{[:nonascii:]"
        "-[:space:].,:!?;'\")}\\[[:nonascii:]"
        "[:space:]" ;; github render '" not supported
        "."
        1))

(global-set-key (kbd "C-x |") #'my/toggle-window-split)
(global-set-key (kbd "M-k") #'kill-buffer) ; unbind kill-sentence
(global-set-key (kbd "M-K") #'delete-window)
(global-set-key (kbd "M-h") #'previous-buffer) ; unbind mark-paragraph
(global-set-key (kbd "M-H") #'next-buffer)
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

;; concise & descriptive mode-line
(setq mode-line-percent-position nil
      mode-line-position-column-line-format '(" %l:%c")
      mode-line-modes (remove "(" (remove ")" mode-line-modes))
      eol-mnemonic-unix " LF "
      eol-mnemonic-dos " CRLF ")
(setf (car mode-line-remote) " "
      (car mode-line-modified) " "  ; RW/RO
      (nth 1 mode-line-modified)  ; buffer modified
      '(:eval (if (and (not buffer-read-only) (buffer-modified-p))
                  (propertize "*" 'face 'error) " ")))
(setf (nth 2 mode-line-mule-info)
      '(:eval (propertize
               (my/coding-string buffer-file-coding-system)
               'help-echo 'mode-line-mule-info-help-echo
               'mouse-face 'mode-line-highlight
               'local-map mode-line-coding-system-map)))

(when (display-graphic-p)
  (add-hook 'after-init-hook #'my/dashboard))

(use-package org
  :custom
  (org-imenu-depth 4)
  (org-src-preserve-indentation t)
  (org-use-speed-commands t)
  (org-html-validation-link nil)
  :hook (org-mode . (lambda () (setq fill-column 80)
                      (org-next-visible-heading 1)
                      (electric-indent-local-mode -1)))
  :config
  (defadvice org-babel-execute-src-block (around load-language nil activate)
    "Load ob-{language} only when needed."
    (let ((lang (org-element-property :language (org-element-at-point))))
      (when (or (string= lang "bash") (string= lang "sh")) (setq lang "shell"))
      (unless (cdr (assoc (intern lang) org-babel-load-languages))
        (add-to-list 'org-babel-load-languages (cons (intern lang) t))
        (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
      ad-do-it))
  (require 'org-tempo) ;; use `<s` to expand src_block
  (use-package display-fill-column-indicator
    :if (not (version< emacs-version "27"))
    :hook (org-mode . display-fill-column-indicator-mode))
  (use-package org-download
    :if (and (fboundp 'org-download-enable) (display-graphic-p))
    :init
    (setq org-download-image-dir "./org-download-images") ;; this dir will be auto-created
    :config
    (add-hook 'org-mode-hook #'org-download-enable)
    (setq org-download-timestamp "")) ;; don't append time string to image name
  (use-package org-superstar
    :ensure t
    :custom (org-hide-leading-stars t)
    :hook (org-mode . org-superstar-mode))
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
  (use-package ox-latex
    :defer t
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
  :config
  (unbind-key "M-h" org-mode-map) ;; unbind `org-mark-element', occupied by `previous-buffer'
)

(use-package tmm
  :defer t
  :custom (tmm-completion-prompt nil))

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
  :bind (:map vertico-map
              ("<next>" . vertico-scroll-up)
              ("<prior>" . vertico-scroll-down))
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
   :preview-key "M-.")
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (project-root project))))
  (defun consult-line-multi-symbol-at-point ()
    (interactive)
    (consult-line-multi t (thing-at-point 'symbol))))

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x f" . consult-dir-jump-file)))

(use-package embark
  :if (fboundp 'embark-act)
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
(global-set-key (kbd "C-. d") #'my/toggle-debug)
(global-set-key (kbd "C-. l") #'my/copy-line-ref)

(use-package restclient
  :mode (("\\.rest\\'" . restclient-mode)
         ("\\.http\\'" . restclient-mode)))

(use-package imenu-list
  :bind ("C-<f8>" . imenu-list-smart-toggle))

(use-package nyan-mode
  :if (display-graphic-p)
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
  :ensure t
  :bind ("C-. j" . my/toggle-dumb-jump-order)
  :config
  (defvar my/dumb-jump-order 100)
  (defun my/toggle-dumb-jump-order ()
    "dumb-jump order in xref, useful to suppress etags prompt"
    (interactive)
    (remove-hook 'xref-backend-functions #'dumb-jump-xref-activate)
    (setq my/dumb-jump-order (- my/dumb-jump-order))
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate my/dumb-jump-order)
    (message "dumb-jump %s" (if (> my/dumb-jump-order 0) "as fallback" "goes first"))))

(use-package recentf
  :init (let ((inhibit-message t)) (recentf-mode))
  :custom (recentf-exclude '("\\.emacs\\.d/recentf$"
                             "\\.emacs\\.d/elpa/"
                             "\\.emacs\\.d/bookmarks$"))
  (recentf-filename-handlers '(abbreviate-file-name)))

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
              (pdf-isearch-minor-mode)
              (pdf-annot-minor-mode)
              (pdf-outline-minor-mode)
              (pdf-misc-context-menu-minor-mode))))

(use-package expand-region
  :if (fboundp 'er/expand-region)
  :custom (expand-region-show-usage-message nil)
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
  (setq which-key-idle-delay 2.0)
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
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

(use-package eglot
  :hook (python-mode . eglot-ensure)
  :bind (:map eglot-mode-map ("C-c p r" . eglot-rename))
  :custom
  (eglot-events-buffer-size 0)
  (eldoc-echo-area-use-multiline-p 1)
  :config
  ;;https://github.com/joaotavora/eglot/issues/454#issuecomment-642978840
  (define-key eglot-mode-map [remap display-local-help] nil)
  (diminish 'flymake-mode flymake-mode-line-counters))

(use-package company
  :ensure t
  :diminish
  :bind (:map company-mode-map
              ([remap completion-at-point] . company-complete))
  :hook ((prog-mode . company-mode)
         (shell-mode . company-mode)
         (eshell-mode . company-mode))
  :custom
  (company-backends '((company-capf company-dabbrev-code) company-files))
  (company-transformers '(delete-consecutive-dups
                          company-sort-by-occurrence
                          company-sort-prefer-same-case-prefix)))

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
  (dired-kill-when-opening-new-dired-buffer t)
  :bind (:map dired-mode-map
              ("l" . dired-up-directory) ;; pretending 'back history', like in Info-mode
              ([remap dired-summary] . which-key-show-major-mode))
  :config
  (defalias 'dired-open-externally 'browse-url-of-dired-file)
  (defun my/dired-dim-git-ignores ()
    "Dim out .gitignore contents, folder/glob not handled"
    (require 'vc)
    (when-let ((ignores (vc-default-ignore-completion-table 'git ".gitignore"))
               (exts (make-local-variable 'completion-ignored-extensions)))
      (dolist (item ignores) (add-to-list exts item))))
  (add-hook 'dired-mode-hook #'my/dired-dim-git-ignores)
  ;; Dim out user & time (time-style can be iso, long-iso)
  (let* ((date (rx (? (= 4 digit) "-") (regex "[01][0-9]-[0-3][0-9]")))
         (datetime (concat date " [0-2][0-9]:[0-5][0-9]")))
    (font-lock-add-keywords
     'dired-mode `((,datetime . 'dired-ignored)
                   (,date . 'dired-ignored)
                   (,user-login-name . 'dired-ignored)) t))
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
  (bind-key "<f7>" #'compile c-mode-base-map)
  (c-add-style "mine" '("gnu"  ;; inherit from gnu, with some customization
                        (c-basic-offset . 4)
                        (c-offsets-alist
                         (innamespace . 0)))) ;; namespace no indent
  (setf (alist-get 'c-mode c-default-style) "mine"
        (alist-get 'c++-mode c-default-style) "mine")
  (add-hook 'c-mode-common-hook
            (lambda () (abbrev-mode -1))))

(use-package citre
  :init (autoload 'citre-mode "citre" nil t)
  :hook ((c++-mode . citre-mode)
         (c-mode . citre-mode))
  :config
  (defun my/filter-imenu (imenu-alist)
    (seq-filter (lambda (item)
                  (member (car item) '("function" "class" "struct" "member" "variable" "typedef")))
                imenu-alist))
  (advice-add 'citre-imenu-create-index-function :filter-return #'my/filter-imenu))

(use-package python
  :defer t
  :config
  (bind-key "<f7>" #'compile python-mode-map)
  (setenv "IPY_TEST_SIMPLE_PROMPT" "1")
  (setenv "PYTHONIOENCODING" "utf-8")
  (add-hook 'python-mode-hook
            (lambda ()
              (setq tab-width 4)
              (setq-local compile-command (concat "python " buffer-file-name))))
  (add-hook 'inferior-python-mode-hook
            (lambda ()
              (setq comint-input-ring-file-name "~/.emacs.d/.python.hist")
              (comint-read-input-ring)
              (add-hook 'kill-buffer-hook #'comint-write-input-ring)))
  (define-auto-insert "\\.py$"
    (lambda ()
      (insert "#! /usr/bin/env python3\n# -*- coding:utf-8 -*-\n"))))

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

(use-package edit-indirect
  :if (display-graphic-p)
  :bind ("C-c j" . my/journal-capture))

(use-package isearch
  :if (version<= "27.1" emacs-version)
  :bind (:map isearch-mode-map
         ([remap isearch-delete-char] . isearch-del-char))
  :custom (isearch-lazy-count t)
  (isearch-allow-motion t))

(use-package anzu
  :ensure t
  :bind ([remap query-replace] . anzu-query-replace))

(use-package mouse
  :if (and (display-graphic-p) (version<= "28.1" emacs-version))
  :init (context-menu-mode))

(use-package pixel-scroll
  :if (and (version<= "29.1" emacs-version) (display-graphic-p))
  :init (pixel-scroll-precision-mode))

(use-package nerd-icons
  :if (require 'nerd-icons nil 'noerror)
  :config
  (use-package nerd-icons-completion
    :ensure t
    :config (nerd-icons-completion-mode))
  (use-package nerd-icons-dired
    :ensure t
    :diminish nerd-icons-dired-mode
    :hook (dired-mode . nerd-icons-dired-mode)))

