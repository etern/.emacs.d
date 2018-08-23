(package-initialize)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(eval-when-compile
  (require 'use-package))

(global-set-key (kbd "C-c , s") 'semantic-ia-show-summary)

;;cmake-mode
(setq auto-mode-alist
          (append
           '(("CMakeLists\\.txt\\'" . cmake-mode))
           '(("\\.cmake\\'" . cmake-mode))
           auto-mode-alist))
(autoload 'cmake-mode "cmake-mode" t)

(when (display-graphic-p)
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
(setq org-agenda-files (directory-files-recursively "E:/opt/knowledges/" "\.org$"))
(setq org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9)))
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

(load "setup-org-publish.el")

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(require 'yasnippet)
(yas-reload-all)
(add-hook 'org-mode-hook #'yas-minor-mode)
(require 'wget)

(global-set-key (kbd "C-<f8>") #'imenu-list-smart-toggle)
(global-set-key [f8] 'neotree-toggle)
(require 'all-the-icons)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(scroll-bar-mode -1)
(nyan-mode)

(setq ring-bell-function 'ignore)
(setq make-backup-files nil)
(setq inhibit-startup-screen t)
