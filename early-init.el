;;; early-init.el --- early init -*- lexical-binding: t; -*-

(setq gc-cons-threshold (* 50 1024 1024))
(setq vc-handled-backends nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
