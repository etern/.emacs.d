;;; bootstrap-bash.el --- Bootstrap my bash env      -*- lexical-binding: t; -*-

;; Copyright (C) 2018  

;; Author:  <jingxiaobing@bigdata-repeat-call23.nmg01>
;; Keywords: files

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(setq git-prompt-sh-url "https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh")

(defun add-git-prompt ()
  (url-retrieve git-prompt-sh-url
		(lambda (status)
		  (write-region (1+ url-http-end-of-headers) (point-max)
				"~/.git-prompt.sh")
		  (message "Save %s to ~/.git-prompt.sh" git-prompt-sh-url)))
  (with-current-buffer (find-file "~/.bashrc")
    (goto-char (point-max))
    (insert "\n")
    (insert ". ~/.git-prompt.sh\n")
    (insert "export GIT_PS1_SHOWDIRTYSTATE=1\n")
    (insert "export PS1='\\u@\\h \\w$(__git_ps1 \" (%s)\")\\$ '\n")
    (save-buffer))
  )

(defun tmux-config ()
  (with-current-buffer (find-file "~/.tmux.conf")
    (goto-char (point-max))
    (insert "\n")
    (insert "set-option -g default-command bash")
    (save-buffer)))

(defun bootstrap-bash ()
  (interactive)
  (add-git-prompt)
  (tmux-config))

(provide 'bootstrap-bash)
;;; bootstrap-bash.el ends here
