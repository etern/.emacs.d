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

(require 'seq)

(setq git-prompt-sh-url "https://gitee.com/mirrors/git/raw/v2.17.1/contrib/completion/git-prompt.sh")
;; git completion 2.18 bug:  https://apple.stackexchange.com/a/328144/304525
;; "https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash"
(setq git-completion-sh-url "https://gitee.com/mirrors/git/raw/v2.17.1/contrib/completion/git-completion.bash")

(defun bb--append-to-file (filename &rest contents)
  (let ((text (apply #'concat contents)))
  (with-current-buffer (find-file filename)
    (goto-char (point-max))
    (unless (or (= (point) 1)
		(char-equal (char-before) ?\n) (insert "\n")))
    (insert text)
    (save-buffer)
    (kill-buffer (current-buffer)))))

(defun bb--append-to-bashrc (&rest contents)
  (apply #'bb--append-to-file "~/.bashrc" contents))

(defun has-git-prompt ()
  (let ((env-ps1 (getenv "PS1")))
    (if env-ps1
	(string-match (regexp-quote "__git_ps1") env-ps1)
      nil)))

(defun add-git-prompt ()
  (unless (has-git-prompt)
    (let ((script "~/.git-prompt.sh"))
      (unless (file-exists-p script)
	(url-retrieve git-prompt-sh-url
                      (lambda (status)
			(write-region (1+ url-http-end-of-headers) (point-max)
                                      script nil 0))))
      (bb--append-to-bashrc
       "source " script "\n"
       "export GIT_PS1_SHOWDIRTYSTATE=1\n"
       "export PS1='\\u@\\h \\w$(__git_ps1 \" (%s)\")\\$ '\n"))))

(defun has-git-completion ()
  (let ((bash-completions (split-string (shell-command-to-string "bash -lic complete")))
        (matches-git (lambda (haystack) (string-match (regexp-quote "git_main") haystack))))
    (seq-some matches-git bash-completions)))

(defun add-git-completion ()
  (unless (has-git-completion)
    (let ((script "~/.git-completion.sh"))
      (unless (file-exists-p script)
    (url-retrieve git-completion-sh-url
                  (lambda (status)
                    (write-region (1+ url-http-end-of-headers) (point-max) script nil 0))))
    (bb--append-to-bashrc "source " script))))

(defun tmux-config ()
  (bb--append-to-file "~/.tmux.conf"
		      "# remap prefix from 'C-b' to 'backtick'\n"
		      "unbind C-b\n"
		      "set -g prefix `\n"
		      "bind-key ` send-prefix\n"
		      "# Start window numbering at 1\n"
		      "set -g base-index 1\n"
		      "set -g status-bg colour240\n"
		      "# Current tab colour\n"
		      "set-window-option -g window-status-current-bg colour250\n"
		      "set-option -g status-position top\n"
		      "set-option -g renumber-windows on\n"
		      "set -g mouse on\n"))

(defun add-aliases ()
  (let* ((existing-aliases (split-string (shell-command-to-string "bash -lic alias") "\n"))
	 (extract-name (lambda (line) (if (string-match "alias *\\([^= ]*\\).*" line)
					  (match-string 1 line) nil)))
	 (existing-aliases (mapcar extract-name existing-aliases))
         (aliases '(("la" . "ls -A")
                    ("l" . "ls -CF")
                    ("ll" . "ls -lh --time-style=long-iso")
                    ("rm" . "rm -I")
                    ("mv" . "mv -i")
                    ("cp" . "cp -i")
                    ("rgrep" . "grep -r")
                    ("cls" . "printf \"\033c\"")))
	 (alias-names (mapcar #'car aliases)))
    (setq filtered (seq-filter (lambda (a) (not (seq-contains existing-aliases (car a)))) aliases))
    (setq alias-lines (mapcar (lambda (elm) (concat "alias " (car elm) "='" (cdr elm) "'")) filtered))
    (bb--append-to-bashrc (mapconcat 'identity alias-lines "\n"))))

(defun config-history ()
  (when (boundp 'exec-path-from-shell-copy-env)
    (exec-path-from-shell-copy-env "HISTIGNORE"))
  (let* ((ignore-str (getenv "HISTIGNORE"))
         (ignores (if ignore-str (split-string ignore-str ":") nil))
         (old-length (length ignores)))
    (add-to-list 'ignores "ls")
    (add-to-list 'ignores "fg")
    (add-to-list 'ignores "ll")
    (when (> (length ignores) old-length)
        (bb--append-to-bashrc"HISTIGNORE=\"" (mapconcat 'identity ignores ":") "\""))))


(defun bootstrap-bash ()
  (interactive)
  (when (y-or-n-p "config history macro?") (config-history))
  (when (y-or-n-p "init git prompt?") (add-git-prompt))
  (when (y-or-n-p "init git completion?") (add-git-completion))
  (when (y-or-n-p "init alias?") (add-aliases))
  (when (y-or-n-p "init tmux config?") (tmux-config)))

(provide 'bootstrap-bash)
;;; bootstrap-bash.el ends here
