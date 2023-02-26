;;; module-eshell.el

;; Author: Mark Feller <mark.feller@member.fsf.org>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defun mjf/set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path` and PATH environment variable to
  match that used by the user's shell. This is particularly
  useful under macOS, where GUI apps are not started from a
  shell."
  (interactive)
  (let ((path-from-shell
         (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(use-package eshell
  :bind
  (:map eshell-mode-map
        ("C-l". evil-scroll-line-to-top))

  :config
  (setq eshell-directory-name "~/.cache/eshell"
        eshell-cmpl-cycle-completions nil
        eshell-buffer-maximum-lines 20000
        eshell-history-size 350
        eshell-buffer-shorthand t ; buffer shorthand -> echo foo > #'buffer
        eshell-highlight-prompt nil
        eshell-plain-echo-behavior t) ; treat 'echo' like shell echo

  (add-hook 'eshell-mode-hook 'disable-line-numbers)
  (add-to-list 'eshell-load-hook 'mjf/set-exec-path-from-shell-PATH))

(provide 'module-eshell)

;;; module-eshell.el ends here
