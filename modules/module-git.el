;;; module-git.el

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

(defun mjf/magit-status-with-prefix ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'magit-status)))

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x G" . mjf/magit-status-with-prefix))
  :init
  (setq transient-history-file "~/.cache/transient/history.el")
  (setq transient-display-buffer-action '(display-buffer-below-selected))
  (setq transient-mode-line-format
        '("%e" mode-line-front-space mode-line-buffer-identification)))

(use-package magithub
  :disabled
  :after magit
  :config
  (magithub-feature-autoinject t)
  (setq magithub-api-timeout 5)
  (setq magithub-dir "~/.cache/magithub"))

(use-package git-timemachine)

(provide 'module-git)

;;; module-git.el ends here
