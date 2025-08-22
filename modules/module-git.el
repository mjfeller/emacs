;;; module-git.el --- Configuration -*- lexical-binding: t; -*-

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

(use-package magit
  :bind (("C-x g" . magit-status))
  :custom
  (transient-history-file "~/.cache/emacs/transient/history.el")
  (transient-display-buffer-action '(display-buffer-below-selected)))

(use-package git-timemachine
  :defer t)

(provide 'module-git)

;;; module-git.el ends here
