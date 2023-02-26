;;; module-emacs-lisp.el

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

(use-package paredit
  :disabled
  :init (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(use-package lispy
  :config
  (add-hook 'emacs-lisp-mode 'lispy-mode))

(use-package paren-face
  :config
  (progn (global-paren-face-mode t)
         (set-face-attribute 'parenthesis nil :inherit 'line-number)))

(use-package macrostep)

(provide 'module-emacs-lisp)

;;; module-emacs-lisp.el ends here
