;;; module-js.el

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

(use-package js2-mode
  :mode (("\\.js\\'"  . js2-mode)
         ("\\.pac\\'" . js2-mode))
  :init
  (progn (add-to-list 'interpreter-mode-alist '("node" . js2-mode))))

(use-package json-mode)

(provide 'module-js)

;;; module-js.el ends here
