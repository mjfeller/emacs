;;; module-rust.el --- Packages used for rust

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

(defun mjf-rust-mode-compilation ()
  "Customize compile command for `rust-mode'"
  (set (make-local-variable 'compile-command)
       "cargo build"))

(use-package rust-mode
  :bind
  (:map rust-mode-map ("C-c C-c" . compile))

  :custom
  (rust-format-on-save t)

  :hook
  (rust-mode-hook . mjf-rust-mode-compilation))

(provide 'module-rust)

;;; module-rust.el ends here
