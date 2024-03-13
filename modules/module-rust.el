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

(with-eval-after-load 'rust-ts-mode
  (require 'reformatter)
  (reformatter-define rust-format :program "rustfmt")

  (defun mjf-rust-ts-mode-compilation ()
    "Customize compile command for `rust-ts-mode'"
    (set (make-local-variable 'compile-command)
         "cargo build"))

  (add-hook 'rust-ts-mode-hook 'mjf-rust-ts-mode-compilation)
  (add-hook 'rust-ts-mode-hook 'rust-format-on-save-mode)

  (bind-keys :map rust-ts-mode-map ("C-c C-c" . compile)))

(provide 'module-rust)

;;; module-rust.el ends here
