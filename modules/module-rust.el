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

(defun mjf/setup-rust-mode-compile ()
  "Customize compile command to run go build"
  (if (not (string-match "cargo" compile-command))
      (set (make-local-variable 'compile-command)
           "cargo build")))

(use-package rust-mode
  :bind
  (:map rust-mode-map
        ("C-c C-c"   . compile)
        ("C-c <tab>" . rust-format-buffer))
  :custom
  (rust-format-on-save t)
  :config
  (add-hook 'rust-mode-hook 'mjf/setup-rust-mode-compile))

(use-package racer
  :after (rust-mode)
  :bind
  (:map rust-mode-map
        ("TAB"   . company-indent-or-complete-common))
  :custom
  (company-tooltip-align-annotations t)
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))

(provide 'module-rust)

;;; module-rust.el ends here
