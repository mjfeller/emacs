;;; module-go.el --- Packages used for golang

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

(with-eval-after-load 'go-ts-mode
  (require 'reformatter)
  (reformatter-define go-format :program "goimports")

  (defun mjf-go-mode-compilation ()
    "Customize compile command for `go-mode'"
    (set (make-local-variable 'compile-command)
         "go build -v && go vet && go test -v"))

  (add-hook 'go-ts-mode-hook 'mjf-go-mode-compilation)
  (add-hook 'go-ts-mode-hook 'go-format-on-save-mode)

  (bind-keys :map go-ts-mode-map ("C-c C-c" . compile)))

(provide 'module-go)

;;; module-go.el ends here
