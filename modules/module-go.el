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

(defvar mjf/go-compile-command
  "go build -v && go vet && go test -covermode=count -coverprofile=out.cover")

(defun mjf/go-prog-init ()
  "if go is install go get all dependencies for emacs go packages"
  (interactive)
  (if (not (eq (executable-find "go") nil))
      (progn
        (mapcar (lambda (pkg)
                  (let ((cmd (concat "go install " pkg)))
                    (call-process-shell-command cmd nil "*go-get-output*" t)))
                '("github.com/nsf/gocode@latest"
                  "golang.org/x/tools/cmd/goimports@latest"
                  "github.com/rogpeppe/godef@latest"
                  "golang.org/x/lint@latest"
                  "golang.org/x/tools/cmd/gorename@latest"
                  "golang.org/x/tools/cmd/guru@latest"
                  "github.com/kisielk/errcheck@latest"
                  "golang.org/x/tools/gopls@latest")))
    (message "go executable not found, install go from https://golang.org/download")))

(defun mjf/go-cover ()
  (interactive)
  (shell-command "go tool cover -html=out.cover"))

(defun mjf/setup-go-mode-compile ()
  "Customize compile command to run go build"
  (set (make-local-variable 'compile-command)
       mjf/go-compile-command))

(defun mjf/go-init-company ()
  "Initialize company mode for go"
  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode))

(use-package go-mode
  :demand
  :bind
  (:map go-mode-map
        ("C-c C-g" . go-goto-imports)
        ("C-c C-c" . compile)
        ("C-c C"   . go-cover))

  :init
  (unless (getenv "GOPATH")
    (setenv "GOPATH" (concat (getenv "HOME") "/development/go")))

  (setenv "PATH" (concat (getenv "PATH") ":"
                         (concat (getenv "GOPATH")"/bin")))

  :config
  (setq gofmt-command "goimports") ; use goimports instead of go-fmt
  (setq godoc-command "godoc")     ; use godoc instead of go doc
  (setq tab-width 8)

  (add-hook 'go-mode-hook 'mjf/setup-go-mode-compile)
  (add-hook 'go-mode-hook 'subword-mode)
  (add-hook 'go-mode-hook 'hs-minor-mode)
  (add-hook 'before-save-hook 'gofmt-before-save))

(provide 'module-go)

;;; module-go.el ends here
