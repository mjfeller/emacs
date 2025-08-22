;;; module-lang.el --- Packages used for language

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
;;
;; Provides configuration for programming languages including:
;; - CSV
;; - Go (tree-sitter)
;; - Nix (tree-sitter)
;; - Java (tree-sitter)
;; - Protobuf
;; - Rust (tree-sitter)
;; - TOML (tree-sitter)
;; - YAML (tree-sitter)
;; - Zig
;;
;; Features:
;; - Automatic formatting for Go and Rust
;; - Custom compilation commands
;; - Tree-sitter based syntax highlighting where available
;; - Consistent keybindings across languages

;;; Code:

(defun mjf-ensure-tool (tool-name)
  "Ensure that TOOL-NAME is available in the system."
  (unless (executable-find tool-name)
    (message "Warning: %s not found. Some features may not work." tool-name)))

;; csv

(use-package csv-mode)

;; go

(use-package go-ts-mode
  :requires (reformatter)

  :bind (:map go-ts-mode-map ("C-c C-c" . compile))

  :hook
  (go-ts-mode-hook . mjf-setup-go-mode-compilation)
  (go-ts-mode-hook . go-format-on-save-mode)
  (go-ts-mode-hook . corfu-mode)

  :mode
  ("\\.go\\'"    . go-ts-mode)
  ("\\go.mod\\'" . go-mod-ts-mode)

  :init
  (mjf-ensure-tool "goimports")
  (reformatter-define go-format :program "goimports")

  (defun mjf-setup-go-mode-compilation ()
    "Setup the compile command for `go-mode'"
    (set (make-local-variable 'compile-command)
         "go build -v && go vet && go test -v")))

;; java

(use-package java-ts-mode
  :mode
  ("\\.java\\'" . java-ts-mode))

;; nix

(use-package nix-ts-mode
  :mode
  ("\\.nix\\'"       . nix-ts-mode)
  ("\\flake.lock\\'" . json-ts-mode))

;; protobuf

(use-package protobuf-mode
  :bind (:map protobuf-mode-map
              ("C-c C-c" . compile)))

;; rust

(use-package rust-ts-mode
  :requires (reformatter)

  :bind (:map rust-ts-mode-map ("C-c C-c" . compile))

  :hook
  (rust-ts-mode-hook . mjf-setup-rust-ts-mode-compilation)
  (rust-ts-mode-hook . rust-format-on-save-mode)

  :mode
  ("\\.rs\\'" . rust-ts-mode)

  :init
  (mjf-ensure-tool "rustfmt")
  (reformatter-define rust-format :program "rustfmt")

  (defun mjf-setup-rust-ts-mode-compilation ()
    "Setup the compile command for `rust-ts-mode'"
    (set (make-local-variable 'compile-command)
         "cargo build")))

;; toml

(use-package toml-ts-mode
  :mode
  ("\\.toml\\'" . toml-ts-mode))

;; yaml

(use-package yaml-ts-mode
  :mode
  ("\\.ya?ml\\(\\.envtpl\\|\\.template\\)?\\'" . yaml-ts-mode))

;; zig

(use-package zig-mode
  :bind (:map zig-mode-map ("C-c C-c" . compile))

  :hook
  (zig-mode-hook . mjf-setup-zig-mode-compilation)

  :init
  (mjf-ensure-tool "rustfmt")

  (defun mjf-setup-zig-mode-compilation ()
    "Setup the compile command for `zig-mode'"
    (set (make-local-variable 'compile-command)
         "zig build")))

(provide 'module-lang)

;;; module-lang.el ends here
