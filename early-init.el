;;; early-init.el --- early emacs startup configuration

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

;; The early-init.el initializes variables before the bulk of emacs loads.

;;; Code:

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

(setq package-user-dir "~/.cache/elpa")                      ; move packages to the cache directory

(setq native-comp-eln-load-path '( "~/.cache/emacs/eln")     ; move compiled code to cache dir
      native-comp-async-report-warnings-errors 'silent       ; suppress compiler warnings
      byte-compile-warnings '(not obsolete)                  ; reduce compiler warnings
      warning-suppress-log-types '((comp) (bytecomp)))       ; suppress extra logs

(setq frame-resize-pixelwise t)
(pixel-scroll-precision-mode t)
(tool-bar-mode 0)                                            ; disable the tool bar
(scroll-bar-mode 0)                                          ; disable the scroll bar
(set-fringe-mode 10)                                         ; set fringe width
(set-frame-parameter nil 'internal-border-width 0)           ; remove frame border

(setq default-frame-alist '((width . 85)
                            (height . 50)

                            ;; You can turn off scroll bars by uncommenting these lines:
                            (vertical-scroll-bars . nil)
                            (horizontal-scroll-bars . nil)

                            ;; Setting the face in here prevents flashes of
                            ;; color as the theme gets activated
                            (background-color . "#000000")
                            (undecorated . t)
                            (ns-appearance . dark)
                            (ns-transparent-titlebar . t)))

(setq shell-file-name "zsh")

(add-to-list 'load-path "~/.config/emacs/lisp")

(require 'prot-modeline)

;;; Mode line
(setq mode-line-compact nil) ; Emacs 28
(setq mode-line-right-align-edge 'right-margin) ; Emacs 30
(setq-default mode-line-format
              '("%e"
                prot-modeline-kbd-macro
                prot-modeline-narrow
                prot-modeline-buffer-status
                prot-modeline-input-method
                prot-modeline-evil
                prot-modeline-buffer-identification
                "  "
                prot-modeline-major-mode
                prot-modeline-process
                "  "
                prot-modeline-vc-branch
                "  "
                prot-modeline-kube-context
                "  "
                prot-modeline-eglot))

;;; early-init.el ends here
