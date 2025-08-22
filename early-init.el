;;; early-init.el --- Early Emacs startup configuration -*- lexical-binding: t; -*-

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

(defvar emacs-dir (expand-file-name "~/.config/emacs/")
  "The root dir of the Emacs distribution.")

(defvar core-dir (expand-file-name "core" emacs-dir)
  "The home of core functionality.")

(defvar modules-dir (expand-file-name "modules" emacs-dir)
  "This directory houses all of the modules.")

(defvar lisp-dir (expand-file-name "lisp" emacs-dir)
  "This directory houses user lisp and site lisp")

(if (file-exists-p (concat emacs-dir "src"))
  (setq source-directory (concat emacs-dir "src")))

;; setup all critical load paths
(add-to-list 'load-path core-dir)
(add-to-list 'load-path modules-dir)
(add-to-list 'load-path lisp-dir)

(setq use-package-compute-statistics t)
(setq gc-cons-threshold (* 1000 1000 50))                    ; reduce frequency of garbage collection
(setq gc-cons-percentage 0.1)
(setq package-user-dir "~/.cache/emacs/elpa")                ; move packages to the cache directory
(setq native-comp-eln-load-path '( "~/.cache/emacs/eln"))    ; move compiled code to cache dir
(setq native-comp-async-report-warnings-errors 'silent)      ; suppress compiler warnings
(setq byte-compile-warnings '(not obsolete))                 ; reduce compiler warnings
(setq warning-suppress-log-types '((comp) (bytecomp)))       ; suppress extra logs
(setq inhibit-startup-echo-area-message user-login-name)
(setq frame-resize-pixelwise t)
(setq load-prefer-newer t)
(setq custom-file (make-temp-file "emacs-custom-"))          ; effectively disable the emacs custom file
(setq shell-file-name "zsh")

(tool-bar-mode 0)                                            ; disable the tool bar
(scroll-bar-mode 0)                                          ; disable the scroll bar
(set-fringe-mode 10)                                         ; set fringe width
(set-frame-parameter nil 'internal-border-width 0)           ; remove frame border

;; avoid white screen flashes on startup
(setq default-frame-alist '((background-color . "#000000")))

(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-to-list 'default-frame-alist '((ns-transparent-titlebar . t))))

(unless (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(undecorated . t)))

;; setup modeline before the default frame has been initialized
(require 'prot-modeline)
(when (>= emacs-major-version 28) (setq mode-line-compact nil))
(when (>= emacs-major-version 30) (setq mode-line-right-align-edge 'right-margin))
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
