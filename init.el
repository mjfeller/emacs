;;; init.el  --- initialize

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

;;---------------------------------------------------------------------
;; Init
;;---------------------------------------------------------------------

(defvar current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

;; Always load newest byte code
(setq load-prefer-newer t)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;;---------------------------------------------------------------------
;; Variables and Load Paths
;;---------------------------------------------------------------------

(defvar emacs-dir (file-name-directory "~/.config/emacs/")
  "The root dir of the Emacs distribution.")

(defvar core-dir (expand-file-name "core" emacs-dir)
  "The home of core functionality.")

(defvar modules-dir (expand-file-name "modules" emacs-dir)
  "This directory houses all of the modules.")

(defvar lisp-dir (expand-file-name "lisp" emacs-dir)
  "This directory houses user lisp and site lisp")

(if (file-exists-p (concat emacs-dir "src"))
  (setq source-directory (concat emacs-dir "src")))

(add-to-list 'load-path core-dir)
(add-to-list 'load-path modules-dir)
(add-to-list 'load-path lisp-dir)

;; load the custom file
(setq custom-file "~/.cache/emacs/custom.el")
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

;;---------------------------------------------------------------------
;; Core
;;---------------------------------------------------------------------

(message "Loading core...")

(require 'core-bootstrap)
(require 'core-defuns)
(require 'core-packages)
(when (eq system-type 'darwin) (require 'core-macos))

;;---------------------------------------------------------------------
;; Modules
;;---------------------------------------------------------------------

(message "Loading modules...")

(require 'module-c)
(require 'module-docker)
(require 'module-eldoc)
(require 'module-emacs-lisp)
(require 'module-evil)
(require 'module-git)
(require 'module-go)
(require 'module-ledger)
(require 'module-notmuch)
(require 'module-org)
(require 'module-osm)
(require 'module-vterm)
(require 'module-yaml)

;; load any custom user provided locals
(require 'init-local)

;;; init.el ends here
