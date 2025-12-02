;;; init.el --- Emacs initialization file -*- lexical-binding: t; -*-

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

(message "Loading core...")

(require 'core-defuns)
(require 'core-packages)
(when (eq system-type 'darwin) (require 'core-macos))

(message "Loading modules...")

(require 'module-emacs-lisp)
(require 'module-evil)
(require 'module-git)
(require 'module-gpt)
(require 'module-lang)
(require 'module-ledger)
(require 'module-org)
(require 'module-osm)

;;; init.el ends here
