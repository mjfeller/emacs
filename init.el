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

(message "Loading core...")

(require 'core-bootstrap)
(require 'core-defuns)
(require 'core-packages)
(when (eq system-type 'darwin) (require 'core-macos))

(message "Loading modules...")

(require 'module-eldoc)
(require 'module-emacs-lisp)
(require 'module-evil)
(require 'module-git)
(require 'module-go)
(require 'module-java)
(require 'module-ledger)
(require 'module-nix)
(require 'module-notmuch)
(require 'module-org)
(require 'module-osm)
(require 'module-rust)
(require 'module-toml)
(require 'module-vterm)
(require 'module-yaml)
(require 'module-zig)

;;; init.el ends here
