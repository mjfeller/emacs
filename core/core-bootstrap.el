;;; core-bootstrap.el --- bootstrap for use-package

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

;; Setup Emacs package repos and grab use-package for rest of package
;; management.

;;; Code:

(require 'package)

(setq package-user-dir "~/.cache/elpa")
(setq package-archives
      '(("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")
        ("gnu"          . "http://elpa.gnu.org/packages/")
        ("org"          . "http://orgmode.org/elpa/")))

(package-initialize)

;; ;; bootstrap use-package
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package)
;;   (package-install 'bind-key))

(require 'use-package)
(setq use-package-always-ensure t)

(provide 'core-bootstrap)

;;; core-bootstrap.el ends here
