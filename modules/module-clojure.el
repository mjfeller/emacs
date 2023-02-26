;;; module-clojure.el

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

(use-package clojure-mode
  :mode "\\.clj\\'"
  :init
  (progn
    (add-hook 'clojure-mode-hook 'paredit-mode)))

(use-package flycheck-clojure
  :after (clojure-mode flycheck-mode)
  :config (add-hook 'clojure-mode-hook 'flycheck-mode))

(use-package cider
  :after (clojure-mode))

(use-package cljdoc
  :after (clojure-mode eldoc))

(provide 'module-clojure)

;;; module-clojure.el ends here
