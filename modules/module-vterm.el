;;; module-vterm.el

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

(use-package vterm
  :bind (:map vterm-mode-map ("M-P" . mjf/pash-copy))
  :config
  (add-to-list 'evil-emacs-state-modes 'vterm-mode)
  (add-to-list 'evil-emacs-state-modes 'compilation-mode))

(if (eq system-type 'darwin)

    ;; macos
    (use-package multi-vterm
      :bind (("C-x p t" . multi-vterm-project)
             ("H-T"     . multi-vterm-dedicated-open)
             ("H-u"     . multi-vterm-next)
             ("H-i"     . multi-vterm-prev)
             ("H-y"     . multi-vterm)))

  ;; linux
  (use-package multi-vterm
    :bind (("C-x p t" . multi-vterm-project)
           ("s-T"     . multi-vterm-dedicated-open)
           ("s-u"     . multi-vterm-next)
           ("s-i"     . multi-vterm-prev)
           ("s-y"     . multi-vterm))))

(provide 'module-vterm)

;;; module-vterm.el ends here
