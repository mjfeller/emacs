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

;; Of all the terminal emulators in emacs vterm appears to be the most reliable
;; and behaves like standard terminals. Requires C libraries to be compiled when
;; initialized.

;;; Code:

(use-package vterm
  :bind (("s-U" . vterm-other-window))

  :custom
  (vterm-max-scrollback 100000) ; max scrollback vterm supports

  :config
  (add-to-list 'evil-emacs-state-modes 'vterm-mode)
  (add-to-list 'evil-emacs-state-modes 'compilation-mode)

  ;; prefer vterm windows to act as dedicated popup windows to the right
  (add-to-list 'display-buffer-alist '("\\*vterm\\*"
                                       (display-buffer-reuse-window display-buffer-in-direction)
                                       (side . right)
                                       (dedicated . t))))

(use-package multi-vterm
  :bind (("s-U" . multi-vterm-dedicated-open)
         ("s-u" . multi-vterm-next)
         ("s-i" . multi-vterm-prev)
         ("s-y" . multi-vterm)))

(provide 'module-vterm)

;;; module-vterm.el ends here
