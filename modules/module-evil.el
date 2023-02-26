;;; module-evil.el

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

(use-package evil
  :bind
  (:map evil-normal-state-map ("C-u" . evil-scroll-up))
  (:map evil-visual-state-map ("C-u" . evil-scroll-up))
  (:map evil-insert-state-map ("C-u" . evil-scroll-up))

  :config
  (setq evil-mode-line-format '(before . mode-line-front-space))
  (setq evil-echo-state nil)
  (setq evil-esc-delay 0)

(add-to-list 'evil-emacs-state-modes 'sly-inspector-mode)
(add-to-list 'evil-emacs-state-modes 'sly-mrepl-mode)
(add-to-list 'evil-emacs-state-modes 'sly-db-mode)
(add-to-list 'evil-emacs-state-modes 'compilation-mode)

  (evil-mode t))

(use-package evil-goggles
  :config
  (setq evil-goggles-pulse t)
  (setq evil-goggles-blocking-duration 0.100)
  (setq evil-goggles-async-duration 0.300)

  (evil-goggles-mode t))

(use-package evil-surround
  :config (global-evil-surround-mode t))

(use-package evil-commentary
  :disabled
  :config (evil-commentary-mode t))

(use-package evil-snipe
  :disabled
  :config (evil-snipe-override-mode t))

(use-package evil-org
  :disabled
  :config
  (require 'evil-org-agenda)

  (add-hook 'org-mode-hook 'evil-org-mode)

  (evil-org-agenda-set-keys)
  (evil-org-set-key-theme '(textobjects
                            insert
                            navigation
                            additional
                            shift
                            todo
                            heading)))

(provide 'module-evil)

;;; module-evil.el ends here
