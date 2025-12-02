;;; module-evil.el --- Evil mode (Vi keybindings) configuration -*- lexical-binding: t; -*-

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
;;
;; Configuration for Evil mode, providing Vi/Vim-like modal editing in Emacs.
;; Includes key bindings and integration with various modes.

;;; Code:

(use-package evil
  :custom
  (evil-want-C-u-scroll t)
  (evil-echo-state nil)
  (evil-esc-delay 0)

  :config
  (evil-set-initial-state 'sly-inspector-mode 'emacs)
  (evil-set-initial-state 'sly-mrepl-mode 'emacs)
  (evil-set-initial-state 'sly-db-mode 'emacs)
  (evil-set-initial-state 'compilation-mode 'emacs)
  (evil-set-initial-state 'grep-mode 'emacs)
  (evil-set-initial-state 'compilation-mode 'emacs)
  (evil-set-initial-state 'xref--xref-buffer-mode 'emacs)
  (evil-mode t)

  (define-key evil-normal-state-map (kbd "C-n") nil)
  (define-key evil-insert-state-map (kbd "C-n") nil)
  (define-key evil-replace-state-map (kbd "C-n") nil)
  (define-key evil-normal-state-map (kbd "C-p") nil)
  (define-key evil-insert-state-map (kbd "C-p") nil)
  (define-key evil-replace-state-map (kbd "C-p") nil))

(use-package evil-goggles
  :custom
  (evil-goggles-pulse t)
  (evil-goggles-blocking-duration 0.100)
  (evil-goggles-async-duration 0.300)

  :config
  (evil-goggles-mode t))

(use-package evil-surround
  :config
  (global-evil-surround-mode t))

(provide 'module-evil)

;;; module-evil.el ends here
