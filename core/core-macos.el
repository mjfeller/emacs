;;; core-osx.el --- solarized module for my emacs

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

;; transparent title bar
(use-package ns-auto-titlebar
  :config (ns-auto-titlebar-mode))

(setq ns-use-srgb-colorspace t)
(setq ns-use-proxy-icon nil)    ; hide icon in title bar
(setq frame-title-format nil)   ; hide text in title bar
(menu-bar-mode t)               ; use macos menu bar

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

;; Enable emoji, and stop the UI from freezing when trying to display them.
(if (fboundp 'set-fontset-font)
    (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

(use-package exec-path-from-shell
  :custom
  ;; speed up loads by using a non-interactive shell
  (exec-path-from-shell-arguments nil))

(dolist (var '("GNUPGHOME" "NOTMUCH_CONFIG" "XDG_DOCUMENTS_DIR"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize)

(defun finder ()
  "Opens file directory in Finder."
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
        (shell-command
         (format "%s %s" (executable-find "open") (file-name-directory file)))
      (error "Buffer is not attached to any file."))))

(when (not (display-graphic-p))
  (defun bw/copy-from-osx ()
    "Copies the current clipboard content using the `pbcopy` command"
    (shell-command-to-string "pbpaste"))

  (defun bw/paste-to-osx (text &optional push)
    "Copies the top of the kill ring stack to the OSX clipboard"
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (setq interprogram-cut-function 'bw/paste-to-osx)
  (setq interprogram-paste-function 'bw/copy-from-osx))

(setq Info-directory-list
      (append '("/opt/homebrew/share/info" "/opt/homebrew/share/info/emacs")
              Info-directory-list))

(set-fontset-font "fontset-default" ? (font-spec :family "MesloLGL Nerd Font" :size 8))
(set-fontset-font "fontset-default" ? (font-spec :family "MesloLGL Nerd Font" :size 8))

(provide 'core-macos)

;;; core-osx.el ends here
