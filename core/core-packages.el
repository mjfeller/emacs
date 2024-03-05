;;; core-packages.el --- Core packages for my Emacs config

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

(use-package emacs
  :custom
  (line-spacing 6)
  (display-line-numbers-width 3)
  (indent-tabs-mode nil)
  (tab-width 8)
  (indicate-empty-lines nil)
  (fill-column 80)

  ;; perfer to split windows vertically instead of horizontally
  (split-height-threshold nil)
  (split-width-threshold 120)

  (ring-bell-function 'ignore) ; disable the annoying bell ring
  (inhibit-startup-screen t)   ; disable startup screen

  ;; nice scrolling
  (scroll-margin 0)
  (scroll-conservatively 100000)
  (scroll-preserve-screen-position 1)

  (echo-keystrokes 0.1)
  (frame-resize-pixelwise t)

  ;; line numbers
  (display-line-numbers-type 'relative)
  (display-line-numbers-current-absolute t)
  (display-line-numbers-widen nil)      ; don't count narrowed regions

  ;; save place improvements
  (save-place-file "~/.cache/emacs/places")
  (backup-by-copying t)
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (version-control t)

  (isearch-lazy-count t)
  (search-whitespace-regexp ".*?")

  (backup-directory-alist `((".*" . ,temporary-file-directory)))
  (auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

  (auto-save-list-file-prefix         "~/.cache/emacs/auto-save-list/.saves-")
  (bookmark-default-file              "~/.cache/emacs/bookmarks.el")
  (ede-project-placeholder-cache-file "~/.cache/emacs/ede-projects.el")
  (semanticdb-default-save-directory  "~/.cache/emacs/semanticdb")
  (abbrev-file-name                   "~/.cache/emacs/abbrev_defs.el")
  (tramp-persistency-file-name        "~/.cache/emacs/tramp.el")
  (recentf-save-file                  "~/.cache/emacs/recentf")
  (org-id-locations-file              "~/.cache/emacs/org-id-locations.el")
  (nsm-settings-file                  "~/.cache/emacs/network-security.el")
  (url-configuration-directory        "~/.cache/emacs/url/")

  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain)

  :config
  (set-frame-font "Iosevka Comfy Wide Motion 10" nil t)

  (setopt use-short-answers t)          ; enable y/n answers
  (scroll-bar-mode 0)                   ; disable the scroll bar
  (menu-bar-mode 0)                     ; disable the menu bar
  (blink-cursor-mode 0)                 ; the blinking cursor is nothing but an annoyance
  (electric-pair-mode t)
  (global-auto-revert-mode t)
  (line-number-mode t)                  ; line numbers in mode line
  (column-number-mode t)                ; column numbers in mode line
  (delete-selection-mode t)             ; delete marked regions
  (show-paren-mode t)                   ; show matching parenthesis

  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)

  :hook
  (before-save-hook . delete-tailing-whitespace)
  (window-setup-hook . on-after-init)

  :bind
  ("H-l" . display-line-numbers-mode))

(use-package project
  :bind (("C-x p g" . consult-ripgrep)
         ("C-x p b" . consult-project-buffer)
         ("C-x p m" . magit-project-status))

  :custom
  (project-list-file "~/.cache/emacs/projects.el")
  (project-switch-commands '((project-find-file    "Find file"      "f")
                             (project-find-dir     "Find directory" "d")
                             (consult-ripgrep      "Find regexp"    "g")
                             (project-kill-buffers "Kill buffers"   "k")
                             (magit-project-status "Magit"          "m"))))

(use-package modus-themes
  :config
  (load-theme 'modus-vivendi t))

(use-package tree-sitter
  :config
  (global-tree-sitter-mode)

  :hook
  (tree-sitter-after-on-hook . tree-sitter-hl-mode))

(use-package eglot
  :bind (:map eglot-mode-map
              ("C-c r"   . eglot-rename)
              ("C-c o"   . eglot-code-action-organize-imports)
              ("C-c h"   . eldoc)
              ("C-c C-J" . xref-show-definitions-function)
              ("M-."     . xref-show-definitions-function)
              ("M-,"     . xref-go-back))

  :config
  (add-to-list 'eglot-stay-out-of 'flymake)

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(nix-mode . ("nil"))
                 '(java-mode . ("java-language-server")))))

(use-package company
  :demand
  :config
  (setq company-idle-delay 0)
  (setq company-tooltip-limit 10)
  (setq company-echo-delay 0)
  (setq company-tooltip-flip-when-above t)
  (setq company-begin-commands '(self-insert-command))

  (global-company-mode))

(use-package comment-dwim-2
  :bind (("M-;" . comment-dwim-2)))

(use-package dired
  :ensure nil
  :bind (("C-x C-j" . dired-jump))
  :custom
  (wdired-use-dired-vertical-movement 'sometimes)
  (dired-listing-switches "-la")

  :config
  (defun dired-sort-dir-first ()
    "Sort dired listings with directories first."
    (save-excursion
      (let (buffer-read-only)
        (forward-line 2) ;; beyond dir. header
        (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
      (set-buffer-modified-p nil)))

  (defadvice dired-readin
      (after dired-after-updating-hook first () activate)
    "Sort dired listings with directories first before adding marks."
    (dired-sort-dir-first)))

(use-package helpful
  :bind (("C-h f" . helpful-function)
         ("C-h o" . helpful-symbol)
         ("C-h k" . helpful-key)
         ("C-h v" . helpful-variable)))

(use-package rg
  :config
  (rg-enable-default-bindings))

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :custom
  (vertico-count 4)
  (vertico-mode t))

(use-package consult
  :bind (("C-c r l"   . consult-register-load)
         ("C-c r s"   . consult-register-store)

         ("M-s M-f"   . consult-find)
         ("M-K"       . consult-keep-lines)
         ("M-F"       . consult-focus-lines)
         ("M-g M-g"   . consult-goto-line)
         ("M-s M-g"   . consult-git-grep)
         ("M-s M-b"   . consult-buffer)
         ("M-s M-i"   . consult-imenu)
         ("M-s M-l"   . consult-line)
         ("M-s M-m"   . consult-mark)
         ("M-s M-s"   . consult-outline)
         ("M-s M-h"   . consult-history)
         ("M-s M-y"   . consult-yank-pop))

  :config
  (require 'consult-imenu))

(use-package embark
  :bind (("C-."   . embark-act)       ;; pick some comfortable binding
         ("C-;"   . embark-dwim)      ;; good alternative: M-.
         ("C-h B" . embark-bindings)  ;; alternative for `describe-bindings'
         (:map minibuffer-mode-map
               ("C-c C-o" . embark-export))))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package olivetti)

(use-package spacious-padding
  :custom
  (spacious-padding-subtle-mode-line '(:mode-line-active shadow
                                       :mode-line-inactive vertical-border))
  :config
  (spacious-padding-mode t))

;; window management
(if (eq system-type 'darwin)
    (setq window-management-prefix "H")
  (setq window-management-prefix "s"))

(bind-keys
 ;; Window Switching
 ((concat window-management-prefix "-e") . windmove-up)
 ((concat window-management-prefix "-d") . windmove-down)
 ((concat window-management-prefix "-f") . windmove-right)
 ((concat window-management-prefix "-s") . windmove-left)

 ;; Window Moving
 ((concat window-management-prefix "-E") . windmove-swap-states-up)
 ((concat window-management-prefix "-D") . windmove-swap-states-down)
 ((concat window-management-prefix "-F") . windmove-swap-states-right)
 ((concat window-management-prefix "-S") . windmove-swap-states-left)

 ;; Window Splitting
 ((concat window-management-prefix "-v") . split-window-vertically)
 ((concat window-management-prefix "-r") . split-window-horizontally)
 ((concat window-management-prefix "-w") . delete-window)
 ((concat window-management-prefix "-q") . delete-other-windows)

 ;; Misc Window Commands
 ((concat window-management-prefix "-a") . balance-windows)
 ((concat window-management-prefix "-t") . toggle-window-split)
 ((concat window-management-prefix "-<return>") . toggle-fullscreen)

 ((concat window-management-prefix "-W") . mjf/focused)
 ((concat window-management-prefix "-n") . narrow-or-widen-dwim))

(provide 'core-packages)

;;; core-packages.el ends here
