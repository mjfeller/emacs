;;; core-packages.el --- Core packages for my Emacs config -*- lexical-binding: t; -*-

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
  (split-width-threshold 140)

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

  (use-short-answers t)                      ; enable y/n answers
  (large-file-warning-threshold 100000000)   ; warn when opening files bigger than 100MB

  (treesit-font-lock-level 3)

  :config
  (set-frame-font "Iosevka Comfy 12" nil t)

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
  (prog-mode-hook . subword-mode)

  :bind (("s-l" . display-line-numbers-mode)

         ;; Window Switching
         ("s-e" . windmove-up)
         ("s-d" . windmove-down)
         ("s-f" . windmove-right)
         ("s-s" . windmove-left)

         ;; Window Moving
         ("s-E" . windmove-swap-states-up)
         ("s-D" . windmove-swap-states-down)
         ("s-F" . windmove-swap-states-right)
         ("s-S" . windmove-swap-states-left)

         ;; Window Splitting
         ("s-v" . split-window-vertically)
         ("s-r" . split-window-horizontally)
         ("s-w" . delete-window)
         ("s-q" . delete-other-windows)

         ;; Misc Window Commands
         ("s-a" . balance-windows)
         ("s-t" . toggle-window-split)
         ("s-<return>" . toggle-fullscreen)

         ("M-P" . mjf-pash-copy)
         ("s-W" . mjf-focused)
         ("s-n" . narrow-or-widen-dwim)

         ("C-c k k" . mjf-kubernetes-context-switch)

         ("s-H" . (lambda () (interactive) (point-to-register ?h)))
         ("s-h" . (lambda () (interactive) (jump-to-register ?h)))
         ("s-J" . (lambda () (interactive) (point-to-register ?j)))
         ("s-j" . (lambda () (interactive) (jump-to-register ?j)))
         ("s-K" . (lambda () (interactive) (point-to-register ?k)))
         ("s-k" . (lambda () (interactive) (jump-to-register ?k)))
         ("s-L" . (lambda () (interactive) (point-to-register ?l)))
         ("s-l" . (lambda () (interactive) (jump-to-register ?l)))))

(use-package project
  :bind (("C-x p g" . consult-ripgrep)
         ("C-x p b" . consult-project-buffer)
         ("C-x p m" . magit-project-status)
         ("C-x p M" . mjf-project-gh-browse)
         ("C-x p P" . mjf-project-gh-pr-create)
         ("C-x p C" . mjf-project-kochiku-canary)
         ("C-x p G" . mjf-clone-project))

  :custom
  (project-list-file "~/.cache/emacs/projects.el")
  (project-switch-commands '((project-find-file          "Find file"      "f")
                             (project-find-dir           "Find directory" "d")
                             (project-compile            "Compile"        "c")
                             (consult-ripgrep            "Find regexp"    "g")
                             (project-kill-buffers       "Kill buffers"   "k")
                             (multi-vterm-project        "vterm"          "v")
                             (magit-project-status       "Magit"          "m")
                             (mjf-project-gh-browse      "GitHub"         "M")
                             (mjf-project-gh-pr-create   "GitHub PR"      "P")
                             (mjf-project-kochiku-canary "Kochiku Canary" "C"))))

(use-package modus-themes
  :config
  (load-theme 'modus-vivendi t))

(use-package comment-dwim-2
  :bind (("M-;" . comment-dwim-2)))

(use-package dired
  :ensure nil
  :bind (("C-x C-j" . dired-jump))

  :custom
  (wdired-use-dired-vertical-movement 'sometimes)
  (dired-listing-switches "-lah")

  :config
  (defun dired-sort-dir-first ()
    "Sort dired listings with directories first."
    (save-excursion
      (let (buffer-read-only)
        (forward-line 2) ;; beyond dir. header
        (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
      (set-buffer-modified-p nil)))

  (advice-add 'dired-readin :after #'dired-sort-dir-first))

(use-package helpful
  :bind (("C-h f" . helpful-function)
         ("C-h o" . helpful-symbol)
         ("C-h k" . helpful-key)
         ("C-h v" . helpful-variable)))

(use-package rg
  :defer t

  :custom
  (rg-executable "/run/current-system/sw/bin/rg")

  :config
  (rg-enable-default-bindings))

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion orderless)))))

(use-package vertico
  :custom
  (vertico-count 4)
  (vertico-mode t))

(use-package consult
  :bind (("M-F"       . consult-focus-lines)
         ("M-K"       . consult-keep-lines)
         ("M-g M-g"   . consult-goto-line)
         ("M-s M-b"   . consult-buffer)
         ("M-s M-f"   . consult-find)
         ("M-s M-g"   . consult-git-grep)
         ("M-s M-h"   . consult-history)
         ("M-s M-i"   . consult-imenu)
         ("M-s M-l"   . consult-line)
         ("M-s M-m"   . consult-mark)
         ("M-s M-s"   . consult-outline)
         ("M-s M-t"   . consult-theme)
         ("M-s M-y"   . consult-yank-pop))

  :config
  (require 'consult-imenu))

(use-package embark
  :bind (("C-."   . embark-act)
         ("C-;"   . embark-dwim)
         ("C-h B" . embark-bindings)

         :map minibuffer-mode-map
         ("C-c C-o" . embark-export)))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle))

  :init
  (marginalia-mode))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 0)
  (corfu-bar-width 0.25)
  (corfu-left-margin-width 1.5)
  (corfu-right-margin-width 1.5)

  :bind
  (:map corfu-map
        ("C-n" . corfu-next)
        ("TAB" . corfu-next)
        ("C-p" . corfu-previous)
        ("S-TAB" . corfu-previous))

  :init
  (global-corfu-mode))

(use-package cape
  :bind ("C-c p" . cape-prefix-map))

(use-package olivetti
  :defer t)

(use-package spacious-padding
  :custom
  (spacious-padding-subtle-mode-line '(:mode-line-active shadow
                                       :mode-line-inactive vertical-border))
  :config
  (spacious-padding-mode t))

(use-package eldoc
  :config (global-eldoc-mode))

(use-package vterm
  :bind (("s-U" . vterm-other-window)

         :map vterm-mode-map
         ("M-P" . mjf/pash-copy))

  :custom
  (vterm-max-scrollback 100000) ; max scrollback vterm supports

  :config
  (evil-set-initial-state 'vterm-mode 'emacs)

  ;; prefer vterm windows to act as dedicated popup windows to the right
  (add-to-list 'display-buffer-alist '("\\*vterm\\*"
                                       (display-buffer-reuse-window display-buffer-in-direction)
                                       (side . right)
                                       (dedicated . t))))

(use-package multi-vterm
  :bind (("s-u" . multi-vterm-next)
         ("s-i" . multi-vterm-prev)
         ("s-y" . multi-vterm)))

(provide 'core-packages)

;;; core-packages.el ends here
