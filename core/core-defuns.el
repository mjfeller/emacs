;;; core-defuns.el --- Personal utility functions -*- lexical-binding: t; -*-

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

(defun transpose-words (arg)
  "[Override for default transpose-words in simple.el]
              Interchange words around point, leaving point at end of
              them. With prefix arg ARG, effect is to take word before or
              around point and drag it backward past ARG other words (forward
              if ARG negative).  If ARG is zero, the words around or after
              point and around or after mark are interchanged."
  (interactive "*p")
  (if (eolp) (forward-char -1))
  (transpose-subr 'backward-word arg)
  (forward-word (+ arg 1)))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (indent-buffer))

(defun toggle-window-split ()
  "Toggle the current window split."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(defun info-mode ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (kill-buffer (current-buffer))
    (info file-name)))
(add-to-list 'auto-mode-alist '("\\.info\\'" . info-mode))

(defadvice move-beginning-of-line (around smarter-bol activate)
  ;; Move to requested line if needed.
  (let ((arg (or (ad-get-arg 0) 1)))
    (when (/= arg 1)
      (forward-line (1- arg))))
  ;; Move to indentation on first call, then to actual BOL on second.
  (let ((pos (point)))
    (back-to-indentation)
    (when (= pos (point))
      ad-do-it)))

;; If the *scratch* buffer is killed, recreate it automatically
;; FROM: Morten Welind
;;http://www.geocrawler.com/archives/3/338/1994/6/0/1877802/
(save-excursion
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer))

(defun kill-scratch-buffer ()
  ;; The next line is just in case someone calls this manually
  (set-buffer (get-buffer-create "*scratch*"))
  ;; Kill the current (*scratch*) buffer
  (remove-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  (kill-buffer (current-buffer))
  ;; Make a brand new *scratch* buffer
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  ;; Since we killed it, don't let caller do that.
  nil)

(defun save-buffer-without-dtw ()
  (interactive)
  (let ((b (current-buffer)))   ; memorize the buffer
    (with-temp-buffer ; new temp buffer to bind the global value of before-save-hook
      (let ((before-save-hook (remove 'delete-trailing-whitespace before-save-hook)))
        (with-current-buffer b  ; go back to the current buffer, before-save-hook is now buffer-local
          (let ((before-save-hook (remove 'delete-trailing-whitespace before-save-hook)))
            (save-buffer)))))))

(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

;; (define-key endless/toggle-map "n"
;;   #'narrow-or-widen-dwim)
;; This line actually replaces Emacs' entire narrowing
;; keymap, that's how much I like this command. Only
;; copy it if that's what you want.
(define-key ctl-x-map "n" #'narrow-or-widen-dwim)

(defun toggle-modeline ()
  (interactive)
  (if (not (consp mode-line-format))
      (setq mode-line-format (default-value 'mode-line-format))
    (setq mode-line-format nil)))

(defun resize-small ()
  (interactive)
  (set-frame-width (selected-frame) 85)
  (set-frame-height (selected-frame) 50))

(defun resize-wide ()
  (interactive)
  (set-frame-width (selected-frame) 180)
  (set-frame-height (selected-frame) 50))

(defun resize-big ()
  (interactive)
  (set-frame-width (selected-frame) 240)
  (set-frame-height (selected-frame) 70))

(defun disable-line-numbers ()
  (display-line-numbers-mode -1))

(defun mjf-pash-copy ()
  (interactive)
  (let ((arg (completing-read "" (mjf/pash-list))))
    (shell-command-to-string (format "pash copy %s" arg))))

(defun mjf-pash-list ()
  (split-string
   (shell-command-to-string "pash list") "\n"))

(defun mjf-focused ()
  (interactive)
  (if (bound-and-true-p olivetti-mode)
      (progn
        (olivetti-mode -1)
        (setq mode-line-format (default-value 'mode-line-format)))
    (progn
      (delete-other-windows)
      (olivetti-mode 1)
      (setq mode-line-format nil))))

(defun mjf-gh-browse ()
  "Browse the current file on the GitHub web UI with the current commit. If
a region is selected then the region will be selected in the web
UI. Useful for linking code."
  (interactive)
  (shell-command
   (concat "gh browse "
           (file-name-nondirectory (buffer-file-name))
           ;; (format " --commit %s" (magit-rev-parse "--short" "HEAD"))
           (if mark-active
               (format ":%s-%s"
                       (line-number-at-pos (region-beginning))
                       (let ((end (region-end)))
                         (if (and (> end (point-min))
                                  (eq (char-before end) ?\n))
                             (1- (line-number-at-pos end))
                           (line-number-at-pos end))))
             ""))))

(defun mjf-project-gh-pr-create (&optional include-all)
  "Open a PR on GitHub using your first commit message as the title and
body of the PR."
  (interactive "P")
  (let* ((pr (project-current t))
         (root (project-root pr))
         (default-directory root))
    (shell-command "gh pr create --fill-first --web" nil)))

(defun mjf-project-gh-browse (&optional include-all)
  "Open the current project on GitHub."
  (interactive "P")
  (let* ((pr (project-current t))
         (root (project-root pr))
         (default-directory root)
         (branch (magit-get-current-branch)))
    (shell-command (format "gh browse --branch %s" branch) nil)))

(defun mjf-project-kochiku-canary (&optional include-all)
  "Open the current commits kochiku build pipeline."
  (interactive "P")
  (let* ((pr (project-current t))
         (root (project-root pr))
         (default-directory root))
    (shell-command "sq kochiku --canary" nil)))

(defun mjf-clone-project (repo-name)
  "Clone a squareup repo into the Developement directory and add it as a
project."
  (interactive "sRepo name: ")
  (let* ((base (expand-file-name "~/Development/"))
         (dir (expand-file-name repo-name base))
         (url (format "org-49461806@github.com:squareup/%s.git" repo-name)))
    (progn
      (shell-command (format "git clone %s %s" url dir))
      (project--remember-dir dir))))

(defun mjf-kubernetes-context-switch ()
  (interactive "")
  (let* ((output (shell-command-to-string "kubectl config get-contexts -o name"))
         (lines (split-string output "\n" t))
         (context (completing-read "Context: " lines)))
    (shell-command (format "kubectl config use-context %s" context))))

(defun mjf-display-ansi-colors ()
  "Interpret ANSI color escape sequences in the current buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun ip-info (start end)
  (interactive "r")
  (let ((subnet (buffer-substring start end)))
    (async-shell-command
     (format "nix run 'nixpkgs#ipcalc' -- -i %s" subnet)
     (format "*ip info %s*" subnet))))

(defun sq-whatis (app)
  (interactive "sApp: ")
  (async-shell-command
   (format "sq whatis %s" app)
   (format "*app info %s*" app)))

(defun vterm-less (content)
  (let ((less-buffer (get-buffer-create (make-temp-name "vterm-less-"))))
    (switch-to-buffer less-buffer)
    (special-mode)
    (insert (base64-decode-string content))))

;(add-to-list 'vterm-eval-cmds (list "less" #'vterm-less))

(provide 'core-defuns)

;;; core-defuns.el ends here
