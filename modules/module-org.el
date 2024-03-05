;;; module-org.el

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

(defun mjf/org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-src-mode-map
         ("C-x C-s" . org-edit-src-exit))

  :custom
  (org-persist-directory "~/.cache/emacs/org-persist")

  :config
  (require 'org-indent)
  (require 'org-src)
  (require 'ob-shell)

  (setq org-startup-indented t)
  (setq org-src-tab-acts-natively t)
  (setq org-src-fontify-natively t)
  (setq org-hide-emphasis-markers t)

  (add-hook 'org-after-todo-statistics-hook 'mjf/org-summary-todo)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

  (setq mjf/org-work-file (format "%s/work.org" (getenv "XDG_DOCUMENTS_DIR")))
  (setq mjf/org-life-file (format "%s/life.org" (getenv "XDG_DOCUMENTS_DIR")))

  ;; personal capture templates
  (setq org-capture-templates
        '(("n" "New note (with Denote)" plain
           (file denote-last-path)
           #'denote-org-capture
           :no-save t
           :immediate-finish nil
           :kill-buffer t
           :jump-to-captured t)

          ;; unfiled work items
          ("w" "Work" entry (file+headline mjf/org-work-file "Unread")
           "* TODO %?\n  %i\n")

          ;; random thoughts I'd like to capture
          ("t" "Thought" entry (file+headline mjf/org-work-file "Thoughts")
           "* %?\n  %i\n")

          ;; unfiled work items in my life
          ("l" "Life" entry (file+headline mjf/org-life-file "Inbox")
           "* TODO %?\n  %i\n")

          ("j" "Journal" entry (file+datetree mjf/org-life-file)
           "* TODO %?\n")))

  ;; when refiling an org header don't search more than 3 levels deep
  (setq org-refile-targets
        '((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 3)))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "BLOCKED(b)" "|" "DONE(d)" "CANCELLED(c)")
          (sequence "RECURRING(r)" "|" "DONE(d)")))

  ;; don't make the title big, I don't like that. Also remove those ugly
  ;; checkboxes
  (set-face-attribute 'org-document-title nil :height 1)
  (set-face-attribute 'org-checkbox nil :box nil))

(use-package org-bullets
  :config
  (setq org-bullets-bullet-list '("●" "○" "◉" "•"))

  ;; bullets on lists
  (font-lock-add-keywords
   'org-mode
   '(("^ +\\([-*]\\) "
      (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

(provide 'module-org)

;;; module-org.el ends here
