;;; module-org.el --- Configuration -*- lexical-binding: t; -*-

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

(defvar mjf-org-custom-daily-agenda
  `((tags-todo "*"
               ((org-agenda-overriding-header "Important tasks without a date\n")
                (org-agenda-skip-function #'mjf-org-agenda-include-priority-no-timestamp)
                (org-agenda-block-separator nil)))
    (agenda "" ((org-agenda-overriding-header "\nToday's agenda\n")
                (org-agenda-span 1)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-scheduled-past-days 0)
                (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                (org-agenda-format-date "%A %-e %B %Y")))
    (agenda "" ((org-agenda-overriding-header "\nNext three days\n")
                (org-agenda-start-on-weekday nil)
                (org-agenda-start-day nil)
                (org-agenda-start-day "+1d")
                (org-agenda-span 3)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))))
    (agenda "" ((org-agenda-overriding-header "\nUpcoming deadlines (+14d)\n")
                (org-agenda-time-grid nil)
                (org-agenda-start-on-weekday nil)
                ;; We don't want to replicate the previous section's
                ;; three days, so we start counting from the day after.
                (org-agenda-start-day "+4d")
                (org-agenda-span 14)
                (org-agenda-show-all-dates nil)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-agenda-entry-types '(:deadline))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done)))))
  "Custom agenda for use in `org-agenda-custom-commands'.")

(defun mjf-org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(defun mjf-org-agenda ()
  (interactive)
  (org-agenda nil "A"))

(defun mjf-org-agenda-include-priority-no-timestamp ()
  "Return nil if heading has a priority but no timestamp.
Otherwise, return the buffer position from where the search should
continue, per `org-agenda-skip-function'."
  (let ((point (point)))
    (if (and (eq (nth 3 (org-heading-components)) ?A)
             (not (org-get-deadline-time point))
             (not (org-get-scheduled-time point)))
        nil
      (line-beginning-position 2))))

(use-package org
  :bind (("C-c l" . org-store-link)
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

  ;; (add-hook 'org-after-todo-statistics-hook 'mjf-org-summary-todo)
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-use-fast-todo-selection 'expert)
  (setq org-default-priority ?A)
  (setq org-priority-faces nil)

  (setq mjf-org-work-file (format "%s/Documents/work.org" (getenv "HOME")))
  (setq mjf-org-life-file (format "%s/Documents/life.org" (getenv "HOME")))
  (setq org-agenda-files `(,mjf-org-work-file ,mjf-org-life-file))

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
          ("w" "Work" entry (file+headline mjf-org-work-file "Unread")
           "* TODO %?\n  %i\n")

          ;; random thoughts I'd like to capture
          ("t" "Thought" entry (file+headline mjf-org-work-file "Thoughts")
           "* %?\n  %i\n")

          ;; unfiled work items in my life
          ("l" "Life" entry (file+headline mjf-org-life-file "Inbox")
           "* TODO %?\n  %i\n")))

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

(use-package org-agenda
  :bind
  (:map global-map
        ("C-c A" . org-agenda)
        ("C-c a" . mjf-org-agenda))

  :config
  (setq org-agenda-custom-commands
        `(("A" "Daily agenda and top priority tasks"
           ,mjf-org-custom-daily-agenda
           ((org-agenda-fontify-priorities nil)
            (org-agenda-prefix-format "	 %t %s")
            (org-agenda-dim-blocked-tasks nil)))))


  (setq org-agenda-sorting-strategy
        '(((agenda habit-down time-up priority-down category-keep)
           (todo priority-down category-keep)
           (tags priority-down category-keep)
           (search category-keep))))
  (setq org-agenda-breadcrumbs-separator "->")
  (setq org-agenda-todo-keyword-format "%-1s")
  (setq org-agenda-fontify-priorities 'cookies)
  (setq org-agenda-category-icon-alist nil)
  (setq org-agenda-remove-times-when-in-prefix nil)
  (setq org-agenda-remove-timeranges-from-blocks nil)
  (setq org-agenda-compact-blocks nil)
  (setq org-agenda-block-separator ?—)

  (setq org-agenda-dim-blocked-tasks t)
  )

(provide 'module-org)

;;; module-org.el ends here
