;;; module-notmuch.el

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

;; Notmuch mail provides email indexing and searching based on tags. See
;; https://notmuchmail.org/

;;; Code:

(require 'smtpmail)

(defun mjf/initialize-gmail ()
  "Configure smtpmail to use gmail"
  (setq message-send-mail-function   'smtpmail-send-it
        send-mail-function           'smtpmail-send-it
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server         "smtp.gmail.com"
        smtpmail-local-domain        "gmail.com"
        smtpmail-stream-type         'ssl
        smtpmail-smtp-service        465))

(defun mjf/initialize-personal-email ()
  "Configure smtpmail to use personal mail server"
  (setq message-send-mail-function   'smtpmail-send-it  ; message-mode
        send-mail-function           'smtpmail-send-it  ; mail-mode
        smtpmail-default-smtp-server "mail.mfeller.io"
        smtpmail-smtp-server         "mail.mfeller.io"
        smtpmail-local-domain        "mfeller.io"
        smtpmail-stream-type         'ssl
        smtpmail-smtp-service        465))

(defun mjf/tag-deleted ()
  "Tag mail at point as deleted. This operation will not delete
the mail, but tag it for later deletion."
  (interactive)
  (notmuch-search-tag '("+deleted" "-unread"))
  (next-line))

(defun mjf/tree-tag-deleted ()
  "Tag mail at point as deleted. This operation will not delete
the mail, but tag it for later deletion."
  (interactive)
  (notmuch-tree-tag '("+deleted" "-unread"))
  (next-line))

(defun mjf/delete-tagged-mail ()
  "Delete mail that is tagged with the deleted tag"
  (interactive)
  (shell-command "notmuch_delete")
  (notmuch-refresh-all-buffers))

(defun mjf/getmail ()
  "Fetch mail using getmail"
  (interactive)
  (message "Fetching mail...")
  (shell-command "getmail --getmaildir=$HOME/.config/getmail")
  (notmuch-poll-and-refresh-this-buffer))

(defun mjf/offlineimap ()
  "Fetch mail using getmail"
  (interactive)
  (message "Fetching mail...")
  (shell-command "offlineimap")
  (notmuch-poll-and-refresh-this-buffer))

(use-package notmuch
  :bind
  (:map notmuch-search-mode-map
        ("d" . mjf/tag-deleted)
        ("D" . mjf/delete-tagged-mail)
        ("/" . notmuch-search))
  (:map notmuch-tree-mode-map
        ("d" . mjf/tree-tag-deleted)
        ("D" . mjf/delete-tagged-mail))
  (:map notmuch-hello-mode-map
        ("r" . mjf/offlineimap))

  :config
  (add-hook 'notmuch-hello-mode-hook 'disable-line-numbers)
  (add-hook 'notmuch-message-mode-hook 'disable-line-numbers)
  (add-hook 'notmuch-search-mode-hook 'disable-line-numbers)
  (add-hook 'notmuch-show-mode-hook 'disable-line-numbers)
  (add-hook 'notmuch-tree-mode-hook 'disable-line-numbers)

  (setq notmuch-saved-searches
        '((:name "inbox"        :query "tag:inbox"   :key "i")
          (:name "unread"       :query "tag:unread"  :key "u")
          (:name "all"          :query "*"           :key "a")
          (:name "deleted"      :query "tag:deleted")
          (:name "sent"         :query "tag:sent"    :key "t")
          (:name "drafts"       :query "tag:draft"   :key "d")
          (:name "work"         :query "mark@getsunday.com tag:unread" :key "w" :search-type tree)
          (:name "OpenBSD tech" :query "tech@openbsd.org tag:unread" :search-type tree)
          (:name "OpenBSD misc" :query "misc@openbsd.org tag:unread" :search-type tree)
          )))

(provide 'module-notmuch)

;;; module-notmuch.el ends here
