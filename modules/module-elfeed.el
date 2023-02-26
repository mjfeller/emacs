;;; module-elfeed.el

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

;; elfeed provides an RSS feed viewer. I added some helper functions for
;; tracking youtube channels and playlists as well as a mechanism to
;; watch videos with mpv. Hopefully this will allow me to follow
;; channels I care about without getting sucked into the YouTube
;; algorithm.

;;; Code:

(defvar mjf/elfeed-ytdl-format
  "bestvideo[height<=?720][fps<=?30]+bestaudio/best"
  "Video and audio format for downloaded youtube content")

(defvar mjf/ytdl-audo-format
  "--extract-audio --audio-quality 0 --audio-format flac"
  "Youtube Download audio format flags")

(defun mjf/play-video-at-point ()
  "Play the link at point with mpv"
  (interactive)
  (shell-command (format "mpv --ytdl-format='%s' '%s' &"
                         mjf/elfeed-ytdl-format
                         (thing-at-point 'url))))

(defun mjf/ytdl-audio (url)
  "Download a youtube video as audio"
  (let ((default-directory "~/Downloads"))
    (async-shell-command (format "youtube-dl %s '%s'"
                                 mjf/ytdl-audo-format
                                 url))))

(defun mjf/yt-channel-feed (channel)
  "Create an elfeed feed for a given YouTube channel"
  (let ((feed (format "https://www.youtube.com/feeds/videos.xml?channel_id=%s"
                      channel)))
    `(,feed youtube)))

(defun mjf/yt-playlist-feed (playlist)
  "Create an elfeed feed for a given YouTube playlist"
  (let ((feed (format "https://www.youtube.com/feeds/videos.xml?playlist_id=%s"
                      playlist)))
    `(,feed youtube)))

(defun mjf/elfeed-search-download-audio ()
  "Visit the current entry in your browser using `browse-url'.
If there is a prefix argument, visit the current entry in the
browser defined by `browse-url-generic-program'."
  (interactive)
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             do (elfeed-untag entry 'unread)
             when (elfeed-entry-link entry)
             do (mjf/ytdl-audio it))
    (mapc #'elfeed-search-update-entry entries)
    (unless (or elfeed-search-remain-on-entry (use-region-p))
      (forward-line))))

(use-package elfeed
  :bind
  (:map elfeed-show-mode-map
        ("C-c o" . mjf/play-video-at-point))
  (:map elfeed-search-mode-map
        ("a" . mjf/elfeed-search-download-audio))

  :config
  (setq elfeed-db-directory "~/.local/share/elfeed")

  (add-hook 'elfeed-search-mode-hook 'disable-line-numbers)
  (add-hook 'elfeed-search-mode-hook 'evil-emacs-state)
  (add-hook 'elfeed-show-mode-hook 'disable-line-numbers)
  (add-hook 'elfeed-show-mode-hook 'evil-emacs-state))

(provide 'module-elfeed)

;;; module-elfeed.el ends here
