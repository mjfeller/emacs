(when (eq system-type 'darwin)
  (resize-small)
  (mjf/center-window)
  (exec-path-from-shell-initialize))

(setq elfeed-feeds
      `("https://thume.ca/atom.xml"
        "https://lukesmith.xyz/rss.xml"
        "https://nullprogram.com/feed/"
        ("https://notrelated.libsyn.com/rss" podcast)
        ("http://mfeller.io/rss.xml" personal)
        ("https://planet.emacslife.com/atom.xml" emacs community)
        ,(mjf/yt-playlist-feed "PLkG-zhy1pVumXUC8e1vdfTciqZgNcpvdq")
        ,(mjf/yt-channel-feed "UCBa659QWEk1AI4Tg--mrJ2A") ; Tom Scott
        ,(mjf/yt-channel-feed "UC0uTPqBCFIpZxlz_Lv1tk_g")
        ,(mjf/yt-channel-feed "UCdJdEguB1F1CiYe7OEi3SBg") ; Jon Tron
        ,(mjf/yt-channel-feed "UCEOXxzW2vU0P-0THehuIIeg") ; Captian D
        ,(mjf/yt-channel-feed "UCrTNhL_yO3tPTdQ5XgmmWjA") ; Red Letter Media
        ,(mjf/yt-channel-feed "UCgXiTWrFg05fTPfw1YLb5Ug") ; Triforce!
        ))

(mjf/initialize-personal-email)
(setq user-mail-address "mark@mfeller.io")
(setq mml-secure-smime-sign-with-sender "mark@mfeller.io")
(setq message-signature-file "~/.local/share/emacs/signature")

(when (eq system-type 'darwin)
  (setq user-mail-address "mfeller@squareup.com")
  (setq mml-secure-smime-sign-with-sender "mfeller@squareup.com"))

(provide 'init-local)
