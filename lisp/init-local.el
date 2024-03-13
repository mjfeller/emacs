(when (eq system-type 'darwin)
  (dolist (var '("GNUPGHOME" "NOTMUCH_CONFIG" "XDG_DOCUMENTS_DIR"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(mjf-initialize-personal-email)
(setq user-mail-address "mark@mfeller.io")
(setq mml-secure-smime-sign-with-sender "mark@mfeller.io")
(setq message-signature-file "~/.local/share/emacs/signature")

(when (eq system-type 'darwin)
  (setq user-mail-address "mfeller@squareup.com")
  (setq mml-secure-smime-sign-with-sender "mfeller@squareup.com"))

(provide 'init-local)
