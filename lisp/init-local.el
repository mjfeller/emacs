(when (eq system-type 'darwin)
  (exec-path-from-shell-initialize))

(mjf/initialize-personal-email)
(setq user-mail-address "mark@mfeller.io")
(setq mml-secure-smime-sign-with-sender "mark@mfeller.io")
(setq message-signature-file "~/.local/share/emacs/signature")

(when (eq system-type 'darwin)
  (setq user-mail-address "mfeller@squareup.com")
  (setq mml-secure-smime-sign-with-sender "mfeller@squareup.com"))

(use-package gptel
  :bind (("C-c RET" . gptel-send)
         ("C-c C-<return>" . gptel-menu))
  :config
  (setq gptel-default-mode 'org-mode))

(provide 'init-local)
