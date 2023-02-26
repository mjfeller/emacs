(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)
         ("C-S-SPC"     . set-rectangular-region-anchor)
         ("C-S-c C->"   . mc/mark-sgml-tag-pair))
  :init (setq mc/list-file "~/.cache/mc-lists.el"))

(provide 'module-multiple-cursors)
