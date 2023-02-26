(use-package vterm
  :config
  (add-to-list 'evil-emacs-state-modes 'vterm-mode)
  (add-to-list 'evil-emacs-state-modes 'compilation-mode))

(use-package multi-vterm
  :bind (
         ("C-x p t" . multi-vterm-project)
         ("s-T"     . multi-vterm-dedicated-open)
         ("s-u"     . multi-vterm-next)
         ("s-i"     . multi-vterm-prev)
         ("s-y"     . multi-vterm)))

(provide 'module-vterm)
