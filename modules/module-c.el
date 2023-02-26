(setq path-to-ctags "/usr/local/Cellar/ctags/5.8_1/bin/ctags")

(use-package ggtags
  :bind (:map ggtags-mode-map
          ("C-c g s" . ggtags-find-other-symbol)
          ("C-c g h" . ggtags-view-tag-history)
          ("C-c g r" . ggtags-find-reference)
          ("C-c g f" . ggtags-find-file)
          ("C-c g c" . ggtags-create-tags)
          ("C-c g u" . ggtags-update-tags)
          ("C-c C-j" . ggtags-find-tag-dwim)
          ("M-,"     . pop-tag-mark))
  :config
  (progn (add-hook 'c-mode-common-hook
           (lambda ()
             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
               (ggtags-mode 1))))

      (setq font-lock-maximum-decoration
        '((c-mode . 3)))

      (setq c-default-style "gnu"
        c-basic-offset 4)))

(use-package company-c-headers
  :after company)

;; (bind-key "C-c C-c" #'compile c-mode-map)

(provide 'module-c)
