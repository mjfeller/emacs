(use-package lsp-mode
  :disabled
  :config
  (setq lsp-keymap-prefix "H-l")
  (add-hook 'go-mode-hook #'lsp))

(use-package lsp-ivy
  :disabled
  :commands lsp-ivy-workspace-symbol)

(provide 'module-lsp)
