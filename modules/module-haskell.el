;;; module-haskell.el

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

(use-package haskell-mode
  :bind (:map haskell-mode-map
              ("C-c C-c" . haskell-compile)
              :map haskell-cabal-mode-map
              ("C-c C-c" . haskell-compile))
  :config
  (progn (setq haskell-stylish-on-save t)
         (setq haskell-doc-prettify-types t)

         (defun haskell-mode-defaults ()
           (subword-mode +1)
           (eldoc-mode +1)
           (haskell-indentation-mode +1)
           (interactive-haskell-mode +1))

         (setq haskell-mode-hook 'haskell-mode-defaults)

         (add-hook 'haskell-mode-hook (lambda ()
                                        (run-hooks 'haskell-mode-hook)))))

(use-package flycheck-haskell
  :after (haskell-mode flycheck-mode))

(provide 'module-haskell)

;;; module-haskell.el ends here
