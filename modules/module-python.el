;;; module-python.el

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

(defun pyenv-init()
  (setq global-pyenv (replace-regexp-in-string "\n" "" (shell-command-to-string "pyenv global")))
  (message (concat "Setting pyenv version to " global-pyenv))
  (pyenv-mode-set global-pyenv)
  (defvar pyenv-current-version nil global-pyenv))

(defun mjf/pyenv-activate-current-project ()
  "Automatically activates pyenv version if .python-version file exists."
  (interactive)
  (f-traverse-upwards
   (lambda (path)
     (message path)
     (let ((pyenv-version-path (f-expand ".python-version" path)))
       (if (f-exists? pyenv-version-path)
           (progn
             (setq pyenv-current-version (s-trim (f-read-text pyenv-version-path 'utf-8)))
             (pyenv-mode-set pyenv-current-version)
             (pyvenv-workon pyenv-current-version)
             (message (concat "Setting virtualenv to " pyenv-current-version))))))))

(use-package python
  :config
  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "-i --simple-prompt")

  ;; Enable hideshow minor mode in python for folding and unfolding
  (add-hook 'python-mode-hook 'hs-minor-mode)
  (add-hook 'python-mode-hook 'subword-mode)

  (add-hook
   'python-mode-hook
   (lambda ()
     (setq pretty-symbols-alist '())
     (mapc (lambda (pair) (push pair prettify-symbols-alist))
           '(;; Syntax
             ("lambda" . ?λ)
             (">="     . ?≥)
             ("<="     . ?≤)
             ("->"     . ?➜))))))

(use-package elpy
  :bind
  (:map elpy-mode-map
        ("C-c C-j" . elpy-goto-definition)
        ("M-,"     . pop-tag-mark))

  :config
  (setq elpy-rpc-backend "jedi")
  (setq elpy-rpc-python-command "python3")
  (setq elpy-shell-echo-output nil)

  (elpy-enable))

(use-package pip-requirements
  :config
  (add-hook 'pip-requirements-mode-hook #'pip-requirements-auto-complete-setup))

(use-package py-autopep8)

(provide 'module-python)

;;; module-python.el ends here
