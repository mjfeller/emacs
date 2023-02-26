;;; module-ledger.el

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

(defun mjf/ledger-report (name command)
  `(,name ,(concat "%(binary) -f %(ledger-file) " command)))

(defun mjf/ledger-reports (lst)
  (mapcar (lambda (x)
            (mjf/ledger-report (car x) (cadr x))) lst))

(use-package ledger-mode
  :mode "\\.ledger\\'"
  :config
  (add-hook 'ledger-report-mode-hook 'disable-line-numbers)
  (add-hook 'ledger-report-mode-hook 'evil-emacs-state)

  (setq ledger-post-amount-alignment-column 62)
  (setq ledger-report-auto-refresh nil)
  (setq ledger-reports
        (mjf/ledger-reports
         '(("bal"              "bal -V")
           ("reg"              "reg")
           ("stats"            "stats")
           ("payee"            "reg @%(payee)")
           ("account"          "reg %(account)")
           ("investments"      "bal ^Assets:Investments")
           ("net worth"        "bal ^Assets ^Liabilities -V")
           ("forecast"         "reg ^Assets ^Liabilities --forecast 'd<[2030]'")
           ("creditcard"       "reg ^Liabilities:Credit --monthly")
           ("budget"           "reg ^Expenses -p 'last month' --monthly --budget")
           ("budget year"      "reg ^Expenses -p 'this year' --monthly --budget")
           ("expenses"         "bal ^Expenses and not Tax -p 'this month'")
           ("expenses monthly" "reg ^Expenses and not Tax -S T -p 'this year' --monthly --collapse")
           ("expenses month"   "bal ^Expenses and not Tax --flat -S T -p 'last month'")
           ("expenses year"    "bal ^Expenses and not Tax --flat -S T -p 'this year'")
           ("average"          "reg -p 'this year' --monthly --average not Tax and ^Expenses")
           ("paystubs"         "print Income:Salary")))))

(use-package flycheck-ledger
  :disabled
  :after (flycheck-mode ledger-mode))

(provide 'module-ledger)

;;; module-ledger.el ends here
