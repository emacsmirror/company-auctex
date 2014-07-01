;;; company-auctex.el --- Company-mode auto-completion for AUCTeX

;; Copyright (C) 2012 Christopher Monsanto, 2014 Alexey Romanov

;; Author: Christopher Monsanto <chris@monsan.to>, Alexey Romanov <alexey.v.romanov@gmail.com>
;; Version: 0.1
;; Package-Requires: ((yasnippet "0.8.0") (company-mode "0.8.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; You can install this by (require 'company-auctex).
;; Feel free to contribute better documentation!

;;; Code:

(require 'tex)
(require 'latex)

(eval-when-compile
  (require 'company)
  (require 'yasnippet))

(defvar company-auctex-arg-lookup-table
  '((TeX-arg-define-macro . ("\\MacroName"))
    (TeX-arg-counter . ("Counter"))
    (TeX-arg-define-counter . ("\\CounterName"))
    (TeX-arg-file . ("Filename"))
    (TeX-arg-bibliography . ("Filename"))
    (TeX-arg-bibstyle . ("Style"))
    (TeX-arg-environment . ("Environment"))
    (TeX-arg-define-environment . ("EnvironmentName"))
    (TeX-arg-size . ("(w, h)"))
    (TeX-arg-ref . ("Name"))
    (TeX-arg-index . ("Index"))
    (TeX-arg-define-label . ("Label"))
    (LaTeX-arg-usepackage . (["opt1,..."] "Package"))
    (LaTeX-env-label . nil)
    (LaTeX-amsmath-env-aligned . (["htbp!"]))
    (LaTeX-amsmath-env-alignat . (["# Columns"]))
    (LaTeX-env-array . (["bct"] "lcrpmb|"))
    (LaTeX-env-item . nil)
    (LaTeX-env-document . nil)
    (LaTeX-env-figure . (["htbp!"]))
    (LaTeX-env-contents . ("Filename"))
    (LaTeX-env-minipage . (["htbp!"] "Width"))
    (LaTeX-env-list . ("Label" "\\itemsep,\\labelsep,..."))
    (LaTeX-env-picture . ("(w, h)" "(x, y)"))
    (LaTeX-env-tabular* . ("Width" ["htbp!"] "lcrpmb|><"))
    (LaTeX-env-bib . ("WidestLabel"))
    (TeX-arg-conditional . ([""]))
    (2 . ("" ""))
    (3 . ("" "" ""))
    (4 . ("" "" "" ""))
    (5 . ("" "" "" "" ""))
    (6 . ("" "" "" "" "" ""))
    (7 . ("" "" "" "" "" "" ""))
    (8 . ("" "" "" "" "" "" "" ""))
    (9 . ("" "" "" "" "" "" "" "" "")))
  "Anything not in this table defaults to '(\"\")")

(defun company-auctex-expand-arg-info (arg-info)
  (loop for item in arg-info
        append (cond
                ((or (stringp item) (and (vectorp item) (stringp (elt item 0))))
                 (list item))
                ((vectorp item)
                 (loop for item-2 in (or (assoc-default (or (car-safe (elt item 0)) (elt item 0))
                                                        company-auctex-arg-lookup-table 'equal) '(""))
                       collect [item-2]))
                (t
                 (or (assoc-default (or (car-safe item) item) company-auctex-arg-lookup-table) '(""))))))

(defun company-auctex-snippet-arg (n arg)
  (let* ((opt (vectorp arg))
         (item (if opt (elt arg 0) arg))
         (m (if (vectorp arg) (1+ n) n))
         (var (format "${%s}" item)))
    (list (1+ m)
          (if opt
              (concat (format "${[") var "]}")
            (concat "{" var "}")))))

(defun company-auctex-prefix (regexp)
  "Returns the prefix for matching given REGEXP."
  (and (derived-mode-p 'latex-mode) (company-grab regexp 1)))


;; Macros
;;

(defun company-auctex-expand-args (str env)
  (yas-expand-snippet (company-auctex-macro-snippet (assoc-default str env))))

(defun company-auctex-macro-snippet (arg-info)
  (let ((count 1))
    (apply 'concat (loop for item in (company-auctex-expand-arg-info arg-info)
                         collect (destructuring-bind (n val)
                                     (company-auctex-snippet-arg count item)
                                   (setq count n)
                                   val)))))

(defun company-auctex-macro-candidates (prefix)
   (let ((comlist (if TeX-symbol-list
                      (mapcar (lambda (item)
                                (or (car-safe (car item)) (car item)))
                            TeX-symbol-list))))
    (all-completions prefix comlist)))

(defun company-auctex-macro-post-completion (candidate)
  (yas-expand-snippet (company-auctex-macro-snippet (assoc-default candidate TeX-symbol-list))))

(defun company-auctex-macros (command &optional arg &rest ignored)
  "company-auctex-macros backend"
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-auctex-macros))
    (prefix (company-auctex-prefix "\\\\\\([a-zA-Z]*\\)\\="))
    (candidates (company-auctex-macro-candidates arg))
    (post-completion (company-auctex-macro-post-completion arg))
    ;;    (meta (company-auctex-meta arg))
    ;;    (annotation (company-auctex-annotation arg))
    ))


;; Symbols
;;

(defun company-auctex-math-all ()
  (append LaTeX-math-list LaTeX-math-default))

(defun company-auctex-symbol-candidates (prefix)
  (all-completions prefix (mapcar 'cadr (company-auctex-math-all))))

(defun company-auctex-symbol-post-completion (candidate)
  (re-search-backward candidate)
  (delete-region (1- (match-beginning 0)) (match-end 0))
  (if (texmathp)
      (progn
        (insert "\\" candidate)
        (yas-expand-snippet (company-auctex-macro-snippet (assoc-default candidate TeX-symbol-list))))
    (insert "$\\" candidate "$")
    (backward-char)
    (yas-expand-snippet (company-auctex-macro-snippet (assoc-default candidate TeX-symbol-list)))))

(defun company-auctex-symbol-annotation (candidate)
  (let ((char (nth 2 (assoc candidate (mapcar 'cdr (company-auctex-math-all))))))
        (if char (concat " " (char-to-string (decode-char 'ucs char))) nil)))

(defun company-auctex-symbols (command &optional arg &rest ignored)
  "company-auctex-symbols backend"
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-auctex-symbols))
    (prefix (company-auctex-prefix "\\\\\\([a-zA-Z]*\\)\\="))
    (candidates (company-auctex-symbol-candidates arg))
    (post-completion (company-auctex-symbol-post-completion arg))
    ;;    (meta (company-auctex-meta arg))
    (annotation (company-auctex-symbol-annotation arg))
    ))


;; Environments
;;

(defvar company-auctex-environment-prefix "beg")

(defun company-auctex-environment-candidates (prefix)
  (let ((envlist (mapcar (lambda (item) (concat company-auctex-environment-prefix (car item)))
                         LaTeX-environment-list)))
    (all-completions prefix envlist)))

(defun company-auctex-environment-post-completion (candidate)
  (re-search-backward candidate)
  (delete-region (1- (match-beginning 0)) (match-end 0))
  (let ((candidate (substring candidate (length company-auctex-environment-prefix))))
    (yas-expand-snippet (format "\\begin{%s}%s\n$0\n\\end{%s}"
                                candidate
                                (company-auctex-macro-snippet (assoc-default candidate LaTeX-environment-list))
                                candidate))))

(defun company-auctex-environments (command &optional arg &rest ignored)
  "company-auctex-environments backend"
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-auctex-environments))
    (prefix (company-auctex-prefix "\\\\\\([a-zA-Z]*\\)\\="))
    (candidates (company-auctex-environment-candidates arg))
    (post-completion (company-auctex-environment-post-completion arg))
    ;;    (meta (company-auctex-meta arg))
    ;;    (annotation (company-auctex-annotation arg))
    ))


;; Refs
;;

(defun company-auctex-label-candidates (prefix)
  (all-completions prefix (mapcar 'car LaTeX-label-list)))

(defun company-auctex-labels (command &optional arg &rest ignored)
  "company-auctex-labels backend"
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-auctex-labels))
    (prefix (company-auctex-prefix "\\\\\\([a-zA-Z]*\\)\\="))
    (candidates (company-auctex-label-candidates arg))
    ;;    (meta (company-auctex-meta arg))
    ;;    (annotation (company-auctex-annotation arg))
    ))


;; Bibs
;;

(defun company-auctex-bib-candidates (prefix)
  (all-completions prefix (mapcar 'car LaTeX-bibitem-list)))

(defun company-auctex-bibs (command &optional arg &rest ignored)
  "company-auctex-bibs backend"
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-auctex-bibs))
    (prefix (company-auctex-prefix "\\\\cite\\(?:\\[[^]]*\\]\\){\\([^},]*\\)\\="))
    (candidates (company-auctex-bib-candidates arg))
    ;;    (meta (company-auctex-meta arg))
    ;;    (annotation (company-auctex-annotation arg))
    ))


;; All together
;;

(defvar company-auctex
  '(company-auctex-macros
    company-auctex-symbols
    company-auctex-environments
    company-auctex-labels
    company-auctex-bibs)
  "Grouped company-auctex backends.")

(provide 'company-auctex)

;;; company-auctex.el ends here
