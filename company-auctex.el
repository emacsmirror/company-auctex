;;; company-auctex.el --- Company-mode auto-completion for AUCTeX

;; Copyright (C) 2012 Christopher Monsanto, 2014 Alexey Romanov

;; Author: Christopher Monsanto <chris@monsan.to>, Alexey Romanov <alexey.v.romanov@gmail.com>
;; Version: 0.1
;; Package-Requires: ((yasnippet "0.6.1") (company-mode "0.8.0"))

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

;; Macros
;;

(defun company-auctex-expand-args (str env)
  (yas/expand-snippet (company-auctex-macro-snippet (assoc-default str env))))

(defun company-auctex-macro-snippet (arg-info)
  (let ((count 1))
    (apply 'concat (loop for item in (company-auctex-expand-arg-info arg-info)
                         collect (destructuring-bind (n val)
                                     (company-auctex-snippet-arg count item)
                                   (setq count n)
                                   val)))))

(defun company-auctex-macro-candidates ()
   (let ((comlist (if TeX-symbol-list
                      (mapcar (lambda (item)
                                (or (car-safe (car item)) (car item)))
                            TeX-symbol-list))))
    (all-completions ac-prefix comlist)))

(defun company-auctex-macro-action ()
  (yas/expand-snippet (company-auctex-macro-snippet (assoc-default candidate TeX-symbol-list))))

(ac-define-source auctex-macros
  '((init . TeX-symbol-list)
    (candidates . company-auctex-macro-candidates)
    (action . company-auctex-macro-action)
    (requires . 0)
    (symbol . "m")
    (prefix . "\\\\\\([a-zA-Z]*\\)\\=")))

;; Symbols
;;

(defun company-auctex-symbol-candidates ()
  (all-completions ac-prefix (mapcar 'cadr LaTeX-math-default)))

(defun company-auctex-symbol-action ()
  (re-search-backward candidate)
  (delete-region (1- (match-beginning 0)) (match-end 0))
  (if (texmathp)
      (progn
        (insert "\\" candidate)
        (yas/expand-snippet (company-auctex-macro-snippet (assoc-default candidate TeX-symbol-list))))
    (insert "$\\" candidate "$")
    (backward-char)
    (yas/expand-snippet (company-auctex-macro-snippet (assoc-default candidate TeX-symbol-list)))))

(defun company-auctex-symbol-document (c)
  (let* ((cl (assoc c (mapcar 'cdr LaTeX-math-default)))
         (decode (if (nth 2 cl) (char-to-string (decode-char 'ucs (nth 2 cl))) ""))
         (st (nth 1 cl))
         (hs (if (listp st) (mapconcat 'identity st " ") st)))
    (and decode (concat hs " == " decode))))

(ac-define-source auctex-symbols
  '((init . LaTeX-math-mode)
    (candidates . company-auctex-symbol-candidates)
    (document . company-auctex-symbol-document)
    (action . company-auctex-symbol-action)
    (requires . 0)
    (symbol . "s")
    (prefix . "\\\\\\([a-zA-Z]*\\)\\=")))


;; Environments
;;


(defvar company-auctex-environment-prefix "beg")

(defun company-auctex-environment-candidates ()
  (let ((envlist (mapcar (lambda (item) (concat company-auctex-environment-prefix (car item)))
                         LaTeX-environment-list)))
    (all-completions ac-prefix envlist)))

(defun company-auctex-environment-action ()
  (re-search-backward candidate)
  (delete-region (1- (match-beginning 0)) (match-end 0))
  (let ((candidate (substring candidate (length company-auctex-environment-prefix))))
    (yas/expand-snippet (format "\\begin{%s}%s\n$0\n\\end{%s}"
                                candidate
                                (company-auctex-macro-snippet (assoc-default candidate LaTeX-environment-list))
                                candidate))))

(ac-define-source auctex-environments
  '((init . LaTeX-environment-list)
    (candidates . company-auctex-environment-candidates)
    (action .  company-auctex-environment-action)
    (requires . 0)
    (symbol . "e")
    (prefix . "\\\\\\([a-zA-Z]*\\)\\=")))


;; Refs
;;


(defun company-auctex-label-candidates ()
  (all-completions ac-prefix (mapcar 'car LaTeX-label-list)))

(ac-define-source auctex-labels
  '((init . LaTeX-label-list)
    (candidates . company-auctex-label-candidates)
    (requires . 0)
    (symbol . "r")
    (prefix . "\\\\ref{\\([^}]*\\)\\=")))


;; Bibs
;;

(defun company-auctex-bib-candidates ()
  (all-completions ac-prefix (mapcar 'car LaTeX-bibitem-list)))

(ac-define-source auctex-bibs
  `((init . LaTeX-bibitem-list)
    (candidates . company-auctex-bib-candidates)
    (requires . 0)
    (symbol . "b")
    (prefix . ,(concat "\\\\cite"
                       "\\(?:"
                         "\\[[^]]*\\]"
                       "\\)?"
                       "{\\([^},]*\\)"
                       "\\="))))

;; Setup
;;


(defun company-auctex-setup ()
  (setq ac-sources (append
                      '(ac-source-auctex-symbols
                        ac-source-auctex-macros
                        ac-source-auctex-environments
                        ac-source-auctex-labels
                        ac-source-auctex-bibs)
                      ac-sources)))

(add-hook 'LaTeX-mode-hook 'company-auctex-setup)

(provide 'company-auctex)

;;; company-auctex.el ends here
