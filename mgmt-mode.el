;;; mgmt-mode.el --- mgmt configuration management language

;; Copyright 2017 Peter Oliver.

;; This file is part of mgmt-mode.
;;
;; mgmt-mode is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; mgmt-mode is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with mgmt-mode.  If not, see <https://www.gnu.org/licenses/>.

;; Author: Peter Oliver <mgmt-mode@mavit.org.uk>
;; Keywords: languages
;; URL: https://github.com/mavit/mgmt-mode

;;; Commentary:
;;
;; Major mode for mgmt, a next generation automation tool.
;; https://github.com/purpleidea/mgmt

;;; Code:

(require 'smie)

(defconst mgmt-smie-grammar
      (smie-prec2->grammar
       (smie-bnf->prec2
        '((map ("{" pairs "}"))
          (pairs (pair) (pair "," pairs))
          (pair (key "=>" value))
          (key)
          (value)))))

(defun mgmt-smie-rules (method token)
  "Rules for indenting the mgmt language.
METHOD and TOKEN are as for `smie-rules-function'."
  (pcase (cons method token)
    ;; Statements in mgmt end at the end of a line.  This causes SMIE
    ;; not to indent every line as if it were a continuation of the
    ;; statement on the previous line:
    (`(:list-intro . ,_) t)))

(defconst mgmt-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    st))

(defconst mgmt-mode-highlights
  `((,(regexp-opt (list "if" "else" "in" "true" "false") 'words)
     . font-lock-keyword-face)
    (,(concat "^\\s-*"
              (regexp-opt (list "actions" "augeas" "autoedge" "autogroup" "aws_ec2" "edge" "exec" "file" "graph" "group" "hostname" "interfaces" "kv" "metaparams" "mgraph" "msg" "noop" "nspawn" "password" "pkg" "print" "refresh" "resources" "semaphore" "sendrecv" "svc" "timer" "uid" "user" "util" "virt") 'words))
     . font-lock-builtin-face)
    (,(regexp-opt
       (list "bool" "str" "int" "float" "struct" "variant") 'words)
     . font-lock-type-face)
    ("\\$\\w+" . font-lock-variable-name-face)))


;;;###autoload
(define-derived-mode mgmt-mode prog-mode "mgmt"
  "Major mode for editing the mgmt language."

  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")

  (setq font-lock-defaults '(mgmt-mode-highlights))

  (smie-setup mgmt-smie-grammar #'mgmt-smie-rules))


(provide 'mgmt-mode)

;;; mgmt-mode.el ends here
