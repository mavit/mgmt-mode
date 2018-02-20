;;; mgmt-mode.el --- mgmt configuration management language

;; Copyright (C) 2017 Peter Oliver

;; Author: Peter Oliver <mgmt-mode@mavit.org.uk>
;; Keywords: languages
;; URL: https://github.com/mavit/mgmt-mode

;;; Commentary:

;; Major mode for mgmt, a next generation automation tool.
;; https://github.com/purpleidea/mgmt

;;; Code:

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

  (setq font-lock-defaults '(mgmt-mode-highlights)))


(provide 'mgmt-mode)

;;; mgmt-mode.el ends here
