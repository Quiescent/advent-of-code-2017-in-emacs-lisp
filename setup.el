;;; setup --- A buffer which should be evalled before I get hacking -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(add-to-list 'load-path default-directory)
(setq-default flycheck-emacs-lisp-load-path load-path)

(provide 'setup)
;;; setup ends here
