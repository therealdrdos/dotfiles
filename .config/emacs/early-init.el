;;; early-init.el --- Runs before package.el and the first frame -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs 30 does not raise the GC threshold during init and collects
;; dozens of times on the way.  Defer it, then keep a working threshold.

;;; Code:

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 32 1024 1024)
                  gc-cons-percentage 0.1))
          100)

(provide 'early-init)
;;; early-init.el ends here
