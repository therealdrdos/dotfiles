;;; terminal-fix.el --- Fix PTY/window width mismatches in Emacs terminals -*- lexical-binding: t; -*-
;; Author: therealdrdos
;; Version: 1.0
;; Package-Requires: ()
;; Keywords: terminals, pty, convenience
;; URL: n/a

;;; Commentary:
;; This module fixes common width/height mismatches between Emacs terminal
;; buffers (vterm/EAT/term-mode) and the underlying PTY.  Typical symptoms:
;; - $COLUMNS is smaller than the actual window-body width
;; - ASCII boxes/lines wrap one or two characters too early/late
;; - Tools (Ansible, Cloud tooling) render “too narrow” or overflow by 1–2 chars
;;
;; What it does when `terminal-fix-mode` is enabled:
;; 1) Enables pixel-precise resizing (avoids HiDPI/zoom rounding off-by-ones).
;; 2) Cleans terminal buffers (no line numbers, margins, or fringes; no soft-wrap).
;; 3) Forces/syncs the PTY size to the window-body (width/height) on changes.
;;
;; Save this file as terminal-fix.el and enable with:
;;   (require 'terminal-fix)
;;   (terminal-fix-mode 1)

;;; Code:

(defgroup terminal-fix nil
  "Keep PTY size in sync with terminal window body in Emacs."
  :group 'terminals
  :prefix "terminal-fix-")

(defcustom terminal-fix-cleanup-ui t
  "If non-nil, disable line numbers, margins, and fringes in terminal buffers.
Also avoid soft-wrapping."
  :type 'boolean
  :group 'terminal-fix)

(defcustom terminal-fix-sync-pty t
  "If non-nil, synchronize PTY size with the window-body size.
This sets $COLUMNS/$LINES correctly."
  :type 'boolean
  :group 'terminal-fix)

(defcustom terminal-fix-terminal-modes
  '(vterm-mode term-mode eat-mode)
  "Major modes considered true terminals whose PTY size should be synced.
Note: ansi-term is not a mode, it creates term-mode buffers."
  :type '(repeat symbol)
  :group 'terminal-fix)

(defcustom terminal-fix-shellish-modes
  '(shell-mode eshell-mode comint-mode)
  "Shell-like modes where UI cleanup is also useful.
Removes line numbers, margins, and fringes."
  :type '(repeat symbol)
  :group 'terminal-fix)

;; Saved global settings so we can restore them when disabling the mode.
(defvar terminal-fix--saved-frame-resize-pixelwise nil
  "Saved value of `frame-resize-pixelwise' before enabling terminal-fix-mode.")
(defvar terminal-fix--saved-window-resize-pixelwise nil
  "Saved value of `window-resize-pixelwise' before enabling terminal-fix-mode.")

(defvar-local terminal-fix--hook-installed nil
  "Non-nil if window-size-change hook is installed in this buffer.")

(defun terminal-fix--buffer-is-terminal-like ()
  "Return non-nil if current buffer is a terminal or shell-like buffer."
  (apply #'derived-mode-p (append terminal-fix-terminal-modes terminal-fix-shellish-modes)))

(defun terminal-fix--buffer-is-pty-terminal ()
  "Return non-nil if current buffer is a true PTY terminal.
Checks for vterm-mode, term-mode, and eat-mode."
  (apply #'derived-mode-p terminal-fix-terminal-modes))

(defun terminal-fix--visual-cleanup ()
  "Disable UI chrome that steals columns and avoid soft-wrapping.
Applies to terminal buffers."
  (when (and terminal-fix-cleanup-ui (terminal-fix--buffer-is-terminal-like))
    ;; No line numbers (they consume columns).
    (when (fboundp 'display-line-numbers-mode) (display-line-numbers-mode -1))
    ;; No extra window margins.
    (set-window-margins nil 0 0)
    ;; No fringes (left/right gutter).
    (set-window-fringes nil 0 0)
    ;; Do not soft-wrap terminal lines.
    (setq-local truncate-lines t)))

(defun terminal-fix--sync-size (&optional window)
  "Set the PTY size of the current process buffer to the window-body height/width.
If WINDOW is nil, use any window displaying the current buffer."
  (when (and terminal-fix-sync-pty
             (terminal-fix--buffer-is-pty-terminal)
             (get-buffer-process (current-buffer)))
    (let ((win (or window (get-buffer-window (current-buffer)))))
      (when win
        ;; Ensure $LINES/$COLUMNS and ioctl(TIOCGWINSZ) reflect the visible text area.
        (ignore-errors
          (set-process-window-size (get-buffer-process (current-buffer))
                                   (window-body-height win)
                                   (window-body-width win)))))))

(defun terminal-fix-apply-now ()
  "Apply UI cleanup and force a PTY size sync in the current buffer."
  (interactive)
  (terminal-fix--visual-cleanup)
  (terminal-fix--sync-size))

(defun terminal-fix--window-size-handler (frame)
  "Sync PTY size when window size change.
This function is added as a buffer-local hook to `window-size-change-functions'."
  (when terminal-fix--hook-installed
    (let ((win (get-buffer-window (current-buffer) frame)))
      (when win
        (terminal-fix--sync-size win)))))

(defun terminal-fix--enable-buffer ()
  "Enable terminal-fix behavior for the current terminal buffer."
  ;; Make changes buffer-local and keep PTY in sync on window size changes.
  (terminal-fix-apply-now)
  (add-hook 'window-size-change-functions
            #'terminal-fix--window-size-handler
            nil t)
  (setq terminal-fix--hook-installed t))

(defun terminal-fix--cleanup-all-buffers ()
  "Remove window-size-change hooks from all buffers where terminal-fix was enabled."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when terminal-fix--hook-installed
          (remove-hook 'window-size-change-functions
                       #'terminal-fix--window-size-handler
                       t)
          (setq terminal-fix--hook-installed nil))))))

(defun terminal-fix--add-hooks ()
  "Attach hooks to terminal and shell-like modes."
  ;; True terminal modes get full treatment (visual-cleanup is called by enable-buffer).
  (dolist (hk '(vterm-mode-hook term-mode-hook))
    (add-hook hk #'terminal-fix--enable-buffer))
  ;; EAT is optional; add after load to avoid hard dependency.
  (with-eval-after-load 'eat
    (add-hook 'eat-mode-hook #'terminal-fix--enable-buffer))
  ;; Shell-ish modes get visual cleanup only.
  (dolist (hk '(shell-mode-hook eshell-mode-hook comint-mode-hook))
    (add-hook hk #'terminal-fix--visual-cleanup)))

(defun terminal-fix--remove-hooks ()
  "Detach hooks from terminal and shell-like modes."
  ;; True terminal modes.
  (dolist (hk '(vterm-mode-hook term-mode-hook))
    (remove-hook hk #'terminal-fix--enable-buffer))
  ;; Shell-ish modes.
  (dolist (hk '(shell-mode-hook eshell-mode-hook comint-mode-hook))
    (remove-hook hk #'terminal-fix--visual-cleanup))
  ;; EAT if already loaded (don't use with-eval-after-load for cleanup).
  (when (featurep 'eat)
    (remove-hook 'eat-mode-hook #'terminal-fix--enable-buffer)))

;;;###autoload
(define-minor-mode terminal-fix-mode
  "Global minor mode to keep PTY size in sync and remove UI chrome.
Applies to terminal buffers."
  :global t
  :group 'terminal-fix
  (if terminal-fix-mode
      (progn
        ;; Save and set pixel-precise resizing (prevents off-by-one issues on HiDPI/zoom).
        (setq terminal-fix--saved-frame-resize-pixelwise frame-resize-pixelwise
              terminal-fix--saved-window-resize-pixelwise window-resize-pixelwise)
        (setq frame-resize-pixelwise t
              window-resize-pixelwise t)
        ;; Add our hooks.
        (terminal-fix--add-hooks))
    ;; Disable: cleanup buffer-local hooks, restore resize settings, remove mode hooks.
    (terminal-fix--cleanup-all-buffers)
    (setq frame-resize-pixelwise terminal-fix--saved-frame-resize-pixelwise
          window-resize-pixelwise terminal-fix--saved-window-resize-pixelwise)
    (terminal-fix--remove-hooks)))

(provide 'terminal-fix)

;;; terminal-fix.el ends here
