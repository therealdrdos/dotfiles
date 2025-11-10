;;; init-server.el --- DrDos Minimal Server Emacs Configuration -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2025  DrDos
;; Author: DrDos <user173@dr-dos.org>
;; This file is NOT part of GNU Emacs.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; 1. Redistributions of source code must retain the above copyright notice,
;;    this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution.
;; 3. Neither the name of the copyright holder nor the names of its
;;    contributors may be used to endorse or promote products derived from this
;;    software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:
;; Stripped-down config for server work. Bash scripts, YAML, Dockerfiles,
;; nginx/haproxy configs. No org-mode bloat, no language servers eating RAM.
;;
;; Quick keybinds:
;;   C-M-x  eval expression under cursor
;;   C-M-i  trigger completion manually
;;   M-x init  reopen this file
;;
;; System dependencies (optional but recommended):
;;   - checkbashisms  (from devscripts or standalone)
;;   - yamllint       (pip install yamllint)
;;   - git


;;; Code:
;;;; Basic Configuration --------------------------------------------------

;; More minimalistic gui
(blink-cursor-mode -1)

;; Quality of life
(global-hl-line-mode 1)
(global-display-line-numbers-mode 1)
(recentf-mode 1)
(save-place-mode 1)
(global-auto-revert-mode 1)
(delete-selection-mode 1)

;; Visual bell instead of beeping
(setq visible-bell t)

;; Current line highlight - terminal-safe colors
(if (display-graphic-p)
    (set-face-attribute 'hl-line nil :background "#D3D3D3")
  (set-face-attribute 'hl-line nil :background "color-252"))

(setq global-auto-revert-non-file-buffers t)

;; Backups in one place, not scattered across filesystem
(let ((backup-dir (expand-file-name "~/.config/emacs/EmacsBackupFiles")))
  (unless (file-directory-p backup-dir)
    (make-directory backup-dir t))
  (setq backup-directory-alist `((".*" . ,backup-dir))))

(setq history-length 25)
(setq delete-by-moving-to-trash t)

;; Quick reload of this config
(defun init ()
  "Open init-server.el for editing."
  (interactive)
  (find-file "~/.config/emacs/init.el"))


;;;; Package Management ---------------------------------------------------

;; Standard repos + MELPA
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

;; Bootstrap
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)


;;;; Minibuffer & Dired Enhancements -------------------------------------

;; Modern completion UI
(use-package vertico
  :custom
  (vertico-resize t)  ; grow/shrink minibuffer as needed
  (vertico-cycle t)   ; wrap around at top/bottom
  :init
  (vertico-mode))

;; Add context info to completion candidates (file sizes, modes, etc)
(use-package marginalia
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; Remember command/file history across sessions
(use-package savehist
  :init
  (savehist-mode))

;; When two dired windows open, assume I want to copy between them
(setq dired-dwim-target t)


;;;; Essential Packages -------------------------------------------------

;; Allows emacsclient to work as $EDITOR (for git commit, etc)
(use-package with-editor
  :defer t)

(use-package magit
  :ensure t)

(use-package which-key
  :init
    (which-key-mode 1)
  :config
  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-side-window-max-height 0.25
        which-key-idle-delay 0.3
        which-key-max-description-length 25
        which-key-allow-imprecise-window-fit t
        which-key-separator " → " ))

;; On-the-fly syntax checking
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))


;;;; Bash Script Support ------------------------------------------------

;; Use tree-sitter for bash if available (Emacs 29+)
;; Falls back gracefully to sh-mode on older versions
(setq major-mode-remap-alist
      '((sh-mode . bash-ts-mode)))

;; Lint for bashisms - useful when writing POSIX-portable scripts
;; Requires: apt install devscripts (or standalone checkbashisms)
(use-package flycheck-checkbashisms
  :after flycheck
  :ensure t
  :config
  (flycheck-checkbashisms-setup)
  (setq flycheck-checkbashisms-newline t
        flycheck-checkbashisms-posix t))


;;;; Configuration File Support -----------------------------------------

(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" . yaml-mode)
  :config
  (setq yaml-indent-offset 2))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

(use-package dockerfile-mode
  :ensure t
  :mode ("\\`Containerfile\\'" "\\`Dockerfile\\(?:\\..*\\)?\\'")
  :interpreter "dockerfile"
  :init
  (setq dockerfile-use-buildkit t))

(use-package nginx-mode
  :ensure t
  :mode ("nginx\\.conf\\'" "/nginx/.+\\.conf\\'" "/etc/nginx/.+\\.conf\\'"))

(use-package haproxy-mode
  :ensure t
  :mode ("haproxy\\.conf\\'" "/haproxy/.*\\.cfg\\'"))


;;;; Productivity Tools -------------------------------------------------

;; Auto-completion popup
(use-package company
  :ensure t
  :init (global-company-mode))

;; Snippet templates - type trigger word + TAB
(use-package yasnippet
  :ensure t
  :init (yas-global-mode))


;;;; Optional Theming ---------------------------------------------------

;; Highlight TODO/FIXME/etc in comments
(use-package hl-todo
  :hook ((prog-mode . hl-todo-mode))
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("URGENT"     error bold)
          ("DONE"       success bold)
          ("PROGRESS"   font-lock-constant-face bold)
          ("NOTE"       success bold))))


;;;; Terminal Enhancement -----------------------------------------------

;; Custom terminal-fix package (if you have it)
;; Loads only if ~/.config/emacs/lisp/terminal-fix.el exists
(let ((terminal-fix-path (expand-file-name "~/.config/emacs/lisp/")))
  (when (file-exists-p (expand-file-name "terminal-fix.el" terminal-fix-path))
    (add-to-list 'load-path terminal-fix-path)
    (require 'terminal-fix nil t)
    (when (fboundp 'terminal-fix-mode)
      (terminal-fix-mode 1))))


;;;; Custom-Set-Variables -----------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(provide 'init-server)
;;; init-server.el ends here
