;;; init.el --- DrDos Emacs Configuration File -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2025  DrDos
;; Author:     DrDos <user173@dr-dos.org>
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
;; Use C-M-x to evaluate while editing
;; Use C-M-i to get autocompletion inside the editor buffer
;; Packages to install system-wide for this config to work:
;; - A nerd font (currenctly using UbuntuSans Nerd Font)
;; - Linters:
;;   - ansible-lint (Ansible)
;;   - pylint (Python)
;;   - eslint (JavaScript)
;; - For Language Support:
;;   - cmake (For C/C++)
;;   - irony-server (For C/C++)
;;   - devscripts (Only on Debian, visit https://github.com/cuonglm/flycheck-checkbashisms for more)
;;   - clang-tidy (For C/C++)
;;   - tidy (For HTML)


;;; Code:
;;;; Basic Configuration --------------------------------------------------

(tool-bar-mode -1)  ; hides toolbar
(scroll-bar-mode -1)  ; hides scrolbar
(blink-cursor-mode -1)  ; hinders the curser from blinking
(global-hl-line-mode 1)  ; line highlighting
(global-display-line-numbers-mode 1)  ; display line numbers
(recentf-mode 1)  ; remembers recent files
(save-place-mode 1)  ; remember edit position in files
(global-auto-revert-mode 1)  ; watch for file changes and refresh on-change
(delete-selection-mode 1)  ; selected text gets deleted when typing

(setq visible-bell t) ; flash when the bell rings
(set-face-attribute 'hl-line nil :background "#D3D3D3") ; line hl light grey
(setq global-auto-revert-non-file-buffers t) ; refresh dired and co on changes automatically
(setq backup-directory-alist '((".*" . "~/.emacs.d/EmacsBackupFiles"))) ; put all backup files in another location
(global-set-key [escape] 'keyboard-escape-quit) ; Escape Minibuffer with one not three escape presses
;; Minibuffer history
(setq history-length 25)
(savehist-mode 1)

;;; Keybinding to open init.el quickly
(defun init ()
  "Quickly open the init.el config file."
  (interactive)
  (find-file "~/.config/emacs/init.el"))


;;;; Package Management ---------------------------------------------------

;; Setup
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)
;; refresh if archive is empty
(unless package-archive-contents
  (package-refresh-contents))
;; install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)


;;;; Theming --------------------------------------------------------------

;; Theme
(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-light t))
(setq solarized-highlight-numbers t)

;; Nerd-Font
(use-package nerd-icons
  :custom
   (nerd-icons-font-family "UbuntuSans Nerd Font")
   )

;; Custom-Modeline (From Doom project)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Highlight TODO and other words
(use-package hl-todo
  :hook ((org-mode . hl-todo-mode)
         (prog-mode . hl-todo-mode))
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("URGENT"     error bold)
	  ("DONE"       success bold)
          ("PROGRESS"   font-lock-constant-face bold)
          ("NOTE"       success bold))))


;;;; Org-Mode -------------------------------------------------------------

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(setq org-log-done 'time)

(use-package toc-org ; enabling table of contents
    :commands toc-org-enable
    :init (add-hook 'org-mode-hook 'toc-org-enable))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-superstar) ; bullet points rather then asterics
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))


;;;; Additional packages -------------------------------------------------

;; With-editor for allowing the emacsclient to be the editor
(use-package with-editor
  :defer t)

;; Magit
(use-package magit
  :ensure t
  )

;; Which-key
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

;; Linter
;; Flycheck especially configured for Ansible and Yaml
;; NOTE to myself: If I have problems with C, I should have a look at flycheck-pkg-config
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (flycheck-define-checker ansible-lint
    "A linter for ansible playbooks"
    :command ("ansible-lint" "--parseable-severity" source-inplace)
    :error-patterns
    ((error line-start
            (file-name) ":" line ":" column ":"
            (id (one-or-more (not (any ":"))))
            ": " (message)
            line-end))
    :modes yaml-mode))


;;;; Additional language support -----------------------------------------
;; Install treesitter grammar
(when (and (fboundp 'treesit-install-language-grammar)
           (not (treesit-language-available-p 'html)))
  (treesit-install-language-grammar 'html))

;; Global Treesitter Mode
(when (fboundp 'global-ts-mode)
  (global-ts-mode))

;; If global treesitter mode does not work correctly, and for my reference
(setq major-mode-remap-alist
      '((c-mode          . c-ts-mode)
        (c++-mode        . c++-ts-mode)
        (python-mode     . python-ts-mode)
        (css-mode        . css-ts-mode)
        (json-mode       . json-ts-mode)
        (sh-mode         . bash-ts-mode)
        ))

;; PHP
(use-package php-mode)

;; Markdown
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

;; Yaml
(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" . yaml-mode)
  :config
  (setq yaml-indent-offset 2)
  )

;; Rust
(use-package rust-mode
  :ensure t                 ;; lädt es beim ersten Bedarf von MELPA
  :mode ("\\.rs\\'" . rust-mode)
  ;; eigene Konfiguration hier:
  :config
  (setq rust-format-on-save t))

;; Company Mode
(use-package company
  :ensure t
  :init (global-company-mode))

;; HTML Prettier on save
(use-package reformatter :ensure t)
(reformatter-define prettier-format
  :program "prettier" :args '("--stdin-filepath" input-file))
(add-hook 'html-ts-mode-hook #'prettier-format-on-save-mode)

;; Company-Mode for Web Development
(use-package company-web
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends 'company-web-html))

;;; Flycheck Stuff
;; Eglot for C/C++
(use-package eglot
  :defer t
  :hook ((c-mode c++-mode objc-mode cuda-mode) . eglot-ensure)
  :config
  (setq eglot-autoshutdown t
        eglot-events-buffer-size 0))

;; Makes flycheck and eglot work together
(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(use-package yasnippet
  :ensure t
  :init (yas-global-mode))

(use-package emmet-mode
  :ensure t
  :hook  (html-ts-mode . emmet-mode)
  :config (setq emmet-move-cursor-between-quotes t))

;; CSS
(when (fboundp 'css-ts-mode)
  (add-to-list 'auto-mode-alist '("\\.css\\'" . css-ts-mode)))

(use-package flycheck-checkbashisms
  :after flycheck
  :ensure t
  :config
  (flycheck-checkbashisms-setup)
  (setq flycheck-checkbashisms-newline t)
  (setq flycheck-checkbashisms-posix   t))

(use-package flycheck-rust
  :ensure t
  :after (rust-mode flycheck)
  :hook (rust-mode . flycheck-rust-setup))


;;;; Custom-Set-Variables added by packages -----------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(flycheck-irony nerd-icons hl-todo solarized-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(provide 'init)
;;; init.el ends here
