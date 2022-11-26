;;; package --- Summary Ryan Jeffrey's init.el.

;;; Copyright (C) Ryan Jeffrey 2019-2022

;;; Author: Ryan Jeffrey <pwishie@gmail.com>
;;; Created: 2019-05-12
;;; Version: 69.420
;;; URL: https://github.com/Ma11ock/dotfiles

;;; License:

;; This file is part of Ryan's configuration.
;;
;; Ryan's configuration is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Ryan's configuration is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Ryan's configuration.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(prefer-coding-system 'utf-8-unix)

(if (eq system-type 'windows-nt)
    (setq user-emacs-directory (concat (getenv "HOME") "/.emacs.d/"))
  ;; Linux.
  (setq user-emacs-directory (concat (getenv "HOME") "/.config/emacs/")))

;; Move where emacs puts its cache variables.
(let* ((my-emacs-custom-file (concat user-emacs-directory "custom-vars.el")))
  ;; Create custom variable file if it does not exist.
  (when (not (file-exists-p my-emacs-custom-file))
    (with-temp-buffer (write-file my-emacs-custom-file)))
  (setq custom-file my-emacs-custom-file)
  (load "custom-vars.el" 'noerror))


;; Set up package management.
(require 'package)

(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; Real package managers.
;; Set up quelpa.
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://github.com/quelpa/quelpa/raw/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(unless (package-installed-p 'quelpa-use-package)
  (package-refresh-contents)
  (package-install 'quelpa-use-package))

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))

(defalias 'yes-or-no-p 'y-or-n-p)

(require 'quelpa-use-package)

(eval-when-compile
  (require 'use-package))

(quelpa
 '(ox-thtml
   :fetcher git
   :url "https://github.com/juanjosegarciaripoll/org-thtml"))


(add-to-list 'load-path (concat user-emacs-directory "lisp/"))

(setq x-gtk-use-system-tooltips nil)

;; Font
(if (string= system-name "Southpark")
    (add-to-list 'default-frame-alist
                 '(font . "Inconsolata Nerd Font Mono:size=16"))
  (add-to-list 'default-frame-alist
               '(font . "Inconsolata Nerd Font Mono:size=16")))

(use-package prettify-symbols-mode
  :init
  (defconst lisp--prettify-symbols-alist
    '(("lambda"  . ?λ)))
  :hook
  (lisp-mode))


(add-to-list 'load-path (concat user-emacs-directory "/themes/"))
(setq custom-safe-themes t)   ; Treat all themes as safe
(quelpa
 '(replace-colorthemes
   :fetcher git
   :url "https://github.com/emacs-jp/replace-colorthemes"))

;; Theme
(use-package modus-themes
  :ensure t
  :init
  (setq modus-themes-bold-constructs t
        modus-themes-mode-line '(3d)
        modus-themes-italic-constructs t
        modus-themes-mixed-fonts nil
        modus-themes-subtle-line-numbers nil
        modus-themes-intense-markup t)

  (modus-themes-load-themes)
  (modus-themes-load-vivendi))

(display-time-mode 1)
(display-battery-mode 1)
;; Custom modeline.
(defvar mode-line-modes
  `(:propertize ("" mode-name)
                help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
                mouse-face mode-line-highlight
                local-map ,mode-line-major-mode-keymap))
;; Themes I like:
;; manoj dark
;; modus vivendi
;; Clarity
;; Hober
;; Comidia
;; Dark laptop
;; euphoria
;; late-night
;; ld-dark
;; simple-1
;; subdued
;; arjen
;; dark laptop
;; euphoria
;; simple-1
;; TODO make joe theme

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                        Misc                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(make-variable-buffer-local 'make-backup-files)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(add-hook 'prog-mode-hook #'(lambda ()
                              (setq show-trailing-whitespace t)))
(save-place-mode 1)
(setq tty-menu-open-use-tmm t)
(global-set-key [f10] 'tmm-menubar)
(put 'upcase-region 'disabled nil)
(electric-pair-mode t)
(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq column-number-mode t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(when (boundp 'scroll-bar-mode)
	(scroll-bar-mode -1))
(setq ring-bell-function 'ignore)
(blink-cursor-mode 0)
(set-language-environment "UTF-8")
(setq redisplay-dont-pause t)
(setq vc-follow-symlinks t) ; Otherwise emacs asks
(setq tramp-terminal-type "tramp") ; See zshrc
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)
(setq-default truncate-lines t)
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(use-package good-scroll
  :ensure t
  :init
  (good-scroll-mode 1))

(use-package smooth-scrolling
  :ensure t
  :init
  (setq smooth-scroll-margin 1)
  (smooth-scrolling-mode 1))
;; Do not jump scroll
(setq auto-window-vscroll nil)
(setq scroll-conservatively 10)
(setq scroll-margin 1)

;; Column 80 fill line.
(setq display-fill-column-indicator-column 80)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'org-mode-hook #'display-fill-column-indicator-mode)

(setenv "MANWIDTH" "100") ; For man mode

;; Man-mode auto-kill frame on exit. Should only be called from shell.
(defun man-mode-shell (man-page)
  (add-hook 'man-end-hook '(lambda ()
                             (setq man-end-hook nil)
                             (delete-frame)
                             (message "Heck")))
  (man man-page)
  (delete-window))

;; Luke Smith style snippets.
(global-set-key (kbd "M-SPC") #'(lambda ()
                                  (interactive)
                                  (search-forward "<++>")
                                  (delete-backward-char 4)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                        Modes                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package eglot
  :ensure t)

(use-package command-log-mode
  :ensure t)

(use-package csharp-mode
  :ensure t
  :config
  (format-all-mode t))

(use-package conf-mode
  :init
  (add-to-list 'auto-mode-alist '("/sxhkdrc\\'" . conf-unix-mode))
  (add-to-list 'auto-mode-alist '("/zshrc\\'" . shell-script-mode))
  (add-to-list 'auto-mode-alist '("\\config\\'" . conf-mode))
  (add-to-list 'auto-mode-alist '("\\.Xdefaults'" . conf-xdefaults-mode))
  (add-to-list 'auto-mode-alist '("\\.Xresources'" conf-xdefaults-mode))
  (add-to-list 'auto-mode-alist '("\\.Xdefaults'" . conf-xdefaults-mode)))


(use-package systemd
  :ensure t
  :mode (("\\.service\\'" . systemd-mode)))

(use-package fish-mode
  :ensure t
  :mode (("\\.fish\\'" . fish-mode)))
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))
(use-package rustic
  :ensure t
  :mode (("\\.rs\\'" . rustic-mode))
  :config
  (setq lsp-rust-server 'rust-analyzer)
  (setq rustic-format-on-save t))

(use-package toml-mode
  :ensure t)

(use-package undo-tree
  :ensure t)
(use-package highlight
  :ensure t)

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (global-undo-tree-mode)
  (setq evil-undo-system 'undo-tree)
  (evil-mode 1)
  (evil-define-command evil-write-and-kill-buffer (path)
    "Save and kill buffer."
    :repeat nil
    :move-point nil
    (interactive "<f>")
    (if (zerop (length path))
        (save-buffer)
      (write-file path))
    (kill-buffer (current-buffer)))

  (evil-ex-define-cmd "wbd[elete]" 'evil-write-and-kill-buffer)

  (use-package evil-collection
    :ensure t
    :init
    (evil-collection-init))

  (setq-default evil-cross-lines t)

  ;; Code snippet for color
  ;;evil-emacs-state-tag    (propertize "  EMACS  " 'face '((:background "turquoise" :foreground "black")))
  (setq evil-normal-state-tag   (propertize "-COMMAND-" 'face '((:foreground "turquoise")))
        evil-emacs-state-tag    (propertize "--EMACS--" 'face '((:foreground "blue")))
        evil-insert-state-tag   (propertize "--INSRT--" 'face '((:foreground "gold")))
        evil-replace-state-tag  (propertize "-REPLACE-" 'face '((:foreground "cyan")))
        evil-motion-state-tag   (propertize "--MOTION-" 'face '((:foreground "grey")))
        evil-visual-state-tag   (propertize "--VISUAL-" 'face '((:foreground "magenta")))
        evil-operator-state-tag (propertize " OPERATE-" 'face '((:foreground "grey"))))
  (setq evil-insert-state-cursor '(bar  "green")
        evil-normal-state-cursor '(box "magenta"))

  (use-package evil-terminal-cursor-changer
    :ensure t
    :init
    (evil-terminal-cursor-changer-activate))

  ;; Leader (space).
  (global-unset-key (kbd "C-SPC"))
  (define-key evil-normal-state-map (kbd "SPC") nil)
  (define-key evil-visual-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "SPC") nil)

  (evil-define-key nil 'global (kbd "<leader>er") #'eval-region)
  (evil-define-key nil 'global (kbd "<leader>ez") #'suspend-frame)
  (evil-define-key nil 'global (kbd "<leader>ss") #'split-window-horizontally)
  (evil-define-key nil 'global (kbd "<leader>so") #'split-window-vertically)
  (evil-define-key nil 'global (kbd "<leader>x") #'execute-extended-command)
  (evil-define-key 'normal 'global (kbd "gr") #'revert-buffer)
  ;; This keybind must be bound to normal map for some reason.
  (evil-define-key 'normal org-mode-map (kbd "TAB") #'org-cycle)
  ;; set leader key in all states
  (evil-set-leader 'insert (kbd "C-SPC"))
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'visual (kbd "SPC"))

  (define-key evil-visual-state-map (kbd "TAB") #'indent-region))

;; global move window keys so non joestar buffers can still have these bindings
(global-set-key (kbd "M-<left>") #'(lambda ()
                                     (interactive)
                                     (other-window -1)))

(global-set-key (kbd "M-<right>") #'(lambda ()
                                      (interactive)
                                      (other-window 1)))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package org
  :init
  (setq org-src-preserve-indentation nil
        org-edit-src-content-indentation 0)
  (require 'org-tempo)

  ;; Configure org mode to use lualatex for TeX export.
  ;; lualatex preview
  (setq org-latex-pdf-process
        '("lualatex -shell-escape -interaction nonstopmode %f"
          "lualatex -shell-escape -interaction nonstopmode %f"))

  (setq luamagick '(luamagick :programs ("lualatex" "convert")
                              :description "pdf > png"
                              :message "you need to install lualatex and imagemagick."
                              :use-xcolor t
                              :image-input-type "pdf"
                              :image-output-type "png"
                              :image-size-adjust (1.0 . 1.0)
                              :latex-compiler ("lualatex -interaction nonstopmode -output-directory %o %f")
                              :image-converter ("convert -density %D -trim -antialias %f -quality 100 %O")))

  (add-to-list 'org-preview-latex-process-alist luamagick)

  (setq org-preview-latex-default-process 'luamagick)

  (setq org-src-tab-acts-natively t)
  :bind (:map org-mode-map
              ("M-S-<up>" . 'text-scale-increase)
              ("M-S-<down>" . 'text-scale-decrease)))
(use-package org-ref
  :ensure t
  :init
  (use-package helm-bibtex
    :ensure t)
  :config
  (require 'bibtex)
  (setq bibtex-completion-bibliography `(,(concat user-emacs-directory "/bibliography/references.bib")
                                         ,(concat user-emacs-directory "/bibliography/dei.bib")
                                         ,(concat user-emacs-directory "/bibliography/master.bib")
                                         ,(concat user-emacs-directory "/bibliography/archive.bib"))
        bibtex-completion-library-path `(,(concat user-emacs-directory "/bibliography/bibtex-pdfs/"))
        bibtex-completion-additional-search-fields '(keywords)
        bibtex-completion-notes-path (concat user-emacs-directory "/bibliography/notes/"))
  (setq bibtex-autokey-year-length 4
        bibtex-autokey-name-year-separator "-"
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-titleword-separator "-"
        bibtex-autokey-titlewords 2
        bibtex-autokey-titlewords-stretch 1
        bibtex-autokey-titleword-length 5)
  (define-key bibtex-mode-map (kbd "H-b") 'org-ref-bibtex-hydra/body)
  (define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link)
  (define-key org-mode-map (kbd "s-[") 'org-ref-insert-link-hydra/body)
  (require 'helm-bibtex)
  (require 'org-ref-helm)
  (require 'org-ref-arxiv)
  (require 'org-ref-scopus)
  (require 'org-ref-wos))

;; (use-package org-ref-ivy
;;   :init (setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
;; 	          org-ref-insert-cite-function 'org-ref-cite-insert-ivy
;; 	          org-ref-insert-label-function 'org-ref-insert-label-link
;; 	          org-ref-insert-ref-function 'org-ref-insert-ref-link
;; 	          org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body))))

(use-package org-indent-mode
  :config
  (org-indent-mode t)
  :hook org-mode)

(quelpa
 '(ox-thtml
   :fetcher git
   :url "https://github.com/Ma11ock/org-thtml"))

(use-package org-bullets
  :ensure t)

(use-package wc-mode
  :ensure t
  :hook org-mode)


(use-package display-line-numbers-mode
  :hook (prog-mode org-mode LaTex-mode)
  :init
  (setq display-line-numbers-type 'relative))


;; Configuring LaTeX must be done like this because of legacy.
(use-package tex-mode
  :ensure auctex
  :config
  (auctex-latexmk-setup)
  :init
  (use-package company-auctex
    :ensure t)

  (use-package auctex-latexmk
    :ensure t)

  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (add-hook 'LaTeX-mode-hook #'flyspell-mode)
  (add-hook 'LaTeX-mode-hook #'wc-mode)
  (add-hook 'LaTeX-mode-hook #'company-auctex-init)
  (add-hook 'LaTeX-mode-hook #'company-mode)
  (add-hook 'LaTeX-mode-hook #'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook #'TeX-interactive-mode))

(when (and module-file-suffix (not (eq system-type 'windows-nt)))
  (use-package vterm
    :ensure t
    :hook
    (vterm-mode . evil-emacs-state)
    (vterm-copy-mode . meliache/evil-normal-in-vterm-copy-mode)
    :config
    (defun meliache/evil-normal-in-vterm-copy-mode ()
      (if (bound-and-true-p vterm-copy-mode)
          (evil-normal-state)
        (evil-emacs-state)))
    :init
    (setq vterm-always-compile-module t)
    :bind (:map vterm-mode-map
                ("M-c" . 'vterm-copy-mode)
                ("M-i" . 'ido-switch-buffer))))

(use-package fzf
  :ensure t
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        ;; fzf/grep-command "rg --no-heading -nH"
        fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 15))

;; Emacs IDE bloat features.
(use-package neotree
  :ensure t
  :init
  (global-set-key [f8] 'neotree-toggle)
  (setq server-after-make-frame-hook #'(lambda () (neotree)))
  (setq after-make-frame-hook #'(lambda () (neotree)))
  :config (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package clippy
  :ensure t)

(use-package minimap
  :ensure t
  :if (display-graphic-p))

(use-package beacon
  :ensure t
  :init (beacon-mode 1))

(use-package helm
  :ensure t
  :init
  (helm-mode 1)

  (use-package helm-ag
    :ensure t)

  (use-package helm-unicode
    :ensure t)

  (use-package helm-xref
    :ensure t)
  (define-key global-map [remap find-file] #'helm-find-files)
  (define-key global-map [remap execute-extended-command] #'helm-M-x)
  (define-key global-map [remap switch-to-buffer] #'helm-mini)
  (define-key global-map (kbd "M-i") #'switch-to-buffer)

  (evil-define-key 'normal 'global (kbd "<leader>ff") #'fzf)
  (evil-define-key 'normal 'global (kbd "<leader>fg") #'fzf-git)
  (evil-define-key 'normal 'global (kbd "<leader>cdf") #'clippy-describe-function)
  (evil-define-key 'normal 'global (kbd "<leader>cds") #'clippy-describe-variable)
  (evil-define-key 'normal 'global (kbd "<leader>df") #'describe-function)
  (evil-define-key 'normal 'global (kbd "<leader>dv") #'describe-variable)
  (evil-define-key 'normal 'global (kbd "<leader>ds") #'describe-symbol)
  ;; Evil helm.
  (evil-define-key 'normal 'global (kbd "<leader>haa") #'helm-ag)
  (evil-define-key 'normal 'global (kbd "<leader>haf") #'helm-ag-this-file)
  (evil-define-key 'normal 'global (kbd "<leader>hu") #'helm-unicode)
  (evil-define-key 'normal 'global (kbd "<leader>SPC") #'helm-M-x)
  ;; Use vim window commands for emacs mode.
  (evil-define-key 'emacs 'global (kbd "C-w C-w") #'evil-window-next)
  (evil-define-key 'emacs 'global (kbd "C-w C-k") #'evil-window-up)
  (evil-define-key 'emacs 'global (kbd "C-w C-p") #'evil-window-mru)
  (evil-define-key 'emacs 'global (kbd "C-w C-p") #'evil-window-mru)
  (evil-define-key 'emacs 'global (kbd "C-w C-l") #'evil-window-right)
  (evil-define-key 'emacs 'global (kbd "C-w C-h") #'evil-window-left)
  (evil-define-key 'emacs 'global (kbd "C-w W") #'evil-window-prev)
  (evil-define-key 'emacs 'global (kbd "C-w |") #'evil-window-set-width)
  (evil-define-key 'emacs 'global (kbd "C-w C-_") #'evil-window-set-height)
  (evil-define-key 'emacs 'global (kbd "C-w n") #'evil-window-new)
  (evil-define-key 'emacs 'global (kbd "C-w C-s") #'evil-window-split)
  (evil-define-key 'emacs 'global (kbd "C-w C-v") #'evil-window-vsplit)
  (evil-define-key 'emacs 'global (kbd "C-w C-x") #'evil-window-exchange)
  (evil-define-key 'emacs 'global (kbd "C-w >") #'evil-window-increase-width)
  (evil-define-key 'emacs 'global (kbd "C-w <") #'evil-window-decrease-width)
  (evil-define-key 'emacs 'global (kbd "C-w +") #'evil-window-increase-height)
  (evil-define-key 'emacs 'global (kbd "C-w -") #'evil-window-decrease-height)
  (evil-define-key 'emacs 'global (kbd "C-w C-r") #'evil-window-rotate-downwards)
  (evil-define-key 'emacs 'global (kbd "C-w R") #'evil-window-rotate-upwards))

(use-package format-all
  :ensure t)

(use-package haskell-mode
  :ensure t)

(use-package haskell-snippets
  :ensure t)

(use-package haskell-tab-indent
  :ensure t)

(use-package lsp-mode
  :ensure t
  :custom
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :init
  ;; TODO automatically format on save

  (setq lsp-before-save-edits t)

  ;(lsp-install-server 'omnisharp) TODO get this to install automagically.

  (setq gc-cons-threshold (* 100 1024 1024)
        read-process-output-max (* 1024 1024)
        treemacs-space-between-root-nodes nil
        company-idle-delay 0.0
        company-minimum-prefix-length 1
        lsp-idle-delay 0.1)  ;; clangd is fast

  (with-eval-after-load 'lsp-mode
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
    (require 'dap-cpptools))

  ;; optional if you want which-key integration
  (use-package which-key
    :ensure t
    :config
    (which-key-mode))
  ;; optionally
  (use-package lsp-ui
    :ensure t
    :commands lsp-ui-mode
    :custom
    (lsp-ui-peek-always-show t)
    (lsp-ui-sideline-show-hover t)
    (lsp-ui-doc-enable nil))

  ;; optionally if you want to use debugger
  (use-package dap-mode
    :ensure t)
  (require 'dap-gdb-lldb)
  (dap-gdb-lldb-setup)

  (use-package lsp-treemacs
    :ensure t)
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook ((c-mode c++-mode) . lsp)
  :commands lsp)


;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(use-package yaml-mode
  :ensure t
  :mode (("\\.clang-format\\'" . yaml-mode)))

(use-package cider
  :ensure t
  :bind (:map cider-mode-map
              ("M-e" . cider-eval-last-sexp)
              ("M-r" . cider-eval-region)
              )
  :init
  (eval-after-load "cider-mode"
    '(define-key cider-mode-map (kbd "C-x") 'joe-nextword)))

(use-package clojure-mode
  :ensure t)

(use-package company
  :ensure t
  :init (add-hook 'prog-mode-hook 'company-mode)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :config
  (setq company-idle-delay 0.3)
  (setq company-tooltip-align-annotations t) ; aligns annotation to the right hand side
  (setq company-minimum-prefix-length 1)
  (setq company-clang-arguments '("-std=c++17"))
  (use-package company-c-headers
    :ensure t
    :init
    (add-to-list 'company-backends 'company-c-headers)))

(use-package flycheck
  :ensure t)

(use-package with-editor
  :ensure t)

(use-package magit
  :ensure t
  :init
  (add-hook 'diff-mode-hook #'whitespace-mode)
  (add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell))

(defun insert-current-date ()
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

(use-package git-modes
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.gitignore\\'" . gitignore-mode)) )

(use-package rainbow-mode
  :ensure t
  :hook (web-mode emacs-lisp-mode))

(use-package crontab-mode
  :ensure t)

(add-hook 'prog-mode-hook #'flyspell-prog-mode) ; Flyspell on comments and strings.

(use-package cmake-mode
  :ensure t
  :hook (cmake-mode . lsp-deferred))

(use-package cmake-font-lock
  :ensure t
  :after cmake-mode
  :config (cmake-font-lock-activate))

(use-package etc-sudoers-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.api\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("/some/react/path/.*\\.js[x]?\\'" . web-mode))

  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-engines-alist
        '(("php"    . "\\.phtml\\'")
          ("blade"  . "\\.blade\\.")
          ("handlebars" . "\\.handlebars\\'")))

  (setq web-mode-content-types-alist
        '(("json" . "/some/path/.*\\.api\\'")
          ("xml"  . "/other/path/.*\\.api\\'")
          ("jsx"  . "/some/react/path/.*\\.js[x]?\\'")))
  (setq web-mode-markup-indent-offset 2)
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.handlebars\\'" . web-mode))
  (define-key web-mode-map (kbd "C-n") 'web-mode-tag-match)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-auto-closing t))

(use-package impatient-mode
  :ensure t
  :hook web-mode)

(use-package emmet-mode
  :ensure t
  :config
  (define-key web-mode-map (kbd "C-j") 'emmet-expand-line)
  (emmet-mode)
                                        ;      (emmet-preview-mode)
  :hook web-mode)

(use-package cc-mode
  :config
  (setq c-default-style "java"
        c-basic-offset 4)
  (c-set-offset 'inline-open '0))

(setq js-indent-level 2)
(use-package json-mode
  :ensure t)
;; Typescript
(use-package typescript-mode
  :ensure t)

(use-package php-mode
  :ensure t
  :init
  (use-package php-language-server
    :ensure t))


(use-package elpy
  :ensure t
  :init
  (add-hook 'python-mode-hook #'(lambda ()
                                  (elpy-enable)
                                  (when (require 'flycheck nil t)
                                    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
                                    (add-hook 'elpy-mode-hook 'flycheck-mode)))))

(use-package blacken
  :ensure t)

(use-package py-autopep8
  :ensure t
  :init
  (add-hook 'elpy-mode-hook #'py-autopep8-enable-on-save))

(use-package go-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  (add-hook 'go-mode-hook 'lsp-deferred)
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package gdscript-mode
  :ensure t
  :init
  (defun lsp--gdscript-ignore-errors (original-function &rest args)
    "Ignore the error message resulting from Godot not replying to the `JSONRPC' request."
    (if (string-equal major-mode "gdscript-mode")
        (let ((json-data (nth 0 args)))
          (if (and (string= (gethash "jsonrpc" json-data "") "2.0")
                   (not (gethash "id" json-data nil))
                   (not (gethash "method" json-data nil)))
              nil ; (message "Method not found")
            (apply original-function args)))
      (apply original-function args)))
  ;; Runs the function `lsp--gdscript-ignore-errors` around `lsp--get-message-type` to suppress unknown notification errors.
  (advice-add #'lsp--get-message-type :around #'lsp--gdscript-ignore-errors)
  (setq gdscript-godot-executable "/usr/bin/godot")
  (setq gdscript-use-tab-indents nil)
  (setq gdscript-indent-offset 4)
  (setq gdscript-docs-local-path "/home/ryan/Documents/godot-docs/_build/html/")
  :config
  (auto-revert-mode))

(use-package glsl-mode
  :ensure t)

(use-package gradle-mode
  :ensure t)

(use-package groovy-mode
  :ensure t)

(use-package lua-mode
  :ensure t)

(setq ispell-program-name (executable-find "hunspell"))
(setq ispell-local-dictionary "en_US")
(setq ispell-local-dictionary-alist
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))

(add-hook 'org-mode-hook 'flyspell-mode)

;; For asynchronous.
(use-package async
  :ensure t)

(use-package emojify
  :ensure t)

(use-package yasnippet
  :ensure t
  :init
  ;; Must be configured this way or we get errors :/
  (require 'yasnippet)
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t)

(add-to-list 'load-path "/usr/share/emacs/site-lisp/emacs-llvm-mode/")
(when (require 'llvm-mode)
  (add-to-list 'auto-mode-alist '("\\.ll\\'" . llvm-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                        Utilities                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun apply-hook-to-modes (modes-list hook-fun &optional depth local)
  "Apply a hook `HOOK-FUN' to a list of modes `MODES-LIST' at depth `DEPTH'.
If `LOCAL' is non-nil, then the hook is applied to the buffer local hook
variable rather than the global one."
  (when (not (null modes-list))
    (add-hook (car modes-list) hook-fun depth local)
    (apply-hook-to-modes (cdr modes-list) hook-fun depth local)))

(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(apply-hook-to-modes '(c-mode-hook c++-mode-hook objc-mode-hook emacs-lisp-mode) #'(lambda ()
                                                                                     (add-hook 'before-save-hook #'indent-buffer nil t)))

(apply-hook-to-modes '(emacs-lisp-mode) #'flycheck-mode)

(defun er-doas-edit (&optional arg)
  "Edit currently visited file as root With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/doas:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/doas:root@localhost:" buffer-file-name))))



(defun er-sudo-edit (&optional arg)
  "Edit currently visited file as root With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; tell emacs not to use the clipboard
                                        ;(setq x-select-enable-clipboard nil)
;; Left-to-right by default for slight performance increase.
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)
;; For slight performance increase with long lines.
(global-so-long-mode 1)

(when
    (or (string= system-name "arlen") (string= system-name "Springfield"))
  (require 'ryan-os))

(put 'narrow-to-region 'disabled nil)

(provide '.emacs)
;;; .emacs ends here
