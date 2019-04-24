
;;;; Ryan's Emacs file

;;; package init stuff
(package-initialize)

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (evil telephone-line vimrc-mode multi-term spacemacs-theme night-owl-theme php-mode robe hlinum ggtags flycheck-perl6 company-rtags neotree modern-cpp-font-lock magit cmake-mode company-c-headers hydra irony company auto-complete-clang cmake-ide rtags fish-mode flycheck iedit auto-complete-c-headers auto-complete markdown-mode pdf-tools geiser projectile clojure-mode-extra-font-locking cider slime grandshell-theme)))
 '(safe-local-variable-values
   (quote
    ((projectile-project-run-cmd . "mkdir -p build; cd build; cmake ..; make run")
     (projectile-project-compilation-cmd . "mkdir -p build; cd build; cmake ..; make")
     (eval setq flycheck-clang-include-path
           (list
            (expand-file-name "~/CSProjects/allnew/head")))
     (cmake-ide-build-dir . "./bin/"))))
 '(spacemacs-theme-comment-italic t)
 '(spacemacs-theme-org-bold nil)
 '(spacemacs-theme-underline-parens nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; theme and font stuff
(load-theme 'spacemacs-dark t)  ;(load-theme 'night-owl t)https://stackoverflow.com/questions/3297

(add-to-list 'default-frame-alist
             '(font . "Menlo:antialias=true:size=16:style=Regular"))


;;; various emacs settings

(put 'upcase-region 'disabled nil)
(electric-pair-mode t)
(desktop-save-mode 1)
(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq column-number-mode t)
(display-time-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq ring-bell-function 'ignore)
(blink-cursor-mode 0)

;;;; Keybinding settings

;;; so backspace works with indentation.
;;; big thanks to https://www.reddit.com/user/clemera for this.

(defun my-backward-delete-char ()
  (interactive)
  (cond ((bolp)
         (delete-char -1)
         (indent-according-to-mode)
         (when (looking-at "\\([ \t]+\\)[^ \t]")
           (delete-region (point) (match-end 1))))
        ((<= (point) (save-excursion (back-to-indentation) (point)))
         (let ((backward-delete-char-untabify-method 'hungry))
           (call-interactively 'backward-delete-char-untabify)
           (delete-char -1))
         (indent-according-to-mode))
        (t
         (let ((backward-delete-char-untabify-method 'hungry))
           (call-interactively 'backward-delete-char-untabify)))))

(global-set-key (kbd "<backspace>") 'my-backward-delete-char)

;;;; Programmer settings

;;; Lisp settings



(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)) ; for markdown editing

(add-to-list 'auto-mode-alist '("\\.fish\\'" . fish-mode)) ; for editing fish scripts

;; auto-complete
(require 'auto-complete)
(ac-config-default)

;; setting the code style to linux
(setq c-default-style "linux"
      c-basic-offset 4)

(global-flycheck-mode 1) ; flycheck

;; for betting modern C++ highlighting
(require 'modern-cpp-font-lock)
(modern-c++-font-lock-global-mode t)

;; for setting up the spellcheck program
(setq ispell-program-name "hunspell")
(setq ispell-local-dictionary "en_US")
(setq ispell-local-dictionary-alist
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))

;; notree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)


;; Set your lisp system and, optionally, some contribs
(setq inferior-lisp-program "/opt/sbcl/bin/sbcl") ;;;; TODO: CHECK
(setq slime-contribs '(slime-fancy))

(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "sbcl")

(setq geiser-active-implementations '(guile))


;; cperl-mode
(fset 'perl-mode 'cperl-mode)
(setq cperl-indent-level 4)
(setq cperl-extra-newline-before-brace t
      cperl-brace-offset              -2
      cperl-merge-trailing-else        nil)

;; C/C++ mode stuff
(semantic-mode 1)

(defun my:add-semantic-to-autocomplete()
  (add-to-list 'ac-sources 'ac-source-semantic))

(add-hook 'c-mode-hook 'my:add-semantic-to-autocomplete)
(add-hook 'c++-mode-hook 'my:add-semantic-to-autocomplete)

(defun my:ac-c-headers-init()
  (require 'auto-complete-c-headers)
  (add-to-list 'achead:include-directories '"/usr/include/")
  (add-to-list 'achead:include-directories '"/usr/include/c++/8.2.1/")
  (add-to-list 'ac-sources 'ac-source-c-headers))

(add-hook 'c-mode-hook 'my:ac-c-headers-init)
(add-hook 'c++-mode-hook 'my:ac-c-headers-init)

(global-ede-mode 1)

(ede-cpp-root-project  "dirw" :file "~/CSProjects/gitdirw/src/main.cpp" :include-path '("../head/"))

;; CHANGE THIS EACH TIME
(add-hook 'c++-mode-hook
          (lambda () (setq flycheck-clang-include-path
                           (list (expand-file-name "~/CSProjects/gitdirw/head/")))))

(setq ggtags-executable-directory "/usr/bin/")

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))
(setq ggtags-use-idutils t)
(setq ggtags-use-project-gtagsconf nil)


(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)

(setq ruby-indent-level 4)

(add-to-list 'magic-mode-alist
                `(,(lambda ()
                     (and (string= (file-name-extension buffer-file-name) "h")
                          (re-search-forward "@\\<interface\\>" 
					     magic-mode-regexp-match-limit t)))
                  . objc-mode))


(add-hook 'term-mode-hook
	  (defun my-term-mode-hook ()
			(setq bidi-paragraph-direction 'left-to-right)))

(set-frame-parameter (selected-frame) 'alpha '(95 . 50))
(add-to-list 'default-frame-alist '(alpha . (95 . 50)))

(setq-default term-suppress-hard-newline t)


;;; terminal emulator
(add-to-list 'load-path "/home/ryan/CSProjects/emacs-libvterm/")
(let (vterm-install)
  (require 'vterm))



(require 'telephone-line)

;;; telephone-line configuration
(setq telephone-line-lhs
        '((evil   . (telephone-line-evil-tag-segment))
          (accent . (telephone-line-vc-segment
                     telephone-line-erc-modified-channels-segment
                     telephone-line-process-segment))
          (nil    . (telephone-line-minor-mode-segment
                     telephone-line-buffer-segment))))

(setq telephone-line-rhs
      '((nil    . (telephone-line-misc-info-segment))
        (accent . (telephone-line-major-mode-segment))
        (evil   . (telephone-line-airline-position-segment))))

(telephone-line-mode 1)

;; evil mode settings
(require 'evil)
(evil-mode 1)

;; absolute line numbers
(require 'linum-relative)
(linum-mode)
(linum-relative-global-mode)
(setq linum-relative-current-symbol "")

