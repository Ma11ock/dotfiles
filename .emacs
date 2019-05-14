;;;; Ryan's Emacs file

;;; package init stuff
(package-initialize)

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.milkbox.net/packages/"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#000000" "#8b0000" "#00ff00" "#ffa500" "#7b68ee" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(comment-style (quote box))
 '(custom-enabled-themes (quote (manoj-dark)))
 '(fci-rule-color "#383838")
 '(package-selected-packages
   (quote
    (org-mime highlight-indent-guides highlight-indentation auctex chess scroll-restore smooth-scroll smooth-scrolling cyberpunk-theme rainbow-mode pretty-mode cargo flycheck-rust racer helm rust-mode rebox2 ssh evil telephone-line vimrc-mode multi-term spacemacs-theme night-owl-theme php-mode robe hlinum ggtags flycheck-perl6 company-rtags neotree modern-cpp-font-lock magit cmake-mode company-c-headers hydra irony company auto-complete-clang cmake-ide rtags fish-mode flycheck iedit auto-complete-c-headers auto-complete markdown-mode pdf-tools geiser projectile clojure-mode-extra-font-locking cider slime grandshell-theme)))
 '(safe-local-variable-values
   (quote
    ((eval setq flycheck-clang-include-path
           (list
            (expand-file-name "~/CSProjects/dirw/include/")))
     (projectile-project-run-cmd . "mkdir -p build; cd build; cmake ..; make run")
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

(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono:antialias=true:size=16:style=Regular"))


;;; various emacs settings
(put 'upcase-region 'disabled nil)                                                                      ;;
(electric-pair-mode t)                                                                                  ;;
(desktop-save-mode 1)                                                                                   ;;
(show-paren-mode 1)                                                                                     ;;
(setq-default indent-tabs-mode nil)                                                                     ;;
(setq-default tab-width 4)                                                                              ;;
(setq column-number-mode t)                                                                             ;;
(display-time-mode 1)                                                                                   ;;
(tool-bar-mode -1)                                                                                      ;;
(scroll-bar-mode -1)                                                                                    ;;
(setq ring-bell-function 'ignore)                                                                       ;;
(blink-cursor-mode 0)                                                                                   ;;
                                                                                                        ;;
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)) ; for markdown editing                     ;;
                                                                                                        ;;
(add-to-list 'auto-mode-alist '("\\.fish\\'" . fish-mode)) ; for editing fish scripts                   ;;
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)) ; for editing fish scripts                     ;;
                                                                                                        ;;
;; auto-complete                                                                                        ;;
(require 'auto-complete)                                                                                ;;
(ac-config-default)                                                                                     ;;
                                                                                                        ;;
;; setting the code style to linux                                                                      ;;
(setq c-default-style "linux"                                                                           ;;
      c-basic-offset 4)                                                                                 ;;
                                                                                                        ;;
(global-flycheck-mode 1)
                                                                                                        ;;
;; for betting modern C++ highlighting                                                                  ;;
(require 'modern-cpp-font-lock)                                                                         ;;
(modern-c++-font-lock-global-mode t)                                                                    ;;
                                                                                                        ;;
;; for setting up the spellcheck program                                                                ;;
(setq ispell-program-name "hunspell")                                                                   ;;
(setq ispell-local-dictionary "en_US")                                                                  ;;
(setq ispell-local-dictionary-alist                                                                     ;;
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))                                ;;
(require 'neotree)                                                                                      ;;
(global-set-key [f8] 'neotree-toggle)                                                                   ;;
                                                                                                        ;;
                                                                                                        ;;
;; Set your lisp system and, optionally, some contribs                                                  ;;
(setq inferior-lisp-program "/opt/sbcl/bin/sbcl")
(setq slime-contribs '(slime-fancy))                                                                    ;;
                                                                                                        ;;
(load (expand-file-name "~/quicklisp/slime-helper.el"))                                                 ;;
;; Replace "sbcl" with the path to your implementation                                                  ;;
(setq inferior-lisp-program "sbcl")                                                                     ;;
                                                                                                        ;;
(setq geiser-active-implementations '(guile))                                                           ;;
                                                                                                        ;;
;; cperl-mode                                                                                           ;;
(fset 'perl-mode 'cperl-mode)                                                                           ;;
(setq cperl-indent-level 4)                                                                             ;;
(setq cperl-extra-newline-before-brace t                                                                ;;
      cperl-brace-offset              -2                                                                ;;
      cperl-merge-trailing-else        nil)                                                             ;;
                                                                                                        ;;
;; C/C++ mode stuff                                                                                     ;;;;
                                                                                                        ;;
(defun my:add-semantic-to-autocomplete()
  (semantic-mode 1)
  (add-to-list 'ac-sources 'ac-source-semantic))                                                        ;;
                                                                                                        ;;
(add-hook 'c-mode-hook 'my:add-semantic-to-autocomplete)
(add-hook 'c++-mode-hook 'my:add-semantic-to-autocomplete)

(defun my:ac-c-headers-init()                                                                           ;;    ;;
  (require 'auto-complete-c-headers)                                                                    ;;    ;;
  (add-to-list 'achead:include-directories '"/usr/include/")                                            ;;    ;;
  (add-to-list 'achead:include-directories '"/usr/include/c++/8.2.1/")                                        ;;
  ;; CURRENT PROJECT                                                                                          ;;
  (add-to-list 'achead:include-directories '"~/CSProjects/dirw/include/") ;; to fix
  (add-to-list 'ac-sources 'ac-source-c-headers))

(add-hook 'c-mode-hook 'my:ac-c-headers-init)                                                           ;;    ;;
(add-hook 'c++-mode-hook 'my:ac-c-headers-init)                                                         ;;    ;;

(global-ede-mode 1)
                                                                                                        
;; CHANGE THIS EACH TIME                                                                                ;;
(add-hook 'c++-mode-hook                                                                                ;;
          (lambda () (setq flycheck-clang-include-path                                                  ;;
                           (list (expand-file-name "~/CSProjects/gitdirw/head/")))))                    ;;
                                                                                                        ;;
(setq ggtags-executable-directory "/usr/bin/")                                                          ;;
                                                                                                        ;;
(add-hook 'c-mode-common-hook                                                                           ;;
          (lambda ()                                                                                    ;;
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)                                         ;;
              (ggtags-mode 1))))                                                                        ;;
(setq ggtags-use-idutils t)                                                                             ;;
(setq ggtags-use-project-gtagsconf nil)                                                                 ;;
                                                                                                        ;;
                                                                                                        ;;
(add-hook 'ruby-mode-hook 'robe-mode)                                                                   ;;
(add-hook 'robe-mode-hook 'ac-robe-setup)                                                               ;;
                                                                                                        ;;
(setq ruby-indent-level 4)                                                                              ;;
                                                                                                        ;;
(add-to-list 'magic-mode-alist                                                                          ;;
                `(,(lambda ()                                                                           ;;
                     (and (string= (file-name-extension buffer-file-name) "h")                          ;;
                          (re-search-forward "@\\<interface\\>"                                         ;;
					     magic-mode-regexp-match-limit t)))                                                ;;
                  . objc-mode))                                                                         ;;
                                                                                                        ;;
                                                                                                        ;;
(add-hook 'term-mode-hook                                                                               ;;
	  (defun my-term-mode-hook ()                                                                          ;;
			(setq bidi-paragraph-direction 'left-to-right)))                                               ;;
  ;;                                                                                                       ;;
;; (set-frame-parameter (selected-frame) 'alpha '(95 . 50))                                                ;;
;; (add-to-list 'default-frame-alist '(alpha . (95 . 50))) ; adds alpha transparency
                                                                                                        ;;
(setq-default term-suppress-hard-newline t)                                                             ;;
                                                                                                        ;;
                                                                                                        ;;
;;; terminal emulator                                                                                   ;;
(add-to-list 'load-path "/home/ryan/CSProjects/emacs-libvterm/")                                        ;;
(let (vterm-install)                                                                                    ;;
  (require 'vterm))                                                                                     ;;

(require 'telephone-line)                                                                               ;;
                                                                                                        ;;
;;; telephone-line configuration'                                                                       ;;
                                                                                                        ;;
(setq telephone-line-lhs                                                                                ;;
      '(                                                                                                ;;
        (accent . (telephone-line-vc-segment                                                            ;;
                   telephone-line-erc-modified-channels-segment                                         ;;
                   telephone-line-process-segment))                                                     ;;
        (nil    . (telephone-line-minor-mode-segment                                                    ;;
                   telephone-line-buffer-segment))))                                                    ;;
                                                                                                        ;;
(setq telephone-line-rhs                                                                                ;;
      '((nil    . (telephone-line-misc-info-segment))                                                   ;;
        (accent . (telephone-line-major-mode-segment))                                                  ;;
        (evil   . (telephone-line-airline-position-segment))))                                          ;;
                                                                                                        ;;
(telephone-line-mode 1)                                                                                 ;;
                                                                                                       ;;
;; absolute line numbers
(require 'linum-relative)                                                                               ;;
(linum-mode)                                                                                            ;;
(linum-relative-global-mode)                                                                            ;;
(setq linum-relative-current-symbol "")

;;; ssh mode stuff                                                                                      ;;
 (require 'ssh)                                                                                         ;;
    (add-hook 'ssh-mode-hook                                                                            ;;
              (lambda ()                                                                                ;;
                (setq ssh-directory-tracking-mode t)                                                    ;;
                (shell-dirtrack-mode t)                                                                 ;;
                (setq dirtrackp nil)))


(require 'rebox2)
(rebox-register-template                                                                                ;;
 75                                                                                                     ;;
 999                                                                                                    ;;
 '("?*************?"                                                                                    ;;
   "?* box123456 *?"                                                                                    ;;
   "?*************?"))                                                                                  ;;
                                                                                                        ;;
(add-hook 'perl-mode-hook (lambda ()                                                                    ;;
; The "style loop" specifies a list of box styles which rebox will cycle                                ;;
; through if you refill (M-q) a box repeatedly. Having "11" in this loop                                ;;
; will allow you to easily "unbox" a comment block, e.g. for "uncomment-region"                         ;;
                (set (make-local-variable 'rebox-style-loop) '(75 11))                                  ;;
; The "min-fill-column" setting ensures that the box is not made narrower                               ;;
; when the text is short                                                                                ;;
                (set (make-local-variable 'rebox-min-fill-column) 79)                                   ;;
                (rebox-mode 1)))                                                                        ;;
                                                                                                        ;;
(add-hook 'objc-mode-hook (lambda ()                                                                    ;;
; The "style loop" specifies a list of box styles which rebox will cycle                                ;;
; through if you refill (M-q) a box repeatedly. Having "11" in this loop                                ;;
; will allow you to easily "unbox" a comment block, e.g. for "uncomment-region"                         ;;
                (set (make-local-variable 'rebox-style-loop) '(75 11))                                  ;;
; The "min-fill-column" setting ensures that the box is not made narrower                               ;;
; when the text is short                                                                                ;;
                (set (make-local-variable 'rebox-min-fill-column) 78)                                   ;;
                (rebox-mode 1)))                                                                        ;;
                                                                                                        ;;
                                                                                                        ;;
(defun er-sudo-edit (&optional arg)                                                                     ;;
  "Edit currently visited file as root With a prefix ARG prompt for a file to visit.  Will also prompt for a file to visit if current buffer is not visiting a file."
  (interactive "P")                                                                                     ;;
  (if (or arg (not buffer-file-name))                                                                   ;;
      (find-file (concat "/sudo:root@localhost:"                                                        ;;
                         (ido-read-file-name "Find file(as root): ")))                                  ;;
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))                           ;;
 
(require 'company)
(require 'racer)
(require 'rust-mode)
(require 'electric)
(require 'eldoc)
(require 'flycheck)
(require 'flycheck-rust)

(setq racer-cmd "/home/ryan/.cargo/bin/racer") ;; Rustup binaries PATH
(setq racer-rust-src-path "/home/ryan/CSProjects/rust/src") ;; Rust source code PATH

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(defun my/create-basic-ligatures ()
  "Create basic ligatures."
  (push '(">=" . ?≥) prettify-symbols-alist)
  (push '("<=" . ?≤) prettify-symbols-alist)
  (push '("!=" . ?≠) prettify-symbols-alist))

(defun my/create-advanced-ligatures ()
  "Create more invasive ligatures."
  (my/create-basic-ligatures)
  (push '("<-" . ?←) prettify-symbols-alist)
  (push '("->" . ?→) prettify-symbols-alist))

(add-hook 'c-mode-common-hook 'my/create-basic-ligatures)
(add-hook 'rust-mode-hook 'my/create-advanced-ligatures)
(add-hook 'c-mode-common-hook 'my/create-advanced-ligatures)

(global-prettify-symbols-mode)

(add-to-list 'load-path "~/.emacs.d/joejoe/")
(load "joejoe.el")
(require 'joejoe)

(setq normal-hooks (list c-mode-common-hook ))

(setq backup-directory-alist `(("." . "~/.saves")))

(require 'highlight-indentation)

(provide '.emacs)
;;; .emacs ends here
