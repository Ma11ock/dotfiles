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
 '(company-c-headers-path-system
   (quote
    ("/usr/include/" "/usr/local/include/" "/usr/include/c++/8.3.0/")))
 '(custom-enabled-themes (quote (manoj-dark)))
 '(fci-rule-color "#383838")
 '(package-selected-packages
   (quote
    (company-irony-c-headers ivy-rtags counsel swiper ivy use-package-chords use-package-el-get use-package-ensure-system-package use-package-hydra use-package req-package org-mime highlight-indent-guides highlight-indentation auctex chess scroll-restore smooth-scroll smooth-scrolling cyberpunk-theme rainbow-mode pretty-mode cargo flycheck-rust racer helm rust-mode rebox2 ssh evil telephone-line vimrc-mode multi-term spacemacs-theme night-owl-theme php-mode robe hlinum ggtags flycheck-perl6 company-rtags neotree modern-cpp-font-lock magit cmake-mode company-c-headers hydra irony company auto-complete-clang cmake-ide rtags fish-mode flycheck iedit auto-complete-c-headers auto-complete markdown-mode pdf-tools geiser projectile clojure-mode-extra-font-locking cider slime grandshell-theme)))
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

;; ivy settings
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "<f5> f") 'counsel-describe-function)
(global-set-key (kbd "<f5> v") 'counsel-describe-variable)
(global-set-key (kbd "<f5> l") 'counsel-find-library)
(global-set-key (kbd "<f5> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f5> u") 'counsel-unicode-char)
(global-set-key (kbd "s-c g") 'counsel-git)
(global-set-key (kbd "s-c j") 'counsel-git-grep)
(global-set-key (kbd "s-c k") 'counsel-ag)
(global-set-key (kbd "s-x l") 'counsel-locate)
(global-set-key (kbd "s-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "s-r") 'counsel-minibuffer-history)

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
;;;; Keybinding settings                                                                                ;;
                                                                                                        ;;
;;; so backspace works with indentation.                                                                ;;
;;; big thanks to https://www.reddit.com/user/clemera for this.                                         ;;
                                                                                                        ;;
;;;; Programmer settings                                                                                ;;
                                                                                                        ;;
;;; Lisp settings
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)) ; for markdown editing                     ;;
                                                                                                        ;;
(add-to-list 'auto-mode-alist '("\\.fish\\'" . fish-mode)) ; for editing fish scripts                   ;;
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)) ; for editing fish scripts                     ;;

(setq company-idle-delay 1)

;; setting the code style to linux                                                                      ;;
(setq c-default-style "linux"                                                                           ;;
      c-basic-offset 4)                                                                                 ;;
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
(global-set-key [f8] 'neotree-toggle)
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
;;; C/C++ mode stuff

;; rtags ;; TODO make sure that melpa rtags does not conflict with compiled rtags
(add-to-list 'load-path "/home/ryan/CSProjects/rtags")
(set-variable 'rtags-path "/usr/local/bin/rc")
(require 'company-rtags)

(require 'rtags)
(require 'company-rtags)

(setq rtags-completions-enabled t)
(eval-after-load 'company
  '(add-to-list
    'company-backends 'company-rtags))
(setq rtags-autostart-diagnostics t)
(rtags-enable-standard-keybindings)

(setq rtags-display-result-backend 'ivy)
(require 'ivy-rtags)
(setq rtags-use-ivy t)

(require 'company-c-headers)
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-c-headers company-rtags)))

(global-company-mode t)
(add-hook 'prog-mode-hook 'flycheck-mode)

;; some ruby stuff
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)
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

;;; terminal emulator                                                                                   ;;
(add-to-list 'load-path "/home/ryan/CSProjects/emacs-libvterm/")                                        ;;
(let (vterm-install)                                                                                    ;;
  (require 'vterm))                                                                                     ;;

(defun vterm-init()
  (local-set-key "<backspace>" 'delete-backward-char))

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
 
(require 'racer)
(require 'rust-mode)
(require 'electric)
(require 'eldoc)
(require 'flycheck-rust)

(setq racer-cmd "/home/ryan/.cargo/bin/racer") ;; Rustup binaries PATH
(setq racer-rust-src-path "/home/ryan/CSProjects/rust/src") ;; Rust source code PATH

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;; https://www.emacswiki.org/emacs/PrettyGreek
(defun pretty-greek ()
  "Prettify greek symbols."
  (let ((greek '("alpha" "beta" "gamma" "delta" "epsilon" "zeta" "eta" "theta" "iota" "kappa" "lambda" "mu" "nu" "xi" "omicron" "pi" "rho" "sigma_final" "sigma" "tau" "upsilon" "phi" "chi" "psi" "omega")))
    (loop for word in greek
          for code = 97 then (+ 1 code)
          do  (let ((greek-char (make-char 'greek-iso8859-7 code))) 
                (font-lock-add-keywords nil
                                        `((,(concatenate 'string "\\(^\\|[^a-zA-Z0-9]\\)\\(" word "\\)[a-zA-Z]")
                                           (0 (progn (decompose-region (match-beginning 2) (match-end 2))
                                                     nil)))))
                (font-lock-add-keywords nil 
                                        `((,(concatenate 'string "\\(^\\|[^a-zA-Z0-9]\\)\\(" word "\\)[^a-zA-Z]")
                                           (0 (progn (compose-region (match-beginning 2) (match-end 2)
                                                                     ,greek-char)
                                                     nil)))))))))  (add-hook 'lisp-mode-hook 'pretty-greek)
(add-hook 'emacs-lisp-mode-hook 'pretty-greek)

(defun my/create-basic-ligatures ()
  "Create basic ligatures."
  (prettify-symbols-mode t)
  ;; boolean and math symbols
  (push '(">=" . ?≥) prettify-symbols-alist)
  (push '("<=" . ?≤) prettify-symbols-alist)
  (push '("!=" . ?≠) prettify-symbols-alist)

  (pretty-greek))

(defun my/create-advanced-ligatures ()
  "Create more invasive ligatures."
  (my/create-basic-ligatures)
  (push '("<-" . ?←) prettify-symbols-alist)
  (push '("->" . ?→) prettify-symbols-alist))

(add-hook 'c-mode-common-hook 'my/create-basic-ligatures)
(add-hook 'rust-mode-hook 'my/create-advanced-ligatures)
(add-hook 'lisp-mode-hook 'my/create-basic-ligatures)
(add-hook 'ruby-mode-hook 'my/create-basic-ligatures)
(add-hook 'cperl-mode-hook 'my/create-basic-ligatures)
(add-hook 'org-mode-hook  'my/create-basic-ligatures)

(setq backup-directory-alist `(("." . "~/.saves")))

(require 'highlight-indentation)

;;; emacs mail config
(require 'org-mime)

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/")
(require 'mu4e)

(setq mu4e-maildir (expand-file-name "~/Maildir"))

; get mail
(setq mu4e-get-mail-command "mbsync -a"
  ;; mu4e-html2text-command "w3m -T text/html" ;;using the default mu4e-shr2text
  mu4e-view-prefer-html t
  mu4e-update-interval 180
  mu4e-headers-auto-update t
  mu4e-compose-signature-auto-include nil
  mu4e-compose-format-flowed t)

;; to view selected message in the browser, no signin, just html mail
(add-to-list 'mu4e-view-actions
  '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;; enable inline images
(setq mu4e-view-show-images t)
;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; every new email composition gets its own frame!
(setq mu4e-compose-in-new-frame nil)

;; don't save message to Sent Messages, IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

(add-hook 'mu4e-view-mode-hook #'visual-line-mode)

;; <tab> to navigate to links, <RET> to open them in browser
(add-hook 'mu4e-view-mode-hook
  (lambda()
;; try to emulate some of the eww key-bindings
(local-set-key (kbd "<RET>") 'mu4e~view-browse-url-from-binding)
(local-set-key (kbd "<tab>") 'shr-next-link)
(local-set-key (kbd "<backtab>") 'shr-previous-link)))

;; from https://www.reddit.com/r/emacs/comments/bfsck6/mu4e_for_dummies/elgoumx
(add-hook 'mu4e-headers-mode-hook
      (defun my/mu4e-change-headers ()
	(interactive)
	(setq mu4e-headers-fields
	      `((:human-date . 25) ;; alternatively, use :date
		(:flags . 6)
		(:from . 22)
		(:thread-subject . ,(- (window-body-width) 70)) ;; alternatively, use :subject
		(:size . 7)))))

;; if you use date instead of human-date in the above, use this setting
;; give me ISO(ish) format date-time stamps in the header list
;(setq mu4e-headers-date-format "%Y-%m-%d %H:%M")

;; spell check
(add-hook 'mu4e-compose-mode-hook
    (defun my-do-compose-stuff ()
       "My settings for message composition."
       (visual-line-mode)
       (org-mu4e-compose-org-mode)
           (use-hard-newlines -1)
       (flyspell-mode)))

(require 'smtpmail)

;;rename files when moving
;;NEEDED FOR MBSYNC
(setq mu4e-change-filenames-when-moving t)

;;set up queue for offline email
;;use mu mkdir  ~/Maildir/acc/queue to set up first
(setq smtpmail-queue-mail nil)  ;; start in normal mode

;;from the info manual
(setq mu4e-attachment-dir  "~/Downloads")

(setq message-kill-buffer-on-exit t)
(setq mu4e-compose-dont-reply-to-self t)

(require 'org-mu4e)

;; convert org mode to HTML automatically
(setq org-mu4e-convert-to-html t)

;;from vxlabs config
;; show full addresses in view message (instead of just names)
;; toggle per name with M-RET
(setq mu4e-view-show-addresses 't)

;; don't ask when quitting
(setq mu4e-confirm-quit nil)

;; mu4e-context
(setq mu4e-context-policy 'pick-first)
(setq mu4e-compose-context-policy 'always-ask)
(setq mu4e-contexts
  (list
   (make-mu4e-context
    :name "personal" 
    :enter-func (lambda () (mu4e-message "Entering context work"))
    :leave-func (lambda () (mu4e-message "Leaving context work"))
    :match-func (lambda (msg)
		  (when msg
		(mu4e-message-contact-field-matches
		 msg '(:from :to :cc :bcc) "pwishie@gmail.com")))
    :vars '((user-mail-address . "pwishie@gmail.com")
	    (user-full-name . "Ryan")
	    (mu4e-sent-folder . "/pwishie-gmail/[pwishie].Sent Mail") ;
	    (mu4e-drafts-folder . "/pwishie-gmail/[pwishie].drafts")
	    (mu4e-trash-folder . "/pwishie-gmail/[pwishie].Bin")
	    (mu4e-compose-signature . (concat "Formal Signature\n" "Emacs 25, org-mode 9, mu4e 1.0\n"))
	    (mu4e-compose-format-flowed . t)
	    (smtpmail-queue-dir . "~/Maildir/pwishie-gmail/queue/cur")
	    (message-send-mail-function . smtpmail-send-it)
	    (smtpmail-smtp-user . "pwishie")
	    (smtpmail-starttls-credentials . (("smtp.gmail.com" 587 nil nil)))
	    (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg")) ;;;; TODO
	    (smtpmail-default-smtp-server . "smtp.gmail.com")
	    (smtpmail-smtp-server . "smtp.gmail.com")
	    (smtpmail-smtp-service . 587)
	    (smtpmail-debug-info . t)
	    (smtpmail-debug-verbose . t)
	    (mu4e-maildir-shortcuts . ( ("/pwishie-gmail/INBOX"            . ?i)
					("/pwishie-gmail/[pwishie].Sent Mail" . ?s)
					("/pwishie-gmail/[pwishie].Bin"       . ?t)
					("/pwishie-gmail/[pwishie].All Mail"  . ?a)
					("/pwishie-gmail/[pwishie].Starred"   . ?r)
					("/pwishie-gmail/[pwishie].drafts"    . ?d)
					))))
   (make-mu4e-context
    :name "school" 
    :enter-func (lambda () (mu4e-message "Entering context personal"))
    :leave-func (lambda () (mu4e-message "Leaving context personal"))
    :match-func (lambda (msg)
		  (when msg
		(mu4e-message-contact-field-matches
		 msg '(:from :to :cc :bcc) "acc2@gmail.com")))
    :vars '((user-mail-address . "acc2@gmail.com")
	    (user-full-name . "User Account2")
	    (mu4e-sent-folder . "/acc2-gmail/[acc2].Sent Mail")
	    (mu4e-drafts-folder . "/acc2-gmail/[acc2].drafts")
	    (mu4e-trash-folder . "/acc2-gmail/[acc2].Trash")
	    (mu4e-compose-signature . (concat "Informal Signature\n" "Emacs is awesome!\n"))
	    (mu4e-compose-format-flowed . t)
	    (smtpmail-queue-dir . "~/Maildir/acc2-gmail/queue/cur")
	    (message-send-mail-function . smtpmail-send-it)
	    (smtpmail-smtp-user . "acc2")
	    (smtpmail-starttls-credentials . (("smtp.gmail.com" 587 nil nil)))
	    (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
	    (smtpmail-default-smtp-server . "smtp.gmail.com")
	    (smtpmail-smtp-server . "smtp.gmail.com")
	    (smtpmail-smtp-service . 587)
	    (smtpmail-debug-info . t)
	    (smtpmail-debug-verbose . t)
	    (mu4e-maildir-shortcuts . ( ("/acc2-gmail/INBOX"            . ?i)
					("/acc2-gmail/[acc2].Sent Mail" . ?s)
					("/acc2-gmail/[acc2].Trash"     . ?t)
					("/acc2-gmail/[acc2].All Mail"  . ?a)
					("/acc2-gmail/[acc2].Starred"   . ?r)
					("/acc2-gmail/[acc2].drafts"    . ?d)
					))))))


(add-to-list 'load-path "~/.emacs.d/joejoe/")
(load "joejoe.el")
(require 'joejoe)

(add-hook 'text-mode-hook 'joejoe-mode)
(add-hook 'prog-mode-hook 'joejoe-mode)

(provide '.emacs)
;;; .emacs ends here
