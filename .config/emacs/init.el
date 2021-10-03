;;; package --- Summary

;;; Commentary:


;;; Code:

(setq user-emacs-directory (concat (getenv "HOME") "/.config/emacs/"))

(require 'package)

(package-initialize)

;(setq package-check-signature nil)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

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
(require 'quelpa-use-package)
(org-babel-load-file (concat user-emacs-directory "config.org"))

(put 'narrow-to-region 'disabled nil)


(provide '.emacs)
;;; .emacs ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-c-headers-path-system
   '("/usr/include/" "/usr/local/include/" "/usr/lib/gcc/x86_64-pc-linux-gnu/11.1.0/../../../../include/c++/11.1.0" "/usr/lib/gcc/x86_64-pc-linux-gnu/11.1.0/../../../../include/c++/11.1.0/x86_64-pc-linux-gnu" "/usr/lib/gcc/x86_64-pc-linux-gnu/11.1.0/../../../../include/c++/11.1.0/backward" "/usr/lib/gcc/x86_64-pc-linux-gnu/11.1.0/include" "/usr/local/include" "/usr/lib/gcc/x86_64-pc-linux-gnu/11.1.0/include-fixed" "/usr/include/c++/11.1.0/"))
 '(org-display-custom-times 1)
 '(org-export-with-sub-superscripts '{})
 '(org-time-stamp-custom-formats '("<%b %e %G>" . "<%m/%d/%y %a %H:%M>"))
 '(package-selected-packages
   '(command-log-mode evil-iedit-state evil-multiedit iedit evil-terminal-cursor-changer evil-collection evil-magit lsp-mode gdscript-mode cmake-ide ivy-rtags rtags unicode-fonts py-autopep8 blacken elpy lua-mode luarocks modus-themes highlight-doxygen org-plus-contrib f company-c-headers cmake-font-lock auctex-cluttex auctex-latexmk auctex-lua company-auctex auctex inf-clojure cider clojure-mode clojure-mode-extra-font-locking clojure-quick-repls clojure-snippets json-mode crontab-mode rainbow-mode impatient-mode vterm rust-mode systemd yasnippet-snippets yasnippet gitignore-mode company flycheck wc-mode htmlize fic-mode emojify org-bullets use-package-hydra use-package-ensure-system-package use-package-el-get use-package-chords undo-tree quelpa-use-package mu4e-query-fragments mu4e-overview mu4e-maildirs-extension mu4e-jump-to-list mu4e-conversation mu4e-alert modus-vivendi-theme modus-operandi-theme ivy-ycmd ivy-avy highlight fish-mode elfeed-web elfeed-score elfeed-protocol elfeed-org elfeed-goodies csgo-conf-mode counsel))
 '(safe-local-variable-values
   '((eval add-to-list 'company-clang-arguments '"-std=c++17")
     (eval add-to-list 'company-clang-arguments "-std=c++17")
     (eval let
           ((root
             (locate-dominating-file default-directory ".dir-locals.el")))
           (add-to-list company-clang-arguments "-std=c++17"))
     (eval let
           ((root
             (locate-dominating-file default-directory ".dir-locals.el")))
           (setq company-clang-arguments
                 (list "-std=c++17")))
     (company-mode)
     (flycheck-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(trailing-whitespace ((t (:background "#08e3f7")))))
(put 'downcase-region 'disabled nil)
