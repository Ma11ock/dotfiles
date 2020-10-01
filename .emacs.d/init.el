;;; package --- Summary

;;; Commentary:


;;; Code:

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

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(org-babel-load-file "~/.emacs.d/lisp/config.org")

(put 'narrow-to-region 'disabled nil)


(provide '.emacs)
;;; .emacs ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-display-custom-times 1)
 '(org-export-with-sub-superscripts (quote {}))
 '(org-time-stamp-custom-formats (quote ("<%b %e %G>" . "<%m/%d/%y %a %H:%M>")))
 '(package-selected-packages
   (quote
    (yasnippet-snippets yasnippet gitignore-mode company flycheck wc-mode htmlize fic-mode emojify org-bullets use-package-hydra use-package-ensure-system-package use-package-el-get use-package-chords undo-tree quelpa-use-package mu4e-query-fragments mu4e-overview mu4e-maildirs-extension mu4e-jump-to-list mu4e-conversation mu4e-alert modus-vivendi-theme modus-operandi-theme ivy-ycmd ivy-avy highlight fish-mode elfeed-web elfeed-score elfeed-protocol elfeed-org elfeed-goodies csgo-conf-mode counsel))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
