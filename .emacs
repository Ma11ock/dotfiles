;;; package --- Summary

;;; Commentary:


;;; Code:

(package-initialize)
(org-babel-load-file "~/.econfig.org")

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "lualatex")
 '(company-c-headers-path-system
   (quote
    ("/usr/include/" "/usr/local/include/" "/usr/include/c++/8.3.0/")))
 '(package-selected-packages
   (quote
    (impatient-mode xah-css-mode psgml emmet-mode web-mode auctex-latexmk company-auctex pdf-view-restore fic-mode seti-theme vimrc-mode use-package-hydra use-package-ensure-system-package use-package-el-get use-package-chords telephone-line ssh spacemacs-theme soothe-theme smooth-scrolling smooth-scroll slime scroll-restore robe req-package rebox2 rainbow-mode racer projectile pretty-mode php-mode pdf-tools org-mime org-babel-eval-in-repl night-owl-theme neotree multi-term modern-cpp-font-lock magit linum-relative klere-theme ivy-rtags iedit hydra hungry-delete hlinum highlight-indentation highlight-indent-guides helm grandshell-theme ggtags geiser flycheck-rust flycheck-perl6 fish-mode evil elfeed-org cyberpunk-theme counsel company-rtags company-irony-c-headers company-c-headers cmake-mode cmake-ide clojure-mode-extra-font-locking cider chess cargo auto-complete-clang auto-complete-c-headers auctex ample-theme)))
 '(pdf-view-continuous nil)
 '(safe-local-variable-values
   (quote
    ((projectile-project-run-cmd . "mkdir -p build; cd build; cmake ..; make run")
     (projectile-project-compilation-cmd . "mkdir -p build; cd build; cmake ..; make")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide '.emacs)
;;; .emacs ends here
(put 'narrow-to-region 'disabled nil)
