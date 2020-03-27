;;; package --- Summary

;;; Commentary:


;;; Code:
(package-initialize)


(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(org-babel-load-file "~/.econfig.org")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "lualatex")
 '(company-c-headers-path-system
   (quote
    ("/usr/include/" "/usr/local/include/" "/usr/include/c++/9.2.0/")))
 '(package-selected-packages
   (quote
    (emojify crontab-mode org-pdfview godot-gdscript js-comint toml toml-mode json-mode js2-mode dracula-theme go-snippets java-snippets yasnippet-snippets company-emacs-eclim gradle-mode eclim rainbow-delimiters flycheck go-mode impatient-mode xah-css-mode psgml emmet-mode web-mode auctex-latexmk company-auctex pdf-view-restore fic-mode seti-theme vimrc-mode use-package-hydra use-package-ensure-system-package use-package-el-get use-package-chords telephone-line ssh spacemacs-theme soothe-theme smooth-scrolling smooth-scroll slime scroll-restore robe req-package rebox2 rainbow-mode racer projectile pretty-mode php-mode pdf-tools org-mime org-babel-eval-in-repl night-owl-theme neotree multi-term modern-cpp-font-lock magit linum-relative klere-theme ivy-rtags iedit hydra hungry-delete hlinum highlight-indentation highlight-indent-guides helm grandshell-theme ggtags geiser flycheck-rust flycheck-perl6 fish-mode evil elfeed-org cyberpunk-theme counsel company-rtags company-irony-c-headers company-c-headers cmake-mode cmake-ide clojure-mode-extra-font-locking cider chess cargo auto-complete-clang auto-complete-c-headers auctex ample-theme)))
 '(pdf-view-continuous nil)
 '(safe-local-variable-values
   (quote
    ((flycheck-clang-include-path "/home/ryan/src/quidcity/head/")
     (flycheck-include-path . "/home/rmj/src/cgol/head/")
     (flycheck-include-path . "/home/rmj/src/misc-sdl-projects/cgol/head/")
     (projectile-project-run-cmd . "mkdir -p build; cd build; cmake ..; make run")
     (projectile-project-compilation-cmd . "mkdir -p build; cd build; cmake ..; make")))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(company-preview ((t (:background "black" :foreground "red"))))
;;  '(company-preview-common ((t (:foreground "red"))))
;;  '(company-preview-search ((t (:inherit company-preview))))
;;  '(company-scrollbar-bg ((t (:background "brightwhite"))))
;;  '(company-scrollbar-fg ((t (:background "red"))))
;;  '(company-template-field ((t (:background "magenta" :foreground "black"))))
;;  '(company-tooltip ((t (:background "brightwhite" :foreground "black"))))
;;  '(company-tooltip-annotation ((t (:background "brightwhite" :foreground "black"))))
;;  '(company-tooltip-annotation-selection ((t (:background "color-253"))))
;;  '(company-tooltip-common ((t (:background "brightwhite" :foreground "red"))))
;;  '(company-tooltip-common-selection ((t (:background "color-253" :foreground "red"))))
;;  '(company-tooltip-mouse ((t (:foreground "black"))))
;;  '(company-tooltip-search ((t (:background "brightwhite" :foreground "black"))))
;;  '(company-tooltip-selection ((t (:background "color-253" :foreground "black")))))

(provide '.emacs)
;;; .emacs ends here
(put 'narrow-to-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#199919991999"))))
 '(company-scrollbar-fg ((t (:background "#0ccc0ccc0ccc"))))
 '(company-tooltip ((t (:inherit default :background "#051e051e051e"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 '(vterm-color-black ((t (:foreground "#3F3F3F" :background "#2B2B2B"))))
 '(vterm-color-blue ((t (:foreground "#7CB8BB" :background "#4C7073"))))
 '(vterm-color-cyan ((t (:foreground "#93E0E3" :background "#8CD0D3"))))
 '(vterm-color-green ((t (:foreground "#7F9F7F" :background "#9FC59F"))))
 '(vterm-color-magenta ((t (:foreground "#DC8CC3" :background "#CC9393"))))
 '(vterm-color-red ((t (:foreground "#AC7373" :background "#8C5353"))))
 '(vterm-color-white ((t (:foreground "#DCDCCC" :background "#656555"))))
 '(vterm-color-yellow ((t (:foreground "#DFAF8F" :background "#9FC59F"))))
 '(vterm-default-bg-color ((t (:inherit vterm-color-black))))
 '(vterm-default-fg-color ((t (:inherit vterm-color-white)))))
