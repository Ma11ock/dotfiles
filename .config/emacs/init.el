;;; package --- Summary

;;; Commentary: My init.el.


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
(require 'quelpa-use-package)

;; Load my actual config.
(org-babel-load-file (concat user-emacs-directory "config.org"))

(put 'narrow-to-region 'disabled nil)

(provide '.emacs)
;;; .emacs ends here
