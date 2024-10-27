;;; ../../src/dotfiles/.config/emacs/init.el -*- lexical-binding: t; -*-

;; To comment something out, you insert at least one semicolon before it and the
;; Emacs Lisp interpreter will ignore everything until the end of the line.
(doom! :lang
       (python +lsp)
       (javascript +lsp)
       (lua +lsp)
       (ruby +lsp)
       (csharp +unity)
       (sh +zsh +lsp)
       (sql +lsp)
       (json +lsp)
       (cc +lsp)
       clojure
       emacs-lisp
       (go +lsp)
       (yaml +lsp)
       (rust +lsp)
       markdown
       gdscript
       web
       (latex +latexmk)
       (org +pretty +journal -dragndrop
            +hugo +roam +pandoc
            +present)

       :completion
       (company +childframe) ; Autocompletion

       :os
       (:if IS-MAC macos)
       tty

       :tools
       debugger
       direnv
       (magit +forge)
       make
       pdf
       tmux

       :term
       vterm
       eshell

       (evil +everywhere)  ; come to the dark side, we have cookies
       file-templates      ; auto-snippets for empty files
       fold                ; (nigh) universal code folding
       (format +onsave)    ; automated prettiness
       snippets              ; my elves. They type so I don't have to

       :ui
       modeline              ; atom-like modeline
       neotree               ; project tree
       ophints               ; highlight the region an operation acts on
       hl-todo               ; Highlight TODO
       vi-tilde-fringe       ; fringe tildes to mark beyond EOB
       window-select         ; visually switch windows
       workspaces            ; tab emulation, persistence & separate workspaces
       zen                   ; distraction-free coding or writing

       )
