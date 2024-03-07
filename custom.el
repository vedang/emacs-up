(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   '(((output-dvi has-no-display-manager) "dvi2tty")
     ((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi")
     (output-pdf "PDF Tools") (output-html "xdg-open")))
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#fffefe" "#9d0000" "#006a00" "#0e1b00" "#6845138" "#840086"
    "#003567" "#494949"])
 '(auto-save-list-file-prefix "~/.emacs.d/temp-files/auto-save-list/.auto-saves-")
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(bookmark-default-file "~/.emacs.d/bookmarks")
 '(cider-repl-history-file "~/.emacs.d/temp-files/nrepl-history.txt")
 '(cider-repl-print-length 200)
 '(completion-styles '(basic partial-completion emacs22))
 '(connection-local-criteria-alist
   '(((:application tramp :protocol "kubernetes")
      tramp-kubernetes-connection-local-default-profile)
     ((:application tramp :protocol "flatpak")
      tramp-container-connection-local-default-flatpak-profile
      tramp-flatpak-connection-local-default-profile)
     ((:application tramp :machine "localhost")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp :machine "raagitkombdi.vedang.me")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile
      tramp-connection-local-default-shell-profile)
     ((:application eshell) eshell-connection-default-profile)))
 '(connection-local-profile-alist
   '((tramp-flatpak-connection-local-default-profile
      (tramp-remote-path "/app/bin" tramp-default-remote-path "/bin"
                         "/usr/bin" "/sbin" "/usr/sbin"
                         "/usr/local/bin" "/usr/local/sbin"
                         "/local/bin" "/local/freeware/bin"
                         "/local/gnu/bin" "/usr/freeware/bin"
                         "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin"
                         "/opt/sbin" "/opt/local/bin"))
     (tramp-kubernetes-connection-local-default-profile
      (tramp-config-check . tramp-kubernetes--current-context-data)
      (tramp-extra-expand-args 97
                               (tramp-kubernetes--container
                                (car tramp-current-connection))
                               104
                               (tramp-kubernetes--pod
                                (car tramp-current-connection))
                               120
                               (tramp-kubernetes--context-namespace
                                (car tramp-current-connection))))
     (tramp-container-connection-local-default-flatpak-profile
      (tramp-remote-path "/app/bin" tramp-default-remote-path "/bin"
                         "/usr/bin" "/sbin" "/usr/sbin"
                         "/usr/local/bin" "/usr/local/sbin"
                         "/local/bin" "/local/freeware/bin"
                         "/local/gnu/bin" "/usr/freeware/bin"
                         "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin"
                         "/opt/sbin" "/opt/local/bin"))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o"
                                        "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o" "state=abcde" "-o"
                                        "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format (pid . number)
                                          (euid . number)
                                          (user . string)
                                          (egid . number) (comm . 52)
                                          (state . 5) (ppid . number)
                                          (pgrp . number)
                                          (sess . number)
                                          (ttname . string)
                                          (tpgid . number)
                                          (minflt . number)
                                          (majflt . number)
                                          (time . tramp-ps-time)
                                          (pri . number)
                                          (nice . number)
                                          (vsize . number)
                                          (rss . number)
                                          (etime . tramp-ps-time)
                                          (pcpu . number)
                                          (pmem . number) (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o"
                                        "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o" "stat=abcde" "-o"
                                        "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format (pid . number)
                                          (user . string)
                                          (group . string) (comm . 52)
                                          (state . 5) (ppid . number)
                                          (pgrp . number)
                                          (ttname . string)
                                          (time . tramp-ps-time)
                                          (nice . number)
                                          (etime . tramp-ps-time)
                                          (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o"
                                        "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o"
                                        "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format (pid . number)
                                          (euid . number)
                                          (user . string)
                                          (egid . number)
                                          (group . string) (comm . 52)
                                          (state . string)
                                          (ppid . number)
                                          (pgrp . number)
                                          (sess . number)
                                          (ttname . string)
                                          (tpgid . number)
                                          (minflt . number)
                                          (majflt . number)
                                          (time . tramp-ps-time)
                                          (pri . number)
                                          (nice . number)
                                          (vsize . number)
                                          (rss . number)
                                          (etime . number)
                                          (pcpu . number)
                                          (pmem . number) (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh") (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":") (null-device . "/dev/null"))
     (eshell-connection-default-profile (eshell-path-env-list))))
 '(custom-safe-themes
   '("97db542a8a1731ef44b60bc97406c1eb7ed4528b0d7296997cbb53969df852d6"
     "47e6f8c23eaea064b89ed1361b5824ee4f9562a8c4a30774ee9ee69f9b9d4f69"
     "1d89fcf0105dd8778e007239c481643cc5a695f2a029c9f30bd62c9d5df6418d"
     "97965ccdac20cae22c5658c282544892959dc541af3e9ef8857dbf22eb70e82b"
     "420459d6eeb45aadf5db5fbcc3d6990b65141c104911f7359454fc29fa9d87a0"
     "35ce59fe20479957989ed789edd305adac5020ed5cf6dabda4ae351d5e380520"
     "02591317120fb1d02f8eb4ad48831823a7926113fa9ecfb5a59742420de206e0"
     "170bb47b35baa3d2439f0fd26b49f4278e9a8decf611aa33a0dad1397620ddc3"
     "fa2af0c40576f3bde32290d7f4e7aa865eb6bf7ebe31eb9e37c32aa6f4ae8d10"
     default))
 '(eldoc-echo-area-prefer-doc-buffer 'maybe)
 '(eww-bookmarks-directory "~/.emacs.d/temp-files/")
 '(fontaine-latest-state-file "~/.emacs.d/temp-files/fontaine-latest-state.eld")
 '(forge-database-file "~/.emacs.d/temp-files/forge-database.sqlite")
 '(helm-adaptive-history-file "~/.emacs.d/temp-files/helm-adaptive-history")
 '(helm-always-two-windows nil)
 '(helm-apropos-show-short-doc t)
 '(helm-buffers-fuzzy-matching t)
 '(helm-commands-using-frame '(helm-apropos))
 '(helm-completion-style 'emacs)
 '(helm-ff-auto-update-initial-value t)
 '(helm-ff-file-name-history-use-recentf t)
 '(helm-imenu-use-icon t)
 '(helm-locate-recursive-dirs-command "fd --hidden --type d .*%s.*$ %s")
 '(helm-mini-default-sources
   '(helm-source-buffers-list helm-source-recentf helm-source-bookmarks
                              helm-source-buffer-not-found
                              helm-source-files-in-all-dired))
 '(helm-reuse-last-window-split-state nil)
 '(helm-use-frame-when-more-than-two-windows t)
 '(helm-use-frame-when-no-suitable-window t)
 '(hl-sexp-background-color "#060404")
 '(ignored-local-variable-values
   '((eval
      (lambda nil
        (defun cider-jack-in-wrapper-function (orig-fun &rest args)
          (if (and (boundp 'use-bb-dev) use-bb-dev)
              (message
               "Use `bb dev` to start the development server, then `cider-connect` to the port it specifies.")
            (apply orig-fun args)))
        (advice-add 'cider-jack-in :around
                    #'cider-jack-in-wrapper-function)
        (when (not (featurep 'clerk))
          (let
              ((init-file-path
                (expand-file-name "clerk.el" default-directory)))
            (when (file-exists-p init-file-path)
              (load init-file-path) (require 'clerk))))))
     (use-bb-dev . t) (prettify-symbols-mode)))
 '(mc/list-file "~/.emacs.d/temp-files/mc-lists.el")
 '(mml-secure-openpgp-sign-with-sender t)
 '(org-clock-persist-file "~/.emacs.d/temp-files/org-clock-save.el")
 '(org-hugo-preserve-filling nil)
 '(org-id-locations-file "~/.emacs.d/temp-files/.org-id-locations")
 '(org-super-agenda-mode t)
 '(package-selected-packages nil)
 '(pdf-annot-default-annotation-properties
   '((t (label . "Vedang Manerikar") (color . "light green"))
     (text (color . "#ff0000") (icon . "Note"))
     (highlight (color . "dark cyan")) (underline (color . "blue"))
     (squiggly (color . "orange")) (strike-out (color . "red"))))
 '(pdf-annot-list-listed-types
   '(file free-text highlight squiggly strike-out text underline))
 '(pdf-annot-minor-mode-map-prefix [3 1])
 '(project-list-file "~/.emacs.d/temp-files/projects")
 '(recentf-exclude
   '("/\\(\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|MERGEREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\\|\\(BRANCH\\|EDIT\\)_DESCRIPTION\\)\\'"
     "~/.emacs.d/temp-files/*"))
 '(recentf-save-file "~/.emacs.d/temp-files/recentf")
 '(safe-local-variable-values
   '((denote-directory .
                       "/Users/nejo/Tresors/Documents/salher-content/docs")
     (denote-file-type . org) (denote-file-type . markdown-yaml)
     (denote-file-type quote markdown-yaml) (system-time-locale . "C")
     (org-taskjuggler-default-global-properties .
                                                "shift s40 \"Working Shift\" {\12   workinghours sat, sun off\12}\12leaves holiday \"New Year\" 2022-01-01\12flags hsc_z, hsc_a\12")
     (org-taskjuggler-default-global-properties .
                                                "shift s40 \"Working Shift\" {\12   workinghours sat, sun off\12}\12leaves holiday \"New Year\" 2020-01-01\12flags hsc_z, hsc_a\12")
     (org-duration-units ("min" . 1) ("h" . 60) ("d" . 480)
                         ("w" . 2400) ("m" . 9600) ("y" . 96000))
     (org-taskjuggler-keep-project-as-task . t)
     (org-taskjuggler-target-version . 3.6)
     (org-taskjuggler-default-global-properties .
                                                "shift s40 \"Working Shift\" {\12   workinghours sat, sun off\12}\12leaves holiday \"New Year\" 2020-01-01\12")
     (checkdoc-package-keywords-flag)
     (eval font-lock-add-keywords nil
           `(
             (,(concat "("
                       (regexp-opt
                        '("sp-do-move-op" "sp-do-move-cl"
                          "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op"
                          "sp-do-del-cl")
                        t)
                       "\\_>")
              1 'font-lock-variable-name-face)))
     (eval define-clojure-indent (clj-action 'defun)
           (implement-action 4))
     (eval define-clojure-indent (on-one-node 1))
     (eval define-clojure-indent (with-action-values 1)
           (with-service-restart 1))
     (eval define-clojure-indent (plan-when 1) (plan-when-not 1))
     (eval ignore-errors
           "Write-contents-functions is a buffer-local alternative to before-save-hook"
           (add-hook 'write-contents-functions
                     (lambda nil (delete-trailing-whitespace) nil))
           (require 'whitespace)
           "Sometimes the mode needs to be toggled off and on."
           (whitespace-mode 0) (whitespace-mode 1))
     (whitespace-line-column . 80)
     (whitespace-style face trailing lines-tail)
     (require-final-newline . t)
     (eval define-clojure-indent (phase-context 2) (defmethod-plan 2))
     (eval define-clojure-indent (def-collect-plan-fn 'defun))
     (eval define-clojure-indent (defplan 'defun)
           (def-aggregate-plan-fn 'defun))
     (eval define-clojure-indent (cluster-spec 1) (group-spec 1))
     (eval define-clojure-indent (facts 'defun) (fact-group 'defun))
     (lexical-binding . t)))
 '(save-place-file "~/.emacs.d/temp-files/saveplace")
 '(savehist-file "~/.emacs.d/temp-files/savehist"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-dim-other-buffers-face ((t (:background "#2E3440")))))
