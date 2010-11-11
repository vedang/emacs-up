;; Settings for ERC
; ==============================================================================

(require 'erc-log)
(require 'erc-notify)
(require 'erc-spelling)
(require 'erc-autoaway)

; (setq erc-modules '(autoaway autojoin button completion fill irccontrols
;                              match menu netsplit noncommands readonly ring
;                              scrolltobottom services stamp track))

;; Autojoin these channels on connecting to freenode
(setq erc-autojoin-channels-alist '(("freenode.net"
                                     "#emacs" "##aawra")))

;; Settings for storing ERC logs
(setq erc-log-channels-directory "~/.erc/logs/")
(setq erc-save-buffer-on-part nil
      erc-save-queries-on-quit nil
      erc-log-write-after-send t
      erc-log-write-after-insert t)

(setq erc-auto-discard-away t)
(setq erc-autoaway-idle-seconds 600)
(setq erc-autoaway-message "[Autoaway] Out of mind, BRB")
;; autoaway when ERC is idle, not emacs itself
(setq erc-auto-set-away t)
;; Show relevant notifications only
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK" "MODE"
                      "324" "328" "329" "332" "333" "353" "477"))

(defface erc-header-line-disconnected
  '((t (:foreground "black" :background "indianred")))
  "Face to use when ERC has been disconnected.")

(defun erc-update-header-line-show-disconnected ()
  "Use a different face in the header-line when disconnected."
  (erc-with-server-buffer
   (cond ((erc-server-process-alive) 'erc-header-line)
         (t 'erc-header-line-disconnected))))
(setq erc-header-line-face-method 'erc-update-header-line-show-disconnected)

(setq erc-header-line-face-method 'erc-update-header-line-show-disconnected)

;; Color code different buddies
(setq erc-keywords '((".*Online.*" (:foreground "green"))
                     (".*Busy" (:foreground "red"))
                     (".*Away" (:foreground "red"))
                     (".*Idle" (:foreground "orange"))
                     ))

(provide 'erc-mode-config)
