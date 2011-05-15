;;; Settings for Jabber

(require 'jabber-autoloads)

;;; Customization settings
(setq jabber-account-list (quote (("ved.manerikar@gmail.com" (:network-server . "talk.google.com") (:connection-type . ssl))
                                  ("vedang@infinitelybeta.com" (:network-server . "talk.google.com") (:connection-type . ssl))))
      jabber-alert-presence-hooks nil
      jabber-default-show "away"
      jabber-default-status "Reality continues to ruin my life..."
      jabber-vcard-avatars-retrieve nil
      jabber-roster-roll-up-group t
      jabber-roster-sort-functions (quote (jabber-roster-sort-by-group jabber-roster-sort-by-status jabber-roster-sort-by-displayname))
      jabber-show-resources nil)

;;; Roll up groups
(eval-after-load "jabber-roster"
  '(defun jabber-roster-ret-action-at-point (&optional other-window)
     "Fixed Action for ret - with prefix opens chat in other window. Before try to roll up/down group. Eval
chat-with-jid-at-point is no group at point"
     (interactive "P")
     (let ((group-at-point (get-text-property (point)
                                              'jabber-group))
           (account-at-point (get-text-property (point)
                                                'jabber-account)))
       (if (and group-at-point account-at-point)
           (jabber-roster-roll-group account-at-point group-at-point)
         (jabber-chat-with-jid-at-point other-window)))))

;;; Make roster more readable
(eval-after-load "jabber-roster"
  '(defun jabber-fix-status (status)
     "Make status strings more readable"
     (when status
       (when (string-match "\n+$" status)
         (setq status (replace-match "" t t status)))
       (when jabber-remove-newlines
         (while (string-match "\n" status)
           (setq status (replace-match " " t t status))))
       (if (> (length status) 10)
           (concat (substring status 0 9) " ...")
         status))))

(provide 'jabber-mode-config)
