;; Settings for ERC
; ==============================================================================

(require 'erc-log)
(require 'erc-notify)
(require 'erc-spelling)
(require 'erc-autoaway)

(setq erc-autojoin-channels-alist '(("freenode.net"
                                     "#emacs" "#clojure")
                                    ("qotdinc.com"
                                     "#dev" "#bakvaas"))
      erc-save-buffer-on-part nil
      erc-save-queries-on-quit nil
      erc-log-write-after-send t
      erc-log-write-after-insert t
      erc-log-channels-directory "~/.erc/logs/"
      erc-auto-discard-away t
      erc-autoaway-idle-seconds 600
      erc-autoaway-message "[Autoaway] Out of mind, BRB"
      erc-auto-set-away t
      erc-hide-list '("JOIN" "PART" "QUIT" "NICK" "MODE"
                      "324" "328" "329" "332" "333" "353" "477")
      erc-keywords '((".*Online.*" (:foreground "green"))
                     (".*Busy" (:foreground "red"))
                     (".*Away" (:foreground "red"))
                     (".*Idle" (:foreground "orange"))
                     ))


(add-hook 'erc-mode-hook 'turn-on-flyspell)


(defface erc-header-line-disconnected
  '((t (:foreground "black" :background "indianred")))
  "Face to use when ERC has been disconnected.")

(defun erc-update-header-line-show-disconnected ()
  "Use a different face in the header-line when disconnected."
  (erc-with-server-buffer
   (cond ((erc-server-process-alive) 'erc-header-line)
         (t 'erc-header-line-disconnected))))

(setq erc-header-line-face-method 'erc-update-header-line-show-disconnected)


;;; awesome doctoring :D
(setq erc-remove-parsed-property nil)

(autoload 'doctor-doc "doctor")
(autoload 'make-doctor-variables "doctor")

(defvar erc-doctor-id "- ")
(defun erc-cmd-DOCTOR (&optional last-sender &rest ignore)
  "Get the last message in the channel and doctor it."
  (let ((limit (- (point) 1000))
        (pos (point))
        doctor-buffer
        last-message
        text)
    ;; Make sure limit is not negative
    (when (< limit 0) (setq limit 0))
    ;; Search backwards for text from someone
    (while (and pos (not (let ((data (get-text-property pos 'erc-parsed)))
                           (and data
                                (string= (aref data 3) "PRIVMSG")
                                (or (not last-sender)
                                    (string= (car (split-string (aref data 2) "!"))
                                             last-sender))))))
      (setq pos (previous-single-property-change
                 pos 'erc-parsed nil limit))
      (when (= pos limit)
        (error "No appropriate previous message to doctor")))
    (when pos
      (setq last-sender (car (split-string
                              (aref (get-text-property
                                     pos 'erc-parsed) 2) "!"))
            doctor-buffer (concat "*ERC Doctor: " last-sender "*")
            last-message (split-string
                          ;; Remove punctuation from end of sentence
                          (replace-regexp-in-string
                           "[ .?!;,/]+$" ""
                           (aref (get-text-property pos
                                                    'erc-parsed) 5)))
            text (mapcar (lambda (s)
                           (intern (downcase s)))
                         ;; Remove salutation if it exists
                         (if (string-match
                              (concat "^" erc-valid-nick-regexp
                                      "[:,]*$\\|[:,]+$")
                              (car last-message))
                             (cdr last-message)
                           last-message))))
    (erc-send-message
     (concat erc-doctor-id
             ;; Only display sender if not in a query buffer
             (if (not (erc-query-buffer-p))
                 (concat last-sender ": "))
             (save-excursion
               (if (get-buffer doctor-buffer)
                   (set-buffer doctor-buffer)
                 (set-buffer (get-buffer-create doctor-buffer))
                 (make-doctor-variables))
               (erase-buffer)
               (doctor-doc text)
               (buffer-string))))))


(provide 'erc-mode-config)
