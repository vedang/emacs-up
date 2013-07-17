(require 'cedet)
(require 'android-mode)


(setq android-mode-sdk-dir "/Users/vedang/Documents/bin/adt-bundle-mac-x86_64/sdk/")

(defun android-man (dir)
  "Look for AndroidManifest.xml file to find project dir of android application."
  (message dir)
  (locate-dominating-file dir "AndroidManifest.xml"))

(defmacro android-in-dir (dir body)
  "Execute BODY form with project dir directory as
dir directory can be found."
  `(let ((android-root (android-man ,dir)))
     (message "Running android in root dir %s" android-root)
     (when android-root
       (let ((default-directory android-root))
         ,body))))

(defun build-helpshift ()
  (interactive)
  (android-in-dir "/Users/vedang/Work/r2d2/QotdApp/"
                  (compile "ant -e debug"))
  (while compilation-in-progress (sleep-for 5))
  (android-in-dir "/Users/vedang/Work/r2d2/QotdApp/"
                  (compile "ant -e installd"))
  (while compilation-in-progress (sleep-for 5))
  (compile (concat android-mode-sdk-dir "platform-tools/adb shell am start -n com.example.qotdapp/com.example.qotdapp.MainActivity"))
  (while compilation-in-progress (sleep-for 5))
  (compile "growlnotify -m 'Android install done'"))

(eval-after-load "android-mode"
 '(progn
    (define-key android-mode-map
      (kbd "C-c C-c v")
      'build-helpshift)
    (android-defun-ant-task "jar")
    (define-key android-mode-map
      (kbd "C-c C-c j")
      'android-ant-jar)))

(defun java-setup ()
  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92)
    indent-tabs-mode nil
    tab-width 4
    fill-column 96
    c-comment-start-regexp "\\(@\\|/\\(/\\|[*][*]?\\)\\)"))

(add-hook 'android-mode-hook 'java-setup)

(provide 'android-config)
