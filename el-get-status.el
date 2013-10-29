((ac-nrepl status "installed" recipe
           (:name ac-nrepl :description "Cider completion source for Emacs auto-complete package" :type github :pkgname "clojure-emacs/ac-nrepl" :depends
                  (auto-complete cider)
                  :features ac-nrepl))
 (ace-jump-mode status "installed" recipe
                (:name ace-jump-mode :website "https://github.com/winterTTr/ace-jump-mode/wiki" :description "A quick cursor location minor mode for emacs" :type github :pkgname "winterTTr/ace-jump-mode" :features ace-jump-mode))
 (auto-complete status "installed" recipe
                (:name auto-complete :website "https://github.com/auto-complete/auto-complete" :description "The most intelligent auto-completion extension." :type github :pkgname "auto-complete/auto-complete" :depends
                       (popup fuzzy)))
 (cider status "installed" recipe
        (:name cider :description "CIDER is a Clojure IDE and REPL." :type github :pkgname "clojure-emacs/cider" :depends
               (dash clojure-mode pkg-info)))
 (cl-lib status "installed" recipe
         (:name cl-lib :builtin "24.3" :type elpa :description "Properly prefixed CL functions and macros" :url "http://elpa.gnu.org/packages/cl-lib.html"))
 (clojure-mode status "installed" recipe
               (:name clojure-mode :website "https://github.com/clojure-emacs/clojure-mode" :description "Emacs support for the Clojure language." :type github :pkgname "clojure-emacs/clojure-mode"))
 (color-theme-zenburn status "installed" recipe
                      (:name color-theme-zenburn :description "Just some alien fruit salad to keep you in the zone" :website "https://github.com/bbatsov/zenburn-emacs" :type github :pkgname "bbatsov/zenburn-emacs" :post-init
                             (add-to-list 'custom-theme-load-path default-directory)))
 (dash status "installed" recipe
       (:name dash :description "A modern list api for Emacs. No 'cl required." :type github :pkgname "magnars/dash.el"))
 (el-get status "installed" recipe
         (:name el-get :website "https://github.com/dimitri/el-get#readme" :description "Manage the external elisp bits and pieces you depend upon." :type github :branch "4.stable" :pkgname "dimitri/el-get" :info "." :load "el-get.el"))
 (el-spice status "installed" recipe
           (:name el-spice :description "Additional flavour for your Emacs Lisp programming" :type github :depends
                  (thingatpt+)
                  :pkgname "vedang/el-spice"))
 (emacs-eclim status "installed" recipe
              (:name emacs-eclim :description "The power of eclim inside Emacs" :type github :pkgname "senny/emacs-eclim"))
 (flymake-cursor status "installed" recipe
                 (:name flymake-cursor :type github :pkgname "illusori/emacs-flymake-cursor" :description "displays flymake error msg in minibuffer after delay (illusori/github)" :website "http://github.com/illusori/emacs-flymake-cursor"))
 (fuzzy status "installed" recipe
        (:name fuzzy :website "https://github.com/auto-complete/fuzzy-el" :description "Fuzzy matching utilities for GNU Emacs" :type github :pkgname "auto-complete/fuzzy-el"))
 (git-modes status "installed" recipe
            (:name git-modes :description "GNU Emacs modes for various Git-related files" :type github :pkgname "magit/git-modes"))
 (ibuffer-vc status "installed" recipe
             (:name ibuffer-vc :description "Group ibuffer's list by VC project, or show VC status" :type github :pkgname "purcell/ibuffer-vc"))
 (isearch+ status "installed" recipe
           (:name isearch+ :auto-generated t :type emacswiki :description "Extensions to `isearch.el' (incremental search)." :website "https://raw.github.com/emacsmirror/emacswiki.org/master/isearch+.el"))
 (isearch-prop status "installed" recipe
               (:name isearch-prop :auto-generated t :type emacswiki :description "Search text-property or overlay-property contexts." :website "https://raw.github.com/emacsmirror/emacswiki.org/master/isearch-prop.el"))
 (linkd status "installed" recipe
        (:name linkd :type http-tar :description "Make hypertext with active links in any buffer" :options
               ("xzvf")
               :url "http://www.emacswiki.org/emacs/download/linkd.tar.gz"))
 (magit status "installed" recipe
        (:name magit :website "https://github.com/magit/magit#readme" :description "It's Magit! An Emacs mode for Git." :type github :pkgname "magit/magit" :depends
               (cl-lib git-modes)
               :info "." :build
               (if
                   (version<= "24.3" emacs-version)
                   `(("make" ,(format "EMACS=%s" el-get-emacs)
                      "all"))
                 `(("make" ,(format "EMACS=%s" el-get-emacs)
                    "docs")))
               :build/berkeley-unix
               (("touch" "`find . -name Makefile`")
                ("gmake"))))
 (markdown-mode status "installed" recipe
                (:name markdown-mode :description "Major mode to edit Markdown files in Emacs" :website "http://jblevins.org/projects/markdown-mode/" :type git :url "git://jblevins.org/git/markdown-mode.git" :before
                       (add-to-list 'auto-mode-alist
                                    '("\\.\\(md\\|mdown\\|markdown\\)\\'" . markdown-mode))))
 (multiple-cursors status "installed" recipe
                   (:name multiple-cursors :description "An experiment in adding multiple cursors to emacs" :type github :pkgname "magnars/multiple-cursors.el" :features multiple-cursors))
 (org-mode-crate status "installed" recipe
                 (:name org-mode-crate :description "A pre-defined org environment for the consummate gtd'er" :type github :branch "reboot" :pkgname "vedang/org-mode-crate"))
 (pkg-info status "installed" recipe
           (:name pkg-info :description "Provide information about Emacs packages." :type github :branch "0.2" :pkgname "lunaryorn/pkg-info.el" :depends
                  (s dash)))
 (popup status "installed" recipe
        (:name popup :website "https://github.com/auto-complete/popup-el" :description "Visual Popup Interface Library for Emacs" :type github :pkgname "auto-complete/popup-el"))
 (s status "installed" recipe
    (:name s :description "The long lost Emacs string manipulation library." :type github :pkgname "magnars/s.el" :features s))
 (smart-tab status "installed" recipe
            (:name smart-tab :description "Intelligent tab completion and indentation." :type github :pkgname "genehack/smart-tab" :features smart-tab))
 (smartparens status "installed" recipe
              (:name smartparens :description "Autoinsert pairs of defined brackets and wrap regions" :type github :pkgname "Fuco1/smartparens" :depends dash))
 (smex status "installed" recipe
       (:name smex :description "M-x interface with Ido-style fuzzy matching." :type github :pkgname "nonsequitur/smex" :features smex :post-init
              (smex-initialize)))
 (thingatpt+ status "installed" recipe
             (:name thingatpt+ :auto-generated t :type emacswiki :description "Extensions to `thingatpt.el'." :website "https://raw.github.com/emacsmirror/emacswiki.org/master/thingatpt+.el"))
 (wgrep status "installed" recipe
        (:name wgrep :description "Writable grep buffer and apply the changes to files" :type github :pkgname "mhayashi1120/Emacs-wgrep"))
 (writegood status "installed" recipe
            (:name writegood :type github :description "Polish up poor writing on the fly." :pkgname "bnbeckwith/writegood-mode")))
