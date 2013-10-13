((cl-lib status "installed" recipe
         (:name cl-lib :builtin "24.3" :type elpa :description "Properly prefixed CL functions and macros" :url "http://elpa.gnu.org/packages/cl-lib.html"))
 (color-theme-zenburn status "installed" recipe
                      (:name color-theme-zenburn :description "Just some alien fruit salad to keep you in the zone" :website "https://github.com/bbatsov/zenburn-emacs" :type github :pkgname "bbatsov/zenburn-emacs" :post-init
                             (add-to-list 'custom-theme-load-path default-directory)))
 (dash status "installed" recipe
       (:name dash :description "A modern list api for Emacs. No 'cl required." :type github :pkgname "magnars/dash.el"))
 (el-get status "installed" recipe
         (:name el-get :website "https://github.com/dimitri/el-get#readme" :description "Manage the external elisp bits and pieces you depend upon." :type github :branch "4.stable" :pkgname "dimitri/el-get" :info "." :load "el-get.el"))
 (git-modes status "installed" recipe
            (:name git-modes :description "GNU Emacs modes for various Git-related files" :type github :pkgname "magit/git-modes"))
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
 (s status "installed" recipe
    (:name s :description "The long lost Emacs string manipulation library." :type github :pkgname "magnars/s.el" :features s))
 (smart-tab status "installed" recipe
            (:name smart-tab :description "Intelligent tab completion and indentation." :type github :pkgname "genehack/smart-tab" :features smart-tab))
 (smartparens status "installed" recipe
              (:name smartparens :description "Autoinsert pairs of defined brackets and wrap regions" :type github :pkgname "Fuco1/smartparens" :depends dash))
 (writegood status "installed" recipe
            (:name writegood :type github :description "Polish up poor writing on the fly." :pkgname "bnbeckwith/writegood-mode")))
