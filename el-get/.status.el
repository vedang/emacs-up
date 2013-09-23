((el-get status "installed" recipe
	 (:name el-get :website "https://github.com/dimitri/el-get#readme" :description "Manage the external elisp bits and pieces you depend upon." :type github :branch "4.stable" :pkgname "dimitri/el-get" :info "." :load "el-get.el"))
 (smart-tab status "installed" recipe
	    (:name smart-tab :description "Intelligent tab completion and indentation." :type github :pkgname "genehack/smart-tab" :features smart-tab)))
