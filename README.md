Emacs-Up is an extensible, easy to maintain emacs config for your coding needs.

***
- [Things that are built-in](#things-that-are-built-in)
- [Things that are in the pipeline](#things-that-are-in-the-pipeline)
- [Pre-requisites](#pre-requisites)
- [Installation Instructions](#installation-instructions)
- [Caveats](#caveats)
- [Upgrade Instructions](#upgrade-instructions)

## Things that are built-in

1. Working and well-tuned configurations for:
   - Clojure Programming (CIDER + other minor modes)
   - Emacs Lisp Programming (ElSpice + other minor modes)
   - Note-taking and Task-tracking through Org Mode
   - Python Programming (ElPy + other minor modes)
   - Go Programming (Official Modes provided by the Golang Team)
   - Rust Programming
   - Scheme Programming (Geiser + other minor modes)
   - Email through Emacs (Notmuch + mbsync + msmtp)

2. Other minor but important additions:
   - Avy Mode (Jumping and Navigation)
   - Company Mode (Completion)
   - Ido Mode in high gear (Completion and Navigation)
   - Magit (Git Interface)
   - Multiple Cursors (Editing)
   - Paredit (Editing Lispy things)
   - YASnippets (Templating and Boiler Plate)
   - Correct path manipulation on OS X (System)

3. Better defaults than "pure" Emacs.

## Things that are in the pipeline

1. Working and well-tuned configurations for:
   - Java Mode
   - Erlang Mode

## Pre-requisites

The following tools should be installed and available on the system:
- [git](http://git-scm.com/)
- [mercurial](http://mercurial.selenic.com/)
- [subversion](https://subversion.apache.org/)
- [aspell](http://aspell.net/)
- [automake](https://www.gnu.org/software/automake/)
- [Texinfo](https://www.gnu.org/software/texinfo/)
- [The Silver Searcher](https://github.com/ggreer/the_silver_searcher)

Make sure that they are on $PATH. If you are on a Mac, `git` needs to
be available at `/usr/local/bin/git`.

1. On a Mac, these can all be installed through Homebrew as follows:
   <code>
   $ brew install git mercurial aspell automake texinfo subversion the_silver_searcher
   </code>

2. On Ubuntu, these can all be installed through Apt as follows:
   <code>
   $ apt install git mercurial aspell automake texinfo subversion silversearcher_ag
   </code>


## Installation Instructions

1. Clone the repository and move it to your `.emacs.d` folder

     <pre>
     $ cd /tmp/
     $ git clone https://github.com/vedang/emacs-up.git
     $ mv emacs-up ~/.emacs.d
     </pre>

2. Start Emacs. Make yourself a cup of tea.

   The first boot will trigger a (one-time) download of all the
   packages that Emacs-Up needs. This can take a lot of time.

   Sometimes (rarely) Emacs will stop and throw an error. If this
   happens, try re-starting Emacs. If the error is still being thrown,
   file an issue with me. Don't forget to include the stacktrace.

   Don't worry, your perfect environment is being baked with love.

3. Follow the instructions in the [Caveats](#caveats) section to install
   third party software required for various modes.

4. ...

5. Profit!


## Caveats

1. Org Mode:

   For org-mode to work, you'll have to set your `org-directory`. To
   do this, add the line

   <code>
   (setq org-directory "/path/to/org/directory")
   </code>

   somewhere in your Emacs config. I recommend that you create the
   file `~/.emacs.d/personal.el` and add the code to that
   file. Evaluate the line by pressing `C-x C-e` at the end of it.

   For more information about the org config, take a look at
   [vedang/org-mode-crate](https://github.com/vedang/org-mode-crate)

2. Clojure interactive development through Cider

   [cider](https://github.com/clojure-emacs/cider/tree/v0.11.0) needs
   a one time setup of
   [cider-nrepl](https://github.com/clojure-emacs/cider-nrepl/tree/v0.11.0),
   [refactor-nrepl](https://github.com/clojure-emacs/refactor-nrepl/tree/v2.0.0)
   and
   [squigly-clojure](https://github.com/clojure-emacs/squiggly-clojure)
   middleware in order to work properly. Please follow the
   instructions
   [here](https://github.com/clojure-emacs/cider/tree/v0.11.0#setting-up-a-standalone-repl)
   to install `cider-nrepl` (curr ver v0.11.0). Please follow the
   instructions
   [here](https://github.com/clojure-emacs/clj-refactor.el/tree/2.0.0#setup)
   to install `refactor-nrepl` (curr ver v2.0.0). Please follow the
   instructions
   [here](https://github.com/clojure-emacs/squiggly-clojure#dependencies-in-clojure)
   to install `squigly-clojure` (curr ver v0.1.5).

3. Python interactive development through Elpy

   [Elpy](https://github.com/jorgenschaefer/elpy/) needs a one-time
   setup of Python packages. Please follow the installation
   instructions on the Elpy Page.

4. Golang development

   Golang integration with Emacs is a breeze, as long as you have set
   the GOPATH correctly, as described
   [here](http://golang.org/doc/code.html). By default, emacs
   auto-completion will not show golang builtins as options. If you
   want to enable this behaviour, run the following code:

   <code>
   $ gocode set propose-builtins true
   </code>

   If you do not wish to install Golang specific recipes, go to
   `features.el` in the main directory and set the value of
   `configure-go-p` to `nil`.

5. Rust development

   Rust integration is simplistic - depends on `rust-mode` for all
   functionality - and only requires the presence of the rustlang
   compiler. Refer [this
   document](https://www.rust-lang.org/learn/get-started) for
   instructions. If you do not wish to install Rust specific recipes,
   go to `features.el` in the main directory and set the value of
   `configure-rust-p` to `nil`.

6. Scheme development

   Scheme integration is provided or Chicken Scheme, and interactive
   development is implemented through Geiser. Refer
   [this document](http://code.call-cc.org/) for instructions to
   install Chicken Scheme. On a Mac, it can be easily installed
   through Homebrew using:

   <code>
   $ brew install chicken
   </code>

   If you do not wish to install Scheme specific recipes, go to
   `features.el` in the main directory and set the value of
   `configure-scheme-p` to `nil`.

## Upgrade Instructions

1. Close running `emacs` session
2. Fetch the latest changes from `vedang/emacs-up`
3. Start `emacs` and run the following code:
   <pre>
   M-x el-get-self-update
   M-x el-get-update-all
   </pre>
4. Restart `emacs`
