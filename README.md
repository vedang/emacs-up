# Emacs-Up
An extensible, easy to maintain emacs config for your coding needs.

## Things that are built-in

1. Working and well-tuned configurations for:
   - Clojure Mode
   - Emacs Lisp Mode
   - Org Mode
   - Python Mode
   - Go Mode


2. Other minor but important additions:
   - Ace Jump Mode
   - Auto Complete Mode
   - Ido Mode in high gear
   - Magit
   - Multiple Cursors
   - Smartparens
   - YASnippets

3. Better defaults than "pure" Emacs.

## Things that are in the pipeline

1. Working and well-tuned configurations for:
   - Java Mode
   - Erlang Mode

2. Email through Emacs

## Pre-requisites ##

The following tools should be installed and available on the system:
- [git](http://git-scm.com/)
- [mercurial](http://mercurial.selenic.com/)
- [bazaar](http://bazaar.canonical.com/en/)

Make sure that they are on $PATH. If you are on a Mac, `git` needs to
be available at `/usr/local/bin/git`

## Installation Instructions

1. Clone the repository and move it to your `.emacs.d` folder

     <pre>
     $ cd /tmp/
     $ git clone https://github.com/vedang/emacs-up.git
     $ mv emacs-up ~/.emacs.d
     </pre>

2. Start Emacs. Make yourself a cup of tea.

   The first boot will trigger a (one-time) download of all the
   packages that Emacs-Up needs. This can take a lot of time. Don't
   worry, your perfect environment is being baked with love.

3. Follow the instructions in Caveats to install third party software
   required for various modes.

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

   [cider](https://github.com/clojure-emacs/cider) needs a one time
   setup of
   [cider-nrepl](https://github.com/clojure-emacs/cider-nrepl)
   middleware in order to work properly. Please follow the
   instructions
   [here](https://github.com/clojure-emacs/cider#cider-nrepl-middleware)
   to install `cider-nrepl` (ver v0.7.0).

3. Python interactive development through Elpy

   [Elpy](https://github.com/jorgenschaefer/elpy/) needs a one-time
   setup of the following packages: `ipython` and `rope`. You
   can install them as follows:

   <code>
   $ sudo pip install ipython rope
   </code>

4. Golang development

   Golang integration with Emacs is a breeze, as long as you have set
   the GOPATH correctly, as described
   [here](http://golang.org/doc/code.html). By default, emacs
   auto-completion will not show golang builtins as options. If you
   want to enable this behaviour, run the following code:

   <code>
   $ gocode set propose-builtins true
   </code>
