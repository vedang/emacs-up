# Emacs-Up
An extensible, easy to maintain emacs config for your coding needs.

## Things that are built-in

1. Working and well-tuned configurations for:
   - Clojure Mode
   - Emacs Lisp Mode
   - Org Mode

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
   - Go Mode
   - Java Mode
   - Python Mode
   - Erlang Mode

2. Email through Emacs


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

3. ...

4. Profit!


## Caveats

1. Org Mode:

   For org-mode to work, you'll have to set your `org-directory`. To
   do this, add the line

   <code>
   (setq org-directory "/path/to/org/directory")
   </code>

   at the end of the init.el file. Evaluate the line by pressing `C-x
   C-e` at the end of it.

   For more information about the org config, take a look at
   https://github.com/vedang/org-mode-crate
