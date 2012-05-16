emacs-spork
===========

Run your tests from emacs with spork

Run tests through spork from your emacs buffers.

    test-file  ; Interactive function that prompts for a test file

    test-current-file ; Run the file in the current buffer

Uses `testdrb` to contact spork.

Requirements
===========

1. Spork server, running
2. testdrb available from your shell. `gem install spork` should do this. Test it out by running `ansi-term` and trying `testdrb`.


Installation
============

1. Download the emacs-spork/ folder somewhere
2. add the path to the folder to your load-path and require it

Example config in .emacs

    (add-to-list 'load-path (expand-file-name "/path/to/emacs-spork"))
    (require 'emacs-spork)
