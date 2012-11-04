emacs-spork
===========

Run your tests from emacs with spork

The emacs-spork extension lets you send tests to existing emacs buffers or tmux panes. Instead of relying on something like guard to automatically run tests when files change, I've found it more useful to be able to quickly run tests on demand. Attempts to find all tests related to the current file - handling functional and unit tests for rails models, controllers, and views.

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

Some suggested keybindings, though C-c C-r conflicts with one of the `inf-ruby` defaults.

    (global-set-key "\C-c\C-t" 'es-run-tests-for-current-file)
    (global-set-key "\C-c\C-r" 'es-redo-last-test)


Configuration
============

Run tests via tmux (default)
------------

While there is a nice simplicity in running your tests inside an emacs buffer, I've found the output formatting and general stability of a real terminal worth sticking with.  You don't need to set these variables, since it's the default, but here they are anyway:

    (setq es-use-tmux-pane t)
    (setq es-use-emacs-buffer nil)

If you'd like to pick which tmux session / window / pane the tests go to, configure the `es-tmux-target` variable.

`es-tmux-target` Needs to be of a format accepted by tmux send-panes -t <value>

You can find the right argument from tmux by using the list-panes function:

    $ tmux list-panes -as

    session_name:0.0: [118x65] [history 1/2000, 830 bytes] %11
    session_name:0.1: [118x65] [history 266/2000, 109035 bytes] %15 (active)
    session_name:1.0: [163x59] [history 5254/100000, 2171915 bytes] %14 (active)
    session_name:2.0: [171x52] [history 1943/2000, 937870 bytes] %10 (active)
    session_name:3.0: [257x58] [history 1976/2000, 686800 bytes] %9 (active)

In my case, I have one tmux session running, called session_name, with 4 windows. Windows 1-3 have only one pane in each, while Window 0 has two panes. To use the second pane in window 0, I would do

(setq es-tmux-target "session_name:0.1")

See `man tmux` for more information.

Run tests via ansi-term
----------

    (setq es-use-tmux-pane nil)
    (setq es-use-emacs-buffer t)

Set the `spork-test-buffer` variable if you want your test to run somewhere besides "*spork-tests*"

Usage
============

A big goal of this library is to *not* be a black box, but rather give you control over exactly what will run, and just make it fast to send a specific command over to spork.

Simplest case - send the current buffer to spork
----------

If you want no magic, just send the buffer you're currently in (must be a saved file) to spork.

    M-x es-run-current-file

This will run `testdrb /path/to/buffer`.

Re-run the most recent command
----------

Often you'll have a buffer open with your test file, run it, see it fail, then go and change some files to make it pass. To re-run the most recent command you sent to spork without switching back to the test file buffer, use:

    M-x es-redo-last-test

Run tests associated with the current file
----------

If you're willing to trust in a little magic, you can run all the unit and functional tests `emacs-spork` can find for the current file.  Uses pattern matching to look up the current model, supporting:

* Models: app/models/*singular_modelname*.rb
* Views: app/views/*plural_modelname*/anything.erb
* Controllers: app/controllers/*plural_modelname*controller.rb
* Tests: If you're in a buffer under the test/ path, just run the buffer you're currently in

The plural to singularization code is taken from (https://github.com/jimm/elisp/blob/master/emacs.el#L244). It's simple but seems to work in most cases.

So from a file associated with the model you want to test, run one of these:

    M-x es-run-tests-for-current-file  ; run both functional and unit tests
    M-x es-run-unit-test-for-current-file  ; run just unit test
    M-x es-run-functional-test-for-current-file  ; run just functional tes

Since `es-run-tests-for-current-file` is smart enough to run the current buffer if you're in the tests folder, it's pretty safe to just use that plus `es-redo-last-test` all the time.

Run a random command
----------

While I wrote the library to run tests, it's sometimes useful to just run a random command via your shell from inside emacs.

    M-x es-send-to-tmux [enter] <input command at prompt> [enter]
