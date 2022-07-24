# nfac - Nondeterministic Finite Automata Creator
![GitHub-Mark-Light](https://github.com/E-A-Griffin/nfac/blob/master/NFA-Logo.png#gh-dark-mode-only)![GitHub-Mark-Dark](https://github.com/E-A-Griffin/nfac/blob/master/nfac-logo-dark.png#gh-light-mode-only)
<p align="center">
  <img width="389" height="155" src="https://github.com/E-A-Griffin/DFAC/blob/master/NFA-Logo.png">
</p>
Application built in Clojure using the Quil library for constructing Nondeterministic Finite Automata and testing input strings for membership in the language described by the user's automaton.

## Starting Application

LightTable - open `core.clj` and press `Ctrl+Shift+Enter` to evaluate the file.

Emacs - run cider, open `core.clj` and press `C-c C-k` to evaluate the file.

REPL - run `(require 'quil-test.core)`.

## Application Instructions

nfac is designed to allow a user to create new NFAs via the keyboard and mouse. 

### Controls

By default, clicking anywhere on the screen should create a new state. To define a transition between states `q_i -> q_j` on input `a`, type `t` then click inside the circle for `q_i` then click inside the circle for `q_j` then type `a`. In the special case that the transition is on `λ`/`ϵ`, after typing `t` and clicking `q_i` and `q_j`, hit `Enter`. To define an existing state `q_k` as accepting/final, type `f` then click on `q_k`.

Given an initially declared state `q_0`, nfac will automatically set `q_0` as the (unique) initial state. Support for changing initial state is not yet supported.

In the event that a command key (e.g. `t` for transition or `f` for final) is pressed but the user decides that they would prefer to create a new state instead, they can type `c` to declare a new state instead. This behavior applies generally (e.g. you could cancel defining a new transition and instead declare that a state is final by pressing `f`).

Two themes exist for nfac, by default the `light-mode` is enabled, to switch to the `dark-mode` type `n` (as in the equivalent name `night-mode`). Once in `dark-mode`, type `b` (as in the equivalent name `bright-mode`) to switch back to `light-mode`.

To test string membership on an existing NFA, type `a` (as in `alphabet`). You will be prompted to type in your string by a popup window. In order to exit this window, `Enter` needs to be pressed twice (once to process the current string and the second time to actually close the window).

nfac supports loading/saving existing NFAs to file. In order to save the current NFA, type `s`. A popup window will appear where the user will be prompted to enter a name for the file that should store the NFA. To load an existing NFA, type `l` and then use the up and down arrow keys to navigate existing NFAs by file name and press `Enter` to load a selected NFA.

## Known Issues

This software has been tested on a particular set of hardware, and tuned to process user clicks in a responsive way without duplicating clicks (as Quil is prone to doing by default), as such, you may find that on your system clicks are processed too slowly or too quickly. If this happens, feel free to open an issue for it and we can try to come up with a more portable solution.

Certain inputs such as `"` that require `Shift + '` on a conventional keyboard, are not processed properly at the moment and should be avoided as input until this issue is addressed.

## License

Copyright © 2016

Distributed under the Eclipse Public License version 1.0.

