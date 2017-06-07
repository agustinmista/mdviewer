# mdviewer
[![CircleCI](https://circleci.com/gh/agustinmista/mdviewer.svg?style=svg)](https://circleci.com/gh/agustinmista/mdviewer)

__mdviewer__ is a minimalistic GTK+ Markdown viewer/converter written in
Haskell. It supports using Css stylesheets both from a built-in styles
repository or from user provided Css files. It is also possible to export files
to Html embedding the selected style into a single self-contained file. 

<center><img src="http://i.imgur.com/X7O6OCW.gif"></center>

## Installation

At the time, __mdviewer__ can be installed both compiling it from source
using Stack, or downloading a precompiled binary provided by CircleCI

### Prerequisites

* WebKit GTK+ development library:
   + On Ubuntu/Debian: [`libwebkitgtk-3.0-dev`](http://packages.ubuntu.com/precise/libwebkitgtk-3.0-dev)
   + On Arch: [`webkitgtk`](https://www.archlinux.org/packages/extra/x86_64/webkitgtk/)

### Compiling from source using [Stack](https://www.haskellstack.org/)
```
git clone https://github.com/agustinmista/mdviewer
cd mdviewer

stack setup
stack install gtk2hs-buildtools 
stack install
```

The installation will usually copy the executable to `$HOME/.local/bin`, so it
is important that you have appended it to your PATH. Other data files such as
the styles repository are copied into your Xdg data folder. This is usually
`$HOME/.local/share/mdviewer`.

### Downloading a precompiled binary
This process is automated by the `install.sh` bash script which downloads 
a precompiled binary from the latest CircleCI build, and the data files from
this GitHub repository using Subversion. Make sure you have installed _wget_
and _svn_ before running the installer.

```
wget https://raw.githubusercontent.com/agustinmista/mdviewer/master/install.sh
chmod +x install.sh
./install.sh
```

This method will copy the executable to `$HOME/.local/bin` and the data files 
to `$HOME/.local/share/mdviewer`.

## Usage

__mdviewer__ currently supports three subcommands:

### _show_

`mdviewer show [input] [-s  STYLE]` 

This subcommand launches an interactive WebKit based window where you can use
the supported key-bindings to open files, change styles, navigate following
links and so on. If you run `mdviewer show` without specifying an input file,
a welcome file is shown.

#### Interactive controls

The current key-bindings mapping intends to provide a _vim_-like user
experience:

| key |  description          |
|:---:|:----------------------|
| `e` | open file             |
| `w` | save as Html          |
| `r` | reload preview        |
| `n` | set next style        |
| `N` | set previous style    |
| `g` | go to page top        |
| `G` | go to page bottom     |
| `z` | go to previous page   |
| `x` | go to following page  |
| `q` | quit                  |

### _convert_ 

`mdviewer convert input [-o  OUTPUT] [-s  STYLE]`

This subcommand converts a Markdown file into an Html file which optionally
embeds an Css stylesheet. If no output file is specified, then the output is
saved to _input_.html.

### _list_

`mdviewer list`

This subcommand lists the Css stylesheets currently available at the styles
repository; you can choose any of them by using the `-s/--style` flag followed
by a style name when calling to `show/convert` subcommands. If you want to use
an external Css stylesheet instead, just use the `-s/--style` flag providing
the path to it and __mdviewer__ will add it to the built-in styles repository
for future usage automatically. 

```
Available styles:
  * air
  * foghorn
  * ghostwriter
  * github
  * github-dark
  * godspeed
  * manuscript
  * markdown
  * ...
``` 

## Acknowledgments
I would like to say thanks to some of the authors of the Css stylesheets built-in into
__mdviewer__:
* [John Otander](https://github.com/johnotander)
* [Jason Milkins](https://github.com/jasonm23)
* [Jocelyn Richard](https://github.com/nWODT-Cobalt)

-------------------------------------------------------------------------------
Please, if you find this software useful don't doubt to give any feedback. PRs
are welcome!
