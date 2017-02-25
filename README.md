# mdviewer

__mdviewer__ is a minimalistic GTK+ Markdown viewer/converter written in
Haskell.  It supports using CSS stylesheets and exporting to Html embedding
them into a single autocontained Html file. 

<center><img src="http://i.imgur.com/X7O6OCW.gif"></center>

## Installation

At the time, the only way to install __mdviewer__ is compiling it from source
using Stack.

### Prerequisites

* [The Haskell Tool Stack](https://www.haskellstack.org/) 
* WebKit GTK+ development library:
   + On Ubuntu/Debian: [`libwebkitgtk-3.0-dev`](http://packages.ubuntu.com/precise/libwebkitgtk-3.0-dev)
   + On Arch: [`webkitgtk`](https://www.archlinux.org/packages/extra/x86_64/webkitgtk/)

### Compiling from source

```
git clone https://github.com/agustinmista/mdviewer
cd mdviever

stack setup
stack install gtk2hs-buildtools 
stack install
```

The installation will usually copy the executable to `$HOME/.local/bin`, so is
important that you have append it your PATH. Other data files such as the
styles repository are copied into your Xdg data dir, this is usualy
`$HOME/.local/share/mdviever`

## Usage

__mdviewer__ currently supports three subcommands

### _show_

`mdviewer show [input] [-s  STYLE]` 

This subcommand launches an interactive WebKit based window where you can use
the supported key-bindings to open files, change styles, navigate links and so
on. If you run `mdviewer show` without specifiying an input file, a welcome
file is shown.

#### Suported interactive key-bindings

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

This subcommand converts an Markdown file into a Html file which optionally
embeds an CSS stylesheet. If no output file is specified then the output is
saved to input.html.

### _list_

`mdviewer list`

This subcommand lists the CSS stylesheets currently available at the styles
repository, you can choose any of them using the `-s/--style` flag and the
style name when calling to `show/convert` subcommands. If you want to use an
external CSS stylesheet instead, just use the `-s/--style` flag providing the
path to it and __mdviewer__ will add it to the built-in stylesheets repository
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
-------------------------------------------------------------------------------

Please, if you find this software useful don't doubt to give any feedback. PRs
are welcome!
