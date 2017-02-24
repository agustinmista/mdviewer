# mdviewer

`mdviewer` is a minimalistic GTK+ Markdown viewer/converter supporting CSS
stylesheets embedding.

# ![](http://i.imgur.com/X7O6OCW.gif)

----------

## Installation

### Prerequisites

You must have installed [stack](https://www.haskellstack.org/), and
`libwebkitgtk-3.0-dev`

### Using Stack
```
git clone https://github.com/agustinmista/mdviewer
cd mdviever

stack setup
stack install gtk2hs-buildtools 
stack install

```

## Usage

`mdviewer` currently supports three subcommands:

### __show__
> Renderizes a Markdown file into a new Gtk+ window using WebKit

```
$ mdviewer show -h

mdviewer show
usage : mdviewer show input [-s  STYLE] [-h] [--version]

mandatory arguments:
 input                         Markdown input file

optional arguments:
 -s, --style  STYLE            Css style to embed
 -h, --help                    show this help message and exit
 --version                     print the program version and exit
```

#### Suported show keybindings

| key |  description          |
|:---:|:----------------------|
| `e` | open file             |
| `w` | save as Html          |
| `r` | reload preview        |
| `n` | set next style        |
| `N` | set previous style    |
| `z` | go to previous page   |
| `x` | go to following page  |
| `q` | quit                  |

### __convert__
> Converts a Markdown file into an Html file (posibly) embedding a CSS style

```
$ mdviewer convert -h

mdviewer convert
usage : mdviewer convert input [-o  OUTPUT] [-s  STYLE] [-h] [--version]

mandatory arguments:
 input                         Markdown input file

optional arguments:
 -o, --output  OUTPUT          Output Html file
 -s, --style  STYLE            Css style to embed
 -h, --help                    show this help message and exit
 --version                     print the program version and exit
```


### __list__
> List the installed Css styles

```
$ mdviewer list

Available styles:
 * air
 * foghorn
 * ghostwriter
 * github
 * github-dark
 * ...
```

