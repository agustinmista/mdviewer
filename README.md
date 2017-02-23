# mdviewer

`mdviewer` is a minimalistic GTK+ Markdown viewer/converter supporting CSS
stylesheets embedding.

![](http://i.imgur.com/X7O6OCW.gif)

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
* __r__ - reload the file
* __s__ - set next style
* __a__ - set previous style
* __z__ - go to previous page
* __x__ - go to following page
* __o__ - opens another file with the current style
* __w__ - saves the current file with the current style
* __q__ - exits `mdviewer`


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

