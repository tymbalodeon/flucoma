# FluCoMa

Tutorials from [FluCoMa](https://www.flucoma.org/), reformatted and expanded.

## Formatting

### Chained methods

Chained methods that start on new lines should be indented one level more than
the object on which they are called.

#### Emacs

In order to auto-format chained methods correctly, [sclang-mode](https://github.com/supercollider/scel "sclang-mode")'s
`sclang-indent-line` function can be monkeypatched with [flucoma-sclang-indent.el](flucoma-sclang-indent.el "flucoma-sclang-indent.el")

Assuming you already have `sclang-mode` installed, add the following to your
emacs configuration:

```emacs-lisp
(add-to-list 'load-path "<path/to>/flucoma")
(require 'flucoma-sclang-indent)
```
