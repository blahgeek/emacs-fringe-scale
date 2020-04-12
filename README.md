## fringe-scale

Emacs fringes are usually small in HiDPI screen because they are defined as bitmaps in fixed width (see below).

![](./images/before.png)

This package scales them (see below).

![](./images/after.png)


### Usage

```
(set-fringe-mode 16)

(require 'fringe-scale)
(fringe-scale-setup)
```

See the document for more details.
