# Create a stamp grob with independent placement and line justification

Create a stamp grob with independent placement and line justification

## Usage

``` r
stampGrob(caption, pos = 1, style = list(), hjust = NULL, gp = NULL)
```

## Arguments

- caption:

  The stamp text.

- pos:

  Placement of the text block (0=left, 1=right).

- style:

  element_text-like list (size, colour, hjust, family, face,
  lineheight). Ignored if gp given.

- hjust:

  Line justification within the block. Taken from style if NULL.

- gp:

  A gpar object. If NULL, derived from style.

## Value

A grob
