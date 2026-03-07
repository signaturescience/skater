# Interpolate over segments

This is an unexported helper used in in
[ibd2kin](https://signaturescience.github.io/skater/reference/ibd2kin.md).
The function interpolates over segments to apply genetic length to the
segment. It is inspired by Python code distributed by the Browning lab
([documentation](http://faculty.washington.edu/sguy/ibd_relatedness.md)).

## Usage

``` r
interpolate(ibd_bp, chromgpos)
```

## Arguments

- ibd_bp:

  Base pair for the IBD segment over which to interpolate

- chromgpos:

  Genetic map data for a specific chromosome

## Value

Numeric vector with the genetic distance shared at the segment.

## References

http://faculty.washington.edu/sguy/ibd_relatedness.html
