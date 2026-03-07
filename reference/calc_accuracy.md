# Calculate Accuracy

Calculates accuracy and related metrics.

## Usage

``` r
calc_accuracy(tabble)
```

## Arguments

- tabble:

  A frequency table created with
  [`table`](https://rdrr.io/r/base/table.html)

## Value

A tibble with the corresponding statistics

## Details

Calculates accuracy, lower and upper bounds, the guessing rate and
p-value of the accuracy vs. the guessing rate. This function is called
by `confusion_matrix`, but if this is all you want, you can simply
supply the table to this function.

## See also

[`binom.test`](https://rdrr.io/r/stats/binom.test.html)

## Author

Michael Clark (see
[m-clark/confusion_matrix](https://github.com/m-clark/confusionMatrix)).
