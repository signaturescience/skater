# Calculate various statistics from a confusion matrix

Given a frequency table of predictions versus target values, calculate
numerous statistics of interest.

## Usage

``` r
calc_stats(tabble, prevalence = NULL, positive, ...)
```

## Arguments

- tabble:

  A frequency table created with
  [`table`](https://rdrr.io/r/base/table.html)

- prevalence:

  Prevalence value. Default is `NULL`

- positive:

  Positive class

- ...:

  Other, not currently used

## Value

A tibble with (at present) columns for sensitivity, specificity, PPV,
NPV, F1 score, detection rate, detection prevalence, balanced accuracy,
FDR, FOR, FPR, FNR. For more than 2 classes, these statistics are
provided for each class.

## Details

Used within confusion_matrix to calculate various confusion matrix
metrics. This is called by `confusion_matrix`, but if this is all you
want you can simply supply the table.

Suppose a 2x2 table with notation

|           |        |          |
|-----------|--------|----------|
|           | target |          |
| Predicted | Event  | No Event |
| Event     | A      | B        |
| No Event  | C      | D        |

The formulas used here are: \$\$Sensitivity = A/(A+C)\$\$
\$\$Specificity = D/(B+D)\$\$ \$\$Prevalence = (A+C)/(A+B+C+D)\$\$
\$\$Positive Predictive Value = (sensitivity \*
prevalence)/((sensitivity\*prevalence) +
((1-specificity)\*(1-prevalence)))\$\$ \$\$Negative Predictive Value =
(specificity \* (1-prevalence))/(((1-sensitivity)\*prevalence) +
((specificity)\*(1-prevalence)))\$\$ \$\$Detection Rate =
A/(A+B+C+D)\$\$ \$\$Detection Prevalence = (A+B)/(A+B+C+D)\$\$
\$\$Balanced Accuracy = (sensitivity+specificity)/2\$\$ \$\$Precision =
A/(A+B)\$\$ \$\$Recall = A/(A+C)\$\$ \$\$F1 = harmonic mean of precision
and recall = (1+beta^2)\*precision\*recall/((beta^2 \*
precision)+recall)\$\$ where `beta = 1` for this function. \$\$False
Discovery Rate = 1 - Positive Predictive Value\$\$ \$\$False Omission
Rate = 1 - Negative Predictive Value\$\$ \$\$False Positive Rate = 1 -
Specificity\$\$ \$\$False Negative Rate = 1 - Sensitivity\$\$ \$\$D' =
qnorm(Sensitivity) - qnorm(1 - Specificity)\$\$ \$\$AUC ~=
pnorm(D'/sqrt(2))\$\$

See the references for discussions of the first five formulas.
Abbreviations:

- Positive Predictive Value: PPV:
- Negative Predictive Value: NPV:
- False Discovery Rate: FDR:
- False Omission Rate: FOR:
- False Positive Rate: FPR:
- False Negative Rate: FNR:

## Note

Different names are used for the same statistics.

- Sensitivity: True Positive Rate, Recall, Hit Rate, Power:
- Specificity: True Negative Rate:
- Positive Predictive Value: Precision:
- False Negative Rate: Miss Rate, Type II error rate, beta:
- False Positive Rate: Fallout, Type I error rate, alpha:

This function is called by `confusion_matrix`, but if this is all you
want, you can simply supply the table to this function.

## References

Kuhn, M. (2008), "Building predictive models in R using the caret
package, " *Journal of Statistical Software*,
(<https://www.jstatsoft.org/article/view/v028i05>).

Altman, D.G., Bland, J.M. (1994) "Diagnostic tests 1: sensitivity and
specificity", *British Medical Journal*, vol 308, 1552.

Altman, D.G., Bland, J.M. (1994) "Diagnostic tests 2: predictive
values," *British Medical Journal*, vol 309, 102.

Velez, D.R., et. al. (2008) "A balanced accuracy function for epistasis
modeling in imbalanced datasets using multifactor dimensionality
reduction.," *Genetic Epidemiology*, vol 4, 306.

## Author

Michael Clark (see
[m-clark/confusion_matrix](https://github.com/m-clark/confusionMatrix)).
