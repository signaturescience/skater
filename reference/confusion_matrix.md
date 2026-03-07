# Calculate various statistics from a confusion matrix

Given a vector of predictions and target values, calculate numerous
statistics of interest. Modified from
[m-clark/confusion_matrix](https://github.com/m-clark/confusionMatrix).

## Usage

``` r
confusion_matrix(
  prediction,
  target,
  positive = NULL,
  prevalence = NULL,
  dnn = c("Predicted", "Target"),
  longer = FALSE,
  ...
)
```

## Arguments

- prediction:

  A vector of predictions

- target:

  A vector of target values

- positive:

  The positive class for a 2-class setting. Default is `NULL`, which
  will result in using the first level of `target`.

- prevalence:

  Prevalence rate. Default is `NULL`.

- dnn:

  The row and column headers for the contingency table returned. Default
  is 'Predicted' for rows and 'Target' for columns.

- longer:

  Transpose the output to long form. Default is FALSE (requires
  `tidyr 1.0`).

- ...:

  Other parameters, not currently used.

## Value

A list of tibble(s) with the associated statistics and possibly the
frequency table as list column of the first element. If classes contain
\>1 numeric class and a single non-numeric class (e.g., "1", "2", "3",
and "Unrelated", the RMSE of the reciprocal of the Targets + 0.5 will
also be returned.)

## Details

This returns accuracy, agreement, and other statistics. See the
functions below to find out more. Originally inspired by the
`confusionMatrix` function from the `caret` package.

## References

Kuhn, M., & Johnson, K. (2013). Applied predictive modeling.

## See also

[`calc_accuracy`](https://signaturescience.github.io/skater/reference/calc_accuracy.md)
[`calc_stats`](https://signaturescience.github.io/skater/reference/calc_stats.md)

## Examples

``` r
prediction = c(0,1,1,0,0,1,0,1,1,1)
target     = c(0,1,1,1,0,1,0,1,0,1)
confusion_matrix(prediction, target, positive = '1')
#> $Accuracy
#> # A tibble: 1 × 5
#>   Accuracy `Accuracy LL` `Accuracy UL` `Accuracy Guessing` `Accuracy P-value`
#>      <dbl>         <dbl>         <dbl>               <dbl>              <dbl>
#> 1      0.8         0.444         0.975                 0.6              0.167
#> 
#> $Other
#> # A tibble: 1 × 17
#>   Positive     N `N Positive` `N Negative` `Sensitivity/Recall/TPR`
#>   <chr>    <int>        <int>        <int>                    <dbl>
#> 1 1           10            6            4                    0.833
#> # ℹ 12 more variables: `Specificity/TNR` <dbl>, `PPV/Precision` <dbl>,
#> #   NPV <dbl>, `F1/Dice` <dbl>, Prevalence <dbl>, `Detection Rate` <dbl>,
#> #   `Detection Prevalence` <dbl>, `Balanced Accuracy` <dbl>, FDR <dbl>,
#> #   FOR <dbl>, `FPR/Fallout` <dbl>, FNR <dbl>
#> 

set.seed(42)
prediction = sample(letters[1:4], 250, replace = TRUE, prob = 1:4)
target     = sample(letters[1:4], 250, replace = TRUE, prob = 1:4)
confusion_matrix(prediction, target)
#> Reciprocal RMSE not calculated: more than one non-numeric class.
#> $Accuracy
#> # A tibble: 1 × 5
#>   Accuracy `Accuracy LL` `Accuracy UL` `Accuracy Guessing` `Accuracy P-value`
#>      <dbl>         <dbl>         <dbl>               <dbl>              <dbl>
#> 1    0.276         0.222         0.336               0.452              1.000
#> 
#> $Other
#> # A tibble: 5 × 15
#>   Class       N `Sensitivity/Recall/TPR` `Specificity/TNR` `PPV/Precision`   NPV
#>   <chr>   <dbl>                    <dbl>             <dbl>           <dbl> <dbl>
#> 1 a        27                      0.111             0.879           0.1   0.891
#> 2 b        39                      0.154             0.782           0.115 0.833
#> 3 c        71                      0.282             0.709           0.278 0.713
#> 4 d       113                      0.354             0.591           0.417 0.526
#> 5 Average  62.5                    0.225             0.740           0.227 0.741
#> # ℹ 9 more variables: `F1/Dice` <dbl>, Prevalence <dbl>,
#> #   `Detection Rate` <dbl>, `Detection Prevalence` <dbl>,
#> #   `Balanced Accuracy` <dbl>, FDR <dbl>, FOR <dbl>, `FPR/Fallout` <dbl>,
#> #   FNR <dbl>
#> 
#> $Table
#>          Target
#> Predicted  a  b  c  d
#>         a  3  3  6 18
#>         b  5  6 21 20
#>         c  8  9 20 35
#>         d 11 21 24 40
#> 
#> $recip_rmse
#> [1] NA
#> 

prediction = c(rep(1, 50), rep(2, 40), rep(3, 60))
target     = c(rep(1, 50), rep(2, 50), rep(3, 50))
confusion_matrix(prediction, target)
#> $Accuracy
#> # A tibble: 1 × 5
#>   Accuracy `Accuracy LL` `Accuracy UL` `Accuracy Guessing` `Accuracy P-value`
#>      <dbl>         <dbl>         <dbl>               <dbl>              <dbl>
#> 1    0.933         0.881         0.968               0.333           3.36e-54
#> 
#> $Other
#> # A tibble: 4 × 15
#>   Class       N `Sensitivity/Recall/TPR` `Specificity/TNR` `PPV/Precision`   NPV
#>   <chr>   <dbl>                    <dbl>             <dbl>           <dbl> <dbl>
#> 1 1          50                    1                 1               1     1    
#> 2 2          50                    0.8               1               1     0.909
#> 3 3          50                    1                 0.9             0.833 1    
#> 4 Average    50                    0.933             0.967           0.944 0.970
#> # ℹ 9 more variables: `F1/Dice` <dbl>, Prevalence <dbl>,
#> #   `Detection Rate` <dbl>, `Detection Prevalence` <dbl>,
#> #   `Balanced Accuracy` <dbl>, FDR <dbl>, FOR <dbl>, `FPR/Fallout` <dbl>,
#> #   FNR <dbl>
#> 
#> $Table
#>          Target
#> Predicted  1  2  3
#>         1 50  0  0
#>         2  0 40  0
#>         3  0 10 50
#> 
#> $recip_rmse
#> [1] 0.02950844
#> 
confusion_matrix(prediction, target) %>% purrr::pluck("Table")
#>          Target
#> Predicted  1  2  3
#>         1 50  0  0
#>         2  0 40  0
#>         3  0 10 50
confusion_matrix(prediction, target, longer=TRUE)
#> $Accuracy
#> # A tibble: 5 × 2
#>   Statistic            Value
#>   <chr>                <dbl>
#> 1 Accuracy          9.33e- 1
#> 2 Accuracy LL       8.81e- 1
#> 3 Accuracy UL       9.68e- 1
#> 4 Accuracy Guessing 3.33e- 1
#> 5 Accuracy P-value  3.36e-54
#> 
#> $Other
#> # A tibble: 56 × 3
#>    Class Statistic               Value
#>    <chr> <chr>                   <dbl>
#>  1 1     N                      50    
#>  2 1     Sensitivity/Recall/TPR  1    
#>  3 1     Specificity/TNR         1    
#>  4 1     PPV/Precision           1    
#>  5 1     NPV                     1    
#>  6 1     F1/Dice                 1    
#>  7 1     Prevalence              0.333
#>  8 1     Detection Rate          0.333
#>  9 1     Detection Prevalence    0.333
#> 10 1     Balanced Accuracy       1    
#> # ℹ 46 more rows
#> 
#> $Table
#>          Target
#> Predicted  1  2  3
#>         1 50  0  0
#>         2  0 40  0
#>         3  0 10 50
#> 
#> $recip_rmse
#> [1] 0.02950844
#> 
confusion_matrix(prediction, target, longer=TRUE) %>%
  purrr::pluck("Other") %>%
  tidyr::spread(Class, Value)
#> # A tibble: 14 × 5
#>    Statistic                 `1`     `2`    `3` Average
#>    <chr>                   <dbl>   <dbl>  <dbl>   <dbl>
#>  1 Balanced Accuracy       1      0.9     0.95   0.95  
#>  2 Detection Prevalence    0.333  0.267   0.4    0.333 
#>  3 Detection Rate          0.333  0.267   0.333  0.311 
#>  4 F1/Dice                 1      0.889   0.909  0.933 
#>  5 FDR                     0      0       0.167  0.0556
#>  6 FNR                     0      0.2     0      0.0667
#>  7 FOR                     0      0.0909  0      0.0303
#>  8 FPR/Fallout             0      0       0.1    0.0333
#>  9 N                      50     50      50     50     
#> 10 NPV                     1      0.909   1      0.970 
#> 11 PPV/Precision           1      1       0.833  0.944 
#> 12 Prevalence              0.333  0.333   0.333  0.333 
#> 13 Sensitivity/Recall/TPR  1      0.8     1      0.933 
#> 14 Specificity/TNR         1      1       0.9    0.967 

# Prediction with an unrelated class
prediction = c(rep(1, 50), rep(2, 40), rep(3, 60), rep("Unrelated", 55))
target     = c(rep(1, 50), rep(2, 50), rep(3, 55), rep("Unrelated", 50))
confusion_matrix(prediction, target)
#> $Accuracy
#> # A tibble: 1 × 5
#>   Accuracy `Accuracy LL` `Accuracy UL` `Accuracy Guessing` `Accuracy P-value`
#>      <dbl>         <dbl>         <dbl>               <dbl>              <dbl>
#> 1    0.927         0.882         0.958               0.268           5.56e-89
#> 
#> $Other
#> # A tibble: 5 × 15
#>   Class         N Sensitivity/Recall/T…¹ `Specificity/TNR` `PPV/Precision`   NPV
#>   <chr>     <dbl>                  <dbl>             <dbl>           <dbl> <dbl>
#> 1 1          50                    1                 1               1     1    
#> 2 2          50                    0.8               1               1     0.939
#> 3 3          55                    0.909             0.933           0.833 0.966
#> 4 Unrelated  50                    1                 0.968           0.909 1    
#> 5 Average    51.2                  0.927             0.975           0.936 0.976
#> # ℹ abbreviated name: ¹​`Sensitivity/Recall/TPR`
#> # ℹ 9 more variables: `F1/Dice` <dbl>, Prevalence <dbl>,
#> #   `Detection Rate` <dbl>, `Detection Prevalence` <dbl>,
#> #   `Balanced Accuracy` <dbl>, FDR <dbl>, FOR <dbl>, `FPR/Fallout` <dbl>,
#> #   FNR <dbl>
#> 
#> $Table
#>            Target
#> Predicted    1  2  3 Unrelated
#>   1         50  0  0         0
#>   2          0 40  0         0
#>   3          0 10 50         0
#>   Unrelated  0  0  5        50
#> 
#> $recip_rmse
#> [1] 0.02711929
#> 
# Prediction with two unrelated classes
prediction = c(rep(1, 50), rep(2, 40), rep("Third", 60), rep("Unrelated", 55))
target     = c(rep(1, 50), rep(2, 50), rep("Third", 55), rep("Unrelated", 50))
confusion_matrix(prediction, target)
#> Reciprocal RMSE not calculated: more than one non-numeric class.
#> $Accuracy
#> # A tibble: 1 × 5
#>   Accuracy `Accuracy LL` `Accuracy UL` `Accuracy Guessing` `Accuracy P-value`
#>      <dbl>         <dbl>         <dbl>               <dbl>              <dbl>
#> 1    0.927         0.882         0.958               0.268           5.56e-89
#> 
#> $Other
#> # A tibble: 5 × 15
#>   Class         N Sensitivity/Recall/T…¹ `Specificity/TNR` `PPV/Precision`   NPV
#>   <chr>     <dbl>                  <dbl>             <dbl>           <dbl> <dbl>
#> 1 1          50                    1                 1               1     1    
#> 2 2          50                    0.8               1               1     0.939
#> 3 Third      55                    0.909             0.933           0.833 0.966
#> 4 Unrelated  50                    1                 0.968           0.909 1    
#> 5 Average    51.2                  0.927             0.975           0.936 0.976
#> # ℹ abbreviated name: ¹​`Sensitivity/Recall/TPR`
#> # ℹ 9 more variables: `F1/Dice` <dbl>, Prevalence <dbl>,
#> #   `Detection Rate` <dbl>, `Detection Prevalence` <dbl>,
#> #   `Balanced Accuracy` <dbl>, FDR <dbl>, FOR <dbl>, `FPR/Fallout` <dbl>,
#> #   FNR <dbl>
#> 
#> $Table
#>            Target
#> Predicted    1  2 Third Unrelated
#>   1         50  0     0         0
#>   2          0 40     0         0
#>   Third      0 10    50         0
#>   Unrelated  0  0     5        50
#> 
#> $recip_rmse
#> [1] NA
#> 
```
