utils::globalVariables(c('Class', 'Positive', 'N Positive', 'N Negative', 'N'))

#' Calculate Accuracy
#'
#' @description Calculates accuracy and related metrics.
#'
#' @author Michael Clark (see [m-clark/confusion_matrix](https://github.com/m-clark/confusionMatrix)).
#'
#' @param tabble A frequency table created with \code{\link{table}}
#'
#' @details Calculates accuracy, lower and upper bounds, the guessing rate and
#'   p-value of the accuracy vs. the guessing rate. This function is called by
#'   \code{confusion_matrix}, but if this is all you want, you can simply supply
#'   the table to this function.
#'
#' @return A tibble with the corresponding statistics
#'
#' @seealso \code{\link{binom.test}}
#'
#' @examples
#' p = sample(letters[1:4], 250, replace = TRUE, prob = 1:4)
#' o = sample(letters[1:4], 250, replace = TRUE, prob = 1:4)
#' calc_accuracy(table(p, o))
#'
#' @export
calc_accuracy <- function(tabble) {

  acc <- sum(diag(tabble))/sum(tabble)

  acc_ci <-
    try(
      stats::binom.test(sum(diag(tabble)), sum(tabble))$conf.int,
      silent = TRUE
    )

  if(inherits(acc_ci, "try-error"))
    acc_ci <- rep(NA, 2)

  acc_p <- try(
    stats::binom.test(
      sum(diag(tabble)),
      sum(tabble),
      p = max(colSums(tabble)/sum(tabble)),
      alternative = "greater"
    ),
    silent = TRUE)

  if (inherits(acc_p, "try-error"))
    acc_p <- c("null.value.probability of success" = NA, p.value = NA)
  else
    acc_p <- unlist(acc_p[c("null.value", "p.value")])

  tibble::tibble(
    Accuracy = acc,
    `Accuracy LL` = acc_ci[1],
    `Accuracy UL` = acc_ci[2],
    `Accuracy Guessing` = acc_p[1],
    `Accuracy P-value` = acc_p[2]
  )
}


#' Calculate various statistics from a confusion matrix
#'
#' @description Given a frequency table of predictions versus target values,
#'   calculate numerous statistics of interest.
#'
#' @author Michael Clark (see [m-clark/confusion_matrix](https://github.com/m-clark/confusionMatrix)).
#'
#' @param tabble  A frequency table created with \code{\link{table}}
#' @param prevalence Prevalence value. Default is \code{NULL}
#' @param positive Positive class
#' @param ... Other, not currently used
#' @details Used within confusion_matrix to calculate various confusion matrix
#'   metrics. This is called by \code{confusion_matrix}, but if this is all you
#'   want you can simply supply the table.
#'
#' Suppose a 2x2 table with notation
#'
#' \tabular{rcc}{ \tab target \tab \cr Predicted \tab Event \tab No Event
#' \cr Event \tab A \tab B \cr No Event \tab C \tab D \cr }
#'
#' The formulas used here are:
#' \deqn{Sensitivity = A/(A+C)}
#' \deqn{Specificity = D/(B+D)}
#' \deqn{Prevalence = (A+C)/(A+B+C+D)}
#' \deqn{Positive Predictive Value = (sensitivity * prevalence)/((sensitivity*prevalence) + ((1-specificity)*(1-prevalence)))}
#' \deqn{Negative Predictive Value = (specificity * (1-prevalence))/(((1-sensitivity)*prevalence) + ((specificity)*(1-prevalence)))} \deqn{Detection Rate = A/(A+B+C+D)}
#' \deqn{Detection Prevalence = (A+B)/(A+B+C+D)}
#' \deqn{Balanced Accuracy = (sensitivity+specificity)/2}
#' \deqn{Precision = A/(A+B)}
#' \deqn{Recall = A/(A+C)}
#' \deqn{F1 = harmonic mean of precision and recall = (1+beta^2)*precision*recall/((beta^2 * precision)+recall)}
#' where \code{beta = 1} for this function.
#' \deqn{False Discovery Rate = 1 - Positive Predictive Value}
#' \deqn{False Omission Rate = 1 - Negative Predictive Value}
#' \deqn{False Positive Rate = 1 - Specificity}
#' \deqn{False Negative Rate = 1 - Sensitivity}
#' \deqn{D' = qnorm(Sensitivity) - qnorm(1 - Specificity)}
#' \deqn{AUC ≈ pnorm(D'/sqrt(2))}
#'
#' See the references for discussions of the first five formulas.
#' Abbreviations:
#' \describe{
#'   \item{Positive Predictive Value: PPV}{}
#'   \item{Negative Predictive Value: NPV}{}
#'   \item{False Discovery Rate: FDR}{}
#'   \item{False Omission Rate: FOR}{}
#'   \item{False Positive Rate: FPR}{}
#'   \item{False Negative Rate: FNR}{}
#' }

#' @note Different names are used for the same statistics.
#' \describe{
#'   \item{Sensitivity: True Positive Rate, Recall, Hit Rate, Power}{}
#'   \item{Specificity: True Negative Rate}{}
#'   \item{Positive Predictive Value: Precision}{}
#'   \item{False Negative Rate: Miss Rate, Type II error rate, β}{}
#'   \item{False Positive Rate: Fallout, Type I error rate, α}{}
#' }
#'
#' This function is called by \code{confusion_matrix}, but if this is all you
#'   want, you can simply supply the table to this function.
#'
#' @return A tibble with (at present) columns for sensitivity, specificity, PPV, NPV, F1 score, detection rate, detection prevalence, balanced accuracy, FDR, FOR, FPR, FNR.  For
#'   more than 2 classes, these statistics are provided for each class.
#'
#' @references Kuhn, M. (2008), "Building predictive models in R using the
#' caret package, " \emph{Journal of Statistical Software},
#' (\url{https://www.jstatsoft.org/article/view/v028i05/v28i05.pdf}).
#'
#' Altman, D.G., Bland, J.M. (1994) "Diagnostic tests 1: sensitivity and
#' specificity", \emph{British Medical Journal}, vol 308, 1552.
#'
#' Altman, D.G., Bland, J.M. (1994) "Diagnostic tests 2: predictive values,"
#' \emph{British Medical Journal}, vol 309, 102.
#'
#' Velez, D.R., et. al. (2008) "A balanced accuracy function for epistasis
#' modeling in imbalanced datasets using multifactor dimensionality
#' reduction.," \emph{Genetic Epidemiology}, vol 4, 306.
#'
#' @examples
#' p = sample(letters[1:4], 250, replace = TRUE, prob = 1:4)
#' o = sample(letters[1:4], 250, replace = TRUE, prob = 1:4)
#' calc_stats(table(p, o), positive='a')
#'
#' @export
calc_stats <- function(tabble, prevalence = NULL, positive, ...) {
  # checks
  # using original all.equal checks will fail
  if (!identical(nrow(tabble), ncol(tabble)))
    stop("the table must have nrow = ncol")

  # this doesn't really check order
  if (!identical(rownames(tabble), colnames(tabble)))
    stop("the table must the same groups in the same order")

  tabble_init <- tabble

  # Calculate Sensitivity ---------------------------------------------------

  if (nrow(tabble_init) > 2) {
    tmp <- tabble_init
    tabble <- matrix(NA, 2, 2)

    colnames(tabble) <- rownames(tabble) <- c("pos", "neg")
    posCol <- which(colnames(tmp) %in% positive)
    negCol <- which(!(colnames(tmp) %in% positive))

    tabble[1, 1] <- sum(tmp[posCol, posCol])
    tabble[1, 2] <- sum(tmp[posCol, negCol])
    tabble[2, 1] <- sum(tmp[negCol, posCol])
    tabble[2, 2] <- sum(tmp[negCol, negCol])
    tabble <- as.table(tabble)

    pos <- "pos"
    neg <- "neg"

    rm(tmp)
  } else {
    pos <- positive
    neg <- rownames(tabble_init)[rownames(tabble_init) != positive]
  }

  numer <- sum(tabble[pos, pos])
  denom <- sum(tabble[, pos])
  sens  <- ifelse(denom > 0, numer/denom, NA)

  detection_rate <- sum(tabble[pos, pos])/sum(tabble)
  detection_prevalence <- sum(tabble[pos, ])/sum(tabble)


  # Calculate Specificity ---------------------------------------------------

  numer <- sum(tabble[neg, neg])
  denom <- sum(tabble[, neg])
  spec  <- ifelse(denom > 0, numer/denom, NA)


  # Calculate Prevalence ----------------------------------------------------

  if (is.null(prevalence))
    prevalence <- sum(tabble_init[, positive]) / sum(tabble_init)


  # Calculate PPV/NPV -------------------------------------------------------

  ppv <-
    (sens * prevalence) /
    ((sens * prevalence) + ((1 - spec) *(1 - prevalence)))

  npv <-
    (spec * (1 - prevalence)) /
    (((1 - sens) * prevalence) + ((spec) * (1 - prevalence)))


  # Calculate F1 ------------------------------------------------------------

  f1 <- 2/(1/sens + 1/ppv)


  # # Calculate d-prime/AUC ---------------------------------------------------
  #
  # # check for inability to calculate
  # if (any(rowSums(tabble) == 0)) {
  #   d_prime <- NA
  #   auc <- NA
  # }
  # else {
  #   d_prime <- stats::qnorm(sens) - stats::qnorm(1-spec)  # primary calculation
  #
  #   # check if sens/spec 1/0 and fudge with warning
  #   if (is.infinite(d_prime)) {
  #     warning('Encountered infinite values for d_prime,
  #   fudge factor introduced to correct.')
  #     sens_   <- abs(sens - .000001)
  #     spec_   <- abs(spec - .000001)
  #     d_prime <- stats::qnorm(sens_) - stats::qnorm(1 - spec_)
  #
  #     xmax <- max(4, d_prime + 3)
  #     x <- seq(-3, xmax, 0.05)
  #
  #     vpx <- stats::pnorm(x + stats::qnorm(sens_))
  #     fpx <- stats::pnorm(x - stats::qnorm(spec_))
  #   }
  #   else {
  #     xmax <- max(4, d_prime + 3)
  #     x <- seq(-3, xmax, 0.05)
  #
  #     vpx <- stats::pnorm(x + stats::qnorm(sens))
  #     fpx <- stats::pnorm(x - stats::qnorm(spec))
  #   }
  #
  #   fpx.diff <- diff(fpx)
  #   lower.sum <- sum(fpx.diff * vpx[-1])
  #   upper.sum <- sum(fpx.diff * vpx[-length(vpx)])
  #   auc <- (lower.sum + upper.sum)/2
  #   auc <- ifelse(auc < .5, 1 - auc, auc)
  #   # shortcut auc = stats::pnorm(tab$`D Prime`/sqrt(2))
  # }


  # Return result -----------------------------------------------------------

  tibble::tibble(
    `Sensitivity/Recall/TPR` = sens,
    `Specificity/TNR` = spec,
    `PPV/Precision` = ppv,
    `NPV` = npv,
    `F1/Dice` = f1,
    `Prevalence` = prevalence,
    `Detection Rate` = detection_rate,
    `Detection Prevalence` = detection_prevalence,
    `Balanced Accuracy` = (sens + spec)/2,
    `FDR` = 1 - ppv,
    `FOR`  = 1 - npv,
    `FPR/Fallout`  = 1 - spec,
    `FNR`  = 1 - sens
    # `D Prime` = d_prime,
    # `AUC` = auc
  )
}

#' Calculate various statistics from a confusion matrix
#'
#' @description Given a vector of predictions and target values, calculate
#'   numerous statistics of interest. Modified from [m-clark/confusion_matrix](https://github.com/m-clark/confusionMatrix).
#' @param prediction A vector of predictions
#' @param target A vector of target values
#' @param positive The positive class for a 2-class setting. Default is
#'   \code{NULL}, which will result in using the first level of \code{target}.
#' @param prevalence Prevalence rate.  Default is \code{NULL}.
#' @param dnn The row and column headers for the contingency table returned. Default is 'Predicted' for rows and 'Target' for columns.
#' @param longer Transpose the output to long form.  Default is FALSE (requires \code{tidyr 1.0}).
#' @param ... Other parameters, not currently used.
#'
#' @details This returns accuracy, agreement, and other statistics. See the
#'   functions below to find out more. Originally inspired by the
#'   \code{confusionMatrix} function from the \code{caret} package.
#'
#' @seealso
#'   \code{\link{calc_accuracy}} \code{\link{calc_stats}}
#'   \code{\link{confusion_matrix}}
#'
#' @return A list of tibble(s) with the associated statistics and possibly the
#'   frequency table as list column of the first element.
#'
#' @references Kuhn, M., & Johnson, K. (2013). Applied predictive modeling.
#'
#' @examples
#' prediction = c(0,1,1,0,0,1,0,1,1,1)
#' target     = c(0,1,1,1,0,1,0,1,0,1)
#' confusion_matrix(prediction, target, positive = '1')
#'
#' set.seed(42)
#' prediction = sample(letters[1:4], 250, replace = TRUE, prob = 1:4)
#' target     = sample(letters[1:4], 250, replace = TRUE, prob = 1:4)
#' confusion_matrix(prediction, target)
#'
#' prediction = c(rep(1, 50), rep(2, 40), rep(3, 60))
#' target     = c(rep(1, 50), rep(2, 50), rep(3, 50))
#' confusion_matrix(prediction, target)
#' confusion_matrix(prediction, target) %>% purrr::pluck("Table")
#' confusion_matrix(prediction, target, longer=TRUE)
#' confusion_matrix(prediction, target, longer=TRUE) %>%
#'   purrr::pluck("Other") %>%
#'   tidyr::spread(Class, Value)
#'
#'
#' @export
confusion_matrix <- function(
  prediction,
  target,
  positive = NULL,
  prevalence = NULL,
  dnn = c('Predicted', 'Target'),
  longer = FALSE,
  ...
) {

  # Initial Checks ----------------------------------------------------------


  # input checks
  if (!is.character(positive) & !is.null(positive))
    stop("Positive argument must be character")

  if (!is.null(prevalence) &&
      (prevalence < 0 | prevalence > 1 | !is.numeric(prevalence)))
    stop('Prevalence should be a value between 0 and 1')

  if (!is.character(dnn) | length(dnn) != 2)
    stop('dnn should be a character vector of length 2')

  if (!is.logical(longer))
    stop('longer should be TRUE or FALSE')


  # other checks

  class_pred <- class(prediction)
  class_obs  <- class(target)

  init <- data.frame(prediction, target) %>%
    dplyr::mutate_if(is.logical, as.numeric) %>%
    dplyr::mutate_all(as.factor)

  if (class_pred != class_obs) {
    # put trycatch here to see if coercible?
  }

  if (any(levels(init$target) != levels(init$prediction))) {
    warning(
      "Levels are not the same for target and prediction.
    \nRefactoring prediction to match. Some statistics may not be available."
    )

    init <- init %>%
      dplyr::mutate(prediction = factor(prediction, levels = levels(target)))
  }

  prediction <- init$prediction
  target   <- init$target

  # changed focus to be on target levels; prediction can have a single class
  # without failure.
  classLevels <- levels(target)
  numLevels   <- length(classLevels)

  if(numLevels < 2)
    stop("There must be at least 2 factors levels in the target")

  if(!is.null(positive) && !positive %in% classLevels)
    stop("Positive is not among the class levels of the target")

  if(numLevels == 2 & is.null(positive))  positive <- levels(target)[1]

  # create confusion matrix

  conf_mat <- table(prediction, target, dnn = dnn)


  # Calculate stats ---------------------------------------------------------

  result_accuracy   <- calc_accuracy(conf_mat)

  if (numLevels == 2) {
    result_statistics <- calc_stats(
      conf_mat,
      prevalence = prevalence,
      positive = positive
    )

    result_statistics <- result_statistics %>%
      dplyr::mutate(
        N = sum(conf_mat),
        Positive = positive,
        `N Positive` = sum(conf_mat[, positive]),
        `N Negative` = N-`N Positive`
      )

    result_statistics <- result_statistics %>%
      dplyr::select(Positive, N, `N Positive`, `N Negative`, dplyr::everything())

    result <- list(
      Accuracy = result_accuracy,
      Other = result_statistics
      # `Association and Agreement` = result_agreement
    )
  } else {
    result_statistics <- lapply(
      classLevels,
      function(i) calc_stats(
        conf_mat,
        prevalence = prevalence,
        positive = i
      )
    )

    result_statistics <- dplyr::bind_rows(result_statistics) %>%
      dplyr::mutate(N = colSums(conf_mat))

    # add averages
    avg <- data.frame(t(colMeans(result_statistics)))

    colnames(avg) <- colnames(result_statistics)

    result_statistics <- result_statistics %>%
      dplyr::bind_rows(avg)

    result_statistics <- result_statistics %>%
      dplyr::mutate(Class = c(classLevels, 'Average')) %>%
      dplyr::select(Class, N, dplyr::everything())

    result <- list(
      Accuracy = result_accuracy,
      Other = result_statistics,
      # `Association and Agreement` = result_agreement
      Table=conf_mat
    )
  }

  # Return result -----------------------------------------------------------

  # Note, can remove version check after a while
  test_tidyr <- tryCatch(utils::packageVersion("tidyr"), error = function(c) "error")

  test_tidyr_installed <- inherits(test_tidyr, 'error')

  if (!test_tidyr_installed)
    tidyr_version <- as.numeric(substr(test_tidyr, start = 1, stop = 1))

  if (longer & (test_tidyr_installed | tidyr_version < 1)) {
    message('Tidyr >= 1.0 not installed. longer argument ignored.')
    longer <- FALSE
  }

  if (longer) {
    result$Accuracy = tidyr::pivot_longer(
      result$Accuracy,
      cols = dplyr::everything(),
      names_to = 'Statistic',
      values_to = 'Value',
    )

    if (numLevels == 2) {
      result$Other = tidyr::pivot_longer(
        result$Other,
        cols = -Positive,
        names_to = 'Statistic',
        values_to = 'Value',
      )
    }
    else {
      result$Other = tidyr::pivot_longer(
        result$Other,
        cols = -Class,
        names_to = 'Statistic',
        values_to = 'Value',
      )
    }


    # result$`Association and Agreement` = tidyr::pivot_longer(
    #   result$`Association and Agreement`,
    #   cols = dplyr::everything(),
    #   names_to = 'Statistic',
    #   values_to = 'Value',
    # )
  }

  result
}
