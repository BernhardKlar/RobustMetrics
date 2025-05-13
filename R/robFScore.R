#' Robust F-Beta Score
#'
#' Compute a robust version of the F-Beta Score. 
#'
#' @param actual	A vector of actual values (1/0 or TRUE/FALSE)
#' @param predicted A vector of prediction values (1/0 or TRUE/FALSE)
#' @param TP Count of true positives (correctly predicted 1/TRUE)
#' @param FN Count of false negatives (predicted 0/FALSE, but actually 1/TRUE)
#' @param FP Count of false positives (predicted 1/TRUE, but actually 0/FALSE)
#' @param TN Count of true negatives (correctly predicted 0/FALSE)
#' @param beta Beta squared is the weight of recall in the harmonic mean
#' @param d0 Weight of the estimated true positive probability in the harmonic mean
#'
#' @details
#' Calculate the robust F-Beta Score \eqn{F_{\beta,d_0}} with two parameters.
#' Provide either:
#' * `actual` and `predicted` or
#' * `TP`, `FN`, `FP` and `TN`. 
#'
#' If \eqn{d_0=0}, the robust F-Beta Score coincides with the F-Beta Score.
#' @md
#'
#' @return robust F-Beta Score.
#'
#' @references
#' Holzmann, H., Klar, B. (2024). Robust performance metrics for imbalanced classification problems.
#' arXiv:2404.07661. \href{https://arxiv.org/abs/2404.07661}{LINK}
#'
#' @examples
#' actual <-    c(1,1,1,1,1,1,0,0,0,0)
#' predicted <- c(1,1,1,1,0,0,1,0,0,0)
#' robFScore(actual, predicted, beta=1, d0=0.1)
#' robFScore(TP=4, FN=2, FP=1, TN=3, beta=1, d0=1)
#'
#' @export
robFScore = function(actual = NULL, predicted = NULL, TP = NULL, FN = NULL,
                  FP = NULL, TN = NULL, beta = 1, d0 = 0.1) {
  valid_input <- FALSE
  if (!is.null(predicted) & !is.null(actual) & is.null(TP) & is.null(FN) &
      is.null(FP) & is.null(TN))
    valid_input <- TRUE
  if ((!is.null(TP) & !is.null(FN) & !is.null(FP) & !is.null(TN)) &
      is.null(predicted) & is.null(actual))
    valid_input <- TRUE
  if (!valid_input)
    stop("Either {'predicted' and 'actual'} or {'TP', 'FN', 'FP', 'TN'} should be provided.")

  if (beta <= 0) stop("beta should be positive.")
  if (d0<0) stop("d0 should be nonnegative.")

  if (is.null(TP)) {
    if (length(actual) != length(predicted))
      stop("'actual' and 'predicted' should have the same length")
    if (!is.logical(actual) && !is.numeric(actual) && !all(actual %in% c(0L, 1L)))
      stop("'actual' should only consist of TRUE/FALSE or 1/0")
    if (!is.logical(predicted) && !is.numeric(predicted) && !all(predicted %in% c(0L, 1L)))
      stop("'predicted' should only consist of TRUE/FALSE or 1/0")
    TP <- sum(actual & predicted)   # True Positives
    FN <- sum(actual & !predicted)  # False Negatives
    FP <- sum(!actual & predicted)  # False Positives
    TN <- sum(!actual & !predicted) # True Negatives
  } else {
    TP <- as.double(TP)
    FP <- as.double(FP)
    TN <- as.double(TN)
    FN <- as.double(FN)
  }

  recall <- TP / (TP + FN)
  precision <- TP / (TP + FP)
  total <- (TP + FN + FP + TN)
  PP <- (TP + FN) / total
  TPP <- TP / total
  FS <- (d0/PP + beta^2 + 1) / (d0/TPP + beta^2/recall + 1/precision )
  return( FS )
}
