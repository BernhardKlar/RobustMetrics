#' Robust Matthews correlation coefficient
#'
#' Compute a robust version of Matthews correlation coefficient (MCC).
#'
#' @param actual	A vector of actual values (1/0 or TRUE/FALSE)
#' @param predicted A vector of prediction values (1/0 or TRUE/FALSE)
#' @param TP Count of true positives (correctly predicted 1/TRUE)
#' @param FN Count of false negatives (predicted 0/FALSE, but actually 1/TRUE)
#' @param FP Count of false positives (predicted 1/TRUE, but actually 0/FALSE)
#' @param TN Count of true negatives (correctly predicted 0/FALSE)
#' @param d Parameter of the robust MCC
#'
#' @details
#' Calculate the robust MCC.
#' Provide either:
#' * `actual` and `predicted` or
#' * `TP`, `FN`, `FP` and `TN`.
#'
#' If \eqn{d=0}, the robust MCC coincides with the MCC.
#' @md
#'
#' @return robust MCC.
#'
#' @references
#' Holzmann, H., Klar, B. (2024). Robust performance metrics for imbalanced classification problems.
#' arXiv:2404.07661. \href{https://arxiv.org/abs/2404.07661}{LINK}
#'
#' @examples
#' actual <- c(1,1,1,1,1,1,0,0,0,0)
#' predicted <- c(1,1,1,1,0,0,1,0,0,0)
#' robMCC(actual, predicted, d=0.05)
#' robMCC(TP=4, FN=2, FP=1, TN=3, d=0.05)
#'
#' @export
robMCC = function(actual = NULL, predicted = NULL, TP = NULL, FN = NULL,
                  FP = NULL, TN = NULL, d = 0.1) {
  valid_input <- FALSE
  if (!is.null(predicted) & !is.null(actual) & is.null(TP) & is.null(FN) &
      is.null(FP) & is.null(TN))
    valid_input <- TRUE
  if ((!is.null(TP) & !is.null(FN) & !is.null(FP) & !is.null(TN)) &
      is.null(predicted) & is.null(actual))
    valid_input <- TRUE
  if (!valid_input)
    stop("Either {'predicted' and 'actual'} or {'TP', 'FN', 'FP', 'TN'} should be provided.")
  if (d<0) stop("d should be nonnegative.")

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

  total <- (TP + FN + FP + TN)
  PP <- (TP + FN) / total
  predPP <- (TP + FP) / total
  TPP <- TP / total
  mcc <- sqrt( d / (PP*(1-PP)) + 1 ) * (TPP - PP*predPP) / sqrt(PP*(1-PP) * (d + predPP*(1-predPP)))
  return( mcc )
}
