#' ROC curve
#'
#' Plot ROC curve together with recall / 1-precision curve.
#'
#' @param actual	A vector of actual values (1/0 or TRUE/FALSE)
#' @param predicted A vector of predicted probabilities (numeric values in \eqn{[0,1]})
#' @param d A vector of length 4
#'
#' @details
#' Instead of a precision-recall curve, a recall / 1-precision curve is plotted
#' in the same coordinate system as the ROC curve.
#'
#' Grey circles show the corresponding MCC optimal points;
#' black symbols show points optimal with respect to the robust MCC for different values of d.
#' @md
#'
#' @return ROC curve.
#'
#' @references
#' Holzmann, H., Klar, B. (2024). Robust performance metrics for imbalanced classification problems.
#' arXiv:2404.07661. \href{https://arxiv.org/abs/2404.07661}{LINK}
#'
#' @examples
#' actual <- rf.data[, 1]
#' predicted <- rf.data[, 2]
#' ROC_curve(actual, predicted, d=c(0.01,0.02,0.1,0.5))
#'
#' @importFrom graphics abline
#' @importFrom graphics legend
#' @importFrom graphics lines
#' @importFrom graphics par
#' @importFrom graphics points
#' @export
ROC_curve <- function(actual, predicted, d = c(0.01,0.05,0.1,0.5)) {

  confusion_matrix <- function(actual, predicted) {
    actual <- as.logical(actual)
    predicted <- as.logical(predicted)
    TP <- sum(actual & predicted)  # True Positives
    FN <- sum(actual & !predicted) # False Negatives
    FP <- sum(!actual & predicted) # False Positives
    TN <- sum(!actual & !predicted) # True Negatives
    return(list(TP = TP, FN = FN, FP = FP, TN = TN))
  }

  n <- length(actual)
  n1 <- sum(actual)
  n0 <- n-n1
  pr <- n1/n

  del.vec <- seq(0.001,0.999,0.001)
  nd <- length(del.vec)
  MCCgen.res <- matrix(0, nrow=nd, ncol=6)
  cond.prop <- matrix(0, nrow=nd, ncol=2)

  dd <- c(0.0, d)
  for (i in 1:nd) {
    lr.pred1 <- ifelse(predicted >= del.vec[i], 1, 0)
    cf1 <- confusion_matrix(actual, lr.pred1)
    cond.prop[i,] <- c( cf1$TP/n1, cf1$TN/n0)
    for (j in 1:5) {
      MCCgen.res[i, j] <- robMCC( TP=cf1$TP, FN=cf1$FN, FP=cf1$FP, TN=cf1$TN, d=dd[j])
    }
  }

  tab <- matrix(0, nrow=3, ncol=5)
  for (j in 1:5) {
    t1 <- del.vec[ which.max( MCCgen.res[,j] ) ]
    lr.pred1 <- ifelse(predicted >= t1, 1, 0)
    cf1 <- confusion_matrix(actual, lr.pred1)
    tab[1:3, j] <-  c(t1, cf1$TP/n1, cf1$TN/n0)
  }

  # plot ROC curve, i.e. i.e. TPR-FPR curve
  tpr.fpr <- cbind( 1-cond.prop[,2], cond.prop[,1])
  par(mar=c(5,4,1,2), mfcol=c(1,1), pty="s")
  plot( tpr.fpr, type="n", xlim=c(0,1), ylim=c(0,1), xaxs="i", yaxs="i",
        lwd=3, xlab="FPR / 1-precision", ylab="TPR")
  lines( tpr.fpr, lwd=3)
  abline(0,1,lty=2)
  # plot recall / 1-precision curve
  prec.rec <- cbind(cond.prop[,1], cond.prop[,1]*pr / (cond.prop[,1]*pr + (1-cond.prop[,2])*(1-pr)) )
  lines(1-prec.rec[,2], prec.rec[,1], lwd=3, lty=3, col="black")
  # plot points
  cond.prop1 <- tab[2:3,1]
  tpr.fpr1 <- cbind( 1-cond.prop1[2], cond.prop1[1])
  points(tpr.fpr1, col="darkgrey", pch=16, cex=1.5)
  prec.rec1 <- cbind(cond.prop1[1], cond.prop1[1]*pr / (cond.prop1[1]*pr + (1-cond.prop1[2])*(1-pr)) )
  points(1-prec.rec1[2], prec.rec1[1], col="darkgrey", pch=16, cex=1.5)

  cond.prop1 <- tab[2:3,2]
  tpr.fpr1 <- cbind( 1-cond.prop1[2], cond.prop1[1])
  points(tpr.fpr1, col="black", pch=17, cex=1.5)
  prec.rec1 <- cbind(cond.prop1[1], cond.prop1[1]*pr / (cond.prop1[1]*pr + (1-cond.prop1[2])*(1-pr)) )
  points(1-prec.rec1[2], prec.rec1[1], col="black", pch=17, cex=1.5)

  cond.prop1 <- tab[2:3,3]
  tpr.fpr1 <- cbind( 1-cond.prop1[2], cond.prop1[1])
  points(tpr.fpr1, col="black", pch=18, cex=1.5)
  prec.rec1 <- cbind(cond.prop1[1], cond.prop1[1]*pr / (cond.prop1[1]*pr + (1-cond.prop1[2])*(1-pr)) )
  points(1-prec.rec1[2], prec.rec1[1], col="black", pch=18, cex=1.5)

  cond.prop1 <- tab[2:3,4]
  tpr.fpr1 <- cbind( 1-cond.prop1[2], cond.prop1[1])
  points(tpr.fpr1, col="black", pch=15, cex=1.5)
  prec.rec1 <- cbind(cond.prop1[1], cond.prop1[1]*pr / (cond.prop1[1]*pr + (1-cond.prop1[2])*(1-pr)) )
  points(1-prec.rec1[2], prec.rec1[1], col="black", pch=15, cex=1.5)

  cond.prop1 <- tab[2:3,5]
  tpr.fpr1 <- cbind( 1-cond.prop1[2], cond.prop1[1])
  points(tpr.fpr1, col="black", pch=16, cex=1.5)
  prec.rec1 <- cbind(cond.prop1[1], cond.prop1[1]*pr / (cond.prop1[1]*pr + (1-cond.prop1[2])*(1-pr)) )
  points(1-prec.rec1[2], prec.rec1[1], col="black", pch=16, cex=1.5)

  legend("topleft", legend=c("ROC curve  ", "recall / 1-precision "), lty=c(1,3), lwd=3, cex=0.9)
  legend("bottomright", cex=1, pch=c(16,16:18,15,16), col=c("darkgrey", rep(1,4)),
         legend=c( expression(MCC), bquote(MCC[.(d[1])]), bquote(MCC[.(d[2])]),
                  bquote(MCC[.(d[3])]), bquote(MCC[.(d[4])]) ))
}
