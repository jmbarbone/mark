#' Optimal threshold from pROC mod
#'
#' Find the optimal threshold from a pROC mod
#'
#' @param mod A roc mod from the pROC package
#' @param method Method to determine optimal threshold
#' @param ... Additional arguments passed to [pROC::ci.thresholds()]
#' @export
#'
#' @examples
#' x <- pROC::aSAH
#' mod <- pROC::roc(x$outcome, x$s100b, levels=c("Good", "Poor"))
#'
#' optimal_threshold(mod)

optimal_threshold <- function(mod, method = c("youden", "top_left"), ...) {
  require_namespace("pROC")
  stopifnot(inherits(mod, "roc"))
  method <- match_arg(method)

  cis <- pROC::ci.thresholds(mod, ...)
  w <- switch(
    method,
    youden = {
      with(mod, which.max(sensitivities + specificities))
    },
    top_left = {
      with(mod, which.min((1 - sensitivities)^2 + (1 - specificities)^2))
    }
  )

  un <- unclass(cis)
  thres <- mod$thresholds[w]
  ci_sp <- subset_rownames(cis$sensitivity, thres)
  ci_se <- subset_rownames(cis$specificity, thres)

  stopifnot(length(ci_sp) == 3L & length(ci_sp) == 3L)

  structure(thres,
            class = c("jordan_roc_thres", "numeric"),
            typeof = "double",
            method = method,
            ci_sp = ci_sp,
            ci_se = ci_se)

}

#' @export
print.jordan_roc_thres <- function(x, ...) {
  a <- attributes(x)
  dig <- getOption("digits", 7L)
  se <- round(a$ci_se, dig)
  sp <- round(a$ci_sp, dig)

  out <- paste0(
    a$method, ": ", x, "\n",
    "Sensitivity = ", se[2], " (95% CI ", se[1], " - ", se[3], ")\n",
    "Specificity = ", sp[2], " (95% CI ", sp[1], " - ", sp[3], ")"
  )

  cat(out)
  invisible(x)
}

subset_rownames <- function(x, y) {
  rn <- rownames(x)
  stopifnot("Object `x` has no rownames" = !is.null(rn))
  x[rn == y, ]
}

#' Plots an ROC model
#'
#' Quickly plots a model based on my own likings
#'
#' @param mod A model from pROC
#' @param thres_method The threshold method to print
#' @param col The color of the line
#' @param ... Additional arguments passed to [pROC::plot.roc()]
#'
#' @export
#' @examples
#' x <- pROC::aSAH
#' mod <- pROC::roc(x$outcome, x$s100b, levels=c("Good", "Poor"))
#'
#' quick_plot_roc(mod)
quick_plot_roc <- function(mod, thres_method = c("youden", "closest.topleft"), col = "blue", ...) {
  require_namespace("pROC")
  stopifnot(inherits(mod, "roc"))
  thres_method <- match_arg(thres_method)

  pROC::plot.roc(
    mod,
    legacy.axes = TRUE,
    asp = NA,
    print.auc = TRUE,
    print.thres = "best",
    print.thres.best.method = thres_method,
    ...
  )

  plot(pROC::smooth(mod), add = TRUE, col = col, lwd = 2, lty = 2)

  graphics::legend("bottomright",
                   legend = c("Empirical", "Smoothed"),
                   col = c(graphics::par("fg"), col),
                   lwd = 2)

  invisible(mod)
}

