#' Optimal threshold from pROC mod
#'
#' Find the optimal threshold from a pROC mod
#'
#' @param mod An object with class "roc" (a ROC model made with pROC::roc(.))
#' @param method Method to determine optimal threshold
#' @param ... Additional arguments passed to [pROC::ci.thresholds()]
#' @export
#'
#' @examples
#' x <- pROC::aSAH
#' mod <- pROC::roc(x$outcome, x$s100b, levels=c("Good", "Poor"))
#'
#' pROC_optimal_threshold(mod)

pROC_optimal_threshold <- function(mod, method = c("youden", "top_left"), ...) {
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
#' Quickly plots a model based on my own liking
#'
#' @details
#' When adding bootstrapped confidence intervals, the core function of
#'   `pROC:::ci.sp.roc()` is replaced with a quicker version that uses parallel
#'   processing to speed up the bootstraps and some other data manipulation.
#'   There will likely be a delay when running with bootstraps.
#'
#' @param mod An object with class "roc" (a ROC model made with [pROC::roc()])
#' @param thres_method The threshold method to print
#' @param col The color of the curve
#' @param ... Additional arguments passed to [pROC::plot.roc()]
#' @param boots Number of bootstrap replications to perform; if < 2L will not
#'   perform any
#'
#' @importFrom grDevices rgb
#'
#' @export
#' @examples
#' x <- pROC::aSAH
#' mod <- pROC::roc(x$outcome, x$s100b, levels = c("Good", "Poor"))
#' pROC_quick_plot(mod)
#'
#' \dontrun{
#' pROC_quick_plot(mod, boots = 100)
#' }

pROC_quick_plot <- function(mod, thres_method = c("youden", "closest.topleft"), col = "blue", ..., boots = 0L) {
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
    ...,
    add = FALSE
  )

  if (boots > 1L) {
    cis <- pROC_ci_sp_roc(mod, boots = boots)
    # Defaults to a grey
    ci_col <- rgb(red = 0, green = 0, blue = 0, alpha = 0.1)
    f <- "pROC" %colons% "plot.ci.sp"
    f(
      cis,
      type = "shape",
      col = ci_col
    )
  }

  # Add smoothed
  plot(pROC::smooth(mod), add = TRUE, col = col, lwd = 2, lty = 2)

  graphics::legend("bottomright",
                   legend = c("Empirical", "Smoothed"),
                   col = c(graphics::par("fg"), col),
                   lwd = 2)

  invisible(mod)
}

# Replaces pROC ::: ci.sp.roc
# pROC function is slow; uses plyr functions and has some slower applications
#   of base functions
pROC_ci_sp_roc <- function(mod, boots = 500, se = seq(0, 1, .01), conf_level = 0.95) {
  require_namespace("future")
  require_namespace("furrr")
  require_namespace("dplyr")

  # Maintain same warnings
  if (conf_level > 1 | conf_level < 0) {
    stop("'conf_level' must be within the interval [0,1].", call. = FALSE)
  }

  f <- "pROC" %colons% "roc.utils.is.perfect.curve"
  if (f(mod)) {
    warning("ci.sp() of a ROC curve with AUC == 1 is always a null interval",
            " and can be misleading.", call. = FALSE)
  }

  # Use of furrr makes bootstrapping much, much quicker
  future::plan(future::multiprocess)

  perfs <- furrr::future_map(
    seq(boots),
    "pROC" %colons% "stratified.ci.sp",
    roc = mod,
    se = se
  )

  # Set back to default
  future::plan(future::sequential)

  # Suppress messages about new names
  perfs <- suppressMessages(dplyr::bind_rows(perfs))

  # anyNA(.) quicker than any(is.na(.))
  if (anyNA(perfs)) {
    warning("NA value(s) produced during bootstrap were ignored.")
    # complete.cases(.) quicker than !apply(., 1, function(x) any(is.na(x)))
    perfs <- perfs[stats::complete.cases(perfs), ]
  }

  cl <- (1 - conf_level) / 2
  probs <- c(cl, 0.5, 1 - cl)
  # Replace apply(., 2) with vapply()
  ci <- t(vapply(perfs, stats::quantile, double(3), probs = probs))

  structure(
    ci,
    # Default Doesn't use percentage
    # rownames = paste0(sensitivities, ifelse(roc$percent, "%", "")),
    rownames = se,
    con.level = conf_level,
    boot.n = boots,
    # Default is not stratified.
    boot.stratified = FALSE,
    sensitivities = se,
    roc = mod
  )
}
