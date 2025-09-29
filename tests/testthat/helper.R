muffle_cnd_conditions <- function(expr) {
  withCallingHandlers(
    expr,
    "cnd::condition" = function(cond) tryInvokeRestart("muffleCondition")
  )
}
