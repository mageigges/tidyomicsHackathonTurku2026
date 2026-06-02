decompose_predicate <- function(q) {
  expr <- rlang::quo_get_expr(q[[1]])

  if (!rlang::is_call(expr) || length(expr) != 3L) {
    stop("Only binary expressions like `col == val` are supported.")
  }

  list(
    col      = rlang::as_string(expr[[2]]),
    operator = rlang::as_string(expr[[1]]),
    value    = expr[[3]]
  )
}

filter <- function(.data, ..., .feature = NULL) {

    if (!is.null(.feature)) {
        return(.data[ ,.data$var[1] == .feature])
    } else {
        dots <- enquos(...)
        pred <- decompose_predicate(dots)

        op_fn <- match.fun(pred$operator)

        col_values <- .data$obs[[pred$col]]
        keep <- op_fn(col_values, pred$value)
        return(.data[keep, ])
    }
}
