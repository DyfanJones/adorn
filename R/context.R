# The name a function was called by, taken from the call that invoked it:
# `fib(1)` -> "fib", `obj$method(1)` or `pkg::fn(1)` -> "method" / "fn".
# Anything else (anonymous calls, `FUN` inside lapply) uses `fallback`.
name_from_call <- function(call, fallback) {
  head <- if (is.call(call)) call[[1L]]
  if (
    is.call(head) &&
      length(head) == 3L &&
      as.character(head[[1L]])[[1L]] %in% c("$", "::", ":::", "@")
  ) {
    head <- head[[3L]]
  }
  if (is.symbol(head) || is.character(head)) {
    name <- as.character(head)
    if (name != "FUN") return(name)
  }
  fallback
}

# Dynamic state shared between `%@%` and running decorators.
state <- new.env(parent = emptyenv())
state$binding_name <- NULL
state$contexts <- list()

binding_name <- function() state$binding_name

with_binding_name <- function(name, expr) {
  old <- state$binding_name
  state$binding_name <- name
  on.exit(state$binding_name <- old)
  expr
}

push_context <- function(ctx) {
  state$contexts <- c(list(ctx), state$contexts)
  invisible()
}

pop_context <- function() {
  state$contexts <- state$contexts[-1L]
  invisible()
}

#' Information about the function being decorated
#'
#' Call this from inside a decorator to find out which function it is
#' currently wrapping.
#'
#' @return A list with `name` (the name the function is bound to, or
#'   `"<anonymous>"`), `decorator` (the decorator's name, see [decorator()]) and `fn` (the
#'   function being wrapped).
#' @export
#' @examples
#' who <- decorator(function(f, ...) {
#'   message("calling ", decorator_context()$name)
#'   f(...)
#' })
#' hello <- who %@% function() "hi"
#' hello()
decorator_context <- function() {
  if (length(state$contexts) == 0L) {
    stop(
      "`decorator_context()` can only be used inside a decorator.",
      call. = FALSE
    )
  }
  state$contexts[[1L]]
}
