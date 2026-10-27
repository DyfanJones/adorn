# The name a function was called by, taken from the call that invoked it:
# `fib(1)` -> "fib", `obj$method(1)` or `pkg::fn(1)` -> "method" / "fn".
# Anything else (anonymous calls, `FUN` inside lapply) uses `fallback`.
name_from_call <- function(call, fallback) {
  head <- if (is.call(call)) call[[1L]]
  if (is.call(head) && length(head) == 3L) {
    op <- head[[1L]]
    if (identical(op, quote(`$`)) || identical(op, quote(`::`)) || identical(op, quote(`:::`)) || identical(op, quote(`@`))) {
      head <- head[[3L]]
    }
  }
  if (is.symbol(head) || is.character(head)) {
    name <- as.character(head)
    if (name != "FUN") return(name)
  }
  fallback
}

# Dynamic state shared between `%@%` and running decorators. Only the name
# being bound lives here: running decorators keep no stack of their own. A
# decorated function's `invoke` carries its static context as an attribute, so
# the contexts in effect are found by scanning the call stack, and only when
# something asks (`decorator_context()` or an error handler).
state <- new.env(parent = emptyenv())
state$binding_name <- NULL

binding_name <- function() state$binding_name

with_binding_name <- function(name, expr) {
  old <- state$binding_name
  state$binding_name <- name
  on.exit(state$binding_name <- old)
  expr
}

# The innermost decorator running at or below frame `from`, or NULL. Scanning
# from the top stops at the first hit, so the cost does not grow with the
# depth of the call stack. The entry holds the decorator's static context, the
# frame of the decorated function's call (0 while a factory-style decorator is
# still being set up), the decoration's `holder` and the holder of the
# decorated function it wraps (`inner`), if that was itself decorated.
find_context <- function(from) {
  for (i in from:1L) {
    fn <- sys.function(i)
    ctx <- attr(fn, "adorn_ctx", exact = TRUE)
    if (!is.null(ctx)) {
      if (isTRUE(attr(fn, "adorn_setup", exact = TRUE))) {
        return(list(ctx = ctx, frame = 0L, holder = NULL, inner = NULL))
      }
      return(list(
        ctx = ctx,
        frame = i - 1L,
        holder = attr(fn, "adorn_holder", exact = TRUE),
        inner = attr(fn, "adorn_inner", exact = TRUE)
      ))
    }
  }
  NULL
}

# The name of the function an entry is running. A function called by another
# decorator keeps the name that decorator resolved rather than the `f(...)`
# call inside its wrapper, so every level of a chain reports the name of the
# outermost call. Looking outward only happens when the function was handed to
# a decorator, which `decorate_function()` records on the holder. The result
# is then cached in the frame of the call it belongs to, so each level of a
# chain is resolved once and the next one down finds it there. The frame is
# gone when the call returns, so nothing needs clearing. Functions that were
# never wrapped skip all of this.
context_name <- function(entry) {
  if (entry$frame == 0L) {
    return(entry$ctx$name)
  }
  if (!isTRUE(entry$holder$wrapped)) {
    return(name_from_call(sys.call(entry$frame), entry$ctx$name))
  }
  frame_env <- sys.frame(entry$frame)
  name <- frame_env$.adorn_name
  if (is.null(name)) {
    outer <- find_context(entry$frame - 1L)
    name <- if (!is.null(outer) && identical(outer$inner, entry$holder)) {
      context_name(outer)
    } else {
      name_from_call(sys.call(entry$frame), entry$ctx$name)
    }
    frame_env$.adorn_name <- name
  }
  name
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
  entry <- find_context(sys.nframe())
  if (is.null(entry)) {
    stop(
      "`decorator_context()` can only be used inside a decorator.",
      call. = FALSE
    )
  }
  ctx <- entry$ctx
  ctx$name <- context_name(entry)
  ctx
}
