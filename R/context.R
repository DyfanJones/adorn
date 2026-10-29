# The name a function was called by: `fib(1)` -> "fib", `obj$method(1)` ->
# "method", `pkg::fn(1)` -> "fn". Anything else (anonymous calls, `FUN` in
# lapply) gives `fallback`.
name_from_call <- function(call, fallback) {
  head <- if (is.call(call)) call[[1L]]
  # The common case, `fib(1)`, needs no further inspection.
  if (is.symbol(head)) {
    name <- as.character(head)
    return(if (name == "FUN") fallback else name)
  }
  if (is.call(head) && length(head) == 3L) {
    op <- head[[1L]]
    if (
      identical(op, quote(`$`)) ||
        identical(op, quote(`::`)) ||
        identical(op, quote(`:::`)) ||
        identical(op, quote(`@`))
    ) {
      head <- head[[3L]]
    }
  }
  if (is.symbol(head) || is.character(head)) {
    name <- as.character(head)
    if (name != "FUN") return(name)
  }
  fallback
}

# Holds the name being bound, for `%@%` and `decorate()`. Running decorators
# keep no stack: each `invoke` carries its context as an attribute, and the
# call stack is scanned only when `decorator_context()` asks.
state <- new.env(parent = emptyenv())
state$binding_name <- NULL

binding_name <- function() state$binding_name

with_binding_name <- function(name, expr) {
  old <- state$binding_name
  state$binding_name <- name
  on.exit(state$binding_name <- old)
  expr
}

# The innermost decorator running at or below frame `from`, or NULL. The entry
# holds the static context, the frame of the decorated call (0 while a
# factory-style decorator is being set up), the decoration's `holder`, and the
# `inner` holder if the wrapped function was itself decorated.
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

# The name of the function an entry is running. In a chain of decorators every
# level reports the name of the outermost call, not the `f(...)` inside its
# wrapper. Only wrapped functions look outward; the result is cached in the
# call's frame so each level is resolved once.
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
  n <- sys.nframe()
  # Called straight from a decorator body, the frame two below is `invoke` (or
  # `setup`, while a factory-style decorator is being set up).
  fn <- if (n >= 3L) sys.function(n - 2L)
  ctx <- attr(fn, "adorn_ctx", exact = TRUE)

  if (is.null(ctx)) {
    # Called from a helper or the decorated function: search the stack.
    entry <- find_context(n)
    if (is.null(entry)) {
      stop(
        "`decorator_context()` can only be used inside a decorator.",
        call. = FALSE
      )
    }
    ctx <- entry$ctx
    ctx$name <- context_name(entry)
    return(ctx)
  }
  if (isTRUE(attr(fn, "adorn_setup", exact = TRUE))) {
    return(ctx)
  }

  holder <- attr(fn, "adorn_holder", exact = TRUE)
  if (isTRUE(holder$wrapped)) {
    entry <- list(
      ctx = ctx,
      frame = n - 3L,
      holder = holder,
      inner = attr(fn, "adorn_inner", exact = TRUE)
    )
    ctx$name <- context_name(entry)
  } else {
    ctx$name <- name_from_call(sys.call(n - 3L), ctx$name)
  }
  ctx
}
