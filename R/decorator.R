#' Create a decorator
#'
#'
#' `decorator()` turns an ordinary function into a decorator. The first
#' argument of `wrapper` receives the function being decorated. There are two
#' styles, chosen by whether `wrapper` has a `...` argument:
#'
#' * **Call style**, `function(f, ..., <params>)`: the body runs on every call.
#'   `...` holds the arguments of that call; forward them with `f(...)`.
#' * **Factory style**, `function(f, <params>)`: the body runs once, when the
#'   function is decorated, and returns the replacement function. Use this
#'   when the decorator needs state, such as a cache.
#'
#' Arguments after `...` (call style) or after `f` (factory style) are
#' decorator parameters. Set them by calling the decorator:
#' `retry(times = 5)`, for a decorator named `retry`.
#'
#' If your decorator stores the result of `f(...)` before returning it, use
#' `withVisible()` to keep functions that return `invisible()` quiet.
#'
#' Whatever the style, the decorated function keeps the formal arguments of
#' the original, so argument matching, `missing()`, defaults and
#' introspection all behave as before.
#'
#' @param wrapper A function, as described above.
#' @param name Name shown when printing the decorator and in
#'   [decorators()]. By default it is taken from how the decorator is written
#'   when applied with `%@%` (`timed %@% f` gives `"timed"`, and
#'   `retry(times = 5) %@% f` gives `"retry"`). Set it to override that;
#'   `"decorator"` is the last resort.
#' @return A decorator: a function that decorates a function when called with
#'   one, and returns a configured decorator when called with parameters.
#' @export
#' @examples
#' # Call style: runs on every call
#' shout <- decorator(function(f, ...) toupper(f(...)))
#'
#' # Call style with a parameter
#' repeated <- decorator(function(f, ..., times = 2) {
#'   rep(f(...), times)
#' })
#'
#' # Factory style: keeps state between calls
#' counted <- decorator(function(f) {
#'   n <- 0
#'   function(...) {
#'     n <<- n + 1
#'     message("call ", n)
#'     f(...)
#'   }
#' })
#'
#' greet <- shout %@% function(who) paste("hello", who)
#' greet("world")
#'
#' ones <- repeated(times = 3) %@% function() 1
#' ones()
decorator <- function(wrapper, name = NULL) {
  stopifnot(is.function(wrapper))
  fmls <- names(formals(wrapper))
  if (length(fmls) == 0L || identical(fmls[[1L]], "...")) {
    stop(
      "`wrapper` must take the function to decorate as its first argument.",
      call. = FALSE
    )
  }
  dots <- match("...", fmls)
  if (is.na(dots)) {
    style <- "factory"
    params <- fmls[-1L]
  } else {
    if (dots != 2L) {
      stop(
        "In a call-style decorator, `...` must directly follow the function argument.",
        call. = FALSE
      )
    }
    style <- "call"
    params <- fmls[seq_along(fmls) > dots]
  }

  new_decorator(wrapper, style, params, list(), name)
}

new_decorator <- function(wrapper, style, params, config, name) {
  self <- function(...) {
    args <- list(...)
    if (length(args) == 1L && is.function(args[[1L]]) && is.null(names(args))) {
      return(decorate_function(args[[1L]], wrapper, style, config, name))
    }
    if (length(args) == 0L) {
      return(object)
    }
    nms <- names(args) %||% rep("", length(args))
    # Unnamed arguments fill the remaining parameters in order.
    free <- setdiff(params, nms)
    nms[nms == ""] <- free[seq_len(sum(nms == ""))]
    bad <- setdiff(nms, params)
    if (length(bad) > 0L || anyNA(nms)) {
      stop(
        sprintf(
          "Unknown parameter(s) for decorator `%s`: %s. Available: %s.",
          name %||% "decorator",
          paste(bad, collapse = ", "),
          if (length(params)) paste(params, collapse = ", ") else "none"
        ),
        call. = FALSE
      )
    }
    names(args) <- nms
    new_decorator(wrapper, style, params, utils::modifyList(config, args), name)
  }
  object <- structure(self, class = c("adorn_decorator", "function"))
  object
}

#' @export
print.adorn_decorator <- function(x, ...) {
  env <- environment(x)
  cat(sprintf("<decorator: %s> [%s style]\n", env$name %||% "decorator", env$style))
  if (length(env$params)) {
    cat("  parameters:", paste(env$params, collapse = ", "), "\n")
  }
  if (length(env$config)) {
    cat(
      "  configured:",
      paste(
        names(env$config),
        vapply(env$config, format, ""),
        sep = " = ",
        collapse = ", "
      ),
      "\n"
    )
  }
  invisible(x)
}

decorate_function <- function(f, wrapper, style, config, dec_name) {
  dec_name <- dec_name %||% "decorator"
  # Bake the configured parameters into the wrapper's defaults.
  w <- wrapper
  if (length(config)) {
    quoted <- lapply(config, function(v) {
      if (is.language(v)) call("quote", v) else v
    })
    formals(w)[names(config)] <- quoted
  }

  # The name known when decorating (from `%@%`), used until a call reveals
  # the name the function is actually called by.
  static_name <- binding_name() %||% "<anonymous>"
  ctx <- list(name = static_name, decorator = dec_name, fn = f)

  # Frameworks such as R6 give each method a new enclosing environment (the
  # object). The decorated function carries that environment, but `f` would
  # still have the old one, so at call time `f` is re-pointed at whatever
  # environment the wrapper currently has.
  f_env <- environment(f)
  rebind <- function(caller) {
    env <- environment(caller)
    if (is.null(f_env) || is.null(env) || identical(env, f_env)) {
      return(f)
    }
    environment(f) <- env
    f
  }

  holder <- new.env(parent = emptyenv())
  holder$fn <- f

  if (style == "call") {
    run <- NULL
  } else {
    # A stand-in with `f`'s signature that always calls the current binding.
    stand_in <- forward_formals(f, function(...) holder$fn(...))
    push_context(ctx)
    on.exit(pop_context())
    run <- with_decorator_errors(w(stand_in), quote(w(stand_in)), dec_name, static_name)
    if (!is.function(run)) {
      stop(
        sprintf("Decorator `%s` must return a function.", dec_name),
        call. = FALSE
      )
    }
  }

  invoke <- function(...) {
    caller <- sys.function(-1L)
    # When another decorator is calling us, keep the name it resolved rather
    # than the `f(...)` call inside its wrapper.
    outer <- if (length(state$contexts)) state$contexts[[1L]]
    ctx$name <- if (
      !is.null(outer) &&
        identical(outer$fn, caller, ignore.environment = TRUE)
    ) {
      outer$name
    } else {
      name_from_call(sys.call(-1L), static_name)
    }
    push_context(ctx)
    on.exit(pop_context(), add = TRUE)
    fn <- rebind(caller)
    if (style == "call") {
      return(with_decorator_errors(
        w(fn, ...),
        quote(w(fn, ...)),
        dec_name,
        ctx$name
      ))
    }
    previous <- holder$fn
    holder$fn <- fn
    on.exit(holder$fn <- previous, add = TRUE)
    with_decorator_errors(run(...), quote(run(...)), dec_name, ctx$name)
  }

  out <- forward_formals(f, invoke)

  prev <- attr(f, "adorn")
  attributes(out) <- c(
    attributes(f)[setdiff(names(attributes(f)), "adorn")],
    list(
      adorn = list(
        original = prev$original %||% f,
        stack = c(dec_name, prev$stack)
      )
    )
  )
  out
}

# Errors raised by a decorator's own code report the internal call (`w()`),
# which says nothing about what failed. Re-signal them as coming from the
# decorator, naming the function it was wrapping. Only errors whose call is
# `wrapper_call` are touched; errors from `f` and from stop(call. = FALSE)
# pass through unchanged. The condition keeps its classes and the original is
# kept as `parent`.
with_decorator_errors <- function(expr, wrapper_call, dec_name, fn_name) {
  withCallingHandlers(
    expr,
    error = function(e) {
      if (!identical(conditionCall(e), wrapper_call)) {
        return()
      }
      stop(structure(
        class = c("adorn_decorator_error", class(e)),
        list(
          message = sprintf(
            "%s\nDecorator `%s` was wrapping `%s()`.",
            conditionMessage(e),
            dec_name,
            fn_name
          ),
          call = as.call(list(as.name(dec_name))),
          parent = e
        )
      ))
    }
  )
}

# Build a function with the same formals as `f` whose body calls `invoke`
# with every formal forwarded. `invoke` is embedded in the body as a literal
# so the wrapper still works if something (R6, for instance) replaces its
# enclosing environment.
forward_formals <- function(f, invoke) {
  fmls <- formals(f) %||% formals(args(f))
  nms <- names(fmls)
  args <- lapply(nms, as.name)
  names(args) <- ifelse(nms == "...", "", nms)
  body <- as.call(c(list(invoke), args))
  out <- as.function(c(fmls %||% list(), list(body)))
  environment(out) <- environment(f) %||% baseenv()
  out
}

#' Apply decorators to a function
#'
#' `%@%` reads left to right: in `a %@% b %@% f`, `a` is the outermost
#' decorator and `f` the innermost function. A plain `function(f)` that returns a function also works as a
#' decorator on the left-hand side.
#'
#' @param lhs A decorator.
#' @param rhs A function, or another decorator to compose with.
#' @return The decorated function, or a composed decorator if `rhs` is
#'   itself a decorator.
#' @export
#' @name apply-decorator
#' @aliases %@%
#' @examples
#' loud <- decorator(function(f, ...) toupper(f(...))) %@% function() "hi"
#' loud()
`%@%` <- function(lhs, rhs) {
  if (!is.function(lhs)) {
    stop("Left-hand side of `%@%` must be a decorator.", call. = FALSE)
  }
  lhs <- name_decorator(lhs, substitute(lhs))
  if (inherits(rhs, "adorn_decorator")) {
    return(compose(lhs, name_decorator(rhs, substitute(rhs))))
  }
  if (!is.function(rhs)) {
    stop("Right-hand side of `%@%` must be a function.", call. = FALSE)
  }
  fn_name <- binding_name()
  if (is.null(fn_name)) {
    e <- substitute(rhs)
    fn_name <- if (is.symbol(e)) as.character(e)
  }
  with_binding_name(fn_name, lhs(rhs))
}

# Name an unnamed decorator after the expression it was written as. An
# explicit `name` given to `decorator()` is never overwritten.
name_decorator <- function(dec, expr) {
  if (!inherits(dec, "adorn_decorator")) {
    return(dec)
  }
  env <- environment(dec)
  nm <- infer_name(expr)
  if (is.null(env$style) || !is.null(env$name) || is.null(nm)) {
    return(dec)
  }
  new_decorator(env$wrapper, env$style, env$params, env$config, nm)
}

# `timed` -> "timed"; `retry(times = 5)` -> "retry"; `pkg::timed` -> "timed".
infer_name <- function(expr) {
  if (is.symbol(expr)) {
    return(as.character(expr))
  }
  if (!is.call(expr) || !is.symbol(expr[[1L]])) {
    return(NULL)
  }
  fn <- as.character(expr[[1L]])
  if (fn %in% c("::", ":::")) {
    return(as.character(expr[[3L]]))
  }
  if (fn %in% c("$", "[[", "[", "@", "(", "function")) {
    return(NULL)
  }
  fn
}

compose <- function(outer, inner) {
  self <- function(...) {
    args <- list(...)
    if (length(args) == 1L && is.function(args[[1L]]) && is.null(names(args))) {
      return(outer(inner(args[[1L]])))
    }
    stop(
      "Composed decorators cannot be configured; configure each one first.",
      call. = FALSE
    )
  }
  structure(self, class = c("adorn_decorator", "function"))
}

#' Inspect a decorated function
#'
#' `layers()` is an alias for `decorators()`, and `strip()` for
#' `undecorate()`.
#'
#' @param f A function.
#' @return `undecorate()` returns the original, undecorated function (or `f`
#'   itself if it is not decorated). `decorators()` returns the names of the
#'   decorators applied, outermost first.
#' @export
undecorate <- function(f) attr(f, "adorn")$original %||% f

#' @rdname undecorate
#' @export
decorators <- function(f) attr(f, "adorn")$stack %||% character()
