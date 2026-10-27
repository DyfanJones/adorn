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

# Names that `invoke` looks up in `call_env`, including the base functions it
# calls by name (faster under the byte compiler than embedded literals). A
# decorator with one of these names is bound under a mangled name instead.
reserved_names <- c(
  ".holder", ".previous", ".previous_env",
  "parent.env", "parent.frame", "identical",
  "{", "if", "<-", "!", "$", "$<-", "on.exit", "return"
)

with_env <- function(fn, env) {
  environment(fn) <- env
  fn
}

# `name`, unless it is `taken`, in which case a dotted variant.
avoid <- function(name, taken) if (identical(name, taken)) paste0(".", name) else name

decorate_function <- function(f, wrapper, style, config, dec_name) {
  dec_name <- dec_name %||% "decorator"
  # Bake configured parameters into the wrapper's defaults.
  w <- wrapper
  if (length(config)) {
    quoted <- lapply(config, function(v) {
      if (is.language(v)) call("quote", v) else v
    })
    formals(w)[names(config)] <- quoted
  }

  # Used until a call reveals the name the function is called by.
  static_name <- binding_name() %||% "<anonymous>"
  ctx <- list(name = static_name, decorator = dec_name, fn = f)

  # R6 gives each method a new enclosing environment (the object), so at call
  # time `f` is re-pointed at the wrapper's current environment. Primitives
  # have none, so `baseenv()` is compared (see `forward_formals()`).
  f_env <- environment(f) %||% baseenv()

  holder <- new.env(parent = emptyenv())
  holder$fn <- f
  # Environment `holder$fn` is currently bound to.
  holder$env <- f_env
  # TRUE once another decorator wraps this decoration.
  holder$wrapped <- FALSE

  # The wrapper is called under the decorator's own name so errors report
  # `logged(f, ...)` rather than an internal call. Everything `invoke` needs
  # lives in `call_env`, so a decorator named `f` cannot shadow it.
  call_env <- new.env(parent = baseenv())
  call_name <- if (dec_name %in% reserved_names) paste0(".", dec_name) else dec_name
  assign(call_name, w, envir = call_env)
  call_sym <- as.name(call_name)
  # The decorated function is passed as `f`, unless the decorator is `f`.
  arg_name <- avoid("f", call_name)
  arg_sym <- as.name(arg_name)
  assign(arg_name, f, envir = call_env)
  assign(".holder", holder, envir = call_env)

  if (style == "call") {
    # Hot path: runs on every call, so no bookkeeping.
    invoke <- eval(
      bquote(function(...) {
        # The parent of the decorated function's frame is its current closure.
        if (identical(parent.env(parent.frame()), .(f_env))) {
          .(call_sym)(.(arg_sym), ...)
        } else {
          .(arg_sym) <- .(with_env)(.(arg_sym), parent.env(parent.frame()))
          .(call_sym)(.(arg_sym), ...)
        }
      }),
      call_env
    )
  } else {
    # Stand-in with `f`'s signature that calls the current `holder$fn`. Having
    # `holder` embedded in its body keeps it to one frame in any environment.
    stand_in <- forward_formals(f, call("$", holder, quote(fn)))
    stand_name <- avoid("stand_in", call_name)
    assign(stand_name, stand_in, envir = call_env)
    # Marked so `decorator_context()` works during setup.
    setup <- eval(bquote(function() .(call_sym)(.(as.name(stand_name)))), call_env)
    attr(setup, "adorn_ctx") <- ctx
    attr(setup, "adorn_setup") <- TRUE
    run <- setup()
    if (!is.function(run)) {
      stop(
        sprintf("Decorator `%s` must return a function.", dec_name),
        call. = FALSE
      )
    }
    # From here the name refers to the returned function.
    assign(call_name, run, envir = call_env)
    invoke <- eval(
      bquote(function(...) {
        env <- parent.env(parent.frame())
        # Usual case: the stand-in is already bound to this environment.
        bound <- .(holder)$env
        if (identical(env, bound)) {
          return(.(call_sym)(...))
        }
        # Otherwise (e.g. R6 objects) rebind the stand-in for this call.
        .previous <- .holder$fn
        .previous_env <- .holder$env
        .holder$fn <- .(with_env)(.(arg_sym), env)
        .holder$env <- env
        on.exit({
          .holder$fn <- .previous
          .holder$env <- .previous_env
        })
        .(call_sym)(...)
      }),
      call_env
    )
  }
  attr(invoke, "adorn_ctx") <- ctx
  attr(invoke, "adorn_holder") <- holder
  # The decoration this one wraps, if `f` was already decorated.
  attr(invoke, "adorn_inner") <- attr(f, "adorn")$holder

  out <- forward_formals(f, invoke)

  prev <- attr(f, "adorn")
  if (!is.null(prev$holder)) {
    prev$holder$wrapped <- TRUE
  }
  attributes(out) <- c(
    attributes(f)[setdiff(names(attributes(f)), "adorn")],
    list(
      adorn = list(
        original = prev$original %||% f,
        stack = c(dec_name, prev$stack),
        holder = holder
      )
    )
  )
  out
}

# Build a function with `f`'s formals whose body calls `invoke` with every
# formal forwarded. `invoke` (a function or expression) is embedded as a
# literal, so the result survives its environment being replaced (e.g. by R6).
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
#' @param name The name to give the decorated function, for
#'   `decorator_context()$name`. Normally the name is read from `rhs` when it is
#'   a symbol, so you only need this for an anonymous function. It is also what
#'   the development version of S7's `:=` supplies:
#'   `add_one := dec %@% function(x) x + 1` names the function `add_one`. It has
#'   no effect when `rhs` is a decorator.
#' @return The decorated function, or a composed decorator if `rhs` is
#'   itself a decorator.
#' @usage `\%@\%`(lhs, rhs, name = NULL)
#' @export
#' @name apply-decorator
#' @aliases %@%
#' @examples
#' loud <- decorator(function(f, ...) toupper(f(...))) %@% function() "hi"
#' loud()
`%@%` <- function(lhs, rhs, name = NULL) {
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
  if (!is.null(name) && !(is.character(name) && length(name) == 1L && !is.na(name))) {
    stop("`name` must be a single string.", call. = FALSE)
  }
  # Precedence: explicit `name`, then `decorate()`, then the symbol `rhs`.
  fn_name <- name %||% binding_name()
  if (is.null(fn_name)) {
    e <- substitute(rhs)
    fn_name <- if (is.symbol(e)) as.character(e)
  }
  with_binding_name(fn_name, lhs(rhs))
}

# Name an unnamed decorator after the expression it was written as; an
# explicit `name` is kept.
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
