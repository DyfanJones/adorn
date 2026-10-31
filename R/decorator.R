#' Create a decorator
#'
#'
#' `decorator()` turns an ordinary function into a decorator. The first
#' argument of `wrapper` receives the function being decorated, whatever it is
#' named, and it can't have a default. adorn can't check the name, so
#' `function(bob, f)` is accepted but treats `bob` as the function; put
#' parameters after the first argument. There are two styles, chosen by whether
#' `wrapper` has a `...` argument:
#'
#' * **Call style**, `function(f, ..., <params>)`: the body runs on every call.
#'   `...` holds the arguments of that call; forward them with `f(...)`.
#' * **Factory style**, `function(f, <params>)`: the body runs once, when the
#'   function is decorated, and returns the replacement function. Use this
#'   when the decorator needs state, such as a cache.
#'
#' Arguments after `...` (call style) or after `f` (factory style) are
#' decorator parameters. Set them by calling the decorator with named
#' arguments: `retry(times = 5)`, for a decorator named `retry`.
#'
#' The decorator takes the formals of `wrapper` without the `...`, so
#' `args(retry)` shows `function(f, times = 3)` and editors can complete it.
#' A function passed to the decorator is always the one to decorate, never a
#' parameter value, so give function-valued parameters by name:
#' `on_call(hook = my_hook)`. If the first argument is not a function, it is
#' taken as the first parameter, so `retry(5)` means `retry(times = 5)`.
#'
#' If your decorator stores the result of `f(...)` before returning it, use
#' `withVisible()` to keep functions that return `invisible()` quiet.
#'
#' By default the decorated function keeps the formal arguments of the
#' original, so argument matching, `missing()` and defaults behave as before.
#' A factory can instead set `formals = "replace"` to give the decorated
#' function the formals of the function it returns, which lets it change the
#' signature (see `vignette("partial-decorators")`).
#'
#' @param wrapper A function, as described above. Its first argument receives
#'   the function to decorate, so it can't have a default.
#' @param name Name shown when printing the decorator and in
#'   [decorators()]. By default it is taken from how the decorator is written
#'   when applied with `%@%` (`timed %@% f` gives `"timed"`, and
#'   `retry(times = 5) %@% f` gives `"retry"`). Set it to override that;
#'   `"decorator"` is the last resort.
#' @param formals What happens to the formal arguments of the decorated
#'   function. `"keep"` (the default) keeps those of the original. `"replace"`
#'   swaps them for those of the function a factory returns; it is an error
#'   for call style, which has no returned function. With `"replace"`, matching,
#'   defaults and `missing()` follow the returned function, so getting them
#'   right is up to the decorator. S3, S4 and S7 methods must still match their
#'   generic.
#' @return A decorator: a function with the formals `function(f, <params>)`.
#'   It decorates `f` when given one, and returns a configured decorator when
#'   `f` is missing.
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
#'
#' # Replacing the formals: the decorated function takes only `y`
#' two_to := decorator(function(f) function(y) f(2, y), formals = "replace")
#' pow := two_to %@% function(x, y) x^y
#' pow(3)
decorator <- function(
  wrapper,
  name = NULL,
  formals = c("keep", "replace")
) {
  # `match.arg()` is slow, so it is only used for a value the user gave.
  formals_mode <- if (identical(formals, formals_choices)) {
    "keep"
  } else {
    match.arg(formals)
  }
  if (!is.function(wrapper)) {
    stop("`wrapper` must be a function.", call. = FALSE)
  }
  wrapper_formals <- formals(wrapper)
  fmls <- names(wrapper_formals)
  if (length(fmls) == 0L || identical(fmls[[1L]], "...")) {
    stop(
      "`wrapper` must take the function to decorate as its first argument.",
      call. = FALSE
    )
  }
  # The caller always supplies the function to decorate, so a default on the
  # first argument means a parameter was put in the wrong place.
  # (An empty default is the empty symbol, which can't be stored in a variable.)
  if (nzchar(as.character(wrapper_formals[1L]))) {
    stop(
      sprintf(
        paste(
          "The first argument of `wrapper` receives the function to decorate,",
          "so `%s` can't have a default.",
          "Put decorator parameters after it, e.g. `function(f, param = ...)`."
        ),
        fmls[[1L]]
      ),
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

  if (style == "call" && formals_mode == "replace") {
    stop(
      paste(
        "`formals = \"replace\"` needs a factory-style decorator.",
        "A call-style `wrapper` has no returned function to take the formals from."
      ),
      call. = FALSE
    )
  }

  new_decorator(wrapper, style, params, list(), name, formals_mode)
}

new_decorator <- function(
  wrapper,
  style,
  params,
  config,
  name,
  formals_mode = "keep"
) {
  # The decorator takes the wrapper's own formals (minus any `...`), so
  # `args()` and code completion show how to use it. The decorated function's
  # hot path is untouched.
  fmls <- formals(wrapper)
  fmls <- fmls[c(1L, match(params, names(fmls)))]
  fmls[1L] <- list(quote(expr = ))
  if (length(config)) {
    fmls[names(config)] <- lapply(config, function(v) {
      if (is.language(v)) call("quote", v) else v
    })
  }
  nms <- names(fmls)

  if (any(startsWith(nms, ".a_"))) {
    # Rare: a parameter could shadow the internal `.a_` names, so embed
    # everything the body needs as literals instead.
    body <- literal_body(
      nms,
      wrapper,
      style,
      params,
      config,
      name,
      formals_mode
    )
  } else {
    # The body depends only on the names, so it is built once per set of names
    # and reused. Its values are read from the variables below.
    key <- paste(nms, collapse = "\r")
    body <- body_cache[[key]]
    if (is.null(body)) {
      body <- body_cache[[key]] <- make_body(nms[[1L]], params)
    }
    .a_new <- new_decorator
    .a_decorate <- decorate_function
    .a_shift <- shift_args
    .a_wrapper <- wrapper
    .a_style <- style
    .a_params <- params
    .a_name <- name
    .a_formals_mode <- formals_mode
    .a_config <- config
  }
  self <- as.function(c(fmls, list(body)), envir = environment())
  class(self) <- decorator_class
  self
}

# Cache of decorator bodies, keyed by the decorator's formal names.
body_cache <- new.env(parent = emptyenv())

clear_body_cache <- function() {
  rm(list = ls(body_cache, all.names = TRUE), envir = body_cache)
  invisible()
}

# Body that reads its values from `.a_` variables in the decorator's
# environment. With no parameters the configuration never changes, so the body
# is just the dispatch.
make_body <- function(fn, params) {
  if (length(params) == 0L) {
    return(with_shift(
      fn,
      dispatch_body(fn, quote(.a_config)),
      quote(.a_params)
    ))
  }
  cfg <- quote(.a_cfg)
  steps <- lapply(params, function(p) {
    subst(param_template, c(list(.p = as.name(p), .cfg = cfg, .key = p), prims))
  })
  rest <- as.call(c(
    list(prims$.brace),
    list(as.call(list(prims$.assign, cfg, quote(.a_config)))),
    steps,
    list(dispatch_body(fn, cfg))
  ))
  with_shift(fn, rest, quote(.a_params))
}

# A first argument that is not a function is not the function to decorate, so
# the call is a configuration whose leading arguments are parameters (`test("a")`
# means `test(foo = "a")`). This check comes before any parameter is read so
# nothing is evaluated twice.
with_shift <- function(fn, rest, params, shift_fn = quote(.a_shift)) {
  fn_sym <- as.name(fn)
  shift <- subst(
    shift_template,
    c(
      list(
        .fn = fn_sym,
        .fname = fn,
        .shift = shift_fn,
        .params = params
      ),
      prims
    )
  )
  subst(
    needs_shift_template,
    c(list(.fn = fn_sym, .shift_call = shift, .rest = rest), prims)
  )
}

# Re-runs the call with an empty first argument, so `f` is missing and R's own
# matching hands the remaining arguments to the parameters.
shift_args <- function(cl, value, fname, self, env, params) {
  args <- as.list(cl)[-1L]
  arg_names <- names(args) %||% character(length(args))
  named <- arg_names[nzchar(arg_names)]
  pos <- if (any(startsWith(fname, named))) {
    NA_integer_
  } else {
    match("", arg_names)
  }
  if (length(params) == 0L || is.na(pos)) {
    stop(
      if (length(params) == 0L) {
        sprintf("`%s` must be a function.", fname)
      } else {
        sprintf(
          "`%s` must be a function. Name decorator parameters, e.g. `%s(%s = ...)`.",
          fname,
          as.character(cl[[1L]])[[1L]],
          params[[1L]]
        )
      },
      call. = FALSE
    )
  }
  # The value was evaluated already; reuse it, quoting code so it stays data.
  args[pos] <- list(if (is.language(value)) call("quote", value) else value)
  # Report errors against the user's call, not the rebuilt one.
  tryCatch(
    eval(as.call(c(list(self, quote(expr = )), args)), env),
    error = function(e) {
      cond <- list(message = e$message, call = cl)
      class(cond) <- c("error", "condition")
      stop(cond)
    }
  )
}

# With a function: decorate it. Without: return a configured decorator.
dispatch_body <- function(fn, cfg) {
  subst(
    dispatch_template,
    c(
      list(
        .fn = as.name(fn),
        .cfg = cfg,
        .new = quote(.a_new),
        .decorate = quote(.a_decorate),
        .wrapper = quote(.a_wrapper),
        .style = quote(.a_style),
        .params = quote(.a_params),
        .name = quote(.a_name),
        .formals_mode = quote(.a_formals_mode)
      ),
      prims
    )
  )
}

# Same body with every value embedded as a literal, so no parameter name can
# shadow it. Slower to build, so only used when a name could collide.
literal_body <- function(
  nms,
  wrapper,
  style,
  params,
  config,
  name,
  formals_mode
) {
  # Local variable holding the configuration; must not collide with a param.
  cfg <- as.name(avoid_all(".config", nms))
  steps <- lapply(params, function(p) {
    subst(param_template, c(list(.p = as.name(p), .cfg = cfg, .key = p), prims))
  })
  dispatch <- subst(
    dispatch_template,
    c(
      list(
        .fn = as.name(nms[[1L]]),
        .cfg = cfg,
        .new = new_decorator,
        .decorate = decorate_function,
        .wrapper = wrapper,
        .style = style,
        .params = params,
        .name = name,
        .formals_mode = formals_mode
      ),
      prims
    )
  )
  rest <- as.call(c(
    list(prims$.brace),
    list(as.call(list(prims$.assign, cfg, config))),
    steps,
    list(dispatch)
  ))
  with_shift(nms[[1L]], rest, params, shift_args)
}

decorator_class <- c("adorn_decorator", "function")
formals_choices <- c("keep", "replace")

# Templates for the body of a decorator; `subst()` fills in the `.name`
# placeholders. `substitute()` is far cheaper than `bquote()`.
#
# The body never looks up a function by name: `if`, `{`, `<-`, `!` and the rest
# are embedded as the functions themselves (the `prims` below). The body runs
# in the same frame as the decorator's parameters, so a parameter holding a
# function and named `if` or `list` would otherwise be called instead.
param_template <- quote(
  .if(.not(.missing(.p)), .assign(.cfg, .set_config(.cfg, .key, .p)))
)

dispatch_template <- quote(
  .if(
    .missing(.fn),
    .new(.wrapper, .style, .params, .cfg, .name, .formals_mode),
    .decorate(.fn, .wrapper, .style, .cfg, .name, .formals_mode)
  )
)

shift_template <- quote(
  .shift(.sys_call(), .fn, .fname, .sys_function(), .parent_frame(), .params)
)

needs_shift_template <- quote(
  .if(.and(.not(.missing(.fn)), .not(.is_function(.fn))), .shift_call, .rest)
)

prims <- list(
  .and = `&&`,
  .is_function = is.function,
  .sys_call = sys.call,
  .sys_function = sys.function,
  .parent_frame = parent.frame,
  .if = `if`,
  .not = `!`,
  .missing = missing,
  .assign = `<-`,
  .brace = `{`,
  .set_config = function(config, key, value) {
    config[key] <- list(value)
    config
  }
)

subst <- function(template, values) {
  eval(call("substitute", template, values))
}

# `name`, with leading dots added until it is not in `taken`.
avoid_all <- function(name, taken) {
  # Usually `name` is free. One dot is almost always enough; the loop only
  # runs if the dotted name is taken too.
  if (any(name == taken)) {
    name <- sprintf(".%s", name)
    while (any(name == taken)) {
      name <- sprintf(".%s", name)
    }
  }
  name
}

#' @export
print.adorn_decorator <- function(x, ...) {
  env <- environment(x)
  cat(sprintf(
    "<decorator: %s> [%s style]\n",
    env$name %||% "anonymous",
    env$style
  ))
  if (!identical(env$formals_mode, "keep")) {
    cat(sprintf("  formals: %s\n", env$formals_mode))
  }
  if (length(env$params)) {
    cat(sprintf("  parameters: %s\n", paste(env$params, collapse = ", ")))
  }
  if (length(env$config)) {
    config <- paste(
      names(env$config),
      vapply(env$config, format, ""),
      sep = " = "
    )
    cat(sprintf("  configured: %s\n", paste(config, collapse = ", ")))
  }
  invisible(x)
}

# Names that `invoke` looks up in `call_env`, including the base functions it
# calls by name (faster under the byte compiler than embedded literals). A
# decorator with one of these names is bound under a mangled name instead.
reserved_names <- c(
  ".holder",
  ".previous",
  ".previous_env",
  "parent.env",
  "parent.frame",
  "identical",
  "{",
  "if",
  "<-",
  "!",
  "$",
  "$<-",
  "on.exit",
  "return"
)

with_env <- function(fn, env) {
  environment(fn) <- env
  fn
}

# `name`, unless it is `taken`, in which case a dotted variant.
avoid <- function(name, taken) {
  if (identical(name, taken)) paste0(".", name) else name
}

# Wrapper templates. `fill()` swaps the `.name` placeholders for values with
# `substitute()`, which is far cheaper than `bquote()` walking the tree in R.
# The `srcref` that a source-keeping build adds to `function` is dropped so the
# result prints as the real code.
no_srcref <- function(x) if (length(x) == 4L) x[-4L] else x

call_template <- no_srcref(quote(function(...) {
  if (identical(parent.env(parent.frame()), .f_env)) {
    .call(.arg, ...)
  } else {
    .arg <- .with_env(.arg, parent.env(parent.frame()))
    .call(.arg, ...)
  }
}))

setup_template <- no_srcref(quote(function() .call(.stand_in)))

factory_template <- no_srcref(quote(function(...) {
  env <- parent.env(parent.frame())
  bound <- .holder_obj$env
  if (identical(env, bound)) {
    return(.call(...))
  }
  .previous <- .holder$fn
  .previous_env <- .holder$env
  .holder$fn <- .with_env(.arg, env)
  .holder$env <- env
  on.exit({
    .holder$fn <- .previous
    .holder$env <- .previous_env
  })
  .call(...)
}))

fill <- function(template, values, env) {
  eval(eval(call("substitute", template, values)), env)
}

decorate_function <- function(
  f,
  wrapper,
  style,
  config,
  dec_name,
  formals_mode = "keep"
) {
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
  call_name <- if (dec_name %in% reserved_names) {
    paste0(".", dec_name)
  } else {
    dec_name
  }
  assign(call_name, w, envir = call_env)
  call_sym <- as.name(call_name)
  # The decorated function is passed as `f`, unless the decorator is `f`.
  arg_name <- avoid("f", call_name)
  arg_sym <- as.name(arg_name)
  assign(arg_name, f, envir = call_env)
  assign(".holder", holder, envir = call_env)

  # Where the decorated function's formals come from; see `decorator()`.
  formals_from <- f

  if (style == "call") {
    # Hot path: runs on every call, so no bookkeeping.
    # The parent of the decorated function's frame is its current closure.
    invoke <- fill(
      call_template,
      list(
        .f_env = f_env,
        .call = call_sym,
        .arg = arg_sym,
        .with_env = with_env
      ),
      call_env
    )
  } else {
    # Stand-in with `f`'s signature that calls the current `holder$fn`. Having
    # `holder` embedded in its body keeps it to one frame in any environment.
    stand_in <- forward_formals(f, call("$", holder, quote(fn)))
    stand_name <- avoid("stand_in", call_name)
    assign(stand_name, stand_in, envir = call_env)
    # Marked so `decorator_context()` works during setup.
    setup <- fill(
      setup_template,
      list(.call = call_sym, .stand_in = as.name(stand_name)),
      call_env
    )
    attr(setup, "adorn_ctx") <- ctx
    attr(setup, "adorn_setup") <- TRUE
    run <- setup()
    if (!is.function(run)) {
      stop(
        sprintf("Decorator `%s` must return a function.", dec_name),
        call. = FALSE
      )
    }
    if (formals_mode == "replace") {
      formals_from <- run
    }
    # From here the name refers to the returned function.
    assign(call_name, run, envir = call_env)
    # Usual case: the stand-in is already bound to this environment. Otherwise
    # (e.g. R6 objects) it is rebound for the duration of the call.
    invoke <- fill(
      factory_template,
      list(
        .holder_obj = holder,
        .call = call_sym,
        .arg = arg_sym,
        .with_env = with_env
      ),
      call_env
    )
  }
  attr(invoke, "adorn_ctx") <- ctx
  attr(invoke, "adorn_holder") <- holder
  # The decoration this one wraps, if `f` was already decorated.
  attr(invoke, "adorn_inner") <- attr(f, "adorn")$holder

  out <- forward_formals(formals_from, invoke, env = f_env)

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
# `env` is the result's environment: normally `f`'s, but when the formals come
# from another function it must still be the decorated function's.
forward_formals <- function(f, invoke, env = environment(f) %||% baseenv()) {
  fmls <- formals(f) %||% formals(args(f))
  nms <- names(fmls)
  args <- lapply(nms, as.name)
  names(args) <- ifelse(nms == "...", "", nms)
  body <- as.call(c(list(invoke), args))
  out <- as.function(c(fmls %||% list(), list(body)))
  environment(out) <- env
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
#'   S7's `:=`, re-exported by adorn, supplies:
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
#'
#' # `:=` is re-exported from S7 and supplies `name`
#' add_one := decorator(function(f, ...) f(...)) %@% function(x) x + 1
#' add_one(1)
`%@%` <- function(lhs, rhs, name = NULL) {
  if (!is.function(lhs)) {
    stop("Left-hand side of `%@%` must be a decorator.", call. = FALSE)
  }
  if (inherits(rhs, "adorn_decorator")) {
    return(compose(lhs, rhs, substitute(lhs), substitute(rhs)))
  }
  if (!is.function(rhs)) {
    stop("Right-hand side of `%@%` must be a function.", call. = FALSE)
  }
  if (
    !is.null(name) &&
      !(is.character(name) && length(name) == 1L && !is.na(name))
  ) {
    stop("`name` must be a single string.", call. = FALSE)
  }
  # Precedence: explicit `name`, then `decorate()`, then the symbol `rhs`.
  fn_name <- name %||% binding_name()
  if (is.null(fn_name)) {
    e <- substitute(rhs)
    fn_name <- if (is.symbol(e)) as.character(e)
  }
  with_binding_name(fn_name, apply_decorator(lhs, rhs, substitute(lhs)))
}

# Apply `dec` to `f`. A decorator made by `decorator()` is applied directly,
# named after the expression `expr` it was written as (unless it already has a
# name), so no named copy has to be built on every use. Anything else (a plain
# function, or a composition) is simply called.
apply_decorator <- function(dec, f, expr) {
  env <- if (inherits(dec, "adorn_decorator")) environment(dec)
  if (is.null(env$style)) {
    return(dec(f))
  }
  decorate_function(
    f,
    env$wrapper,
    env$style,
    env$config,
    env$name %||% infer_name(expr),
    env$formals_mode
  )
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

# `outer_expr` and `inner_expr` are the expressions the decorators were written
# as, used to name them.
compose <- function(outer, inner, outer_expr, inner_expr) {
  self <- function(f) {
    if (missing(f)) {
      stop(
        "Composed decorators cannot be configured; configure each one first.",
        call. = FALSE
      )
    }
    apply_decorator(outer, apply_decorator(inner, f, inner_expr), outer_expr)
  }
  class(self) <- decorator_class
  self
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
