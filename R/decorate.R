#' @include context.R decorator.R
#' @importFrom methods getClass getMethod setMethod
#' @importFrom S7 new_generic new_S3_class S7_dispatch method method<- class_function class_character
NULL

#' Decorate a function or an existing method
#'
#' `decorate()` applies a decorator to a target: a function, or a method that
#' already exists on an S3, S4, S7 or R6 class. Use `%@%` to decorate a
#' function you are defining, and `decorate()` for code you did not write.
#'
#' | `target` | Example | Extra argument |
#' |---|---|---|
#' | a function | `decorate(fn, d)`, the same as `d %@% fn` | |
#' | the name of an S3 generic | `decorate("print", d, class = "money")` | `class` |
#' | an S4 generic | `decorate(area, d, signature = "Square")` | `signature` |
#' | an S7 generic | `decorate(speak, d, class = Dog)` | `class` |
#' | the name of an R6 method | `decorate("add", d, class = Counter)` | `class` |
#'
#' For S3 and R6 the method is named by a string, and `class` gives the class or
#' R6 generator. For S4 and S7 the generic is the target. The replacement form
#' (see below) works for all four systems.
#'
#' @param target What to decorate; see the table above.
#' @param decorator A decorator, or a stack of them built with `%@%`.
#' @param ... Arguments for specific kinds of target:
#'   * `class`: for S3, the class name as a string. For S7, the class or
#'     signature, as in [S7::method()]. For R6, the class generator.
#'   * `signature`: for S4, the signature, as in [methods::setMethod()].
#'   * `envir`: for S3, the environment to find the method and generic in.
#'     Defaults to the caller's.
#'   * `where`: for S4, the environment to store the method in. Defaults to
#'     the top-level environment of the caller. For R6, the part of the class
#'     that holds the method: `"public"` (the default), `"private"` or
#'     `"active"`.
#' @section R6 classes:
#' `decorate("name", decorator, class = Generator)` replaces the method in the
#' generator, so objects created afterwards use it. Objects that already exist
#' keep the old method, because R6 copies methods into each object when it is
#' created. An inherited method is copied into `Generator` and decorated there;
#' the parent class is unchanged. Give several names to decorate several methods.
#' @return For a function, the decorated function. For R6, the class generator,
#'   invisibly. For the others, the decorated method, invisibly; the method is
#'   replaced in place.
#' @export
#' @examples
#' # A function
#' shout <- decorator(function(f, ...) toupper(f(...)))
#' greet <- decorate(function(who) paste("hello", who), shout)
#' greet("world")
#'
#' # An existing S3 method
#' area <- function(x, ...) UseMethod("area")
#' area.square <- function(x, ...) x$side^2
#' decorate("area", shout, class = "square")
#' area(structure(list(side = 3), class = "square"))
#'
#' # An R6 method
#' if (requireNamespace("R6", quietly = TRUE)) {
#'   Greeter <- R6::R6Class("Greeter", public = list(
#'     hi = function(who) paste("hello", who)
#'   ))
#'   decorate("hi", shout, class = Greeter)
#'   Greeter$new()$hi("world")
#' }
decorate <- new_generic(
  "decorate",
  "target",
  function(target, decorator, ...) S7_dispatch()
)

check_decorator <- function(decorator) {
  if (!is.function(decorator)) {
    stop("`decorator` must be a decorator.", call. = FALSE)
  }
  invisible(decorator)
}

# Decorate `fn`, recording `name` for `decorator_context()$name`.
apply_named <- function(decorator, fn, name) {
  check_decorator(decorator)
  with_binding_name(name, decorator(fn))
}

method(decorate, class_function) <- function(target, decorator, ...) {
  check_decorator(decorator)
  decorator(target)
}

method(decorate, class_character) <- function(
  target,
  decorator,
  ...,
  class,
  where = "public",
  envir = parent.frame(2)
) {
  if (!missing(class) && inherits(class, "R6ClassGenerator")) {
    return(decorate_r6_methods(class, decorator, target, where))
  }
  if (length(target) != 1L) {
    stop("`target` must be a single generic name.", call. = FALSE)
  }
  if (missing(class)) {
    stop(
      "Supply the `class` of the method to decorate: an S3 class name or an R6 generator.",
      call. = FALSE
    )
  }
  decorate_s3_method(target, decorator, class, envir)
}

# Name of the S3 generic `fn` dispatches for, read from its `UseMethod()` call,
# or NULL. Needed because `decorate(print, "money") <- d` only sees the function.
s3_generic_name <- function(fn) {
  find <- function(x) {
    if (is.call(x)) {
      if (identical(x[[1L]], quote(UseMethod)) && is.character(x[[2L]])) {
        return(x[[2L]])
      }
      for (i in seq_along(x)) {
        if (
          !identical(x[[i]], quote(expr = )) && !is.null(res <- find(x[[i]]))
        ) {
          return(res)
        }
      }
    }
    NULL
  }
  find(body(fn))
}

decorate_s3_method <- function(generic, decorator, class, envir) {
  if (!is.character(class) || length(class) != 1L) {
    stop("Supply the S3 `class` as a single string.", call. = FALSE)
  }
  method_name <- paste(generic, class, sep = ".")
  method <- utils::getS3method(generic, class, envir = envir)
  decorated <- apply_named(decorator, method, method_name)
  # A copy in `envir` is found before the registry, so replace it too.
  if (exists(method_name, envir = envir, inherits = FALSE)) {
    assign(method_name, decorated, envir = envir)
  }
  registerS3method(generic, class, decorated, envir = envir)
  invisible(decorated)
}

# S4 and S7 helpers shared by `decorate()` and `decorate<-`.
decorate_s4_method <- function(target, decorator, signature, where) {
  if (missing(signature)) {
    stop("Supply the S4 `signature` of the method to decorate.", call. = FALSE)
  }
  generic <- as.character(target@generic)
  s4_method <- methods::getMethod(generic, signature)
  fn <- s4_method@.Data
  # S4 keeps the decoration metadata on the method object, not the function.
  attr(fn, "adorn") <- attr(s4_method, "adorn")
  decorated <- apply_named(decorator, fn, generic)
  methods::setMethod(generic, signature, decorated, where = where)
  invisible(decorated)
}

decorate_s7_method <- function(target, decorator, class) {
  if (missing(class)) {
    stop("Supply the S7 `class` of the method to decorate.", call. = FALSE)
  }
  current <- method(target, class)
  decorated <- apply_named(decorator, current, target@name)
  suppressMessages(method(target, class) <- decorated)
  invisible(decorated)
}

# Find an R6 method on `gen` or its parents. `field` is the generator's list
# (e.g. `public_methods`) and `where` its name for error messages.
find_r6_method <- function(gen, name, field, where) {
  gen_name <- gen$classname %||% "<generator>"
  while (!is.null(gen)) {
    fn <- gen[[field]][[name]]
    if (is.function(fn)) {
      return(fn)
    }
    gen <- gen$get_inherit()
  }
  stop(
    sprintf(
      "No %s method `%s` found on `%s` or its parents.",
      where,
      name,
      gen_name
    ),
    call. = FALSE
  )
}

decorate_r6_methods <- function(target, decorator, method, where = "public") {
  if (!is.character(method) || length(method) == 0L) {
    stop("Supply the R6 method name(s) to decorate.", call. = FALSE)
  }
  where <- match.arg(where, c("public", "private", "active"))
  field <- switch(
    where,
    public = "public_methods",
    private = "private_methods",
    active = "active"
  )
  # Look everything up first so a typo leaves the class untouched.
  methods <- lapply(method, function(m) find_r6_method(target, m, field, where))
  for (i in seq_along(method)) {
    decorated <- apply_named(decorator, methods[[i]], method[[i]])
    target$set(where, method[[i]], decorated, overwrite = TRUE)
  }
  invisible(target)
}

method(decorate, new_S3_class("R6ClassGenerator")) <- function(
  target,
  decorator,
  ...
) {
  stop(
    "To decorate an R6 method use `decorate(\"method\", decorator, class = Generator)`.",
    call. = FALSE
  )
}

method(decorate, methods::getClass("standardGeneric")) <- function(
  target,
  decorator,
  ...,
  signature,
  where = topenv(parent.frame(2))
) {
  decorate_s4_method(target, decorator, signature, where)
}

method(decorate, new_S3_class("S7_generic")) <- function(
  target,
  decorator,
  ...,
  class
) {
  decorate_s7_method(target, decorator, class)
}

#' @rdname decorate
#' @section Replacement form:
#' `decorate(target, class) <- decorator` mirrors S7's `method(generic, class) <- fn`
#' and works for all four systems:
#'
#' ```r
#' decorate(speak, Dog) <- shout        # S7
#' decorate(area, "Square") <- shout    # S4
#' decorate(print, "money") <- shout    # S3, with the generic function
#' decorate(Counter, "add") <- shout    # R6, public methods
#' ```
#'
#' R does not allow a string as the target of a replacement call, so for S3 pass
#' the generic function. Its name is read from its `UseMethod()` call, so the
#' generic must call `UseMethod()` directly. For R6 the form reaches public
#' methods only; use the explicit form for `where`.
#' @param signature For the replacement form, the S7 class, S4 signature, S3
#'   class name, or R6 method name(s), as above.
#' @param value For the replacement form, the decorator to apply.
#' @export
`decorate<-` <- new_generic(
  "decorate<-",
  "target",
  function(target, signature, ..., value) S7_dispatch()
)

method(`decorate<-`, class_function) <- function(
  target,
  signature,
  ...,
  value
) {
  generic <- s3_generic_name(target)
  if (is.null(generic)) {
    stop(
      "`target` must be an S3 generic that calls `UseMethod()`. ",
      "Otherwise use `decorate(\"method\", decorator, class = class)`.",
      call. = FALSE
    )
  }
  check_decorator(value)
  decorate_s3_method(generic, value, signature, parent.frame(2))
  target
}

method(`decorate<-`, methods::getClass("standardGeneric")) <- function(
  target,
  signature,
  ...,
  value
) {
  decorate_s4_method(target, value, signature, topenv(parent.frame(2)))
  target
}

method(`decorate<-`, new_S3_class("S7_generic")) <- function(
  target,
  signature,
  ...,
  value
) {
  decorate_s7_method(target, value, signature)
  target
}

method(`decorate<-`, new_S3_class("R6ClassGenerator")) <- function(
  target,
  signature,
  ...,
  value
) {
  decorate_r6_methods(target, value, signature)
  target
}
