#' @include context.R decorator.R
#' @importFrom methods getClass getMethod setMethod
NULL

#' Decorate a function or an existing method
#'
#' `decorate()` applies a decorator to a target. What it does depends on what
#' the target is:
#'
#' | `target` | Meaning | Extra arguments |
#' |---|---|---|
#' | a function | Returns the decorated function. Same as `decorator %@% target`. | |
#' | a string | The name of an S3 generic. Decorates one of its methods in place. | `class` |
#' | an S4 generic | Decorates one of its methods in place. | `signature` |
#' | an S7 generic | Decorates one of its methods in place. | `class` |
#' | an R6 class generator | Decorates one or more of its methods in place. | `method`, `where` |
#'
#' Use `%@%` to decorate a function as you define it, and `decorate()` for a
#' method that already exists, such as one from a class you did not write.
#'
#' `decorate()` is an S7 generic, so other packages can add support for their
#' own kinds of target with `S7::method(decorate, <class>) <- function(target,
#' decorator, ...) ...`.
#'
#' @section Decorating an S3 generic:
#' You can decorate a generic itself (`describe <- shout %@% function(x, ...)
#' UseMethod("describe")`). R looks for methods in the environment that calls
#' the generic, which is now inside adorn, so methods must be registered or
#' defined at top level (as they are in a package or script). Methods defined
#' only inside another function will not be found.
#'
#' @param target What to decorate; see the table above.
#' @param decorator A decorator, or a stack of them built with `%@%`.
#' @param ... Arguments for specific kinds of target:
#'   * `class`: for S3, the class name as a string. For S7, the class (or a
#'     signature, as accepted by [S7::method()]).
#'   * `signature`: for S4, the method's signature, as in
#'     [methods::setMethod()].
#'   * `envir`: for S3, the environment to look for the method and generic in.
#'     Defaults to the caller's.
#'   * `method`: for R6, the name(s) of the method(s) to decorate.
#'   * `where`: for S4, the environment the method is stored in. Defaults to
#'     the top-level environment of the caller. For R6, which part of the
#'     class holds the method: `"public"` (the default), `"private"` or
#'     `"active"`.
#' @section Decorating an R6 class:
#' `decorate(Generator, decorator, method = "name")` replaces the method in the
#' generator, so objects created afterwards use the decorated version. Objects
#' that already exist keep the old method, because R6 copies methods when an
#' object is created. A method the class only inherits is copied into
#' `Generator` and decorated there; the parent class is not changed.
#' @return For a function, the decorated function. For an R6 generator, the
#'   generator, invisibly. For the others, the decorated method, invisibly;
#'   the method is replaced in place.
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
#'   decorate(Greeter, shout, method = "hi")
#'   Greeter$new()$hi("world")
#' }
decorate <- S7::new_generic(
  "decorate",
  "target",
  function(target, decorator, ...) S7::S7_dispatch()
)

check_decorator <- function(decorator) {
  if (!is.function(decorator)) {
    stop("`decorator` must be a decorator.", call. = FALSE)
  }
  invisible(decorator)
}

# Decorate `fn` while telling the decorator which name it belongs to, so
# `decorator_context()$name` is meaningful where there is no call to read.
apply_named <- function(decorator, fn, name) {
  check_decorator(decorator)
  with_binding_name(name, decorator(fn))
}

S7::method(decorate, S7::class_function) <- function(target, decorator, ...) {
  check_decorator(decorator)
  decorator(target)
}

S7::method(decorate, S7::class_character) <- function(
  target,
  decorator,
  ...,
  class,
  envir = parent.frame(2)
) {
  if (length(target) != 1L) {
    stop("`target` must be a single generic name.", call. = FALSE)
  }
  if (missing(class) || !is.character(class) || length(class) != 1L) {
    stop("Supply the S3 `class` of the method to decorate.", call. = FALSE)
  }
  method_name <- paste(target, class, sep = ".")
  method <- utils::getS3method(target, class, envir = envir)
  decorated <- apply_named(decorator, method, method_name)
  # A method defined in `envir` is found there before the registry, so
  # replace that copy too.
  if (exists(method_name, envir = envir, inherits = FALSE)) {
    assign(method_name, decorated, envir = envir)
  }
  registerS3method(target, class, decorated, envir = envir)
  invisible(decorated)
}

# The S4 and S7 work is shared by `decorate()` and `decorate<-`.
decorate_s4_method <- function(target, decorator, signature, where) {
  if (missing(signature)) {
    stop("Supply the S4 `signature` of the method to decorate.", call. = FALSE)
  }
  generic <- as.character(target@generic)
  method <- methods::getMethod(generic, signature)
  fn <- method@.Data
  # S4 keeps adorn's bookkeeping on the method object, not its function.
  attr(fn, "adorn") <- attr(method, "adorn")
  decorated <- apply_named(decorator, fn, generic)
  methods::setMethod(generic, signature, decorated, where = where)
  invisible(decorated)
}

decorate_s7_method <- function(target, decorator, class) {
  if (missing(class)) {
    stop("Supply the S7 `class` of the method to decorate.", call. = FALSE)
  }
  method <- S7::method(target, class)
  decorated <- apply_named(decorator, method, target@name)
  suppressMessages(S7::`method<-`(target, class, value = decorated))
  invisible(decorated)
}

# Find an R6 method on `gen` or, failing that, on the classes it inherits
# from. `field` is the generator's list (public_methods, private_methods or
# active) and `where` the matching name used in error messages.
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
    sprintf("No %s method `%s` found on `%s` or its parents.", where, name, gen_name),
    call. = FALSE
  )
}

decorate_r6_methods <- function(target, decorator, method, where = "public") {
  if (missing(method) || !is.character(method) || length(method) == 0L) {
    stop("Supply the R6 `method` name(s) to decorate.", call. = FALSE)
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

S7::method(decorate, S7::new_S3_class("R6ClassGenerator")) <- function(
  target,
  decorator,
  ...,
  method,
  where = "public"
) {
  decorate_r6_methods(target, decorator, method, where)
}

S7::method(decorate, methods::getClass("standardGeneric")) <- function(
  target,
  decorator,
  ...,
  signature,
  where = topenv(parent.frame(2))
) {
  decorate_s4_method(target, decorator, signature, where)
}

S7::method(decorate, S7::new_S3_class("S7_generic")) <- function(
  target,
  decorator,
  ...,
  class
) {
  decorate_s7_method(target, decorator, class)
}

#' @rdname decorate
#' @section Replacement form:
#' For S4 and S7, where the target is an object that carries its own name,
#' `decorate(generic, signature) <- decorator` is the same as
#' `decorate(generic, decorator, signature = signature)`. It mirrors S7's own
#' `method(generic, class) <- fn`:
#'
#' ```r
#' decorate(speak, Dog) <- shout                       # S7
#' decorate(area, "Square") <- shout                 # S4
#' decorate(Counter, "add") <- shout                 # R6, public methods
#' ```
#'
#' Like `method<-`, it also rebinds the generic's name in the calling
#' environment (to the same generic). R does not allow a string as the target
#' of a replacement call, so S3 methods use the explicit form.
#' @param signature For the replacement form, the S7 class or S4 signature,
#'   as `class` or `signature` above, or the name(s) of public R6 methods.
#' @param value For the replacement form, the decorator to apply.
#' @export
`decorate<-` <- S7::new_generic(
  "decorate<-",
  "target",
  function(target, signature, value) S7::S7_dispatch()
)

S7::method(`decorate<-`, S7::class_function) <- function(
  target,
  signature,
  value
) {
  stop(
    "The replacement form works for S4 and S7 generics and R6 generators. ",
    "For S3 methods use `decorate(\"generic\", decorator, class = \"class\")`.",
    call. = FALSE
  )
}

S7::method(`decorate<-`, methods::getClass("standardGeneric")) <- function(
  target,
  signature,
  value
) {
  decorate_s4_method(target, value, signature, topenv(parent.frame(2)))
  target
}

S7::method(`decorate<-`, S7::new_S3_class("S7_generic")) <- function(
  target,
  signature,
  value
) {
  decorate_s7_method(target, value, signature)
  target
}

S7::method(`decorate<-`, S7::new_S3_class("R6ClassGenerator")) <- function(
  target,
  signature,
  value
) {
  decorate_r6_methods(target, value, signature)
  target
}
