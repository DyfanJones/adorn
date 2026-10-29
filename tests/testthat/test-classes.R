who <- decorator(function(f, ...) {
  c(name = decorator_context()$name, value = f(...))
})
plus1 <- decorator(function(f, ...) f(...) + 1)

test_that("S3 methods can be decorated inline and for generics", {
  area <- function(x, ...) UseMethod("area")
  area.sq <- plus1 %@% function(x, ...) x$side^2
  expect_equal(area(structure(list(side = 3), class = "sq")), 10)

  # Methods must be visible from the top level, as in a script or package.
  assign(
    "describe_pt",
    plus1 %@% function(x, ...) UseMethod("describe_pt"),
    envir = globalenv()
  )
  assign("describe_pt.default", function(x, ...) 1, envir = globalenv())
  on.exit(rm(
    list = c("describe_pt", "describe_pt.default"),
    envir = globalenv()
  ))
  expect_equal(get("describe_pt", globalenv())(0), 2)
})

test_that("decorate() decorates an existing S3 method", {
  area <- function(x, ...) UseMethod("area")
  area.sq <- function(x, ...) x$side^2
  decorate("area", plus1, class = "sq")
  expect_equal(area(structure(list(side = 3), class = "sq")), 10)
})

test_that("S4 methods can be decorated inline and after the fact", {
  setClass("PCircle", representation(r = "numeric"))
  setGeneric("parea", function(shape, ...) standardGeneric("parea"))
  setMethod("parea", "PCircle", plus1 %@% function(shape, ...) shape@r)
  expect_equal(parea(new("PCircle", r = 2)), 3)

  decorate(parea, plus1, signature = "PCircle")
  expect_equal(parea(new("PCircle", r = 2)), 4)
  expect_equal(
    decorators(getMethod("parea", "PCircle")),
    c("decorator", "plus1")
  )
})

test_that("S4 validity functions can be decorated", {
  calls <- 0
  count <- decorator(function(f, ...) {
    calls <<- calls + 1
    f(...)
  })
  setClass(
    "PValid",
    representation(x = "numeric"),
    validity = count %@% function(object) TRUE
  )
  new("PValid", x = 1)
  expect_gte(calls, 1)
})

test_that("S7 methods and properties can be decorated", {
  skip_if_not_installed("S7")
  Dog <- S7::new_class(
    "PDog",
    properties = list(
      name = S7::class_character,
      loud = S7::new_property(
        S7::class_character,
        getter = plus1 %@% function(self) nchar(self@name)
      )
    )
  )
  # getter returns a number here, only to check it runs through the decorator
  speak <- S7::new_generic("speak", "x")
  S7::method(speak, Dog) <- plus1 %@% function(x) nchar(x@name)
  d <- Dog(name = "rex")
  expect_equal(speak(d), 4)

  decorate(speak, plus1, class = Dog)
  expect_equal(speak(d), 5)
})

test_that("R6 methods can be decorated and can use self/private", {
  skip_if_not_installed("R6")
  Counter <- R6::R6Class(
    "PCounter",
    public = list(
      n = 0,
      add = who %@%
        function(k = 1) {
          self$n <- self$n + k
          private$bump()
          self$n
        },
      get = function() self$n
    ),
    private = list(bump = function() self$n <- self$n + 100),
    active = list(
      double = plus1 %@%
        function(value) {
          if (missing(value)) self$n * 2 else stop("read only")
        }
    )
  )
  a <- Counter$new()
  res <- a$add(2)
  expect_equal(res[["name"]], "add")
  expect_equal(as.numeric(res[["value"]]), 102)
  expect_equal(a$double, 205)
  expect_error(a$double <- 1, "read only")

  b <- Counter$new()
  b$add()
  expect_equal(c(a$get(), b$get()), c(102, 101))
})

test_that("R6 decorated methods work with inheritance and clone", {
  skip_if_not_installed("R6")
  Base <- R6::R6Class(
    "PBase",
    public = list(
      n = 1,
      val = plus1 %@% function() self$n
    )
  )
  Child <- R6::R6Class(
    "PChild",
    inherit = Base,
    public = list(
      val = function() super$val() * 10
    )
  )
  expect_equal(Child$new()$val(), 20)
  obj <- Base$new()
  obj$n <- 5
  cl <- obj$clone()
  cl$n <- 7
  expect_equal(c(obj$val(), cl$val()), c(6, 8))
})

test_that("factory-style decorators work on R6 methods", {
  skip_if_not_installed("R6")
  cache <- decorator(function(f) {
    seen <- list()
    function(...) {
      key <- as.character(..1)
      if (is.null(seen[[key]])) {
        seen[[key]] <<- f(...)
      }
      seen[[key]]
    }
  })
  Acc <- R6::R6Class(
    "PAcc",
    public = list(
      base = 10,
      calc = cache %@% function(x) self$base + x
    )
  )
  a <- Acc$new()
  expect_equal(a$calc(1), 11)
  a$base <- 100
  expect_equal(a$calc(1), 11) # cached
  expect_equal(a$calc(2), 102)
})

test_that("decorate() on a function matches %@%", {
  f <- decorate(function(x) x, plus1)
  expect_equal(f(1), 2)
  expect_equal(decorators(f), "decorator")
  expect_error(
    decorate(function(x) x, "not a decorator"),
    "must be a decorator"
  )
})

test_that("decorate() gives clear errors for bad targets and missing arguments", {
  expect_error(decorate(1, plus1), "Can't find method")
  expect_error(decorate("print", plus1), "Supply the `class`")
  expect_error(decorate(c("a", "b"), plus1, class = "x"), "single generic name")
  setGeneric("perr", function(x, ...) standardGeneric("perr"))
  expect_error(decorate(perr, plus1), "Supply the S4 `signature`")
  g <- S7::new_generic("perr7", "x")
  expect_error(decorate(g, plus1), "Supply the S7 `class`")
})

test_that("decorate() decorates stacks and names methods", {
  area <- function(x, ...) UseMethod("area")
  area.sq <- function(x, ...) x$side^2
  decorate("area", who %@% plus1, class = "sq")
  res <- area(structure(list(side = 2), class = "sq"))
  expect_equal(res[["name"]], "area.sq")
  expect_equal(as.numeric(res[["value"]]), 5)
  expect_equal(decorators(area.sq), c("who", "plus1"))
})

test_that("other packages can add decorate() methods", {
  Box <- S7::new_class("PBox", properties = list(fn = S7::class_function))
  S7::method(decorate, Box) <- function(target, decorator, ...) {
    target@fn <- decorator(target@fn)
    target
  }
  b <- decorate(Box(fn = function() 1), plus1)
  expect_equal(b@fn(), 2)
})

test_that("replacement form decorates S7 methods", {
  Cat <- S7::new_class("PCat", properties = list(name = S7::class_character))
  meow <- S7::new_generic("meow", "x")
  S7::method(meow, Cat) <- function(x) nchar(x@name)
  decorate(meow, Cat) <- plus1 %@% plus1
  expect_equal(meow(Cat(name = "tom")), 5)
  decorate(meow, Cat) <- plus1
  expect_equal(meow(Cat(name = "tom")), 6)
})

test_that("replacement form decorates S4 methods", {
  setClass("PTri", representation(b = "numeric"))
  setGeneric("tarea", function(shape, ...) standardGeneric("tarea"))
  setMethod("tarea", "PTri", function(shape, ...) shape@b)
  decorate(tarea, "PTri") <- plus1
  expect_equal(tarea(new("PTri", b = 2)), 3)
  expect_equal(decorators(getMethod("tarea", "PTri")), "decorator")
})

test_that("replacement form stacks like decorate()", {
  Fox <- S7::new_class("PFox", properties = list(n = S7::class_numeric))
  run <- S7::new_generic("run", "x")
  S7::method(run, Fox) <- function(x) x@n
  decorate(run, Fox) <- plus1 %@% plus1
  decorate(run, Fox) <- who # added last, so outermost
  res <- run(Fox(n = 1))
  expect_equal(res[["value"]], "3")
  expect_match(res[["name"]], "^method\\(run, ")
})

test_that("replacement form explains that S3 is not supported", {
  expect_error(decorate(sum, "money") <- plus1, "UseMethod")
})

test_that("decorate() decorates R6 methods in a generator", {
  skip_if_not_installed("R6")
  Counter <- R6::R6Class(
    "DCounter",
    public = list(
      n = 0,
      add = function(k = 1) {
        self$n <- self$n + private$bump(k)
        invisible(self)
      },
      get = function() self$n
    ),
    private = list(bump = function(k) k),
    active = list(double = function(value) self$n * 2)
  )
  res <- decorate("get", who, class = Counter)
  expect_identical(res, Counter)

  obj <- Counter$new()
  obj$add(2)$add(3)
  expect_equal(obj$get()[["name"]], "get")
  expect_equal(as.numeric(obj$get()[["value"]]), 5)

  # private and active methods, several at once
  decorate("bump", plus1, class = Counter, where = "private")
  expect_equal(Counter$new()$add(2)$n, 3)
  decorate("double", plus1, class = Counter, where = "active")
  expect_equal(Counter$new()$double, 1)

  # objects created before keep the old method
  expect_equal(obj$n, 5)
})

test_that("decorate() on an R6 class copies inherited methods into the child", {
  skip_if_not_installed("R6")
  Parent <- R6::R6Class("DParent", public = list(val = function() 1))
  Child <- R6::R6Class("DChild", inherit = Parent)
  decorate("val", plus1, class = Child)
  expect_equal(Child$new()$val(), 2)
  expect_equal(Parent$new()$val(), 1)
})

test_that("decorate() on an R6 class validates its input", {
  skip_if_not_installed("R6")
  Counter <- R6::R6Class("DCheck", public = list(a = function() 1))
  expect_error(decorate(Counter, plus1), "class = Generator")
  expect_error(decorate("a", plus1), "Supply the `class`")
  expect_error(
    decorate("nope", plus1, class = Counter),
    "No public method `nope`"
  )
  expect_error(decorate(c("a", "nope"), plus1, class = Counter), "nope")
  expect_equal(Counter$new()$a(), 1)
})

test_that("the replacement form decorates R6 methods", {
  skip_if_not_installed("R6")
  Counter <- R6::R6Class("DRepl", public = list(a = function() 1))
  decorate(Counter, "a") <- plus1
  expect_equal(Counter$new()$a(), 2)
})

test_that("the replacement form decorates S3 methods from the generic", {
  area <- function(x, ...) UseMethod("area")
  area.rsq <- function(x, ...) x$side^2
  decorate(area, "rsq") <- plus1
  expect_equal(area(structure(list(side = 3), class = "rsq")), 10)
})

test_that("decorate() on S3 generics validates its inputs", {
  expect_error(decorate("print", plus1, class = c("a", "b")), "single string")
})

test_that("the replacement form finds UseMethod() in nested calls", {
  gen_nested <- function(x) {
    if (is.null(x)) {
      return(NULL)
    }
    UseMethod("gen_nested")
  }
  gen_nested.numeric <- function(x) x
  decorate(gen_nested, "numeric") <- plus1
  expect_equal(gen_nested(1), 2)

  braced <- function(x) {
    UseMethod("braced")
  }
  braced.numeric <- function(x) x
  decorate(braced, "numeric") <- plus1
  expect_equal(braced(1), 2)
})

test_that("decorate() on R6 generators needs method names", {
  skip_if_not_installed("R6")
  Gen <- R6::R6Class("CovGen", public = list(a = function() 1))
  expect_error(decorate(Gen, character()) <- plus1, "Supply the R6 method name")
})
