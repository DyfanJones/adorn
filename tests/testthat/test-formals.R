# Reference partial application, as shown in `vignette("partial-decorators")`.
partial <- decorator(
  function(f, .fixed = list()) {
    fmls <- formals(f)
    keep <- fmls[setdiff(names(fmls), names(.fixed))]
    fwd <- lapply(names(keep), as.name)
    names(fwd) <- ifelse(names(keep) == "...", "", names(keep))
    fixed <- lapply(.fixed, function(v) {
      if (is.language(v)) call("quote", v) else v
    })
    as.function(c(keep, list(as.call(c(list(f), fixed, fwd)))))
  },
  formals = "replace"
)

log <- character()
logged <- decorator(function(f, ...) {
  log <<- c(log, "called")
  f(...)
})

pow <- function(x, y) x^y

test_that("the default formals mode is unchanged", {
  id <- decorator(function(f) function(...) f(...))
  g <- id %@% pow
  expect_equal(names(formals(g)), c("x", "y"))
  expect_equal(names(formals(logged %@% pow)), c("x", "y"))
  expect_error(decorator(function(f) f, formals = "nope"), "should be one of")
})

test_that("a fixed leading or trailing argument shrinks the formals", {
  two_to <- partial(.fixed = list(x = 2)) %@% pow
  expect_equal(names(formals(two_to)), "y")
  expect_equal(two_to(3), 8)
  expect_equal(two_to(y = 4), 16)
  square <- partial(.fixed = list(y = 2)) %@% pow
  expect_equal(names(formals(square)), "x")
  expect_equal(square(4), 16)
})

test_that("defaults and missing() work for the remaining arguments", {
  f <- function(x, y = 10, z) {
    c(x, y, if (missing(z)) "no z" else z)
  }
  g <- partial(.fixed = list(x = 1)) %@% f
  expect_equal(names(formals(g)), c("y", "z"))
  expect_equal(g(), c("1", "10", "no z"))
  expect_equal(g(z = "z"), c("1", "10", "z"))
  expect_equal(g(5, "z"), c("1", "5", "z"))
})

test_that("symbols and calls in fixed values are not evaluated early", {
  f <- function(x, y) x
  g <- partial(.fixed = list(y = quote(undefined_symbol))) %@% f
  expect_equal(g(1), 1)
  k <- partial(.fixed = list(x = quote(stop("early")))) %@% function(x, y) y
  expect_equal(k(3), 3)
  l <- partial(.fixed = list(x = quote(a + b))) %@% function(x) x
  expect_equal(l(), quote(a + b))
})

test_that("remaining dots are forwarded", {
  f <- function(x, ...) sum(x, ...)
  g <- partial(.fixed = list(x = 1)) %@% f
  expect_equal(names(formals(g)), "...")
  expect_equal(g(2, 3), 6)
  expect_equal(g(), 1)
})

test_that("all arguments can be fixed", {
  g <- partial(.fixed = list(x = 2, y = 5)) %@% pow
  expect_length(formals(g), 0L)
  expect_equal(g(), 32)
})

test_that("introspection reports the partial and recovers the original", {
  g <- partial(.fixed = list(x = 2)) %@% pow
  expect_equal(decorators(g), "partial")
  expect_identical(undecorate(g), pow)
  expect_equal(names(formals(undecorate(g))), c("x", "y"))
})

test_that("stacking works in either order and the outer layer sees the formals", {
  log <<- character()
  a <- logged %@% partial(.fixed = list(x = 2)) %@% pow
  expect_equal(names(formals(a)), "y")
  expect_equal(a(3), 8)
  expect_equal(decorators(a), c("logged", "partial"))
  expect_identical(undecorate(a), pow)

  b <- partial(.fixed = list(x = 2)) %@% logged %@% pow
  expect_equal(names(formals(b)), "y")
  expect_equal(b(3), 8)
  expect_equal(decorators(b), c("partial", "logged"))
  expect_identical(undecorate(b), pow)
  expect_equal(log, c("called", "called"))
})

test_that("partials compose and can be partially applied again", {
  both <- partial(.fixed = list(x = 1)) %@% partial(.fixed = list(y = 2))
  g <- both %@% function(x, y, z) c(x, y, z)
  expect_equal(names(formals(g)), "z")
  expect_equal(g(3), c(1, 2, 3))
})

test_that("decorator_context() names the function inside and outside a partial", {
  seen <- character()
  spy <- decorator(function(f, ...) {
    seen <<- c(seen, decorator_context()$name)
    f(...)
  })
  add <- function(x, y) x + y
  inner_spy <- partial(.fixed = list(x = 1)) %@% spy %@% add
  expect_equal(inner_spy(2), 3)
  outer_spy <- spy %@% partial(.fixed = list(x = 1)) %@% add
  expect_equal(outer_spy(2), 3)
  expect_equal(seen, c("inner_spy", "outer_spy"))

  setup_name <- NULL
  who <- decorator(
    function(f) {
      setup_name <<- decorator_context()$name
      function(...) f(...)
    },
    formals = "replace"
  )
  named <- who %@% add
  expect_equal(setup_name, "add")
})

test_that("decorate() works for plain functions, S3 and R6 methods", {
  g <- decorate(pow, partial(.fixed = list(x = 2)))
  expect_equal(g(3), 8)

  area <- function(shape, scale) UseMethod("area")
  area.sq <- function(shape, scale) shape$side^2 * scale
  area.sq <- decorate(area.sq, partial(.fixed = list(scale = 2)))
  expect_equal(area(structure(list(side = 3), class = "sq")), 18)

  skip_if_not_installed("R6")
  Counter <- R6::R6Class(
    "Counter",
    public = list(
      n = 10,
      add = function(a, b) self$n + a + b
    )
  )
  decorate("add", partial(.fixed = list(a = 1)), class = Counter)
  x <- Counter$new()
  expect_equal(names(formals(x$add)), "b")
  expect_equal(x$add(5), 16)
  y <- Counter$new()
  y$n <- 100
  expect_equal(y$add(5), 106)
  expect_equal(x$add(5), 16)
})

test_that("configured copies and named decorators keep the formals mode", {
  fix <- decorator(
    function(f, value = 1) {
      force(value)
      function(y) f(value, y)
    },
    formals = "replace"
  )
  g <- fix(value = 5) %@% pow
  expect_equal(names(formals(g)), "y")
  expect_equal(g(2), 25)
  named <- decorator(
    function(f) function(y) f(2, y),
    name = "two",
    formals = "replace"
  )
  expect_equal(names(formals(named %@% pow)), "y")
  expect_output(print(fix), "formals: replace")
  expect_output(print(fix(value = 2)), "formals: replace")
  expect_false(any(grepl("formals", capture.output(print(logged)))))
})

test_that("a decorator can add arguments", {
  with_scale <- decorator(
    function(f) function(x, scale = 10) f(x) * scale,
    formals = "replace"
  )
  g <- with_scale %@% function(x) x
  expect_equal(names(formals(g)), c("x", "scale"))
  expect_equal(g(2), 20)
  expect_equal(g(2, scale = 1), 2)
})

test_that("formals = 'replace' with a call-style wrapper is an error", {
  expect_error(
    decorator(function(f, ...) f(...), formals = "replace"),
    "needs a factory-style decorator"
  )
})

test_that("a factory that does not return a function still errors", {
  bad <- decorator(function(f) 1, formals = "replace")
  expect_error(bad %@% pow, "must return a function")
})

test_that("a curry can give each step the real argument name", {
  curry <- decorator(
    function(f) {
      args <- setdiff(names(formals(f)), "...")
      step <- function(done) {
        arg <- args[[length(done) + 1L]]
        last <- length(done) + 1L == length(args)
        next_fn <- if (last) function(all) do.call(f, all) else step
        value <- as.call(c(
          list(as.name("list")),
          setNames(list(as.name(arg)), arg)
        ))
        body <- as.call(list(next_fn, call("c", done, value)))
        as.function(
          c(setNames(alist(x = ), arg), list(body)),
          envir = baseenv()
        )
      }
      step(list())
    },
    formals = "replace"
  )
  box <- function(width, height, depth) width * height * depth
  volume <- curry %@% box
  expect_equal(names(formals(volume)), "width")
  expect_equal(names(formals(volume(2))), "height")
  expect_equal(volume(2)(3)(4), 24)

  # Argument names that clash with internals do not shadow them.
  clash <- curry %@% function(done, f, args) paste(done, f, args)
  expect_equal(clash(1)(2)(3), "1 2 3")

  slab <- curry %@% partial(.fixed = list(depth = 10)) %@% box
  expect_equal(names(formals(slab)), "width")
  expect_equal(slab(2)(3), 60)
})
