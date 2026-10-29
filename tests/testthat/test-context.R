test_that("names are inferred from `pkg::dec` and ignored for other calls", {
  dec <- decorator(function(f, ...) decorator_context()$name)
  f <- dec %@% function() 1
  expect_equal(f(), "f")
  ns <- list(dec = dec)
  g <- ns$dec %@% function() 1
  expect_equal(g(), "g")
  h <- ns[["dec"]] %@% function() 1
  expect_equal(h(), "h")
  base_dec <- base::identity
  expect_error(base_dec %@% function() 1, NA)
})

test_that("calls through `pkg::fn` and `obj$fn` report the called name", {
  dec <- decorator(function(f, ...) decorator_context()$name)
  fn <- dec %@% function() 1
  env <- list(fn = fn)
  expect_equal(env$fn(), "fn")
  expect_equal(local(fn()), "fn")
  expect_equal(lapply(1, function(i) fn())[[1]], "fn")
})

test_that("decorator_context() works from a helper called by a decorator", {
  seen <- character()
  helper <- function() decorator_context()$name
  spy <- decorator(function(f, ...) {
    seen <<- c(seen, helper())
    f(...)
  })
  fn <- spy %@% function(x) x
  fn(1)
  expect_equal(seen, "fn")
})

test_that("decorator_context() works from the decorated function itself", {
  spy <- decorator(function(f, ...) f(...))
  fn <- spy %@% function() decorator_context()$name
  expect_equal(fn(), "fn")
})

test_that("decorator_context() works from a helper while a factory sets up", {
  seen <- NULL
  helper <- function() decorator_context()$name
  setup <- decorator(function(f) {
    seen <<- helper()
    function(...) f(...)
  })
  fn <- `%@%`(setup, function() 1, name = "fn")
  expect_equal(seen, "fn")
})

test_that("decorator_context() errors outside a decorator", {
  expect_error(decorator_context(), "inside a decorator")
  helper <- function() decorator_context()
  expect_error(helper(), "inside a decorator")
})

test_that("names resolve through wrapped chains called indirectly", {
  seen <- character()
  helper <- function() decorator_context()$name
  spy <- decorator(function(f, ...) {
    seen <<- c(seen, helper())
    f(...)
  })
  chain <- spy %@% spy %@% function(x) x
  lapply(1, chain)
  expect_equal(seen, rep("<anonymous>", 2))
  chain(1)
  expect_equal(seen, c(rep("<anonymous>", 2), rep("chain", 2)))
})
