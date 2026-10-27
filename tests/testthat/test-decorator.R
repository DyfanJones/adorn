test_that("call-style decorators wrap every call and keep formals", {
  shout <- decorator(function(f, ...) toupper(f(...)))
  greet <- shout %@% function(who, greeting = "hello") paste(greeting, who)
  expect_equal(greet("world"), "HELLO WORLD")
  expect_equal(greet(greeting = "hi", who = "x"), "HI X")
  expect_equal(names(formals(greet)), c("who", "greeting"))
})

test_that("missing() and lazy evaluation survive wrapping", {
  id <- decorator(function(f, ...) f(...))
  f <- id %@% function(x, y) if (missing(y)) "no y" else y
  expect_equal(f(1), "no y")
  expect_equal(f(1, 2), 2)
  g <- id %@% function(x) 1
  expect_equal(g(stop("never evaluated")), 1)
})

test_that("... in the original function is forwarded", {
  id <- decorator(function(f, ...) f(...))
  f <- id %@% function(x, ...) sum(x, ...)
  expect_equal(f(1, 2, 3), 6)
})

test_that("parameterised decorators are configured by calling them", {
  repeated <- decorator(function(f, ..., times = 2) rep(f(...), times))
  f <- repeated(times = 3) %@% function() "a"
  expect_equal(f(), rep("a", 3))
  g <- repeated(4) %@% function() "b"
  expect_equal(length(g()), 4)
  expect_error(repeated(nope = 1), "Unknown parameter")
})

test_that("factory-style decorators keep per-function state", {
  counted <- decorator(function(f) {
    n <- 0
    function(...) {
      n <<- n + 1
      f(...) + n
    }
  })
  a <- counted %@% function() 0
  b <- counted %@% function() 0
  expect_equal(c(a(), a(), b()), c(1, 2, 1))
})

test_that("decorators stack top to bottom", {
  add <- function(s) decorator(function(f, ...) paste0(s, f(...)))
  f <- add("a")() %@% add("b")() %@% function() "x"
  expect_equal(f(), "abx")
  expect_equal(decorators(f), c("decorator", "decorator"))
})

test_that("decorators can see the name the function is called by", {
  who <- decorator(function(f, ...) decorator_context()$name)
  hello <- who %@% function() "hi"
  expect_equal(hello(), "hello")
  obj <- list(method = who %@% function() "hi")
  expect_equal(obj$method(), "method")
  expect_equal((who %@% function() "hi")(), "<anonymous>")
  named <- who %@% (function() "hi")
  expect_equal(sapply(1, function(i) named()), "named")
})

test_that("context is available inside factory-style decorators at call time", {
  fac <- decorator(function(f) function(...) decorator_context()$name)
  g <- fac %@% function() 1
  expect_equal(g(), "g")
})

test_that("undecorate() and decorators() introspect the stack", {
  f0 <- function(x) x
  outer <- decorator(function(f, ...) f(...), name = "outer")
  inner <- decorator(function(f, ...) f(...), name = "inner")
  f <- outer %@% inner %@% f0
  expect_equal(decorators(f), c("outer", "inner"))
  expect_identical(undecorate(f), f0)
})

test_that("wrapper survives having its environment replaced", {
  id <- decorator(function(f, ...) f(...) + 1)
  f <- id %@% function(x) x
  environment(f) <- new.env()
  expect_equal(f(1), 2)
})

test_that("invalid decorators are rejected", {
  expect_error(decorator(function(...) 1), "first argument")
  expect_error(decorator(function(f, x, ...) 1), "directly follow")
  bad <- decorator(function(f) 1)
  expect_error(bad %@% function() 1, "must return a function")
})

test_that("stacked decorators all see the outermost call name", {
  who <- decorator(function(f, ...) {
    c(decorator_context()$name, f(...))
  })
  stacked <- who %@% who %@% function() "end"
  expect_equal(stacked(), c("stacked", "stacked", "end"))
})

test_that("decorators using withVisible() preserve invisible results", {
  passthrough <- decorator(function(f, ...) {
    result <- withVisible(f(...))
    if (result$visible) result$value else invisible(result$value)
  })
  quiet <- passthrough %@% function() invisible(1)
  expect_invisible(quiet())
  loud <- passthrough %@% function() 1
  expect_visible(loud())
})

test_that("decorator names are inferred from how they are written", {
  timed <- decorator(function(f, ...) f(...))
  retry <- decorator(function(f, ..., times = 3) f(...))
  loud <- decorator(function(f, ...) f(...), name = "custom")

  f <- timed %@% function() 1
  expect_equal(decorators(f), "timed")

  g <- retry(times = 5) %@% function() 1
  expect_equal(decorators(g), "retry")

  h <- loud %@% function() 1
  expect_equal(decorators(h), "custom")

  k <- timed %@% retry %@% function() 1
  expect_equal(decorators(k), c("timed", "retry"))
})

test_that("inferred names reach decorator_context() and print()", {
  who <- decorator(function(f, ...) decorator_context()$decorator)
  f <- who %@% function() 1
  expect_equal(f(), "who")

  expect_output(print(decorator(function(f, ...) f(...))), "<decorator: decorator>")
  expect_output(print(decorator(function(f, ...) f(...), name = "mine")), "<decorator: mine>")

  lst <- list(who = who)
  g <- lst$who %@% function() 1
  expect_equal(decorators(g), "decorator")

  h <- base::identity
  expect_equal(decorators(who %@% h), "who")
})

test_that("errors in a decorator's own code are reported against the decorator", {
  logged <- decorator(function(f, ...) {
    f(...)
    not_defined
  })
  double <- logged %@% function(x) x * 2
  err <- tryCatch(double(2), error = identity)
  expect_match(conditionMessage(err), "not_defined")
  expect_identical(conditionCall(err), quote(logged(f, ...)))

  # Errors from the decorated function itself keep their own call.
  inner <- logged %@% function(x) stop("inner")
  err <- tryCatch(inner(1), error = identity)
  expect_match(conditionMessage(err), "inner")
  expect_false(identical(conditionCall(err), quote(logged(f, ...))))

  # Factory style, at decoration and at call time.
  setup_fails <- decorator(function(f) stop("setup"))
  err <- tryCatch(setup_fails %@% function() 1, error = identity)
  expect_identical(conditionCall(err), quote(setup_fails(stand_in)))
  call_fails <- decorator(function(f) function(...) stop("call"))
  err <- tryCatch((call_fails %@% function() 1)(), error = identity)
  expect_identical(conditionCall(err), quote(call_fails(...)))
})

test_that("decorators named like internal symbols still work", {
  for (nm in c("f", "identical", "holder", "env", "f_env")) {
    call_style <- decorator(function(f, ...) f(...) + 1, name = nm)
    factory <- decorator(function(f) function(...) f(...) + 1, name = nm)
    expect_equal((call_style %@% function(x) x)(1), 2)
    expect_equal((factory %@% function(x) x)(1), 2)
  }
})

test_that("primitives can be decorated", {
  total <- decorator(function(f, ...) f(...) + 1) %@% sum
  expect_equal(total(1, 2), 4)
  neg <- decorator(function(f) function(...) -f(...)) %@% sum
  expect_equal(neg(1, 2), -3)
})
