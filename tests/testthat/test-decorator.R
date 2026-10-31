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
  expect_error(repeated(nope = 1), "unused argument")
})

test_that("a function passed positionally is always decorated", {
  on_call <- decorator(function(f, hook = function(x) NULL) {
    function(...) {
      hook("called")
      f(...)
    }
  })
  seen <- NULL
  g <- on_call(hook = function(x) seen <<- x) %@% function() 1
  expect_equal(g(), 1)
  expect_equal(seen, "called")
  # A positional function is the one to decorate, never a parameter value.
  h <- on_call(function() 2)
  expect_equal(h(), 2)
  # Parameters can be given alongside the function.
  seen <- NULL
  k <- on_call(function() 3, hook = function(x) seen <<- x)
  expect_equal(k(), 3)
  expect_equal(seen, "called")
})

test_that("decorators show the wrapper's formals", {
  repeated <- decorator(function(f, ..., times = 2, sep) NULL)
  expect_equal(names(formals(repeated)), c("f", "times", "sep"))
  expect_equal(formals(repeated)$times, 2)
  expect_equal(formals(repeated(times = 5))$times, 5)
  setup <- decorator(function(fn) function(...) fn(...))
  expect_equal(names(formals(setup)), "fn")
  expect_equal(names(formals(setup %@% setup)), "f")
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

  expect_output(
    print(decorator(function(f, ...) f(...))),
    "<decorator: anonymous>"
  )
  expect_output(
    print(decorator(function(f, ...) f(...), name = "mine")),
    "<decorator: mine>"
  )

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
  for (nm in c(
    "f",
    "identical",
    "holder",
    "env",
    "fn",
    "stand_in",
    "parent.env",
    ".holder",
    ".previous",
    "if"
  )) {
    call_style <- decorator(function(f, ...) f(...) + 1, name = nm)
    factory <- decorator(function(f) function(...) f(...) + 1, name = nm)
    expect_equal((call_style %@% function(x) x)(1), 2)
    expect_equal((factory %@% function(x) x)(1), 2)
  }
})

test_that("a decorator keeps its own name in errors, whatever it is called", {
  for (nm in c("f", "invoke", "fn", "stand_in", "holder")) {
    bad <- decorator(function(f, ...) stop("wrong"), name = nm)
    err <- tryCatch((bad %@% function(x) x)(1), error = identity)
    expect_identical(conditionCall(err)[[1L]], as.name(nm))
  }
})

test_that("primitives can be decorated", {
  total <- decorator(function(f, ...) f(...) + 1) %@% sum
  expect_equal(total(1, 2), 4)
  neg <- decorator(function(f) function(...) -f(...)) %@% sum
  expect_equal(neg(1, 2), -3)
})

test_that("every level of a nested chain reports the outermost call's name", {
  seen <- character()
  spy <- decorator(function(f, ...) {
    seen <<- c(seen, decorator_context()$name)
    f(...)
  })
  inner_only <- spy %@% function(x) x
  chain <- spy %@% spy %@% spy %@% function(x) x

  chain(1)
  expect_equal(seen, rep("chain", 3))

  # Calling a function that is also wrapped elsewhere uses its own name.
  seen <- character()
  inner_only(1)
  expect_equal(seen, "inner_only")

  # The cached name belongs to one call: another name next time round.
  seen <- character()
  alias <- chain
  alias(1)
  chain(1)
  expect_equal(seen, c(rep("alias", 3), rep("chain", 3)))

  # A wrapper that calls the function twice sees the same name both times.
  seen <- character()
  twice <- decorator(function(f, ...) {
    f(...)
    f(...)
  })
  both <- twice %@% spy %@% function(x) x
  both(1)
  expect_equal(seen, c("both", "both"))
})

test_that("`name` names an anonymous function, including via S7's `:=`", {
  seen <- NULL
  at_setup <- decorator(function(f) {
    seen <<- decorator_context()$name
    f
  })

  expect_equal(decorators(at_setup %@% function(x) x), "at_setup")
  at_setup %@% function(x) x
  expect_equal(seen, "<anonymous>")

  `%@%`(at_setup, function(x) x, name = "given")
  expect_equal(seen, "given")

  # An explicit name wins over the symbol `rhs` was written as.
  fn <- function(x) x
  `%@%`(at_setup, fn, name = "given")
  expect_equal(seen, "given")
  at_setup %@% fn
  expect_equal(seen, "fn")

  expect_error(`%@%`(at_setup, fn, name = 1), "single string")
  expect_error(`%@%`(at_setup, fn, name = c("a", "b")), "single string")
})

test_that("decorating adds only a few stack frames per level of recursion", {
  frames_per_level <- function(dec) {
    frames <- integer()
    f <- dec %@%
      function(k) {
        frames[length(frames) + 1L] <<- sys.nframe()
        if (k == 0) 0 else 1 + f(k - 1)
      }
    f(3)
    diff(frames)[1L]
  }
  expect_lte(frames_per_level(decorator(function(f, ...) f(...))), 4L)
  expect_lte(frames_per_level(decorator(function(f) function(...) f(...))), 5L)
})

test_that("print() lists a decorator's parameters and configuration", {
  repeated <- decorator(function(f, ..., times = 2) rep(f(...), times))
  expect_output(print(repeated), "parameters: times")
  expect_output(print(repeated(times = 3)), "configured: times = 3")
})

test_that("configured language values are quoted into the wrapper", {
  tagged <- decorator(function(f, ..., tag = quote(x)) {
    list(f(...), tag)
  })
  f <- tagged(tag = quote(y + 1)) %@% function() "a"
  expect_equal(f(), list("a", quote(y + 1)))
})

test_that("composed decorators cannot be configured", {
  a <- decorator(function(f, ...) f(...))
  b <- decorator(function(f, ...) f(...))
  both <- a %@% b
  expect_error(both(), "Composed decorators cannot be configured")
  expect_error(both(times = 2), "unused argument")
})

test_that("the right-hand side of %@% must be a function", {
  a <- decorator(function(f, ...) f(...))
  expect_error(a %@% 1, "must be a function")
  expect_error(`%@%`(1, function() 1), "must be a decorator")
})

test_that("non-decorator functions work as plain decorators", {
  plain <- function(f) function(...) f(...) + 1
  f <- plain %@% function() 1
  expect_equal(f(), 2)
})

test_that("parameters named like the internal variable do not collide", {
  # `.config` and `..config` are the names the decorator would use internally.
  dec <- decorator(function(f, ..., .config = "a", ..config = "b") {
    paste(.config, ..config)
  })
  g <- dec(.config = "X", ..config = "Y") %@% function() 1
  expect_equal(g(), "X Y")
  expect_equal(names(formals(dec)), c("f", ".config", "..config"))
})

test_that("avoid_all() returns a name that is not taken", {
  avoid_all <- getFromNamespace("avoid_all", "adorn")
  expect_equal(avoid_all("x", c("a", "b")), "x")
  expect_equal(avoid_all("x", c("x")), ".x")
  expect_equal(avoid_all("x", c("x", ".x")), "..x")
  expect_equal(avoid_all("x", c("x", ".x", "..x")), "...x")
})
