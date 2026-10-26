<!-- badges: start -->
[![R-CMD-check](https://github.com/DyfanJones/adorn/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/DyfanJones/adorn/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/DyfanJones/adorn/graph/badge.svg)](https://app.codecov.io/gh/DyfanJones/adorn)
<!-- badges: end -->

# adorn <img src="man/figures/logo.png" align="right" height="139" alt="adorn hex sticker" />

A framework for creating function decorators in R.

A decorator wraps a function to add behaviour such as logging, timing or
caching, without changing the function itself. adorn applies decorators with an
infix operator, `%@%`, which reads top to bottom:

```r
library(adorn)

shout <- decorator(function(f, ...) toupper(f(...)))

greet <- shout %@% function(who) paste("hello", who)
greet("world")
#> [1] "HELLO WORLD"
```

The point of adorn is that **writing your own decorator is easy**. adorn
ships no decorators of its own; it is the framework for making them.
`vignette("writing-decorators")` defines `timed`, `logged`, `memoize`, `once` and
`retry` with the public API. The examples below that use them assume you have
copied those definitions.

> adorn is in early development, so the API may still change.

## Installation

```r
# install.packages("pak")
pak::pak("dyfanjones/adorn")
```

## Using decorators

```r
slow_add <- timed %@% function(x, y = 1) {
  Sys.sleep(0.1)
  x + y
}
slow_add(2)
#> slow_add took 0.1 s
#> [1] 3
```

Stack decorators with more `%@%`; the one on the left is the outermost.
`decorators()` lists the stack and `undecorate()` returns the original function.

```r
tracked <- logged %@% timed(digits = 2) %@% function(x) x * 2
tracked(21)
#> -> tracked
#> tracked took 0 s
#> <- tracked
#> [1] 42

decorators(tracked)        # "logged" "timed"
undecorate(tracked)(5)     # 10, the original function
```

Decorators can be combined into a reusable bundle before they are applied:

```r
cached_timed <- timed %@% memoize
f <- cached_timed %@% function(x) { Sys.sleep(0.1); x }
```

### Names

A decorator is named from how you write it next to `%@%`: `timed %@% f` and
`timed(digits = 2) %@% f` both give `"timed"`. That is the name `decorators()`
reports. Pass `name` to `decorator()` to set it yourself. Inside a decorator,
`decorator_context()$name` is the name the decorated function was called by
(`fib(10)` gives `"fib"`); calls with no usable name, such as `lapply(x, f)`,
fall back to the name of the decorated function, or `"<anonymous>"`.

A decorated function keeps the formal arguments of the original, so argument
matching, defaults, `missing()`, lazy evaluation and `formals()` behave as
before.

## Writing your own decorator

A decorator is an ordinary function whose first argument is the function being
decorated. `decorator()` turns it into a decorator. There are two styles,
chosen by whether your function has a `...` argument.

**Call style** runs on every call. Arguments after `...` are parameters, set by
calling the decorator.

```r
require_positive <- decorator(function(f, ..., arg = "x") {
  args <- list(...)
  if (any(args[[arg]] <= 0)) stop(arg, " must be positive", call. = FALSE)
  f(...)
})

root <- require_positive(arg = "x") %@% function(x) sqrt(x)
root(16)   # 4
root(-1)   # Error: x must be positive
```

**Factory style** has no `...`. Your function runs **once**, when the function
is decorated, and returns the replacement, so each decorated function gets its
own state.

```r
counted <- decorator(function(f) {
  n <- 0
  function(...) {
    n <<- n + 1
    message(decorator_context()$name, " call #", n)
    f(...)
  }
})

ping <- counted %@% function() "pong"
ping()
#> ping call #1
```

| You want | Write |
|---|---|
| Code on every call | `decorator(function(f, ...) { ...; f(...) })` |
| State per decorated function | `decorator(function(f) { state <- ...; function(...) { ...; f(...) } })` |
| Options | Arguments after `...` (call style) or after `f` (factory style) |
| The function's name | `decorator_context()$name` |

If a decorator's own code fails, the error names the decorator and the function
it was wrapping, for example ``Decorator `logged` was wrapping `double()`.``
`vignette("writing-decorators")` has fuller examples, including `memoize`,
`once` and `retry`.

## Decorating existing code: `decorate()`

`%@%` is for functions you write. `decorate()` is for code that **already
exists**, and is the only way to reach a method that lives inside a generic or
class. It is an S7 generic that dispatches on the target:

| `target` | Example | Returns |
|---|---|---|
| a function | `decorate(stats::runif, logged)` | the decorated function (the original is untouched) |
| the name of an S3 generic | `decorate("print", logged, class = "money")` | the method, replaced in place |
| an S4 generic | `decorate(area, timed, signature = "Square")` | the method, replaced in place |
| an S7 generic | `decorate(speak, timed, class = Dog)` | the method, replaced in place |
| the name of an R6 method | `decorate("add", logged, class = Counter)` | the generator, with the method replaced |

For a function, `decorate(fn, d)` is the same as `d %@% fn`. Pass a stack to
apply several at once; the first is outermost.

```r
safe_sq <- decorate(function(x) x^2, retry(times = 2) %@% timed(digits = 1))
decorators(safe_sq)
#> [1] "retry" "timed"
```

Every method can be decorated in the explicit form,
`decorate(method, decorator, class)`, where S3 and R6 name the method with a
string and S4 and S7 pass the generic itself. There is also a replacement form
that works for all of them and mirrors S7's `method(generic, class) <- fn`:

```r
decorate(speak, Dog) <- logged %@% timed     # S7
decorate(area, "Square") <- timed            # S4
decorate(print, "money") <- logged           # S3, give the generic function
decorate(Counter, "add") <- logged           # R6, public methods
```

Each call wraps whatever the method is now, so calling `decorate()` again adds
another layer on the outside. A method is replaced in place, so every caller of
the generic sees the decorated version. In a package that decorates another
package's classes, do it in `.onLoad()` and expect the change to be seen by the
whole session.

`vignette("classes")` walks through S3, S4, S7 and R6, both for methods you
define and for methods that already exist.

Any package can add a target type, because `decorate()` is an S7 generic:

```r
library(S7)

Pipeline <- new_class("Pipeline", properties = list(steps = class_list))

method(decorate, Pipeline) <- function(target, decorator, ...) {
  target@steps <- lapply(target@steps, decorator)
  target
}
```

adorn does not define `:=`, to avoid clashing with S7, rlang and data.table.
