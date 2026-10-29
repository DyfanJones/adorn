# adorn (development version)

* `decorator()` gains a `formals` argument. With `formals = "replace"`, a
  factory-style decorator gives the decorated function the formal arguments of
  the function it returns instead of those of the original, so decorators can
  change the signature (for example partial application or currying). The
  default, `"keep"`, is unchanged. Using `"replace"` with a call-style
  decorator is an error. Printing a decorator shows a non-default `formals`.
* New vignette, `vignette("partial-decorators")`.

# adorn 0.1.0

* Initial CRAN submission.
