test_that("NULL", {
  expect_same_serialization(NULL)
})

test_that("R_UnboundValue", {
  expect_same_serialization(list(unbound_value()))
})

test_that("R_MissingArg", {
  expect_same_serialization(missing_arg())
})

test_that("ALTREP", {
  expect_same_serialization(1:10)
})

test_that("SYMSXP", {
  foo <- as.name("foo")
  bar <- as.name("bar")
  expect_same_serialization(foo)
  expect_same_serialization(list(foo, foo, bar, foo))
})

test_that("ENVSXP", {
  # special envs
  expect_same_serialization(emptyenv())
  expect_same_serialization(baseenv())
  expect_same_serialization(globalenv())
  expect_same_serialization(.BaseNamespaceEnv)

  # package env
  suppressWarnings(
    expect_same_serialization(as.environment("package:covrlabs"))
  )
  # namespace env
  expect_same_serialization(asNamespace("covrlabs"))

  # noname env
  e <- new.env(parent = emptyenv())
  e2 <- new.env(parent = e)
  e2$foo <- "bar"
  e2$bar <- c(1:3, 4L)
  e2$parent <- e
  expect_same_serialization(e2)

  attr(e2, "a") <- list(1L)
  expect_same_serialization(e2)

  # hashing
  expect_same_serialization(list(emptyenv(), emptyenv()))
})

test_that("LISTSXP", {
  pl <- as.pairlist(list(a = 1, b = 2))
  expect_same_serialization(pl)
  attr(pl, "a") <- list(1L)
  expect_same_serialization(pl)
})

test_that("LANGSXP", {
  lng <- as.call(list(as.name("foo"), a = 1, b = 2))
  expect_same_serialization(lng)
  attr(lng, "a") <- list(1L)
  expect_same_serialization(lng)
})

test_that("PROMSXP", {
  f <- function(x) {
    expect_same_serialization(environment())
  }
  f(10)
})

test_that("DOTSXP", {
  f <- function(...) {
    expect_same_serialization(environment())
  }
  f(10)
})

test_that("CLOSXP", {
  f <- function() NULL
  expect_same_serialization(f)

  # arg
  f2 <- function(a = 1) a
  expect_same_serialization(f2)

  # missing arg
  f3 <- function(a) a
  expect_same_serialization(f3)

  # attr
  attr(f, "a") <- list(1L)
  expect_same_serialization(f)
})

test_that("EXTPTRSXP", {
  expect_same_serialization((xptrsxp()))
})

test_that("WEAKREFSXP", {
  expect_same_serialization((weakrefsxp()))
})

test_that("SPECIALSXP", {
  expect_same_serialization(base::on.exit)
})

test_that("BUILTINSXP", {
  expect_same_serialization(base::names)
})

test_that("CHARSXP", {
  expect_same_serialization("foo")
  expect_same_serialization(structure("foo", names = "bar"))
})

test_that("LGLSXP", {
  expect_same_serialization(logical())
  expect_same_serialization(c(TRUE))
  expect_same_serialization(c(TRUE))
  expect_same_serialization(NA)
  expect_same_serialization(c(TRUE, FALSE, NA))
  expect_same_serialization(c(a = TRUE, FALSE, c = NA))
})

test_that("INTSXP", {
  expect_same_serialization(integer())
  expect_same_serialization(c(100L))
  expect_same_serialization(NA_integer_)
  expect_same_serialization(c(0L, 2L, NA))
  expect_same_serialization(c(a = 0L, 2L, c = NA))
})

test_that("REALSXP", {
  expect_same_serialization(double())
  expect_same_serialization(c(100))
  expect_same_serialization(NA_real_)
  expect_same_serialization(c(0, 2, NA))
  expect_same_serialization(c(a = 0, 2, c = NA))
})

test_that("CPLXSXP", {
  expect_same_serialization(as.complex(1:10))
  expect_same_serialization(structure(as.complex(1:10), a = list(1)))
})

test_that("STRSXP", {
  expect_same_serialization("a")
  expect_same_serialization(character())
  expect_same_serialization(c("foo", "bar"))
  expect_same_serialization(NA_character_)
  expect_same_serialization(c("foo", NA, "bar"))
  expect_same_serialization(c(a = "foo", NA, c = "bar"))
})

test_that("VECSXP", {
  expect_same_serialization(list())
  expect_same_serialization(list(list()))
  expect_same_serialization(list(new.env(parent = emptyenv()), runif(5)))
  expect_same_serialization(list(a = 1, b = runif(10)))
})

test_that("EXPRSXP", {
  expect_same_serialization(expression(1 + foo))
  expect_same_serialization(structure(expression(1 + foo), a = list(1)))
})

test_that("BCODESXP", {
  f <- function() NULL
  f <- compiler::cmpfun(f)

  expect_same_serialization(f)
})

test_that("RAWSXP", {
  expect_same_serialization(raw())
  expect_same_serialization(raw(100))
  expect_same_serialization(as.raw(as.integer(runif(100) * 100)))
  expect_same_serialization(structure(raw(100), a = list(1)))
})

test_that("immediate bindings", {
  if (getRversion() < "4.0.0") skip("Needs newer R version")
  expect_same_serialization(bnd_cell_int(41L))
  expect_same_serialization(bnd_cell_real(41))
  expect_same_serialization(bnd_cell_lgl(TRUE))
})

test_that("OBJSXP", {
  s4 <- methods::getClass("double")
  expect_same_serialization(s4)
})

test_that("closxp_callback", {
  f <- function() {
    1
    2
    3
  }
  # otherwise the environment, including f, f2, etc. is also saved
  environment(f) <- globalenv()
  f2 <- unserialize(serialize(f, closxp_callback = trace_calls))
  expect_equal(f2, trace_calls(f))
})