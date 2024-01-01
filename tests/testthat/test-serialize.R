test_that("NULL", {
  expect_same_serialization(NULL)
})

test_that("R_UnboundValue", {
  # TODO
})

test_that("R_MissingArg", {
  # TODO
})

test_that("ALTREP", {
  # TODO
})

test_that("SYMSXP", {
  foo <- as.name("foo")
  bar <- as.name("bar")
  expect_same_serialization(foo)
  expect_same_serialization(list(foo, foo, bar, foo))
})

test_that("ENVSXP", {
  expect_same_serialization(emptyenv())
  expect_same_serialization(baseenv())
  expect_same_serialization(globalenv())
  expect_same_serialization(.BaseNamespaceEnv)
  # TODO expect_same_serialization(asNamespace("covrlabs"))
  # TODO expect_same_serialization(parent.env(asNamespace("covrlabs")))
})

test_that("LISTSXP", {

})

test_that("LANGSXP", {

})

test_that("PROMSXP", {

})

test_that("DOTSXP", {

})

test_that("CLOSXP", {
  f <- function() NULL
  environment(f) <- globalenv()
  expect_same_serialization(f)

  # arg
  f2 <- function(a = 1) a
  environment(f2) <- globalenv()
  expect_same_serialization(f2)

  # missing arg
  f3 <- function(a) a
  environment(f3) <- globalenv()
  expect_same_serialization(f3)
})

test_that("EXTPTRSXP", {

})

test_that("WEAKREFSXP", {

})

test_that("SPECIALSXP", {

})

test_that("BUILTINSXP", {

})

test_that("CHARSXP", {

})

test_that("LGLSXP", {

})

test_that("INTSXP", {

})

test_that("REALSXP", {

})

test_that("CPLXSXP", {

})

test_that("STRSXP", {
  expect_same_serialization("a")
  expect_same_serialization(character())
  expect_same_serialization(c("foo", "bar"))
  expect_same_serialization(NA_character_)
  expect_same_serialization(c("foo", NA, "bar"))
})

test_that("VECSXP", {

})

test_that("EXPRSXP", {

})

test_that("BCODESXP", {
  f <- function() NULL
  f <- remove_source(f)
  environment(f) <- globalenv()
  f <- compiler::cmpfun(f)

  expect_same_serialization(f)
})

test_that("RAWSXP", {

})

test_that("OBJSXP", {

})

test_that("immediate bindings", {

})
