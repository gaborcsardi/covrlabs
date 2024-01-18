test_that("find_last_line", {
  txt <-
'  -:    0:Source:init.c
   -:    1:
   -:    2:#include "cli.h"
   -:    3:#include "keypress.h"
   -:    4:#include "cleancall.h"
   -:    5:
   -:    6:#include <R_ext/Rdynload.h>
'

  expect_equal(find_last_line(charToRaw(txt)), 6L)
  # w/o trailing newline
  expect_equal(find_last_line(head(charToRaw(txt), -1)), 6L)
  # w/o trailing newline on windows, remove \r\n
  expect_equal(find_last_line(head(charToRaw(txt), -2)), 6L)

  # -- irregular line at end ----------------------------------------------

  txt <-
'  -:    0:Source:init.c
   -:    1:
   -:    2:#include "cli.h"
   -:    3:#include "keypress.h"
   -:    4:#include "cleancall.h"
   -:    5:
   -:    6:#include <R_ext/Rdynload.h>
function clic_unload called 1 returned 100% blocks executed 100%
'

  expect_equal(find_last_line(charToRaw(txt)), 6L)
  expect_equal(find_last_line(head(charToRaw(txt), -1)), 6L)
  expect_equal(find_last_line(head(charToRaw(txt), -2)), 6L)

  # -- multiple irregular lines at end ------------------------------------

  txt <-
'  -:    0:Source:init.c
   -:    1:
   -:    2:#include "cli.h"
   -:    3:#include "keypress.h"
   -:    4:#include "cleancall.h"
   -:    5:
   -:    6:#include <R_ext/Rdynload.h>
function clic_unload called 1 returned 100% blocks executed 100%
function clic_unload called 1 returned 100% blocks executed 100%
function clic_unload called 1 returned 100% blocks executed 100%
'

  expect_equal(find_last_line(charToRaw(txt)), 6L)
  expect_equal(find_last_line(head(charToRaw(txt), -1)), 6L)
  expect_equal(find_last_line(head(charToRaw(txt), -2)), 6L)

  # -- larger line number -------------------------------------------------

  txt <-
'  -:    0:Source:init.c
   -:    1:
   -:  123:#include "cli.h"
'

  expect_equal(find_last_line(charToRaw(txt)), 123L)
  expect_equal(find_last_line(head(charToRaw(txt), -1)), 123L)
  expect_equal(find_last_line(head(charToRaw(txt), -2)), 123L)

  # -- edge case: single line ---------------------------------------------

  txt <- '  -:    1:Source:init.c\n'

  expect_equal(find_last_line(charToRaw(txt)), 1L)
  expect_equal(find_last_line(head(charToRaw(txt), -1)), 1L)
  expect_equal(find_last_line(head(charToRaw(txt), -2)), 1L)

  # -- edge case: no number -----------------------------------------------

  txt <- '  -:    0:Source:init.c\n'

  expect_equal(find_last_line(charToRaw(txt)), 0L)
  expect_equal(find_last_line(head(charToRaw(txt), -1)), 0L)
  expect_equal(find_last_line(head(charToRaw(txt), -2)), 0L)

  txt <- 'function clic_unload called 1 returned 100% blocks\n'

  expect_equal(find_last_line(charToRaw(txt)), 0L)
  expect_equal(find_last_line(head(charToRaw(txt), -1)), 0L)
  expect_equal(find_last_line(head(charToRaw(txt), -2)), 0L)

  txt <- '\n'

  expect_equal(find_last_line(charToRaw(txt)), 0L)
  expect_equal(find_last_line(head(charToRaw(txt), -1)), 0L)
  expect_equal(find_last_line(head(charToRaw(txt), -2)), 0L)
})