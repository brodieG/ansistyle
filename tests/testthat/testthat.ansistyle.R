library(ansistyle)
old.opt <- options(ansistyle.enabled=TRUE)
test_that("colors working", {
  expect_identical(ansi_style("hello", "red"), "\033[31mhello\033[39m")
  expect_identical(
    ansi_style(letters[1:3], "red"),
    c("\033[31ma\033[39m", "\033[31mb\033[39m", "\033[31mc\033[39m")
  )
  expect_identical(
    clrd <- ansi_style(letters[1:3], c("red", NA, "green")),
    c("\033[31ma\033[39m", "b", "\033[32mc\033[39m")
  )
  expect_error(ansi_style(letters[1:3], c("red", "green")), "style")
  expect_identical(contains_ansi(clrd), c(TRUE, FALSE, TRUE))
  expect_identical(contains_ansi(letters[1:3]), rep(FALSE, 3L))
  expect_identical(strip_ansi(clrd), letters[1:3])
  expect_is(valid_ansi_styles(), "character")
  # For visual inspection

  cat("\n")
  ansi_style_palette()
} )
test_that("misc", {
  expect_warning(ansi_available("blahblah"), "assuming NULL")
} )
test_that("emacs testing", {
  emacs.1 <- Sys.getenv("EMACS", unset=NA)
  emacs.2 <- Sys.getenv("INSIDE_EMACS", unset=NA)

  Sys.setenv(EMACS="t")

  # leaving out single quotes b/c wreak havoc with older version of crayon used
  # by testthat (i.e. should really be "'24.1,2344'")

  Sys.setenv(INSIDE_EMACS="24.1,2344")

  expect_true(ansistyle:::inside_emacs())
  expect_equal(ansistyle:::emacs_version(), c(24, 1))

  Sys.unsetenv("EMACS")
  Sys.unsetenv("INSIDE_EMACS")

  expect_false(ansistyle:::inside_emacs())
  expect_equal(ansistyle:::emacs_version(), NA_integer_)

  if(!is.na(emacs.1)) Sys.setenv(EMACS=emacs.1)
  if(!is.na(emacs.2)) Sys.setenv(INSIDE_EMACS=emacs.2)

} )
options(old.opt)
