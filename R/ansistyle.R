#' Style Console Output With Ansi Escape Sequences
#'
#' Stripped down dependency-less version of the \code{crayon} package by
#' Gabor Csardi. Modifies character vectors by adding ANSI escape sequences to
#' render text with style in terminals that support ANSI style escape sequences.
#'
#' @name ansistyle-package
#' @docType package

NULL

# Terminal styling functions
#
# The code is taken and adapted from the "crayon" package by Gabor Csardi
# (https://github.com/gaborcsardi/crayon) under the MIT license:
#
#    License: MIT + file LICENSE
#    YEAR: 2014-2015
#    COPYRIGHT HOLDER: Gabor Csardi
#
# The modified code is under the GPL-3 license.

# - R/has_ansi.r ---------------------------------------------------------------
# https://github.com/gaborcsardi/crayon/commit/5de3d97fe6d4d0627cdfa2b8b2f4d402dc404c63

# ansi style code regex pattern

ansi_regex <- paste0("(?:(?:\\x{001b}\\[)|\\x{009b})",
  "(?:(?:[0-9]{1,3})?(?:(?:;[0-9]{0,3})*)?[A-M|f-m])",
  "|\\x{001b}[A-M]")

#' Check For or Remove ANSI Escape Sequences From a String
#'
#' Uses the \code{ansistyle:::ansi_regex} pattern.
#'
#' @export
#' @param string character vector
#' @return input, without ANSI escape sequences for \code{strip_ansi}, TRUE or
#'   FALSE for \code{contains_ansi}
#' @aliases contains_ansi

strip_ansi <- function(string) {
  gsub(ansi_regex, "", string, perl = TRUE)
}
#' @export
#' @rdname strip_ansi

contains_ansi <- function(string) {
  grepl(ansi_regex, string, perl = TRUE)
}

# - R/has_color.R --------------------------------------------------------------
# https://github.com/gaborcsardi/crayon/commit/7c3319a75781c601bdab20c1b35cbd5d4c5085c8

#' Attempt to Detect Whether Terminal Supports ANSI Styles
#'
#' @export
#' @param force TRUE, FALSE, or NULL; set to NULL to allow auto-detection
#'   (default), or TRUE or FALSE to force this function to return TRUE or FALSE
#' @return TRUE or FALSE

ansi_available <- function(force=getOption("ansistyle.enabled")) {
  ## Colors forced?

  if(isTRUE(force) || identical(force, FALSE)) return(force)
  if(!is.null(force)) {
    warning(
      "Option `ansistyle.enabled` must be TRUE, FALSE, or NULL; assuming NULL"
    )
    force <- NULL
  }
  # nocov start
  # no great way of testing this

  res <- if(!is.null(force)) force else {
    if (!interactive()) {
      ## Are we in interactive mode?
      FALSE
    } else if (.Platform$OS.type == "windows") {
      ## Are we in a windows terminal?
      FALSE
    } else if (
      inside_emacs() &&
      !any(is.na(e_v <- emacs_version())) && e_v[1] >= 23
    ) {
      ## Running in a recent Emacs?
      TRUE
    } else if ("COLORTERM" %in% names(Sys.getenv())) {
      ## COLORTERM set?
      TRUE
    } else if (Sys.getenv("TERM") == "dumb") {
      ## dumb terminal is not good
      FALSE
    } else {
      ## Otherwise try to guess based on TERM
      grepl("^screen|^xterm|^vt100|color|ansi|cygwin|linux",
        Sys.getenv("TERM"), ignore.case = TRUE, perl = TRUE)
    }
  }
  res
  # nocov end
}
# - R/styles.r ----------------------------------------------------------------
# https://github.com/gaborcsardi/crayon/commit/5d6170adefd5f996558f9b9158680715caf65be5

## Styles

ansi.codes <- list(
  reset = c(0, 0),
  bold = c(1, 22), # 21 isn't widely supported and 22 does the same thing
  blurred = c(2, 22),
  italic = c(3, 23),
  underline = c(4, 24),
  inverse = c(7, 27),
  hidden = c(8, 28),
  strikethrough = c(9, 29),

  black = c(30, 39),
  red = c(31, 39),
  green = c(32, 39),
  yellow = c(33, 39),
  blue = c(34, 39),
  magenta = c(35, 39),
  cyan = c(36, 39),
  white = c(37, 39),
  silver = c(90, 39),

  bgBlack = c(40, 49),
  bgRed = c(41, 49),
  bgGreen = c(42, 49),
  bgYellow = c(43, 49),
  bgBlue = c(44, 49),
  bgMagenta = c(45, 49),
  bgCyan = c(46, 49),
  bgWhite = c(47, 49)
)
codes.mx <- do.call(rbind, ansi.codes)

#' Return Available ANSI styles
#'
#' This runs \code{names(ansistyle:::ansi.codes)}.  If you want to look at the
#' actual codes used try \code{ansistyle:::ansi.codes)}.  If you access the
#' internal objects directly keep in mind that those may change in the future,
#' whereas this function should continue to behave as currently documented.
#'
#' @export
#' @return character vector with allowable styles

valid_ansi_styles <- function() names(ansi.codes)

#' Add ANSI Style Escape Sequences Codes Around Elements of a Character Vector
#'
#' Prepends and appends the necessary ANSI sequences to produce the requested
#' style.  Valid styles are those in \code{names(ansistyle:::ansi.codes)}.
#' Nesting is \bold{not} supported, except for those ANSI sequences that have
#' different terminators (e.g. font colors vs. background colors). For a better
#' and more complete implementation see the \code{crayon} package by Gabor
#' Csardi.
#'
#' @export
#' @param txt character vector to style; every element of the vector will have
#'   escape sequences added at the beginning and end of the string
#' @param style character, either length 1L or same length as \code{txt}, what
#'   style to use, see \code{link{ansi_styles}}
#' @param use.style logical(1L) whether to use style or not; provides a
#'   mechanism for turning off styling for systems that do not support it
#' @return character vector, \code{txt} with ansi escape sequences added

ansi_style <- function(
  txt, style, use.style=ansi_available()
) {
  if(!isTRUE(use.style) && !identical(use.style, FALSE))
    stop("Argument `use.style` must be TRUE or FALSE.")
  if(!isTRUE(use.style)) return(txt)
  if(!is.character(txt)) stop("Argument `txt` must be character")
  style.no.na <- Filter(Negate(is.na), style)
  if(
    !is.character(style) || !all(style.no.na %in% rownames(codes.mx)) ||
    (length(style) != 1L && length(style) != length(txt))
  )
    stop(
      "Argument `style` must be character(1L) or the same length as `txt` ",
      "and may only contain valid style codes."
    )
  if(length(style) == 1L) style <- rep(style, length(txt))
  ifelse(
    is.na(style), txt,
    sprintf(
      "\u001b[%sm%s\u001b[%sm", codes.mx[, 1L][style], txt,
      codes.mx[, 2L][style]
  ) )
}
# - R/utils.r ------------------------------------------------------------------
#
# https://github.com/gaborcsardi/crayon/commit/7e2e0963acf414e20bfe40a58e9a3bc6a7fe411f

inside_emacs <- function() {
  Sys.getenv("EMACS") != ""
}
emacs_version <- function() {
  ver <- Sys.getenv("INSIDE_EMACS")
  if (ver == "") return(NA_integer_)

  ver <- gsub("'", "", ver)
  ver <- strsplit(ver, ",", fixed = TRUE)[[1]]
  ver <- strsplit(ver, ".", fixed = TRUE)[[1]]
  as.numeric(ver)
}
