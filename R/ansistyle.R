#' Style Console Output With ANSI Escape Sequences
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

#' Regex Pattern To Match ANSI Escape Sequences
#'
#' @export

.ansistyle_ansi_regex <- paste0("(?:(?:\\x{001b}\\[)|\\x{009b})",
  "(?:(?:[0-9]{1,3})?(?:(?:;[0-9]{0,3})*)?[A-M|f-m])",
  "|\\x{001b}[A-M]")

#' Check For or Remove ANSI Escape Sequences From a String
#'
#' Uses the \code{ansistyle:::ansistyle_ansi_regex} pattern.
#'
#' @export
#' @param string character vector
#' @return input, without ANSI escape sequences for \code{strip_ansi}, TRUE or
#'   FALSE for \code{contains_ansi}
#' @aliases contains_ansi
#' @examples
#' in.red <- ansi_style("hello\n", "red", use.style=TRUE)
#' contains_ansi(in.red)
#' cat(in.red)
#' contains_ansi(strip_ansi(in.red))
#' cat(strip_ansi(in.red))

strip_ansi <- function(string) {
  gsub(.ansistyle_ansi_regex, "", string, perl = TRUE)
}
#' @export
#' @rdname strip_ansi

contains_ansi <- function(string) {
  grepl(.ansistyle_ansi_regex, string, perl = TRUE)
}

# - R/has_color.R --------------------------------------------------------------
# https://github.com/gaborcsardi/crayon/commit/7c3319a75781c601bdab20c1b35cbd5d4c5085c8

#' Attempt to Detect Whether Terminal Supports ANSI Styles
#'
#' Loosely based on \code{crayon:::has_color}, but with some modifications.  In
#' particular we test for interactive sessions instead of checking that
#' \code{stdout} is a terminal to allow running with captured output.
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
      # Are we in interactive mode?
      FALSE
    } else if (.Platform$OS.type == "windows") {
      # Are we in a windows terminal that supports color?
      Sys.getenv("ConEmuANSI") == "ON" || Sys.getenv("CMDER_ROOT") != ""
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
  inverse = c(7, 27),
  underline = c(4, 24),
  hidden = c(8, 28),
  strikethru = c(9, 29),

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
#' @seealso \code{\link{ansi_style_palette}}
#' @return character vector with allowable styles
#' @examples
#' valid_ansi_styles()

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
#'   style to use, see \code{\link{valid_ansi_styles}}
#' @param use.style logical(1L) whether to use style or not. Provides a
#'   mechanism for turning off styling for systems that do not support it; if
#'   set to NULL will auto detect with \code{\link{ansi_available()}}
#' @return character vector, \code{txt} with ansi escape sequences added
#' @seealso \code{\link{valid_ansi_styles}}, \code{\link{ansi_style_palette}}
#' @examples
#' cat(ansi_style("hello", "red"), "\n")
#'
#' ## some nesting possible if escape sequences allow it
#' cat(ansi_style(c("baz", ansi_style("foo", "bgGreen"), "bar"), "blue"), "\n")

ansi_style <- function(
  txt, style, use.style=getOption("ansistyle.use.style")
) {
  use.style <- valid_use_style(use.style)
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

# - Other ----------------------------------------------------------------------

#' Demonstrate ANSI Styles
#'
#' Displays each available style with the style applied.  Will apply ANSI escape
#' sequences no matter what, so if your terminal does not support them you will
#' see may see them displayed along with the text, instead of the styled text.
#'
#' @export
#' @return NULL, invisibly
#' @examples
#' ansi_style_palette()

ansi_style_palette <- function() {
  if(!ansi_available())
    message(
      "ANSI escape sequences do not appear to be supported by your terminal."
    )
  styles <- valid_ansi_styles()
  styled <- ansi_style(rep("Abcd", length(styles)), styles, use.style=TRUE)
  len <- length(styled)
  rows <- ceiling(len / 3)
  len.tot <- rows * 3
  out <- c(
    paste0(format(paste0(styles, ":")), styled), character(len.tot - len)
  )
  sep <- "|"
  for(i in seq_len(rows))
    cat(out[i], sep, out[i + rows], sep, out[i + 2 * rows], "\n")
  invisible(NULL)
}
#' Count Characters Accounting For ANSI Escape Sequences
#'
#' Behaves just like \code{\link{nchar}} when \code{use.style} is \code{FALSE},
#' and ignores ANSI escape sequences in counts if it is \code{TRUE}
#'
#' @export
#' @param txt character vector to count characters of
#' @param use.style TRUE, FALSE, or NULL, whether we should interpret ANSI
#'   escape sequences as escape sequences, or literally; NULL will auto-detect
#'   ansi support with \code{\link{ansi_available}}
#' @return integer same length as \code{txt}

ansi_style_nchar <- function(txt, use.style=getOption("ansistyle.use.style")) {
  use.style <- valid_use_style(use.style)
  if(!isTRUE(use.style)) {
    nchar(txt)
  } else {
    nchar(gsub(.ansistyle_ansi_regex, "", txt))
  }
}
# Validation, will return error message with parent.call

valid_use_style <- function(use.style) {
  if(!isTRUE(use.style) && !identical(use.style, FALSE) && !is.null(use.style))
    stop(
      simpleError(
        "Argument `use.style` must be TRUE, FALSE, or NULL.",
        call=sys.call(-1L)
    ) )
  if(is.null(use.style)) use.style <- ansi_available()
  use.style
}
