#' Convert numbers to English words
#'
#' @param x integer vector 
#' @param cap logical, if TRUE capitalise first letter of word. 
#' @param hyphen logical, if TRUE insert \code{-} between 21 and
#'   99 (except 30, 40, etc.).
#' @param and logical, if TRUE insert \code{and} between hundreds and tens, 
#'   e.g. 110 as \dQuote{one hundred and ten} 
#' @param round logical, if TRUE decimals are rounded to integers, 
#'   using \code{round()}. If FALSE, decimals cause failure
#' @return character vector.
#' @examples 
#' numWord(0, cap = TRUE)
#' numWord(0:121, and = TRUE)
#' numWord(1e6)
#' numWord(1e11+12345678)
#' numWord(-987654321)
#' numWord(1e15-1)
#' numWord(c(1,2,3,4.55), round = TRUE)
#' @export
#' 
numWord <- function(x, cap = FALSE, hyphen = TRUE, and = FALSE, round = FALSE) {

  # Define function, test if whole
  isWhole <- function(x, tol = .Machine$double.eps^0.5) {
    abs(x - round(x)) < tol
  }

  if (round) {
    x <- round(x, 0)
  } else {
    if (!all(isWhole(x))) {
      stop("Input is not an integer.")
    }
  }

  # Check if value is too large
  if (any(abs(x) >= 1e15)) {
    stop("Absolute value >1e15.")
  }

  # Avoid scientific notation
  opts <- options(scipen = 15)
  on.exit(options(opts), add = TRUE)  

  # Define words
  zero_to_19 <- c("zero", "one", "two", "three", "four", "five", "six", 
    "seven", "eight", "nine", "ten", "eleven", "twelve", 
    paste0(c("thir", "four", "fif", "six", "seven", "eigh", "nine"), "teen")
  )
  names(zero_to_19) = as.character(0:19)

  tens <- c('twenty', 'thirty', 'forty', 'fifty', 'sixty', 'seventy', 'eighty', 'ninety')
  names(tens) <- as.character(seq(20, 90, 10))

  marks <- c('', 'thousand,', 'million,', 'billion,', 'trillion,')

  # Define conversion functions
  # 0-9
  convert_1 <- function(x_c) {
    zero_to_19[x_c]
  }

  # 10-99
  convert_2 <- function(x_c) {
    x_cs <- strsplit(x_c, split = '')[[1]]
    if (x_cs[1] == 1) return(zero_to_19[x_c])  # 10-19
    if (x_cs[2] == 0) return(tens[x_c])  # 20, 30, 40, ...
    # 21, 22, etc.
    paste(tens[as.integer(x_cs[1]) - 1], convert_1(x_cs[2]), sep = if (hyphen) '-' else ' ')
  }

  # 100-999
  convert_3 = function(x_c) {
    x_cs = strsplit(x_c, split = '')[[1]]
    n_hundreds = paste(convert_1(x_cs[1]), 'hundred', sep = ' ')
    out = if (x_cs[2] == '0') {
      if (x_cs[3] == '0') return(n_hundreds)  # x00
      convert_1(x_cs[3])  # x0x
    } else {
      convert_2(paste(x_cs[2:3], collapse = ''))  # xxx
    }
    paste(n_hundreds, out, sep = if (and) ' and ' else ' ')
  }

  convert_le3 = function(x_c) {
    x_c = gsub('^0+', '', x_c) # avoid something like 000, 001, 010; but also remove 0
    n = nchar(x_c)
    if (n == 0) return('')
    if (n == 1) return(convert_1(x_c))
    if (n == 2) return(convert_2(x_c))
    if (n == 3) return(convert_3(x_c))
  }

  convert_one = function(x) {
    minus = if (x >= 0) '' else {
      x = abs(x); 'minus '
    }
    if (x == 0) {
      out = 'zero'  # because convert_le3 removed all 0s
    } else {
      x_marks = strsplit(format(x, big.mark = ','), split = ',')[[1]]  # e.g. 123,456,789
      out = vapply(x_marks, convert_le3, character(1))  # group by 3 digits
      x_marks2 = marks[length(x_marks):1]  # units?
      x_marks2[which(out == '')] = ''  # e.g. 4,000,123, 000, remove millions
      out = paste(out, x_marks2, sep = ' ', collapse = ' ')  # zip together
    }
    out = paste0(minus, out)
    out = gsub('^ *|,? *$', '', out)  # trim heading/trailing space
    out = gsub(' {2,}', ' ', out)  # remove multiple spaces
    if (cap) out = sub('^([a-z])', '\\U\\1', out, perl = TRUE)
    out
  }

  if (length(x) > 1) vapply(x, convert_one, character(1)) else convert_one(x)
}
