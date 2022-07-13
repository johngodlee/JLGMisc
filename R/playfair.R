#' Create a playfair-style cipher
#'
#' @param x character string to encode
#'
#' @return list with three slots: (1) encoded message (2) decoder matrix 
#'     (3) decoder lookup table
#'
#' @details Creates a cipher based on the original playfair cipher. 
#'     Unlike the original playfair cipher this method produces a 
#'     6x6 grid of upper and lowercase letters. Additionally, the 
#'     behaviour when a keypair appear on the same row or column of 
#'     the decoder matrix is different. In this version keypairs which
#'     appear on the same row or column are merely swapped rather than
#'     transposed as in the original cipher. 
#'     Messages to be encoded are converted to uppercase and all
#'     non-alphabet characters are stripped out.
#'
#' @examples
#' x <- "This is a test"
#' playfair(x)
#' 
#' @export
#' 
playfair <- function(x) {
  # List all letters, upper and lowercase (52 chr)
  all_chr <- c(letters, LETTERS)

  # Create 6x6 matrix of distinct letters
  mat <- matrix(sample(all_chr, 6*6), 6, 6)

  # Get all pairwise combinations of grid positions
  locs_pairs <- matrix(combn(seq(length(mat)), 2), ncol = 2)
  locs_clean <- unique(locs_pairs[locs_pairs[,1] != locs_pairs[,2],])

  # Randomly sample pairs of grid positions 
  # 26 times to create windows for each letter
  locs_letters <- locs_clean[sample(nrow(locs_clean), 26),]

  # Order the pairs to always take the top left of each pair
  locs_pairs <- apply(locs_letters, 1, function(y) {
    c(min(y), max(y))
    })

  # Search matrix for grid positions to get letter combinations
  combins <- apply(locs_pairs, 2, function(y) {
    paste0(mat[y[1]], mat[y[2]])
  })

  # Make tidy dataframe of letter codes
  code_df <- data.frame(input = combins,
    output = LETTERS)

  # Split x into component characters, 
  # remove spaces and non-letter characters
  x_string <- unlist(strsplit(toupper(x), 
    split = ""))
  x_string_clean <- x_string[x_string %in% LETTERS]
  decoded <- code_df[match(x_string_clean, code_df$output), "input"]

  # For each character, encode
  out <- unlist(lapply(decoded, function(i) {
    # Split string
    i_split <- unlist(strsplit(i, split = ""))

    # Find locations in matrix
    letter_one <- c(which(mat == i_split[1], arr.ind = TRUE))
    letter_two <- c(which(mat == i_split[2], arr.ind = TRUE))

    # Get opposite locations
    if (letter_one[1] == letter_two[1]) {
      opp_one <- mat[letter_one[1], letter_two[2]]
      opp_two <- mat[letter_two[1], letter_one[2]]
    } else {
      opp_one <- mat[letter_two[1], letter_one[2]]
      opp_two <- mat[letter_one[1], letter_two[2]]
    }

    # Combine into one string
    out <- paste0(opp_one, opp_two)

    out
  }))
  return(list(code = out, matrix = mat, key = code_df))
}

