preprocessInput <- function(input){

  # tolower
  input <- tolower(input)

  # transforms various ASCII codes to appropriate language
  input <- gsub(x = input,
                  pattern = "\\&",
                  replacement = " and ")

  # transforms single letter "u" to "you"
  input <- gsub(x = input,
                    pattern = " u ",
                    replacement = " you ")

  # transforms 're to are
  input <- gsub(x = input,
                    pattern = "'re",
                    replacement = " are ")

  # transforms 'll to will
  input <- gsub(x = input,
                    pattern = "'ll",
                    replacement = " will ")

  # transforms 've to have
  input <- gsub(x = input,
                    pattern = "'ve",
                    replacement = " have ")

  # transforms 'm to am
  input <- gsub(x = input,
                    pattern = "'m",
                    replacement = " am")

  # remove ponctuation
  input <- gsub(x = input,
                pattern = "[[:punct:]]+",
                replacement = "")

  # removes remaining apostrophes
  input <- gsub(x = input,
                    pattern = "'",
                    replacement = "")

  # removes numbers
  input <- gsub(x = input,
                pattern = "[0-9]+",
                replacement = "")


  # removes and errant <> brackets remaining
  # myCorpus[[j]][[1]] <- gsub("<>", " ", myCorpus[[j]][[1]])
  input <- gsub(x = input,
                pattern = "[<>]+",
                replacement = " ")

  # whitespace
  input <- gsub(x = input,
                pattern = "\\s+",
                replacement = " ")
  input <- stringr::str_trim(string = input, side = "both")

  return(input)
}



