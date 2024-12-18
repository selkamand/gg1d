# These functions are for counting the number of tests per assertion
# and updating the json used to display the relevant badge

#' List assertion names
#'
#' List all assertion names
#'
#' @param exclude_create_and_chain exclude assert_create and assert_create_chain (flag)
#'
#' @return unique set of assertion names (character)
#'
assertion_names <- function(exclude_create_and_chain =TRUE){

  # Get path to scripts
  path_r_directory <- system.file("R", package = "assertions")
  path_scripts=dir(path_r_directory, full.names = TRUE, pattern = ".R$")

  # Get list of assertions
  ## Read lines
  char_lines <- unlist(lapply(path_scripts, function(path) {readLines(path)}))
  char_lines <- gsub(x=char_lines, pattern = " ", replacement = "")

  ## Find assertions
  assertions_names <-
    char_lines[
      grepl(x=char_lines, pattern = "^assert_[a-zA-Z0-9_]+") &
        grepl(x=char_lines, pattern = "(assert_create|assert_create_chain)") &
        !grepl(x=char_lines, pattern = "^#")
      ]

  assertions_names <- sub(x=assertions_names, pattern = "(<-|=|\\().*", replacement = "")
  assertions_names <- unique(assertions_names)
  assertions_names <- sort(assertions_names)

  if(exclude_create_and_chain){
    assertions_names <- assertions_names[!assertions_names %in% c('assert_create', 'assert_create_chain')]
  }

  assertions_names <- assertions_names[!assertions_names %in% c('assert_create_chain_example')]


  return(assertions_names)
}


#' Count tests per Assertion
#'
#' Count the number of unit-tests per assertion.
#' Note assertion_tests only finds tests where `expect_` and `assert_` are on the same line.
#'
#'
#' @return two column data.frame describing assertion name and number of tests (expect_statement)
#'
assertion_tests <- function(){
  #message("Warning: assertion_tests only finds tests where `expect_` and `assert_` are on the same line")
  path_r_directory <- testthat::test_path()
  path_scripts=dir(path_r_directory, full.names = TRUE, pattern = ".R$")


  ## Read all test lines
  char_lines <- unlist(lapply(path_scripts, function(path) {readLines(path)}))
  char_lines <- gsub(x=char_lines, pattern = " ", replacement = "")
  #char_lines <- gsub(x=char_lines, pattern = "^[ \t]?\n", replacement = "")
  #char_lines <- collapse_function_calls(char_lines)

  expect_lines = char_lines[
    grepl(x=char_lines, pattern = "^expect_[a-zA-Z0-9_]+") &
      grepl(x=char_lines, pattern = "assert_")
    ]

  tested_assertion = sub(x=expect_lines, ".*(assert_[a-zA-Z0-9_]+()).*", "\\1")


  df_assertion_test_counts <- as.data.frame(table(tested_assertion))
  names(df_assertion_test_counts) <- c("assertion", "tests")
  df_assertion_test_counts <- df_assertion_test_counts[df_assertion_test_counts[["assertion"]] %in% assertion_names(),]


  assertion_names <- assertion_names()
  assertions_missing_tests <- assertion_names[!assertion_names %in% df_assertion_test_counts[["assertion"]]]

  df_assertion_test_counts <- rbind(
    df_assertion_test_counts,
    data.frame(assertion = assertions_missing_tests, tests = rep(0, times = length(assertions_missing_tests)))
  )

  df_assertion_test_counts <- df_assertion_test_counts[order(df_assertion_test_counts[["tests"]], decreasing = TRUE),]

  return(df_assertion_test_counts)
}

#' Check assertions are tested enough
#'
#' @param min_required_tests min number of tests (expect statements) per assertion
#'
#' @return TRUE if all assertions sufficiently tested. Otherwise throws error
#'
check_all_assertions_are_tested_enough <- function(min_required_tests = 5){

  undertested_assertions <- assertion_tests()[assertion_tests()[["tests"]] < min_required_tests,][["assertion"]]

  if(length(undertested_assertions) == 0)
    return(TRUE)

  stop(paste0("The following assertions have too few unit tests (< ",min_required_tests,"): ", paste0(undertested_assertions, collapse = ", ")))
}
