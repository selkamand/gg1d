# Functions ---------------------------------------------------------------

all_files_exist <- function(x){
  all(file.exists(x))
}


is_dir <- function(x){
  all(dir.exists(x))
}

is_file <- function(x){
  all(file.exists(x) & !dir.exists(x))
}

file_or_dir <- function(x){
  if(is_file(x)) return ('File')
  else if(is_dir(x)) return('Directory')
  else return('Unknown filetype')
}

get_file_extensions <- function(filenames) {
  filenames <- basename(filenames)

  vapply(filenames, function(x) {
    extension <- sub("^.*\\.", "", x)
    if (extension == x) extension <- ""
    extension
  }, character(1))
}

#' Has Extension
#'
#' @param x object to test
#' @param extensions valid extensions (character vector). Do not include the '.', e.g. supply `extensions = 'txt'` not `extensions = '.txt'`
#' @param compression should compression extension ‘.gz’, ‘.bz2’ or ‘.xz’ be removed first?
#'
#' @return TRUE if all x have valid extensions as supplied by `extensions` (flag)
#'
has_extension <- function(x, extensions, compression = FALSE){
  if(compression){
    x = sub(x = x,"\\.(gz|bz2|xz)$","")
  }

  observed_ext <- get_file_extensions(x)
  all(observed_ext %in% extensions)
}

# Which of the filenames are missing the required extension?
files_missing_extension <- function(x, extensions, compression = FALSE){
  original = x
  if(compression){
  x = sub(x = x,"\\.(gz|bz2|xz)$","")
  }

  observed_ext <- get_file_extensions(x)
  original[!observed_ext %in% extensions]
}

# File Assertions ---------------------------------------------------------------

#' Assert that all files exist
#'
#' Assert all files in vector exist. To assert a single file exists, see [assert_file_exists()]
#'
#' @include assert_create.R
#' @include is_functions.R
#' @param x Paths to files (character)
#' @param msg A character string containing the error message if any files in `x` is does not exist
#' @inheritParams common_roxygen_params
#' @inheritParams assert_character_vector
#' @return invisible(TRUE) if all files in `x` exist, otherwise aborts with the error message specified by `msg`
#'
#' @examples
#' real_file <- system.file("DESCRIPTION", package = "assertions")
#'
#' try({
#' assert_all_files_exist(c(real_file, real_file))
#' assert_all_files_exist(c("foo", "bar")) # Throws Error
#' })
#'
#' @concept assert_file
#' @export
assert_all_files_exist <- assert_create_chain(
  assert_character_vector_or_glue,
  assert_create(func = all_files_exist, default_error_msg = "Failed to find file{?s}: {.file {arg_value[!file.exists(arg_value)]}}"),
  assert_create(func = is_file, default_error_msg = "{x[dir.exists(x)]} {?is a/are} {.strong director{?y/ies}}, not {?a/} {.strong file{?s}}")
)


#' Assert a file exists
#'
#' Assert that a file exists.
#' To assert all files in a vector exist, see [assert_all_files_exist()]
#'
#' @include assert_create.R
#' @include is_functions.R
#' @param x Path to a file (string)
#' @param msg A character string containing the error message if file `x` is does not exist
#' @inheritParams common_roxygen_params
#' @inheritParams assert_all_files_exist
#' @return invisible(TRUE) if file `x` exists, otherwise aborts with the error message specified by `msg`
#'
#' @examples
#' real_file <- system.file("DESCRIPTION", package = "assertions")
#'
#' try({
#' assert_file_exists(real_file) # PASSES
#' assert_file_exists("foo") # Throws Error
#' assert_file_exists(c(real_file, real_file)) # Throws Error (should use assert_all_files_exist)
#'
#' })
#'
#' @concept assert_file
#' @export
assert_file_exists <- assert_create_chain(
  assert_string,
  assert_all_files_exist
)


#' Assert a file does not exist
#'
#' Assert that a file does not exist. Useful for avoiding overwriting.
#'
#' @param x Path to a file (string)
#' @param msg A character string containing the error message if file `x` already exists
#' @inheritParams common_roxygen_params
#' @return invisible(TRUE) if file `x` does not exist, otherwise aborts with the error message specified by `msg`
#'
#' @examples
#' real_file <- system.file("DESCRIPTION", package = "assertions")
#'
#' try({
#' assert_file_does_not_exist("foo") # Passes
#' assert_file_does_not_exist(real_file) # Throws error
#' assert_file_does_not_exist(c("foo", "bar")) # Throws Error (single file only)
#' })
#'
#' @concept assert_file
#' @export
assert_file_does_not_exist <- assert_create_chain(
  assert_string,
  assert_create(
    func = function(x){!file.exists(x)},
    default_error_msg = "{.strong {file_or_dir(arg_value)} ({.path {arg_value}})} already exists"
    )
)

#' Assert a directory does not exist
#'
#' Assert that a directory does not already exist. Useful for avoiding overwriting.
#' This function is an exact copy of [assert_file_does_not_exist()] and included to make assertion code more readable.
#'
#' @param x Path to a file (string)
#' @param msg A character string containing the error message if file `x` already exists
#' @inheritParams common_roxygen_params
#' @return invisible(TRUE) if directory `x` does not already exist, otherwise aborts with the error message specified by `msg`
#'
#' @examples
#' real_dir <- system.file("tests", package = "assertions")
#'
#' try({
#' assert_directory_does_not_exist("foo") # Passes
#' assert_directory_does_not_exist(real_dir) # Throws error
#' assert_directory_does_not_exist(c("foo", "bar")) # Throws Error (single file only)
#' })
#'
#' @concept assert_file
#' @export
assert_directory_does_not_exist <- assert_file_does_not_exist

#' Assert all files are directories
#'
#' Assert that all paths supplied exist and are directories.
#' To assert a single directory exists, see [assert_directory_exists()]
#'
#'
#' @include assert_create.R
#' @include is_functions.R
#' @param x Paths to directories (character)
#' @param msg A character string containing the error message if file `x` is does not exist
#' @inheritParams common_roxygen_params
#' @inheritParams assert_character
#'
#' @return invisible(TRUE) if `x` is exists and is a directory, otherwise aborts with the error message specified by `msg`
#'
#' @examples
#' try({
#' assert_directory(system.file("package = assertions")) # PASSES
#' assert_directory("foo") # Throws Error
#' })
#'
#' @concept assert_file
#' @export
assert_all_directories_exist <- assert_create_chain(
  assert_character_vector_or_glue,
  assert_create(func = all_files_exist, default_error_msg = "Failed to find director{?y/ies}: {.file {arg_value[!file.exists(arg_value)]}}"),
  assert_create(is_dir, default_error_msg = "{.strong {arg_value[!dir.exists(arg_value)]}} {?is a/are} {.strong file{?s}}, not{? a/} {.strong director{?y/ies}}")
)

#' Assert are directory exists
#'
#' Assert a directory exists.
#' To assert all directories in a vector exist, see [assert_all_directories_exist()]
#'
#' @include assert_create.R
#' @include is_functions.R
#' @param x Path to a directory (string)
#' @param msg A character string containing the error message if file `x` is does not exist
#' @inheritParams common_roxygen_params
#' @inheritParams assert_all_directories_exist
#' @return invisible(TRUE) if `x` is exists and is a directory, otherwise aborts with the error message specified by `msg`
#'
#' @examples
#' try({
#' assert_directory_exists(system.file("package = assertions")) # PASS
#' assert_all_directories_exist("foo") # Throws Error
#' })
#'
#' @concept assert_file
#' @export
assert_directory_exists <- assert_create_chain(
  assert_string,
  assert_all_directories_exist
)

#' Assert file extensions
#'
#' Assert that all filepaths supplied have one of the selected extensions. Does not require file to actually exist.
#'
#'
#' @include assert_create.R
#' @include is_functions.R
#'
#' @param x An object
#' @param msg A character string containing the error message if file `x` does not have the specified extensions
#' @inheritParams common_roxygen_params
#' @inheritParams has_extension
#' @inheritParams assert_character
#' @return invisible(TRUE) if `x` has any of the specified extensions, otherwise aborts with the error message specified by `msg`
#'
#' @examples
#' try({
#' assert_all_files_have_extension(c("foo.txt", "bar.txt"), extensions = "txt") # Passes
#' assert_all_files_have_extension(c("foo.txt", "bar.csv"), extensions = "csv") # Throws Error
#' })
#'
#' @concept assert_file
#'
#' @export
assert_all_files_have_extension <- assert_create_chain(
  assert_character_vector_or_glue,
  assert_create(has_extension, "'{.strong {arg_name}}' {cli::qty(files_missing_extension(arg_value, extensions, compression))}{?has/have} an invalid extension (required extension/s: {.strong {extensions}}).
  The following {cli::qty(files_missing_extension(arg_value, extensions, compression))} file{?s} ha{?s an/ve} unexpected extension{?s}:
  [{files_missing_extension(arg_value, extensions, compression)}]"
  )
)

#' Assert file extensions
#'
#' Assert that a filepath includes one of the selected extensions. Does not require file to actually exist.
#'
#'
#' @include assert_create.R
#' @include is_functions.R
#'
#' @param x An object
#' @param msg A character string containing the error message if file `x` does not have the specified extensions
#' @inheritParams common_roxygen_params
#' @inheritParams has_extension
#' @inheritParams assert_all_files_have_extension
#' @return invisible(TRUE) if `x` has any of the specified extensions, otherwise aborts with the error message specified by `msg`
#'
#' @examples
#' try({
#' assert_file_has_extension("foo.txt", extensions = "txt") # Passes
#' assert_file_has_extension("file.txt", extensions = "csv") # Throws Error
#' })
#'
#' @concept assert_file
#'
#' @export
assert_file_has_extension <- assert_create_chain(
  assert_string,
  assert_all_files_have_extension
)
