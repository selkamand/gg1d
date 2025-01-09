#' Compute Mutual Information
#'
#' Computes mutual information between each feature in the `features` data frame and the `target` vector.
#' The features are discretized using the "equalfreq" method from [infotheo::discretize()].
#'
#' @param features A data frame of features. These will be discretized using the "equalfreq" method
#' (see [infotheo::discretize()]).
#' @param target A vector (character or factor) representing the variable to compute mutual information with.
#' @param return_colnames Logical; if `TRUE`, returns the column names from `features` ordered by their
#' mutual information with `target` (highest to lowest). If `FALSE`, returns mutual information values. (default: `FALSE`)
#'
#' @return
#' If `return_colnames = FALSE`, a named numeric vector of mutual information scores is returned (one for each column in `features`), sorted in descending order.
#' The names of the vector correspond to the column names of `features`.
#' If `return_colnames = TRUE`, only the ordered column names of `features` are returned.
#'
#' @examples
#' data(iris)
#' # Compute mutual information scores
#' mutinfo(iris[1:4], iris[[5]])
#'
#' # Get column names ordered by mutual information with target column (most mutual info first)
#' mutinfo(iris[1:4], iris[[5]], return_colnames = TRUE)
#'
#' @export
mutinfo <- function(features, target, return_colnames = FALSE){
  rlang::check_installed("infotheo", reason = "to order axes based on mutual information")

  # Assertions & conversions
  assertions::assert_dataframe(features)
  if(is.factor(target)) { target <- as.character(target) }
  assertions::assert_vector(target)

  # Discretize as required for mutual information computations
  discretized <- infotheo::discretize(features, disc = "equalfreq")

  # Compute mutual information between each column in data and the target vector
  mutual_info <- vapply(discretized, FUN = function(feature){
    infotheo::mutinformation(X = feature, Y = target, method = "emp")
  }, FUN.VALUE = numeric(1))

  # mutual info should be a named vector of mutual informations between
  # each feature in the features data.frame and the target vector
  mutual_info <- sort(mutual_info, decreasing = TRUE)

  if(return_colnames){
    return(names(mutual_info))
  }

  return(mutual_info)
}

similarity_matrix_mutinfo <- function(data, normalize = TRUE){
  mx <- infotheo::mutinformation(infotheo::discretize(data), method = "emp")

  if(normalize){
    mx <- uniminmax(mx)
  }

  return(mx)
}

dist_matrix_mutinfo <- function(data){
  1-similarity_matrix_mutinfo(data, normalize = TRUE)
}

# Mutinfo based distance from Joe 1989 https://www.jstor.org/stable/2289859?seq=3
dist_matrix_mutinfo2 <- function(data){
  mx = similarity_matrix_mutinfo(data, normalize = FALSE)
  1 - (1 - exp( -2 *mx )) ^.5
}

