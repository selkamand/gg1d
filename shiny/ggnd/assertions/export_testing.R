exported_functions <- function(){
  exports <- readLines(system.file("NAMESPACE", package = "assertions"))
  exports <- exports[exports != ""]
  exports <- exports[!startsWith(exports, prefix = "#")]
  exports <- sub(x=exports, pattern = "export\\((.*)\\)", replacement = "\\1")
  return(exports)
}

all_exports_are_assertions <- function(exceptions = "assert"){

  exports <- exported_functions()
  non_assertion_exports <- exports[!grepl(pattern = "^assert_", x = exports)]

  non_assertion_exports <- non_assertion_exports[!non_assertion_exports %in% exceptions]

  if(length(non_assertion_exports) == 0) return(TRUE)
  else{
    stop('Exporting functions that are NOT assertions: \n', paste0('   > ' , non_assertion_exports, '\n'))
  }
}
