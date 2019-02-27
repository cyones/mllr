#' Select the experiment where the results would be saved
#'
#' This is the main function of the miRNAss package and implements the miRNA
#' @param x Number of threads used for the calculations. If it is NA
#' @return Returns a vector with the same size of the input vector y with the
#' @examples
#' # First construct the label vector with the CLASS column
#' a = 1+1
#'
#' @export
toc = function(name=NULL) {
	name = ifelse(!is.null(name), paste0(name, "_elapsedTime"), "elapsedTime")
	assign(name, Sys.time() - get(name, envir = .MLLRenv), envir = .MLLRenv)
}

