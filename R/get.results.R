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
get.result = function(name=NULL, results.path="./results/") {
	if(is.null(name))
		name = get("current.experiment", envir = .MLLRenv)
	load(paste0(results.path, name))
	return(results)
}

