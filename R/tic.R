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
tic = function(name="") {
	name = ifelse(nchar(name)>0, paste0(name, "_elapsedTime"), "elapsedTime")
	assign(name, Sys.time(), envir = .MLLRenv)
}

