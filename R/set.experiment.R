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
set.experiment = function(name, overwrite=F, results.path="./results/") {
	dir.create(results.path, showWarnings = F)
	fname = paste0(results.path, name, ".rda")
	if(!file.exists(fname) || overwrite) {
		results = data.frame()
		save(results, file = fname)
	}
	assign("current.experiment", value=name, env=.MLLRenv)
	assign("results.path", value=results.path, env=.MLLRenv)
}

.MLLRenv = new.env(parent = emptyenv())
