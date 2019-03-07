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
log.results = function(gtruth, pred, mask=NULL, problem.type=NULL, ...) {
	if(!is.null(mask)) {
		gtruth = gtruth[mask]
		pred = pred[mask]
	}
	stopifnot(class(gtruth) == class(pred))
	stopifnot(length(gtruth) == length(pred))
	if(is.null(problem.type))
		problem.type = guess.problem.type(gtruth)

	cexp = get("current.experiment", envir = .MLLRenv)
	rpath = get("results.path", envir = .MLLRenv)
	load(paste0(rpath, cexp, ".rda"))

	nmea = nrow(results)+1
	lst = list(...)
	for(i in 1:length(lst))
		results[nmea, names(lst)[i]] = lst[[i]]

	if(problem.type=="binary") {
		Acc = mean(pred == gtruth)
		SE = mean(pred[gtruth])
		SP = mean(!pred[!gtruth])
		Pr = mean(gtruth[pred])
		results[nmea, "Acc"] = Acc
		results[nmea, "SE"] = SE
		results[nmea, "SP"] = SP
		results[nmea, "Pr"] = Pr
		results[nmea, "Gm"] = sqrt(SE*SP)
		results[nmea, "G"]  = sqrt(SE*Pr)
		results[nmea, "F1"] = (2 * SE * Pr) / (SE * Pr)
	}
	if(problem.type=="multiclass") {
		stopifnot(!all(sort(levels(pred)) == sort(levels(gtruth))))
		results[nmea, "Acc"] = mean(pred == gtruth)
		for(lv in levels(pred)) {
			cn = paste0("SE_", lv)
			results[nmea, cn] = mean(pred[gtruth==lv]==lv)
		}
	}
	if(problem.type=="regresion") {
		results[nmea, "RMSE"] = sqrt(mean((pred - gtruth)^2))
		results[nmea, "MAE"] = mean(abs(pred - gtruth))
	}
	for(tm in grep("elapsedTime", ls(envir = .MLLRenv), value=T))
		results[nmea, tm] = get(tm, envir = .MLLRenv)

	if(dir.exists(".git")) {
		results[nmea, "git_hash"] = system("git rev-parse HEAD", intern=T)
		results[nmea, "git_message"] = system("git log -1 --pretty=%B", intern=T)[1]
	}
	save(results, file=paste0(rpath, cexp, ".rda"))
}

aucroc <- function(labels, scores) {
  score_order <- order(scores, decreasing=TRUE)
  labels <- as.logical(labels[score_order])
  scores <- scores[score_order]
  pos_scores <- scores[labels]
  neg_scores <- scores[!labels]
  n_pos <- sum(labels)
  n_neg <- sum(!labels)
  M <- outer(sum(labels):1, 1:sum(!labels),
             function(i, j) (1 + sign(pos_scores[i] - neg_scores[j]))/2)
  AUC <- mean (M)
  return(AUC)
}


guess.problem.type <- function(gtruth) {
	if(class(gtruth)=="logical")
		return("binary")
	if(class(gtruth)=="factor" && length(levels(gtruth)) > 2)
		return("multiclass")
	if(class(gtruth)=="factor" && length(levels(gtruth)) == 2)
		return("binary")
	if(class(gtruth)=="character" && length(unique(gtruth)) > 2)
		return("multiclass")
	if(class(gtruth)=="character" && length(unique(gtruth)) == 2)
		return("binary")
	if(class(gtruth)=="numeric" && length(unique(gtruth)) > 2 &&
	   length(unique(gtruth))/length(gtruth) < 0.01) {
		warning("The class problem is set as multiclass,
			since there are few unique values for the labels")
		return("multiclass")
	}
	if(class(gtruth)=="numeric" && length(unique(gtruth)) == 2) {
		warning("The class problem is set as binary,
			since there are two unique values for the labels")
		return("binary")
	}
	if(class(gtruth) == "numeric")
		return("regresion")
	stop("Failed to guess type of problem")
}
