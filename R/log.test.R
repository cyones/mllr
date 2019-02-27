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
log.test = function(gtruth, pred, mask=NULL, ...) {
	if(!is.null(mask)) {
		gtruth = gtruth[mask]
		pred = pred[mask]
	}
	stopifnot(class(gtruth) == class(pred))
	stopifnot(length(gtruth) == length(pred))

	cexp = get("current.experiment", envir = .MLLRenv)
	rpath = get("results.path", envir = .MLLRenv)
	load(paste0(rpath, cexp))

	nmea = nrow(results)+1
	lst = list(...)
	for(i in 1:length(lst))
		results[nmea, names(pr)[i]] = pr[[i]]

	if(is.logical(pred)) {
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
	if(is.factor(pred)) {
		stopifnot(!all(sort(levels(pred)) == sort(levels(gtruth))))
		results[nmea, "Acc"] = mean(pred == gtruth)
		for(lv in levels(pred)) {
			cn = paste0("SE_", lv)
			results[nmea, cn] = mean(pred[gtruth==lv]==lv)
		}
	}
	if(is.numeric(pred)) {
		results[nmea, "RMSE"] = sqrt(mean((pred - gtruth)^2))
		results[nmea, "MAE"] = mean(abs(pred - gtruth))
	}
	for(tm in grep("elapsedTime", ls(envir = .MLLRenv), value=T))
		results[nmea, tm] = get(tm, envir = .MLLRenv)

	if(dir.exists(".git")) {
		results[nmea, "git_hash"] = system("git rev-parse HEAD", intern=T)
		results[nmea, "git_message"] = system("git log -1 --pretty=%B", intern=T)[1]
	}
	save(results, paste0(rpath, cexp))
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
