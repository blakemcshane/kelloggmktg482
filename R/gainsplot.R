#' Plotting Gains Curves
#'
#' This function plots the gains curve and returns the area under the gains curve (AUGC) for one or more scores.
#' @param outcome.var, a binary outcome variable to be assessed by the scores
#' @param ..., one or more scores to assess the outcome variable
#' @export
#' @examples
#' gainsplot(-lean$rec, lean$freq, lean$mon, outcome.var=lean$offer)
gainsplot <- function (outcome.var, ...){
    arglist <- list(...)
    for (i in 1:length(arglist)) {
        if (class(arglist[[i]]) == "matrix") {
            if (min(dim(arglist[[i]])) > 1) {
                stop("One of the scores is matrix with more than one column")
            }
        }
        arglist[[i]] <- as.vector(arglist[[i]])
    }
    scores <- as.data.frame(do.call(cbind, arglist))
    for (i in 3:length(match.call())) {
        names(scores)[i - 2] <- deparse(match.call()[[i]])
    }
    p.hat <- as.numeric(table(outcome.var)/length(outcome.var))[2]
    o.hat <- p.hat / (1-p.hat)
    gains.data.build <- NULL
    augc.build <- NULL
    for (i in seq_along(scores)) {
        pred <- ROCR::prediction(scores[[i]], factor(outcome.var))
        gains <- ROCR::performance(pred, "tpr", "rpp")
        auroc <- ROCR::performance(pred, measure = "auc")@y.values[[1]]
        gains.data <- data.frame(
			Score = colnames(scores)[i], 
            Prop.buyers = as.numeric(unlist(gains@y.values)), 
            Prop.customers = as.numeric(unlist(gains@x.values)))
        augc <- (auroc + o.hat/2)/(1 + o.hat)
        tmp <- bind_cols(score=colnames(scores)[i], augc=round(augc,3))
        augc.build <- bind_rows(augc.build, tmp)
        gains.data.build <- bind_rows(gains.data.build, gains.data)
    }
    gains.data.build$Score <- factor(gains.data.build$Score, levels=colnames(scores))
	gg <- ggplot(gains.data.build, aes(x=Prop.customers, y=Prop.buyers, col=Score)) +
			annotate(geom="segment", x=0, y=0, xend=1, yend=1, linetype=2, linewidth=0.25) +
			geom_line() + 
			labs(x="Proportion of Customers", y="Proportion of Buyers", col=NULL)
	print(gg)
    augc.build <- data.frame(augc.build)
	colnames(augc.build) <- c("Score", "AUGC")
    print(augc.build, row.names=FALSE)
}