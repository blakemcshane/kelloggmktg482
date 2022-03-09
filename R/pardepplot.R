#' A function to plot a partial dependence plot.
#'
#' Relies on the pdp package partial function.
#' @param object: a model of class lm, glm, nnet, or ranger.
#' @param pred.var: a character or a vector of two characters giving the predictor variables for which a partial dependence plot is sought; predictor variables must be of type numeric or factor.
#' @param data: a dataframe supplying the variables for plotting; typically the dataframe used to estimate the model.
#' @param ylim: optional vector of length two specifying the y-axis limits.
#' @param hline: optimal number specifying the location of a horizontal line.
#' @export
#' @examples
#' pardepplot(lr, pred.var="art", data=bbb)

pardepplot <- function(object, pred.var, data, ylim=NULL, hline=NULL){
	if(length(pred.var) > 2){ stop("This function supports no more than two pred.vars.") }
	pred.var.class <- sapply(pred.var, function(x){class(data[,x,drop=TRUE])})
	if("character" %in% pred.var.class){ stop("This function does not work with predictor variables of type character. Convert all categorical variables to type factor, rerun the model, and try again.") } 
	if(length(pred.var)==2 && pred.var.class[1]=="factor" && pred.var.class[2]!="factor"){
		pred.var <- pred.var[2:1]
		pred.var.class <- pred.var.class[2:1]
	}

	if(!any(class(object) %in% c("lm", "glm", "nnet", "ranger"))){
		stop("This function works only for objects of type lm, glm, nnet, and ranger.")
	}
	if("lm" %in% class(object) & !("glm" %in% class(object))){
		p0 <- pdp::partial(object, pred.var=pred.var, plot=FALSE, train=data) }
	if("glm" %in% class(object)){
		p0 <- pdp::partial(object, pred.var=pred.var, prob=TRUE, plot=FALSE, train=data) }
	if("nnet" %in% class(object)){
		p0 <- pdp::partial(object, pred.var=pred.var, prob=TRUE, plot=FALSE, train=data) }	
	if("ranger" %in% class(object)){
		p0 <- pdp::partial(object, pred.var=pred.var, prob=TRUE, plot=FALSE, train=data, 
					  which.class=colnames(object$predictions)[2]) }
	if("lm" %in% class(object) & !("glm" %in% class(object))){ ylab <- "yhat" }else{ ylab <- "phat"}
		
	if(length(pred.var)==1){
		colnames(p0)[1] <- "xx"
		p0 <- ggplot(p0, aes(x=xx,y=yhat)) + ylab(ylab) + xlab(pred.var)
		if(pred.var.class!="factor"){ p0 <- p0 + geom_line() }
		if(pred.var.class=="factor"){ p0 <- p0 + geom_point() }
	}
	
	if(length(pred.var)==2 & pred.var.class[1]!="factor" & pred.var.class[2]!="factor"){
		colnames(p0)[1:2] <- paste0("xx", 1:2)
		p0 <- ggplot(p0, aes(x=xx1,y=xx2,fill=yhat)) + geom_tile() +
			xlab(pred.var[1]) + ylab(pred.var[2]) + labs(fill=ylab)
	}

	if(length(pred.var)==2 & pred.var.class[1]!="factor" & pred.var.class[2]=="factor"){
		colnames(p0)[1:2] <- paste0("xx", 1:2)
		p0 <- ggplot(p0, aes(x=xx1,y=yhat,col=xx2)) + geom_line() +
			xlab(pred.var[1]) + ylab(ylab) + labs(col=pred.var[2])
	}

	if(length(pred.var)==2 & pred.var.class[1]=="factor" & pred.var.class[2]!="factor"){
		stop("This should never trigger.")
	}

	if(length(pred.var)==2 & pred.var.class[1]=="factor" & pred.var.class[2]=="factor"){
		colnames(p0)[1:2] <- paste0("xx", 1:2)
		p0 <- ggplot(p0, aes(x=xx1,y=yhat,col=xx2)) + 
			geom_point(position=position_dodge(width=0.2)) +
			xlab(pred.var[1]) + ylab(ylab) + labs(col=pred.var[2])
	}

	if(length(pred.var)==2 & pred.var.class[1]!="factor" & pred.var.class[2]!="factor"){
		print(p0)
	}else{
		if(!is.null(ylim)){ p0 <- p0 + ylim(ylim) }
		if(!is.null(hline)){ p0 <- p0 + geom_hline(yintercept=hline,lty=2,size=0.25) }
		print(p0)
	}
}