#' A function to plot a variable importance plot.
#'
#' Relies on the vip package vi function.
#' @param object: a model of class lm, glm, nnet, or ranger.
#' @param data: a dataframe on which to evaluate variable importance; typically a test dataframe.
#' @export
#' @examples
#' varimpplot(lr, target="buyer", data=bbb)

varimpplot <- function(object, target, data){
	if(!any(class(object) %in% c("lm", "glm", "nnet", "ranger"))){
		stop("This function works only for objects of type lm, glm, nnet, and ranger.")
	}
	if("lm" %in% class(object) & !("glm" %in% class(object))){
		predict.internal <- function(object, newdata){ 
			predict(object, newdata=newdata) }
	}
	if("glm" %in% class(object)){
		predict.internal <- function(object, newdata){ 
			predict(object, newdata=newdata, type="response") }
	}			
	if("nnet" %in% class(object)){
		predict.internal <- function(object, newdata){ 
			predict(object, newdata=newdata, type="raw")[,1] }
	}			
	if("ranger" %in% class(object)){
		predict.internal <- function(object, newdata){ 
			predict(object, data=newdata, type="response")[[1]][,2] }
	}

	if("lm" %in% class(object) & !("glm" %in% class(object))){
		vi0 <- vip::vi(object, target=target,
			method="permute", metric="rmse", pred_wrapper=predict.internal, train=data)
	}else{
		vi0 <- vip::vi(object, target=target, event_level="first",
      	  	method="permute", metric="roc_auc", pred_wrapper=predict.internal, train=data)
		vi0$Importance <- -vi0$Importance
		
    # Convert Change in AUROC to Change in AUGC
		p.hat <- as.numeric(table(data[,target])/nrow(data))[2]
		o.hat <- p.hat / (1 - p.hat)
    vi0$Importance <- vi0$Importance / (1 + o.hat)
				
		vi0 <- vi0[nrow(vi0):1,]
	}

	vi0 <- vi0 %>% filter(Importance != 0) %>% 
			mutate(Variable=factor(Variable,levels=rev(Variable)))	
			
	print(ggplot(vi0, aes(x=Variable,y=Importance)) + geom_col() + coord_flip())
}
