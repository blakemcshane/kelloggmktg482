#' A function to plot a variable importance plot.
#'
#' Relies on the vip package vi function.
#' @param object: a model of class lm, glm, nnet, or ranger.
#' @param data: a dataframe on which to evaluate variable importance; optional for models of all classes except ranger; typically a test dataframe.
#' @export
#' @examples
#' varimpplot(lr, target="buyer")

varimpplot <- function(object, target, data=NULL){
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
		if(is.null(data)){  
			stop("This function requires a test dataset on which to evaluate variable importance for models of class ranger.")
		}
	}

	if("lm" %in% class(object) & !("glm" %in% class(object))){
		if(is.null(data)){
			vi0 <- vip::vi(object, target=target,
				method="permute", metric="rmse", pred_wrapper=predict.internal)
		}else{
			vi0 <- vip::vi(object, target=target,
				method="permute", metric="rmse", pred_wrapper=predict.internal, train=data)
		}
	}else{
		if(is.null(data)){
			vi0 <- vip::vi(object, target=target, reference_class=1,
	      	  	method="permute", metric="auc", pred_wrapper=predict.internal)
		}else{
			vi0 <- vip::vi(object, target=target, reference_class=1,
	      	  	method="permute", metric="auc", pred_wrapper=predict.internal, train=data)
		}		
	}

	vi0 <- vi0 %>% filter(Importance != 0) %>% 
			mutate(Variable=factor(Variable,levels=rev(Variable)))	
			
	print(ggplot(vi0, aes(x=Variable,y=Importance)) + geom_col() + coord_flip())
}