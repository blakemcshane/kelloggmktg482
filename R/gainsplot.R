#' A function to plot a gains curve
#'
#' This function allows you to compare model performace by comparing the gains curves of models.
#' @param label.var (required)
#' @param score1, score 2, ... (at least 1 required)
#' @keywords gains, auc
#' @export
#' @examples
#' gainsplot(logit1$fitted.values, rf$fitted.values, nn$fitted.values, label.var = bbb$buyer)

gainsplot <- function(label.var,...) {
  arglist <- list(...)
  
  for (i in 1:length(arglist)) {
    if (class(arglist[[i]]) == "matrix") {
      if(min(dim(arglist[[i]])) >1 ){
        stop("One of the predictions is matrix with more than one column")
      }
    }
    arglist[[i]] <- as.vector(arglist[[i]])
  }
  
  pred.vars <- as_tibble(do.call(cbind, arglist))
  
  for (i in 3:length(match.call())) {
    names(pred.vars)[i - 2] <- deparse(match.call()[[i]])
  }
  
  p.hat <- as.numeric(table(label.var)/length(label.var))[2]
  o.hat <- p.hat / (1 - p.hat)
  gains.data.build <- NULL
  augc.build <- NULL
  for (i in seq_along(pred.vars)) {
    pred.var <- pred.vars[[i]]
    pred <- ROCR::prediction(pred.var, factor(label.var))
    gain <- ROCR::performance(pred, "tpr", "rpp")
    gains.data <- tibble(Model = colnames(pred.vars)[i],
                         Percent.buyers=as.numeric(unlist(gain@y.values)),
                         Percent.customers=as.numeric(unlist(gain@x.values))) %>%
      mutate(Percent.buyers=Percent.buyers*100,
             Percent.customers=Percent.customers*100)
    auroc <- ROCR::performance(pred, measure = "auc")@y.values[[1]]
    augc <- (auroc + o.hat/2) / (1 + o.hat)
    tmp <- bind_cols(
                model = colnames(pred.vars)[i],
                augc =  round(augc, 3)
            )
    augc.build <- bind_rows(augc.build, tmp)
    gains.data.build <- bind_rows(gains.data.build, gains.data)
  }
  gains.data.build$Model <- factor(gains.data.build$Model, levels=colnames(pred.vars))
  no.model.data <- as_tibble(data.frame(Percent.buyers=c(0,100),
                                        Percent.customers=c(0,100)))
  print(ggplot() +
          geom_line(data=gains.data.build,
                    aes(Percent.customers,Percent.buyers,color = Model)) +
          geom_line(data=no.model.data,
                    aes(Percent.customers,Percent.buyers), linetype=3) +
          labs(x="Percent Customers",
               y="Percent Buyers"))
  augc.build <- as_tibble(data.frame(augc.build))
  return(augc.build)
}
