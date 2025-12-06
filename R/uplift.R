#' Computing Qini Tables
#'
#' This function computes the Qini table used to evaluate the performance of an uplift model.
#' @param data: a data frame containing the treatment, the outcome and the predictors.
#' @param treat: name of a binary (numeric) vector representing the treatment assignment (coded as 0/1)
#' @param outcome: name of a binary response (numeric) vector (coded as 0/1)
#' @param score: a score to sort the observations from highest to lowest.
#' @param nb.group: (optional, default = 10)
#' @export
#' @examples
#' QiniTable(
#'  expdata_stacked.test,
#'  treat = "ad",
#'  outcome = "converted",
#'  score = "pred_lr_uplift",
#'  nb.group = 20
#')
QiniTable <- function(data, treat, outcome, score, nb.group = 10){
   # Computes the performance of an uplift estimator.
   # 11-10-2019: Adapted from QiniTable in the tools4uplift package
   # by Florian Zettelmeyer to ensure that the n-tiles are calculated
   # based on the treatment observations, not the stacked observations.
   #
   # Args:
   #   data: a data frame containing the treatment, the outcome and the predictors.
   #   treat: name of a binary (numeric) vector representing the treatment
   #          assignment (coded as 0/1).
   #   outcome: name of a binary response (numeric) vector (coded as 0/1).
   #   score: a score to sort the observations from highest to lowest
   #   ... and default parameters.
   #
   # Returns:
   #   The performance of an uplift estimator in a table.

   # Error handling
   if (nb.group < 2) {
      stop("The number of groups must be greater or equal to 2")
   }

   # First, we need to rank and sort the observations
   data <- data.frame(data)
   df <- data[data[[treat]] == 1, ]
   breaks <- c(-Inf, quantile(df[[score]], probs = seq(0, 1, by = 1/nb.group))[2:nb.group],Inf)
   data$group <- nb.group+1 - cut(data[[score]], breaks,
                                  labels = FALSE,include.lowest = TRUE)

   dataResults <- data.frame(matrix(rep(0), nb.group, 8))
   colnames(dataResults) <- c("Cum_Prop", "T_Y1", "T_n", "C_Y1",
                              "C_n", "Incremental_Y1", "Inc_Uplift", "Uplift")

   # Incremental observed uplift
   for(i in 1:nb.group){
      subset <- data[data$group <= i, ]
      dataResults[i,1] <- i/nb.group
      dataResults[i,2] <- sum(subset[[treat]] == 1 & subset[[outcome]] == 1)
      dataResults[i,3] <- sum(subset[[treat]] == 1)
      dataResults[i,4] <- sum(subset[[treat]] == 0 & subset[[outcome]] == 1)
      dataResults[i,5] <- sum(subset[[treat]] == 0)
      dataResults[i,6] <- dataResults[i, 2] - dataResults[i, 4]*dataResults[i, 3]/dataResults[i, 5]
   }
   dataResults[,7] <- dataResults[,6]/dataResults[nb.group,3]


   # Observed uplift in each group
   for (i in 1:nb.group){
      subset <- data[data$group == i, ]
      dataResults[i,8] <- sum(subset[[treat]] == 1 & subset[[outcome]] == 1) / sum(subset[[treat]] == 1) -
         sum(subset[[treat]] == 0 & subset[[outcome]] == 1) / sum(subset[[treat]] == 0)
   }

   return(dataResults)
}

#' Plotting Qini Bar Plots
#'
#' This function plots the Qini bar plot for one or more scores.
#' @param ..., one or more Qini tables
#' @param score.names = c("score1", "score2") (optional)
#' @export
#' @examples
#' QiniBarPlot(PerfTable_uplift, PerfTable_propensity, score.names = c("LR Uplift", "LR Propensity"))
QiniBarPlot <- function(..., score.names=NULL) {
   arglist <- list(...)

   # Assemble the dataframe
   combined_table=arglist[[1]][,c("Cum_Prop","Uplift")]
   if (length(arglist) > 1) {
      for (i in 2:length(arglist)) {
         temp2 = arglist[[i]][, c("Uplift")]
         combined_table = cbind(combined_table, temp2)
      }
   }

   # Adjust names and add score.names if they have been specified
   if (is.null(score.names) == TRUE) {
      if (length(arglist) > 1) {
         for (i in 2:length(match.call())) {
            names(combined_table)[i] <- deparse(match.call()[[i]])
         }
      }
   } else {
      names(combined_table) <- c("Cum_Prop", score.names)
   }

   # Plot uplift graph
   plot <- combined_table %>%
      gather(key="score", value="Uplift", -Cum_Prop) %>%
      ggplot() + geom_col(aes(x=Cum_Prop, y=Uplift, fill=score), position="dodge") +
	  labs(x="Proportion of Customers", y="Uplift", fill=NULL)
   print(plot)
}


#' Plotting Qini Curves
#'
#' This function plots the Qini curve for one or more scores.
#' @param ..., one or more Qini tables
#' @param score.names = c("score1", "score2") (optional)
#' @export
#' @examples
#' QiniCurve2(PerfTable_uplift, PerfTable_propensity, score.names = c("LR Uplift", "LR Propensity"))
QiniCurve <- function(..., score.names=NULL) {
   arglist <- list(...)

   notargeting_line <- data.frame(x=0, xend=1,
                                  y=0, yend=arglist[[1]][nrow(arglist[[1]]),7])

   # Assemble the dataframe
   combined_table=arglist[[1]][,c("Cum_Prop","Inc_Uplift")]
   if (length(arglist) > 1) {
      for (i in 2:length(arglist)) {
         temp2 = arglist[[i]][, c("Inc_Uplift")]
         combined_table = cbind(combined_table, temp2)
      }
   }

   # Adjust names and add score.names if they have been specified
   if (is.null(score.names) == TRUE) {
      if (length(arglist) > 1) {
         for (i in 2:length(match.call())) {
            names(combined_table)[i] <- deparse(match.call()[[i]])
         }
      }
   } else {
      names(combined_table) <- c("Cum_Prop", score.names)
   }

   # Plot uplift graph
   plot <- combined_table %>%
      rbind(c(0,0), .) %>%
      gather(key = "score", value = "Inc_Uplift", -Cum_Prop) %>%
      ggplot() + geom_line(aes(x=Cum_Prop, y=Inc_Uplift, col=score)) +
      geom_segment(data=notargeting_line,
		  		   aes(x=x, y=y, xend=xend, yend=yend), linetype=2, linewidth=0.25) +
	  labs(x="Proportion of Customers", y="Incremental Uplift", col=NULL)
   print(plot)
}
