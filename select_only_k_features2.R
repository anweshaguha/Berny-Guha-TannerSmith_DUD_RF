select_only_k_features2 <- function (x, k = 10) 
{
 contribution_sum <- tapply(x$contribution, x$variable_name, 
                            function(contribution) sum(abs(contribution), na.rm = TRUE))
 contribution_ordered_vars <- names(sort(contribution_sum[!(names(contribution_sum) %in% 
                                                             c("", "intercept"))]))
 variables_keep <- tail(contribution_ordered_vars, k)
 variables_remove <- setdiff(contribution_ordered_vars, variables_keep)
 if (length(variables_remove) > 0) {
  x_remove <- x[x$variable_name %in% variables_remove, 
  ]
  x_keep <- x[!(x$variable_name %in% c(variables_remove, 
                                       "")), ]
  x_prediction <- x[x$variable == "Prediction", ]
  row.names(x_prediction) <- x_prediction$label
  remainings <- tapply(x_remove$contribution, x_remove$label, 
                       sum, na.rm = TRUE)
  x_keep$position <- as.numeric(as.factor(x_keep$position)) + 
   2
  for (i in 1:nrow(x_keep)) {
   if (x_keep[i, "variable_name"] == "intercept") {
    x_keep[i, "cumulative"] <- x_keep[i, "contribution"]
   }
   else {
    x_keep[i, "cumulative"] <- x_keep[i - 1, "cumulative"] + 
     x_keep[i, "contribution"]
   }
  }
  x_others <- data.frame(variable = "+ All Other Factors", 
                         contribution = remainings, variable_name = "+ All Other Factors", 
                         variable_value = "", cumulative = x_prediction[names(remainings), 
                                                                        "cumulative"], sign = sign(remainings), position = 2, 
                         label = names(remainings))
  x <- rbind(x_keep, x_others, x_prediction)
 }
 x
}