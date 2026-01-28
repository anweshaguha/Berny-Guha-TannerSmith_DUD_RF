prepare_data_for_break_down_plot2 <- function (x, baseline, rounding_function, digits) 
{
 x$sign[x$variable_name == ""] <- "X"
 x$sign[x$variable == "intercept"] <- "X"
 x$prev <- x$cumulative - x$contribution
 broken_baseline <- x[x$variable_name == "intercept", ]
 x$text <- x$prev
 if (is.na(baseline)) {
  for (lab in broken_baseline$label) {
   x[x$label == lab & x$variable == "Prediction", "prev"] <- broken_baseline[broken_baseline$label == 
                                                                              lab, "contribution"]
   x[x$label == lab & x$variable == "intercept", "prev"] <- broken_baseline[broken_baseline$label == 
                                                                             lab, "contribution"]
  }
 }
 else {
  broken_baseline$contribution <- baseline
  x[x$variable == "Prediction", "prev"] <- baseline
  x[x$variable == "intercept", "prev"] <- baseline
 }
 x$trans_contribution <- x$cumulative - x$text
 x$right_side <- pmax(x$cumulative, x$cumulative - x$contribution)
 pretty_trans_contribution <- as.character(rounding_function(x$trans_contribution, 
                                                             digits))
 x$pretty_text <- paste0(ifelse((substr(pretty_trans_contribution, 
                                        1, 1) == "-") | (x$variable == "Prediction") | (x$variable == 
                                                                                         "intercept"), "", "+"), pretty_trans_contribution)
 list(x = x, broken_baseline = broken_baseline)
}