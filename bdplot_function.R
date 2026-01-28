bdplot <- function (x, ..., baseline = NA, max_features = 10, min_max = NA, 
                    vcolors = DALEX::colors_breakdown_drwhy(), digits = 2, rounding_function = round, 
                    add_contributions = TRUE, shift_contributions = 0.025, plot_distributions = FALSE, 
                    vnames = NULL, title = "Break Down profile", subtitle = "", 
                    max_vars = NULL) 
{
 position <- cumulative <- prev <- pretty_text <- right_side <- contribution <- NULL
 colnames(x) <- gsub(colnames(x), pattern = "cummulative", 
                     replacement = "cumulative")
 if (!is.null(max_vars)) {
  max_features <- max_vars
 }
 if (plot_distributions) {
  vorder <- c(as.character(x$variable)[order(x$position)], 
              "all data")
  df <- attr(x, "yhats_distribution")
  if (is.null(df)) 
   stop("You need to use keep_distributions=TRUE in the break_down() ")
  pl <- plot_break_down_distributions(df, vorder)
 }
 else {
  x <- select_only_k_features2(x, max_features)
  tmp <- prepare_data_for_break_down_plot2(x, baseline, 
                                          rounding_function, digits)
  broken_baseline <- tmp$broken_baseline
  x <- tmp$x
  if (any(x[x$variable == "Prediction", "right_side"] < 
          broken_baseline$contribution)) {
   x[x$variable == "Prediction", "right_side"] <- pmax(x[x$variable == 
                                                          "Prediction", "right_side"], broken_baseline$contribution)
  }
  if (any(x[x$variable == "intercept", "right_side"] < 
          broken_baseline$contribution)) {
   x[x$variable == "intercept", "right_side"] <- pmax(x[x$variable == 
                                                         "intercept", "right_side"], broken_baseline$contribution)
  }
  pl <- ggplot(x, aes(x = position + 0.5, y = pmax(cumulative, 
                                                   prev), xmin = position + 0.15, xmax = position + 
                       0.85, ymin = cumulative, ymax = prev, fill = sign, 
                      label = pretty_text))
  pl <- pl + geom_errorbarh(data = x[x$variable_name != 
                                      "", ], aes(xmax = position - 0.85, xmin = position + 
                                                  0.85, y = cumulative), height = 0, color = "black") + 
   geom_rect(alpha = 0.9, color = "black") + geom_hline(data = broken_baseline, 
                                                        aes(yintercept = contribution), lty = 3, alpha = 0.5, 
                                                        color = "black") + facet_wrap(~label, scales = "free_y", 
                                                                                      ncol = 1)
  if (add_contributions) {
   drange <- diff(range(x$cumulative))
   pl <- pl + geom_text(aes(y = right_side), vjust = 0.5, 
                        nudge_y = drange * shift_contributions, hjust = 0, 
                        color = "black")
  }
  if (any(is.na(min_max))) {
   x_limits <- scale_y_continuous(expand = c(0.05, 
                                             0.05), name = "")
  }
  else {
   x_limits <- scale_y_continuous(expand = c(0.05, 
                                             0.05), name = "", limits = min_max)
  }
  if (is.null(vnames)) 
   vnames <- x$variable
  pl <- pl + x_limits + scale_x_continuous(labels = vnames, 
                                           breaks = x$position + 0.5, name = "") + scale_fill_manual(values = vcolors)
 }
 pl + coord_flip() +  theme_bw() +
  theme(
   legend.position = "none",
   strip.text.x = element_text(margin = margin(.075,0,0.075,0, "cm")),
   panel.grid.minor = element_blank(),
   text = element_text(size = 12),
   strip.text = element_text(size = 13),
   axis.text = element_text(size = 12)
  )
}