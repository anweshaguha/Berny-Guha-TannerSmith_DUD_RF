rf_la_untest <- as.data.frame(rf_la_un)

rownames(rf_la_untest) <- NULL

rf_la_untest <- rf_la_untest %>% 
 filter(B > 0) %>% 
 select(variable_name, contribution) %>% 
 group_by(variable_name) %>% 
 summarise(mean = mean(contribution), median = median(contribution),
           min = min(contribution), max = max(contribution),
           q1 = quantile(contribution, 0.25), q3 = quantile(contribution, 0.75)) %>% 
 arrange(desc(abs(mean)))

breakdown_hc_contributions <- as.data.frame(breakdown_hc)

rownames(breakdown_hc_contributions) <- NULL

breakdown_hc_contributions <- breakdown_hc_contributions %>% 
 filter(variable_name != "intercept" & variable_name != "") %>% 
 select(variable_name, contribution, variable) %>% 
 mutate(sign = ifelse(contribution > 0, "Positive", "Negative"),
        label = "Simulated Case #1")

hc_unc_joined <- left_join(rf_la_untest, breakdown_hc_contributions) %>% 
 arrange(desc(abs(contribution))) %>% 
 slice_head(n = 8)

# hc_unc_joined %>% 
#  ggplot(aes(x = reorder(variable, abs(mean)), y = mean, fill = sign)) + geom_col(color = "black") +
#  geom_linerange(data = hc_unc_joined, aes(ymin = min, ymax = max), linewidth = 1, color = "#371ea3") +
#  geom_linerange(data = hc_unc_joined, aes(ymin = q1, ymax = q3), linewidth = 4, color = "#371ea3") +
#  geom_point(data = hc_unc_joined, aes(y = contribution), fill = "orange", color = "black",
#             size = 3, shape = 23, stroke = 1, alpha = .85) +
#  ylim(-.075,.075) +
#  scale_fill_manual(values = c("#f05a71", "#8bdcbe")) +
#  coord_flip() +  
#  theme_bw() +
#  theme(legend.position = "none") +
#  labs(y = "Contribution to Predicted Probability", x = "")


rf_la_lctest <- as.data.frame(rf_la_lc)

rownames(rf_la_lctest) <- NULL

rf_la_lctest <- rf_la_lctest %>% 
 filter(B > 0) %>% 
 select(variable_name, contribution) %>% 
 group_by(variable_name) %>% 
 summarise(mean = mean(contribution), median = median(contribution),
           min = min(contribution), max = max(contribution),
           q1 = quantile(contribution, 0.25), q3 = quantile(contribution, 0.75)) %>% 
 arrange(desc(abs(mean)))

breakdown_lc_contributions <- as.data.frame(breakdown_lc)

rownames(breakdown_lc_contributions) <- NULL

breakdown_lc_contributions <- breakdown_lc_contributions %>% 
 filter(variable_name != "intercept" & variable_name != "") %>% 
 select(variable_name, contribution, variable) %>% 
 mutate(sign = ifelse(contribution > 0, "Positive", "Negative"),
        label = "Simulated Case #2")

lc_unc_joined <- left_join(rf_la_lctest, breakdown_lc_contributions) %>% 
 arrange(desc(abs(contribution))) %>% 
 slice_head(n = 8)

# lc_unc_joined %>% 
#  ggplot(aes(x = reorder(variable, abs(mean)), y = mean, fill = sign)) + geom_col(color = "black") +
#  geom_linerange(data = lc_unc_joined, aes(ymin = min, ymax = max), linewidth = 1, color = "#371ea3") +
#  geom_linerange(data = lc_unc_joined, aes(ymin = q1, ymax = q3), linewidth = 4, color = "#371ea3") +
#  geom_point(data = lc_unc_joined, aes(y = contribution), fill = "orange", color = "black",
#             size = 3, shape = 23, stroke = 1, alpha = .85) +
#  ylim(-.08,.08) +
#  scale_fill_manual(values = c("#f05a71", "#8bdcbe")) +
#  coord_flip() +  
#  theme_bw() +
#  theme(legend.position = "none") +
#  labs(y = "Contribution to Predicted Probability", x = "")

binded_unc <- rbind(hc_unc_joined, lc_unc_joined)

unc_bdplot <- binded_unc %>%
 ggplot(aes(x = reorder(variable, abs(mean)), y = mean, fill = sign)) + geom_col(color = "black") +
 geom_hline(yintercept = 0, color = "black", linewidth = 1) +
 geom_linerange(data = binded_unc, aes(ymin = min, ymax = max), linewidth = 1, color = "#371ea3") +
 geom_linerange(data = binded_unc, aes(ymin = q1, ymax = q3), linewidth = 4, color = "#371ea3") +
 geom_point(data = binded_unc, aes(y = contribution), fill = "goldenrod1", color = "black",
            size = 2.5, shape = 23, stroke = 1.25, alpha = .85) +
 facet_col(~label, scales = "free") +
 ylim(-.08,.08) +
 scale_fill_manual(values = c("#f05a71", "#8bdcbe")) +
 coord_flip() +
 theme_bw() +
 theme(legend.position = "none", 
       strip.text.x = element_text(margin = margin(.075,0,0.075,0, "cm")),
       text = element_text(size = 10),
       strip.text = element_text(size = 11),
       axis.text = element_text(size = 10)) +
 labs(y = "Contribution to Predicted Probability", x = "")

# binded_unc$legend_point <- "Contribution Point"
# binded_unc %>%
#  ggplot(aes(x = reorder(variable, abs(mean)), y = mean, fill = sign)) +
#  geom_col(color = "black", show.legend = FALSE) +  # Hide legend for geom_col
#  geom_hline(yintercept = 0, color = "black", linewidth = 1) +
#  geom_linerange(aes(ymin = min, ymax = max), linewidth = 1, color = "#371ea3") +
#  geom_linerange(aes(ymin = q1, ymax = q3), linewidth = 4, color = "#371ea3") +
#  geom_point(aes(y = contribution, shape = legend_point, color = legend_point), fill = "goldenrod1",
#             size = 2.5, stroke = 1.25, alpha = .75) +
#  facet_col(~label, scales = "free") +
#  ylim(-0.08, 0.08) +
#  scale_fill_manual(values = c("#f05a71", "#8bdcbe")) +
#  scale_shape_manual(values = c("Contribution Point" = 23)) +
#  scale_color_manual(values = c("Contribution Point" = "black")) +
#  coord_flip() +
#  theme_bw() +
#  theme(legend.position = "bottom", 
#        strip.text.x = element_text(margin = margin(.075, 0, 0.075, 0, "cm")),
#        text = element_text(size = 10),
#        strip.text = element_text(size = 11),
#        axis.text = element_text(size = 10)) +
#  labs(y = "Contribution to Predicted Probability", x = "", shape = "", color = "")
