library(ggplot2)
library(here)
library(patchwork)
library(stringr)
library(ggdist)

# ---- Plot 1: Proportion Correct by Condition ----

blue_shades = c(
  "real" = "#1B9E77",        
  "artificial" = "#2171B5",  
  "scram" = "#D95F02"  
)

summary_p_correct$plot_label = "Proportion Correct by Condition"

plot_p_correct = ggplot(summary_p_correct, aes(x = condition, y = `P(correct)`, fill = condition)) +
  geom_col(width = 0.4, color = "black") +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.2) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_fill_manual(values = blue_shades) +
  facet_wrap(~plot_label) +
  labs(
    x = "Condition",
    y = "P(correct)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "grey20"),
    strip.text = element_text(color = "white", size = 14, face = "bold", margin = margin(t = 10, b = 10)),
    legend.position = "none",
    panel.border = element_rect(color = "grey", fill = NA, size = 1),
    axis.line = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )

# ---- Plot 2: Response Types by Condition ----
summary_all_response_types$plot_label = "Proportion of Selected Response Types by Condition"

summary_filtered = summary_all_response_types %>%
  filter(response_type %in% c("2AFC_correct", "none_correct"))

response_type_colors_filtered = c(
  "2AFC_correct" = "#2171B5",
  "none_correct" = "#D95F02"
)

plot_response_types = ggplot(summary_filtered, aes(
  x = condition,
  y = proportion,
  group = response_type
)) +
  geom_point(aes(shape = response_type, fill = response_type),
             position = position_dodge(width = 0.4),
             size = 5, stroke = 1.2, color = "black") +
  scale_shape_manual(values = c("2AFC_correct" = 22, "none_correct" = 21)) +
  scale_fill_manual(values = response_type_colors_filtered) +
  scale_color_manual(values = response_type_colors_filtered) +
  scale_y_continuous(limits = c(0, 0.5), breaks = seq(0, 0.5, 0.1)) +
  facet_wrap(~plot_label) +
  labs(
    x = "Condition",
    y = "Proportion",
    fill = "Response Type",
    shape = "Response Type",
    color = "Response Type"
  ) +
  theme_classic(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "grey20"),
    strip.text = element_text(color = "white", size = 14, face = "bold", margin = margin(t = 10, b = 10)),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.spacing.x = unit(0.4, 'cm'),
    panel.border = element_rect(color = "grey", fill = NA, size = 1),
    axis.line = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )


# ---- M3 Model ----


# ---- Plot Posterior Distributions for a ----

p1 = ggplot(a_draws, aes(x = diff_real_scram_a)) +
  geom_density(fill = blue_shades["real"], alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Real-world vs Scrambled") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(color = "grey", fill = NA),
    axis.line = element_blank(),
    axis.title = element_blank()
  )

p2 = ggplot(a_draws, aes(x = diff_real_artificial_a)) +
  geom_density(fill = blue_shades["artificial"], alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Real-world vs Artificial") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(color = "grey", fill = NA),
    axis.line = element_blank(),
    axis.title = element_blank()
  )

p3 = ggplot(a_draws, aes(x = diff_artificial_scram_a)) +
  geom_density(fill = blue_shades["scram"], alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Artificial vs Scrambled") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(color = "grey", fill = NA),
    axis.line = element_blank(),
    axis.title = element_blank()
  )

title_strip = ggplot() +
  theme_void() +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "grey20") +
  annotate("text", x = 0.5, y = 0.5, label = "Posterior Distributions for Item Memory Differences",
           color = "white", size = 5.5, fontface = "bold", hjust = 0.5, vjust = 0.5) +
  theme(
    plot.background = element_rect(fill = "grey20", color = "black", size = 1)
  )

title_element = patchwork::wrap_elements(full = title_strip)

plot_item_memory = (title_element / (p1 | p2 | p3)) +
  plot_layout(heights = c(0.04, 1))


# ---- Plot Posterior Distributions for c ----

p1 = ggplot(c_draws, aes(x = diff_real_scram_c)) +
  geom_density(fill = blue_shades["real"], alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Real-world vs Scrambled") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(color = "grey", fill = NA),
    axis.line = element_blank(),
    axis.title = element_blank()
  )

p2 = ggplot(c_draws, aes(x = diff_real_artificial_c)) +
  geom_density(fill = blue_shades["artificial"], alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Real-world vs Artificial") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(color = "grey", fill = NA),
    axis.line = element_blank(),
    axis.title = element_blank()
  )

p3 = ggplot(c_draws, aes(x = diff_artificial_scram_c)) +
  geom_density(fill = blue_shades["scram"], alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Artificial vs Scrambled") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(color = "grey", fill = NA),
    axis.line = element_blank(),
    axis.title = element_blank()
  )

title_strip = ggplot() +
  theme_void() +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "grey20") +
  annotate("text", x = 0.5, y = 0.5, label = "Posterior Distributions for Binding Memory Differences",
           color = "white", size = 5.5, fontface = "bold", hjust = 0.5, vjust = 0.5) +
  theme(
    plot.background = element_rect(fill = "grey20", color = "black", size = 1)
  )

title_element = patchwork::wrap_elements(full = title_strip)

plot_binding_memory = (title_element / (p1 | p2 | p3)) +
  plot_layout(heights = c(0.04, 1))


# ---- Estimates a - c ----
condition_colors = c(
  "real" = "#1B9E77",
  "artificial" = "#2171B5",
  "scram" = "#D95F02"
)

plot_estimates = ggplot()+
  stat_halfeye(
    data = plot_data,
    aes(x = condition, y = exp(.value), fill = condition),
    side = "left",
    adjust = 0.7,
    slab_alpha = 1,
    slab_color = "black",
    slab_linewidth = 0.4,
    width = 0.6,
  ) +
  facet_wrap(~param, scales = "fixed") +
  scale_fill_manual(values = condition_colors) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    x = "Condition",
    y = "Value"
  ) +
  theme_classic(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "grey20"),
    strip.text = element_text(color = "white", face = "bold"),
    panel.border = element_rect(color = "grey", fill = NA),
    legend.position = "bottom",
    axis.line = element_blank(),
    axis.title = element_blank()
  )


all_plots = list(
  "Proportion Correct by Condition" = plot_p_correct,
  "Proportion of Selected Response Types by Condition" = plot_response_types,
  "Posterior Distributions for Item Memory Differences" = plot_item_memory,
  "Posterior Distributions for Binding Memory Differences" = plot_binding_memory,
  "Parameters Estimates" = plot_estimates
)

for (name in names(all_plots)) {
  file_name = paste0(str_replace_all(tolower(name), "[^a-z0-9]+", "_"), ".png")

  print(paste("Done:", file_name))

  ggsave(
    filename = here("Exp2/Exp2_data_analysis/plots/", file_name),
    plot = all_plots[[name]],
    width = 8,
    height = 5,
    dpi = 300
  )

}

# Plot 2
summary_p_correct_2AFC$plot_label = "Proportion 2AFC Correct by Condition"
ggplot(summary_p_correct_2AFC, aes(x = condition, y = `P(correct)`, fill = condition)) +
  geom_col(width = 0.4, color = "black") +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.2) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_fill_manual(values = blue_shades) +
  facet_wrap(~plot_label) +
  labs(
    x = "Condition",
    y = "P(correct)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "grey20"),
    strip.text = element_text(color = "white", size = 14, face = "bold", margin = margin(t = 10, b = 10)),
    legend.position = "none",
    panel.border = element_rect(color = "grey", fill = NA, size = 1),
    axis.line = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )

plot_2AFC_6AFC = ggplot(summary_combined, aes(x = condition, y = `P(correct)`, fill = condition)) +
  geom_col(width = 0.4, color = "black", position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.2, position = position_dodge(width = 0.6)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_fill_manual(values = blue_shades) +
  facet_wrap(~type) +
  labs(
    x = "Condition",
    y = "Proportion Correct"
  ) +
  theme_classic(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "grey20"),
    strip.text = element_text(color = "white", size = 14, face = "bold", margin = margin(t = 10, b = 10)),
    legend.position = "none",
    panel.border = element_rect(color = "grey", fill = NA, size = 1),
    axis.line = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggplot(summary_combined, aes(x = condition, y = `P(correct)`, fill = type)) +
  geom_col(
    width = 0.4,
    color = "black",
    position = position_dodge(width = 0.5)
  ) +
  geom_errorbar(
    aes(ymin = CI_low, ymax = CI_high),
    width = 0.15,
    position = position_dodge(width = 0.5)
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_fill_manual(
    values = c(
      "Both Correct" = "#1B9E77",    
      "2AFC Correct" = "#A6D9C2"     
    )
  ) +
  labs(
    x = "Condition",
    y = "Proportion Correct",
    fill = "Response Type"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    panel.border = element_rect(color = "grey", fill = NA, size = 1),
    axis.line = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )