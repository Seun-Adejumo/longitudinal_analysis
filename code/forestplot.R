library(ggplot2)
library(dplyr)

data <- data.frame(
  Characteristic = c(
    "Age: 21–25", "Age: 26–30", "Age: 31–35", "Age: 36–40",
    "Gender: Male",
    "Primary Health Care", "Secondary Health Care",
    "Heard U=U: Yes",
    "Training: Rarely", "Training: Regularly",
    "Adequately Trained: Yes",
    "U=U Stigma: Unsure", "U=U Stigma: Yes"
  ),
  AOR = c(1.631, 0.098, 0.037, 0.787,
          2.351,
          7.609, 2.784,
          25.176,
          2.113, 0.300,
          41.635,
          1.107, 0.495),
  CI_lower = c(0.384, 0.025, 0.007, 0.131,
               1.129,
               2.451, 1.286,
               7.379,
               0.910, 0.104,
               17.295,
               0.339, 0.195),
  CI_upper = c(6.920, 0.375, 0.186, 4.741,
               4.896,
               23.620, 6.024,
               85.900,
               4.905, 0.860,
               100.232,
               3.610, 1.260),
  p_value = c(0.507, 0.0003, 0.0001, 0.793,
              0.022,
              0.0001, 0.009,
              0.0001,
              0.081, 0.025,
              0.0001,
              0.575, 0.140)
)

data <- data %>%
  mutate(Characteristic = factor(Characteristic, levels = rev(Characteristic)))

# Calculate limits with equal margins on both sides and extra padding
left_limit  <- min(data$CI_lower) * 0.8
right_limit <- max(data$CI_upper) * 2
left_margin_width <- 1 - left_limit
right_margin_width <- right_limit - 1
max_margin <- max(left_margin_width, right_margin_width)

# Multiply margin by 1.5 to add extra space
padding_factor <- 1.5
new_left_limit <- 1 - max_margin * padding_factor
new_right_limit <- 1 + max_margin * padding_factor

ggplot(data, aes(x = AOR, y = Characteristic)) +
  geom_point(size = 5, color = "red") +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.3) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black") +
  geom_text(aes(label = paste0("p = ", sprintf("%.4f", p_value)), x = CI_upper * 1.15),
            hjust = 0, size = 7, color = "black") +
  scale_x_log10(
    limits = c(new_left_limit, new_right_limit),
    expand = c(0, 0)
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 20) +
  labs(
    title = "Adjusted Odds Ratios with 95% CI and p-values",
    x = "Adjusted Odds Ratio (AOR)",
    y = "Variables"
  ) +
  theme(
    axis.text.y = element_text(size = 18, color = "black"),
    axis.text.x = element_text(size = 16, color = "black"),
    axis.title.x = element_text(size = 20, color = "black"),
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5, color = "red"),
    plot.margin = margin(5, 30, 5, 5)
  )
