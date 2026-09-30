setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

kd_df <- read.csv("Cottonstrip_data.csv")
# Make sure variables are factors in the desired order
kd_df <- kd_df %>%
  mutate(
    Land_use = factor(Land_use,
                      levels = c("Forest", "Pasture", "Urban")),
    Treatment = factor(Treatment,
                  levels = c("Instream", "Riparian")),
    Final_month = factor(Final_month,
                         levels = c("March", "April", "July", "August"))
  )
#figure intearction analysis-----------------------------------------
# Calculate mean and standard error across land uses for plotting
kd_summary <- kd_df %>%
  group_by(Land_use, Treatment, Final_month) %>%
  summarise(
    mean_kd = mean(kd, na.rm = TRUE),
    SE = sd(kd, na.rm = TRUE) / sqrt(sum(!is.na(kd))),
    .groups = "drop"
  )
# Make figure
p <- ggplot(kd_summary,
            aes(x = Final_month,
                y = mean_kd,
                group = Land_use,
                color = Land_use)) +
  
  # Lines
  geom_line(linewidth = 1) +
  
  # Points
  geom_point(size = 3) +
  
  # Error bars
  geom_errorbar(
    aes(ymin = mean_kd - SE,
        ymax = mean_kd + SE),
    width = 0.10,
    linewidth = 0.7
  ) +
  
  # Separate Instream and Riparian columns,
  # Land use rows
  facet_grid(
    Land_use ~ Treatment
  ) +
  
  # Colors
  scale_color_manual(
    values = c(
      "Forest" = "#0072B2",
      "Pasture" = "#E6B800",
      "Urban" = "#B04A4A"
    )
  ) +
  
  # Labels
  labs(
    x = NULL,
    y = "Mean kd (± SE)",
    color = "Land cover"
  ) +
  
  # Theme
  theme_bw(base_size = 12) +
  
  theme(
    legend.position = "right",
    
    # Gridlines similar to your example
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95"),
    
    # Facet labels
    strip.background = element_rect(
      fill = "white",
      color = "black"
    ),
    strip.text = element_text(
      face = "bold"
    ),
    
    # Make x-axis labels horizontal
    axis.text.x = element_text(angle = 0),
    
    # Remove redundant legend title styling
    legend.title = element_text(face = "bold")
  );p


#Analysis------------------
#Calculate mean kd across replicates within a site
kd_site <- kd_df %>%
  group_by(
    Site,
    Treatment,
    Final_month
  ) %>%
  summarise(
    kd = mean(kd, na.rm = TRUE),
    Land_cover = first(Land_cover),
    Land_use = first(Land_use),
    Season = first(Season),
    Locality=first(Locality),
    .groups = "drop"
  )
#Set references
kd_site <- kd_site %>%
  mutate(
    Land_use = factor(
      Land_use,
      levels = c("Forest", "Pasture", "Urban")
    ),
    Season = factor(
      Season,
      levels = c("DRY", "WET")
    ),
    Treatment = factor(
      Treatment,
      levels = c("Instream", "Riparian")
    )
  )

# Use Forest, DRY, and Instream as reference levels
options(contrasts = c("contr.treatment", "contr.poly"))
#Run the interaction model
kd_model <- lmer(
  log(kd) ~ Land_use * Season * Treatment + (1 | Site),
  data = kd_site
)

summary(kd_model)
anova(kd_model, type = 3)
kd_df %>%
  distinct(Locality, Land_use) %>%
  count(Land_use)

#Forest plot of mixed model results------------------
# Fixed-effect estimates with 95% Wald confidence intervals
kd_coefs <- as.data.frame(coef(summary(kd_model)))
kd_ci <- confint(kd_model, parm = "beta_", method = "Wald")

# Build contrast labels from the factor levels in proper case, e.g. "Season" -> "Wet vs Dry"
make_contrasts <- function(var) {
  lv <- levels(kd_site[[var]])
  lv_label <- tools::toTitleCase(tolower(lv))
  setNames(paste(lv_label[-1], "vs", lv_label[1]), paste0(var, lv[-1]))
}
contrast_lookup <- c(
  make_contrasts("Land_use"),
  make_contrasts("Season"),
  make_contrasts("Treatment")
)

forest_df <- data.frame(
  term = rownames(kd_coefs),
  estimate = kd_coefs[, "Estimate"],
  conf.low = kd_ci[rownames(kd_coefs), 1],
  conf.high = kd_ci[rownames(kd_coefs), 2]
) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    # p-values are only present when lmerTest is loaded
    p.value = if ("Pr(>|t|)" %in% names(kd_coefs)) kd_coefs[term, "Pr(>|t|)"] else NA_real_,
    # Significant if the 95% CI excludes zero
    Significant = ifelse(conf.low > 0 | conf.high < 0, "95% CI excludes 0", "95% CI includes 0"),
    # Main effect, 2-way, or 3-way interaction
    Effect_type = factor(
      case_when(
        lengths(strsplit(term, ":")) == 1 ~ "Main effect",
        lengths(strsplit(term, ":")) == 2 ~ "Two-way interaction",
        TRUE ~ "Three-way interaction"
      ),
      levels = c("Main effect", "Two-way interaction", "Three-way interaction")
    ),
    # Main effects: "Wet vs Dry"; interactions: "Pasture × Wet", "Urban × Wet × Riparian"
    label = unname(sapply(strsplit(term, ":"), function(x) {
      if (length(x) == 1) {
        contrast_lookup[x]
      } else {
        lv <- gsub("^(Land_use|Season|Treatment)", "", x)
        paste(tools::toTitleCase(tolower(lv)), collapse = " \u00d7 ")
      }
    })),
    # Keep model order from top to bottom
    label = factor(label, levels = rev(unique(label)))
  )

p_forest <- ggplot(forest_df,
                   aes(x = estimate,
                       y = label,
                       color = Significant)) +
  
  # Zero reference line
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  
  # Confidence intervals
  geom_errorbar(
    aes(xmin = conf.low,
        xmax = conf.high),
    width = 0.2,
    linewidth = 0.7
  ) +
  
  # Point estimates
  geom_point(size = 3) +
  
  # Group terms by effect type
  facet_grid(
    Effect_type ~ .,
    scales = "free_y",
    space = "free_y"
  ) +
  
  # Colors
  scale_color_manual(
    values = c(
      "95% CI excludes 0" = "#D7191C",
      "95% CI includes 0" = "#8EC5E8"
    )
  ) +
  
  # Labels
  labs(
    x = "Estimate (log kd) ± 95% CI",
    y = NULL,
    color = NULL,
    caption = "Main effects are at the reference levels (Forest, Dry, Instream)"
  ) +
  
  # Theme
  theme_bw(base_size = 12) +
  
  theme(
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    
    # Facet labels
    strip.background = element_rect(
      fill = "white",
      color = "black"
    ),
    strip.text = element_text(
      face = "bold"
    ),
    strip.text.y = element_text(angle = 0)
  );p_forest

# Estimates are on the log scale; exp(estimate) gives the
# multiplicative change in kd relative to the reference
forest_df %>%
  mutate(
    ratio = exp(estimate),
    ratio.low = exp(conf.low),
    ratio.high = exp(conf.high)
  ) %>%
  select(label, estimate, conf.low, conf.high, ratio, ratio.low, ratio.high, p.value)
