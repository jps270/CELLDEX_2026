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
# Fixed-effect estimates with 95% and 50% Wald confidence intervals
kd_coefs <- as.data.frame(coef(summary(kd_model)))
kd_ci <- confint(kd_model, parm = "beta_", method = "Wald")
kd_ci50 <- confint(kd_model, parm = "beta_", method = "Wald", level = 0.50)

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
  conf.high = kd_ci[rownames(kd_coefs), 2],
  conf.low50 = kd_ci50[rownames(kd_coefs), 1],
  conf.high50 = kd_ci50[rownames(kd_coefs), 2]
) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    # p-values are only present when lmerTest is loaded
    p.value = if ("Pr(>|t|)" %in% names(kd_coefs)) kd_coefs[term, "Pr(>|t|)"] else NA_real_,
    # Significant if the 95% CI excludes zero on the log scale (excludes 1 as a ratio)
    Significant = ifelse(conf.low > 0 | conf.high < 0, "95% CI excludes 1", "95% CI includes 1"),
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
                   aes(x = exp(estimate),
                       y = label,
                       color = Significant)) +
  
  # No-effect reference line (ratio = 1)
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  
  # 95% confidence intervals (thin whiskers)
  geom_errorbar(
    aes(xmin = exp(conf.low),
        xmax = exp(conf.high)),
    width = 0.2,
    linewidth = 0.7
  ) +
  
  # 50% confidence intervals (thick inner band)
  geom_errorbar(
    aes(xmin = exp(conf.low50),
        xmax = exp(conf.high50)),
    width = 0,
    linewidth = 2.5
  ) +
  
  # Point estimates
  geom_point(size = 3, shape = 21, fill = "white", stroke = 1.2) +
  
  # Group terms by effect type
  facet_grid(
    Effect_type ~ .,
    scales = "free_y",
    space = "free_y"
  ) +
  
  # Log scale so increases and decreases are symmetric around 1
  scale_x_log10() +
  
  # Colors
  scale_color_manual(
    values = c(
      "95% CI excludes 1" = "#D7191C",
      "95% CI includes 1" = "#8EC5E8"
    )
  ) +
  
  # Labels
  labs(
    x = "Multiplicative effect on kd (exp(estimate))",
    y = NULL,
    color = NULL,
    caption = paste0(
      "Thick bars: 50% CI; thin whiskers: 95% CI\n",
      "Main effects are at the reference levels (Forest, Dry, Instream)"
    )
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

# Save forest plot
ggsave(
  "kd_forest_plot.png",
  plot = p_forest,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300
)

# Same forest plot on the log(kd) scale (model estimates, not back-transformed)
p_forest_log <- ggplot(forest_df,
                       aes(x = estimate,
                           y = label,
                           color = Significant)) +
  
  # No-effect reference line (estimate = 0)
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  
  # 95% confidence intervals (thin whiskers)
  geom_errorbar(
    aes(xmin = conf.low,
        xmax = conf.high),
    width = 0.2,
    linewidth = 0.7
  ) +
  
  # 50% confidence intervals (thick inner band)
  geom_errorbar(
    aes(xmin = conf.low50,
        xmax = conf.high50),
    width = 0,
    linewidth = 2.5
  ) +
  
  # Point estimates
  geom_point(size = 3, shape = 21, fill = "white", stroke = 1.2) +
  
  # Group terms by effect type
  facet_grid(
    Effect_type ~ .,
    scales = "free_y",
    space = "free_y"
  ) +
  
  # Colors (same groups as above; on this scale the cutoff is 0)
  scale_color_manual(
    values = c(
      "95% CI excludes 1" = "#D7191C",
      "95% CI includes 1" = "#8EC5E8"
    ),
    labels = c(
      "95% CI excludes 1" = "95% CI excludes 0",
      "95% CI includes 1" = "95% CI includes 0"
    )
  ) +
  
  # Labels
  labs(
    x = "Estimate (log kd)",
    y = NULL,
    color = NULL,
    caption = paste0(
      "Thick bars: 50% CI; thin whiskers: 95% CI\n",
      "Main effects are at the reference levels (Forest, Dry, Instream)"
    )
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
  );p_forest_log

# Save log-scale forest plot
ggsave(
  "kd_forest_plot_log.png",
  plot = p_forest_log,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300
)

#Simple effects forest plot (emmeans)------------------
# Wet vs Dry effect within each land use and strip position
kd_emm <- emmeans::emmeans(kd_model, ~ Season | Land_use * Treatment)
season_eff <- emmeans::contrast(kd_emm, method = "revpairwise")  # WET - DRY

# 95% CIs and p-values Bonferroni-adjusted across all 6 land use x position groups
# (by = NULL makes the 6 contrasts one family; otherwise each group is its own family of 1)
season_ci <- as.data.frame(summary(season_eff, by = NULL, adjust = "bonferroni",
                                   infer = c(TRUE, TRUE), level = 0.95))
# 50% band left unadjusted
season_ci50 <- as.data.frame(confint(season_eff, by = NULL, adjust = "none", level = 0.50))

season_df <- season_ci %>%
  mutate(
    conf.low50 = season_ci50$lower.CL,
    conf.high50 = season_ci50$upper.CL,
    # Significant if the adjusted 95% CI excludes zero on the log scale (excludes 1 as a ratio)
    Significant = ifelse(lower.CL > 0 | upper.CL < 0, "95% CI excludes 1", "95% CI includes 1"),
    # Forest at the top
    Land_use = factor(Land_use, levels = rev(levels(kd_site$Land_use)))
  )

p_season <- ggplot(season_df,
                   aes(x = exp(estimate),
                       y = Land_use,
                       color = Significant)) +
  
  # No-effect reference line (ratio = 1)
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  
  # 95% confidence intervals (thin whiskers)
  geom_errorbar(
    aes(xmin = exp(lower.CL),
        xmax = exp(upper.CL)),
    width = 0.2,
    linewidth = 0.7
  ) +
  
  # 50% confidence intervals (thick inner band)
  geom_errorbar(
    aes(xmin = exp(conf.low50),
        xmax = exp(conf.high50)),
    width = 0,
    linewidth = 2.5
  ) +
  
  # Point estimates
  geom_point(size = 3, shape = 21, fill = "white", stroke = 1.2) +
  
  # Separate Instream and Riparian rows
  facet_grid(
    Treatment ~ .
  ) +
  
  # Log scale so increases and decreases are symmetric around 1
  scale_x_log10() +
  
  # Colors
  scale_color_manual(
    values = c(
      "95% CI excludes 1" = "#D7191C",
      "95% CI includes 1" = "#8EC5E8"
    )
  ) +
  
  # Labels
  labs(
    x = "Wet vs Dry effect on kd (ratio)",
    y = NULL,
    color = NULL,
    caption = paste0(
      "Thick bars: 50% CI (unadjusted)\n",
      "Thin whiskers: 95% CI (Bonferroni-adjusted, 6 comparisons)"
    )
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
  );p_season

# Save simple effects forest plot
ggsave(
  "kd_season_simple_effects.png",
  plot = p_season,
  width = 7,
  height = 4,
  units = "in",
  dpi = 300
)

# Wet vs Dry simple effects as ratios
season_df %>%
  mutate(
    ratio = exp(estimate),
    ratio.low = exp(lower.CL),
    ratio.high = exp(upper.CL)
  ) %>%
  select(Treatment, Land_use, estimate, lower.CL, upper.CL, ratio, ratio.low, ratio.high, p.value)

#Land use simple effects forest plot (emmeans)------------------
# Pasture vs Forest and Urban vs Forest within each season and strip position
landuse_emm <- emmeans::emmeans(kd_model, ~ Land_use | Season * Treatment)
landuse_eff <- emmeans::contrast(landuse_emm, method = "trt.vs.ctrl")  # Pasture - Forest, Urban - Forest

# 95% CIs and p-values Bonferroni-adjusted across all 8 contrasts
# (2 land use contrasts x 4 season/position groups; by = NULL makes them one family)
landuse_ci <- as.data.frame(summary(landuse_eff, by = NULL, adjust = "bonferroni",
                                    infer = c(TRUE, TRUE), level = 0.95))
# 50% band left unadjusted
landuse_ci50 <- as.data.frame(confint(landuse_eff, by = NULL, adjust = "none", level = 0.50))

landuse_df <- landuse_ci %>%
  mutate(
    conf.low50 = landuse_ci50$lower.CL,
    conf.high50 = landuse_ci50$upper.CL,
    # Significant if the adjusted 95% CI excludes zero on the log scale (excludes 1 as a ratio)
    Significant = ifelse(lower.CL > 0 | upper.CL < 0, "95% CI excludes 1", "95% CI includes 1"),
    # "Pasture - Forest" -> "Pasture vs Forest", Pasture at the top
    contrast = factor(gsub(" - ", " vs ", contrast),
                      levels = c("Urban vs Forest", "Pasture vs Forest")),
    # Proper case season labels for the facets
    Season = factor(tools::toTitleCase(tolower(as.character(Season))),
                    levels = c("Dry", "Wet"))
  )

p_landuse <- ggplot(landuse_df,
                    aes(x = exp(estimate),
                        y = contrast,
                        color = Significant)) +
  
  # No-effect reference line (ratio = 1)
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  
  # 95% confidence intervals (thin whiskers)
  geom_errorbar(
    aes(xmin = exp(lower.CL),
        xmax = exp(upper.CL)),
    width = 0.2,
    linewidth = 0.7
  ) +
  
  # 50% confidence intervals (thick inner band)
  geom_errorbar(
    aes(xmin = exp(conf.low50),
        xmax = exp(conf.high50)),
    width = 0,
    linewidth = 2.5
  ) +
  
  # Point estimates
  geom_point(size = 3, shape = 21, fill = "white", stroke = 1.2) +
  
  # Season rows, Instream and Riparian columns
  facet_grid(
    Season ~ Treatment
  ) +
  
  # Log scale so increases and decreases are symmetric around 1
  scale_x_log10() +
  
  # Colors
  scale_color_manual(
    values = c(
      "95% CI excludes 1" = "#D7191C",
      "95% CI includes 1" = "#8EC5E8"
    )
  ) +
  
  # Labels
  labs(
    x = "Land use effect on kd (ratio)",
    y = NULL,
    color = NULL,
    caption = paste0(
      "Thick bars: 50% CI (unadjusted)\n",
      "Thin whiskers: 95% CI (Bonferroni-adjusted, 8 comparisons)"
    )
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
  );p_landuse

# Save land use simple effects forest plot
ggsave(
  "kd_landuse_simple_effects.png",
  plot = p_landuse,
  width = 8,
  height = 4.5,
  units = "in",
  dpi = 300
)

# Land use simple effects as ratios
landuse_df %>%
  mutate(
    ratio = exp(estimate),
    ratio.low = exp(lower.CL),
    ratio.high = exp(upper.CL)
  ) %>%
  select(Season, Treatment, contrast, estimate, lower.CL, upper.CL, ratio, ratio.low, ratio.high, p.value)

# Estimates are on the log scale; exp(estimate) gives the
# multiplicative change in kd relative to the reference
forest_df %>%
  mutate(
    ratio = exp(estimate),
    ratio.low = exp(conf.low),
    ratio.high = exp(conf.high)
  ) %>%
  select(label, estimate, conf.low, conf.high, ratio, ratio.low, ratio.high, p.value)
