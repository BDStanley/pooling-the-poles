#####Prepare workspace#####
system("git pull")
pacman::p_load(
  tidyverse,
  googledrive,
  rio,
  readxl,
  sf,
  glue,
  sjlabelled,
  lubridate,
  brms,
  stringr,
  tidybayes,
  ggdist,
  ggblend,
  seatdist
)

set.seed(780045)

# Constants
PARTY_COLS <- c(
  "PiS",
  "KO",
  "Lewica",
  "Razem",
  "Polska2050",
  "PSL",
  "Konfederacja",
  "Other"
)
PARTY_COLORS <- c(
  "PiS" = "blue",
  "KO" = "orange",
  "Polska 2050" = "goldenrod",
  "PSL" = "darkgreen",
  "Konfederacja" = "midnightblue",
  "Lewica" = "red",
  "Razem" = "purple",
  "MN" = "yellow",
  "Other" = "gray50"
)
TINY_CONSTANT <- 0.0005

# Theme functions
theme_plots <- function(base_size = 11, base_family = "Jost") {
  theme_bw(base_size, base_family) +
    theme(
      panel.background = element_rect(fill = "#ffffff", colour = NA),
      title = element_text(size = rel(1), family = "Jost", face = "bold"),
      plot.subtitle = element_text(
        size = rel(0.8),
        family = "Jost",
        face = "plain"
      ),
      plot.caption = element_text(
        margin = margin(t = 10),
        size = rel(0.6),
        family = "Jost",
        face = "plain"
      ),
      panel.border = element_rect(
        color = "grey50",
        fill = NA,
        linewidth = 0.15
      ),
      panel.spacing = unit(1, "lines"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.25, colour = "grey90"),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_text(
        size = rel(0.8),
        family = "Jost",
        face = "plain"
      ),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(hjust = 1, margin = margin(r = 10)),
      legend.position = "bottom",
      legend.title = element_text(
        size = rel(0.8),
        vjust = 0.5,
        family = "Jost",
        face = "bold"
      ),
      legend.key.size = unit(0.7, "line"),
      legend.key = element_blank(),
      legend.spacing = unit(0.1, "lines"),
      legend.justification = "left",
      legend.margin = margin(t = -5, b = 0, l = 0, r = 0),
      strip.text = element_text(
        size = rel(0.9),
        hjust = 0,
        family = "Jost",
        face = "plain"
      ),
      strip.background = element_rect(fill = "white", colour = NA),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
    )
}

theme_plots_map <- function(base_size = 11, base_family = "Jost") {
  theme_minimal(base_size, base_family) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      strip.text.x = element_text(size = 10, family = "Jost", face = "plain"),
      legend.text = element_text(size = 9, family = "Jost", face = "plain"),
      title = element_text(size = rel(1), family = "Jost", face = "bold"),
      plot.subtitle = element_text(
        size = rel(0.8),
        family = "Jost",
        face = "plain"
      ),
      plot.caption = element_text(
        margin = margin(t = 10),
        size = rel(0.6),
        family = "Jost",
        face = "plain"
      ),
      legend.title = element_text(family = "Jost", face = "plain"),
      plot.title = element_text(family = "Jost", face = "bold"),
      aspect.ratio = 1,
      legend.position = "none"
    )
}

my_date_format <- function() {
  function(x) {
    m <- format(x, "%b")
    y <- format(x, "\n%Y")
    ifelse(duplicated(y), m, paste(m, y))
  }
}

options(mc.cores = parallel::detectCores())

#####Helper functions#####
# Apply threshold and normalize
apply_threshold_and_normalize <- function(df, party_cols, threshold = 0.05) {
  outcome_matrix <- as.matrix(df[, party_cols])
  outcome_matrix_fixed <- outcome_matrix + TINY_CONSTANT
  outcome_matrix_fixed <- outcome_matrix_fixed / rowSums(outcome_matrix_fixed)

  for (i in seq_along(party_cols)) {
    df[[party_cols[i]]] <- outcome_matrix_fixed[, i]
  }

  df$outcome <- outcome_matrix_fixed
  df
}

# Calculate seats for constituency
calculate_constituency_seats <- function(data, weights, party_cols_list) {
  # Join weights data by okreg (select only needed columns to avoid conflicts)
  data_joined <- data %>%
    left_join(
      weights %>%
        select(
          okreg,
          magnitude,
          electors,
          validvotes,
          TDcoef,
          Lewicacoef,
          PiScoef,
          Konfcoef,
          KOcoef
        ),
      by = "okreg"
    )

  # Calculate weighted votes for each party
  for (party in party_cols_list) {
    coef_col <- case_when(
      party %in% c("PiS") ~ "PiScoef",
      party %in% c("KO") ~ "KOcoef",
      party %in% c("Lewica", "Razem") ~ "Lewicacoef",
      party %in% c("Konfederacja") ~ "Konfcoef",
      party %in% c("Polska 2050", "PSL") ~ "TDcoef",
      TRUE ~ NA_character_
    )

    if (!is.na(coef_col)) {
      data_joined[[party]] <- data_joined$validvotes *
        data_joined[[party]] *
        data_joined[[coef_col]]
    }
  }

  data_joined
}

# Create constituency ID mapping lookup table
create_const_id_mapping <- function() {
  tibble(
    cst = 1:41,
    id = c(
      24,
      27,
      4,
      7,
      28,
      34,
      25,
      26,
      29,
      36,
      31,
      33,
      37,
      40,
      13,
      12,
      22,
      1,
      6,
      14,
      35,
      21,
      10,
      38,
      39,
      16,
      17,
      30,
      23,
      18,
      11,
      32,
      41,
      15,
      5,
      19,
      20,
      2,
      3,
      8,
      9
    )
  )
}

# Generate seat map plots
generate_seat_map <- function(
  plotdata,
  party_name,
  display_name,
  color,
  limits = c(0, 20)
) {
  ggplot(plotdata) +
    geom_sf(aes(fill = as.integer(.data[[party_name]]))) +
    theme(aspect.ratio = 1) +
    geom_label(
      aes(
        x = x,
        y = y,
        group = .data[[party_name]],
        label = .data[[party_name]]
      ),
      fill = "white"
    ) +
    scale_fill_gradient(
      name = display_name,
      limits = limits,
      low = "white",
      high = color,
      guide = "colorbar"
    ) +
    labs(
      title = paste("Constituency-level share of seats for", display_name),
      subtitle = "Seat distribution reflects regional levels of support at October 2023 election",
      caption = ""
    ) +
    theme_plots_map()
}

#####Read in, adjust and subset data#####
source("poll data scraper.R")

polls <- polls_cleaned %>%
  select(startDate, endDate, org, all_of(PARTY_COLS[1:7]), Other, DK) %>%
  mutate(
    org = as.factor(org),
    startDate = as.Date(startDate),
    endDate = as.Date(endDate),
    midDate = as.Date(
      startDate + (difftime(endDate, startDate, units = "days") / 2)
    ),
    midDate_int = as.integer(midDate)
  ) %>%
  filter(midDate >= as.Date('2023-10-15'))

# Adjust for "Don't Know" responses
polls <- polls %>%
  mutate(across(all_of(PARTY_COLS[1:7]), ~ 100 / ((100 - DK)) * .x))

# Calculate time variables
polls <- polls %>%
  mutate(
    time = as.integer(difftime(midDate, min(midDate), units = "days")),
    pollster = as.integer(factor(org)),
    time = interval(min(midDate), midDate) / years(1)
  )

# Convert percentages to proportions
polls <- polls %>%
  mutate(across(
    all_of(PARTY_COLS[1:7]),
    ~ as.numeric(str_remove(as.character(.x), "%")) / 100
  ))

# Calculate Other and check totals
polls <- polls %>%
  mutate(
    Other = 1 - rowSums(across(all_of(PARTY_COLS[1:7]))),
    check = rowSums(across(all_of(PARTY_COLS)))
  ) %>%
  filter(Other > 0)

# Apply threshold fix and normalize
polls <- apply_threshold_and_normalize(polls, PARTY_COLS)

# Load weights and shapefile
weights <- read_excel('2023_elec_percentages.xlsx')
const <- st_read('GRED_20190215_Poland_2011.shp', quiet = TRUE)

# Generate pollster names
names <- glue_collapse(
  get_labels(as.factor(get_labels(polls$org))),
  ", ",
  last = " and "
)

#####Run model#####
m1 <- brm(
  formula = bf(
    outcome ~ 1 + s(time, k = 12, bs = "cs", m = 2) + (1 | pollster)
  ),
  family = dirichlet(link = "logit", refcat = "Other"),
  prior = prior(normal(0, 1.5), class = "Intercept", dpar = "muPiS") +
    prior(exponential(2), class = "sd", dpar = "muPiS") +
    prior(exponential(2), class = "sds", dpar = "muPiS") +
    prior(normal(0, 1.5), class = "Intercept", dpar = "muKO") +
    prior(exponential(2), class = "sd", dpar = "muKO") +
    prior(exponential(2), class = "sds", dpar = "muKO") +
    prior(normal(0, 1.5), class = "Intercept", dpar = "muLewica") +
    prior(exponential(2), class = "sd", dpar = "muLewica") +
    prior(exponential(2), class = "sds", dpar = "muLewica") +
    prior(normal(0, 1.5), class = "Intercept", dpar = "muRazem") +
    prior(exponential(2), class = "sd", dpar = "muRazem") +
    prior(exponential(2), class = "sds", dpar = "muRazem") +
    prior(normal(0, 1.5), class = "Intercept", dpar = "muPolska2050") +
    prior(exponential(2), class = "sd", dpar = "muPolska2050") +
    prior(exponential(2), class = "sds", dpar = "muPolska2050") +
    prior(normal(0, 1.5), class = "Intercept", dpar = "muPSL") +
    prior(exponential(2), class = "sd", dpar = "muPSL") +
    prior(exponential(2), class = "sds", dpar = "muPSL") +
    prior(normal(0, 1.5), class = "Intercept", dpar = "muKonfederacja") +
    prior(exponential(2), class = "sd", dpar = "muKonfederacja") +
    prior(exponential(2), class = "sds", dpar = "muKonfederacja") +
    prior(gamma(1, 0.01), class = "phi"),
  data = polls,
  seed = 780045,
  iter = 5000,
  backend = "cmdstanr",
  threads = threading(3),
  chains = 3,
  cores = 12,
  refresh = 5,
  control = list(adapt_delta = .95, max_treedepth = 15)
)

#####Trend plot#####
today <- interval(min(polls$midDate), Sys.Date()) / years(1)

pred_dta <- tibble(
  time = seq(0, today, length.out = nrow(polls)),
  date = as.Date(time * 365, origin = min(polls$midDate))
) %>%
  add_fitted_draws(model = m1, newdata = ., re_formula = NA) %>%
  group_by(date, .category) %>%
  rename(party = .category) %>%
  mutate(
    party = factor(
      party,
      levels = c(
        "PiS",
        "KO",
        "Polska2050",
        "PSL",
        "Lewica",
        "Razem",
        "Konfederacja",
        "Other"
      ),
      labels = c(
        "PiS",
        "KO",
        "Polska 2050",
        "PSL",
        "Lewica",
        "Razem",
        "Konfederacja",
        "Other"
      )
    )
  )

point_dta <- polls %>%
  select(midDate, all_of(PARTY_COLS)) %>%
  pivot_longer(cols = -midDate, names_to = "party", values_to = "est") %>%
  mutate(
    party = factor(
      party,
      levels = c(
        "PiS",
        "KO",
        "Polska2050",
        "PSL",
        "Lewica",
        "Razem",
        "Konfederacja",
        "Other"
      ),
      labels = c(
        "PiS",
        "KO",
        "Polska 2050",
        "PSL",
        "Lewica",
        "Razem",
        "Konfederacja",
        "Other"
      )
    )
  )

trends_parl <- pred_dta %>%
  ggplot(aes(x = date, color = party, fill = party)) +
  ggdist::stat_lineribbon(
    aes(y = .value, fill_ramp = stat(.width)),
    .width = seq(0, 0.95, 0.01)
  ) |>
    partition(vars(party)) |>
    blend("multiply") +
  geom_point(
    data = point_dta,
    aes(x = midDate, y = est, colour = party, fill = party),
    size = 1,
    show.legend = FALSE
  ) +
  scale_color_manual(values = PARTY_COLORS) +
  scale_fill_manual(values = PARTY_COLORS, guide = FALSE) +
  ggdist::scale_fill_ramp_continuous(range = c(1, 0), guide = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(date_breaks = "1 month", labels = my_date_format()) +
  coord_cartesian(
    xlim = c(min(polls$midDate), max(polls$midDate)),
    ylim = c(0, .5)
  ) +
  labs(
    y = "",
    x = "",
    title = "Trends",
    subtitle = str_wrap(
      str_c("Data from ", paste(names, collapse = ", "), "."),
      width = 120
    ),
    color = "",
    caption = "."
  ) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, fill = NA))) +
  theme_plots()

ggsave(
  trends_parl,
  file = "trends_parl.png",
  width = 7,
  height = 5,
  units = "cm",
  dpi = 600,
  scale = 3,
  bg = "white",
  device = png(type = "cairo")
)

#####Latest plot#####
plotdraws <- add_fitted_draws(
  model = m1,
  newdata = tibble(time = today),
  re_formula = NA
) %>%
  group_by(.category) %>%
  mutate(
    .category = factor(
      .category,
      levels = c(
        "PiS",
        "KO",
        "Lewica",
        "Razem",
        "Konfederacja",
        "Other",
        "PSL",
        "Polska2050"
      ),
      labels = c(
        "PiS",
        "KO",
        "Lewica",
        "Razem",
        "Konfederacja",
        "Other",
        "PSL",
        "Polska 2050"
      )
    )
  )

medians <- plotdraws %>%
  summarise(est = median(.value) * 100, .groups = "drop")

# Calculate dynamic probability comparison
comparison_data <- plotdraws %>%
  pivot_wider(names_from = .category, values_from = .value) %>%
  mutate(
    PiS_leading = PiS > KO,
    KO_leading = KO > PiS
  )

pis_median <- medians$est[medians$.category == "PiS"] / 100
ko_median <- medians$est[medians$.category == "KO"] / 100

if (pis_median > ko_median) {
  lead_prob <- mean(comparison_data$PiS_leading)
  lead_text <- paste("Pr(PiS > KO) = ", round(lead_prob, 2))
  lead_party <- "PiS"
} else {
  lead_prob <- mean(comparison_data$KO_leading)
  lead_text <- paste("Pr(KO > PiS) = ", round(lead_prob, 2))
  lead_party <- "KO"
}

# Calculate 5% threshold probabilities
threshold_probs <- plotdraws %>%
  group_by(.category) %>%
  summarise(
    median = median(.value),
    lower_95 = quantile(.value, 0.025),
    upper_95 = quantile(.value, 0.975),
    prob_above_5 = mean(.value >= 0.05),
    .groups = "drop"
  ) %>%
  filter(
    (lower_95 < 0.05 & upper_95 > 0.05) | (median < 0.06 & median > 0.04)
  ) %>%
  mutate(threshold_text = paste("Pr(≥5%) = ", round(prob_above_5, 2)))

latest_parl <- plotdraws %>%
  ggplot(aes(
    y = reorder(.category, dplyr::desc(-.value)),
    x = .value,
    color = .category
  )) +
  geom_vline(
    xintercept = 0.05,
    color = "grey60",
    linetype = "dashed",
    linewidth = 0.5
  ) +
  stat_interval(
    aes(x = .value, color_ramp = stat(.width)),
    .width = ppoints(100)
  ) %>%
    partition(vars(.category)) +
  scale_fill_manual(values = PARTY_COLORS, guide = FALSE) +
  scale_color_manual(name = " ", values = PARTY_COLORS, guide = FALSE) +
  ggdist::scale_color_ramp_continuous(range = c(1, 0), guide = FALSE) +
  scale_y_discrete(name = "") +
  geom_text(
    data = medians,
    aes(y = .category, x = est / 100, label = round(est, 0)),
    size = 3.5,
    hjust = 0.5,
    vjust = -1,
    family = "Jost",
    inherit.aes = FALSE
  ) +
  annotate(
    geom = "text",
    label = lead_text,
    y = lead_party,
    x = quantile(plotdraws$.value[plotdraws$.category == lead_party], 0.005),
    adj = c(1),
    family = "Jost",
    fontface = "plain",
    size = 3,
    color = "red"
  ) +
  {
    if (nrow(threshold_probs) > 0) {
      lapply(1:nrow(threshold_probs), function(i) {
        party_name <- threshold_probs$.category[i]
        threshold_text <- threshold_probs$threshold_text[i]
        x_pos <- quantile(
          plotdraws$.value[plotdraws$.category == party_name],
          0.995
        )

        annotate(
          geom = "text",
          label = threshold_text,
          y = party_name,
          x = x_pos,
          adj = c(0),
          family = "Jost",
          fontface = "plain",
          size = 3,
          color = "red"
        )
      })
    }
  } +
  scale_x_continuous(
    breaks = c(0, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5),
    labels = c("0", "5", "10", "20", "30", "40", "50")
  ) +
  expand_limits(x = 0) +
  labs(
    y = "",
    x = "",
    title = "Latest estimates",
    subtitle = str_wrap(
      str_c("Data from ", paste(names, collapse = ", "), "."),
      width = 120
    ),
    color = "",
    caption = "."
  ) +
  theme_plots()

ggsave(
  latest_parl,
  file = "latest_parl.png",
  width = 7,
  height = 5,
  units = "cm",
  dpi = 600,
  scale = 3,
  bg = "white"
)

#####Seat maps#####
library(seatdist)

# Calculate median estimates with threshold
median_estimates <- medians %>%
  mutate(est_threshold = ifelse(est >= 5, est, 0)) %>%
  select(.category, est_threshold)

# Calculate vote shares for each party by constituency
party_shares <- list(
  PiS = weights$PiScoef *
    median_estimates$est_threshold[median_estimates$.category == "PiS"],
  KO = weights$KOcoef *
    median_estimates$est_threshold[median_estimates$.category == "KO"],
  Lewica = weights$Lewicacoef *
    median_estimates$est_threshold[median_estimates$.category == "Lewica"],
  Razem = weights$Lewicacoef *
    median_estimates$est_threshold[median_estimates$.category == "Razem"],
  Konfederacja = weights$Konfcoef *
    median_estimates$est_threshold[
      median_estimates$.category == "Konfederacja"
    ],
  Polska2050 = weights$TDcoef *
    median_estimates$est_threshold[median_estimates$.category == "Polska 2050"],
  PSL = weights$TDcoef *
    median_estimates$est_threshold[median_estimates$.category == "PSL"],
  MN = c(
    0.12,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    5.37,
    rep(0, 20)
  )
)

# Vectorized seat calculation
poldHondt <- map_dfr(1:42, function(i) {
  votes <- map_dbl(party_shares, ~ (weights$validvotes[i] / 100) * .x[i])
  seats <- giveseats(
    v = votes,
    ns = weights$magnitude[i],
    method = "dh",
    thresh = 5
  )$seats
})

# Process seats table
seats <- bind_cols(
  poldHondt[-1, ],
  weights[-1, ] %>% select(-Lewica, -PiS, -KO, -TD, -Konf)
) %>%
  mutate(
    cst = row_number(),
    PiSKO = abs(PiS - KO),
    PiSmKO = PiS - KO
  )

# Map constituency IDs
const_mapping <- create_const_id_mapping()
const <- const %>%
  left_join(const_mapping, by = "cst")
seats <- seats %>%
  left_join(const_mapping, by = "cst")

# Generate label points
label_points <- const %>%
  st_point_on_surface() %>%
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(const %>% st_drop_geometry() %>% select(id)) %>%
  rename(x = X, y = Y)

plotdata <- const %>%
  left_join(seats %>% st_drop_geometry(), by = "id") %>%
  left_join(label_points, by = "id")

# Generate all seat maps
seat_map_configs <- list(
  list(party = "PiS", display = "PiS", color = "blue"),
  list(party = "KO", display = "Koalicja Obywatelska", color = "orange"),
  list(party = "Lewica", display = "Lewica", color = "red"),
  list(party = "Razem", display = "Razem", color = "purple"),
  list(party = "PSL", display = "PSL", color = "darkgreen"),
  list(
    party = "Konfederacja",
    display = "Konfederacja",
    color = "midnightblue"
  ),
  list(party = "Polska2050", display = "Polska 2050", color = "goldenrod")
)

for (config in seat_map_configs) {
  p <- generate_seat_map(plotdata, config$party, config$display, config$color)
  filename <- paste0(config$party, "_seats.png")
  ggsave(
    p,
    file = filename,
    width = 7,
    height = 7,
    units = "cm",
    dpi = 600,
    scale = 3,
    bg = "white"
  )
}

# PiS vs KO difference map
p_pis_ko <- ggplot(plotdata) +
  geom_sf(aes(fill = as.integer(PiSmKO))) +
  theme(aspect.ratio = 1) +
  scale_fill_gradient2(
    name = "PiSKO",
    limits = c(-20, 20),
    low = "orange",
    mid = "white",
    high = "blue",
    midpoint = 0,
    guide = "colorbar"
  ) +
  labs(
    title = "Constituency-level differences in share of seats for PiS and Koalicja Obywatelska",
    subtitle = "Constituencies in shades of blue have more PiS MPs; constituencies in orange have more KO MPs;\nconstituencies in white have equal numbers of PiS and KO MPs",
    caption = ""
  ) +
  theme_plots_map()
ggsave(
  p_pis_ko,
  file = "PiSKO_seats.png",
  width = 7,
  height = 7,
  units = "cm",
  dpi = 600,
  scale = 3,
  bg = "white"
)

#####Seats plot#####
plotdraws_seats <- add_fitted_draws(
  model = m1,
  newdata = tibble(time = today),
  re_formula = NA,
  n = 1000
) %>%
  mutate(.draw = row_number()) %>%
  group_by(.category, .draw) %>%
  mutate(
    .category = factor(
      .category,
      levels = c(
        "PiS",
        "KO",
        "Lewica",
        "Razem",
        "Konfederacja",
        "Other",
        "PSL",
        "Polska2050"
      ),
      labels = c(
        "PiS",
        "KO",
        "Lewica",
        "Razem",
        "Konfederacja",
        "Other",
        "PSL",
        "Polska 2050"
      )
    )
  )

# Calculate party medians for threshold application
party_medians <- plotdraws_seats %>%
  ungroup() %>%
  group_by(.category) %>%
  summarise(median_value = median(.value), .groups = "drop")

# Apply 5% threshold based on medians
plotdraws_wide <- plotdraws_seats %>%
  pivot_wider(names_from = .category, values_from = .value) %>%
  mutate(MN = rnorm(n(), mean = 0.079, sd = 0.00001))

# Apply threshold - set parties below 5% to zero
plotdraws_wide <- plotdraws_wide %>%
  mutate(
    across(
      any_of(c(
        "PiS",
        "KO",
        "Lewica",
        "Razem",
        "Konfederacja",
        "Polska 2050",
        "PSL"
      )),
      ~ {
        party_name <- cur_column()
        med_val <- party_medians$median_value[
          party_medians$.category == party_name
        ]
        if (length(med_val) > 0 && !is.na(med_val) && med_val < 0.05) {
          0
        } else {
          .x
        }
      }
    )
  )

# Expand to constituencies
consts <- plotdraws_wide %>%
  uncount(41, .id = "okreg") %>%
  calculate_constituency_seats(
    weights %>% filter(okreg != 0),
    c("PiS", "KO", "Lewica", "Razem", "Konfederacja", "Polska 2050", "PSL")
  )

# Handle MN special case (only okreg 21)
consts <- consts %>%
  mutate(MN = ifelse(okreg == 21, validvotes * MN, 0))

# Vectorized seat allocation
poldHondt_sim <- consts %>%
  rowwise() %>%
  mutate(
    seats = list(
      giveseats(
        v = c(KO, Konfederacja, Lewica, Razem, MN, PiS, `Polska 2050`, PSL),
        ns = magnitude,
        method = "dh",
        thresh = 0
      )$seats
    )
  ) %>%
  ungroup() %>%
  mutate(
    KO_seats = map_dbl(seats, ~ .x[1]),
    Konfederacja_seats = map_dbl(seats, ~ .x[2]),
    Lewica_seats = map_dbl(seats, ~ .x[3]),
    Razem_seats = map_dbl(seats, ~ .x[4]),
    MN_seats = map_dbl(seats, ~ .x[5]),
    PiS_seats = map_dbl(seats, ~ .x[6]),
    Polska2050_seats = map_dbl(seats, ~ .x[7]),
    PSL_seats = map_dbl(seats, ~ .x[8])
  ) %>%
  group_by(.draw) %>%
  summarise(
    KO = sum(KO_seats),
    PiS = sum(PiS_seats),
    Konfederacja = sum(Konfederacja_seats),
    `Polska 2050` = sum(Polska2050_seats),
    PSL = sum(PSL_seats),
    MN = sum(MN_seats),
    Lewica = sum(Lewica_seats),
    Razem = sum(Razem_seats),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(
      "KO",
      "Konfederacja",
      "Lewica",
      "Razem",
      "MN",
      "PiS",
      "Polska 2050",
      "PSL"
    ),
    names_to = "party",
    values_to = "seats"
  )

# Calculate seat statistics
frame <- poldHondt_sim %>%
  group_by(party) %>%
  summarise(median_qi(seats, .width = 0.8), .groups = "drop") %>%
  mutate(across(c(y, ymin, ymax), ~ round(.x, 0)))

# Add 2023 baseline
seats_2023 <- c(
  KO = 157,
  PiS = 194,
  Lewica = 19,
  Razem = 7,
  MN = 0,
  Konfederacja = 18,
  `Polska 2050` = 33,
  PSL = 32
)

frame <- frame %>%
  mutate(
    in2023 = seats_2023[party],
    party = factor(
      party,
      levels = c(
        "PiS",
        "KO",
        "Lewica",
        "Razem",
        "Konfederacja",
        "Polska 2050",
        "PSL",
        "MN"
      )
    ),
    diffPres = sprintf("(%+d)", y - in2023),
    party = reorder(party, -y)
  )

# Calculate coalition statistics
coalition_pis_konf <- frame$y[frame$party == "PiS"] +
  frame$y[frame$party == "Konfederacja"]
coalition_opposition <- sum(frame$y[
  frame$party %in% c("KO", "Lewica", "Polska 2050", "PSL")
])

pis_konf_status <- ifelse(coalition_pis_konf >= 231, "MAJORITY", "NO MAJORITY")
opposition_status <- ifelse(
  coalition_opposition >= 231,
  "MAJORITY",
  "NO MAJORITY"
)

pis_konf_text <- paste0(
  "PiS + Konfederacja: ",
  coalition_pis_konf,
  " seats\n",
  pis_konf_status
)
opposition_text <- paste0(
  "KO + Lewica + Polska 2050 + PSL: ",
  coalition_opposition,
  " seats\n",
  opposition_status
)

# Generate seats plot
seats_parl <- ggplot(
  data = frame,
  mapping = aes(x = party, y = y, fill = party)
) +
  geom_bar(stat = "identity", width = .75, show.legend = FALSE) +
  geom_hline(yintercept = c(231, 276, 307), colour = "gray10", linetype = 3) +
  scale_y_continuous(
    'Number of seats',
    limits = c(0, 320),
    breaks = c(0, 50, 100, 150, 200, 231, 276, 307)
  ) +
  scale_fill_manual(name = "Party", values = PARTY_COLORS) +
  annotate(
    "label",
    x = 2,
    y = 231,
    label = "Legislative majority",
    size = 2.5,
    hjust = 0,
    fill = "grey95",
    family = "Jost"
  ) +
  annotate(
    "label",
    x = 2,
    y = 276,
    label = "Overturn presidential veto",
    size = 2.5,
    hjust = 0,
    fill = "grey95",
    family = "Jost"
  ) +
  annotate(
    "label",
    x = 2,
    y = 307,
    label = "Constitutional majority",
    size = 2.5,
    hjust = 0,
    fill = "grey95",
    family = "Jost"
  ) +
  annotate(
    "label",
    x = 5,
    y = 280,
    label = pis_konf_text,
    size = 2.5,
    hjust = 0,
    fill = ifelse(coalition_pis_konf >= 231, "lightgreen", "lightcoral"),
    family = "Jost"
  ) +
  annotate(
    "label",
    x = 5,
    y = 250,
    label = opposition_text,
    size = 2.5,
    hjust = 0,
    fill = ifelse(coalition_opposition >= 231, "lightgreen", "lightcoral"),
    family = "Jost"
  ) +
  geom_text(
    aes(x = party, y = y + 18, label = y),
    size = 3,
    family = "Jost",
    hjust = 0.5
  ) +
  geom_text(
    aes(x = as.numeric(party) + 0.1, y = y + 18, label = diffPres),
    size = 2.5,
    family = "Jost",
    fontface = "italic",
    hjust = 0
  ) +
  geom_text(
    aes(x = party, y = y + 8, label = paste0("(", ymin, "\u2013", ymax, ")")),
    size = 2,
    family = "Jost"
  ) +
  labs(
    x = "",
    y = "Number of seats",
    title = "Estimated share of seats",
    subtitle = "Median estimated seat share with 80% credible intervals. Sum total may not equal 460.",
    caption = "Figures in brackets show change from 2023 share of seats."
  ) +
  theme_plots()

ggsave(
  seats_parl,
  file = "seats_parl.png",
  width = 7,
  height = 5,
  units = "cm",
  dpi = 600,
  scale = 3,
  bg = "white"
)

#####Save to Github#####
system("git add -A")
system('git commit -m "Update $(date +"%Y-%m-%d %H:%M:%S")"')
system("git push")
