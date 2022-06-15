theme_changes <- theme(axis.title.y = element_text(family="Roboto Condensed Light"), axis.title.x = element_text(family="Roboto Condensed Light"),
                       axis.text.x = element_text(size=10, family="Roboto Condensed Light"), axis.text.y = element_text(size=10, family="Roboto Condensed Light"),
                       strip.text.x = element_text(size = 10, family="Roboto Condensed Light"), legend.text = element_text(size=9, family="Roboto Condensed Light"), 
                       legend.title = element_text(size=10, family="Roboto Condensed Light"), plot.title = element_text(size=14, family="Roboto Condensed"),
                       plot.subtitle = element_text(size=10, family="Roboto Condensed Light"))

priors = c(
  prior(normal(0.5,0.02), class = b)
)

priors %>%
  parse_dist(prior) %>%
  ggplot(aes(y = class, xdist = .dist, args = .args)) +
  stat_halfeye() +
  labs(
    title = "stat_halfeye()",
    subtitle = "with brms::prior() and ggdist::parse_dist() to visualize priors",
    x = NULL
  )



dta <- data.frame(rdirichlet(75, c(28, 52, 20)))
colnames(dta) <- c("tak", "nie", "trpo")
dta <-
  dta %>%
  mutate(
    outcome = as.matrix(dta[names(dta) %in% c("tak", "nie", "trpo")])
  )

model <-
  brm(formula = bf(outcome ~ 1),
      family = dirichlet(link = "logit", refcat = "trpo"),
      prior =
        prior(normal(0.8,0.02), class = "Intercept", dpar = "mutak") +
        prior(normal(0.2,0.02), class = "Intercept", dpar = "munie"),      
      data = dta,
      seed = 780045,
      iter = 5000,
      backend="cmdstanr", chains=6, cores=12, threads = threading(6),
      refresh = 5,
      control =
        list(
          adapt_delta = .95,
          max_treedepth = 15
        )
  )

pred_dta <-
  tibble(
    est = seq(0, length.out = nrow(dta))
  )

plotdraws <-
  add_fitted_draws(
    model = model,
    newdata = pred_dta,
    re_formula = NA
  ) %>%
  group_by(.category) %>%
  rename(answer = .category) %>%
  mutate(
    answer =
      answer %>%
      factor(
        levels = c("tak", "nie", "trpo"),
        labels = c("Tak", "Nie", "Trudno powiedzieć")
      )
  )

medians <- plotdraws %>%
  summarise(est = median(.value)*100, .groups = "drop")

taknie.diff <- plotdraws %>%
  pivot_wider(names_from=answer, values_from=.value) %>%
  mutate(., taknie = Tak-Nie,
         taknie = sum((taknie < 0) / length(taknie)),
         taknie = round(taknie, 2)) %>%
  pull(taknie) %>%
  last(.)

plot <-
  add_fitted_draws(
    model = model,
    newdata = pred_dta,
    re_formula = NA
  ) %>%
  group_by(.category) %>%
  rename(answer = .category) %>%
  mutate(
    answer =
      answer %>%
      factor(
        levels = c("tak", "nie", "trpo"),
        labels = c("Tak", "Nie", "Trudno powiedzieć")
      )
  ) %>%
  ggplot(aes(x=.value*100, fill=answer)) + stat_halfeye(alpha=0.9, .width=0.95) +
  scale_fill_manual(name="", values=c("#659952", "#eb2d2d", "#ffe600")) +
  guides(fill = guide_legend(override.aes = list(linetype = 0, point_alpha=0))) +  
  labs(caption="Ben Stanley (@BDStanley; benstanley.pl).", x="%", y="Gęstość", title="Czy świadczenie 500+ powinno podlegać corocznej waloryzacji o wskaźnik inflacji - tak jak renty czy emerytury?",
       subtitle="Lewica (n=75)") +
  theme_minimal() +
  theme_ipsum_rc() +
  theme_changes
ggsave(plot, file = "500plus_prior_left.png", 
       width = 7, height = 5, units = "cm", dpi = 320, scale = 4, bg="white")