#===============================================================================
# code pour faire des analyses statistiques
#-------------------------------------------------------------------------------

# charger les dépendances ------------------------------------------------------
if (!existsFunction("read_excel")) library ("tidyverse")
if (!existsFunction("brm")) library ("brms")
if (!existsFunction("summarize_draws")) library ("posterior")

# charger les données ----------------------------------------------------------
if (!exists(("d"))) source("0_lire_données.R")

# variation du pH de la sève ---------------------------------------------------
mod_sap_pH <- brm(
  formula = pH ~ (1 | p) + s * e,
  data = d %>% filter(m == "EAU") %>% select(p, s, e, pH),
  family = gaussian(),  # Default is Gaussian, so this line is optional
  prior = c(prior(normal(7, 5), class = "Intercept"), # Prior for the intercept
            prior(normal(0, 2), class = "b")),        # Prior for the fixed effects
  iter = 6000,      # Number of iterations
  warmup = 2000,    # Number of warmup iterations
  chains = 4,       # Number of chains
  cores = 4,        # Number of cores to use
  seed = 1259       # Seed for reproducibility
)

# inspect le modèle ------------------------------------------------------------
plot(mod_sap_pH)
pp_check(mod_sap_pH, ndraws = 100)
pp_check(mod_sap_pH, type = 'error_hist',  ndraws = 10)
pp_check(mod_sap_pH, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement

# coefficients -----------------------------------------------------------------
summary(mod_sap_pH)
fixef(mod_sap_pH)
ranef(mod_sap_pH)$p[, , "Intercept"]

# Extraire des échantillons des prostérieures ----------------------------------
post_échan <- as_draws(mod_sap_pH)
summarize_draws(post_échan)

# Create a new data frame for predictions
new_data <- expand.grid(
  p = unique(unlist(d %>% filter(m == "EAU") %>% select(p))),
  s = unique(unlist(d %>% filter(m == "EAU") %>% select(s))),
  e = unique(unlist(d %>% filter(m == "EAU") %>% select(e)))
  )

# Generate posterior predictions
posterior_predictions <- fitted(mod_sap_pH, newdata = new_data)

# Combine posterior predictions with new_data
posterior_df <- cbind(new_data, posterior_predictions)

# Convert to long format for ggplot
posterior_long <- posterior_df %>%
  pivot_longer(cols = starts_with("Estimate"), names_to = "Type", values_to = "pH")

ggplot(posterior_long, aes(x = pH, fill = p)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ p) +
  labs(title = "Posterior Distributions of pH by Producer",
       x = "pH",
       y = "Density") +
  theme_minimal()

ggplot(posterior_long, aes(x = pH, fill = s)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ s) +
  labs(title = "Posterior Distributions of pH by Season",
       x = "pH",
       y = "Density") +
  theme_minimal()

ggplot(posterior_long, aes(x = pH, fill = e)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ e) +
  labs(title = "Posterior Distributions of pH by Environmental Factor",
       x = "pH",
       y = "Density") +
  theme_minimal()

# variation du pH du sirop -----------------------------------------------------
mod_sirop_pH <- brm(
  formula = pH ~ (1 | p) + s * e,
  data = d %>% filter(m == "SIROP") %>% select(p, s, e, pH),
  family = gaussian(),  # Default is Gaussian, so this line is optional
  prior = c(prior(normal(7, 3), class = "Intercept"), # Prior for the intercept
            prior(normal(0, 2), class = "b")),        # Prior for the fixed effects
  iter = 6000,      # Number of iterations
  warmup = 2000,    # Number of warmup iterations
  chains = 4,       # Number of chains
  cores = 4,        # Number of cores to use
  seed = 1259       # Seed for reproducibility
)

# inspect le modèle ------------------------------------------------------------
plot(mod_sirop_pH)
pp_check(mod_sirop_pH, ndraws = 100)
pp_check(mod_sirop_pH, type = 'error_hist',  ndraws = 10)
pp_check(mod_sirop_pH, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement

# coefficients -----------------------------------------------------------------
summary(mod_sirop_pH)
fixef(mod_sirop_pH)
ranef(mod_sirop_pH)$p[, , "Intercept"]

# variation du contenu des solubles totaux (°Brix) -----------------------------
mod_sap_brix <- brm(
  formula = brix ~ (1 | p) + s + e,
  data = d %>% filter(m == "EAU") %>% select(p, s, e, brix),
  family = gaussian(),  # Default is Gaussian, so this line is optional
  prior = c(prior(normal(2, 2), class = "Intercept"), # Prior for the intercept
            prior(normal(0, 2), class = "b")),        # Prior for the fixed effects
  iter = 6000,      # Number of iterations
  warmup = 2000,    # Number of warmup iterations
  chains = 4,       # Number of chains
  cores = 4,        # Number of cores to use
  seed = 1259       # Seed for reproducibility
)

# inspect le modèle ------------------------------------------------------------
plot(mod_sap_brix)
pp_check(mod_sap_brix, ndraws = 100)
pp_check(mod_sap_brix, type = 'error_hist',  ndraws = 10)
pp_check(mod_sap_brix, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement

# coefficients -----------------------------------------------------------------
summary(mod_sap_brix)
fixef(mod_sap_brix)
ranef(mod_sap_brix)$p[, , "Intercept"]

# variation dans la transmittance du sirop (% à 560 nm) ------------------------
mod_trans <- brm(
  formula = transmittance ~ (1 | p) + s + e,
  data = d %>% filter(m == "SIROP") %>% select(p, s, e, transmittance),
  family = gaussian(),  # Default is Gaussian, so this line is optional
  prior = c(prior(normal(35, 10), class = "Intercept"), # Prior for the intercept
            prior(normal(0, 2), class = "b")),        # Prior for the fixed effects
  iter = 6000,      # Number of iterations
  warmup = 2000,    # Number of warmup iterations
  chains = 4,       # Number of chains
  cores = 4,        # Number of cores to use
  seed = 1259       # Seed for reproducibility
)

# inspect le modèle ------------------------------------------------------------
plot(mod_trans)
pp_check(mod_trans, ndraws = 100)
pp_check(mod_trans, type = 'error_hist',  ndraws = 10)
pp_check(mod_trans, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement

# coefficients -----------------------------------------------------------------
summary(mod_trans)
fixef(mod_trans)
ranef(mod_trans)$p[, , "Intercept"]

# variation dans la transmittance du sirop (% à 560 nm) ------------------------
mod_transi <- brm(
  formula = transmittance ~ (1 | p) + s * e,
  data = d %>% filter(m == "SIROP") %>% select(p, s, e, transmittance),
  family = gaussian(),  # Default is Gaussian, so this line is optional
  prior = c(prior(normal(35, 10), class = "Intercept"), # Prior for the intercept
            prior(normal(0, 10), class = "b")),        # Prior for the fixed effects
  iter = 6000,      # Number of iterations
  warmup = 2000,    # Number of warmup iterations
  chains = 4,       # Number of chains
  cores = 4,        # Number of cores to use
  seed = 1259       # Seed for reproducibility
)

# inspect le modèle ------------------------------------------------------------
plot(mod_transi)
pp_check(mod_transi, ndraws = 100)
pp_check(mod_transi, type = 'error_hist',  ndraws = 10)
pp_check(mod_transi, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement

# coefficients -----------------------------------------------------------------
summary(mod_transi)
fixef(mod_transi)
ranef(mod_transi)$p[, , "Intercept"]

# variation dans la conductivité de la sève ------------------------------------
mod_sap_con <- brm(
  formula = conductivité ~ (1 | p) + s + e,
  data = d %>% filter(m == "EAU") %>% select(p, s, e, conductivité),
  family = gaussian(),  # Default is Gaussian, so this line is optional
  prior = c(prior(normal(350, 10), class = "Intercept"), # Prior for the intercept
            prior(normal(0, 2), class = "b")),        # Prior for the fixed effects
  iter = 6000,      # Number of iterations
  warmup = 2000,    # Number of warmup iterations
  chains = 4,       # Number of chains
  cores = 4,        # Number of cores to use
  seed = 1259       # Seed for reproducibility
)

# inspect le modèle ------------------------------------------------------------
plot(mod_sap_con)
pp_check(mod_sap_con, ndraws = 100)
pp_check(mod_sap_con, type = 'error_hist',  ndraws = 10)
pp_check(mod_sap_con, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement

# coefficients -----------------------------------------------------------------
summary(mod_sap_con)
fixef(mod_sap_con)
ranef(mod_sap_con)$p[, , "Intercept"]

# variation dans la conductivité du sirop --------------------------------------
mod_con <- brm(
  formula = conductivité ~ (1 | p) + s + e,
  data = d %>% filter(m == "SIROP") %>% select(p, s, e, conductivité),
  family = gaussian(),  # Default is Gaussian, so this line is optional
  prior = c(prior(normal(350, 10), class = "Intercept"), # Prior for the intercept
            prior(normal(0, 2), class = "b")),        # Prior for the fixed effects
  iter = 6000,      # Number of iterations
  warmup = 2000,    # Number of warmup iterations
  chains = 4,       # Number of chains
  cores = 4,        # Number of cores to use
  seed = 1259       # Seed for reproducibility
)

# inspect le modèle ------------------------------------------------------------
plot(mod_con)
pp_check(mod_con, ndraws = 100)
pp_check(mod_con, type = 'error_hist',  ndraws = 10)
pp_check(mod_con, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement

# coefficients -----------------------------------------------------------------
summary(mod_con)
fixef(mod_con)
ranef(mod_con)$p[, , "Intercept"]

# variation dans la contamination microbienne de la sève -----------------------
mod_micro <- brm(
  formula = CTAM ~ (1 | p) + s + e,
  data = d %>% filter(m == "EAU") %>% select(p, s, e, CTAM),
  family = lognormal(),  # Default is Gaussian, so this line is optional
  prior = c(prior(normal(0, 2), class = "b")),  # Prior for the fixed effects
  control = list(max_treedepth = 12),
  iter = 6000,      # Number of iterations
  warmup = 2000,    # Number of warmup iterations
  chains = 4,       # Number of chains
  cores = 4,        # Number of cores to use
  seed = 1259       # Seed for reproducibility
)

# inspect le modèle ------------------------------------------------------------
plot(mod_micro)
pp_check(mod_micro, ndraws = 100)
pp_check(mod_micro, type = 'error_hist',  ndraws = 10)
pp_check(mod_micro, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement

# coefficients -----------------------------------------------------------------
summary(mod_micro)
fixef(mod_micro)
ranef(mod_micro)$p[, , "Intercept"]

# variation du compte de levures et moissisures dans la sève -------------------
mod_lev <- brm(
  formula = levures ~ (1 | p) + s + e,
  data = d %>% filter(m == "EAU") %>% select(p, s, e, levures),
  family = lognormal(),  # Default is Gaussian, so this line is optional
  prior = c(#prior(normal(350, 10), class = "Intercept"), # Prior for the intercept
    prior(normal(0, 2), class = "b")),        # Prior for the fixed effects
  control = list(max_treedepth = 12),
  iter = 6000,      # Number of iterations
  warmup = 2000,    # Number of warmup iterations
  chains = 4,       # Number of chains
  cores = 4,        # Number of cores to use
  seed = 1259       # Seed for reproducibility
)

# inspect le modèle ------------------------------------------------------------
plot(mod_lev)
pp_check(mod_lev, ndraws = 100)
pp_check(mod_lev, type = 'error_hist',  ndraws = 10)
pp_check(mod_lev, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement

# coefficients -----------------------------------------------------------------
summary(mod_lev)
fixef(mod_lev)
ranef(mod_lev)$p[, , "Intercept"]

# variation du compte de levures et moissisures dans la sève -------------------
mod_ABA <- brm(
  formula = ABA ~ (1 | p) + s + e,
  data = d %>% filter(m == "EAU") %>% select(p, s, e, ABA),
  family = gaussian(),  # Default is Gaussian, so this line is optional
  #prior = c(#prior(normal(350, 10), class = "Intercept"), # Prior for the intercept
  #  prior(normal(0, 10), class = "b")),        # Prior for the fixed effects
  control = list(max_treedepth = 12),
  iter = 6000,      # Number of iterations
  warmup = 2000,    # Number of warmup iterations
  chains = 4,       # Number of chains
  cores = 4,        # Number of cores to use
  seed = 1259       # Seed for reproducibility
)

# inspect le modèle ------------------------------------------------------------
plot(mod_ABA)
pp_check(mod_ABA, ndraws = 100)
pp_check(mod_ABA, type = 'error_hist',  ndraws = 10)
pp_check(mod_ABA, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement

# coefficients -----------------------------------------------------------------
summary(mod_ABA)
fixef(mod_ABA)
ranef(mod_ABA)$p[, , "Intercept"]

#===============================================================================