###################################
# Replication script for          #
# "The International Dimension of #
# Post-Conflict Police Reform"    #
###################################

# Chapter 7 in
# "Institutional Reforms and Peacebuilding. Change, 
# Path-Dependency and Societal Divisions in Post-War 
# Communities" 

# (ed. by Nadine Ansorg & Sabine Kurtenbach)

# Authors: Nadine Ansorg, Felix Haass, Julia Strasheim
# Date: 14. September 2016

# Load libraries & auxiliary functions & clear workspace ------------------

library(texreg)
library(visreg)
library(Cairo)
library(ggplot2)
library(dplyr)

# External function necessary to implement cluster robust standard errors in R
source("./functions/clrobustSE.R")

# Load main data ----------------------------------------------------------

load("./data/police_implementation.rdata")

# Data transformation prior to analysis ----------------------------------

# naming factors
p_comb$unops_fac <- factor(p_comb$unops, labels = c("No PKO", "Trad. PKO", 
                                                    "\n Robust/Multid.\n PKO"))
p_comb$undummy <- factor(p_comb$undummy, labels = c("No PKO", "PKO"))


# Models for Table 7.1 ------------------------------------------------------

# Model 1
m_accstrength <- glm(accstrength ~ 
                       ln_SSRaid_gdp + 
                       unops_fac +
                       Shagov +
                       conflict_intensity +
                       ln_gdp_pc + 
                       fh, 
                     data = p_comb, x = T, y = T, 
                     family = "binomial")

# Model 2
m_accstrength_pc <- glm(accstrength ~ 
                          ln_SSRaid_pc +
                          unops_fac + 
                          Shagov +
                          conflict_intensity +
                          ln_gdp_pc + 
                          fh, 
                        data = p_comb, x = T, y = T, 
                        family = "binomial")

# Model 3
m_accstrength_troops <- glm(accstrength ~ 
                              ln_SSRaid_gdp + 
                              I(total_t1 / 1000) + 
                              Shagov +
                              conflict_intensity +
                              ln_gdp_pc + 
                              fh, 
                            data = p_comb, x = T, y = T, 
                            family = "binomial")

# Model 4
m_accstrength_troops_pc <- glm(accstrength ~ 
                              ln_SSRaid_pc + 
                              I(total_t1 / 1000) + 
                              Shagov +
                              conflict_intensity +
                              ln_gdp_pc + 
                              fh, 
                            data = p_comb, x = T, y = T, 
                            family = "binomial")




# Output Table 1

screenreg(l = list(m_accstrength, m_accstrength_pc, 
                 m_accstrength_troops, m_accstrength_troops_pc),
          symbol = "+",
         # file = "./output/accstrength.models.html",
          stars = c(0.001, 0.01, 0.05, 0.1),
          custom.coef.names = c("Intercept", 
                                "SSR Aid / GDP", 
                                "UN PKO Mandate: Trad.",
                                "UN PKO Mandate: Robust",
                                "Power Sharing",
                                "Conflict Intensity",
                                "GDP p/c", 
                                "Freedom House",
                                "SSR Aid / PC",
                                "PKO Troops"),
          override.se = list(clrobustse(m_accstrength, "CID")$summary[,2],
                             clrobustse(m_accstrength_pc, "CID")$summary[,2],
                             clrobustse(m_accstrength_troops, "CID")$summary[,2],
                             clrobustse(m_accstrength_troops_pc, "CID")$summary[,2]),
          override.pval = list(clrobustse(m_accstrength, "CID")$summary[,4],
                             clrobustse(m_accstrength_pc, "CID")$summary[,4],
                             clrobustse(m_accstrength_troops, "CID")$summary[,4],
                             clrobustse(m_accstrength_troops_pc, "CID")$summary[,4]),
          
          omit.coef = "Intercept",
          reorder.coef = c(1, 8, 2,3, 9, 4, 5, 6, 7),
          caption = "", 
          bold = 0.1,
          custom.note = "%stars, Standard errors in parentheses, clustered by conflict")


# Models for Table 7.2 ------------------------------------------------------

# Model 1 (General)
m_comp_general <- glm(comp_general ~ 
                       ln_SSRaid_gdp + 
                       unops_fac + 
                       Shagov +
                       conflict_intensity +
                       ln_gdp_pc + 
                       fh, data = p_comb, x = T, y = T, 
                      family = "binomial")

# Model 2 (Warring Parties)
m_comp_warring<- glm(comp_warring ~ 
                           ln_SSRaid_gdp + 
                           unops_fac + 
                           Shagov +
                           conflict_intensity +
                           ln_gdp_pc + 
                           fh, data = p_comb, x = T, y = T, 
                     family = "binomial")

# Model 3 (Identity Groups)
m_comp_identity <- glm(comp_identity ~ 
                       ln_SSRaid_gdp + 
                       unops_fac + 
                       Shagov +
                       conflict_intensity +
                       ln_gdp_pc + 
                       fh, data = p_comb, x = T, y = T, 
                       family = "binomial")

# Model 4 (Gender)
m_comp_gender <- glm(comp_gender ~ 
                         ln_SSRaid_gdp + 
                         unops_fac + 
                         Shagov +
                         conflict_intensity +
                         ln_gdp_pc + 
                         fh, data = p_comb, x = T, y = T, 
                     family = "binomial")


# Output Table 2

screenreg(l = list(m_comp_general, m_comp_warring, 
                   m_comp_identity, m_comp_gender),
          symbol = "+",
       #   file = "./output/comp_general.models.html",
          stars = c(0.001, 0.01, 0.05, 0.1),
          custom.coef.names = c("Intercept", 
                                "SSR Aid / GDP", 
                                "UN PKO Mandate: Trad.",
                                "UN PKO Mandate: Robust",
                                "Power Sharing",
                                "Conflict Intensity",
                                "GDP p/c", 
                                "Freedom House"),
          #      reorder.coef = c(1, 3, 4, 10, 2, 5:9),
          override.se = list(clrobustse(m_comp_general, "CID")$summary[,2],
                             clrobustse(m_comp_warring, "CID")$summary[,2],
                             clrobustse(m_comp_identity, "CID")$summary[,2],
                             clrobustse(m_comp_gender, "CID")$summary[,2]),
          override.pval = list(clrobustse(m_comp_general, "CID")$summary[,4],
                               clrobustse(m_comp_warring, "CID")$summary[,4],
                               clrobustse(m_comp_identity, "CID")$summary[,4],
                               clrobustse(m_comp_gender, "CID")$summary[,4]),
          omit.coef = "Intercept",
          caption = "", 
          bold = 0.1,
          custom.note = "%stars, Standard errors in parentheses, clustered by conflict",
          custom.model.names = c("(1) General", " (2) Warring Parties",
                                 "(3) Identity Groups", "(4) Gender"))
          

# Plots -------------------------------------------------------------------


# Figure 7.1 ----------------------------------------------------------------

# load aid & pko data
load("./data/aiddataSSRaid.rdata")
load("./data/DS2006pko.rdata")


# Plot SSR aid development over time
plot_ssr_trend <- aid_agg %>% group_by(Year) %>% 
  summarise(aid = mean(SSRshare) * 100) %>% 
  filter(Year > 1975) %>% 
  ggplot(aes(x = Year, y = aid)) + geom_line() +
  theme_bw() +
  scale_x_continuous(breaks = seq(1975,2010,5)) +
  labs(y = "Average annual share \n of SSR aid as % of Total Aid", x = "")


ggsave(plot_ssr_trend, filename = "./figures/ssr_trend.png", width = 8, height = 5,
       type = "cairo-png", units = "in", scale = 0.7)

# Plot PKO Trend over time
plot_pko_trend <- pko %>% 
  mutate(strongdummy = ifelse(DS_ordinal >= 3, 1, 0)) %>% 
  filter(strongdummy == 1) %>% 
  filter(Year >= 1975) %>% 
  group_by(Year) %>% summarise(n = n()) %>%
  left_join(data.frame(Year = 1975:2010), .) %>% # add empty years
  mutate(n = ifelse(is.na(n), 0, n)) %>% # replace NAs in years with zero
  ggplot(aes(x = Year, y = n)) + geom_line() +
  theme_bw() +
  scale_x_continuous(breaks = seq(1975,2010,5)) +
  scale_y_continuous(breaks = seq(0,8,1)) +
  labs(y = "Annual count of UN PKOs with\n robust/multidimensional mandate ", x = "")


ggsave(plot_pko_trend, filename = "./figures/pko_trend.png", width = 8, height = 5,
       type = "cairo-png", units = "in", scale = 0.7)


# Figure 7.2 ----------------------------------------------------------------


# Plot model results: political control / accountability
CairoPNG(filename = "./figures/model_output.png", width = 16, height = 5,
         dpi = 300, units = "in", pointsize = 21)
par(mfrow = c(1,3))

visreg(m_accstrength, xvar = "ln_SSRaid_gdp", 
       type = "conditional", scale = "response", 
       ylab = "P(Implement. of account. reforms)", 
       xlab = "SSR Aid as % of GDP (log)",
       line = list(col = "black"))

visreg(m_accstrength, xvar = "unops_fac", 
       type = "conditional", scale = "response", 
       ylab = "P(Implement. of account. reforms)", 
       xlab = "Extensiveness of UN PKO mandate",
       line = list(col = "black"))

visreg(m_accstrength, xvar = "fh", 
       type = "conditional", scale = "response", 
       ylab = "P(Implement. of account. reforms)", 
       xlab = "Freedom House score",
       line = list(col = "black"))

par(mfrow = c(1,1))
dev.off()



# Figure 7.3 ----------------------------------------------------------------

# Plot model results: composition
CairoPNG(filename = "./figures/model_output_comp.png", width = 10, height = 10,
         dpi = 200, units = "in", pointsize = 15)
par(mfrow = c(2,2))

visreg(m_comp_general, xvar = "unops_fac",
       main = "General",
       type = "conditional", scale = "response", 
       ylab = "P(Implement. of account. reforms)", 
       xlab = "",
       line = list(col = "black"))

visreg(m_comp_warring, xvar = "unops_fac",
       main = "Warring parties",
       type = "conditional", scale = "response", 
       ylab = "P(Implement. of account. reforms)", 
       xlab = "",
       line = list(col = "black"))

pred_models <- visreg(m_comp_identity, xvar = "unops_fac",
                      main = "Identity groups",
                      type = "conditional", scale = "response", 
                      ylab = "P(Implement. of account. reforms)", 
                      xlab = "",
                      line = list(col = "black"))

plot_data <- visreg(m_comp_gender, xvar = "unops_fac",
                    main = "Gender",
                    type = "conditional", scale = "response", 
                    ylab = "P(Implement. of account. reforms)", 
                    xlab = "",
                    line = list(col = "black"))

par(mfrow = c(1,1))
dev.off()
