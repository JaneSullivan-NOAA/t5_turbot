# Tier 5 version of the Greenland turbot assessment

# set up ----
libs <- c("dplyr", "tidyr", "readr", "here", "ggplot2")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

# devtools::install_github("afsc-assessments/rema")
library(rema)

ggplot2::theme_set(cowplot::theme_cowplot(font_size = 10) +
                     cowplot::background_grid() +
                     cowplot::panel_border())

yr <- 2024 # assessment yr
dir.create(here(yr))
dir.create(here(yr, "results"))

# analysis ----

bts <- read_csv(here(yr, "data", "raw", "bts_biomass_turbot.csv")) %>% 
  select(strata = survey, year, biomass, cv)
lls <- read_csv(here(yr, "data", "raw", "lls_turbot.csv")) %>% 
  # choice to be made here about whether to use RPNs or RPWs. I went with RPNs
  # because that's what are currently used
  mutate(strata, year, cpue = rpn, cv = rpn_cv)
  # mutate(strata, year, cpue = rpw, cv = rpw_cv)

sort(unique(bts$strata))
input <- prepare_rema_input(model_name = 'Tier 5 turbot',
                            multi_survey = 1, # this is 1 because we want to use the LLS information
                            biomass_dat = bts, 
                            cpue_dat = lls,
                            start_year = 1991,
                            end_year = yr+1,
                            sum_cpue_index = 1, # this is 1 because LLS RPN/RPWs are summable 
                            # specify shared/pooled process error across the 3
                            # strata (default is c(1,2,3))
                            PE_options = list(pointer_PE_biomass = c(1, 1, 1)),
                            # LLS only available in AI and on EBS slope.
                            # estimate separate q's for these and specify EBS
                            # shelf as NA
                            q_options = list(pointer_biomass_cpue_strata = c(1, NA, 2)))
mod1 <- fit_rema(input)
mod1
saveRDS(mod1, here::here(yr, "results", "t5_turbot.rds"))
out <- tidy_rema(mod1)
out

# create empty placeholder so that strata plot panels are aligned for the
# BTS/LLS
out$cpue_by_strata <- out$cpue_by_strata %>% 
  bind_rows(data.frame(strata = "EBS Shelf",
                       year = unique(out$cpue_by_strata$year)))
plots <- plot_rema(out, cpue_ylab = 'RPN', biomass_ylab = 'Biomass (t)')
p1 <- plots$biomass_by_strata + facet_wrap(~strata, nrow = 1) + ggtitle('Bottom trawl survey')
p2 <- plots$cpue_by_strata + facet_wrap(~strata, nrow = 1) + ggtitle('Longline survey')
p3 <- plots$total_predicted_biomass + 
  scale_y_continuous(labels = scales::comma, expand = c(0.02, 0), limits = c(0, NA)) +
  ggtitle("Total predicted biomass")
cowplot::plot_grid(p1, p2, p3, ncol = 1)
ggsave(here(yr, "results", "t5_turbot.png"), units = 'in', bg = 'white',
       height = 9.5, width = 8, dpi = 300)

natmat <- 0.112 # from 2024 assessment
specs <- out$total_predicted_biomass %>% 
  filter(year == yr+1) %>% 
  mutate(ofl = round(natmat * pred, 0),
         abc = round(natmat * 0.75 * pred, 0))
glimpse(specs)
# $ model_name <chr> "Tier 5 turbot"
# $ variable   <chr> "tot_biomass_pred"
# $ year       <int> 2025
# $ pred       <dbl> 14776.7
# $ pred_lci   <dbl> 8848.229
# $ pred_uci   <dbl> 24677.36
# $ ofl        <dbl> 1655
# $ abc        <dbl> 1241

# 2025 Tier specs from the 2024 assessment:
# OFL = 2598 t
# maxABC = 2237 t
# ABC = 2013 t

# Apportionment -----

# Maybe base appo on RPW instead of RPN because its weight-based (that's what is
# done for other stocks)
lls <- read_csv(here(yr, "data", "raw", "lls_turbot.csv")) %>% 
  mutate(strata, year, cpue = rpw, cv = rpw_cv)

sort(unique(bts$strata))
input <- prepare_rema_input(model_name = 'Turbot Apportionment',
                            multi_survey = 1, # this is 1 because we want to use the LLS information
                            biomass_dat = bts, 
                            cpue_dat = lls,
                            start_year = 1991,
                            end_year = yr+1,
                            sum_cpue_index = 1, # this is 1 because LLS RPN/RPWs are summable 
                            # specify shared/pooled process error across the 3
                            # strata (default is c(1,2,3))
                            PE_options = list(pointer_PE_biomass = c(1, 1, 1)),
                            # LLS only available in AI and on EBS slope.
                            # estimate separate q's for these and specify EBS
                            # shelf as NA
                            q_options = list(pointer_biomass_cpue_strata = c(1, NA, 2)))
mod2 <- fit_rema(input)
mod2
saveRDS(mod2, here::here(yr, "results", "appo_turbot.rds"))
out <- tidy_rema(mod2)
out
# apportionment:  last 10 years (have to combine EBS Strata)
out$proportion_biomass_by_strata 
appo <- out$proportion_biomass_by_strata %>% 
  mutate(EBS = `EBS Shelf` + `EBS Slope`) %>% 
  select(-`EBS Shelf`, -`EBS Slope`)

appo  %>% tail(10) # last 10 yr
# average proportion predicted biomass in the AI over the last 10 yr
appo  %>% tail(10) %>% summarise(mean(AI)) 

appo %>% 
  dplyr::filter(year > max(year) - 10) %>% 
  tidyr::pivot_longer(cols = -c(model_name, year)) %>% 
  ggplot(aes(x = factor(year), y = value,
             fill = reorder(name, (value)))) + 
  geom_bar(position = "stack", stat = "identity") + 
  scale_fill_brewer(palette = "Greys") + 
  labs(x = NULL, y = NULL, fill = NULL, title = "Proportion biomass by strata") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0)))

ggsave(here(yr, "results", "appo_turbot.png"), units = 'in', bg = 'white',
       height = 4, width = 6, dpi = 300)
