## load packages ----
library(tidymodels)
library(tidyverse)
library(car)
library(splines)
library(ggthemes)
library(mgcv)

# handle conflicts
tidymodels_prefer()

# read in data ----
states_data <- read.table("data/States.txt") |> 
  as_tibble() |> 
  # relocate outcome variable for ease in reading graphics
  relocate(satMath,.after = everything()) |> 
  janitor::clean_names()

# quick look at data ----
GGally::ggpairs(states_data)

states_data |> 
  skimr::skim_without_charts()

states_data |> 
  skimr::skim_without_charts()

states_data |> 
  # standardize (z-score) numerical vars
  mutate(across(where(is.numeric), \(x) (x - mean(x)) / sd(x))) |> 
  skimr::skim_without_charts()

# states_data |> 
#   naniar::miss_var_summary()
# 
# states_data |> 
#   naniar::gg_miss_var()

# Work on models -----

# Part (a) ----



# Part (b) ----
model_b1 <- loess(sat_math ~ population + percent_taking + teacher_pay, 
                  data = states_data,
                  degree = 1)

model_b2 <- loess(sat_math ~ population + percent_taking + teacher_pay, 
                  data = states_data,
                  degree = 2)

model_b3 <- loess(sat_math ~ percent_taking, 
                  data = states_data,
                  degree = 1)

model_b4 <- loess(sat_math ~ percent_taking, 
                  data = states_data,
                  degree = 2,
                  )

anova(model_b1, model_b2, model_b3, model_b4)


model_gam1 <- gam(sat_math ~ s(population) + s(percent_taking) + s(teacher_pay),
                  data = states_data) 
          #the s() structure for the covariates is a type of spline, we could use other types of splines


model_gam2 <- gam(sat_math ~ s(population) + s(percent_taking) + s(teacher_pay),
                  data = states_data)

              #another potential model struture: natural spline, follows a syntax like this
              # or something similar like that
              lm( y ~ ns(x, number))
              
    
### EXAMINING PREDICTIONS
              
      ## Extract the
      mod_a_predobs <- tibble(
        .pred = model_a$fitted.values,
        sat_math = model_a$model$sat_math
      ) 
      
      
      # rmse _ these are all from the yardstick package
      mod_a_predobs %>% 
        rmse(sat_math, .pred)
      
      #r-squared
      mod_a_predobs %>% 
        rsq(sat_math, .pred)
        
      #model average error
      mod_a_predobs %>% 
        mae(sat_math, .pred)

      
      my_metrics <-  metric_set(rmse, mae, rsq, mape)

      mod_a_predobs %>% 
        my_metrics(sat_math, .pred)
      # this will create the desired metrics for the model that are listed in the my_metrics object above. 
      # Makes the process easier, faster
      
      ## Plots
      ggplot(mod_a_predobs, aes(sat_math, .pred)) +
               geom_point(alpha = .4, size = 2) +
              geom_abline(linetype = "dashed") +
              geom_smooth(se = F)
              coord_obs_pred() +
              theme_minimal(base_size = 14)
      
      
      
      