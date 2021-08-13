## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE
)

## ----setup--------------------------------------------------------------------
library(LogRegEquiv)

## ----model construction, echo=TRUE--------------------------------------------
formula <- "final_fail ~ ."
female_model <- glm(formula = formula, family = binomial(link = "logit"),
                    data = ptg_stud_f_train)
male_model <- glm(formula = formula, family = binomial(link = "logit"),
                    data = ptg_stud_m_train)

## ----beta equivalence, echo=TRUE----------------------------------------------
delta_beta <- 0.1
print(beta_equivalence(model_a = female_model,
                       model_b = male_model,
                       delta = delta_beta,
                       alpha = 0.05))

## ----coef vector equivalence, echo=TRUE---------------------------------------
print(descriptive_equiv(data_a = ptg_stud_f_train,
                              data_b = ptg_stud_m_train, 
                              formula = formula,
                              delta = delta_beta,
                              alpha = 0.05))

## ----beta vectorial delta, echo=TRUE------------------------------------------
set.seed(1)
delta_beta_vec <- 0.01 * runif(39)
print(beta_equivalence(model_a = female_model,
                       model_b = male_model,
                       delta = delta_beta_vec,
                       alpha = 0.05))

## ----individual predictive equivalence female test, echo=TRUE-----------------
r <- 0.05
print(individual_predictive_equiv(model_a = female_model,
                  model_b = male_model,
                  test_data = ptg_stud_f_test,
                  r = r,
                  alpha = 0.05))

## ----individual predictive equivalence male test, echo=TRUE-------------------
r <- 0.025
print(individual_predictive_equiv(model_a = female_model,
                  model_b = male_model,
                  test_data = ptg_stud_m_test,
                  r = r,
                  alpha = 0.05))

## ----performance equivalence female test, echo=TRUE---------------------------
testing_data <- ptg_stud_m_test
delta_b <- 1 / (2 * sqrt(nrow(testing_data)))
print(performance_equiv(model_a = female_model, 
                        model_b = male_model, 
                        test_data = testing_data,
                        dv_index = 30,
                        t = delta_b,
                        alpha = 0.05))


print(performance_equiv(model_a = female_model, 
                        model_b = male_model, 
                        test_data = ptg_stud_f_test,
                        dv_index = 30,
                        t = 0.1,
                        alpha = 0.05))

