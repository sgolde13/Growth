####################################################################
####################################################################
## Outline
##
## 1)  Packages and Setup
## 2)  Data Preparation
## 3)  Exploratory Data Analysis
##       a) Tables
##       b) Plots
## 4)  Models tested
## 5)  Multicollinearity
## 6)  Stepwise selection
## 7)  All possible
## 8)  Hypothesis testing
##       a) Partial F_0 Test
##       b) Plots
## 9)  Residuals
## 10) Cross validation



# Notes
# Multicolinearity PDF pg 142, 249-250, Chp 9

####################################################################
####################################################################
## 1) Packages and Setup
library("readxl")
library("tidyverse")
library("ggplot2")
library("ggpubr")
library("smplot2")
library("moments")
library("RColorBrewer")
library("wesanderson")
library("car")
library("reshape2")
library("ggcorrplot")
library("mctest")
library("olsrr")
library("regclass")


"%!in%" <- function(x,y)!('%in%'(x,y))



####################################################################
####################################################################
## 2) Data Preparation
df <- read_excel("Growth.xlsx", 
                 col_types = c("text", "logical", "logical", "numeric", 
                               "numeric", "numeric", "numeric", "numeric", 
                               "numeric", "numeric")) %>% as.data.frame()

df$development_status <- ifelse(df$development_status %in% TRUE, 1, 0)
df$least_developed <- ifelse(df$least_developed %in% TRUE, 1, 0)


####################################################################
####################################################################
## 3) Exploratory Data Analysis

##########################
##########################
## 3a) Tables

# Check that all countries are represented once
all(table(df$country_name) == 1)

# All countries did not have half their exports from oil in 1960
table(df$oil)

# Development status
table(df$development_status)
table(df$least_developed)

tapply(df$rgdp60, df$development_status, summary)

# Political stability
tapply(df$rev_coups, df$development_status, summary)
tapply(df$assasinations, df$development_status, summary)


not_developed_df = df[df$development_status %in% "0", ]
developed_df = df[df$development_status %in% "1", ]
least_df = df[df$least_developed %in% "1", ]

tapply(not_developed_df$rev_coups, not_developed_df$least_developed, summary)
tapply(not_developed_df$assasinations, not_developed_df$least_developed, summary)

summary(developed_df$rev_coups)
summary(not_developed_df[not_developed_df$least_developed %in% "1", ]$rev_coups)


df[df$rev_coups == 0 & df$assasinations == 0, ] %>% nrow()
developed_df[developed_df$rev_coups == 0 & developed_df$assasinations == 0, ] %>% nrow()
least_df[least_df$rev_coups == 0 & least_df$assasinations == 0, ] %>% nrow()

# GDP
tapply(df$rgdp60, df$development_status, summary)
tapply(not_developed_df$rgdp60, not_developed_df$least_developed, summary)

# Summary statistics
summary(df)

summary(df$tradeshare)
summary(df$yearsschool)

# Moments of y to measure normal shape
apply(df[, names(df) %!in% c("country_name", "development_status", "least_developed", "oil")], 2, skewness)
apply(df[, names(df) %!in% c("country_name", "development_status", "least_developed", "oil")], 2, kurtosis)



##########################
##########################
## 3b) Plots

# Choosing color palette
names(wes_palettes)
wes_palette("AsteroidCity1", type = "discrete")

my_colors <- wes_palette("AsteroidCity1", type = "discrete")[c(1, 5)]


# Scatterplots showing the relationships between Growth and Regressors
# Change the x by names(df)
ggplot(df, aes(x = rgdp60, y = growth, by = as.character(development_status), 
               color = as.character(development_status))) +
  geom_point() + theme_minimal() +
  theme(legend.position = "bottom") +
  scale_colour_manual(values = my_colors, labels = c("No", "Yes")) +
  labs(title = "1960 GDP by Percentage Growth", 
       x ="1960 GDP", y = "Percentage Growth", fill = "Developed?", colour = "Developed?",
       caption = "")

ggplot(df, aes(x = tradeshare, y = growth, by = as.character(development_status), 
               color = as.character(development_status))) +
  geom_point() + theme_minimal() +
  theme(legend.position = "bottom") +
  scale_colour_manual(values = my_colors, labels = c("No", "Yes")) +
  labs(title = "Trade Share by Percentage Growth", 
       x ="Trade Share", y = "Percentage Growth", fill = "Developed?", colour = "Developed?",
       caption = "")

ggplot(df, aes(x = yearsschool, y = growth, by = as.character(development_status), 
               color = as.character(development_status))) +
  geom_point() + theme_minimal() +
  theme(legend.position = "bottom") +
  scale_colour_manual(values = my_colors, labels = c("No", "Yes")) +
  labs(title = "Years in School by Percentage Growth", 
       x ="Years of School for Adults in 1960", y = "Percentage Growth", 
       fill = "Developed?", colour = "Developed?",
       caption = "")


# Correlation plot with Revolutions and Political Asssinations
ggplot(df, aes(x = rev_coups, y = assasinations)) +
  geom_point() +
  sm_statCorr(corr_method = "spearman", 
              colour = wes_palette("AsteroidCity1", type = "discrete")[4]) +
  #stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  #stat_cor(method = "spearman", label.x = 0, label.y = 2.2) +
  theme_minimal() +
  labs(title = "Correlation Between Assasinations and Revolts", 
       x ="Revolutions/Coups\n", y = "Assasinations",
       caption = "Correlation calculated using the Spearman method")


with_unrest = df[df$rev_coups %!in% 0 & df$assasinations %!in% 0, ]

ggplot(df, aes(x = rev_coups, y = assasinations, 
                        by = as.character(development_status), 
                        color = as.character(development_status))) +
  geom_point() +
  sm_statCorr(corr_method = "spearman") +
  #stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  #stat_cor(method = "spearman", label.x = 0, label.y = 2.2) +
  theme_minimal() +
  theme(legend.position="bottom") +
  scale_colour_manual(values = my_colors, labels = c("No", "Yes")) +
  labs(title = "Correlation Between Assasinations and Revolts", 
       x ="Revolutions/Coups", y = "Assasinations", 
       fill = "Developed?", colour = "Developed?",
       caption = "Correlation calculated using the Spearman method")


# GDP and Percentage Growth Histograms
ggplot(df, aes(x = rgdp60, group = as.character(development_status), 
               colour = as.character(development_status), 
               fill = as.character(development_status))) + 
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_histogram(aes(y=..density..), alpha = 0.6) +
  geom_density(alpha = 0.3) +
  scale_colour_manual(values = my_colors, labels = c("No", "Yes")) +
  scale_fill_manual(values = my_colors, labels = c("No", "Yes")) +
  geom_vline(xintercept = tapply(df$rgdp60, df$development_status, summary)[['0']]["Mean"],
             color = my_colors[1], size = 1.1) +
  geom_vline(xintercept = tapply(df$rgdp60, df$development_status, summary)[['1']]["Mean"],
             color = my_colors[2], size = 1.1) +
  labs(title = "Gross Domestic Product (GDP) in 1960 Density Plot", 
       x ="1960 GDP", y = "Density", fill = "Developed?", colour = "Developed?",
       caption = "")


ggplot(df, aes(x = growth, group = as.character(development_status), 
               colour = as.character(development_status), 
               fill = as.character(development_status))) + 
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_histogram(aes(y = ..density..), alpha = 0.6) +
  geom_density(alpha = 0.3) +
  scale_colour_manual(values = my_colors, labels = c("No", "Yes")) +
  scale_fill_manual(values = my_colors, labels = c("No", "Yes")) +
  geom_vline(xintercept = tapply(df$growth, df$development_status, summary)[['0']]["Mean"],
             color = my_colors[1], size = 1.1) +
  geom_vline(xintercept = tapply(df$growth, df$development_status, summary)[['1']]["Mean"],
             color = my_colors[2], size = 1.1) +
  labs(title = "Average Annual Percentage GDP Growth", 
       x ="Percentage Growth", y = "Density", fill = "Developed?", colour = "Developed?",
       caption = "")

ggplot(df, aes(x = growth)) + 
  theme_minimal() +
  theme(legend.position = "none") +
  geom_histogram(aes(y = ..density..), colour = wes_palette("AsteroidCity1", type = "discrete")[4], 
                 fill = wes_palette("AsteroidCity1", type = "discrete")[4], 
                 alpha = 0.6) +
  geom_density(alpha = 0.3) +
  labs(title = "Average Annual Percentage GDP Growth", 
       x ="Percentage Growth", y = "Density",
       caption = "")



####################################################################
####################################################################
## 4) Models tested

# Subset columns for the model evaluation by hand
# Necessary for the matrix calculations of the OLS result
df[, c("dev_60", "dev_school", "dev_rev", "dev_as")] = 
  data.frame(as.numeric(df$development_status) * df$rgdp60, 
             as.numeric(df$development_status) * df$yearsschool,
             as.numeric(df$development_status) * df$rev_coups,
             as.numeric(df$development_status) * df$assasinations)

df[, c("least_60", "least_school", "least_rev", "least_as")] = 
  data.frame(as.numeric(df$least_developed) * df$rgdp60, 
             as.numeric(df$least_developed) * df$yearsschool,
             as.numeric(df$least_developed) * df$rev_coups,
             as.numeric(df$least_developed) * df$assasinations)

df[, "school_sq"] <- df$yearsschool^2


# variables in each model
full_var = c("rgdp60", "tradeshare", "yearsschool", "rev_coups", "assasinations", 
             "development_status", "least_developed", "dev_60", "dev_school", 
             "dev_rev", "dev_as", "least_60", "least_school", "least_rev", "least_as")


model_a_var = c("rgdp60", "tradeshare", "yearsschool", "rev_coups", "development_status", 
                "least_developed", "dev_60", "dev_school", "least_rev")


model_b1_var = c("rgdp60", "tradeshare", "yearsschool", "rev_coups",
                 "dev_rev", "dev_60", "school_sq")
model_b2_var = c("rgdp60", "tradeshare", "yearsschool", "school_sq", "rev_coups",
                 "least_developed", "dev_60")


model_c1_var = c("rgdp60", "tradeshare", "yearsschool", "school_sq", "rev_coups")
model_c2_var = c("rgdp60", "tradeshare", "rev_coups", "least_developed")
model_c3_var = c("tradeshare", "yearsschool", "school_sq", "rev_coups", "least_developed")


# From the initial model, the following models were tried
# These are listed in order of trial
initial_model = lm(str_c("growth", sep = " ~ ", str_c(full_var, collapse = " + ")), 
                   data = df)

model_a = lm(str_c("growth", sep = " ~ ", str_c(model_a_var, collapse = " + ")), 
             data = df)


model_b1 = lm(str_c("growth", sep = " ~ ", str_c(model_b1_var, collapse = " + ")), 
              data = df)
model_b2 = lm(str_c("growth", sep = " ~ ", str_c(model_b2_var, collapse = " + ")), 
              data = df)


model_c1 = lm(str_c("growth", sep = " ~ ", str_c(model_c1_var, collapse = " + ")), 
              data = df)
model_c2 = lm(str_c("growth", sep = " ~ ", str_c(model_c2_var, collapse = " + ")), 
              data = df)
model_c3 = lm(str_c("growth", sep = " ~ ", str_c(model_c3_var, collapse = " + ")), 
              data = df)



####################################################################
####################################################################
## 5) Multicollinearity

# improve matrix conditioning by centering the design matrix, X
center_df = sapply(df[, -1], function(x) x - mean(x)) %>% as.data.frame()

centered_model = lm(str_c("growth", sep = " ~ ", str_c(model_c2_var, collapse = " + ")), 
                    data = center_df)

# Variance decomposition proportions 
# MVP PDF pg 325
eigprop(centered_model)

# Correlation matrix
# MVP PDF pg 317
correlation_matrix <- function(data, model_var){
  data[, model_var] <- apply(data[, model_var], 2, as.numeric)
 
  X = data[, model_var] %>% `colnames<-`(NULL) %>% `rownames<-`(NULL) %>% as.matrix()
  
  MC = X %>% `colnames<-`(model_var) %>% cor() %>% as.data.frame() 
  
  MC
}

cor_mat = correlation_matrix(df, model_c2_var)

ggcorrplot(cor_mat, hc.order = TRUE, outline.col = "white", type = "lower",
           ggtheme = ggplot2::theme_minimal,
           colors = c(wes_palette("AsteroidCity1", type = "discrete")[1],
                      "white", wes_palette("AsteroidCity1", type = "discrete")[3]),
           lab = TRUE,
           title = "Correlation Matrix - Model C3")
           #lab = TRUE, lab_size = 2)  # adds correlation value on squares



####################################################################
####################################################################
## 6) Stepwise selection

# Model setup for the step() process
# MVP PDF pg 376
min_model = lm("growth ~ 1", data = df)
max_model = lm(str_c("growth", sep = " ~ ", str_c(model_b2_var, collapse = " + ")), 
               data = df)


step(min_model, criteria = "AIC", direction = "forward", 
     scope = list(upper = max_model, lower = ~1)) %>% summary()

step(max_model, criteria = "AIC", direction = "backward") %>% summary()

step(min_model, criteria = "AIC", direction = "both", 
     scope = list(upper = max_model, lower = ~1)) %>% summary()



####################################################################
####################################################################
## 7) All possible

# MVP PDF pg 370
all = ols_step_all_possible(model_c3)$result[, c("mindex", "n", 
                                                 "predictors", "rmse", 
                                                 "cp", "aic")]

all[, 4:6] <- apply(all[, 4:6], 2, function(x) round(x, digits = 2))

all[order(all$rmse, decreasing = TRUE), ] %>% tail()
all[order(all$cp, decreasing = TRUE), ] %>% tail()
all[order(all$aic, decreasing = TRUE), ] %>% tail()



####################################################################
####################################################################
## 8) Hypothesis testing

# OLS by hand
ols_matrices <- function(X_var, y_var, data){
  X = cbind(rep(1, nrow(data)), data[, X_var]) %>% 
    `colnames<-`(NULL) %>% `rownames<-`(NULL) %>% as.matrix()
  
  y = data[, y_var] %>% `colnames<-`(NULL) %>% `rownames<-`(NULL) %>% as.matrix()
  
  inv_Xsq = t(X) %*% X %>% solve()
  Xy = t(X) %*% y
  
  beta_hat = inv_Xsq %*% Xy
  
  
  # Analysis of Variance table elements by hand
  p = ncol(X) - 1
  n = nrow(X)
  
  SS_R = t(beta_hat) %*% Xy - (sum(y)^2 / n)
  SS_Res = t(y) %*% y - t(beta_hat) %*% Xy
  SS_T = t(y) %*% y - (sum(y)^2 / n)
  
  MS_R = SS_R/p
  MS_Res = SS_Res/(n - p - 1)
  
  F_0 = MS_R/MS_Res
  
  result <- list(X, y, inv_Xsq, Xy, beta_hat, p, n, SS_R, SS_Res, SS_T, MS_R, MS_Res, F_0)
  names(result) <- c("X", "y", "inv_Xsq", "Xy", "beta_hat", "p", "n", 
                     "SS_R", "SS_Res", "SS_T", "MS_R", "MS_Res", "F_0")
  
  result
}


##########################
##########################
## 8a) Partial F_0 Test

# H_0: beta_2 = 0 and H_1: beta_2 != 0
# MVP PDF pg. 108
partial_F_test <- function(X1_var, X2_var, y_var, data, alpha){
  results_X1 <- ols_matrices(X1_var, y_var, df)
  results_X2 <- ols_matrices(X2_var, y_var, df)
  
  # Check X’s are orthogonal
  orthogonal <- t(results_X2$X[, -1]) %*% results_X1$X[, -1]
  
  # S_R(beta 2 given beta 1)
  p = ncol(results_X1$X) + ncol(results_X2$X) - 2
  r = ncol(results_X2$X) - 1
  n = nrow(results_X1$X)
  
  S_R_beta   = ols_matrices(c(X1_var, X2_var), "growth", df)$SS_R
  S_R_beta_1 = ols_matrices(X1_var, "growth", df)$SS_R
  
  SS_R_beta_2 = S_R_beta - S_R_beta_1
  partial_F_0 = (SS_R_beta_2/r) / ols_matrices(c(X1_var, X2_var), "growth", df)$MS_R
  
  F_sig = qf(alpha, df1 = r, df2 = (n-p), lower.tail=FALSE)
  
  # Reject H_0 if F_0 > F_sig
  rejection = partial_F_0 > F_sig
  
  
  result <- list(orthogonal, p, r, n, S_R_beta, S_R_beta_1, SS_R_beta_2, 
                 partial_F_0, F_sig, rejection)
  names(result) <- c("orthogonal", "p", "r", "n", "SS_R (beta)", "SS_R (beta_1)", 
                     "SS_R (beta_2 given beta_1)", "F_0", "F_sig", "Reject Null?")
  
  result
}


# Model B2
partial_F_test(model_b2_var[c(1, 3:7)], model_b2_var[c(2)], "growth", df, 0.05)
partial_F_test(model_b2_var[c(1:3, 5:7)], model_b2_var[c(4)], "growth", df, 0.05)
partial_F_test(model_b2_var[c(1:6)], model_b2_var[c(7)], "growth", df, 0.05)

# Model C1
partial_F_test(model_c1_var[c(1, 3:5)], model_c1_var[2], "growth", df, 0.05)
partial_F_test(model_c1_var[c(2:5)], model_c1_var[c(1)], "growth", df, 0.05)
partial_F_test(model_c1_var[c(1:2, 5)], model_c1_var[c(3:4)], "growth", df, 0.05)

partial_F_test(model_c1_var[c(2, 5)], model_c1_var[c(1, 3:4)], "growth", df, 0.05)

# Model C2
partial_F_test(model_c2_var[c(2:4)], model_c2_var[1], "growth", df, 0.05)
partial_F_test(model_c2_var[c(1:2, 4)], model_c2_var[c(3)], "growth", df, 0.05)

# Model C3
partial_F_test(model_c3_var[c(1:2, 4:5)], model_c3_var[c(3)], "growth", df, 0.05)



####################################################################
####################################################################
## 9) Residuals

plot_residuals <- function(X_var, y_var, data){
  fit <- ols_matrices(X_var, y_var, data)
  
  # Hat box
  H = fit$X %*% fit$inv_Xsq %*% t(fit$X) %>% as.matrix()
  h_ii = diag(H)
  
  
  # External studentized residuals
  y_hat = fit$X %*% fit$beta_hat
  e = fit$y - y_hat
  
  numerator = ( (fit$n - fit$p) * as.vector(fit$MS_Res) ) - ( as.vector(fit$MS_Res)^2/(1 - h_ii) )
  S = numerator/(fit$n - fit$p - 1)
  
  t = e/( S * (1 - h_ii) )
  
  
  # Studentized residuals
  # MVP PDF pg. 153
  r = e/sqrt(as.vector(fit$MS_Res) * (1 - h_ii))
  
  
  # Normal residual plot
  # MVP PDF pg. 158
  i = seq(1, fit$n, by = 1)
  P = (i - 0.5)/fit$n
  
  norm_plot = data.frame("r" = sort(r), "P" = P)
  
  
  # Residual Plot vs Predicted y_hat
  # MVP PDF pg. 161
  pred_plot = data.frame("t_i" = t, "y_hat" = y_hat, "r" = r)
  
  
  # PRESS
  press = (e/(1 - h_ii))^2 %>% sum()
  R_pred = 1 - (press/fit$SS_T)
  
  
  result <- list(norm_plot, pred_plot, press, R_pred)
  names(result) <- c("norm_plot", "pred_plot", "PRESS", "R_pred")
  result
}


no_malta <- df[-35, ]


norm       <- plot_residuals(model_c1_var, "growth", no_malta)$norm_plot
prediction <- plot_residuals(model_c1_var, "growth", no_malta)$pred_plot


# Normal residual plot
# MVP PDF pg. 158
line = lm("P ~ r", data = norm) %>% summary() %>% .$coefficients %>% .[, "Estimate"]

ggplot(norm_plot, aes(x = r, y = P)) + theme_minimal() +
  geom_point() +
  geom_abline(intercept = line[1], slope = line[2], color = "blue") +
  scale_fill_discrete(name ="Modeling Method") +
  labs(title = "Normal Residual Plot - Model B2", 
       x = "Standardized Residuals", y = "Cumulative Probability",
       caption = "")


# Residual Plot vs Predicted y_hat
# MVP PDF pg. 161
ggplot(prediction, aes(x = y_hat, y = t_i)) + theme_minimal() +
  geom_point() +
  geom_hline(aes(yintercept = mean(r)), color = "blue") +
  scale_fill_discrete(name ="Modeling Method") +
  labs(title = "Prediction Residual Plot - Model B2", 
       x = "Predictions", y = "External Standardized Residuals",
       caption = "")



####################################################################
####################################################################
## 10) Cross Validation

# variance inflation factor (VIF)
# Rank severity of multicollinarity by excess of VIF >= 5 or >= 10
# MVP PDF pg. 407
vif(model_b2) %>% .[. >= 5 & . < 10]
vif(model_b2) %>% .[. >= 10]

vif(model_c1) %>% .[. >= 5 & . < 10]
vif(model_c1) %>% .[. >= 10]

vif(model_c2) %>% .[. >= 5 & . < 10]
vif(model_c2) %>% .[. >= 10]

vif(model_c3) %>% .[. >= 5 & . < 10]
vif(model_c3) %>% .[. >= 10]


# function to calculate cross validation results
# MVP PDF pg. 406
cross_val <- function(data, y, model_var, num_remove, replicate){
  output = list()
  for(i in 1:replicate){
    # subset rows for cross validation
    remove = sample(1:nrow(data), num_remove, replace = FALSE)
    training = data[-remove, ]
    testing = data[remove, ]
    
    
    # subset columns for the model evaluation by hand
    #training_model = cbind(rep(1, nrow(training)), training[, model_var]) %>% 
    #                 `colnames<-`(NULL) %>% `rownames<-`(NULL) %>% as.matrix()
    testing_model = cbind(rep(1, nrow(testing)), testing[, model_var]) %>% 
      `colnames<-`(NULL) %>% `rownames<-`(NULL) %>% as.matrix()
    
    #y_model_train = training[, y] %>% `colnames<-`(NULL) %>% `rownames<-`(NULL) %>% as.matrix()
    y_model_test = testing[, y] %>% `colnames<-`(NULL) %>% `rownames<-`(NULL) %>% as.matrix()
    
    
    # create the model expression for lm()
    expression = str_c(y, sep = " ~ ", str_c(model_var, collapse = " + "))
    
    model <- summary(lm(expression, data = training))
    train_R = model$r.squared
    
    
    # model estimation
    inv_Xsq = t(testing_model) %*% testing_model %>% solve()
    Xy = t(testing_model) %*% y_model_test
    
    beta_hat = inv_Xsq %*% Xy
    
    
    # R^2
    y_hat = testing_model %*% beta_hat
    e = y_model_test - y_hat
    
    SS_Res = t(y_model_test) %*% y_model_test - t(beta_hat) %*% Xy
    SS_T = t(y_model_test) %*% y_model_test - (sum(y_model_test)^2 / nrow(testing_model))
    
    R_pred = 1 - (SS_Res/SS_T)
    
    output[[i]] = data.frame("Trained" = round(train_R, 3), "Predicted" = round(R_pred, 3))
  }
  
  result = do.call(rbind, output)
  result
}


# Model B2
plot_residuals(model_b2_var, "growth", df)$PRESS
plot_residuals(model_b2_var, "growth", df)$R_pred

#b2_cross = cross_val(df, "growth", model_b2_var, 25, 50)
apply(b2_cross, 2, mean)
apply(b2_cross, 2, sd)

ols_matrices(model_b2_var, "growth", df)$MS_Res


# Model C1
plot_residuals(model_c1_var, "growth", df)$PRESS
plot_residuals(model_c1_var, "growth", df)$R_pred

#c1_cross = cross_val(df, "growth", model_c1_var, 25, 50)
apply(c1_cross, 2, mean)
apply(c1_cross, 2, sd)

ols_matrices(model_c1_var, "growth", df)$MS_Res


# Model C2
plot_residuals(model_c2_var, "growth", df)$PRESS
plot_residuals(model_c2_var, "growth", df)$R_pred

#c2_cross = cross_val(df, "growth", model_c2_var, 25, 50)
apply(c2_cross, 2, mean)
apply(c2_cross, 2, sd)

ols_matrices(model_c2_var, "growth", df)$MS_Res


# Model C3
plot_residuals(model_c3_var, "growth", df)$PRESS
plot_residuals(model_c3_var, "growth", df)$R_pred

#c3_cross = cross_val(df, "growth", model_c3_var, 25, 50)
apply(c3_cross, 2, mean)
apply(c3_cross, 2, sd)

ols_matrices(model_c3_var, "growth", df)$MS_Res





