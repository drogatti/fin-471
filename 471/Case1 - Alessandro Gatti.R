#create exhibit 1 df
ex1 <- data.frame(
  Year = rep(2009:2015),
  Lyxor_ChinaH = c(2.00, 4.25, -29.40, 13.23, 8.86, 2.31, -2.96),
  Lyxor_MSIndia = c(5.86, 22.40, -27.07, 0.60, -6.84, 33.87, -9.28),
  Lyxor_USDJIA = c(5.56, 6.11, 7.94, 18.29, 17.09, 14.20, -4.71),
  Lyxor_World = c(7.69, 5.79, -3.28, 20.75, 14.14, 15.06, -4.28)
)
ex1

#create exhibit 2 df
ex2 <- data.frame(
  Assets = c("Lyxor ChinaH", "Lyxor MSIndia", "Lyxor USDJIA"),
  Existing_Portfolio_Weights = c(60, 40, 0),
  New_Portfolio_Weights = c(40, 30, 30)
)
ex2

#find mean return and standard deviation of Lyxor ChinaH
mean_ret_ChinaH <- mean(ex1$Lyxor_ChinaH)
mean_ret_ChinaH
std_ChinaH <- sd(ex1$Lyxor_ChinaH)
std_ChinaH
#find mean return and standard deviation of Lyxor MSIndia
mean_ret_MSIndia <- mean(ex1$Lyxor_MSIndia)
mean_ret_MSIndia
std_MSIndia <- sd(ex1$Lyxor_MSIndia)
std_MSIndia
#find covariance and correlation for Lyxor ChinaH and Lyxor MSIndia
cov_ChinaH_MSIndia <- cov(ex1$Lyxor_ChinaH, ex1$Lyxor_MSIndia)
cov_ChinaH_MSIndia
corr_ChinaH_MSIndia <- cor(ex1$Lyxor_ChinaH, ex1$Lyxor_MSIndia)
corr_ChinaH_MSIndia

#calculate return standard deviation and return of existing portfolio (ex_port)
var_ChinaH <- var(ex1$Lyxor_ChinaH)
var_MSIndia <- var(ex1$Lyxor_MSIndia)
portvar_ex_port <- (0.6^2 * var_ChinaH) + (0.4^2 * var_MSIndia) + (2 * 0.6 * 0.4 * cov_ChinaH_MSIndia)
std_ex_port <- sqrt(portvar_ex_port)
std_ex_port
mean_ret_ex_port <- mean_ret_ChinaH*0.6 + mean_ret_MSIndia*0.4
mean_ret_ex_port

#calculate new portfolio's standard deviation and return
var_USDJIA <- var(ex1$Lyxor_USDJIA)
cov_ChinaH_USDJIA <- cov(ex1$Lyxor_ChinaH, ex1$Lyxor_USDJIA)
cov_MSIndia_USDJIA <- cov(ex1$Lyxor_MSIndia, ex1$Lyxor_USDJIA)
portvar_new_port <- (0.4^2 * var_ChinaH) + (0.3^2 * var_MSIndia) + (0.3^2 * var_USDJIA) +
  (2 * 0.4 * 0.3 * cov_ChinaH_MSIndia) + (2* 0.4 * 0.3 * cov_ChinaH_USDJIA) + (2 * 0.3 * 0.3 * cov_MSIndia_USDJIA)
std_new_port <- sqrt(portvar_new_port)
std_new_port
mean_ret_USDJIA <- mean(ex1$Lyxor_USDJIA)
mean_ret_new_port <- mean_ret_ChinaH*0.4 + mean_ret_MSIndia*0.3 + mean_ret_USDJIA*0.3
mean_ret_new_port

#calcualte the betas of the three ETFs
model_ChinaH <- lm(Lyxor_ChinaH ~ Lyxor_World, data = ex1)
beta_ChinaH <- coef(model_ChinaH)[2]
beta_ChinaH

model_MSIndia <- lm(Lyxor_MSIndia ~ Lyxor_World, data = ex1)
beta_MSIndia <- coef(model_MSIndia)[2]
beta_MSIndia

model_USDJIA <- lm(Lyxor_USDJIA ~ Lyxor_World, data = ex1)
beta_USDJIA <- coef(model_USDJIA)[2]
beta_USDJIA

#calcualte the required return for the three ETFs
risk_free_rate <- 0.025
market_risk_premium <- 0.055

required_return_ChinaH <- risk_free_rate + beta_ChinaH * market_risk_premium
required_return_ChinaH

required_return_MSIndia <- risk_free_rate + beta_MSIndia * market_risk_premium
required_return_MSIndia

required_return_USDJIA <- risk_free_rate + beta_USDJIA * market_risk_premium
required_return_USDJIA

#calculate the betas of the current and new portfolios
beta_ex_port <- beta_ChinaH*0.6 + beta_MSIndia*0.4
beta_ex_port
beta_new_port <- beta_ChinaH*0.4 + beta_MSIndia*0.3 + beta_USDJIA*0.3
beta_new_port

#calculate the required returns for both portfolios
required_return_ex_port <- risk_free_rate + beta_ex_port * market_risk_premium
required_return_ex_port
required_return_new_port <- risk_free_rate + beta_new_port * market_risk_premium
required_return_new_port

#calculate the correlation coefficients for the three pairs of ETFs
subset_ex1 <- ex1[, c("Lyxor_ChinaH", "Lyxor_MSIndia", "Lyxor_USDJIA")]
correlation_matrix <- cor(subset_ex1)
print(correlation_matrix)