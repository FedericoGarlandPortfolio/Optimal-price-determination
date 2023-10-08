# Pricing 
# Load packages
library(tidyverse)
library(ggplot2)
library(psych)
library(pscl)
library(DescTools)

# Load data
full_data <- as.data.frame(read.csv("C:/Users/User/Documents/Data/Proyecto Pricing/Data Pricing.csv", header = TRUE, sep = ";"))
full_data %>% head

# Exploratory analysis 
summary(full_data)
psych::describe(full_data)

# PRICING FOR SEVERAL PRODUCTS (AUTOMATED PRICING) ---- 
# Selecting products for the analysis 
# Price variability 
CV <- data.frame(
  SD = tapply(full_data$unit_price, full_data$product_id, sd), 
  Mean = tapply(full_data$unit_price, full_data$product_id, mean)) %>%
  mutate(CV = SD/Mean)
CV


# a. OLS linear regression  ---- 
# Fitting linear demand models for all products 
models_base <- list()
models <- list()
R2 <- c()
Alpha <- c()
Beta <- c()
for(i in unique(full_data$product_id)){
  models_base[[i]] <- lm(qty ~ unit_price, full_data[full_data$product_id == i,])
  models[[i]] <- summary(models_base[[i]])
  R2[i] <- models[[i]]$r.squared
  Alpha[i] <- models[[i]]$coefficients[2]
  Beta[i] <- models[[i]]$coefficients[1]
}
models
R2
Alpha
Beta

selected_products <- data.frame(Product = names(R2),
                                R_squared = c(R2), 
                                Alpha = c(Alpha), 
                                Beta = c(Beta)) %>% 
  arrange(-R_squared) %>%  
  filter(R_squared > 0.50 & Alpha < 0)
selected_products

# Optimal prices for each product 
cost <- c(5, 6, 8, 9, 3)
selected_products <- cbind(selected_products, cost)
selected_products
selected_products

optimal_prices <- selected_products %>% 
  mutate(Optimal_revenue_price = -Beta/(2*Alpha), 
         Optimal_profit_price = (Alpha*cost - Beta)/(2*Alpha), 
         Estimated_units_revenue = Alpha*Optimal_revenue_price + Beta,
         Estimated_units_profit = Alpha*Optimal_profit_price + Beta,
         Max_revenue = Optimal_revenue_price*Estimated_units_revenue, 
         Max_profit = (Optimal_profit_price-cost)*Estimated_units_profit)

optimal_prices

# Plotting optimal prices 
full_price_ranges <- list()
demand_curves <- list()
plots <- list()
filename <- c()
for(i in unique(selected_products$Product)){
  full_price_ranges[[i]] <- c(seq(0.1, 1.2*max(full_data[full_data$product_id == i, "unit_price"]), by = 0.1))
  demand_curves[[i]] <- data.frame(price = full_price_ranges[[i]],
                                   quantity = optimal_prices[optimal_prices$Product == i, "Beta"] + optimal_prices[optimal_prices$Product == i, "Alpha"]*full_price_ranges[[i]]) %>% 
    mutate(revenue = price*quantity, profit = (price-optimal_prices[optimal_prices$Product == i, "cost"])*quantity)
  
  plots[[i]] <- ggplot(demand_curves[[i]], aes(price, revenue)) +
    geom_smooth(method = "loess", aes(col = "Revenue")) +  
    geom_segment(x = optimal_prices[optimal_prices$Product == i, "Optimal_revenue_price"], xend = optimal_prices[optimal_prices$Product == i, "Optimal_revenue_price"],
                 y = -Inf, yend = optimal_prices[optimal_prices$Product == i, "Max_revenue"], linetype = "dashed", color = "blue") + 
    geom_text(x = optimal_prices[optimal_prices$Product == i, "Optimal_revenue_price"], y = 1.05*optimal_prices[optimal_prices$Product == i, "Max_revenue"], label = round(optimal_prices[optimal_prices$Product == i, "Optimal_revenue_price"], 0),
              color = "black", vjust = -0.5, hjust = 1) +
    geom_smooth(method = "loess", aes(price, profit, col = "Profit")) + 
    geom_segment(x = optimal_prices[optimal_prices$Product == i, "Optimal_profit_price"], xend = optimal_prices[optimal_prices$Product == i, "Optimal_profit_price"], 
                 y = -Inf, yend = optimal_prices[optimal_prices$Product == i, "Max_profit"], linetype = "dashed", color = "red") + 
    geom_text(x = optimal_prices[optimal_prices$Product == i, "Optimal_profit_price"], y = 1.05*optimal_prices[optimal_prices$Product == i, "Max_profit"], label = round(optimal_prices[optimal_prices$Product == i, "Optimal_profit_price"], 0),
              color = "black", vjust = -0.5, hjust = 1) + 
    scale_color_manual(name= NULL,
                       breaks=c("Revenue", "Profit"),
                       values=c("Revenue" = "blue", "Profit" = "red")) + 
    guides(color = guide_legend(override.aes = list(linetype = c(1, 1),
                                                    shape = c(1, 1),
                                                    size = c(1, 1), 
                                                    fill = c("white", "white")))) +
    coord_cartesian(ylim = c(0, 1.2*optimal_prices[optimal_prices$Product == i, "Max_revenue"])) +
    ggtitle(label = paste("Optimal price for maximum revenue and profit in", i)) +
    labs(x = "Price", y = "Revenue") + 
    theme(legend.title= element_text(size= 15),
          legend.text= element_text(size= 15),
          legend.position = c(0.85, 0.85),
          legend.background = element_rect(fill="white", 
                                           size=0.5, linetype="solid",
                                           colour ="black"),
          axis.line.y = element_line(),
          axis.line.x = element_line(), 
          panel.border = element_rect(colour = "black", fill = NA), 
          plot.title = element_text(hjust = 0.5, size = 15), 
          axis.text.y = element_text(size = 13, angle = 90, hjust = 0.5),
          axis.text.x = element_text(size = 13),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15))
  
  filename[i] <- paste("plot_", i, ".jpeg", sep = "")
  ggsave(filename[i], plot = plots[[i]], width = 8, height = 6, dpi = 300)
  
  
}

full_price_ranges
demand_curves
plots

# Final results 
ols <- cbind(optimal_prices[,c("Product", "Optimal_revenue_price", "Optimal_profit_price")], 
             Demand_model = rep("ols", nrow(optimal_prices)))
ols

# b. Exponential (Poisson) model ---- 
# Fitting exponential demand models for all products 
base_models <- list() 
models <- list()
AIC <- c()
Alpha <- c()
Beta <- c()
Pseudo_R2 <- c()
for(i in unique(full_data$product_id)){
  base_models[[i]] <- glm(qty ~ unit_price, full_data[full_data$product_id == i,], family = poisson(link = "log"))
  models[[i]] <- summary(glm(qty ~ unit_price, full_data[full_data$product_id == i,], family = poisson(link = "log")))
  AIC[i] <- models[[i]]$aic
  Alpha[i] <- models[[i]]$coefficients[2]
  Beta[i] <- models[[i]]$coefficients[1]
  Pseudo_R2[i] <- PseudoR2(base_models[[i]], which = "McFadden")
}
AIC
Alpha
Beta
models
base_models

selected_products2 <- data.frame(Product = names(AIC),
                                 AIC = c(AIC), 
                                 Alpha = c(Alpha), 
                                 Beta = c(Beta), 
                                 Pseudo_R2 = c(Pseudo_R2)) %>% 
  arrange(AIC) %>%  
  filter(Alpha < 0 & Pseudo_R2 > 0.4)
selected_products2



# Optimal prices for each product ---- 
# Assigning costs 
cost <- c(5, 4, 7, 6, 9, 10)
selected_products2 <- cbind(selected_products2, cost)
selected_products2
selected_products2



# Looping
revenue_function <- function(price, model) {
  estimated_sold_units <- predict(model, data.frame(unit_price = price))
  return(price * exp(estimated_sold_units))
}

profit_function <- function(price, model, cost) {
  estimated_sold_units <- predict(model, data.frame(unit_price = price))
  return((price-cost) * exp(estimated_sold_units))
}

prices <- list()
optimal_prices_revenue <- c()
optimal_prices_profit <- c()
curves <- list()
plots <- list()
filename <- c()
for(i in selected_products2$Product){
  prices[[i]] <- seq(0.01, 1.6*max(full_data[full_data$product_id == i, "unit_price"]), by = 0.01)
  optimal_prices_revenue[i] <- optimize(revenue_function,
                                        interval = c(1, max(prices[[i]])),
                                        model = base_models[[i]],
                                        maximum = TRUE)$maximum
  optimal_prices_profit[i] <- optimize(profit_function,
                                       interval = c(1, max(prices[[i]])),
                                       model = base_models[[i]],
                                       cost = selected_products2$cost[which(selected_products2$Product == i)],
                                       maximum = TRUE)$maximum
  curves[[i]] <- data.frame(Price = prices[[i]],
                            Revenue = revenue_function(prices[[i]], base_models[[i]]), 
                            Profit = profit_function(prices[[i]], base_models[[i]], cost = selected_products2$cost[which(selected_products2$Product == i)]))
  
  plots[[i]] <- ggplot(curves[[i]], aes(x = Price, y = Revenue)) + 
    geom_line(aes(col = "Revenue")) +
    geom_line(aes(x = Price, y = Profit, col = "Profit")) + 
    geom_text(x = optimal_prices_revenue[i], y = 1.05*max(curves[[i]]$Revenue), label = round(optimal_prices_revenue[i], 0),
              color = "black", vjust = -0.5, hjust = 1) + 
    geom_segment(x = optimal_prices_revenue[i], 
                 yend = max(curves[[i]]$Revenue),
                 xend = optimal_prices_revenue[i], 
                 y = -Inf, linetype = "dashed", col = "blue") +
    geom_segment(x = optimal_prices_profit[i], 
                 yend = max(curves[[i]]$Profit),
                 xend = optimal_prices_profit[i], 
                 y = -Inf, linetype = "dashed", col = "red") + 
    geom_text(x = optimal_prices_profit[i], y = 1.05*max(curves[[i]]$Profit), label = round(optimal_prices_profit[i], 0),
              color = "black", vjust = -0.5, hjust = 1) +
    scale_color_manual(name= NULL,
                       breaks=c("Revenue", "Profit"),
                       values=c("Revenue" = "blue", "Profit" = "red")) + 
    guides(color = guide_legend(override.aes = list(linetype = c(1, 1),
                                                    shape = c(1, 1),
                                                    size = c(1, 1), 
                                                    fill = c("white", "white")))) +
    coord_cartesian(ylim = c(0, 1.2*max(curves[[i]]$Revenue))) +
    ggtitle(label = paste("Optimal price curve for product", i)) + labs(x = "Price", y = "Revenue") + 
    theme(legend.title= element_text(size= 15),
          legend.text= element_text(size= 15),
          legend.position = c(0.85, 0.85),
          legend.background = element_rect(fill="white", 
                                           size=0.5, linetype="solid",
                                           colour ="black"),
          axis.line.y = element_line(),
          axis.line.x = element_line(), 
          panel.border = element_rect(colour = "black", fill = NA), 
          plot.title = element_text(hjust = 0.5, size = 15), 
          axis.text.y = element_text(size = 13, angle = 90, hjust = 0.5),
          axis.text.x = element_text(size = 13),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15))
  
  filename[i] <- paste("plot_poisson_", i, ".jpeg", sep = "")
  ggsave(filename[i], plot = plots[[i]], width = 8, height = 6, dpi = 300)
}

prices
optimal_prices_revenue
optimal_prices_profit
plots


# Preliminary Results 
glm <- data.frame(Product = selected_products2$Product, 
                  Optimal_revenue_price = optimal_prices_revenue, 
                  Optimal_profit_price = optimal_prices_profit, 
                  Demand_model = rep("glm", length(selected_products2$Product)))
glm 
results <- union(ols, glm) %>% arrange(Product)
results

# Adding average price 
means <- data.frame(Product = names(tapply(full_data$unit_price, full_data$product_id, mean)),
                    Average_price = tapply(full_data$unit_price, full_data$product_id, mean))

preliminary_results <- left_join(results, means, by = "Product") %>% 
  mutate(AverageVsOptimal = Optimal_revenue_price/Average_price - 1) %>% 
  filter(AverageVsOptimal > -0.9)
preliminary_results


# Deciding between glm and ols for repeated products
models_ols <- base_models
models_glm <- models_base

repeated_products <- preliminary_results %>% 
  group_by(Product) %>% 
  summarize(Freq = n()) %>% 
  filter(Freq > 1)

AIC_ols <- c()
AIC_glm <- c()
for(i in repeated_products$Product){
  AIC_ols[i] <- AIC(models_ols[[i]])
  AIC_glm[i] <- AIC(models_glm[[i]])
}

comparison <- data.frame(Product = repeated_products$Product, 
                         AIC_ols = c(AIC_ols), 
                         AIC_glm = c(AIC_glm), 
                         Decision = ifelse(AIC_ols > AIC_glm, "ols", "glm")
)
comparison

# Final results 
final_results <- preliminary_results %>%
  filter(!(Product %in% c("consoles1", "watches7", "watches8") & Demand_model == "glm"))
final_results


# Visualizing demand curves with both models for all products
plots <- list()
filename <- c()
for(i in unique(final_results$Product)){
  plots[[i]] <- ggplot(full_data[full_data$product_id == i,], aes(x = unit_price, y = qty)) + 
    geom_point() + 
    geom_smooth(method = "lm", aes(col = "OLS"), se = FALSE) + 
    geom_smooth(method = "glm", 
                method.args = list(family = poisson(link = "log")), 
                aes(col = "Poisson"), se = FALSE) + 
    scale_color_manual(name= NULL,
                       breaks=c("OLS", "Poisson"),
                       values=c("OLS" = "green", "Poisson" = "orange")) + 
    guides(color = guide_legend(override.aes = list(linetype = c(1, 1),
                                                    shape = c(1, 1),
                                                    size = c(1, 1), 
                                                    fill = c("white", "white")))) +
    ggtitle(label = paste("Demand curves for product", i)) + labs(x = "Price", y = "Quantity") + 
    theme(legend.title= element_text(size= 15),
          legend.text= element_text(size= 15),
          legend.position = c(0.85, 0.85),
          legend.background = element_rect(fill="white", 
                                           size=0.5, linetype="solid",
                                           colour ="black"),
          axis.line.y = element_line(),
          axis.line.x = element_line(), 
          panel.border = element_rect(colour = "black", fill = NA), 
          plot.title = element_text(hjust = 0.5, size = 15), 
          axis.text.y = element_text(size = 13, angle = 90, hjust = 0.5),
          axis.text.x = element_text(size = 13),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15))
  
  filename[i] <- paste("plot_demand_", i, ".jpeg", sep = "")
  ggsave(filename[i], plot = plots[[i]], width = 8, height = 6, dpi = 300)
  
  
}
plots