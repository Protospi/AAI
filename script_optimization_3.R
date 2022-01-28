
##########################################################################################################

# Sketch optimization problem to generate reports

##########################################################################################################

# Load libraries
library(tidyverse)
library(gridExtra)
library(rmarkdown)

##########################################################################################################

# Load data
X <- read_csv("PROCESSED_DATA/features_x.csv")  %>% arrange(date) %>% slice(1:2473)
colnames(X) <- c("Data","Dolar", "Euro", "Yuhan Chines", "Peso Argentino", "Gasolina", "Selic", "Cesta Basica")
Y <- read_csv("PROCESSED_DATA/targets_y.csv") %>%  arrange(date) %>%  slice(2:2474)

##########################################################################################################

# Define date ranges
date_range <- as.character(seq(as.Date("2012-01-03"), as.Date("2016-01-03"), by = "year"))

# Define lisp of combinations
combs_range <- list()

# Define combination of variables
for(i in 1:7){
  
  # Generate combinations aux variable
  aux <- combn(1:7, i) 
  
  # Loop to extract columns as vectors to the list
  for(comb in 1:ncol(aux))
    
    # Populate list
    combs_range <- append(combs_range, list(aux[, comb]))
  
}


##########################################################################################################

# Final reports

##########################################################################################################

# Define list of stocks
stocks <- colnames(Y[, 2:ncol(Y)]) %>%  sort()

# emove Stock with problems
which(stocks == "RSUL3")
stocks <- stocks[-266]

# Preprocess data
df_feature <- scale(X[, -1]) %>% as_tibble()

##########################################################################################################

# Best models
df_final <- tibble(stock = rep("stock", 322),
                   start_date = rep("2012-01-03",322),
                   combs_range = rep(0, 322),
                   train_test_split = rep(0, 322),
                   rmse = rep(0, 322),
                   r2 = rep(0,322))

# Loop over stocks
for(index in 1:length(stocks)){
  
  # Define start_time
  start_time <- Sys.time()
  
  # Define data
  df_target <- Y[, c("date",stocks[index])] %>%
    rename(target = stocks[index]) %>% 
    mutate(target = log(target)) %>%
    select(date, target)
  
  # Define data frame
  df <- df_target %>% 
    bind_cols(df_feature)
  
  # Split train test range
  train_test_split <- seq(0.6, 0.9, by = 0.01)
  
  # Define list of start dates
  model_df <- tibble(start_date = rep("2012-01-03",19685),
                     combs_range = rep(0, 19685), # rep("vars", 19685),
                     train_test_split = rep(0, 19685),
                     rmse = rep(0, 19685),
                     r2 = rep(0, 19685))
  
  # Define counter o models
  counter = 1
  
  # Loop over date range
  for(start_date in date_range){
    
    # Define matrix train test split as columns and variables combinations as rows
    mat_temp <- matrix(0, nrow = length(train_test_split), ncol = length(combs_range))
    
    # Set temporary dataframe 1
    df_temp1 <- df %>%  filter(date > start_date)
    
    # Loop over variables
    for(cr in 1:length(combs_range)){
      
      # Set temporary dataframe 2
      df_temp2 <- df_temp1[, c(2,(combs_range[[cr]] + 2))]
      
      # Loop over train_test_split
      for(tts in 1:length(train_test_split)){
        
        # Define train sample
        train_sample <- 1:(round(nrow(df_temp2) * train_test_split[tts] ))
        
        # Define train and test
        train <- df_temp2[train_sample, ]
        test <- df_temp2[-train_sample, ]
        
        # Print Model
        model <- lm(target ~ ., data = train)
        
        # Define predicted and target
        predicted <- predict(model, newdata = test[, -1])
        target <- test$target
        
        # Calculate rmse
        residuals <- predicted - target
        rmse <- round(sqrt(sum(residuals^2)) / (length(predicted) - length(model$coefficients)),6)
        
        # Populate model list
        model_df[counter, "start_date"] <- start_date
        model_df[counter, "combs_range"] <- cr 
        model_df[counter, "train_test_split"] <- train_test_split[tts]
        model_df[counter, "rmse"] <- rmse
        model_df[counter, "r2"] <- summary(model)$adj.r.squared
        
        # Increment couter
        counter <- counter + 1
        
        # Check Counter
        print(paste0(stocks[index], " - ", counter))
        
      }
      
    }
    
  }
  
  # Define end_time
  end_time <- Sys.time()
  
  # Define computational cost
  cost <- round((end_time - start_time) / 60, 4)
  
  # Save plot and table report as html report
  render("template_report.rmd",
         encoding = "UTF-8",
         output_file = paste0("REPORTS_REQM_SCALED_MINUS_R2/",
                              stocks[index],
                              '.pdf'))
  
}

# Save models csv
write_csv(df_final, "REPORTS_REQM_SCALED_MINUS_R2/optimized_models.csv")

##########################################################################################################

# Return coeficicnets to original scale and interpret results

##########################################################################################################

# Explain Coeficients 
x <- X$`Peso Argentino`
mean_x <- mean(x)
sd_x <- sd(x)
x[1:5]
(x[1:5] - mean_x) / sd_x
hist((x - mean_x) / sd_x)
(1 * sd(X$`Peso Argentino`) + mean(X$`Peso Argentino`))

# Return variable ars to original scale
increment <- 1 / (1 * sd(X$`Peso Argentino`) + mean(X$`Peso Argentino`))

# Coeficient for ars = -0.7
100 * (exp(increment * -0.7) - 1)

# Return variable cny to original scale
increment <- 1 / (1 * sd(X$`Yuhan Chines`) + mean(X$`Yuhan Chines`))

# Coeficient for ars = -0.7
100 * (exp(increment * 0.6180) - 1)

# Yuhan hist
hist(X$`Yuhan Chines`)




