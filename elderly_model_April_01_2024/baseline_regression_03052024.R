# install.packages("dplyr")
# install.packages("leaps")
# install.packages("stats")
# install.packages("lm.beta")
# install.packages("psych")
# install.packages("ggplot2")
# install.packages("rsq")
# install.packages("car")

library(dplyr)
library(leaps)
library(stats)
library(lm.beta)
library(psych)
library(ggplot2)
library(rsq)
library(car)

# getwd()
# setwd("D:/elderly_data")

# Get the directory path of the current script
current_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the directory of the current script
setwd(current_dir)


# Read the CSV file
df <-
  read.csv("V_13.1.csv", fileEncoding = 'UTF-8-BOM', row.names = NULL)

nfq.formula <-
  as.formula(
  "NFQ01 ~ PCL_TOTAL + PCL_Reexperiencing + CCFQ_Cognitive_Control_Over_Emotion + DASS_TOT_Stress + DASS_TOT_Depression + AEF_TOTAL + BFI_TOT_Neuroticism + RDN_NMARO + RDN_NMDOM + RDN_NMVAL"
)
ndq.formula <-
  as.formula(
  "NDQ_TOTAL ~ PCL_TOTAL + PCL_Reexperiencing + CCFQ_Cognitive_Control_Over_Emotion + DASS_TOT_Stress + DASS_TOT_Depression + AEF_TOTAL + BFI_TOT_Neuroticism + RDN_NMARO + RDN_NMDOM + RDN_NMVAL"
)

nes.formula <-
  as.formula(
  "NES_TOTAL ~ PCL_TOTAL + PCL_Reexperiencing + CCFQ_Cognitive_Control_Over_Emotion + DASS_TOT_Stress + DASS_TOT_Depression + AEF_TOTAL + BFI_TOT_Neuroticism + RDN_NMARO + RDN_NMDOM + RDN_NMVAL"
)

anx.formula <-
  as.formula(
  "DASS_TOT_Anxiety ~ PCL_TOTAL + PCL_Reexperiencing + CCFQ_Cognitive_Control_Over_Emotion + DASS_TOT_Stress + DASS_TOT_Depression + AEF_TOTAL + BFI_TOT_Neuroticism + RDN_NMARO + RDN_NMDOM + RDN_NMVAL"
)

get.regression.df <- function(formula.var, csv.name) {
  # Fit linear regression model
  m <- lm(formula.var, data = df)
  p.vals <- summary(m)$coefficients[, "Pr(>|t|)"]
  betas <- lm.beta(m)$standardized.coefficients
  adj.r.sq <- summary(m)$adj.r.squared
  
  # Create a dataframe to store results
  results_df <- data.frame(
    Independent_Variable = names(m$coefficients),
    Beta = betas,
    P_Value = p.vals,
    Adjusted_R_Squared_Value = NA
  )
  
  # Add adjusted R-squared to the last row
  results_df <- rbind(results_df, c("Adjusted_R_Squared", NA, NA, adj.r.sq))
  
  # print(adj.r.sq)
  
  # Print the dataframe
  View(results_df)
  # write.csv(results_df, paste0("baseline_regressions_march_05_2024/", csv.name), row.names = FALSE)
}

get.regression.df(nfq.formula, "baseline_NFQ.csv")

get.regression.df(nes.formula, "baseline_NES.csv")

get.regression.df(ndq.formula, "baseline_NDQ.csv")

get.regression.df(anx.formula, "baseline_ANX.csv")


# p.vals
# betas
# adj.r.sq
