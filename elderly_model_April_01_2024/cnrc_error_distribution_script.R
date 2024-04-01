# install.packages("moments")

# Get the directory path of the current script
current_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the directory of the current script
setwd(current_dir)

library(moments)

# read inputs
survey.base <-
  read.csv("sim_input_v3.csv",
           check.names = FALSE,
           row.names = 1)

# View(survey.base)

original.data <-
  read.csv("V_13.1.csv", check.names = FALSE, row.names = 1)

# distribution <- function(data.vec, vec.name) {
#   print(paste0("min (", vec.name, "): ", min(data.vec)))
#   print(paste0("max (", vec.name, "): ", max(data.vec)))
#   print(paste0("mean (", vec.name, "): ", mean(data.vec)))
#   print(paste0("standard deviation (", vec.name, "): ", sd(data.vec)))
#   
#   hist(data.vec,
#        xlab = vec.name,
#        main = paste0("distribution ", vec.name))
# }

# distribution(survey.base$PCL_TOTAL, "PTSD")
# distribution(survey.base$PCL_Reexperiencing, "PTSD Image Arousal Subscale")
# distribution(survey.base$CCFQ_Cognitive_Control_Over_Emotion, "CCOE")
# distribution(survey.base$DASS_TOT_Stress, "DASS Stress")
# distribution(survey.base$DASS_TOT_Depression, "DASS Depression")
# distribution(survey.base$AEF_TOTAL, "Executive Score")
# distribution(survey.base$BFI_TOT_Neuroticism, "Neuroticism")
# distribution(survey.base$RDN_NMDOM, "Base Image Dominance")
# distribution(survey.base$RDN_NMARO, "Base Image Arousal")
# distribution(survey.base$RDN_NMVAL, "Base Image Valence")
#
# distribution(survey.base$NDQ_TOTAL, "Nightmare Distress")
# distribution(survey.base$NES_TOTAL, "Nightmare Effects")
# distribution(survey.base$DASS_TOT_Anxiety, "DASS Anxiety")
# distribution(survey.base$NFQ01, "Nightmare Frequency")

# predicted
# NightmareDistress	NightmareEffects	NightmareFrequency	DASSAnxiety
# outputs <- read.csv("ParametersVariationOutput.csv", check.names = FALSE, row.names = 1)
# previous is ParametersVariationOutput_outscaled.csv

# # PFC
# suffix <- "pfc02"

# suffix <- "init"
# suffix <- "inoutscaled"
# suffix <- "io_base"
# suffix <- "io_check"
# suffix <- "io_4feas"
suffix <- "io_4feas"

output.file.name <-
  paste0("ParametersVariationOutput_", suffix, ".csv")
outputs <-
  read.csv(output.file.name,
           check.names = FALSE,
           row.names = 1)
merged.df <- merge(survey.base, outputs, by.x = 0, by.y = 0)

merged.df$NightmareDistress
merged.df$NightmareFrequency
merged.df$DASSAnxiety

# distribution(merged.df$NightmareDistress, "ndq")

# ndq: (13, 52)
# nes: (0, 25)
# ANX: (0, 26)
# NFQ: (1, 364)

# expected enters the function wide, here (scale it)

# finds the residual between expected and predicted anylogic predictions
get.residual <-
  function(expected,
           predicted,
           min.exp = NULL,
           max.exp = NULL) {
    # target.min <- min(expected)
    # target.max <- max(expected)
    target.min <- 0
    target.max <- 1
    
    min.p <- min(predicted)
    max.p <- max(predicted)
    
    min.e <- min(expected)
    max.e <- max(expected)
    
    if (!is.null(min.exp) & !is.null(max.exp)) {
      min.e <- min.exp
      max.e <- max.exp
    }
    
    # print(min.p)
    # print(max.p)
    
    expected.scaled <-
      (((expected - min.e) * (target.max - target.min)) / (max.e - min.e)) + target.min
    predicted.scaled <-
      predicted
      # (((predicted - min.p) * (target.max - target.min)) / (max.p - min.p)) + target.min
    # residual <- expected.scaled - predicted.scaled
    residual <-  predicted.scaled - expected.scaled
    # print(paste0("p: ", predicted.scaled, " e: ", expected.scaled))
    # residual <-  predicted - expected
    
    # hist(residual)
    
    return(residual)
  }

# gets the mean squared error of the anylogic model
get.anylogic.mse <-
  function(expected,
           predicted,
           base.min = NULL,
           base.max = NULL) {
    target.min <- 0
    target.max <- 1
    
    min.e <- min(expected)
    max.e <- max(expected)
    
    if (!is.null(base.min)) {
      min.e <- base.min
    }
    
    if (!is.null(base.max)) {
      max.e <- base.max
    }
    
    # print(max.e)
    
    expected.scaled <-
      (((expected - min.e) * (target.max - target.min)) / (max.e - min.e)) + target.min
    # print(paste0("min: ", min.e))
    # print(paste0("max: ", max.e))
    sse <- sum((predicted - expected.scaled) ^ 2)
    sae <- sum(abs(predicted - expected.scaled))
    
    print(paste0("mse: ", sse / length(predicted)))
    print(paste0("mae: ", sae / length(predicted)))
  }

# finds the lowest confidence interval where 0 is between the lower and upper bounds
get.min.conf.int <-
  function(name_,
           expected,
           predicted,
           surv.min = NULL,
           surv.max = NULL) {
    resid <- get.residual(expected, predicted, surv.min, surv.max)
    
    finished <- FALSE
    n <- -1
    my.num <- ""
    
    while (!finished) {
      my.num <- "0.9"
      if (n == -1) {
        my.num <- "0.95"
      } else {
        for (i in 0:n) {
          if (i != 0) {
            my.num <- paste0(my.num, "9")
          }
        }
      }
      
      t.test.results.two <-
        t.test(resid, mu = 0, conf.level = as.numeric(my.num))
      conf <- t.test.results.two$conf.int
      lower <- conf[1]
      upper <- conf[2]
      
      if ((0 > lower) & (0 < upper)) {
        finished <- TRUE
      } else {
        n <- n + 1
      }
    }
    print(paste0(suffix, name_, ": ", my.num, " (", n, " 9s)"))
  }

# merged.df$NDQ_TOTAL
#
merged.df$NightmareDistress

get.min.conf.int("Distress",
                 merged.df$NDQ_TOTAL,
                 merged.df$NightmareDistress,
                 13,
                 65)

get.min.conf.int("Effects",
                 merged.df$NES_TOTAL,
                 merged.df$NightmareEffects,
                 0,
                 44)

get.min.conf.int("Anxiety",
                 merged.df$DASS_TOT_Anxiety,
                 merged.df$DASSAnxiety,
                 0,
                 42)

get.min.conf.int("Frequency",
                 merged.df$NFQ01,
                 merged.df$NightmareFrequency)


# prints distribution statistics of model output errors
get.dist.stats <-
  function(expected,
           predicted,
           title,
           xlab,
           ylab,
           surv.min = NULL,
           surv.max = NULL) {
    resid <- get.residual(expected, predicted, surv.min, surv.max)
    
    edited.title <- paste0(title, "(", output.file.name, ")")

    hist(
      resid,
      main = edited.title,
      xlab = xlab,
      ylab = ylab,
      col = "lightblue",
      border = "black"
    )
    
    # distressRS += Math.pow(p.NightmareDistress - p.expectedNightmareDistress, 2);
    
    mean_ <- mean(resid)
    sdv <- sd(resid)
    skew <- skewness(resid)
    kurt <- kurtosis(resid)
    t.test.results.two <- t.test(resid, mu = 0, conf.level = 0.95)
    # t.test.results.one <- t.test(abs(resid), mu = 0, alternative = "less",
    #                              conf.level = 0.999)
    
    # // min max scaling expected values
    # person.expectedNightmareFrequency = (Double.parseDouble(row[19]) - 1) / 363.0;
    # person.expectedNightmareDistress = (Double.parseDouble(row[17]) - 13) / (52 - 13);
    # person.expectedNightmareEffects = (Double.parseDouble(row[18]) - 0) / (25 - 0);
    # person.expectedAnxiety = (Double.parseDouble(row[20]) - 0) / (26 - 0);
    
    if (!is.null(surv.min) & !is.null(surv.max)) {
      get.anylogic.mse(expected, predicted, surv.min, surv.max)
    } else {
      get.anylogic.mse(expected, predicted)
    }
    
    print(paste0("mean: ", mean_))
    print(paste0("sdv: ", sdv))
    print(paste0("skewness: ", skew))
    print(paste0("kurtosis: ", kurt))
    print("two-tailed t-test")
    print(t.test.results.two)
  }

hist(merged.df$NightmareDistress)
merged.df$NDQ_TOTAL

hist(merged.df$NightmareFrequency)

hist(merged.df$DASSAnxiety)

hist(merged.df$NightmareEffects)

get.dist.stats(
  merged.df$NDQ_TOTAL,
  merged.df$NightmareDistress,
  "Nightmare Distress Error Distribution",
  "error",
  "frequency"
  ,
  13,
  65
)

get.dist.stats(
  merged.df$NES_TOTAL,
  merged.df$NightmareEffects,
  "Nightmare Effects Error Distribution",
  "error",
  "frequency"
  ,
  0,
  44
)

get.dist.stats(
  merged.df$DASS_TOT_Anxiety,
  merged.df$DASSAnxiety,
  "DASS Anxiety Error Distribution",
  "error",
  "frequency"
  ,
  0,
  42
)

get.dist.stats(
  merged.df$NFQ01,
  merged.df$NightmareFrequency,
  "Nightmare Frequency Error Distribution",
  "error",
  "frequency"
)
