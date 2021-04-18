#install.packages("readxl") 
#install.packages("contextual")
#install.packages("data.table")
library("readxl")
library(contextual)
library(data.table)

atukunda_a <- read.csv("atukunda_a.csv", sep = ",")

###############################################################################
#
# Atukunda's study: Sublingual misoprostol versus intramuscular oxytocin for 
# prevention of postpartum hemorrhage in Uganda: a double-blind randomized non-inferiority trial.
#
# Trial: Different types of drugs: misoprostol & oxytocin
# misoprostol = 0, oxytocin = 1 
# Outcome: Bloodloss within 24 hr >= 500 ml (positive = 262, negative = 878)
# postive = 1, negative = 0
# Context: age, bmi, edu, history of birth
# age (1 = 18 - 35, 0 = >35)
# bmi (1 = > 25, 0 = <= 25)
# highest edu (3 = tertiary, 2 = secondary, 1 = primary, 0 = No schooling)
# birth (no = 0, yes = 1)
#############

############# Variable Analysis - Thompson Sampling


atukunda_a[atukunda$blood_loss_24hrs >= 500, 6] <- 1
atukunda_a[atukunda$blood_loss_24hrs < 500, 6] <- 0
atukunda_a$study_group <- as.numeric(as.factor(atukunda$study_group)) - 1 
atukunda_a$age_cat <- as.numeric(as.factor(atukunda$age_cat)) - 1 
atukunda_a$bmi_cat <- as.numeric(as.factor(atukunda$bmi_cat)) - 1 
atukunda_a$highesteduc <- as.numeric(as.factor(atukunda$highesteduc)) - 1 
atukunda_a$histhomebirth <- as.numeric(as.factor(atukunda$histhomebirth)) - 1 

atukunda_a <- data.table(atukunda_a)

atukunda_a[, t := .I]
atukunda_a[, sim := 1]



colnames(atukunda_a)
nrow((as.data.frame(atukunda_a[atukunda_a$study_group == 1, 6])))
sum(as.data.frame(atukunda_a[atukunda_a$study_group == 1, 6]))
99/570

nrow((as.data.frame(atukunda_a[atukunda_a$study_group == 0, 6])))
sum(as.data.frame(atukunda_a[atukunda_a$study_group == 0, 6]))
163/570
######################Bandits
simulations <- 1
horizon <- nrow(atukunda_a)
log_S <- atukunda_a
formula <- "blood_loss_24hrs ~ study_group"
bandit <- OfflineReplayEvaluatorBandit$new(formula,log_S)
#?RandomPolicy
agents <-
  list(Agent$new(ThompsonSamplingPolicy$new(1.0,1.0), bandit, "ThompsonSampling"),
       Agent$new(UCB1Policy$new(), bandit, "UCB1"))

simulation <-
  Simulator$new(
    agents = agents,
    simulations = simulations,
    horizon = horizon,
    save_context = FALSE,
    progress_file = TRUE,
    do_parallel = FALSE
  )

sim <- simulation$run()
plot(sim, type = "cumulative", legend_title = "Simulation",
     rate = TRUE, regret = FALSE, legend_position = "bottomright")
results <- data.frame(sim$data)
results_Random <- results[results$agent == 'Random',c(4,15)]
Random_cumrate <- colMeans(matrix(results_Random$cum_reward_rate, nrow=100))
results_TS <- results[results$agent == 'ThompsonSampling',c(4,15)]
TS_cumrate <- colMeans(matrix(results_TS$cum_reward_rate, nrow=100))
plot(1:570,Random_cumrate, xlab = "rounds", ylab = "cumulative rewards rate", type="l")
lines(1:570,TS_cumrate,col="blue")

write.csv(atukunda_a, "atukunda_a.csv")
