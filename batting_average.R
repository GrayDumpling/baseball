library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

source("freqacc.R")
all_batting_data<- read.csv("baseballdatabank-2017.1/core/Batting.csv")
all_appearce_data<-read.csv("baseballdatabank-2017.1/core/Appearances.csv")
# 2.2 Batting Table
# playerID       Player ID code
# yearID         Year
# stint          player's stint (order of appearances within a season)
# teamID         Team
# lgID           League
# G              Games
# AB             At Bats
# R              Runs
# H              Hits
# 2B             Doubles
# 3B             Triples
# HR             Homeruns
# RBI            Runs Batted In
# SB             Stolen Bases
# CS             Caught Stealing
# BB             Base on Balls
# SO             Strikeouts
# IBB            Intentional walks
# HBP            Hit by pitch
# SH             Sacrifice hits
# SF             Sacrifice flies
# GIDP           Grounded into double plays

# 2.22 Appearances table
# 
# yearID         Year
# teamID         Team
# lgID           League
# playerID       Player ID code
# G_all          Total games played
# GS             Games started
# G_batting      Games in which player batted
# G_defense      Games in which player appeared on defense
# G_p            Games as pitcher
# G_c            Games as catcher
# G_1b           Games as firstbaseman
# G_2b           Games as secondbaseman
# G_3b           Games as thirdbaseman
# G_ss           Games as shortstop
# G_lf           Games as leftfielder
# G_cf           Games as centerfielder
# G_rf           Games as right fielder
# G_of           Games as outfielder
# G_dh           Games as designated hitter
# G_ph           Games as pinch hitter
# G_pr           Games as pinch runner

real_pp_names = c("pitcher",
                  "catcher",
                  "first baseman",
                  "second baseman",
                  "third baseman",
                  "shortstop",
                  "left fielder",
                  "center fielder",
                  "right fielder")

batting_2012<-all_batting_data[all_batting_data$yearID==2012,]


appearance_2012 <- all_appearce_data[all_appearce_data$yearID==2012,]


data_2012 <- merge(batting_2012, appearance_2012)

pp_names = c("G_p", "G_c", "G_1b", "G_2b", "G_3b", "G_ss", "G_lf", "G_cf", "G_rf")
names(pp_names)<-real_pp_names
pp_names_subset = pp_names["picher"]
pp = max.col(data_2012[, pp_names], ties.method="random")
unique(pp)
table(pp) 
pp_names



data_2012$pp = pp
data_2012_AB = data_2012[data_2012$AB >= 1, ]
# Use only a subset of postitions to reduce computation time 
pp_names_subset = c(1)
data_2012_AB_subset = data_2012_AB[data_2012_AB$pp %in% pp_names_subset,]

table(data_2012_AB_subset$pp) 
pp_names
nrow(data_2012_AB_subset)

baseball_data = list(N = nrow(data_2012_AB_subset),
                     M = length(pp_names_subset),
                     AB = data_2012_AB_subset$AB,
                     H = data_2012_AB_subset$H,
                     pp = data_2012_AB_subset$pp,
                     A = 1,
                     B = 1,
                     S = 0.001,
                     R = 0.001)
                     # S1 = 0.001,
                     # R1 = 0.001,
                     # S2 = 0.001,
                     # R2 = 0.001)

bootstrap_num = 50
# posterior_mean_for_mu=c()
for (i in 1:bootstrap_num) {
  bootstrap_indices = sample(1:nrow(data_2012_AB_subset), nrow(data_2012_AB_subset), TRUE)
  baseball_data = list(N = nrow(data_2012_AB_subset),
                       M = length(pp_names_subset),
                       AB = data_2012_AB_subset$AB[bootstrap_indices],
                       H = data_2012_AB_subset$H[bootstrap_indices],
                       pp = data_2012_AB_subset$pp[bootstrap_indices],
                       A = 1,
                       B = 1,
                       S = 0.001,
                       R = 0.001)
  fit <- stan(file = 'baseball_simper.stan', data = baseball_data, 
            iter = 200000, chains = 1)
  print(paste("Iteration", i,  "times used:", sum(get_elapsed_time(fit))))
  write(summary(fit, "mu", probs=c())$summary[,"mean"],
        append=TRUE, file="posterior_mean_for_mu")
}
# save.image(file="output.RData")
# bootstrap_sd = var(posterior_mean_for_mu)
# 
# get_elapsed_time(fit)
# Calculate Var(X) at theta = theta_mle.
# V_mle = diag(data_2012_AB_subset$AB *
#                (data_2012_AB_subset$H/data_2012_AB_subset$AB) *
#                (1-data_2012_AB_subset$H/data_2012_AB_subset$AB))
# 
# 
# #Calculate frequentest sd and add to posterior_summary_mu
# posterior_summary_mu = data.frame(summary(fit, "mu", probs=c())$summary)
# for (par in rownames(posterior_summary_mu)) {
#   posterior_summary_mu[par, "sdfreq"] = freqacc(tt=extract(fit, par)[[1]],
#                                              aa=extract(fit)$theta,
#                                              V=V_mle)["sdfreq"]
# }

# 
# # Plot a bar plot for sdfreq and sdbayes
# pp_sds = t(as.matrix(posterior_summary_mu[, c("sdfreq", "sd")]))
# colnames(pp_sds) <- real_pp_names
# barplot(pp_sds, beside=TRUE, col=c("blue","red"))
# legend("topright", c("sdfreq", "sdbayes"), fill = c("blue","red"))
# 
# 
# 
# # Automatic generate a latex table for sdfreq and sdbayes for mus.
# temp = data.frame(posterior_summary[mus, c("sdfreq", "sd")])
# temp$ratio =  pp_sds[1,]/pp_sds[2,]
# rownames(temp)<-real_pp_names
# library(xtable)
# xtable(temp, digits=4)
# 
# print(xtable, type="latex")
# eigenvalues_of_H <- eigenvalues_of_H(aa=extract(fit)$theta, V=V_mle)$values
# sqrt(max(eigenvalues_of_H))
# 
# 
# 
# ## Generate a histogram of ratios for all thetas
# thetas = sapply(1:nrow(data_2012_AB_subset), FUN = function(x) paste0("theta[", x, "]"))
# temp_theta = data.frame(posterior_summary[thetas, c("sdfreq", "sd")])
# temp_theta$ratio = temp_theta$sdfreq/ temp_theta$sd
# library("ggplot2")
# hist(temp_theta$ratio, xlab = "ratio", 
#      main= expression(paste("Histogram of sd ratios for player means ",theta ['i'])), col="gray")
