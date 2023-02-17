#### Alter & Counsell 2022 SIMULATION ####

library(SimDesign)
# Fixed parameters
alpha= 0.05
SESOI <- 0.15
l.delta <- -abs(SESOI)
u.delta <- abs(SESOI)
mu <- c(0,0,0,0,0)


# calling on functions for the difference-based (traditional NHST), TOST, and AH tests.
source('Alter-Counsell-2022-test-functions.R')
##### Design ##### 

Design <- createDesign(N = c(50, 75, 100, 250, 500, 1000),
                       test= c("DB", "TOST", "AH"),
                       beta=c(1, 2, 3, 4, 5),
                       cors= c(0, 0.25, 0.5, 0.75))

Design

##### Generate ##### 


Generate <- function(condition, fixed_objects = NULL ) {
  Attach(condition)
  sigma <- matrix(data=c(1,cors,cors,cors,cors,
                         cors,1,cors,cors,cors,
                         cors,cors,1,cors,cors,
                         cors, cors, cors, 1, cors,
                         cors, cors, cors, cors, 1),
                  nrow=5,ncol=5)
  xs <- rmvnorm(N, mean = mu, sigma = sigma)
  e <- rnorm(N)
  xs <- as.data.frame(xs)
  y <- 1 + 0*xs$V1+ 0.05*xs$V2 + 0.1*xs$V3 + 0.15*xs$V4 + 0.2*xs$V5 + e
  dat <- data.frame(xs,y)
  dat
}

##### Analyse ##### 

Analyse <- function(condition, dat, fixed_objects = NULL) {
  Attach(condition)
  if(test=="DB"){
    p<- modelstats(dat, beta)$p
  }
  if(test=="TOST"){
    p <- TOST(dat, beta)$p
  }
  if(test=="AH"){
    p <- AH(dat, beta)$p
  }
  ret <- c(p=p)
  ret
}

##### Summarise ##### 

Summarise <- function(condition, results, fixed_objects = NULL) {
  Attach(condition)
  ifelse(test=="DB",neg <- 1 - EDR(results, alpha=alpha), neg <- EDR(results, alpha=alpha))
  ret <- c(concluding_negligible=neg)
  ret
}

##### Run #####

res <- runSimulation(design=Design, replications=5000, generate=Generate, 
                     analyse=Analyse, summarise=Summarise)
RES <- res
#View(res)


####################### SAVING DATA ################################
write.csv(res, "Alter_Counsell_Simulation_Results_with_CORS DEC26.csv")
library(writexl)
write_xlsx(res, "~/Library/CloudStorage/GoogleDrive-udi.alter@gmail.com/My Drive/Master's thesis/TQMP submission/Final/Full_results_from_SimDesign_Correlations DEC26.xlsx")


####################### VISUALIZING DATA ################################

library(readr)
dat <- read_csv("Google Drive/My Drive/Master's thesis/TQMP submission/Final/Alter_Counsell_Simulation_Results_with_CORS DEC26.csv")
#View(dat)

library(tidyverse)
library(ggrepel)
library(patchwork)
library(RColorBrewer)                            # Load RColorBrewer


simresults <- dat
View(simresults)
simresults["beta"][simresults["beta"] == 1] <- 0
simresults["beta"][simresults["beta"] == 2] <- 0.05
simresults["beta"][simresults["beta"] == 3] <- 0.1
simresults["beta"][simresults["beta"] == 4] <- 0.15
simresults["beta"][simresults["beta"] == 5] <- 0.2
simresults$cors <- factor(simresults$cors)
simresults$label <- paste("β =", as.character(simresults$beta))



# CORRECT 
simresults |>
  filter(beta == 0 | beta==0.05 | beta == 0.1) |>
  ggplot( aes(x = factor(N), y = concluding_negligible.p, 
              group= interaction(test, cors), 
              colour= test,  linetype = cors))+
  geom_line(linewidth=1.5, alpha=0.8)+ 
  scale_linetype_manual(values = c("solid", "longdash", "dotdash", "dotted"))+
  facet_wrap(~label)+
  theme_minimal()+
  theme(axis.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        strip.text = element_text(size = 18),
        text=element_text(family="Times"))+
  labs(color = "Test", linetype="Correlations")+
  scale_colour_brewer(palette = "Dark2")+
  scale_y_continuous(breaks=seq(0,1,0.1))+
  geom_hline(yintercept=0.8, linetype='dotted', col = 'blue')+
  labs(y="Rate of Correct Negligible Association Conclusions", x = "Sample Size")+ #title = "Correctly Concluding Negligible Association by Test, Effect, Correlation, and Sample Size"
  annotate("text",x="50" , y = 0.8, label = "1-\u03B2 = .80", vjust=-.7,hjust=.2, family="Times", size= 5)
#ggsave("Correct_in_colour.png", width = 20, height=15, units = "cm")

# INCORRECT 
simresults |>
  filter(beta == 0.15 | beta==0.2 ) |>
  ggplot( aes(x = factor(N), y = concluding_negligible.p, 
              group= interaction(test, cors), 
              colour= test, linetype = cors))+
  geom_line(linewidth=1.5, alpha=0.8)+ 
  scale_linetype_manual(values = c("solid", "longdash", "dotdash", "dotted"))+
  facet_wrap(~label)+
  theme_minimal()+
  theme(axis.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        strip.text = element_text(size = 18),
        text=element_text(family="Times"))+
  labs(color = "Test", linetype="Correlations")+
  scale_colour_brewer(palette = "Dark2")+
  scale_y_continuous(breaks=seq(0,1,0.1))+
  geom_hline(yintercept=0.05, linetype='dotted', col = 'red')+
  labs( y="Rate of Incorrect Negligible Association Conclusions", x = "Sample Size")+ #title = "Incorrectly Concluding Negligible Association by Test, Effect, Correlation, and Sample Size",
  annotate("text",x="50" , y = 0.05, label = "\u03B1 = .05", vjust=-.7,hjust=.55, family="Times",  size= 5)
#ggsave("Incorrect_in_colour.png", width = 20, height=15, units = "cm")


