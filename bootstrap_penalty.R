library(tidyverse)
library(rms)
library(pROC)
dat = readRDS("ABCD_ERS_youth.rds")
incdat = dat[which(dat$MDDLifetime_b==0),]
incdat = incdat %>% select("sex", "white", "trauma3", "sch1", "loneliness01", "fights01", "run01", "everdrugs", "care1q", "care2q", "conflictq", "incidentMDD", "incSeverity")

completecols = names(incdat)

inc.complete = incdat[complete.cases(incdat),]

# make sure the abcd data levels will be in the correct order
inc.complete = inc.complete %>% mutate(
  sex = factor(sex, levels = c("M", "F")),
  white = factor(white, levels = c("white", "nonwhite")),
  trauma3 = factor(trauma3, levels = c("none", "one", "multiple")),
  sch1 = factor(sch1, levels = c(0, 1)),
  loneliness01 = factor(loneliness01, levels = c(0, 1)),
  fights01 = factor(fights01, levels = c(0, 1)),
  run01 = factor(run01, levels = c(0,1)),
  everdrugs = factor(everdrugs, levels = c(0,1)),
  care1q = factor(care1q, levels = c("best", "very good", "good", "poor", "poorest")),
  care2q = factor(care2q, levels = c("best", "very good", "good", "poor", "poorest")),
  conflictq = factor(conflictq, levels = c("least conflict", "little conflict", "moderate conflict", "much conflict", "most conflict"))
)

mod1 = lrm(incidentMDD ~ sex + white + trauma3 + sch1 + loneliness01 + fights01 + 
             run01 + everdrugs + care1q + care2q + conflictq + 
             sex*white +
             sex*sch1 + sex*everdrugs + sex*loneliness01 + sex*fights01 +
             sex*run01 + sex*trauma3 + sex*care1q + sex*care2q + sex*conflictq,
           data = inc.complete, linear.predictors = T, maxit=1000, x=T, y=T)

## test penalties of different orders of magnitudes first to settle on a range
penalties = 10^seq(5, -2, length = 50)

## bootstrap pentrace to this range of penalties - doing it 1000 times
pen_out = data.frame()

n = nrow(mod1$x)

for (i in 1:1000) {
  p_bs = try( pentrace(mod1, penalty=penalties, maxit=1000, which = "aic.c", subset = sample(n, n, TRUE)) )
  
  if (class(p_bs)=="pentrace") {
  res = p_bs$results.all[which(p_bs$results.all$penalty==p_bs$penalty),]
  pen_out = rbind(pen_out, res)
  }
}

## remove any penalties that are greater than

plot = ggplot(data = pen_out, aes(x = penalty)) +
  geom_histogram(binwidth = 10, color="black", fill="white") +
  scale_y_continuous(n.breaks = 8, expand = expansion(mult = c(0, .03))) +
  scale_x_continuous(n.breaks = 10, expand = expansion(mult = c(0, .03)))

ggsave(plot,filename="rms_penalties_magnitude.png")

write.csv(pen_out, "rms_penalties_magnitude.csv")

## very few penalties below 1 or above 200
penalties_restrict = seq(1, 200, length=50)

pen_restrict_out = data.frame()

n = nrow(mod1$x)

for (i in 1:10) {
  p_bs = try(pentrace(mod1, penalty=penalties_restrict, maxit=1000, which = "aic.c", subset = sample(n, n, TRUE)))
  
  if (class(p_bs)=="pentrace") {
  res = p_bs$results.all[which(p_bs$results.all$penalty==p_bs$penalty),]
  pen_restrict_out = rbind(pen_restrict_out, res)
  }
}


plot_restrict = ggplot(data = pen_restrict_out, aes(x = penalty)) +
  geom_histogram(binwidth = 10, color="black", fill="white") +
  scale_y_continuous(n.breaks = 8, expand = expansion(mult = c(0, .03))) +
  scale_x_continuous(n.breaks = 10, expand = expansion(mult = c(0, .03)))

ggsave(plot_restrict,"rms_penalties_restrict1.png")

write.csv(pen_restrict_out, "rms_penalties_restrict1.csv")


## further restricting to between 1 and 50
penalties_restrict2 = seq(1, 50, length=50)

pen_restrict_out2 = data.frame()

n = nrow(mod1$x)

for (i in 1:10) {
  p_bs = try(pentrace(mod1, penalty=penalties_restrict2, maxit=1000, which = "aic.c", subset = sample(n, n, TRUE)))
  
  if (class(p_bs)=="pentrace") {
    res = p_bs$results.all[which(p_bs$results.all$penalty==p_bs$penalty),]
    pen_restrict_out2 = rbind(pen_restrict_out2, res)
  }
}

plot_restrict2 = ggplot(data = pen_restrict_out, aes(x = penalty)) +
  geom_histogram(binwidth = 10, color="black", fill="white") +
  scale_y_continuous(n.breaks = 8, expand = expansion(mult = c(0, .03))) +
  scale_x_continuous(n.breaks = 10, expand = expansion(mult = c(0, .03)))

ggsave(plot_restrict2,"rms_penalties_restrict2.png")

write.csv(pen_restrict_out2, "rms_penalties_restrict2.csv")