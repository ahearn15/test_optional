# Project: 565 Final Proj
# Author: Adam Hearn
# Last edited: Mon May 4 2020


## Import Modules
library(tidyverse)
library(reshape2) # for melt
library("ggplot2")
library(plyr)
library(rlang)



#setwd("/Users/adamhearn/Desktop/sp2020/565/final_proj")

# Figure 1
dta <- read.csv("to_cumulative.csv")
ggplot(data=dta, aes(x=year, y=cum_schools, group=1)) +
  geom_line() +
  theme_classic() +
  xlab("") + ylab("Number of schools test-optional") +
  geom_vline(aes(xintercept = 2020), linetype = "dashed") +
  #scale_color_manual(name = "", values = c("covid-19" = "black")) + theme(legend.position = "bottom") +
  labs(caption = "Source: FairTest.org")

#Figures 2, 3
var_imp = read.csv("var_imp.csv")
ggplot(var_imp) +
  geom_bar(aes(x = reorder(Variable, Importance), y = Importance), fill = "#041E42", stat = "identity") +
  coord_flip() +
  ylab("Importance") + xlab("")  +   
  scale_y_continuous(limits = c(0,.31), expand = c(0,0)) + scale_x_discrete(expand = c(0,1)) +
  labs(caption = "n:
                  Test-optional:") +
  theme_classic() +
  ggtitle("") +
  theme(axis.text.y = element_text(size=10, color = "black"),
        axis.text.x = element_text(size=10, color = "black"),
        plot.caption = element_text(size=10, color="black"),
        plot.title = element_text(hjust=0.5, size=rel(1.2)))

#Figure 4
classes <- c("Most competitive", "Highly competitive", "Very competitive", "Competitive", "Less competitive")
"Pct. minority" <- c(0.380278	,0.070995,	0.028211,	0.039037,	0.112308)
"Pct. lowinc" <- c(0.266946,	0.175823,	0.117229,	0.158652,	0.083267)

minority <- data.frame(classes, `Pct. minority`,`Pct. lowinc`)
minority$classes <- factor(minority$classes, levels = minority$classes)

data_long = as_tibble(melt(minority, id="classes"))

ggplot(data = data_long, aes(x = classes, y = value, group = variable, linetype = variable)) +
  geom_line() +
  geom_point() +
  ylab("Importance") +
  theme_classic() +
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_linetype_discrete(name = "", labels = c("Pct. minority", "Pct. low-income"))+ 
  xlab("Barron's classification")

#Figure 5
"Yield rate" <- c(0.082064,	0.259379,	0.135245,	0.180812,	0.127432)
"Adm. rate" <- c(0,	0.003794,	0.182135,	0.097672,	0.269411)

adm <- data.frame(classes, `Yield rate`,`Adm. rate`)
adm$classes <- factor(adm$classes, levels = adm$classes)

data_long = as_tibble(melt(adm, id="classes"))


ggplot(data = data_long, aes(x = classes, y = value, group = variable, linetype = variable)) +
  geom_line() +
  geom_point() +
  ylab("Importance") +
  theme_classic() +
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_linetype_discrete(name = "", labels = c("Yield rate", "Adm. rate"))+ 
  xlab("Barron's classification")


#diff in means (Table 3, 4)
dta_priv <- dta %>% 
  filter(private == 1)

dta_pub <- dta %>% 
  filter(private == 0)

mod = lm(endowment_end ~ test_optional, data = dta_priv)
broom::tidy(mod)

mod = lm(total_enrollment ~ test_optional, data = dta_priv)
broom::tidy(mod)

mod = lm(retention_rate ~ test_optional, data = dta_priv)
broom::tidy(mod)

mod = lm(completion_rate_6yr ~ test_optional, data = dta_priv)
broom::tidy(mod)

mod = lm(adm_rate ~ test_optional, data = dta_priv)
broom::tidy(mod)

mod = lm(female_pct ~ test_optional, data = dta_priv)
broom::tidy(mod)

mod = lm(yield_rate ~ test_optional, data = dta_priv)
broom::tidy(mod)

mod = lm(endowment_end ~ test_optional, data = dta_pub)
broom::tidy(mod)

mod = lm(lowincome_pct ~ test_optional, data = dta_pub)
broom::tidy(mod)

mod = lm(perc_minority ~ test_optional, data = dta_pub)
broom::tidy(mod)


mod = lm(Pacific ~ test_optional, data = dta_pub)
broom::tidy(mod)


mod = lm(adm_rate ~ test_optional, data = dta_pub)
broom::tidy(mod)
gsdf
mod = lm(total_enrollment ~ test_optional, data = dta_pub)
broom::tidy(mod)

mod = lm(female_pct ~ test_optional, data = dta_pub)
broom::tidy(mod)

mod = lm(student_faculty_ratio ~ test_optional, data = dta_pub)
broom::tidy(mod)
