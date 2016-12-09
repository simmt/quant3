library(foreign)
library(sandwich)
library(lmtest)
library(stargazer)

Bes_Per<-read.dta("~/GitHub/quant3/causalA16/inst/extdata/Besley/AER.dta")
library(countrycode)
Bes_Per$region<-as.factor(countrycode(Bes_Per$countryname, "country.name",  "region"))
Bes_Per$continent<-as.factor(countrycode(Bes_Per$countryname, "country.name",  "continent"))

#replace mincometaxrevenuegdp=mincometaxrevenuegdp/37.15
Bes_Per$mincometaxrevenuegdp<-Bes_Per$mincometaxrevenuegdp/37.15
#replace mtradetax_share=1-(mtradetax_share/0.683)
Bes_Per$mtradetax_share<-1-(Bes_Per$mtradetax_share/0.683)
#replace mtradeindirecttax_share = 1-(mtradeindirecttax_share/0.81) replace mtaxrevenue=mtaxrevenue/50.89
Bes_Per$mtradeindirecttax_share<-1-(Bes_Per$mtradeindirecttax_share/0.81)
#replace mcred = 1-(mcred/174)
Bes_Per$mcred<-1-(Bes_Per$mcred/174)
#replace mpcy=mpcy/123.33
Bes_Per$mpcy<-Bes_Per$mpcy/123.33
#replace mprot=1-(mprot/173)
Bes_Per$mprot<-1-(Bes_Per$mprot/173)

#Table 1:
#Table 2:

reg1<-lm(mpcy ~ avg_war75 +avg_dem75+avg_demparl75+
           l_eng+l_soc+l_ger+l_sca, data = Bes_Per)
reg2<-lm(mcred ~ avg_war75 +avg_dem75+avg_demparl75+
           l_eng+l_soc+l_ger+l_sca, data = Bes_Per)
reg3<-lm(mprot ~ avg_war75 +avg_dem75+avg_demparl75+
           l_eng+l_soc+l_ger+l_sca, data = Bes_Per)
reg4<-lm(mgadp ~ avg_war75 +avg_dem75+avg_demparl75+
           l_eng+l_soc+l_ger+l_sca, data = Bes_Per)
reg5<-lm(mtradetax_share ~ avg_war75 +avg_dem75+avg_demparl75+
           l_eng+l_soc+l_ger+l_sca, data = Bes_Per)
reg6<-lm(mtradeindirecttax_share ~ avg_war75 +avg_dem75+avg_demparl75+
           l_eng+l_soc+l_ger+l_sca, data = Bes_Per)
reg7<-lm(mincometaxrevenuegdp ~ avg_war75 +avg_dem75+avg_demparl75+
           l_eng+l_soc+l_ger+l_sca, data = Bes_Per)
reg8<-lm(mtaxrevenue ~ avg_war75 +avg_dem75+avg_demparl75+
           l_eng+l_soc+l_ger+l_sca, data = Bes_Per)
#with robust SE
reg1a<-coeftest(reg1, vcov = vcovHC(reg1, "HC1"))    # robust; HC1 (Stata default)
reg2a<-coeftest(reg2, vcov = vcovHC(reg2, "HC1"))    # robust; HC1 (Stata default)
reg3a<-coeftest(reg3, vcov = vcovHC(reg3, "HC1"))    # robust; HC1 (Stata default)
reg4a<-coeftest(reg4, vcov = vcovHC(reg4, "HC1"))    # robust; HC1 (Stata default)
reg5a<-coeftest(reg5, vcov = vcovHC(reg5, "HC1"))    # robust; HC1 (Stata default)
reg6a<-coeftest(reg6, vcov = vcovHC(reg6, "HC1"))    # robust; HC1 (Stata default)
reg7a<-coeftest(reg7, vcov = vcovHC(reg7, "HC1"))    # robust; HC1 (Stata default)
reg8a<-coeftest(reg8, vcov = vcovHC(reg8, "HC1"))    # robust; HC1 (Stata default)

stargazer(reg1a, reg2a, reg3a, reg4a, reg5a, reg6a, reg7a, reg8a)

Bes_Per111<-na.omit(Bes_Per[, c("mpcy", "l_eng", "avg_war75", "avg_dem75", "avg_demparl75",
                                "l_soc", "l_ger", "l_sca")])
summary(as.factor(Bes_Per111$l_ger))
summary(as.factor(Bes_Per111$l_sca))

#library(boot)
#results <- boot(data=Bes_Per, R=1000, statistic = rsq, formula=mpcy ~ avg_war75 +avg_dem75+avg_demparl75+
#                  l_eng+l_soc+l_ger+l_sca)

library(car)
reg1_bs<-Boot(reg1, R = 999)
reg2_bs<-Boot(reg2, R = 999)
reg3_bs<-Boot(reg3, R = 999)
reg4_bs<-Boot(reg4, R = 999)
reg5_bs<-Boot(reg5, R = 999)
reg6_bs<-Boot(reg6, R = 999)
reg7_bs<-Boot(reg7, R = 999)
reg8_bs<-Boot(reg8, R = 999)
stargazer(reg7_bs)
stargazer(confint(reg7_bs, level=.90, type="norm"))


#####
reg1<-lm(mpcy ~ avg_war75 +avg_dem75+avg_demparl75+
           l_eng+l_soc+l_ger+l_sca, data = Bes_Per)
reg1_bs<-Boot(reg1, R = 999)
confint(reg1_bs, level=.90, type="norm")
confint(reg2_bs, level=.90, type="norm")
confint(reg3_bs, level=.90, type="norm")
confint(reg4_bs, level=.90, type="norm")
confint(reg5_bs, level=.90, type="norm")
confint(reg6_bs, level=.90, type="norm")
confint(reg7_bs, level=.90, type="norm")
confint(reg8_bs, level=.90, type="norm")


############################
###Alternate Causal Story###
############################

####Add regional
Bes_Per$continent<-relevel(Bes_Per$continent, ref = "Europe")
Bes_Per$l_orig<-NA
Bes_Per$l_orig[Bes_Per$l_soc==1]<-"soc"
Bes_Per$l_orig[Bes_Per$l_eng==1]<-"eng"
Bes_Per$l_orig[Bes_Per$l_fre==1]<-"fre"
Bes_Per$l_orig[Bes_Per$l_ger==1]<-"ger"
Bes_Per$l_orig[Bes_Per$l_sca==1]<-"sca"
Bes_Per$l_orig<-as.factor(Bes_Per$l_orig)

####add state antiquity
state_antiquity<-read.csv("~/Downloads/state_antiquity/sratiov3-Table 1.csv")
names(state_antiquity)[1]<-paste("ncode")

Bes_Per_Eng_Fr_sa<-merge(Bes_Per, state_antiquity, by = c("ncode"))
Bes_Per_Eng_Fr_sa$region<-as.factor(countrycode(Bes_Per_Eng_Fr_sa$countryname, "country.name",  "region"))
Bes_Per_Eng_Fr_sa$continent<-as.factor(countrycode(Bes_Per_Eng_Fr_sa$countryname, "country.name",  "continent"))


#####bootstraps
####for alt causal story
reg_alt<-lm(mincometaxrevenuegdp ~ avg_war75 +avg_dem75+avg_demparl75+
              l_eng+l_soc+l_ger+l_sca+continent+statehistn00v3, data = Bes_Per_Eng_Fr_sa)
reg_alt_a<-coeftest(reg_alt, vcov = vcovHC(reg_alt, "HC1"))    # robust; HC1 (Stata default)
reg_alt_a

reg_alt_bs<-Boot(reg_alt, R = 999)
reg_alt_bs_90<-confint(reg_alt_bs, level=.90, type="norm")
stargazer(reg_alt_bs_90)


#####on/off switch for which data set to pull from
#if turn on, then we are using only english and french
#Bes_Per_Eng_Fr_sa<-Bes_Per_Eng_Fr_sa[Bes_Per_Eng_Fr_sa$l_orig=="eng"|Bes_Per_Eng_Fr_sa$l_orig=="fre",]
###turn off if above is on
Bes_Per_Eng_Fr_sa$l_orig<-NA
Bes_Per_Eng_Fr_sa$l_orig[Bes_Per_Eng_Fr_sa$l_soc==1]<- 0
Bes_Per_Eng_Fr_sa$l_orig[Bes_Per_Eng_Fr_sa$l_eng==1] <- 0
Bes_Per_Eng_Fr_sa$l_orig[Bes_Per_Eng_Fr_sa$l_fre==1] <- 1
Bes_Per_Eng_Fr_sa$l_orig[Bes_Per_Eng_Fr_sa$l_ger==1] <- 0
Bes_Per_Eng_Fr_sa$l_orig[Bes_Per_Eng_Fr_sa$l_sca==1] <- 0
Bes_Per_Eng_Fr_sa$l_orig <- as.numeric(Bes_Per_Eng_Fr_sa$l_orig == 1)

###note now can make it E, W or S ... to get rid of Oceania problem
Bes_Per_Eng_Fr_sa$continent[Bes_Per_Eng_Fr_sa$continent=="Oceania"]<-"Asia"
Bes_Per_Eng_Fr_sa$continent <- factor(Bes_Per_Eng_Fr_sa$continent)

#Bes_Per_Eng_Fr_sa$continent[Bes_Per_Eng_Fr_sa$continent=="Africa"]<-"South"
#Bes_Per_Eng_Fr_sa$continent[Bes_Per_Eng_Fr_sa$continent=="Americas"]<-"West"
#Bes_Per_Eng_Fr_sa$continent[Bes_Per_Eng_Fr_sa$continent=="Asia"]<-"East"
summary(Bes_Per_Eng_Fr_sa$continent)



###
###
###
#Bes_Per_Eng_Fr_sa<-na.omit(Bes_Per_Eng_Fr_sa[, c("mincometaxrevenuegdp", "l_orig", "continent", "statehistn00v3", "region", "avg_war75", "avg_dem75", "avg_demparl75")])
nrow(Bes_Per_Eng_Fr_sa)

Bes_Per_Eng_Fr_sa$l_orig[Bes_Per_Eng_Fr_sa$l_soc==1]<- 0
Bes_Per_Eng_Fr_sa$l_orig[Bes_Per_Eng_Fr_sa$l_eng==1] <- 0
Bes_Per_Eng_Fr_sa$l_orig[Bes_Per_Eng_Fr_sa$l_fre==1] <- 1
Bes_Per_Eng_Fr_sa$l_orig[Bes_Per_Eng_Fr_sa$l_ger==1] <- 0
Bes_Per_Eng_Fr_sa$l_orig[Bes_Per_Eng_Fr_sa$l_sca==1] <- 0

Bes_Per_Eng_Fr_sa<-na.omit(Bes_Per_Eng_Fr_sa[, c("mincometaxrevenuegdp",
                                                 "mtradetax_share", "mtradeindirecttax_share",
                                                 "mtaxrevenue",
                                                 "l_orig", "l_eng", "l_soc", "continent",
                                                 "statehistn00v3", "region", "avg_war75",
                                                 "avg_dem75", "avg_demparl75")])

####

Bes_Per_Eng_Fr_sa$continent<-relevel(Bes_Per_Eng_Fr_sa$continent, ref = "Europe")

#t.test(Bes_Per_Eng_Fr_sa$statehistn00v3[Bes_Per_Eng_Fr_sa$l_orig==1], Bes_Per_Eng_Fr_sa$statehistn00v3[Bes_Per_Eng_Fr_sa$l_orig==0])

#####
fitted_match_reg_hist<-glm(l_orig~continent+statehistn00v3, family = binomial, data = Bes_Per_Eng_Fr_sa)
fitted_match_reg_hist1<-glm(l_eng~continent+statehistn00v3, family = binomial, data = Bes_Per_Eng_Fr_sa)
fitted_match_reg_hist2<-glm(l_soc~continent+statehistn00v3, family = binomial, data = Bes_Per_Eng_Fr_sa)

stargazer(fitted_match_reg_hist, fitted_match_reg_hist1, fitted_match_reg_hist2)

#####MATCHING FRENCH######
Yyy<-Bes_Per_Eng_Fr_sa$mincometaxrevenuegdp
##note: Frence legal origin is now 1
Tr<-Bes_Per_Eng_Fr_sa$l_eng
Tr<-as.numeric(Bes_Per_Eng_Fr_sa$l_orig)

Tr1<-Bes_Per_Eng_Fr_sa$l_eng
Tr1<-as.numeric(Bes_Per_Eng_Fr_sa$l_eng)

Tr2<-Bes_Per_Eng_Fr_sa$l_soc
Tr2<-as.numeric(Bes_Per_Eng_Fr_sa$l_soc)

###alt measure, all vs. French
Xxx<-fitted_match_reg_hist$fitted.values
Xxx1<-fitted_match_reg_hist1$fitted.values
Xxx2<-fitted_match_reg_hist2$fitted.values
###kicks back 1/1000 percent shift
library(Matching)
Bes_Per_Match <- Match(Y = Yyy, Tr = Tr, X = Xxx,
                       M=1, estimand = "ATE", Bes_Per_Eng_Fr_sa)
Bes_Per_Match1 <- Match(Y = Yyy, Tr = Tr1, X = Xxx1,
                        M=1, estimand = "ATE", Bes_Per_Eng_Fr_sa)
Bes_Per_Match2 <- Match(Y = Yyy, Tr = Tr2, X = Xxx2,
                        M=1, estimand = "ATE", Bes_Per_Eng_Fr_sa)
summary(Bes_Per_Match2)


Bes_Per_Match_test<-MatchBalance(l_orig~continent+statehistn00v3, match.out = Bes_Per_Match, nboots = 5000, data = Bes_Per_Eng_Fr_sa)
Bes_Per_Match_test1<-MatchBalance(l_eng~continent+statehistn00v3, match.out = Bes_Per_Match1, nboots = 5000, data = Bes_Per_Eng_Fr_sa)
Bes_Per_Match_test2<-MatchBalance(l_soc~continent+statehistn00v3, match.out = Bes_Per_Match2, nboots = 5000, data = Bes_Per_Eng_Fr_sa)


#####now inverse propensity weigthing

library(causalA16)
library(ipw)
library(ggplot2)

make_ip_weights <- function(tr, vars, dat, stabilized = FALSE)
{
  if(stabilized == FALSE)
  {
    fmla <- as.formula(paste(tr, " ~ ", paste(vars, collapse= "+")))
    mod <- glm(formula = fmla, data = dat, family = "binomial")
    dat$prop_score <- predict(mod, type = "response")
    weights <- ifelse(dat[, c(tr)] == 1, 1/dat$prop_score,
                      1/(1 - dat$prop_score))
    return(weights)
  }
  if(stabilized == TRUE)
  {
    prob1 <- (sum(dat[, c(tr)] == 1)) / (nrow(dat))
    prob0 <- (sum(dat[, c(tr)] == 0)) / (nrow(dat))
    fmla <- as.formula(paste(tr, " ~ ", paste(vars, collapse= "+")))
    mod <- glm(formula = fmla, data = dat, family = "binomial")
    dat$prop_score <- predict(mod, type = "response")
    weights <- ifelse(dat[, c(tr)] == 1, prob1/dat$prop_score,
                      prob0/(1 - dat$prop_score))
    weights
  }
}

vars <- c("continent", "statehistn00v3")
tr <- "l_orig"
dat <- Bes_Per_Eng_Fr_sa

weight_test <- make_ip_weights(tr = "l_orig",
                               vars = c("continent", "statehistn00v3"), dat = Bes_Per_Eng_Fr_sa,
                               stabilized = FALSE)
dat$weights <- weight_test
library(ggplot2)
ggplot(dat, aes(weights)) + geom_histogram(bins = 100) +
  theme_bw() +
  facet_wrap(~ l_orig, ncol = 1, scales = "free_y")
range(dat$weights)
nrow(dat)

###original
summary(lm(mincometaxrevenuegdp~l_orig+avg_war75+avg_dem75+avg_demparl75, weights = weights, data = dat))
##with our shit added
summary(lm(mincometaxrevenuegdp~l_orig+avg_war75+avg_dem75+avg_demparl75+statehistn00v3+continent, weights = weights, data = dat))
summary(lm(mincometaxrevenuegdp~l_orig+avg_war75+avg_dem75+avg_demparl75+statehistn00v3+continent, data = dat))



######

#"mincometaxrevenuegdp", "mgadp",
#"mtradetax_share", "mtradeindirecttax_share",
#"mtradeindirecttax_share",
#"mprot", "mcred",

#####
summary(lm(mtradetax_share~l_orig+avg_war75+avg_dem75+avg_demparl75, data = dat))
summary(lm(mtradetax_share~l_orig+avg_war75+avg_dem75+avg_demparl75, weights = weights, data = dat))

summary(lm(mtradeindirecttax_share~l_orig+avg_war75+avg_dem75+avg_demparl75, weights = weights, data = dat))
summary(lm(mtradeindirecttax_share~l_orig+avg_war75+avg_dem75+avg_demparl75, data = dat))




###main findings
###
###
summary(lm(mincometaxrevenuegdp~l_orig+avg_war75+avg_dem75+avg_demparl75+statehistn00v3+continent, weights = weights, data = dat))
summary(lm(mincometaxrevenuegdp~l_orig+avg_war75+avg_dem75+avg_demparl75, weights = weights, data = dat))
reg_minc<-lm(mincometaxrevenuegdp~l_orig+avg_war75+avg_dem75+avg_demparl75, data = dat)
summary(reg_minc)
reg_minc_1<-coeftest(reg_minc, vcov = vcovHC(reg_minc, "HC1"))    # robust; HC1 (Stata default)
reg_minc_1


dat$statehist_bin<-NA
dat$statehist_bin[dat$statehistn00v3>median(dat$statehistn00v3)]<-0
dat$statehist_bin[dat$statehistn00v3<=median(dat$statehistn00v3)]<-1
summary(lm(mincometaxrevenuegdp~l_orig+avg_war75+avg_dem75+avg_demparl75+statehist_bin+continent,  data = dat))

###
###
###

summary(lm(mtaxrevenue~l_orig+avg_war75+avg_dem75+avg_demparl75, weights = weights, data = dat))
summary(lm(mtaxrevenue~l_orig+avg_war75+avg_dem75+avg_demparl75, data = dat))
