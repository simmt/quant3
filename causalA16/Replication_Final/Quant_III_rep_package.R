###https://github.com/simmt/quant3/tree/master/causalA16/Replication_Final
library(foreign)
library(sandwich)
library(lmtest)
library(stargazer)
library(countrycode)
library(boot)
library(car)
library(Matching)
library(causalA16)
library(ipw)
library(ggplot2)

Bes_Per<-read.dta("~/GitHub/quant3/causalA16/Replication_Final/AER.dta")
Bes_Per$region<-as.factor(countrycode(Bes_Per$countryname, "country.name",
                                      "region"))
Bes_Per$continent<-as.factor(countrycode(Bes_Per$countryname, "country.name",
                                         "continent"))

# Replace mincometaxrevenuegdp=mincometaxrevenuegdp/37.15
Bes_Per$mincometaxrevenuegdp<-Bes_Per$mincometaxrevenuegdp/37.15

# Replace mtradetax_share=1-(mtradetax_share/0.683)
Bes_Per$mtradetax_share<-1-(Bes_Per$mtradetax_share/0.683)

# Replace mtradeindirecttax_share = 1-(mtradeindirecttax_share/0.81) replace
Bes_Per$mtaxrevenue=Bes_Per$mtaxrevenue/50.89
Bes_Per$mtradeindirecttax_share<-1-(Bes_Per$mtradeindirecttax_share/0.81)

# Replace mcred = 1-(mcred/174)
Bes_Per$mcred<-1-(Bes_Per$mcred/174)

# Replace mpcy=mpcy/123.33
Bes_Per$mpcy<-Bes_Per$mpcy/123.33

# Replace mprot=1-(mprot/173)
Bes_Per$mprot<-1-(Bes_Per$mprot/173)

# Original replication tables:
reg1<-lm(mpcy ~ avg_war75 +avg_dem75+avg_demparl75+l_eng+l_soc+l_ger+l_sca,
         data = Bes_Per)
reg2<-lm(mcred ~ avg_war75 +avg_dem75+avg_demparl75+l_eng+l_soc+l_ger+l_sca,
         data = Bes_Per)
reg3<-lm(mprot ~ avg_war75 +avg_dem75+avg_demparl75+l_eng+l_soc+l_ger+l_sca,
         data = Bes_Per)
reg4<-lm(mgadp ~ avg_war75 +avg_dem75+avg_demparl75+l_eng+l_soc+l_ger+l_sca,
         data = Bes_Per)
reg5<-lm(mtradetax_share ~ avg_war75 +avg_dem75+avg_demparl75+l_eng+l_soc+l_ger+
           l_sca, data = Bes_Per)
reg6<-lm(mtradeindirecttax_share ~ avg_war75 +avg_dem75+avg_demparl75+l_eng+
           l_soc+l_ger+l_sca, data = Bes_Per)
reg7<-lm(mincometaxrevenuegdp ~ avg_war75 +avg_dem75+avg_demparl75+l_eng+l_soc+
           l_ger+l_sca, data = Bes_Per)
reg8<-lm(mtaxrevenue ~ avg_war75 +avg_dem75+avg_demparl75+
           l_eng+l_soc+l_ger+l_sca, data = Bes_Per)

# With robust standard errors; replication tables
reg1a<-coeftest(reg1, vcov = vcovHC(reg1, "HC1"))  # robust; HC1 (Stata default)
reg2a<-coeftest(reg2, vcov = vcovHC(reg2, "HC1"))  # robust; HC1 (Stata default)
reg3a<-coeftest(reg3, vcov = vcovHC(reg3, "HC1"))  # robust; HC1 (Stata default)
reg4a<-coeftest(reg4, vcov = vcovHC(reg4, "HC1"))  # robust; HC1 (Stata default)
reg5a<-coeftest(reg5, vcov = vcovHC(reg5, "HC1"))  # robust; HC1 (Stata default)
reg6a<-coeftest(reg6, vcov = vcovHC(reg6, "HC1"))  # robust; HC1 (Stata default)
reg7a<-coeftest(reg7, vcov = vcovHC(reg7, "HC1"))  # robust; HC1 (Stata default)
reg8a<-coeftest(reg8, vcov = vcovHC(reg8, "HC1"))  # robust; HC1 (Stata default)

# Table in Appendix
stargazer(reg1a, reg2a, reg3a, reg4a, reg5a, reg6a, reg7a, reg8a)

Bes_Per111<-na.omit(Bes_Per[, c("mpcy", "l_eng", "avg_war75", "avg_dem75",
                                "avg_demparl75", "l_soc", "l_ger", "l_sca")])
summary(as.factor(Bes_Per111$l_ger))
summary(as.factor(Bes_Per111$l_sca))

# Bootstrapping

reg1_bs<-Boot(reg1, R = 999)
reg2_bs<-Boot(reg1, R = 999)
reg3_bs<-Boot(reg1, R = 999)
reg4_bs<-Boot(reg1, R = 999)
reg5_bs<-Boot(reg1, R = 999)
reg6_bs<-Boot(reg1, R = 999)
reg7_bs<-Boot(reg1, R = 999)
reg8_bs<-Boot(reg1, R = 999)
stargazer(reg7_bs)
stargazer(confint(reg7_bs, level=.90, type="norm"))

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
### B&P causal story     ###
############################

# Dep variable: incometaxrevenuegdp
# Matching with English legal origin

replication2a <- na.omit(Bes_Per[,c("mincometaxrevenuegdp", "l_eng",
                                    "avg_war75", "avg_dem75", "avg_demparl75")])
Y <- replication2a$mincometaxrevenuegdp
Tr1 <- replication2a$l_eng
glm1 <- glm(Tr1 ~ avg_war75 + avg_dem75 + avg_demparl75, family = binomial,
            data = replication2a)
summary(glm1)
prop <- predict(glm1)
rr1 <- Match(Y = Y, Tr = Tr1, X = glm1$fitted, estimand="ATE")
summary(rr1)

# Matching with French legal origin

replication2b <- na.omit(Bes_Per[,c("mincometaxrevenuegdp", "l_fre",
                                    "avg_war75", "avg_dem75", "avg_demparl75")])
Y <- replication2b$mincometaxrevenuegdp
Tr2 <- replication2b$l_fre
glm4 <- glm(Tr2 ~ avg_war75 + avg_dem75 + avg_demparl75, family = binomial,
            data = replication2b)
summary(glm4)
prop <- predict(glm4)
rr4 <- Match(Y = Y, Tr = Tr2, X = glm4$fitted, estimand="ATE")
summary(rr4)

# Matching with Socialist legal origin

replication2 <- na.omit(Bes_Per[,c("mincometaxrevenuegdp", "l_soc",
                                   "avg_war75", "avg_dem75", "avg_demparl75")])
Y <- replication2$mincometaxrevenuegdp
Tr3 <- replication2$l_soc
glm5 <- glm(Tr3 ~ avg_war75 + avg_dem75 + avg_demparl75, family = binomial,
            data = replication2)
summary(glm5)
prop <- predict(glm5)
rr5 <- Match(Y = Y, Tr = Tr3, X = glm5$fitted, estimand="ATE")
summary(rr5)

############################
###Alternate Causal Story###
############################

# Add regional variable
Bes_Per$continent<-relevel(Bes_Per$continent, ref = "Europe")
Bes_Per$l_orig<-NA
Bes_Per$l_orig[Bes_Per$l_soc==1]<-"soc"
Bes_Per$l_orig[Bes_Per$l_eng==1]<-"eng"
Bes_Per$l_orig[Bes_Per$l_fre==1]<-"fre"
Bes_Per$l_orig[Bes_Per$l_ger==1]<-"ger"
Bes_Per$l_orig[Bes_Per$l_sca==1]<-"sca"
Bes_Per$l_orig<-as.factor(Bes_Per$l_orig)

# Add state antiquity variable
state_antiquity<-read.csv("~/GitHub/quant3/causalA16/Replication_Final/sratiov3-Table 1.csv")
names(state_antiquity)[1]<-paste("ncode")
Bes_Per_Eng_Fr_sa<-merge(Bes_Per, state_antiquity, by = c("ncode"))
Bes_Per_Eng_Fr_sa$region<-as.factor(countrycode(Bes_Per_Eng_Fr_sa$countryname,
                                                "country.name",  "region"))
Bes_Per_Eng_Fr_sa$continent<-as.factor(countrycode(Bes_Per_Eng_Fr_sa$countryname,
                                                   "country.name",  "continent"))

# Create dummy variable for our treatment
Bes_Per_Eng_Fr_sa$l_orig<-NA
Bes_Per_Eng_Fr_sa$l_orig[Bes_Per_Eng_Fr_sa$l_soc==1] <- 0
Bes_Per_Eng_Fr_sa$l_orig[Bes_Per_Eng_Fr_sa$l_eng==1] <- 0
Bes_Per_Eng_Fr_sa$l_orig[Bes_Per_Eng_Fr_sa$l_fre==1] <- 1
Bes_Per_Eng_Fr_sa$l_orig[Bes_Per_Eng_Fr_sa$l_ger==1] <- 0
Bes_Per_Eng_Fr_sa$l_orig[Bes_Per_Eng_Fr_sa$l_sca==1] <- 0
Bes_Per_Eng_Fr_sa$l_orig <- as.numeric(Bes_Per_Eng_Fr_sa$l_orig == 1)

# Merging Oceania with Asia
Bes_Per_Eng_Fr_sa$continent[Bes_Per_Eng_Fr_sa$continent=="Oceania"]<-"Asia"
Bes_Per_Eng_Fr_sa$continent <- factor(Bes_Per_Eng_Fr_sa$continent)

# Deleting missing values
Bes_Per_Eng_Fr_sa<-na.omit(Bes_Per_Eng_Fr_sa[, c("mincometaxrevenuegdp",
                                                 "l_orig", "l_soc", "l_eng",
                                                 "l_fre","l_ger","l_sca",
                                                 "continent", "statehistn00v3",
                                                 "region", "avg_war75",
                                                 "avg_dem75", "avg_demparl75")])
Bes_Per_Eng_Fr_sa$continent<-relevel(Bes_Per_Eng_Fr_sa$continent, ref = "Europe")

# Matching for alternate story
# Creating weights for matching
fitted_match_reg_hist<-glm(l_orig~continent+statehistn00v3, family = binomial,
                           data = Bes_Per_Eng_Fr_sa)
fitted_match_reg_hist1<-glm(l_eng~continent+statehistn00v3, family = binomial,
                            data = Bes_Per_Eng_Fr_sa)
fitted_match_reg_hist2<-glm(l_soc~continent+statehistn00v3, family = binomial,
                            data = Bes_Per_Eng_Fr_sa)
stargazer(fitted_match_reg_hist, fitted_match_reg_hist1, fitted_match_reg_hist2)

Yyy<-Bes_Per_Eng_Fr_sa$mincometaxrevenuegdp

# Note: France legal origin is now 1
Tr<-Bes_Per_Eng_Fr_sa$l_eng
Tr<-as.numeric(Bes_Per_Eng_Fr_sa$l_orig)
Tr1<-Bes_Per_Eng_Fr_sa$l_eng
Tr1<-as.numeric(Bes_Per_Eng_Fr_sa$l_eng)
Tr2<-Bes_Per_Eng_Fr_sa$l_soc
Tr2<-as.numeric(Bes_Per_Eng_Fr_sa$l_soc)

# Alternative measure, all vs. French
Xxx<-fitted_match_reg_hist$fitted.values
Xxx1<-fitted_match_reg_hist1$fitted.values
Xxx2<-fitted_match_reg_hist2$fitted.values
Bes_Per_Match <- Match(Y = Yyy, Tr = Tr, X = Xxx,
                       M=1, estimand = "ATE", Bes_Per_Eng_Fr_sa)
Bes_Per_Match1 <- Match(Y = Yyy, Tr = Tr1, X = Xxx1,
                        M=1, estimand = "ATE", Bes_Per_Eng_Fr_sa)
Bes_Per_Match2 <- Match(Y = Yyy, Tr = Tr2, X = Xxx2,
                        M=1, estimand = "ATE", Bes_Per_Eng_Fr_sa)

# Balance test for matching
Bes_Per_Match_test<-MatchBalance(l_orig~continent+statehistn00v3,
                                 match.out = Bes_Per_Match, nboots = 5000,
                                 data = Bes_Per_Eng_Fr_sa)
Bes_Per_Match_test1<-MatchBalance(l_eng~continent+statehistn00v3,
                                  match.out = Bes_Per_Match1, nboots = 5000,
                                  data = Bes_Per_Eng_Fr_sa)
Bes_Per_Match_test2<-MatchBalance(l_soc~continent+statehistn00v3,
                                  match.out = Bes_Per_Match2, nboots = 5000,
                                  data = Bes_Per_Eng_Fr_sa)

# Inverse propensity weigthing (function)
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
                               vars = c("continent", "statehistn00v3"),
                               dat = Bes_Per_Eng_Fr_sa, stabilized = FALSE)
dat$weights <- weight_test

# Original model with weights
original_weighted<-lm(mincometaxrevenuegdp~l_orig+avg_war75+avg_dem75+
                        avg_demparl75, weights = weights, data = dat)
# Original model plus our covariates and weights
with_covars<-lm(mincometaxrevenuegdp~l_orig+avg_war75+avg_dem75+avg_demparl75+
                  statehistn00v3+continent, weights = weights, data = dat)
stargazer(original_weighted, with_covars)

# Other model specifications that hold
orig_paper_no_alt<-lm(mincometaxrevenuegdp~l_eng+l_soc
                      +l_sca+ l_ger+avg_war75+
                        avg_dem75+avg_demparl75, data = dat)
orig_paper_with_covars_and_weights<-lm(mincometaxrevenuegdp~l_eng+l_soc+
                                         +l_sca+ l_ger+avg_war75+
                                         avg_dem75+avg_demparl75+
                                         statehistn00v3+continent,
                                       weights = weights, data = dat)

# Appendix: Matching
# Matching Balance for English legal origin
MatchBalance(Tr1 ~ avg_war75 + avg_dem75 + avg_demparl75, match.out = rr1,
             nboots=1000, data=replication2a)
# Matching Balance for French legal origin
MatchBalance(Tr2 ~ avg_war75 + avg_dem75 + avg_demparl75, match.out = rr4,
             nboots=1000, data=replication2b)
# Matching Balance for Socialist legal origin
MatchBalance(Tr3 ~ avg_war75 + avg_dem75 + avg_demparl75, match.out = rr5,
             nboots=1000, data=replication2)

# Appendix: Weights graph
ggplot(dat, aes(weights)) + geom_histogram(bins = 100) +
  theme_bw() +
  facet_wrap(~ l_orig, ncol = 1, scales = "free_y")
range(dat$weights)
nrow(dat)
