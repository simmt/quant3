library(foreign)
library(sandwich)
library(lmtest)
print(Bes_Per)
Bes_Per<-read.dta("~/GitHub/quant3/causalA16/inst/extdata/Besley/AER.dta")
attach(Bes_Per)
#replace mincometaxrevenuegdp=mincometaxrevenuegdp/37.15
mincometaxrevenuegdp<-mincometaxrevenuegdp/37.15
#replace mtradetax_share=1-(mtradetax_share/0.683)
mtradetax_share<-1-(mtradetax_share/0.683)
#replace mtradeindirecttax_share = 1-(mtradeindirecttax_share/0.81) replace mtaxrevenue=mtaxrevenue/50.89
mtradeindirecttax_share<-1-(mtradeindirecttax_share/0.81)
#replace mcred = 1-(mcred/174)
mcred<-1-(mcred/174)
#replace mpcy=mpcy/123.33
mpcy<-mpcy/123.33
#replace mprot=1-(mprot/173)
mprot<-1-(mprot/173)

#Table 1:
  #reg mpcy $politics75 $legal, robust
#reg mcred $politics75 $legal, robust
#reg mprot $politics75 $legal, robust
#reg mgadp $politics75 $legal, robust
#Table 2:
  #reg mtradetax_share $politics75 $legal, robust
#reg mtradeindirecttax_share $politics75 $legal, robust
#reg mincometaxrevenuegdp $politics75 $legal, robust
#reg mtaxrevenue $politics75 $legal, robust
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
