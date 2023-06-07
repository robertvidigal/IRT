# # --------------------------------------------------------------------------------------
# VII Workshop de Comportamento Político e Opinião Pública. 7 de Junho de 2023. 
# Minicurso de Teoria de Resposta ao Item (IRT). 
# Robert Vidigal, PhD. Center for Social Media and Politics, NYU.
# # --------------------------------------------------------------------------------------
rm(list=ls()); gc(full=T); objects() 
set.seed(10012)

# Data Collected in Sept 2018.
gmo<-read.csv("~/Documents/IRT/data/GMO_MTurk_WORK-CLEAN.csv", header=TRUE)
#write.csv(gmo, "~/Documents/IRT-Workshop/GMO_MTurk_WORK-CLEAN.csv", row.names=F)
str(gmo); names(gmo)

# # --------------------------------------------------------------------------------------
# VARIABLES 
# # --------------------------------------------------------------------------------------
# Party ID (dummy)
table(gmo$dem)
table(gmo$ind)
table(gmo$rep)

# Ideology (0 Ext Left --- Ext Right 7)
table(gmo$ideo)

# Gender
table(gmo$female) # Women = 1

# Education
table(gmo$edu) # (1) No high school to Doctoral Degree (8)
table(gmo$collegeabove) # college and above = 1

# # --------------------------------------------------------------------------------------
# Conhecimento Politico
# # --------------------------------------------------------------------------------------
# Medical marijuana use is legal in over 30 US states. (T)
table(gmo$pk_medmarij); table(gmo$pkT_medmarij) 

# The Trans-Pacific Partnership (TPP) has effectively replaced the NAFTA in 2017. (F)
table(gmo$pk_nafta); table(gmo$pkF_nafta) 

# The U.S. is the only nation that have not ratified the Paris Agreement. (T)
table(gmo$pk_parisagree); table(gmo$pkT_parisagree) 

# The Guantanamo Bay Detention Center has been closed in Obama's administration. (F)
table(gmo$pk_guantanamo); table(gmo$pkF_guantanamo) 

# The US Gross Domestic Product (GDP) grew in the last quarter of 2018. (T)
table(gmo$pk_gdpgrow); table(gmo$pkT_gdpgrow) 

# The projected US federal budget deficit for 2018 is $585 billions. (F)
table(gmo$pk_deficit2018); table(gmo$pkF_deficit2018) 

# ADDITIVE SCALE
# # --------------------------------------------------------------------------------------
gmo$polknowTF<-(gmo$pk_medmarij+gmo$pk_nafta+gmo$pk_parisagree+
                            gmo$pk_guantanamo+gmo$pk_gdpgrow+gmo$pk_deficit2018)
table(gmo$polknowTF)

gmo$polknowMC<-(gmo$pkT_medmarij+gmo$pkF_nafta+gmo$pkT_parisagree+
                            gmo$pkF_guantanamo+gmo$pkT_gdpgrow+gmo$pkF_deficit2018)
table(gmo$polknowMC)

### SUBSETS
# # --------------------------------------------------------------------------------------
polMC<-subset(gmo, select=c(pk_medmarij, pk_nafta, pk_parisagree, 
                            pk_guantanamo, pk_deficit2018, pk_gdpgrow, polknowMC))
psych::describe(polMC); polMC<-na.omit(polMC)

polTF<-subset(gmo, select=c(pkT_medmarij, pkF_nafta, pkT_parisagree, 
                            pkF_guantanamo, pkF_deficit2018, pkT_gdpgrow, polknowTF))
psych::describe(polTF); polTF<-na.omit(polTF)

### CORRELATION TABLE
# # --------------------------------------------------------------------------------------
pol.corMC <- cor(polMC, use="complete.obs")
pol.corMC

pol.corTF <- cor(polTF, use="complete.obs")
pol.corTF

# # --------------------------------------------------------------------------------------
# ITEM EXAM
# # --------------------------------------------------------------------------------------
# Let's obtain some basic descriptive information about the items. All this information is
# given by the **psychometric** package.
psychometric::item.exam(polMC[,1:6], discrim=TRUE, y=polMC[,7])
psychometric::item.exam(polTF[,1:6], discrim=TRUE, y=polTF[,7])

psychometric::alpha(polMC)
psychometric::alpha(polTF)

# Item.total:  correlation between items and the total score.
# Item.Tot.woi: correlation between items and the total score by omitting the item.
# Difficulty: proportion of correct responses.
# Discrimination: values above 0.40 are satisfactory and values below 0.19 suggest removal,

?psychometric::item.exam # For each column value

polMC<-subset(gmo, select=c(pk_medmarij, pk_nafta, pk_parisagree, 
                            pk_guantanamo, pk_deficit2018, pk_gdpgrow))
psych::describe(polMC); polMC<-na.omit(polMC)

polTF<-subset(gmo, select=c(pkT_medmarij, pkF_nafta, pkT_parisagree, 
                            pkF_guantanamo, pkF_deficit2018, pkT_gdpgrow))
psych::describe(polTF); polTF<-na.omit(polTF)

# # --------------------------------------------------------------------------------------
# One-parameter IRT models (1-PL a.k.a. the Rasch Model)
# # --------------------------------------------------------------------------------------

# ltm package

# Discrimination fixed at 1 (not allowed to vary)
fitMC <- ltm::rasch(polMC, constraint=cbind(ncol(polMC) +1, 1)) # N of cols, +1, 1 (fixed)
summary(fitMC) # 'value' column is each item difficulty + std.err

#fitTF <- ltm::rasch(polTF, constraint=cbind(ncol(polTF) +1, 1)) # N of cols + 1, 1 (fixed)
#summary(fitTF) # 'value' column is each item difficulty + std.err

# Model Summary: logLik, AIC, and BIC are model fit stats

# TRANSFORMING PK TF FOR RASH MODEL
# # --------------------------------------------------------------------------------------
require(dplyr)
gmo <- gmo %>% mutate(pkT_medmarij_01=pkT_medmarij, 
                      pkF_nafta_01=pkF_nafta, 
                      pkT_parisagree_01=pkT_parisagree,
                      pkF_guantanamo_01=pkF_guantanamo, 
                      pkT_gdpgrow_01=pkT_gdpgrow, 
                      pkF_deficit2018_01=pkF_deficit2018) %>% 
  mutate_at(c("pkT_medmarij_01","pkF_nafta_01", "pkT_parisagree_01", "pkF_guantanamo_01",
                           "pkT_gdpgrow_01", "pkF_deficit2018_01"), 
            ~as.numeric(dplyr::recode(., `-2`=0, `-1`=0, `1`=1, `2`=1)))

polTF_01<-subset(gmo, select=c(pkT_medmarij_01, pkF_nafta_01, pkT_parisagree_01, 
                            pkF_guantanamo_01, pkF_deficit2018_01, pkT_gdpgrow_01))
psych::describe(polTF_01); polTF<-na.omit(polTF_01)

fitTF <- ltm::rasch(polTF_01, constraint=cbind(length(polTF_01) +1, 1)) # N of cols + 1, 1 (fixed)
summary(fitTF) # 'value' column is each item difficulty + std.err

# ICC plot
# # --------------------------------------------------------------------------------------
# the slope parameter is missing, which implies that all slopes are the same.
plot(fitMC, type=c("ICC")) 
plot(fitTF, type=c("ICC")) 
# they are parallel because model estimation forced them to be

# lowest to highest difficulty coefs
coef(fitMC, prob=TRUE, order=TRUE) 
coef(fitTF, prob=TRUE, order=TRUE) 

# Test of absolute fit: we want to fail to reject the null.
# # --------------------------------------------------------------------------------------
# Null: the model fits the data (chi-square test)
ltm::GoF.rasch(fitMC, B=1000)
ltm::GoF.rasch(fitTF, B=1000)

ltm::item.fit(fitMC, simulate.p.value = T)
ltm::item.fit(fitTF, simulate.p.value = T)

# Latent trait estimates for Rasch: theta scores for each individual in the sample.
# # --------------------------------------------------------------------------------------
theta.raschMC<-ltm::factor.scores(fitMC)
plot(theta.raschMC)
summary(theta.raschMC$score.dat$z1)
sqrt(var(theta.raschMC$score.dat$z1))

theta.raschTF<-ltm::factor.scores(fitTF)
plot(theta.raschTF)
summary(theta.raschTF$score.dat$z1)
sqrt(var(theta.raschTF$score.dat$z1))

# # --------------------------------------------------------------------------------------
# 1-PL Model
fit1MC <- ltm::rasch(polMC)
summary(fit1MC)

# # --------------------------------------------------------------------------------------
# Two-parameter IRT model (2-PL)
# # --------------------------------------------------------------------------------------
fit2MC <- ltm::ltm(polMC ~ z1)
summary(fit2MC)  # it provides item difficulty and item discrimination
coef(fit2MC, order = TRUE) 
# these results show that some items have really different discrimination parameters, 
# which affects item difficulties.

fit2TF <- ltm::ltm(polTF ~ z1)
summary(fit2TF)  # it provides item difficulty and item discrimination
coef(fit2TF, order = TRUE) 

# Plot Item Characteristic Curves for two-parameter model
# # --------------------------------------------------------------------------------------
par(mfrow=c(2,1)); plot(fit2MC, type=c("ICC")); plot(fit2TF, type=c("ICC"))

# Compare with the Rasch Model above...
par(mfrow=c(2,1)); plot(fitMC, type=c("ICC")); plot(fit2MC, type=c("ICC"))
par(mfrow=c(2,1)); plot(fitTF, type=c("ICC")); plot(fit2TF, type=c("ICC"))

# Plot Item Characteristic Curves
plot(fit2MC, type=c("ICC"), items=c(1,6)) # bad items (prob better to exclude)
plot(fit2MC, type=c("ICC"), items=c(2,3,4,5)) # good items

plot(fit2TF, type=c("ICC"), items=c(1,6)) # bad items (prob better to exclude)
plot(fit2TF, type=c("ICC"), items=c(2,3,4,5)) # good items

# Plot Item Information curves (IIC)
# # --------------------------------------------------------------------------------------
# For each item it gives the relative amount of info that the item yields, 
# the highest are the most informative, if the slope is shallow, it does not help at all, 
# because they do not detect any differences.
plot(fit2MC, type=c("IIC"), items=c(1,6))
plot(fit2TF, type=c("IIC"), items=c(1,6))

plot(fit2MC, type=c("IIC"), items=c(2,3,4,5))
plot(fit2TF, type=c("IIC"), items=c(2,3,4,5))

# Overall, we want a curve that stays high among a substantive range of THETA (x-axis)
plot(fit2MC, type=c("IIC"), items=0) 
plot(fit2TF, type=c("IIC"), items=0) 

dev.off()

# # --------------------------------------------------------------------------------------
# Estimate a three-parameter model (TPM or 3-PL)
# # --------------------------------------------------------------------------------------

# The 3-parameter model is usually employed to handle the phenomenon of 
# non-random guessing in the case of difficult items.
fit3MC <- ltm::tpm(polMC, type=c("latent.trait"), max.guessing=.5)
fit3MC

plot(fit3MC, type=c("ICC"))
plot(fit3MC, type=c("IIC"))

fit3TF <- ltm::tpm(polTF, type=c("latent.trait"), max.guessing=.5)
fit3TF

plot(fit3TF, type=c("ICC"))
plot(fit3TF, type=c("IIC"))

# Warning: The 3-parameter model is known to have numerical problems like non-convergence,
# especially for the guessing parameters. These problems usually result in a zero estimate 
# for some guessing parameters and/or in a non positive definite Hessian matrix.
### For mathematical details: https://www.jstatsoft.org/article/view/v017i05

# # --------------------------------------------------------------------------------------
# DIFFERENTIAL ITEM FUNCTIONING (DIF) METHODS
# # --------------------------------------------------------------------------------------

# uniform DIF (different item difficulty parameters)
# non-uniform DIF (different item discrimination parameters)

# Compare knowledge distributions for different respondents
# # -----------------------------------------------------------------------
genderMC<-subset(gmo, select=c("pk_medmarij", "pk_nafta", "pk_parisagree", "pk_guantanamo", 
                               "pk_deficit2018", "pk_gdpgrow", "female", "rep"))
genderMC<-na.omit(genderMC)

genderTF<-subset(gmo, select=c("pkT_medmarij_01", "pkF_nafta_01", "pkT_parisagree_01", 
                               "pkF_guantanamo_01", "pkF_deficit2018_01", "pkT_gdpgrow_01", 
                               "female", "rep"))
genderTF<-na.omit(genderTF)
             
know.scaleMC <- apply(genderMC, 1, mean) 
know.scaleTF <- apply(genderTF, 1, mean) 

par(mfrow=c(2,2)) 
# Focal group: 1 (green); Reference group: 0 (red)
sm::sm.density.compare(know.scaleMC, genderMC$female, xlab="Gender: Women")
sm::sm.density.compare(know.scaleMC, genderMC$rep, xlab="PID: Republican")

sm::sm.density.compare(know.scaleTF, genderTF$female, xlab="Gender: Women")
sm::sm.density.compare(know.scaleTF, genderTF$rep, xlab="PID: Republican")

dev.off()

# # --------------------------------------------------------------------------------------
# Mantel-Haenszel DIF diagnostics (difR package) 
# # --------------------------------------------------------------------------------------

# Reference and Focal Groups
# # --------------------------------------------------------------------------------------
femaleMC<-genderMC$female # women = 1 men = 0
PIDMC<-genderMC$rep # rep = 1

femaleTF<-genderTF$female # women = 1 men = 0
PIDTF<-genderTF$rep # rep = 1

# M.H. DIF
# # --------------------------------------------------------------------------------------
# The function used, difMH, requires that the dataset appear first, 
# followed by the name of the grouping variable, in this case gender and rep.

# One of the genders must be identified as the focal group. 
# We selected males (coded as 1 in the data).
names(genderMC)

# We requested that the matching scale scores be purified. 
# When nonpurified items are included in the matching score, the accuracy of DIF detection 
# can be greatly diminished and false positives are more likely. 

gender.MH <- difR::difMH(genderMC, group=femaleMC, focal.name=0, purify=TRUE, 
                         p.adjust.method = "BH")  
print(gender.MH) # chi-square values (alphaMH), large values indicate DIF.
# we see the log of the odds ratio for each item (alphaMH) 
# as well as the ETS Δ mentioned previously (deltaMH).

# Finally, it is easy to obtain a graphical display for the MH chi-square test results, 
# which provides an easy-to-interpret visual display. 
difR::plot.MH(gender.MH)

# FOR PID
# --------------------------------------------------------------------------------------
PID.MH <- difR::difMH(genderMC, group=PIDMC, focal.name=0, purify=TRUE, 
                      p.adjust.method = "BH")  
print(PID.MH) # chi-square values (alphaMH), large values indicate DIF.
difR::plot.MH(PID.MH) # 

#  According to Kim and Oshima (2013), Holm and Benjamini-Hochberg adjustments 
# (set respectively by "Holm" and "BH") perform best for DIF purposes.

# # --------------------------------------------------------------------------------------
# Logistic regression (LR) DIF diagnotics (difR) 
# # --------------------------------------------------------------------------------------
# Note: here is also the 'lordif' package, that exclusively uses the logistic function. 

# The logistic regression method (Swaminathan & Rogers, 1990) allows for detecting both uniform 
# and non-uniform DIF without requiring an item response model approach. 

# It has an advantage over MH when the researcher is interested in checking the data for both 
# uniform and nonuniform DIF, as it can easily be adapted for both. 

# It consists in fitting a logistic model with the matching criterion, 
# the group membership and an interaction between both as covariates.

genderMClr<-genderMC[,1:6]

dlgMC <- difR::difLogistic(genderMClr, group=femaleMC, focal.name=1, 
                           p.adjust.method = "BH", type="both") # "nudif" or "udif"
difR::print.Logistic(dlgMC)

dlPIDMC <- difR::difLogistic(genderMClr, group=PIDMC, focal.name=1, 
                             p.adjust.method = "BH", type="both") # "nudif" or "udif"
difR::print.Logistic(dlPIDMC)

# The likelihood ratio statistics are displayed on the Y axis, for each item.
# The detection threshold is displayed by a horizontal line, and items flagged as DIF are 
# printed with the color defined by argument col.

plot(dlgMC, plot="lrStat", itemFit = "best", 
     pch = 8, number = F, col = "red", colIC = rep("black", 2), ltyIC = c(1, 2))

plot(dlPIDMC, plot="lrStat", itemFit = "best", 
     pch = 8, number = F, col = "red", colIC = rep("black", 2), ltyIC = c(1, 2))

# # --------------------------------------------------------------------------------------
genderTFlr<-genderTF[,1:6]

dlgTF <- difR::difLogistic(genderTFlr, group=femaleTF, focal.name=1, 
                           p.adjust.method = "BH", type="both")
difR::print.Logistic(dlgTF)

dlPIDTF <- difR::difLogistic(genderTFlr, group=PIDTF, focal.name=1, 
                             p.adjust.method = "BH", type="both") 
dlPIDTF

plot(dlgTF, plot="lrStat", itemFit = "best", 
     pch = 8, number = F, col = "red", colIC = rep("black", 2), ltyIC = c(1, 2))

plot(dlPIDTF, plot="lrStat", itemFit = "best", 
     pch = 8, number = F, col = "red", colIC = rep("black", 2), ltyIC = c(1, 2))

# # --------------------------------------------------------------------------------------
# Lord Chi-square DIF
# # --------------------------------------------------------------------------------------
lordMC.outG<-difR::difLord(genderMC, group="female", focal.name=1, purify=TRUE, 
                           model="1PL", p.adjust.method = "BH")
lordMC.outG
plot(lordMC.outG)

lordTF.outG<-difR::difLord(genderTF, group="female", focal.name=1, purify=TRUE, 
                           model="1PL", p.adjust.method = "BH")
lordTF.outG
plot(lordTF.outG)

lordMC.outR<-difR::difLord(genderMC, group="rep", focal.name=1, purify=TRUE, 
                           model="2PL", p.adjust.method = "BH")
lordMC.outR

lordTF.outR<-difR::difLord(genderTF, group="rep", focal.name=1, purify=TRUE, 
                           model="2PL", p.adjust.method = "BH")
lordTF.outR


# NEXT STEPS TO CONTINUE YOUR IRT JOURNEY
# # --------------------------------------------------------------------------------------
# Rasch Models for Ordered Polytomous Data (PC Model)
# Non-Rasch Models for Ordered Polytomous Data (GPC and GR Models)
# Nominal Polytomous Data (NRM)
# Models for Multidimensional Data (MIRT)