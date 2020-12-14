# install.packages("TMB")
# install.packages("glmmTMB")
library("glmmTMB")
library("bbmle") ## for AICtab
library("ggplot2")
# library("tidyr")

rm(list = ls())
# 
# 
# TEST DIFFERENCE BTW VIPS
dp <- read.csv(file = "Lsax_fertilisation_time/data/mating_time_pilot.csv", stringsAsFactors = FALSE)
head(dp)

dd <- read.csv(file = "Lsax_fertilisation_time/data/FLs_size_mm_sex.csv", stringsAsFactors = FALSE)
head(dd)
dd <- dd[complete.cases(dd), ]

dd <- rbind(dp, dd)
head(dd)
dd$dev <- rowSums(x = dd[, c("misdev", "veliger", "shelled", "crawlaway")])
dd <- dd[, c("snail_ID", "termination", "size_mm", "sex", "ecotype", "time_min", "egg", "misdev", "dev",
             "vip", "notes", "stage")]

table(dd$time_min)
ggplot(data = dd, aes(x = time_min, y = egg, col = vip)) +
  facet_wrap(~snail_ID) +
  geom_point(size = 3, alpha = 0.7)
dd$p_dev <- dd$dev/(dd$dev+dd$egg)
dd$p_dev <- ifelse(test = is.na(dd$p_dev), yes = 0, no = dd$p_dev)

SK <- data.frame(table(dd$snail_ID))
SK <- as.character(SK[SK$Freq>1, 1])
SKp <- ggplot(data = dd[dd$snail_ID %in% SK, ], aes(x = time_min, y = p_dev, col = vip)) +
  facet_wrap(~snail_ID) +
  geom_point(size = 3, alpha = 0.7)
SKp
ggsave(filename = "/Users/samuelperini/Desktop/embryo_count_KS.pdf", plot = SKp, scale = 0.7, dpi = "screen")

dd[dd$snail_ID=="A", ]
dd[dd$snail_ID=="F", ]
dd[dd$snail_ID=="H00", ]
dd[dd$snail_ID=="H12", ]
dd[dd$snail_ID=="H15", ]
dd[dd$snail_ID=="H21", ]
dd[dd$snail_ID=="H51", ]
dd[dd$snail_ID=="J", ]
dd[dd$snail_ID=="O", ]
dd[dd$snail_ID=="Q", ]

write.table(x = dd, file = "Lsax_fertilisation_time/data/Fert_Lsax_clean.csv", append = FALSE, quote = FALSE,
            sep = ",", row.names = FALSE, col.names = TRUE)
# 
# 
# 
# READ DATA
rm(list = ls())
dd <- read.csv(file = "Lsax_fertilisation_time/data/Fert_Lsax_clean.csv", stringsAsFactors = FALSE)
head(dd)

# dl <- gather(dd, stage, count, egg:crawlaway, factor_key=TRUE)
# dl <- gather(dd, stage, count, egg:dev, factor_key=TRUE)
# head(dl)
# table(dl$sex)

dd$p_egg <- dd$egg/(dd$egg+dd$dev)
dd <- dd[dd$vip=="S", ]
dd$misdev <- NULL
dd$tot <- dd$egg+dd$dev
dd <- dd[dd$termination!="NATURAL", ]
dd$proj <- ifelse(test = dd$notes=="pilot", yes = "pilot", no = "followup")

# dl <- dl[dl$stage!="misdev", ]
table(dd$time_min)
dd$time_min <- ifelse(test = dd$time_min=="10" | dd$time_min=="30", yes = "10+", no = dd$time_min)
dd$time_min <- factor(x = dd$time_min, levels = c("Control", "1", "5", "10+"))
dd$termination <- factor(x = dd$termination, levels = c("NONE", "ARTIFICIAL"))

ggplot(data = dd, aes(x = time_min, y = p_dev)) +
  geom_point(aes(size=tot))
ggplot(data = dd, aes(x = termination, y = p_dev)) +
  geom_point(aes(size=tot))
ggplot(data = dd, aes(x = time_min, y = p_egg)) +
  geom_point(aes(size=tot))


# TEST FOR DIFFERENCE IN DEV BETWEEN CONTROL, TREATS and EXPERIMENTS
head(dd)
table(dd$termination)
fit_bin <- glmmTMB(formula = p_dev ~ termination + (1|snail_ID), data = dd, family = "binomial",
                   control=glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS")))

fit_betabin <- glmmTMB(formula = p_dev ~ termination + (1|snail_ID), data = dd, family = "betabinomial",
                       control=glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS")))
AICtab(fit_bin, fit_betabin)
summary(fit_betabin)

fit_bin <- glmmTMB(formula = p_dev ~ time_min + (1|snail_ID), data = dd, family = "binomial",
                   control=glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS")))

fit_betabin <- glmmTMB(formula = p_dev ~ time_min + (1|snail_ID), data = dd, family = "betabinomial",
                       control=glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS")))
AICtab(fit_bin, fit_betabin)

summary(fit_bin)

fixef(fit_betabin)

ff <- fixef(fit_betabin)$cond

# zinbm0 = glmmTMB(count~spp + (1|site), zi=~spp, Salamanders, family=nbinom2)
# fixef(zinbm0)
# ff <- fixef(zinbm0)$zi
# ff
round(plogis(c(Arti=unname(ff[1]),ff[-1]+ff[1])),3)
# Salamanders <- transform(Salamanders, GP=as.numeric(spp=="GP"))
# table(Salamanders$GP)
# zinbm0_A = update(zinbm0, ziformula=~GP)
dd <- transform(dd, Ctrl=as.numeric(time_min=="Control"))
head(dd)
fit_betabinC <- update(fit_betabin, formula=~Ctrl)
fixef(fit_betabinC)
summary(fit_betabinC)
ff <- fixef(fit_betabinC)$cond
round(plogis(c(Treats=unname(ff[1]),ff[-1]+ff[1])),3)

fit_zibetabin <- glmmTMB(formula = p_dev ~ termination + (1|snail_ID), ziformula = ~termination,
                         data = dd,
                         family = "betabinomial",
                         control=glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS")))
ff <- fixef(fit_zibetabin)$zi
round(plogis(c(Control=unname(ff[1]),ff[-1]+ff[1])),3)

zinbm0_B = update(fit_zibetabin, ziformula=~(1|termination))
fixef(zinbm0_B)[["zi"]]
# 
# 
# GLM and ML
id_without <- dd[dd$termination=="ARTIFICIAL" & dd$dev==0, "snail_ID"]
dd_wo <- dd[!dd$snail_ID %in% id_without, ]
fit_bin <- glm(formula = p_dev ~ termination, weights = tot, family = "binomial", data = dd_wo)
summary(fit_bin)
fit_betabin <- glm(formula = p_dev ~ termination, weights = tot, family = "betabinomial", data = dd_wo)
sfit_betabin <- summary(fit_betabin)
rbind(sfit_betabin$coefficients, dispersion=sfit_betabin$dispersion)
fit_quasibin <- glm(formula = p_dev ~ termination, weights = tot, family = "quasibinomial", data = dd_wo)
summary(fit_quasibin)
devia <- c(binomial=deviance(fit_bin), betabin=deviance(fit_betabin), quasibin=deviance(fit_quasibin))
devia