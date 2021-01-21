rm(list = ls())

library("glmmTMB")
library("bbmle")
library("ggplot2")
library("patchwork")
library("emdbook")
library("VGAM")
library("tibble")
library("dplyr")

dd <- read.csv(file = "Lsax_fertilisation_time/data/Fert_Lsax_clean.csv", stringsAsFactors = FALSE)

table(dd$stage)
dd[dd$stage=="SAME",]

dd <- dd[dd$vip=="S", ]
dd$vip <- NULL
dd$misdev <- NULL

dd[dd$termination=="NATURAL", ]
round(range(dd[dd$termination=="NATURAL", "p_dev"]), 1)
nrow(dd[dd$termination=="NATURAL", ])
table(dd$termination)
table(dd$ecotype)

dd$proj <- ifelse(test = dd$notes=="pilot", yes = "pilot", no = "followup")

# write.table(x = dd, file = "/Users/samuelperini/Desktop/Fert_Lsax.csv", append = FALSE, quote = FALSE,
#             sep = ",", row.names = FALSE, col.names = TRUE)

data.frame(table(dd$ecotype, dd$proj))
tb1 <- data.frame(table(dd$termination, dd$ecotype, dd$time_min, dd$proj))
tb1 <- tb1[tb1$Freq!=0, ]

dd <- dd[dd$termination!="NATURAL", ]

dd$tot <- dd$egg+dd$dev
dd$p_egg <- dd$egg/dd$tot
table(dd$tot)

d0 <- nrow(dd[dd$tot==0,])

dd <- dd[dd$tot!=0,]

dd$time_min <- ifelse(test = dd$time_min=="10" | dd$time_min=="30", yes = "10+", no = dd$time_min)
dd$time_min <- factor(x = dd$time_min, levels = c("Control", "1", "5", "10+"))
dd$termination <- factor(x = dd$termination, levels = c("NONE", "ARTIFICIAL"))
# dd$termination <- factor(x = dd$termination, levels = c("NONE", "ARTIFICIAL", "NATURAL"))

tb1 <- data.frame(table(dd$termination, dd$ecotype, dd$time_min, dd$proj))
tb1 <- tb1[tb1$Freq!=0, ]
sum(tb1$Freq)
data.frame(table(dd$time_min))

CTs <- ggplot(data = dd, aes(x = time_min, y = p_dev)) +
  geom_point(aes(size=tot), alpha=0.4) +
  labs(y = "", size = "") +
  theme(legend.position = "none")
CT <- ggplot(data = dd, aes(x = termination, y = p_dev)) +
  geom_point(aes(size=tot), alpha=0.4) +
  labs(y = "n. developed/(n. dev. + n. eggs)", size = "n dev + n eggs") +
  theme(legend.position = "top", legend.text = element_text(size = 8),
        legend.title = element_text(size = 8))
CT + CTs

range(dd$tot)

fit <- Coef(vglm(cbind(dev, tot-dev) ~ 1, betabinomial, data = dd, trace = FALSE))
fit

detach("package:VGAM", unload=TRUE)
table(dd$proj)

ggplot(data = dd, aes(x = proj, y = p_dev)) +
  geom_point(aes(size=tot), alpha=0.4) +
  labs(y = "n. developed/(n. dev. + n. eggs)", size = "n dev + n eggs") +
  theme(legend.position = "top", legend.text = element_text(size = 8),
        legend.title = element_text(size = 8))

aggregate(x = dd$tot, by = list(dd$time_min), mean)
dts <- aggregate(x = dd$tot, by = list(dd$time_min), sum)
tcq <- chisq.test(dts$x)
tcq
tcq$expected

tcq <- chisq.test(dts$x[-1])
tcq
tcq$expected
round(tcq$p.value, 5)

dtm <- aggregate(x = dd$tot, by = list(dd$time_min), mean)
mcq <- chisq.test(dtm$x)
mcq
mcq$expected

mcq <- chisq.test(dtm$x[-1])
mcq
mcq$expected



# fit_betabin <- glm(formula = p_dev ~ proj, weights = tot, family = "betabinomial", data = dd)
# summary(fit_betabin)

fit_betabin <- glm(formula = p_dev ~ proj*termination, weights = tot, family = "betabinomial", data = dd)
fit_betabin <- glm(formula = p_dev ~ termination, weights = tot, family = "betabinomial", data = dd)
summary(fit_betabin)
AIC(fit_betabin)

fit_betabin <- glm(formula = p_dev ~ termination, weights = tot, family = "betabinomial", data = dd)
summary(fit_betabin)

head(dd)

fit_betabin <- glm(formula = p_dev ~ termination, family = "betabinomial", data = dd)
fit_betabin <- glm(formula = p_dev ~ time_min, family = "betabinomial", data = dd)
fit_betabin <- glm(formula = p_dev ~ time_min + size_mm, family = "betabinomial", data = dd)
fit_betabin <- glm(formula = p_dev ~ time_min + tot, family = "betabinomial", data = dd)
fit_betabin <- glm(formula = p_dev ~ time_min + size_mm + tot, family = "betabinomial", data = dd)
fit_betabin <- glm(formula = p_dev ~ time_min + size_mm + tot + proj, family = "betabinomial", data = dd)
fit_betabin <- glm(formula = p_dev ~ termination + size_mm, family = "betabinomial", data = dd)
fit_betabin <- glm(formula = p_dev ~ termination + tot, family = "betabinomial", data = dd)
fit_betabin <- glm(formula = p_dev ~ termination + size_mm + tot, family = "betabinomial", data = dd)
fit_betabin <- glm(formula = p_dev ~ termination + size_mm + tot + proj, family = "betabinomial", data = dd)

summary(fit_betabin)

fit_betabin <- glm(formula = p_dev ~ termination, weights = tot, family = "betabinomial", data = dd)
fit_betabin <- glm(formula = p_dev ~ time_min, weights = tot, family = "betabinomial", data = dd)
fit_betabin <- glm(formula = p_dev ~ time_min + size_mm, weights = tot, family = "betabinomial", data = dd)
fit_betabin <- glm(formula = p_dev ~ time_min + proj, weights = tot, family = "betabinomial", data = dd)
fit_betabin <- glm(formula = p_dev ~ termination * proj, weights = tot, family = "betabinomial", data = dd)
fit_betabin <- glm(formula = p_dev ~ time_min * proj, weights = tot, family = "betabinomial", data = dd)

summary(fit_betabin)
# anova(fit_betabin)

sfit <- round(summary(fit_betabin)$coefficients, 3)
# pr_fit <- profile(fit_betabin)
# confint(pr_fit)

fam <- family(fit_betabin)
fam
str(fam)
ilink <- fam$linkinv
ilink
rm(ndata)
ndata <- bind_cols(dd, setNames(as_tibble(predict(fit_betabin, dd, se.fit = TRUE)[1:2]),
                                c('fit_link','se_link')))
ndata <- mutate(ndata,
                fit_resp  = ilink(fit_link),
                right_upr = ilink(fit_link + (2 * se_link)),
                right_lwr = ilink(fit_link - (2 * se_link)))
ndata <- add_column(ndata, fit = predict(fit_betabin, newdata = ndata, type = 'response'))
head(ndata)
ndata <- unique(ndata[, c("time_min", "fit_resp", "right_upr", "right_lwr")])
ndata <- unique(ndata[, c("termination", "fit_resp", "right_upr", "right_lwr")])
ndata <- unique(ndata[, c("termination", "proj", "fit_resp", "right_upr", "right_lwr")])
cbind(ndata[,1], round(ndata[, -1], 2))
cbind(ndata[,1:2], round(ndata[, -1:-2], 2))

pCTs <- ggplot(data = dd, aes(x = termination, y = p_dev)) +
  geom_point(aes(size=tot), alpha=0.5) +
  labs(y = "Proportion of offspring", size = "", x = "Time (min)") +
  theme(legend.position = "none",
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        axis.line = element_line(size = 0.2, linetype = "solid",
                                 colour = "black"),
        panel.grid = element_line(colour = "gray70", size = 0.2)) +
  geom_errorbar(data = ndata, aes(y = fit_resp, ymin = right_lwr, ymax = right_upr),
                width = 0.1, size = 1) +
  geom_point(data = ndata, aes(x = termination, y = fit_resp), col = "blue", size = 3)
pCTs

pCTs <- ggplot(data = dd, aes(x = time_min, y = p_dev)) +
  geom_point(aes(size=tot), alpha=0.5) +
  labs(y = "Proportion of offspring", size = "", x = "Time (min)") +
  theme(legend.position = "none",
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        axis.line = element_line(size = 0.2, linetype = "solid",
                                 colour = "black"),
        panel.grid = element_line(colour = "gray70", size = 0.2)) +
  geom_errorbar(data = ndata, aes(y = fit_resp, ymin = right_lwr, ymax = right_upr),
                width = 0.1, size = 1) +
  geom_point(data = ndata, aes(x = time_min, y = fit_resp), col = "blue", size = 3)
pCTs
ggsave(filename = "Lsax_fertilisation_time/figures/prop_dev_betabinomial.pdf", plot = pCTs,
       scale = 0.7, dpi = "screen")
# dd[dd$p_dev==0,]  

ddt <- dd[dd$termination=="ARTIFICIAL", ]
fit_betabin <- glm(formula = p_dev ~ time_min, weights = tot, family = "betabinomial", data = ddt)
summary(fit_betabin)

pCTs <- ggplot(data = dd, aes(x = termination, y = p_dev)) +
  geom_point(aes(size=tot), alpha=0.5) +
  labs(y = "Proportion of offspring", size = "", x = "Time (min)") +
  theme(legend.position = "none",
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        axis.line = element_line(size = 0.2, linetype = "solid",
                                 colour = "black"),
        panel.grid = element_line(colour = "gray70", size = 0.2)) +
  geom_errorbar(data = ndata, aes(y = fit_resp, ymin = right_lwr, ymax = right_upr),
                width = 0.1, size = 1) +
  geom_point(data = ndata, aes(x = termination, y = fit_resp), col = "blue", size = 3)
pCTs
