
library(implicitMeasures)
data("raw_data")
head(raw_data)

levels(raw_data$blockcode)
levels(raw_data$response)

iat_cleandata <- clean_iat(raw_data, sbj_id = "Participant",
                           block_id = "blockcode",
                           mapA_practice = "practice.iat.Milkbad",
                           mapA_test = "test.iat.Milkbad",
                           mapB_practice = "practice.iat.Milkgood",
                           mapB_test = "test.iat.Milkgood",
                           latency_id = "latency",
                           accuracy_id = "correct",
                           trial_id = "trialcode",
                           trial_eliminate = c("reminder", "reminder1"),
                           demo_id = "blockcode",
                           trial_demo = "demo")
str(iat_cleandata)

iat1 = compute_iat(iat_cleandata, Dscore = "d1")
summary(iat1)
plot(iat1)
plot(iat1, graph = "points")
plot(iat1, graph = "points", order_sbj = "D-increasing")

t.test(iat1[iat1$cond_ord %in% "MappingA_First", "dscore_d1"],
       iat1[iat1$cond_ord %in% "MappingB_First", "dscore_d1"])$statistic

multi = multi_dscore(iat_cleandata$data_keep)
head(multi)
summary(multi)
plot(multi, graph = "individual")


iat1mapping = iat1[, c("participant", "cond_ord")]
iat1multiverse = merge(iat1mapping, multi$scores)

labels = colnames(iat1multiverse)[grep("dscore", colnames(iat1multiverse))]

results = data.frame(dscore = labels, 
                     t_stat = 0, 
                     p_value = 0)

for (i in 1:length(labels)) {
  temp = t.test(iat1multiverse[iat1multiverse$cond_ord %in% "MappingA_First", labels[i]],
                iat1multiverse[iat1multiverse$cond_ord %in% "MappingB_First", labels[i]])
results[i, "t_stat"] = temp$statistic
results[i, "p_value"] = temp$p.value
}
results$sig = factor(results$p_value < .05)
library(tidyverse)

ggplot(results, 
       aes(x = dscore, y = t_stat, size = p_value, col = sig)) + 
  geom_point(size = 3)

# contro lo 0 ---- 

t.test(iat1multiverse[, labels[i]])


resultsSingle = data.frame(dscore = labels, 
                     t_stat = 0, 
                     p_value = 0, 
                     d = 0, dlow = 0, dhigh= 0)
library(effectsize)
for (i in 1:length(labels)) {
  temp = t.test(iat1multiverse[ labels[i]])
  b = cohens_d(iat1multiverse[, labels[i]])
  resultsSingle[i, "t_stat"] = temp$statistic
  resultsSingle[i, "p_value"] = temp$p.value
  resultsSingle[i, "d"] = b$Cohens_d
  resultsSingle[i, "dlow"] = b$CI_low
  resultsSingle[i, "dhigh"] = b$CI_high
}
resultsSingle$sig = factor(results$p_value < .05)


ggplot(resultsSingle, 
       aes(x = dscore, y = t_stat, size = p_value, col = sig)) + 
  geom_point(size = 3)


ggplot(resultsSingle, 
       aes(x = dscore, y = d,  col = sig)) + 
  geom_point(size = 3) + 
  geom_errorbar(aes(ymin = dlow, ymax= dhigh))

# esercitazione analisi classiche ---- 
dati = read.delim("analisi/data/iat_data.dat")
head(dati)

iatcleandata = clean_iat(dati, 
                         sbj_id = "id", 
                         block_id = "blocknum", 
                         mapA_practice = 3, mapA_test = 4, 
                         mapB_practice = 6, mapB_test = 7)
iat1 = compute_iat(iatcleandata, Dscore = "d1")
iat1$cond_ord

multiiat = multi_dscore(iatcleandata$data_keep)

sbj = dati[, c("id", "order")] %>% 
  distinct()
colnames(sbj)[1] = "participant"
newMultiverse = merge(multiiat$scores, sbj)


summary(multiiat)
plot(multiiat, graph = "individual")

newresults =  data.frame(dscore = labels, 
                         t_stat = 0, 
                         p_value = 0)

for (i in 1:length(labels)) {
  temp = t.test(newMultiverse[newMultiverse$order %in% 1, labels[i]],
                newMultiverse[newMultiverse$order %in% 2, labels[i]])
  newresults[i, "t_stat"] = temp$statistic
  newresults[i, "p_value"] = temp$p.value
}
newresults$sig = factor(newresults$p_value < .05)
ggplot(newresults, 
       aes(x = dscore, y = t_stat, size = p_value, col = sig)) + 
  geom_point(size = 3)


newresultsSingle =  data.frame(dscore = labels, 
                         t_stat = 0, 
                         p_value = 0)

for (i in 1:length(labels)) {
  temp = t.test(newMultiverse[ , labels[i]], 
                alternative = "greater")
  newresultsSingle[i, "t_stat"] = temp$statistic
  newresultsSingle[i, "p_value"] = temp$p.value
}
colMeans(newMultiverse)
newresultsSingle$sig = factor(newresultsSingle$p_value < .05)
ggplot(newresultsSingle, 
       aes(x = dscore, y = t_stat, size = p_value, col = sig)) + 
  geom_point(size = 3)
