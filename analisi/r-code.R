library(lme4) # Fitting LMMs
library(ggplot2) # Plots
# import
data = read.csv("data/example-data.csv",
                header = TRUE, sep =",")
head(data)
# fit glmms----
accuracy1 = glmer(accuracy ~ 0 + condition + (1|stimuli) + (1|respondent),
                  data = data,
                  family = "binomial")
accuracy2 = glmer(accuracy ~ 0 + condition + (0 + condition|stimuli) +
                    (1|respondent),
                  data = data,
                  family = "binomial")
accuracy3 = glmer(accuracy ~ 0 + condition + (1|stimuli) +
                    (0 + condition|respondent),
                  data = data,
                  family = "binomial")
# fit lmms ----
logtime1 = lmer(log(latency) ~ 0 + condition + (1|stimuli) + (1|respondent),
                data = data,
                REML = FALSE)
logtime2 = lmer(log(latency) ~ 0 + condition + (0 + condition|stimuli) +
                  (1|respondent),
                data = data,
                REML = FALSE)
logtime3 = lmer(log(latency) ~ 0 + condition + (1|stimuli) +
                  (0 + condition|respondent),
                data = data,
                REML = FALSE)
# model comparison ----
anova(accuracy1, accuracy2, accuracy3)
anova(logtime1, logtime2, logtime3)
# chatgpt (cause I've lost my orginal code, sorry) ---
parole <- c("annoying", "bf14", "bf23", "bf56", "bm14", "bm23", "bm56", "evil",
            "failure", "glory", "good", "happiness", "hate", "horrible", "joy",
            "laughter", "love", "pain", "peace", "pleasure", "terrible", "wf2",
            "wf3", "wf6", "wicked", "wm1", "wm4", "wm6")

negative <- c("annoying", "evil", "failure", "hate", "horrible", "pain", "terrible", "wicked")
positive <- c("glory", "good", "happiness", "joy", "laughter", "love", "peace", "pleasure")

# Funzione di categorizzazione
categorizza <- function(x) {
  if (x %in% negative) {
    return("negative")
  } else if (x %in% positive) {
    return("positive")
  } else if (grepl("^wf|^wm", x)) {
    return("wp")
  } else if (grepl("^bf|^bm", x)) {
    return("bp")
  } else {
    return("altro")  # per sicurezza
  }
}

# ability estimates -----
ab_iat =  as.data.frame(ranef(accuracy2,   # BLUP of the stimuli
                              condVar = TRUE)) 
ab_iatSingles = ab_iat[ab_iat$grpvar %in% "respondent", c("grpvar", "condval", "condsd")]

ab_iat = data.frame(sbj = rownames(coef(accuracy2)$respondent),
                    theta = coef(accuracy2)$respondent[,1]) 
# density ability ----
ggplot(ab_iat, 
                         aes(x = theta)) + 
  geom_density()  + # object containing the starting plot
  geom_density(fill = "springgreen4", # change the color of the filling of the density distribution
               alpha = .50) + 
  theme_light() + # define the theme of the plot
  theme(legend.position =  "none", # remove the legend
        axis.title.y = element_blank(), # remove the title of the y-axis
        axis.title.x = element_text(size = 30, face = "bold"), # increase the size of x-axis title and set it to bold
        axis.text = element_text(size = 26, face = "bold")) + # increase the size of x-axis text and set it to bold
  xlab(expression(theta))
# indvidual differences ability ----
ab_iatSingles$grpvar = paste(ab_iatSingles$grpvar, 1:nrow(ab_iatSingles), sep = "_")
ggplot(ab_iatSingles, 
       aes(y = reorder(grpvar, condval), 
           x = condval, color = grpvar)) + geom_point() + 
  ylab("Respondents")  +theme_light() + 
  theme(legend.position =  "none",
        axis.text.y = element_blank(),
        axis.title = element_text(size = 24), 
        axis.text.x = element_text(size = 22)) +  geom_errorbarh(aes(xmin = condval - 1.96*condsd, xmax= condval + 1.96*condsd)) +
  xlab(expression(theta)) + scale_fill_manual(values = rainbow(65))

# easiness ----
easiness_iat$stimuli = as.character(easiness_iat$stimuli)
easiness_iat$b = with(easiness_iat, fix.est + blup)
easiness_iat$categoria <- sapply(easiness_iat$stimuli, categorizza)
cat_levels <- c("negative", "positive", "bp", "wp")
easiness_iat$categoria <- factor(easiness_iat$categoria, levels = cat_levels)
easiness_iat <- easiness_iat[order(easiness_iat$categoria, easiness_iat$stimuli), ]
easiness_iat$stimuli <- factor(easiness_iat$stimuli, levels = unique(easiness_iat$stimuli))
# graphs stimuli ----
graph_b_ci = ggplot(easiness_iat, # dataset with the easiness estimates
                    aes(x = b, # easiness estimates on the x-axis
                        y = stimuli, # labels of the stimuli on the y axis
                        color = Condition)) + # change the color according to the condition
  geom_point() + # draw points to represent the easiness of each stimulus
  geom_errorbarh(aes(xmin = b - 1.96*se, # draw the error bars to represent the confidence intervals  (CI)
                     xmax= b + 1.96*se)) 

ggplot(easiness_iat, 
       aes(x = b,fill = Condition)) + geom_density(alpha =.50) + 
  xlim(-1, 4) + 
  scale_fill_manual(values = c("royalblue4", "gold"), 
                    labels = c("WBBG", "WGBB")) + 
  theme_light() + 
  theme(legend.position = "inside",
        legend.position.inside = c(.2, .6),
        legend.title = element_blank(),
        axis.title.x = element_text(size = 24), 
        axis.title.y = element_blank(), 
        axis.text = element_text(size = 24), 
        legend.text = element_text(size = 24))
graph_b_ci +  # object containing the starting plot
  geom_point(size = 2) + # increase the size of the points
  geom_errorbarh(aes(xmin = b - 1.96*se, xmax= b + 1.96*se), 
                 linewidth=.95) + 
  theme_light() +  
  theme(legend.position = "none", 
        axis.title.x = element_text(size = 24), 
        axis.title.y = element_blank(), 
        axis.text = element_text(size = 18, color=c(rep("darkgreen", 8), rep("firebrick4", 8),
                                                    rep("tan3", 6), 
                                                    rep("salmon", 6) ))) + 
  scale_color_manual(values = c("royalblue4", "gold"), 
                     labels = c("WBBG", "WGBB")) 
# speed estimates ----
speed = as.data.frame(ranef(logtime3), condvar= TRUE)
fixef_speed = data.frame(term = names(fixef(logtime3)), # assign the labels of the conditions to the term column
                         est = (fixef(logtime3))) # assign the estimates of the fixed effects to the est column
# merge together the fixed effects and stimuli BLUP with their variances
speed = merge(speed, fixef_speed)
speed$tau = speed$condval + speed$est
# graph density ----
ggplot(speed, 
       (aes(x = tau, fill  = term))) + geom_density(alpha = .5)  + 
  theme_light()+
  scale_fill_manual(values = c("royalblue4", "gold"), 
                    labels = c("WBBG", "WGBB")) + 
  xlab(expression(tau)) +theme(legend.position = "inside",
                               legend.position.inside = c(.8, .8),
                               legend.title = element_blank(),
                               axis.title.x = element_text(size = 24), 
                               axis.title.y = element_blank(), 
                               axis.text = element_text(size = 24), 
                               legend.text = element_text(size = 24))
# graph individual differences -----
ggplot(speed, 
       (aes(x = tau, y = grp, color  = term))) + geom_point()  + 
  theme_light()+
  scale_color_manual(values = c("royalblue4", "gold"), 
                     labels = c("WBBG", "WGBB")) + 
  xlab(expression(tau)) +theme(legend.position = "inside",
                               legend.position.inside = c(.8, .18),
                               legend.title = element_blank(),
                               axis.title.x = element_text(size = 24), 
                               axis.title.y = element_blank(), 
                               axis.text.x = element_text(size = 24), 
                               axis.text.y = element_blank(),
                               legend.text = element_text(size = 24)) + 
  geom_errorbar(aes(xmin = tau - 1.96*condsd, xmax = tau + 1.96*condsd), linewidth=.95)

# time intensity ------
intensity = as.data.frame(ranef(logtime3), condvar = TRUE)
intensity = intensity[intensity$grpvar %in% "stimuli", ]
colnames(intensity)[3] = "stimuli"
intensity$categoria <- sapply(intensity$stimuli, categorizza)
cat_levels <- c("negative", "positive", "bp", "wp")
intensity$categoria <- factor(intensity$categoria, levels = cat_levels)
intensity <- intensity[order(intensity$categoria, intensity$stimuli), ]

intensity$stimuli <- factor(intensity$stimuli, levels = unique(intensity$stimuli))

ggplot(intensity, 
       aes(x = condval)) + geom_density(fill = "orchid", alpha =.5) + xlim(-.35, .35) + theme_light() + 
  theme_light() + # define the theme of the plot
  theme(legend.position =  "none", # remove the legend
        axis.title.y = element_blank(), # remove the title of the y-axis
        axis.title.x = element_text(size = 30, face = "bold"), # increase the size of x-axis title and set it to bold
        axis.text = element_text(size = 26, face = "bold")) + # increase the size of x-axis text and set it to bold
  xlab(expression(delta))
ggplot(intensity, 
       aes(x = condval, y = stimuli, col = categoria)) + geom_point() + # increase the size of the points
  geom_errorbarh(aes(xmin = condval - 1.96*condsd, 
                     xmax= condval + 1.96*condsd)) + 
  theme_light() +  scale_color_manual(values = c("darkgreen", "firebrick4", "tan3", "salmon")) + xlab(expression(delta)) + 
  theme(legend.position = "none", 
        axis.title.x = element_text(size = 24), 
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 18, color=c(rep("darkgreen", 8), rep("firebrick4", 8),
                                                      rep("tan3", 6), 
                                                      rep("salmon", 6) ))) 
