library(ggplot2)
library(rstanarm)
library(brms)
library(mgcv)
library(dplyr)
library(segmented)
library(tidyr)
library(HDInterval)
library(entropy)
library(tidybayes)
library(tibble)
library(knitr)
library(entropart)
library(splus2R)
library(tseries)
library(attempt)
library(stringr)

rm(list = ls())
set.seed(30062020)

# corpus size
corpus.cds.word = read.table(file="cds_word_count.txt")
corpus.cs.word = read.table(file="cs_word_count.txt")
corpus.cds = read.table(file = "cds_meta.txt", sep = "\t") %>%
  rename(session = V1, utterance_no = V2) %>%
  cbind(corpus.cds.word) %>%
  rename(word_count = V1) %>%
  group_by(session) %>%
  mutate(word = sum(word_count)) %>%
  add_tally() %>%
  select(c("session","word","n")) %>%
  distinct()
corpus.cs = read.table(file = "child_meta.txt", sep = "\t") %>%
  rename(session = V1, utterance_no = V2) %>%
  cbind(corpus.cs.word) %>%
  rename(word_count = V1) %>%
  group_by(session) %>%
  mutate(word = sum(word_count)) %>%
  add_tally() %>%
  select(c("session","word","n")) %>%
  distinct()
rm(corpus.cds.word)
rm(corpus.cs.word)

corpus.summary = read.table(file = "tur_children.csv", sep = ",") %>%
  select(c("V3","V8","V10","V12")) %>%
  rename(session = V3, name = V8, age = V10, sex = V12)
corpus.summary$cds_utterance = corpus.cds$n
corpus.summary$cs_utterance = corpus.cs$n
corpus.summary$cds_word = corpus.cds$word
corpus.summary$cs_word = corpus.cs$word
corpus.summary %>%
  group_by(name) %>%
  mutate(cds_utterance = sum(cds_utterance),
         cs_utterance = sum(cs_utterance),
         cds_word = sum(cds_word),
         cs_word = sum(cs_word)) %>%
  select(c("name", "sex","cds_utterance", "cds_word", "cs_utterance", "cs_word","age")) %>%
  slice(1,n())


# to replace all NaN with 0
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
is.inf.data.frame <- function(x)
  do.call(cbind, lapply(x, is.inf))
# raw data
results.cds = filter(
  data.frame(read.csv("results_cds_anony.csv",
                      sep="\t",header = TRUE)),
  child!="c4")
results.cds = results.cds[with(results.cds, order(child, age)),]
results.cds[is.inf.data.frame(results.cds)] = 0

results.child = filter(
  data.frame(read.csv("results_child_anony.csv",
                      sep="\t", header = TRUE)),
  child!="c4")
results.child = results.child[with(results.child, order(child, age)),]
results.child[is.inf.data.frame(results.child)] = 0

# Analysis 1: ellipsis level

study1.data.lex = data.frame(child = results.cds$child,
                           age = results.cds$age,
                           child_prop = results.child$lex_obj,
                           cds_prop = results.cds$lex_obj) %>%
                           
  mutate(prop_ratio = child_prop/cds_prop)
study1.data.lex[is.nan(study1.data.lex)] = 0
study1.data.lex = transform(study1.data.lex,
                          prop_ratio = ifelse(prop_ratio>1,
                                                 1,
                                                 prop_ratio))

levels(study1.data.lex$child) = c("Child 1","Child 2",
                                  "Child 3","Child 4",
                                  "Child 4","Child 5",
                                  "Child 6","Child 7")
ggplot(study1.data.lex, aes(age, 1-cds_prop))+
  geom_line()+
  facet_wrap(vars(child))+
  ylab("proportion of object drop")+
  theme_bw()+
  theme(panel.spacing.x = unit(4, "mm"))+
  theme(text=element_text(size=15))

ggsave("results/Figure1.pdf",width = 8, height = 6)

study1.data.morph = data.frame(child = results.cds$child,
                           age = results.cds$age,
                           child_prop = results.child$morph_obj,
                           cds_prop = results.cds$morph_obj)%>%
  mutate(prop_ratio = child_prop/cds_prop)
study1.data.morph[is.nan(study1.data.morph)] = 0
study1.data.morph = transform(study1.data.morph,
                          prop_ratio = ifelse(prop_ratio>1,
                                              1,
                                              prop_ratio))
levels(study1.data.morph$child) = c("Child 1","Child 2",
                                  "Child 3","Child 4",
                                  "Child 4","Child 5",
                                  "Child 6","Child 7")
ggplot(study1.data.morph, aes(age, 1-cds_prop))+
  geom_line()+
  facet_wrap(vars(child))+
  ylab("proportion of object drop")+
  theme_bw()+
  theme(panel.spacing.x = unit(4, "mm"))+
  theme(text=element_text(size=15))

ggsave("results/Figure2.pdf",width = 8, height = 6)

study1.data.diff = rbind(study1.data.lex, study1.data.morph) 
study1.data.diff$construction = rep(c("lexical","morphological"),
                                  each=343)
study1.model.diff = brm(bf(1-cds_prop~construction+(1+construction|child),
                         zoi ~ construction,
                         coi ~ construction,
                         phi ~ construction),
                      data = study1.data.diff,
                      family = zero_one_inflated_beta(),
                      prior = c(prior(student_t(5, 0, 3), class = "b"),
                                prior(student_t(5, 0.5, 3), class = "Intercept")),
                      seed = 1,
                      cores = 4,
                      iter = 2000,
                      control = list(adapt_delta=0.999999,
                                     max_treedepth = 15)
)

print(summary(study1.model.diff), digits=5)

plot(conditional_effects(study1.model.diff))$construction +
  ggplot2::ylab("proportion of object ellipsis")+
  ggplot2::theme_bw()+
  ggplot2::theme(text=element_text(size=16))

ggsave("results/Figure3.pdf",width = 8, height = 6)

# to print the (quantile-based) credible intervals of the posterior predictive samples
print(conditional_effects(exp2.model.diff)$construction)


# Analysis 2: lex and morph in child speech approach adult\'s level

# lexical

study2.data.lex = data.frame(child = results.cds$child,
                           age = results.cds$age,
                           child_entropy = results.child$lex_entropy,
                           child_var = results.child$lex_var,
                           cds_entropy = results.cds$lex_entropy,
                           cds_var = results.cds$lex_var) %>%
  mutate(entropy_ratio = child_entropy/cds_entropy)

study2.data.lex[is.nan(study2.data.lex)] = 0

# morphological

study2.data.morph = data.frame(child = results.cds$child,
                           age = results.cds$age,
                           child_entropy = results.child$morph_entropy,
                           child_var = results.child$morph_var,
                           cds_entropy = results.cds$morph_entropy,
                           cds_var = results.cds$morph_var) %>%
  mutate(entropy_ratio = child_entropy/cds_entropy)

study2.data.morph[is.nan(study2.data.morph)] = 0

# noncaus

study2.data.noncaus = data.frame(child = results.cds$child,
                           age = results.cds$age,
                           child_entropy = results.child$noncaus_entropy,
                           child_var = results.child$noncaus_var,
                           cds_entropy = results.cds$noncaus_entropy,
                           cds_var = results.cds$noncaus_var) %>%
  mutate(entropy_ratio = child_entropy/cds_entropy)
study2.data.noncaus[is.nan(study2.data.noncaus)] = 0

# all verbs

study2.data.verb = data.frame(child = results.cds$child,
                           age = results.cds$age,
                           child_entropy = results.child$verb_entropy,
                           child_var = results.child$verb_var,
                           cds_entropy = results.cds$verb_entropy,
                           cds_var = results.cds$verb_var) %>%
  mutate(entropy_ratio = child_entropy/cds_entropy)
study2.data.verb[is.nan(study2.data.verb)] = 0

# segmented analysis, child by child

segmented_analysis <- function(dataset){
  models = list()
  par(mfrow=c(3,3))
  for (chi in levels(dataset$child)){
    if (chi != "Ekin"){
      child.data = filter(dataset,child == chi)
      # find the onset
      onset = 0
      for (session in 1:nrow(child.data)){
        if (child.data$entropy_ratio[session] !=0){
          onset = child.data$age[session]
          break
        }
      }
      print(onset)
      
      
      chi.reg = lm(entropy_ratio ~ age, 
                   data=child.data)
      chi.segmented = try_catch(segmented(chi.reg, npsi=2))
      
      print(chi.segmented$psi)
      if (!is.null(chi.segmented$psi)){
        plot(chi.segmented, main = chi, res=TRUE)
        models[[chi]] = chi.segmented
      }
      else{
        chi.segmented = (segmented(chi.reg, npsi=1))
        print(chi.segmented$psi)
        if (!is.null(chi.segmented$psi)){
          models[[chi]] = chi.segmented
          plot(chi.segmented, main = chi, res=TRUE)
        }
        else{
          plot(y = filter(dataset,child == chi)$entropy_ratio, 
               x = filter(dataset,child == chi)$age,
               ylab = "Effect of age",
               xlab = "age",
               main = chi)
          models[[chi]] = NA
        }
      }
    }
  }
  title(deparse(substitute(dataset)), line = -1,outer = TRUE)
  return(models)
}


set.seed(1)
lex.models = segmented_analysis(study2.data.lex)
morph.models = segmented_analysis(study2.data.morph)
noncaus.models = segmented_analysis(study2.data.noncaus)
verb.models = segmented_analysis(study2.data.verb)

plot_child <- function(chi,groups){
  meta = filter(study2.data.verb, child==chi)
  results = rbind(data.frame(age=meta$age,
                             ratio=lex.models[[chi]]$fitted.values),
                  data.frame(age=meta$age,
                             ratio=morph.models[[chi]]$fitted.values),
                  data.frame(age=meta$age,
                             ratio=dir.models[[chi]]$fitted.values),
                  data.frame(age=meta$age,
                             ratio=noncaus.models[[chi]]$fitted.values),
                  data.frame(age=meta$age,
                             ratio=verb.models[[chi]]$fitted.values))
  results$category = rep(c("Lexical","Morphological",
                                  "DIr","Non-caus","All verbs"), 
                               each=length(meta$age))
  results$child = factor(rep(chi,each=5*length(meta$age)))
  ggplot(subset(results, category %in% groups),
         aes(age, ratio))+
    geom_line(aes(linetype=category))
  return(results)
}

segmented.results = data.frame()
for (chi in levels(study2.data.verb$child)){
  segmented.results = rbind(segmented.results,
                            plot_child(chi,c("Lexical","All verbs","Non-caus","Morphological")))
}
levels(segmented.results$child) = c("Child 1","Child 2",
                                    "Child 3",
                                    "Child 4","Child 5",
                                    "Child 6","Child 7")
segmented.results = rename(segmented.results, group=category)
ggplot(subset(segmented.results, group %in% 
                c("All verbs", "Lexical",
                  "Non-caus")),
       aes(age, ratio))+
  geom_line(aes(linetype=group)) + 
  geom_hline(yintercept = 1.0, color="grey",
             size=1)+
  facet_wrap(vars(child)) +
  theme_bw()+
  theme(panel.spacing.x = unit(4, "mm"))+
  theme(text=element_text(size=15))

ggsave("results/Figure4.pdf",
       width = 9,
       height = 7)

ggplot(subset(segmented.results, group %in% 
                c("All verbs",
                  "Morphological")),
       aes(age, ratio))+
  geom_line(aes(linetype=group)) + 
  geom_hline(yintercept = 1.0, color="grey",
             size=1)+
  facet_wrap(vars(child)) +
  theme_bw()+
  theme(panel.spacing.x = unit(4, "mm"))+
  theme(text=element_text(size=15))
ggsave("results/Figure5.pdf",
       width = 9,
       height = 7)
