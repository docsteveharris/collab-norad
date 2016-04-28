# author: Steve Harris
# date: 2016-03-15
# subject: model mortality (ICU)

# Readme
# ======


# Todo
# ====


# Log
# ===
# 2016-03-15
# - file cloned from model_ne24
# 2016-03-27
# - drop steroids from final model as too much missingness (lose all of RLH and GSTT)

require(Hmisc)
require(data.table)
require(dsbc)
require(arm)
require(ggplot2)
require(reshape2)
require(assertthat)
require(XLConnect)
require(gmodels)

rm(list=ls(all=TRUE))
load("../data/strobe.Rdata")
source(file="../share/functions4paper.R")

describe(wdt$sepsis.site)
with(wdt, table(mort.itu, hosp.id.sort))

# Null model
m <- glm(mort.itu ~ 1 , data=wdt)
m <- glm(mort.itu ~ age.rs , data=wdt)
# coef.plot(m)

# Prepare scaled variables
wdt[, `:=` (
    age.rs = rescale(age),
    weight.rs = rescale(weight),
    sofa.1.rs = rescale(sofa.1),
    lac.1.rs = rescale(lac.1),
    sedation.24.rs = rescale(sedation.24),
    fb.24.rs = rescale(fb.24),
    hr.24.rs = rescale(hr.24),
    ne.24.rs = rescale(ne.24)
            )
]
wdt

#  ===================================
#  = Exploratory model building work =
#  ===================================

# Check MAP
# Univariate
gg.q <- ggplot(data=wdt, aes(x=map.24, y=mort.itu))
gg.q    +
    geom_smooth(method="loess") + 
    geom_rug(data=wdt[mort.itu==1], sides='t', position='jitter', alpha=1/10) +
    geom_rug(data=wdt[mort.itu==0], sides='b', position='jitter', alpha=1/10) +
    coord_cartesian(x=c(40,120), y=c(0,1)) +
    theme_minimal()
m <- glm(mort.itu ~ map.24.rs, data=wdt)
display(m)
m <- glm(mort.itu ~ map.high.rs, data=wdt)
display(m)
gg.q <- ggplot(data=wdt, aes(x=map.24, y=mort.itu))

# Univariate by site
gg.q    +
    geom_smooth(method="lm") + 
    # geom_smooth(method="loess") + 
    facet_grid(. ~ hosp.id.sort) +
    geom_rug(data=wdt[mort.itu==1], sides='t', position='jitter', alpha=1/10) +
    geom_rug(data=wdt[mort.itu==0], sides='b', position='jitter', alpha=1/10) +
    coord_cartesian(x=c(40,120), y=c(0,1)) +
    theme_minimal()

# Varying intercept
m <- glmer(mort.itu ~ map.24.rs + (1 | hosp.id), data=wdt, family=binomial(link="logit"))
display(m)
coef.plot(m)

# Check Heart rate
# Univariate
gg.q <- ggplot(data=wdt, aes(x=hr.24, y=mort.itu))
gg.q    +
    geom_smooth(method="loess") + 
    geom_rug(data=wdt[mort.itu==1], sides='t', position='jitter', alpha=1/10) +
    geom_rug(data=wdt[mort.itu==0], sides='b', position='jitter', alpha=1/10) +
    coord_cartesian(xlim=c(40,120), ylim=c(0,1)) +
    theme_minimal()
m <- glm(mort.itu ~ hr.24.rs, data=wdt)
display(m)
m <- glm(mort.itu ~ hr.high, data=wdt)
display(m)
gg.q <- ggplot(data=wdt, aes(x=hr.24, y=mort.itu))

# Univariate by site
gg.q    +
    geom_smooth(method="lm") + 
    # geom_smooth(method="loess") + 
    facet_grid(. ~ hosp.id.sort) +
    geom_rug(data=wdt[mort.itu==1], sides='t', position='jitter', alpha=1/10) +
    geom_rug(data=wdt[mort.itu==0], sides='b', position='jitter', alpha=1/10) +
    coord_cartesian(x=c(40,120)) +
    theme_minimal()
m <- glmer(mort.itu ~ hr.24.rs + (1 | hosp.id), data=wdt, family=binomial(link="logit"))
display(m)
# coef.plot(m)
m <- glmer(mort.itu ~ hr.high + (1 | hosp.id), data=wdt, family=binomial(link="logit"))
display(m)
# coef.plot(m)

# Check Noradrenaline
# Univariate
gg.q <- ggplot(data=wdt, aes(x=ne.24, y=mort.itu))
gg.q    +
    geom_smooth(method="loess") + 
    geom_rug(data=wdt[mort.itu==1], sides='t', position='jitter', alpha=1/10) +
    geom_rug(data=wdt[mort.itu==0], sides='b', position='jitter', alpha=1/10) +
    coord_cartesian(x=c(0,2)) +
    theme_minimal()
m <- glm(mort.itu ~ ne.24.rs, data=wdt)
display(m)
gg.q <- ggplot(data=wdt, aes(x=ne.24, y=mort.itu))
# Univariate by site
gg.q    +
    geom_smooth(method="lm") + 
    # geom_smooth(method="loess") + 
    facet_grid(. ~ hosp.id.sort) +
    geom_rug(data=wdt[mort.itu==1], sides='t', position='jitter', alpha=1/10) +
    geom_rug(data=wdt[mort.itu==0], sides='b', position='jitter', alpha=1/10) +
    coord_cartesian(x=c(0,2)) +
    theme_minimal()
m <- glmer(mort.itu ~ ne.24.rs + (1 | hosp.id), data=wdt, family=binomial(link="logit"))
display(m)
# coef.plot(m)

    
#  ====================
#  = Multilevel model =
#  ====================
with(wdt, summary(ne.24))

# - define patient level predictors
#   + age
#   + sex
#   + weight
#   + sepsis site
#   + initial SOFA score (at hour 1) 
vars.pt <- c("age.rs", "male", "weight.rs", "sepsis.site", "sofa.1.rs", "lac.1.rs")

# - define treatment variables for adjustment
#   + mechanical ventilation
#   + renal replacement therapy
#   + other vasopressors (yes/no)
#   + other inotropes (yes/no) ? or just combine with above and calculate by subtracting from VADI
#   + sedatation dose
# vars.rx <- c("mv.24", "rrt.24", "vadi.24.rs", "sedation.24.rs", "rx.roids")
vars.rx <- c("mv.24",
             "rrt.24",
             "sedation.24.rs",
             "vadi.other.24.logical",
             # "rx.roids", - [ ] NOTE(2016-03-27): too much missingess
             "fb.24.rs"
             )

# - norad and interactions of interest
vars.ne <- c(
            # "hr.24.rs",
            "hr.high",
            # "map.24.rs",
            "map.high",
            "ne.24.rs",
            "hr.high:ne.24.rs",
            "ne.24.rs:hr.high",
            "ne.24.rs:map.high"
                   )

m.labels <- c(
    "Age",
    "Male",
    "Weight",
    "Abdominal sepsis",
    "Genito-urinary sepsis",
    "Respiratory sepsis",
    "SOFA (baseline)",
    "Lactate (baseline)",
    "Mechanical ventilation",
    "Renal replacement therapy",
    "Sedation score",
    "Additional vasopressors",
    # "Steroid treatment", - [ ] NOTE(2016-03-27): too much missingness
    "Fluid balance",
    "Heart rate",
    "Mean arterial pressure",
    "Noradrenaline dose",
    "Noradrenaline:HR > 95 interaction",
    "Noradrenaline:MAP > 75 interaction"
     )

# Null model
f <- formula(paste("mort.itu ~ (1 | hosp.id)"))
m <- glmer(f, data=wdt, family=binomial(link="logit"))
display(m)

# Define formula
# Varying intercept (for ne.24)
f <- formula(paste("mort.itu ~ ", paste(c(vars.pt, vars.rx, vars.ne), collapse="+"), "+ (1 | hosp.id)"))
print(f)

# - [ ] NOTE(2016-04-28): losing GSTT b/c no baseline SOFA
m <- glmer(f, data=wdt, family=binomial(link="logit"))
coef.plot(m)
display(m)

# Define formula
# Varying intercept and varying slope
f <- formula(paste("mort.itu ~ ", paste(c(vars.pt, vars.rx, vars.ne), collapse="+"), "+ (1 + ne.24.rs | hosp.id)"))
m <- glmer(f, data=wdt, family=binomial(link="logit"))
coef.plot(m)
display(m)

# Need to examine for missingness
vars.pt
require(dplyr)
f.missing <- wdt %>% 
    group_by(hosp) %>%
    select( age,
            male,
            weight,
            sepsis.site,
            # - [ ] NOTE(2016-04-22): ditto for sofa.1 at GSTT
            #   will need to exclude GSTT from model
            sofa.1,
            lac.1,
            mv.24,
            rrt.24,
            sedation.24,
            vadi.other.24.logical,
            # - [ ] NOTE(2016-04-22): inspection shows that misssing all rx.roids from RLH
            #   drop this from the model (not worth including)
            # rx.roids,
            fb.24,
            hr.24,
            map.24,
            ne.24
           ) %>%
    summarise_each(funs(sum(is.na(.))))
f.missing
# View(f.missing)
f.missing %>% group_by(hosp) %>% mutate(tot.missing = sum(.)) %>% select(hosp,tot.missing)




f
m <- glmer(f , data=wdt, family=binomial(link="logit"))
display(m)
coef.plot(m)


# Full model suggests interaction between norad and heart rate
# Check in a simplified model
vars.ne <- c(
            # "hr.24.rs",
            "hr.high",
            # "map.24.rs",
            "map.high",
            "ne.24.rs",
            "hr.high:ne.24.rs",
            "ne.24.rs:hr.high",
            "ne.24.rs:map.high"
                   )

f <- formula(paste("mort.itu ~ ", paste(c(vars.ne), collapse="+"), "+ (1 + ne.24.rs | hosp.id)"))
print(f)
m <- glmer(f, data=wdt, family=binomial(link="logit"))
coef.plot(m)
display(m)

# Check in a simplified model (now remove map)
vars.ne <- c(
            # "hr.24.rs",
            "hr.high",
            # "map.24.rs",
            # "map.high",
            "ne.24.rs",
            "hr.high:ne.24.rs",
            "ne.24.rs:hr.high"
            # "ne.24.rs:map.high"
                   )

# So comparison of simplified model with/without interaction suggests a very marginal improvement
f <- formula(paste("mort.itu ~ ", paste(c(vars.ne), collapse="+"), "+ (1 + ne.24.rs | hosp.id)"))
print(f)
m0 <- glmer(mort.itu ~ hr.high +ne.24.rs 
           + (1 + ne.24.rs | hosp.id), data=wdt, family=binomial(link="logit"))
print(m0)
m1 <- glmer(mort.itu ~ hr.high +ne.24.rs + hr.high:ne.24.rs
           + (1 + ne.24.rs | hosp.id), data=wdt, family=binomial(link="logit"))
print(m1)
anova(m0,m1)

# Let's formally report this in the full model
rm(m0,m1)
# - [ ] NOTE(2016-04-28): no evidence for ne.24 and map.high interaction (so drop)
# - [ ] NOTE(2016-04-28): compare m0 and m1 to test if there is evidence for a ne.24 and hr interaction
vars.ne <- c(
            "hr.24.rs",
            # "hr.high",
            # "map.24.rs",
            # "map.high",
            "ne.24.rs",
            # "hr.high:ne.24.rs",
            "ne.24.rs:hr.high"
            # ,
            # "ne.24.rs:map.high"
                   )
# problems with convergence - possible solutions 
# - rescale vars
f <- formula(paste("mort.itu ~ ", paste(c(vars.pt, vars.rx, vars.ne), collapse="+"), "+ (1 + ne.24.rs | hosp.id)"))
f
m0 <- glmer(f, data=wdt, family=binomial(link="logit"))
# https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
# check for singlularity
tt <- getME(m0,"theta")
ll <- getME(m0,"lower")
min(tt[ll==0])
# OK

# Try improving the fit by further iterations starting from prev model
ss <- getME(m0,c("theta","fixef"))
m0 <- update(m0,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
m0
# Works!?

display(m0)
data.frame(summary(m0)$coefficients)
# - [ ] NOTE(2016-04-28): NOT plotting correctly (plots that last plot in memory?)
# coef.plot(model=m0)

vars.ne <- c(
            "hr.24.rs",
            # "hr.high",
            # "map.24.rs",
            # "map.high",
            "ne.24.rs"
            # ,
            # "hr.high:ne.24.rs",
            # "ne.24.rs:hr.high"
            # "ne.24.rs:map.high"
                   )
f <- formula(paste("mort.itu ~ ", paste(c(vars.pt, vars.rx, vars.ne), collapse="+"), "+ (1 + ne.24.rs | hosp.id)"))
f
m1 <- glmer(f, data=wdt, family=binomial(link="logit"))
# print(m0)
# print(m1)
# marginal but 'significant' improvement in fit
anova(m0,m1)

# - [ ] NOTE(2016-04-22): shows that mort ~ ne.24 depends on HR
# check by plotting
# but plot is _not_ convincing, we should not report this

tdt <- wdt[!is.na(hr.high)]
gg.q <- ggplot(data=tdt, aes(x=ne.24, y=mort.itu))
# Univariate by site
gg.q    +
    geom_smooth(method="lm") + 
    # geom_smooth(method="loess") + 
    facet_grid(hr.high ~ hosp.id.sort) +
    geom_rug(data=tdt[mort.itu==1], sides='t', position='jitter', alpha=1/10) +
    geom_rug(data=tdt[mort.itu==0], sides='b', position='jitter', alpha=1/10) +
    coord_cartesian(x=c(0,2)) +
    theme_minimal()

#  ===================================================
#  = Exploring simulations of the predicted response =
#  ===================================================
# - [ ] TODO(2016-04-28): bootstrap respecting hierarchical structure @next
# - [ ] TODO(2016-04-28): bootstrap more simulations @next
# - [ ] TODO(2016-04-28): bootstrap both models for comparison @later

m <- m1 # use model without interaction
coef(m)
fixef(m)
ranef(m)
# Obtain predictions
y.tilde <- predict(m, type="response")
describe(y.tilde)

# Now bootstrap for CI
require(boot)
FUN <- function(m) {
    y.tilde <- predict(m, type="response")
    return(y.tilde)
}

# Running with just 3 simulations while developing
y.tilde.boot <- bootMer(m, FUN, nsim=3)
y.tilde.boot
str(y.tilde.boot)
summary(y.tilde.boot)
summary(y.tilde.boot$t)
head(y.tilde.boot$t)


# Summarise mean of bootstrapped predictions
y.tilde <- t(as.data.frame(y.tilde.boot$t) %>% summarise_each(funs(mean)))
y.tilde

scale <- function (r=raw, s=scaled) {
    # Reverses the scaling applied to by arm `rescale`
    # only works for numerical values
    require(arm)
    r <- r[!is.na(r)]
    return((s * 2 * sd(r)) + mean(r)) 
}

d <- data.table(cbind(y.tilde.boot$data, y.tilde))
# d[, .(hr.24.rs, scale(r=wdt$hr.24, s=hr.24.rs))]
head(d)
d[,hr.high:=ifelse(scale(wdt$hr.24, hr.24.rs) >= 95, "High", "Low" )]

# Plot predicted mortality and interaction
# - [ ] TODO(2016-04-28): lines fitted to bootstrap predictions, would
#   be better to recover coefficients directly from bootstrapping
ggplot(data=d,
    aes(y=y.tilde,
        x=scale(wdt$ne.24, ne.24.rs),
        group=hr.high, colour=hr.high
        )) +
    geom_smooth(method="lm", aes(colour=hr.high)) + 
    geom_point() +
    coord_cartesian(x=c(0,2), y=c(0,1)) + guides(colour=FALSE) +
    xlab("Noradrenaline (mcg/kg/min)") +
    ylab("Predicted mortality") +
    theme_minimal()



# Prepare table to save
summary(m)$coefficients
m.table <- data.table(summary(m)$coefficients, keep.rownames=TRUE)
colnames(m.table)[2] <- "est"
colnames(m.table)[3] <- "se"
# - [ ] FIXME(2016-03-26): using normal as approximation of
#   t-distribution b/c don't know how to get a the degrees of freedom
#   for the t
colnames(m.table)[4] <- "t"
m.table$z <- m.table$est / m.table$se
m.table$p <- pnorm(abs(m.table$z), lower.tail=FALSE)
# Calculate confidence intervals 
m.table$ci95.l <- m.table$est - (qnorm(0.975) * m.table$se)
m.table$ci95.u <- m.table$est + (qnorm(0.975) * m.table$se)

m.table <- cbind(label=c("Intercept", m.labels), m.table)
m.table

p <- coef.plot(m) 
p <- coef.plot(m, m.labels=m.labels)
print(p)
ggsave(p, file="../write/figures/model_mortality.pdf", width=3, height=3, scale=2)


(m.var.ranef <- summary(m)$varcor$hosp.id[1])
(m.icc <- m.var.ranef / (m.var.ranef + (pi^2)/3))
str(se.ranef(m))
(model_parameters <- list(data.frame(m.icc)))


# Now write to Excel
wb <- loadWorkbook("../write/tables/model_mortality.xlsx", create = TRUE)

sheet1 <- paste0("model")
# if (existsSheet(wb, sheet1)) {
#   removeSheet(wb, sheet1)
# }
createSheet(wb, name = sheet1)
writeWorksheet(wb, model_parameters, sheet1)

sheet2 <- paste0("raw")
# if (existsSheet(wb, sheet2)) {
#   removeSheet(wb, sheet2)
# }
createSheet(wb, name = sheet2)
writeWorksheet(wb, m.table, sheet2)

saveWorkbook(wb)
