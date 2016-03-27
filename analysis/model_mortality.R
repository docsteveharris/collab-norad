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
with(wdt, CrossTable(mort.itu, hosp.id.sort))

# Null model
m <- glm(mort.itu ~ 1 , data=wdt)
m <- glm(mort.itu ~ rescale(age) , data=wdt)
coef.plot(m)

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
m <- glm(mort.itu ~ map.24, data=wdt)
display(m)
m <- glm(mort.itu ~ map.high, data=wdt)
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
m <- glmer(mort.itu ~ map.24 + (1 | hosp.id), data=wdt, family=binomial(link="logit"))
display(m)
display(m)
coef.plot(m)

# Check Heart rate
# Univariate
gg.q <- ggplot(data=wdt, aes(x=hr.24, y=mort.itu))
gg.q    +
    geom_smooth(method="loess") + 
    geom_rug(data=wdt[mort.itu==1], sides='t', position='jitter', alpha=1/10) +
    geom_rug(data=wdt[mort.itu==0], sides='b', position='jitter', alpha=1/10) +
    coord_cartesian(x=c(40,120)) +
    theme_minimal()
m <- glm(mort.itu ~ hr.24, data=wdt)
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
m <- glmer(mort.itu ~ hr.24 + (1 | hosp.id), data=wdt, family=binomial(link="logit"))
display(m)
coef.plot(m)
m <- glmer(mort.itu ~ hr.high + (1 | hosp.id), data=wdt, family=binomial(link="logit"))
display(m)
coef.plot(m)

# Check Noradrenaline
# Univariate
gg.q <- ggplot(data=wdt, aes(x=ne.24, y=mort.itu))
gg.q    +
    geom_smooth(method="loess") + 
    geom_rug(data=wdt[mort.itu==1], sides='t', position='jitter', alpha=1/10) +
    geom_rug(data=wdt[mort.itu==0], sides='b', position='jitter', alpha=1/10) +
    coord_cartesian(x=c(0,2)) +
    theme_minimal()
m <- glm(mort.itu ~ ne.24, data=wdt)
display(m)
gg.q <- ggplot(data=wdt, aes(x=ne.24, y=mort.itu))
# Univariate by site
gg.q    +
    # geom_smooth(method="lm") + 
    geom_smooth(method="loess") + 
    facet_grid(. ~ hosp.id.sort) +
    geom_rug(data=wdt[mort.itu==1], sides='t', position='jitter', alpha=1/10) +
    geom_rug(data=wdt[mort.itu==0], sides='b', position='jitter', alpha=1/10) +
    coord_cartesian(x=c(0,2)) +
    theme_minimal()
m <- glmer(mort.itu ~ ne.24 + (1 | hosp.id), data=wdt, family=binomial(link="logit"))
display(m)
coef.plot(m)

    
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
vars.pt <- c("rescale(age)", "male", "rescale(weight)", "sepsis.site", "rescale(sofa.1)", "rescale(lac.1)")

# - define treatment variables for adjustment
#   + mechanical ventilation
#   + renal replacement therapy
#   + other vasopressors (yes/no)
#   + other inotropes (yes/no) ? or just combine with above and calculate by subtracting from VADI
#   + sedatation dose
# vars.rx <- c("mv.24", "rrt.24", "rescale(vadi.24)", "rescale(sedation.24)", "rx.roids")
vars.rx <- c("mv.24",
             "rrt.24",
             "rescale(sedation.24)",
             "vadi.other.24.logical",
             # "rx.roids", - [ ] NOTE(2016-03-27): too much missingess
             "rescale(fb.24)"
             )

# - norad and interactions of interest
vars.ne <- c(
            "rescale(hr.24)",
            "rescale(map.24)",
            "rescale(ne.24)",
            "hr.high:rescale(ne.24)",
            "map.high:rescale(ne.24)"
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
# Varying intercept and slope (for ne.24)
f <- formula(paste("mort.itu ~ ", paste(c(vars.pt, vars.rx, vars.ne), collapse="+"), "+ (1 | hosp.id)"))
print(f)

m <- glmer(f, data=wdt, family=binomial(link="logit"))
coef.plot(m)
display(m)

# Define formula
# Varying intercept and varying slope
f <- formula(paste("mort.itu ~ ", paste(c(vars.pt, vars.rx, vars.ne), collapse="+"), "+ (1 + rescale(ne.24) | hosp.id)"))

# Need to examine for missingness
vars.pt
require(dplyr)
f.missing <- wdt %>% 
    group_by(hosp) %>%
    select( age,
            male,
            weight,
            sepsis.site,
            sofa.1,
            lac.1,
            mv.24,
            rrt.24,
            sedation.24,
            vadi.other.24.logical,
            rx.roids,
            fb.24,
            hr.24,
            map.24,
            ne.24
           ) %>%
    summarise_each(funs(sum(is.na(.))))

f.missing %>% group_by(hosp) %>% mutate(tot.missing = sum(.)) %>% select(hosp,tot.missing)

print(f)

m <- glmer(f , data=wdt, family=binomial(link="logit"))
display(m)
coef.plot(m)

#  ===================================================
#  = Exploring simulations of the predicted response =
#  ===================================================
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
y.tilde.boot <- bootMer(m, FUN, nsim=3)
y.tilde.boot
str(y.tilde.boot)
summary(y.tilde.boot)
summary(y.tilde.boot$t)
head(y.tilde.boot$t)




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
