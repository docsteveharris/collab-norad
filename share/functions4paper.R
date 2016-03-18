# Load functions
# intra-class correlation
ICC <- function(m) {
    require(lme4)
    v <- VarCorr(m)
    print(v)
    # get the group level standard deviation
    sd.g <- attr(v[[1]], "stddev")
    # get the residual standard deviation
    sd.r <- attr(v, "sc")
    # calculate the ICC
    icc <- (sd.g^2 / (sd.g^2 + sd.r^2))
    # set the name
    names(icc)[1] <- "ICC"
    # calculate the variance ratio
    var.ratio <- sd.g^2 / sd.r^2
    names(var.ratio)[1] <- "Variance ratio"
    return(c(icc,var.ratio))
}

# function to produce coefficient plots
coef.plot <- function(model=m) {

    # Load packages
    require(data.table)
    require(ggplot2)

    # Test the model class
    if(class(m)[1]=="lm") {
        m.coef <- summary(m)$coefficients
        m.coef <- data.frame(summary(m)$coefficients)
        colnames(m.coef)[1] <- "est"
        colnames(m.coef)[2] <- "se"
        colnames(m.coef)[3] <- "t"
        colnames(m.coef)[4] <- "p"
    } else if (class(m)[1]=="lmerMod") {
        m.coef <- data.frame(summary(m)$coefficients)
        colnames(m.coef)[1] <- "est"
        colnames(m.coef)[2] <- "se"
        colnames(m.coef)[3] <- "t"
    } else if (class(m)[1]=="glm") {
        # If converting then will need to know link function
        m.coef <- data.frame(summary(m)$coefficients)
        colnames(m.coef)[1] <- "est"
        colnames(m.coef)[2] <- "se"
        colnames(m.coef)[3] <- "t"
        colnames(m.coef)[4] <- "p"
    } else if (class(m)[1]=="glmerMod") {
        # If converting then will need to know link function
        m.coef <- data.frame(summary(m)$coefficients)
        colnames(m.coef)[1] <- "est"
        colnames(m.coef)[2] <- "se"
        colnames(m.coef)[3] <- "z"
        colnames(m.coef)[4] <- "p"
    } else {
        warning("Unknown model class: exiting function call")
        return(NULL)
    }

    # Convert to data.table and keep rownames
    m.coef <- data.table(m.coef, keep.rownames=TRUE)
    # Remove the intercept
    m.coef <- m.coef[rn!="(Intercept)"]
    print(m.coef)

    gg.p <- ggplot(data=m.coef, aes(x=rn, y=est)) + 
        geom_hline(yintercept=0, colour="grey", ) +
        geom_linerange(aes(ymin=est - 2 * se, ymax=est + 2 * se), colour="grey") +
        geom_pointrange(aes(ymin=est - 1 * se, ymax=est + 1 * se), lwd=0.5, fatten=2) +
        xlab("Predictor") + ylab("Effect estimate") +
        coord_flip() +
        theme_minimal()
    return(gg.p)
}

