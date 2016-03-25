# Load functions

# Mosaic plot with ggplot function
# Mosaic plot with ggplot function
ggMMplot <- function(var1, var2, xlab="", ylab=""){
  require(ggplot2)
  require(ggthemes)
  levVar1 <- length(levels(var1))
  levVar2 <- length(levels(var2))

  jointTable <- prop.table(table(var1, var2))
  plotData <- as.data.frame(jointTable)
  plotData$marginVar1 <- prop.table(table(var1))
  plotData$var2Height <- plotData$Freq / plotData$marginVar1
  plotData$var1Center <- c(0, cumsum(plotData$marginVar1)[1:levVar1 -1]) +
    plotData$marginVar1 / 2

  ggplot(plotData, aes(var1Center, var2Height)) +
    geom_bar(stat = "identity", aes(width = marginVar1, fill = var2), col = "White") +
    scale_fill_grey(start=0,end=0.9) +
    ylab(ylab) +
    xlab(xlab) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels=NULL) +
    geom_text(aes(label = as.character(var1), x = var1Center, y = -0.05, size=0.5)) +
    guides(fill="none", size="none") +
    coord_fixed(ratio=1) +
    theme_pander()
}


t1.trend <- function(v=var, strata, data=wdt, var.cont=FALSE) {
    '
    Report if there is a trend across categories (strata)
    Assumes that var is categorical (then Cochran-Armitage) unless var.cont=TRUE
    '
    require(data.table)
    require(assertthat)
    if (var.cont) {
        m <- lm(get(v)~ as.numeric(get(strata)), data=data)
        return(summary(m)$coefficients[2,4])
    } else {
        t.n <- table(data[!is.na(get(v)), strata, with=FALSE])
        t.0 <- table(data[!is.na(get(v)) & get(v) == 0, strata, with = FALSE])
        t.1 <- table(data[!is.na(get(v)) & get(v) == 1, strata, with = FALSE])
        # lapply(list(v, t.n, t.0, t.1), function(x) print(x)) # PRINT FOR DEBUGGING
        # Check the variable is binary
        assert_that(sum(as.vector(t.0) + as.vector(t.1) - as.vector(t.n))==0)
        return(prop.trend.test(t.1,t.n)$p.value)
    }
}

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
coef.plot <- function(model=m, m.labels=NULL) {

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
        # - [ ] FIXME(2016-03-18): Cheating and using the normal approx to get p values
        m.coef$z <- m.coef$est / m.coef$se
        m.coef$p <- pnorm(abs(m.coef$z), lower.tail=FALSE)
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
    # Force ordering of indep vars
    m.coef[, ivar := factor(.I, labels=m.coef$rn, ordered=TRUE)]
    print(m.coef)
    length(m.coef$rn)

    if (is.null(m.labels)) {
        m.labels <- rev(m.coef$rn)
    } else {
        assert_that(length(m.labels)==length(m.coef$rn))
        # Check they line up
        print(data.frame(m.labels, m.coef$rn))
        # Now reverse so ordered top to bottom in plot
        m.labels <- rev(m.labels)
    }

    gg.p <- ggplot(data=m.coef, aes(x=ivar, y=est)) + 
        geom_hline(yintercept=0, colour="grey", ) +
        geom_linerange(aes(ymin=est - 2 * se, ymax=est + 2 * se), colour="grey") +
        geom_pointrange(aes(ymin=est - 1 * se, ymax=est + 1 * se), lwd=0.5, fatten=2) +
        scale_x_discrete(limits=rev(levels(m.coef$ivar)), labels=m.labels) +
        xlab("Predictor") + ylab("Effect estimate") +
        coord_flip() +
        theme_minimal()
    # print(gg.p)
    return(gg.p)
}

