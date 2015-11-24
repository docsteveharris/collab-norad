# 2014-11-29
# Trying to make a function call without using character vectors in fg_var_by_site.R
# Giving up!
# Function to compare by site 
compare.bysite <- function(var, byvar=hosp, byvar.id=id.hosp, data=wdt) {
    # Assumes data is stored in wdt
    args <- as.list(match.call()[-1])
    # extract arg vars
    var <- as.character(args$var)
    byvar <- as.character(args$byvar)
    byvar.id <- as.character(args$byvar.id)
    # Convert to dataframe for ease of referencing
    tdf <- as.data.frame(data)
    tdf <- tdf[,c(byvar, var, byvar.id)]
    str(tdf)
    # tplot <- ggplot(tdf, aes(x=byvar.id, y=var))
    # # tplot +
    # #     facet_grid(. ~ byvar, labeller = label_both) +
    # #     geom_point(alpha=0.3, size=1.5) +
    # #     geom_smooth(method="lm")
    # return(tplot)
}
compare.bysite(var=map.1)
