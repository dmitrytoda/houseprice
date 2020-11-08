# Copied from https://stackoverflow.com/questions/47342553/boruta-box-plots-in-r

# Here, I create a new function based on the source function plot.Boruta, and add a function argument pars that takes the names of variables/predictors that we'd like to include in the plot.



# generateCol is needed by plot.Boruta
generateCol<-function(x,colCode,col,numShadow){
 #Checking arguments
 if(is.null(col) & length(colCode)!=4)
  stop('colCode should have 4 elements.');
 #Generating col
 if(is.null(col)){
  rep(colCode[4],length(x$finalDecision)+numShadow)->cc;
  cc[c(x$finalDecision=='Confirmed',rep(FALSE,numShadow))]<-colCode[1];
  cc[c(x$finalDecision=='Tentative',rep(FALSE,numShadow))]<-colCode[2];
  cc[c(x$finalDecision=='Rejected',rep(FALSE,numShadow))]<-colCode[3];
  col=cc;
 }
 return(col);
}


# We now modify plot.Boruta, and add a function parameter pars, by which we filter our list of variables.

# Modified plot.Boruta
plot.Boruta.sel <- function(
    x,
    pars = NULL,
    colCode = c('green','yellow','red','blue'),
    sort = TRUE,
    whichShadow = c(TRUE, TRUE, TRUE),
    col = NULL, xlab = 'Attributes', ylab = 'Importance', ...) {

    #Checking arguments
    if(class(x)!='Boruta')
        stop('This function needs Boruta object as an argument.');
    if(is.null(x$ImpHistory))
        stop('Importance history was not stored during the Boruta run.');

    #Removal of -Infs and conversion to a list
    lz <- lapply(1:ncol(x$ImpHistory), function(i)
        x$ImpHistory[is.finite(x$ImpHistory[,i]),i]);
    colnames(x$ImpHistory)->names(lz);

    #Selection of shadow meta-attributes
    numShadow <- sum(whichShadow);
    lz <- lz[c(rep(TRUE,length(x$finalDecision)), whichShadow)];

    #Generating color vector
    col <- generateCol(x, colCode, col, numShadow);

    #Ordering boxes due to attribute median importance
    if (sort) {
        ii <- order(sapply(lz, stats::median));
        lz <- lz[ii];
        col <- col[ii];
    }

    # Select parameters of interest
    if (!is.null(pars)) lz <- lz[names(lz) %in% pars];

    #Final plotting
    graphics::boxplot(lz, xlab = xlab, ylab = ylab, col = col, ...);
    invisible(x);
}


# Now all we need to do is call plot.Boruta.sel instead of plot, and specify the variables that we'd like to include.