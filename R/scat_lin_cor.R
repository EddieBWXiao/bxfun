#' Scatter plot with correlation and optional regression line
#'
#' Quick scatter plot between two variables with regression line, correlation coefficient title, and optional reference lines. Designed for parameter recovery, model validation etc.
#' The function is similar to `scatr::scat` with more options but no univariate density plots.
#'
#' @param data A data frame containing the variables to be plotted.
#' @param x A string specifying the name of the variable on the x-axis.
#' @param y A string specifying the name of the variable on the y-axis.
#' @param xlab (optional) A string for the x-axis label. Defaults to the value of `x`.
#' @param ylab (optional) A string for the y-axis label. Defaults to the value of `y`.
#' @param alpha (optional) A numeric value for the transparency of points, between 0 and 1. Default is 0.8.
#' @param reg (optional) A logical indicating whether to fit a regression line. Default is TRUE.
#' @param ci (optional) A logical indicating whether to display a confidence interval around the regression line. Default is TRUE.
#' @param refline (optional) A logical indicating whether to add a reference line (y = x). Default is FALSE.
#' @param BF (optional) A logical indicating whether to display the Bayes Factor. Default is FALSE.
#' @param pval (optional) A logical indicating whether to display the p-value. Default is FALSE.
#' @param method (optional) A string specifying the correlation method to be used. Options are "pearson", "spearman", and "kendall". Default is "pearson".
#'
#' @details
#' The function computes the correlation between the variables `x` and `y` using the specified method
#' and displays the correlation coefficient in the plot title. If `BF` is TRUE, the function calculates
#' and displays the Bayes Factor for Pearson correlation (currently not supported for Spearman or Kendall).
#' If `pval` is TRUE, the p-value from the correlation test is also displayed. The function
#' adds a regression line, confidence interval, and optionally adds a reference line for comparison.
#'
#' @return A `ggplot` object representing the scatter plot.
#'
#' @examples
#' # Example usage:
#' data(mtcars)
#' scat.lin.cor(mtcars, x = "mpg", y = "wt", reg = TRUE, ci = TRUE, pval = TRUE)
#'
#' @import ggplot2
#' @import stats
#' @export
scat.lin.cor<-function(data,
                       x,y, #need to be strings
                       xlab = x, ylab=y,
                       alpha = 0.8,
                       #cor = "title", #future options will include c("title","none","inside")
                       reg = T, #whether to fit a regression line
                       ci = T,
                       refline = F,#a la MATLAB refline
                       BF = F,
                       pval = F,
                       method="pearson"){
    # scatter plot between two variables, adding geom_smooth and correlations
    # similar to scatr::scat, but with cor.test built in and with reference lines
    # default: show correlation coefficient, not p values

    #actual plot
    outplot<-ggplot(data,
                    aes(x = .data[[x]], y = .data[[y]])) +
      geom_point(alpha = alpha)
    if(reg){
      outplot<-outplot+
        geom_smooth(method=lm,
                    se=ci) #not sure why called se when it is in fact the confint
    }

    # correlation
    getCor<-stats::cor.test(as.numeric(unlist(data[,c(x)])),
                           as.numeric(unlist(data[,c(y)])),
                           method=method,exact=F)

    # For label of correlation coefficient
    if(method=="pearson"){
      corrlabel <- sprintf("Pearson's r = %.2f",getCor$estimate[[1]])
    }else if(method=="spearman"){
      corrlabel <- sprintf("Spearman's rho = %.2f",getCor$estimate[[1]])
    }else if(method=="kendall"){
      corrlabel <- sprintf("Kendall's tau = %.2f",getCor$estimate[[1]])
    }

    #for Bayes Factor:
    if(BF){
      if(method=="pearson"){
        BFcor<-BayesFactor::correlationBF(as.numeric(unlist(data[,c(x)])),
                                          as.numeric(unlist(data[,c(y)])))
        theBF<-BayesFactor::extractBF(BFcor)$bf
      }else if(method=="kendall"){
        # BFcor<-KendallBayesFactor(as.numeric(unlist(data[,c(x)])),
        #                           as.numeric(unlist(data[,c(y)])))
        # theBF<-BFcor$bf10
        stop("Bayes Factor unavailable for Kendall's tau")
      }else if(method=="spearman"){
        stop("Bayes Factor unavailable for Spearman's rho")
      }
    }

    if(BF){
      corrlabel <- paste0(corrlabel, ", BF = ",
                          format(theBF, scientific = TRUE,
                                 digits = 3))
    }
    if(pval){
      if(getCor$p.value<0.001){
        corrlabel <- paste0(corrlabel,", p < 0.001")
      }else{
        corrlabel <- paste0(corrlabel,", ",sprintf("p = %.3f", getCor$p.value))
      }
    }

    # reflines
    if(refline){
      outplot<-outplot+
        geom_abline(intercept = 0, slope = 1,
                    colour = "red",linetype="dashed")
    }

    #assemble & style the final figure
    outplot<-outplot+labs(y= ylab, x = xlab, title = corrlabel)+
      theme_classic()+
      theme(plot.title = element_text(hjust = 0.5))

    return(outplot)
  }
