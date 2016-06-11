require(XML)
require(reshape2)
require(ggplot2)
require(plyr)
require(scales)
require(ggiraph)

#' Human Numbers: Format numbers so they're legible for humans
#' Use this in ggplot for labels where you might use the comma or percent functions from the
#' Scales package.
#' Checks whether numbers are positive or negative.
#' Allows up to 1 significant figure
#' sapply used for element-wise application of the humanity function as a vector may include
#' numbers where billions, millions or thousands are appropriate.
#'
#' Source: https://github.com/fdryan/R/blob/master/ggplot2_formatter.r
#' ---------------------------------------------------------------------------------------------
#' Formatting functions for ggplot  graph axis
#' ---------------------------------------------------------------------------------------------
#'
#' @return a character vector the same length as the input vector
#' @param x a numeric vector to format
#' @param smbl a symbol you'd like to prefix your numbers by
#' @param allpos A logical value. If true, an absolute value will be returned
#' @examples
#'# human_numbers(c(1000000 , 1500000, 10000000000))
#'# human_numbers(c(1.200000e+05, -2.154660e+05, 2.387790e+05, 4.343500e+04 ,5.648675e+12), "$")
#'# ggplot2 + scale_y_continuous(labels = human_numbers)
#'# ggplot2 + scale_x_continuous(labels = human_numbers)
human_numbers <- function(x = NULL, smbl ="", allpos = FALSE){
    humanity <- function(y){

        if (!is.na(y)){

            b <- round_any(abs(y) / 1e9, 0.1)
            m <- round_any(abs(y) / 1e6, 0.1)
            k <- round_any(abs(y) / 1e3, 0.1)

            if ( y >= 0 ){
                y_is_positive <- ""
            } else {
                y_is_positive <- "-"
            }
            if(allpos) y_is_positive <- ""

            if ( k < 1 ) {
                paste0(y_is_positive, smbl, y )
            } else if ( m < 1){
                paste0 (y_is_positive, smbl,  k , "k")
            } else if (b < 1){
                paste0 (y_is_positive, smbl, m ,"m")
            } else {
                paste0 (y_is_positive, smbl,  comma(b), "b")
            }
        }
    }

    sapply(x,humanity)
}

#' Human versions of large currency numbers - for "£"
#'@param x a numeric vector to format, smbl a symbol you'd like to prefix your numbers by
#'@return a character vector the same length as the input vector
human_gbp   <- function(x){human_numbers(x, smbl = "\uc2a3")}

#' Human versions of large currency numbers - for "$"
#'@param x a numeric vector to format, smbl a symbol you'd like to prefix your numbers by
#'@return a character vector the same length as the input vector
human_usd   <- function(x){human_numbers(x, smbl = "\u24")}

#' Human versions of large currency numbers - for "€"
#'@param x a numeric vector to format, smbl a symbol you'd like to prefix your numbers by
#'@return a character vector the same length as the input vector
human_euro  <- function(x){human_numbers(x, smbl = "\ue282a0")}

#' Human versions of large numbers, no units
#'@param x a numeric vector to format, smbl a symbol you'd like to prefix your numbers by
#'@return a character vector the same length as the input vector
human_num   <- function(x){human_numbers(x, smbl = "")}

#' Human versions of large numbers, no units
#'@param x a numeric vector to format, smbl a symbol you'd like to prefix your numbers by
#'@return a character vector the same length as the input vector
human_num2   <- function(x){human_numbers(x, smbl = "",allpos=TRUE)}

#'Grabs the required population data from the US Census Bureau’s International Data Base and outputs a data frame in the right format for ggplot2.
#'
#'Modified from the source: rpubs.com/walkerke/pyramids_ggplot2
#'@param country  \href{https://en.wikipedia.org/wiki/List_of_FIPS_country_codes}{a FIPS 10-4 country code} for the country you want
#'@param year The year you want to visualize.
#'@return a data.frame
#'
#'@examples
#'#KS2016=get_popdata("KS",2016)
#'#ggBidirectionalBar(data=KS2016,left="Male",right="Female",label="Age")
#'#ggBidirectionalBar(data=KS2016,left="Male",right="Female",label="Age",interactive=TRUE)
get_popdata <- function(country, year) {
        c1 <- "http://www.census.gov/population/international/data/idb/region.php?N=%20Results%20&T=10&A=separate&RT=0&Y="
        c2 <- "&R=-1&C="
        url <- paste0(c1, year, c2, country)
        df <- data.frame(readHTMLTable(url))
        keep <- c(2, 4, 5)
        df <- df[,keep]
        names(df) <- c("Age", "Male", "Female")
        cols <- 2:3
        df[,cols] <- apply(df[,cols], 2, function(x) as.numeric(as.character(gsub(",", "", x))))
        df <- df[df$Age != 'Total', ]
        df
}


#'Draw an interactive bidirectional bar plot
#'@param data A data.frame
#'@param left  A character vector of column names be assigned to left-sided bar
#'@param right  A character vector of column names be assigned to right-sided bar
#'@param label A character vector of column names be assigned to the label
#'@param title Plot title
#'@param interactive A logical value. If TRUE, an interactive plot will be returned
#'
#'@return An interactive interactive bidirectional bar plot
#'
#'@examples
#'#KS2016=get_popdata("KS",2016)
#'#ggBidirectionalBar(data=KS2016,left="Male",right="Female",label="Age")
#'#ggBidirectionalBar(data=KS2016,left="Male",right="Female",label="Age",interactive=TRUE)
ggBidirectionalBar=function(data,left=NULL,right=NULL,label=NULL,
                            title="",interactive=FALSE){


    data[[left]] <- -1 * data[[left]]
    data[[label]] <- factor(data[[label]],levels=data[[label]])
    longdf <- melt(data,id.vars=label )
    longdf$xmin=as.numeric(longdf[[label]])-0.95
    longdf$xmax=as.numeric(longdf[[label]])-0.05
    longdf$id=as.character(1:nrow(longdf))
    longdf$tooltip=paste0(longdf$variable,"(",longdf[[label]],")",abs(longdf$value))


            p<-ggplot(longdf, aes_string(xmin="xmin",xmax="xmax"
                                         ,ymin="0",ymax = "value", fill = "variable",
                                         tooltip="tooltip",data_id="id")) +
                geom_rect_interactive(data=subset(longdf, variable == right), stat = "identity",alpha=0.7)+
                geom_rect_interactive(aes(ymin=value,ymax=0),data=subset(longdf, variable == left), stat = "identity",alpha=0.7)+
                coord_flip() +
                scale_fill_brewer(palette = "Set1") +
                theme_bw()+theme(legend.position=c(0.15,0.92))+
                guides(fill=guide_legend(title=NULL,reverse=TRUE))+
                scale_x_continuous(breaks=1:length(data[[label]]),labels=levels(data[[label]]))+
                scale_y_continuous(labels=human_num2)+ylab("")+ggtitle(title)
            if(interactive) p<-ggiraph(code=print(p))
            p

}


#'Get population data and draw popuation pyramid
#'
#'@param country \href{https://en.wikipedia.org/wiki/List_of_FIPS_country_codes}{a FIPS 10-4 country code} for the country you want
#'@param year The year you want to visualize.
#'@param interactive A logical value. If TRUE, an interactive plot will be returned
#'
#'@return A population pyramid
#'@examples
#'#PopPyramid("KS",2016)
#'#PopPyramid("NI",2015)
#'#PopPyramid("JA",2015)
#'#PopPyramid("VQ",2015)
PopPyramid=function(country,year,interactive=FALSE){
        popdata=get_popdata(country,year)
        ggBidirectionalBar(data=popdata,left="Male",right="Female",label="Age",
                title=paste("Population",country,year),interactive=interactive)
}









