require(ggplot2)
require(ggiraph)

#' Draw a Wilkinson dot plot
#'@param data a data.frame
#'@param xvar A character string of column name be used as a x-axis variable
#'@param yvar A character string of column name be used as a y-axis variable.
#'@param fillvar A character string of column name be used as a fill variable. Default value is NULL
#'@param stackdir which direction to stack the dots. "up" (default), "down", "center", "centerwhole" (centered, but with dots aligned)
#'@param binaxis The axis to bin along, "x" (default) or "y"
#'@param binwidth When method is "dotdensity", this specifies maximum bin width. When method is "histodot", this specifies bin width. Defaults to 1/30 of the range of the data
#'@param method	"dotdensity" (default) for dot-density binning, or "histodot" for fixed bin widths (like stat_bin)
#'@param position Position adjustment. If 0, no adjustment.
#'@param boxwidth The width of boxplot
#'@param boxfill Fill color of boxplot
#'@param ... other arguments passed on to geom_dotplot
#'
#'@examples
#'require(ggplot2)
#'require(gcookbook) # for data heightweight
#'require(moonBook) #for use data radial
#'ggDot(radial,yvar="height",xvar="sex",fillvar="sex",boxfill="white",position=0,binwidth=1,boxwidth=1)
#'ggDot(heightweight,yvar="heightIn",xvar="sex",binwidth=0.4)
#'ggDot(radial,xvar="height",fillvar="sex",binwidth=1)
#'ggDot(heightweight,xvar="heightIn")
#'ggDot(heightweight,xvar="heightIn",fillvar="sex")
ggDot=function(data,xvar,yvar=NULL,fillvar=NULL,
               stackdir="center",binaxis="y",binwidth=0.5,method="dotdensity",
               position=0.2,boxwidth=0.25,
               boxfill=NULL,...){

    if(is.null(yvar)){
        binaxis="x"

        if(is.null(fillvar)) p<-ggplot(data,aes_string(x=xvar))
        else p<-ggplot(data,aes_string(x=xvar,fill=fillvar))
        p<-p+geom_dotplot(method=method,stackdir=stackdir,binaxis=binaxis,binwidth=binwidth,...)
    } else{

        if(is.null(fillvar)) {
            p<-ggplot(data,aes_string(x=xvar,y=yvar))
        } else {
            p<-ggplot(data,aes_string(x=xvar,y=yvar,fill=fillvar))
        }
        if(position==0){
            if(is.null(boxfill)) p<- p + geom_boxplot(width=boxwidth)
            else p<- p + geom_boxplot(fill=boxfill,width=boxwidth)
            p<-p+geom_dotplot(method=method,stackdir=stackdir,binaxis=binaxis,binwidth=binwidth)
        } else{
            if(is.null(boxfill)) p<- p + geom_boxplot(aes_string(x=paste0("as.numeric(",xvar, ")+ ",position),group=xvar),
                                                      width=boxwidth)
            else p<- p + geom_boxplot(aes_string(x=paste0("as.numeric(",xvar, ")+ ",position),group=xvar),
                                      fill=boxfill,width=boxwidth)

            p<-p+geom_dotplot(aes_string(x=paste0("as.numeric(",xvar, ")- ",position),group=xvar),
                              method=method,stackdir=stackdir,binaxis=binaxis,binwidth=binwidth)
            p<-p+scale_x_continuous(breaks=1:nlevels(data[[xvar]]),labels=levels(data[[xvar]]))
        }
        p<-p+theme(legend.position='none')
    }

    p
}

