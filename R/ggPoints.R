require(ggplot2)
require(moonBook)
require(plotly)

#' Draw scatterplot
#'
#'@param x An R object to ggPoints
ggPoints<-function(x,...) UseMethod("ggPoints")


#'@describeIn ggPoints
#' Draw scatterplot
#'
#'@param x A data.frame
#'@param xvar A character string of column name be used as a x-axis variable
#'@param yvar A character string of column name be used as a y-axis variable.
#'@param colour A character string of column name be used as a colour variable. Default value is NULL
#'@param fill A character string of column name be used as a fill variable. Default value is NULL
#'@param smooth A logical value. If TRUE, a layer of geom_smooth() added
#'@param method smoothing method (function) to use, eg. lm, glm, gam, loess, rlm.
#'         For datasets with n < 1000 default is \code{\link[stats]{loess}}.
#'         For datasets with 1000 or more observations defaults to gam, see \code{\link[mgcv]{gam}} for more details.
#'@param formula formula formula to use in smoothing function, eg. y ~ x, y ~ poly(x, 2), y ~ log(x)
#'@param se display confidence interval around smooth? (TRUE by default, see level to control
#'@param jitter A logical value. If TRUE, a layer of geom_jitter() used instead of geom_point()
#'@param fullrange should the fit span the full range of the plot, or just the data
#'@param level level of confidence interval to use (0.95 by default)
#'@param interactive A logical value. If TRUE, an interactive plot will be returned
#'
#'@return An interactive plot or ggplot
#'
#' @examples
#'# require(ggplot2)
#'# require(moonBook)
#'# ggPoints(iris,xvar=Sepal.Length,yvar=Sepal.Width,colour="Species",interactive=TRUE)
#'# ggPoints(mtcars,yvar=mpg,xvar=wt,fill="factor(cyl)",pch=21,smooth=FALSE)
#'# ggPoints(acs,xvar=height,yvar=weight,colour="sex",interactive=TRUE,
#'#           position="jitter",method="lm",formula=y~poly(x,2))
ggPoints.default=function(x,xvar,yvar,colour=NULL,fill=NULL,smooth=TRUE,
                  method="auto",formula=y~x,se=TRUE,level=0.95,jitter=FALSE,
                  fullrange=FALSE,interactive=FALSE,...){

    data<-x
    data$data_id=1:nrow(data)
    xvar<-as.character(substitute(xvar))
    yvar<-as.character(substitute(yvar))

    if(is.null(colour) & is.null(fill)) {
        mapping=aes_string(x=xvar,y=yvar)
    } else if(is.null(colour)) {
        mapping=aes_string(x=xvar,y=yvar,fill=fill)
    } else if(is.null(fill)) {
        mapping=aes_string(x=xvar,y=yvar,colour=colour)
    } else{
        mapping=aes_string(x=xvar,y=yvar,colour=colour,fill=fill)
    }

    p<-ggplot(data=data,mapping)
    if(jitter) p<-p+geom_jitter(aes(text=paste0("no:",data_id)),...)
    else p<-p+ geom_point(aes(text=paste0("no:",data_id)),...)

    if(smooth) p<- p+geom_smooth(method=method,formula=formula,se=se,level=level,fullrange=fullrange)
    if(interactive) p<-ggplotly(p)
    p
}

#'@describeIn ggPoints
#' Draw scatterplot
#'
#'@param form A formula of type y ~ x | B
#'@param data A data.frame
#'@param ... other arguments passed on to ggPoint.defalut
#'
#'@examples
#'#ggPoints.formula(weight~height|sex,data=radial,interactive=TRUE)
#'#ggPoints.formula(weight~height,data=radial)
ggPoints.formula=function(x,data,...){
    call = paste(deparse(x), ", ", "data= ", substitute(data),
                 sep = "")
    f = x
    myt = terms(f, data = data)
    # str(f)
    # str(myt)
    # str(call)
    # print(as.character(f[[1]]))

    y=as.character(f[[2]])
    # cat("\ny=",y)
    a=as.character(f[[3]])
    if(length(a)==1) {
        x=a
        # cat("\nx=",x)
        temp=paste("ggPoints(x=",substitute(data),",yvar=",y,",xvar=",x,",...)")
        # print(temp)

    } else if(length(a)==3) {
    #cat("\na=",a,"length(a)=",length(a))
       # cat("\nx=",a[2])
        x<-a[2]
       # cat("\ngroup=",a[3])
         group<-a[3]
         # cat("\ngroup=",group,"\n")
         temp=paste0("ggPoints(x=",substitute(data),",yvar=",y,",xvar=",x,
                    ",colour='",group,"',...)")
         # print(temp)

    }
    eval(parse(text=temp))
}


