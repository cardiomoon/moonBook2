require(ggplot2)
require(reshape2)
require(ggiraph)
require(plotly)

#' Make an interactive scatter and line plot
#'
#' @param data a data.frame
#' @param vars names of columns which are incuded in plot
#' @param colorvar names of column which is assigned as a colour variable
#' @param idcolor Logical cvalue. If TRUE, row numbers uses as a color variable
#' @param horizontal Logical cvalue. If TRUE, coord_flip() function is used to make a horizontal plot
#' @param interactive Logical cvalue. If TRUE, an interactive plot using ggiraph() function will be returned
#'
#' @examples
#' require(ggplot2)
#' require(ggiraph)
#' ggPair(iris,interactive=TRUE)
#' ggPair(iris[3:5],interactive=TRUE)
#' ggPair(iris,colorvar="Species",interactive=TRUE)
#' ggPair(iris,colorvar="Species",horizontal=TRUE, interactive=TRUE)
#' ggPair(iris,vars=c("Sepal.Length","Sepal.Width"),interactive=TRUE)
#' ggPair(iris,vars=c("Sepal.Length","Sepal.Width"),colorvar="Species",interactive=TRUE)
#' ggPair(iris,vars=c("Sepal.Length","Sepal.Width"),colorvar="Species",horizontal =TRUE,interactive=TRUE)
ggPair=function(data,vars=NULL,colorvar=NULL,idcolor=TRUE,horizontal=FALSE,interactive=FALSE) {
    df=data
    if(is.null(vars)) {

        select=unlist(lapply(data,is.numeric))
        if(!is.null(colorvar)) select[colorvar]=TRUE
        df1=df[c(select)]
        vars=colnames(df1)
        vars=setdiff(vars,colorvar)
        varcount=length(vars)

    } else {
        df1=df[c(vars,colorvar)]
        varcount=length(vars)
    }

    df1$id=1:nrow(df1)
    temp=df1$id
    for(i in 1:(length(df1)-1)) {
        temp=paste0(temp,"<br>",names(df1)[i],":",df1[[i]])
    }

    df1$tooltip=temp
    #df1$tooltip=paste0(df1$id,"<br>",df1[[1]],"<br>",df[[2]])
    #str(df1)
    longdf=reshape2::melt(df1,id=c("id","tooltip",colorvar))
    if(is.null(colorvar) & idcolor) colorvar="id"

    #str(longdf)
    #str(longdf)
    p<-ggplot(data=longdf,
              aes_string(x="variable",y="value",group="id",colour=colorvar))+
        geom_point_interactive(aes(data_id=id,tooltip=tooltip))+
        geom_path_interactive(aes(data_id=id,tooltip=tooltip))+xlab("")+ylab("")
    if(horizontal) p<-p+coord_flip()
    if(!is.null(colorvar)) {
        if(colorvar=="id") p<-p+guides(colour=FALSE)
    }
    if(varcount==2) {
        longdf1=longdf[longdf$variable==vars[1],]
        #longdf1$toolitip=paste0(longdf1$id,"<br>",vars[1])
        p<-p+geom_boxplot_interactive(data=longdf1,
                                      aes(x=as.numeric(variable)-0.2,group=NULL,data_id=id,tooltip=vars[1]),width=0.2)
        longdf2=longdf[longdf$variable==vars[2],]
        #longdf2$toolitip=paste0(longdf2$id,"<br>",vars[2])
        p<-p+geom_boxplot_interactive(data=longdf2,
                                      aes(x=as.numeric(variable)+0.2,group=NULL,data_id=id,tooltip=vars[2]),width=0.2)
    }
    tooltip_css <- "background-color:white;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;"
    hover_css="fill-opacity=.3;cursor:pointer;stroke:gold;"
    if(interactive) p<-ggiraph(code=print(p),tooltip_extra_css=tooltip_css,tooltip_opacity=.75,
                               zoom_max=10,hover_css=hover_css)
    p
}


#' Make an interactive scatter and line plot with long form data.frame
#'
#' @param data a data.frame with a long form
#' @param idvar a name of column used as an id variable
#' @param variablevar a name of column used as an variable
#' @param valuevar a name of column containa a value
#' @param colorvar names of column which is assigned as a colour variable
#' @param idcolor Logical cvalue. If TRUE, row numbers uses as a color variable
#' @param horizontal Logical cvalue. If TRUE, coord_flip() function is used to make a horizontal plot
#' @param interactive Logical cvalue. If TRUE, an interactive plot using ggiraph() function will be returned
#'
#' @examples
#'require(reshape2)
#'require(ggplot2)
#'iris$id=1:nrow(iris)
#'irislong=melt(iris[3:6],id=c("id","Species"))
#'
#'ggPairLong(irislong,idvar="id",colorvar = "Species")
#'ggPairLong(irislong,idvar="id",colorvar = "Species",interactive=TRUE)
#'ggPairLong(irislong,idvar="id",interactive=TRUE)
ggPairLong=function(data,idvar="id",variablevar="variable", valuevar="value",colorvar=NULL,
                     idcolor=TRUE,horizontal=FALSE,interactive=FALSE){
    longdf=data
    temp=idvar
    if(!is.null(colorvar)) temp=paste0(temp,"+",colorvar)
    temp=paste0(temp,"~",variablevar)
    temp=paste0(temp,",value.var='",valuevar,"'")
    #temp
    wdf=eval(parse(text=paste0("dcast(data,",temp,")")))
    df=eval(parse(text=paste0("subset(wdf,select=-",idvar,")")))
    #str(df)

    ggPair(df,colorvar=colorvar,idcolor=idcolor,horizontal=horizontal,interactive=interactive)
}


