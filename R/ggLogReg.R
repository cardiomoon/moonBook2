require(ggplot2)
require(ggiraph)

#' Draw a jittered scatter plot for logistic regression model
#'@param data a data.frame
#'@param yvar A character string of column name be used as a response(y-axis) variable.
#'@param xvar A character string of column name be used as a explanatory(x-axis) variable
#'@param se Logical. display confidence interval around linear regression? (TRUE by default)
#'@param interactive A logical value. If TRUE, an interactive plot will be returned
#'@param ... other arguments passed on to geom_point
ggLogReg=function(data,yvar,xvar,se=TRUE,interactive=FALSE,...){
    data$id=1:nrow(data)
    data$tooltip=paste0(data$id,"<br>",xvar,"=",data[[xvar]],"<br>",yvar,"=",data[[yvar]])
    p<-ggplot(data,aes_string(x=xvar,y=yvar))+
        stat_smooth(method='glm',method.args=list(family='binomial'),se=se)+
        geom_point_interactive(aes(tooltip=tooltip,data_id=id),
                               position=position_jitter(width=0.3,height=0.06),alpha=0.5,...)
    if(interactive){
        tooltip_css <- "background-color:white;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;"
        #hover_css="fill-opacity=.3;cursor:pointer;stroke:gold;"
        hover_css="r:4px;cursor:pointer;stroke-width:6px;"
        selected_css = "fill:#FF3333;stroke:black;"
        p<-ggiraph(code=print(p),tooltip_extra_css=tooltip_css,tooltip_opacity=.75,
                   zoom_max=10,hover_css=hover_css,selected_css=selected_css)
    }
    p

}


