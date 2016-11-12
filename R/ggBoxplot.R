require(ggplot2)
require(ggiraph)
require(reshape2)

#' Draw boxplots of a data.frame
#'
#'@param data a data.frame
#'@param rescale if true, rescale the data.frame
#'@param horizontal if true, horizontal boxplots will be made
#'@param interactive A logical value. If TRUE, an interactive plot will be returned
#'@param ... other arguments passed on to geom_boxplot_interactive
#'
#'@examples
#'ggBoxplot(mtcars)
#'ggBoxplot(mtcars,horizontal=TRUE,interactive=TRUE)
ggBoxplot=function(data,rescale=FALSE,horizontal=FALSE,interactive=FALSE,...){
#    data=acs;rescale=FALSE;horizontal=FALSE;interactive=FALSE
    numeric=unlist(lapply(data,is.numeric))
    df=data[numeric]
    if(rescale) df<-data.frame(apply(df,2,scale))
    df$id=1:nrow(df)

    longdf=reshape2::melt(df,id="id")
    #str(longdf)
    p<-ggplot(longdf,aes(y=value,x=variable,color=variable,data_id=id,tooltip=variable))+
        #geom_boxplot()
        geom_boxplot_interactive(...)+
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              legend.position="none")

    if(horizontal) p<-p+coord_flip()

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
