require(reshape2)
require(plyr)
require(ggplot2)
require(ggiraph)
require(scales)


#'The radar coordinate system is a modification of polar coordinate system, commly used for radar chart
#'
#'@param theta variable to map angle to (x or y)
#'@param start offset of starting point from 12 o'clock in radians
#'@param direction 1, clockwise; -1, counterclockwise
coord_radar <- function (theta = "x", start = 0, direction = 1)
{
        theta <- match.arg(theta, c("x", "y"))
        r <- if (theta == "x")
                "y"
        else "x"
        ggproto("CoordRadar", ggplot2::CoordPolar, theta = theta, r = r, start = start,
                direction = sign(direction),
                is_linear = function(coord) TRUE)
}

#'Rescale all numeric variables of a data.frame except grouping variable
#'
#'@param data A data.frame
#'@param groupvar A column name used as a grouping variable
#'
#'@return A rescaled data.frame
rescale_df=function(data,groupvar=NULL){
        if(is.null(groupvar)) df=data
        else df=data[,-which(names(data) %in% groupvar)]

        select=sapply(df,is.numeric)
        df[select]=lapply(df[select], scales::rescale)
        if(!is.null(groupvar)) {
                df=cbind(df,data[[groupvar]])
                colnames(df)[length(df)]=groupvar
        }
        df
}

#'Draw a radar chart
#'
#'@param data A data.frame
#'@param xvars A character vector of column names be assigned to x-axis variable
#'       If NULL, all continuous variables in the data.rame are used
#'@param yvar A character string of column name be assigned to the y variable.
#'       Maybe preprocessed nrow or mean. Be used only in the preprocessed data.frame
#'@param groupvar  A character string of column name be used as a grouping variable
#'@param rescale A logical value. If TRUE, all continuous variables in the data.frame are rescaled.
#'@param legend.position Legend position. One of c("top","bottom","left","right","none")
#'@param radar A logical value. If TRUE, a radar chart will be made
#'@param polar A logical value. If TRUE, a polar chart will be made
#'@param mean A logical value. If TRUE, a radar chart rescaled by mean will be made.
#'@param nrow A logical value. If TRUE, a radar chart rescaled by nrow will be made.
#'@param colour A name of color to be assigned as a color variable
#'@param ylim A numeric vector of length 2, giving the y coordinates ranges.
#'@param interactive A logical value. If TRUE, an interactive plot will be returned
#'
#'@return An interactive Pie and Donut plot
#'@examples
#'#ggRadar(data=iris,rescale=TRUE,groupvar="Species",interactive=TRUE,ylim=c(0,1))
#'#ggRadar(data1)
#'#ggRadar(data1,rescale=TRUE)+ylim(0,1)
#'#ggRadar(data1,groupvar="Class")
#'#ggRadar(data1,groupvar="Class",rescale=TRUE,interactive=TRUE)
#'#ggRadar(mtcars,groupvar="cyl",rescale=TRUE,interactive=TRUE)
#'#ggRadar(mtcars,rescale=TRUE,group="am")+ylim(0,1)
#'#ggRadar(mtcars,groupvar=c("cyl"))
#'#p<-ggRadar(mtcars,rescale=TRUE,group="am")
#'# ggsave("radarplot2.png",p)
#'#require(moonBook)
#'#ggRadar(data=acs,xvars=c("TC","TG","HDLC","LDLC"),groupvar="smoking")
ggRadar=function(data=iris,
                 xvars=NULL,
                 yvar=NULL,
                 groupvar=NULL,
                 rescale=FALSE,
                 legend.position="bottom",
                 radar=TRUE,polar=FALSE,
                 mean=TRUE,nrow=FALSE,
                 colour="red", ylim=NULL,
                 interactive=FALSE){


        if(is.null(xvars)) {
                select=sapply(data,is.numeric)
                xvars=colnames(data)[select]
        }
        if(is.null(yvar)){
                # if(!is.null(groupvar)) {
                #         for(i in 1:length(groupvar)) data[[groupvar[i]]]=factor(data[[groupvar[i]]])
                # }
                # data
                if(rescale) data=rescale_df(data,groupvar)
                longdf=melt(data,id.vars=groupvar,measure.vars=xvars)
                longdf
                if(mean)
                  df=ddply(longdf,c(groupvar,"variable"),summarize,mean(value,na.rm=TRUE))
                if(nrow)
                        df=ddply(longdf,c(groupvar,"variable"),"nrow")

                colnames(df)[length(df)]="value"
                #print(df)
        } else{
                longdf=data
        }


        if(is.null(groupvar)){
                df$id=1:nrow(df)
                df$tooltip=paste0(df$variable,"=",round(df$value,1))
                p<-ggplot(data=df,aes_string(x="variable",y="value",group=1,tooltip="tooltip",data_id="id"))+
                        geom_point_interactive(size=3,colour=colour)+
                        geom_polygon(colour=colour,fill=colour,alpha=0.4)

        } else {

                df=df[!(df$variable %in% groupvar),]
                df$id2=df[[groupvar]]
                df$id=1:nrow(df)
                df$tooltip=paste0(groupvar,"=",df[[groupvar]],"<br>",df$variable,"=",round(df$value,1))
                df$tooltip2=paste0(groupvar,"=",df[[groupvar]])
                df
                for(i in 1:length(groupvar)) df[[groupvar[i]]]=factor(df[[groupvar[i]]])
                p<-ggplot(data=df,aes_string(x="variable",y="value",tooltip="tooltip",data_id="id",
                                          colour=groupvar,fill=groupvar,group=groupvar))+
                        geom_polygon_interactive(aes(tooltip=tooltip2,data_id=id2),alpha=0.4)+
                        geom_point_interactive(size=3)
        }
        p<- p+ xlab("")+ylab("")+theme(legend.position=legend.position)

        if(radar==TRUE) p<-p+coord_radar()
        if(polar==TRUE) p<-p+coord_polar()
        if(!is.null(ylim)) p<-p+expand_limits(y=ylim)
        if(interactive) p<-ggiraph(code=print(p),hover_css="fill:orange;r:6px;cursor:pointer")
        p
}






