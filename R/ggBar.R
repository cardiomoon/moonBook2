require(moonBook)
require(ggplot2)
require(ggiraph)


#'Draw an interactive barplot
#'
#'@param data A data.frame
#'@param xvar A character string of column name be assigned to the x-axis variable
#'@param fillvar A character string of column name be assigned to the fill variable
#'@param yvar A character string of column name be assigned to the y-axis variable. Uses only when stat="identity".
#'@param stat The statistical transformation to use on the data for this layer, as a string
#'            c("count","identity")
#'@param position Position adjustment. One of the c("fill","stack","dodge")
#'@param palette A character string indicating the color palette
#'@param width Bar width
#'@param digits integer indicating the number of decimal places
#'@param horizontal A logical value. If TRUE,a horizontal bar plot will be returned
#'@param yangle A integer. The value will be used adjust the angle of axis.text.y
#'@param addlabel A logical value. If TRUE, label will be added to the plot
#'@param polar A logical value. If TRUE, coord_polar() function will be added
#'@param interactive A logical value. If TRUE, an interactive plot will be returned
#'@param ... other arguments passed on to geom_bar_Interactive.
#'
#'@return An interactive barplot
#'
#'@examples
#'require(moonBook)
#'require(ggplot2)
#'require(ggiraph)
#'ggBar(acs,"Dx","smoking",interactive=TRUE,width=1,colour="white",size=0.2,polar=TRUE)
#'ggBar(acs,"Dx","smoking",position="fill",addlabel=TRUE,horizontal=TRUE,width=0.5)
#'ggBar(acs,"Dx","smoking",position="fill",interactive=TRUE,addlabel=TRUE)
#'ggBar(acs,"Dx","smoking",position="dodge",interactive=TRUE)
#'ggBar(rose,Month,group,"value",stat="identity",polar=TRUE,palette="Reds",width=1,
#'       color="black",size=0.1,interactive=TRUE)
ggBar=function(data,xvar,fillvar,yvar=NULL,stat="count",position="stack",palette=NULL,
               width=NULL,digits=1,horizontal=FALSE,yangle=0,
               addlabel=FALSE,polar=FALSE,interactive=FALSE,...){


    fillvar=as.character(substitute(fillvar))
    xvar=as.character(substitute(xvar))
    contmode=0
    # print(fillvar)
    # print(xvar)
    # print(yvar)
    # print(data)
    if(is.numeric(data[[xvar]])){
        if(is.null(width)) width=1
        result=num2cut(data[[xvar]])
        b=result$x1
        breaks=result$breaks
        a=table(data[[fillvar]],b)
        a
        df=reshape2::melt(a)
        df=df[c(2,1,3)]
        colnames(df)=c(xvar,fillvar,"nrow")
        df
        contmode=1

    } else if((stat=="identity") &(!is.null(yvar))){
        df=data[c(xvar,fillvar,yvar)]
        colnames(df)[3]="nrow"
        myformula=as.formula(paste(fillvar,"~",xvar))
        myformula
        b=reshape2::dcast(df,myformula,value=nrow)
        a=b[,-1]
        rownames(a)=b[[1]]
        #str(a)
        a=as.matrix(a)

    } else {
        df=ddply(data,c(xvar,fillvar),"nrow")
        df
        a=table(data[[fillvar]],data[[xvar]])
        a

    }
    if(is.null(width)) width=ifelse(contmode,1,0.9)
    barwidth=width

    df
    a
    if(is.null(width)) width=0.9
    df$xno=as.numeric(factor(df[[1]]))
    df$yno=as.numeric(factor(df[[2]]))

    total=sum(a)
    (csum=colSums(a))
    (csum/total)<0.05
    (rsum=rowSums(a))
    (xmax=cumsum(csum))
    (xmin=cumsum(csum)-csum)
    (x=(xmax+xmin)/2)
    (width=csum*width)
    (xmax=x+width/2)
    (xmin=x-width/2)
    df$xmin=df$xmax=df$x=df$csum=df$width=0
    for(i in 1:max(df$xno)){
        df[df$xno==i,]$csum=csum[i]
        df[df$xno==i,]$xmin=xmin[i]
        df[df$xno==i,]$xmax=xmax[i]
        df[df$xno==i,]$x=x[i]
        df[df$xno==i,]$width=width[i]
    }

    count=max(df$xno)

    if(position=="dodge"){
        df$ymax=df$nrow
        df$ymin=0
        df$y=(df$ymax+df$ymin)/2
        ycount=max(df$yno)
        df$xmin2=df$xmin+(df$yno-1)*(df$width/ycount)
        df$xmax2=df$xmin2+(df$width/ycount)
        df$xmin=df$xmin2
        df$xmax=df$xmax2
        df$x=(df$xmax+df$xmin)/2
        df2=df
    } else{
        for(i in 1:count){
            dfsub=df[df$xno==i,]
            dfsub$ratio=round(dfsub$nrow*100/csum[i],digits)
            dfsub$ymax=cumsum(dfsub$nrow)
            dfsub$ymin=dfsub$ymax-dfsub$nrow
            if(position=="fill"){
                dfsub$ymax=dfsub$ymax*100/csum[i]
                dfsub$ymin=dfsub$ymin*100/csum[i]
            }
            dfsub$y=(dfsub$ymin+dfsub$ymax)/2
            if(i==1) df2=dfsub
            else df2=rbind(df2,dfsub)
        }

    }

    df2$data_id=as.character(1:nrow(df2))
    df2$tooltip=paste0(df2[[xvar]],"<br>",df2[[fillvar]],"<br>",df2$nrow)
    df2$label=ifelse((df2$csum/total)>0.04,df2$nrow,"")
    if(position!="dodge") {
        df2$tooltip=paste0(df2$tooltip,"(",df2$ratio,"%)")
        df2$label=ifelse((df2$csum/total)>0.04,paste0(df2$ratio,"%"),"")
    }

    #print(df2)

    if(contmode) {
        xlabels=breaks[2:length(breaks)]
        xlabels
        xlabels[csum/total<0.04]=""
    } else xlabels=levels(factor(df[[1]]))

    ylabels=levels(factor(df[[2]]))
    if(contmode) {
        ycount=3
        (pos=1:ycount)
        y=(100/ycount)*(pos-1)+(100/ycount)/2
    } else y=df2[df2$xno==1,"y"]


    df2
    xvar
    fillvar
    p<-ggplot(mapping=aes_string(x=xvar,fill=fillvar,y="nrow"),data=df2)+
        # geom_bar(stat="identity")
        # geom_bar_interactive(stat="identity")
        geom_bar_interactive(aes(tooltip=tooltip,data_id=data_id),stat="identity",
                             position=position,width=barwidth,...)
    p


    # if(contmode) p<-p+scale_x_continuous(breaks=xmax,labels=xlabels,limits = c(0,total))
    # else p<-p+scale_x_continuous(breaks=x,labels=xlabels,limits = c(0,total))
    #
     # if(position!="dodge") {
     #     p<-p+ scale_y_continuous(breaks=y,labels=ylabels)+
     #         #scale_fill_discrete(guide=FALSE)+
     #         scale_fill_brewer(palette=palette,guide=FALSE)+
     #         ylab("")
     # } else p<-p+ylab("count")+scale_fill_brewer(palette=palette)

    if(addlabel) {
        if(position=="stack") {
            p=p+geom_text(aes(x=df2$xno,y=df2$y,label=df2$label))
        } else if(position=="fill") {
             p=p+geom_text(aes(x=df2$xno,y=(df2$y)/100,label=df2$label))
        } else {
            p=p+geom_text(aes(label=df2$label),position=position_dodge(0.9),vjust=1.5)
        }

    }


    # p<-p+theme_bw()+
    #     theme(axis.text.y=element_text(angle=90),axis.ticks.y=element_blank())
    #
    if(polar==TRUE) p<-p+ coord_polar()
    if(horizontal==TRUE) p<-p+ coord_flip()
    if(yangle!=0) p<-p+theme(axis.text.y=element_text(angle=90,hjust = 0.5))
    if(!is.null(palette)) p<-p+scale_fill_brewer(palette=palette)
    if(interactive)
        p<-ggiraph(code={print(p)})
    p


}

#'Draw an interactive Rose plot
#'
#'@param data A data.frame
#'@param palette A character string indicating the color palette
#'@param colour Bar colour
#'@param size Bar size
#'@return An interactive Rose plot
#'
#'@examples
#'require(moonBook)
#'require(ggplot2)
#'require(ggiraph)
#'ggRose(rose,Month,group,"value",interactive=TRUE)
#'ggRose(acs,"Dx","smoking",interactive=TRUE)
#'ggRose(rose,Month,group,"value",interactive=TRUE)
#'ggBar(rose,Month,group,"value",stat="identity",polar=TRUE,palette="Reds",width=1,
#'       color="black",size=0.1,interactive=TRUE)
ggRose=function(data,xvar,fillvar,yvar="None",...,palette="Reds",color="black",size=0.1){

    data=as.character(substitute(data))
    fillvar=as.character(substitute(fillvar))
    xvar=as.character(substitute(xvar))

    if(yvar=="None") {
    temp=paste0("ggBar(data=",data,",xvar=",xvar,",fillvar=",fillvar,",stat='identity',width=1,color='",color,
                "',size=",size,",palette='",palette,"',polar=TRUE,...)")
    } else{
        temp=paste0("ggBar(data=",data,",xvar=",xvar,",yvar='",yvar,"',fillvar=",fillvar,
                    ",stat='identity',width=1,color='",color,
                    "',size=",size,",palette='",palette,"',polar=TRUE,...)")
    }
    #print(temp)
    p<-eval(parse(text=temp))
    p

}


#' Make an interactive Heatmap
#'
#'@param data A data.frame
#'@param xvar A character string of column name be assigned to the x-axis variable
#'@param yvar A character string of column name be assigned to the y-axis variable. Uses only when stat="identity".
#'@param fillvar A character string of column name be assigned to the fill variable
#'@param facet A character string of column name be assigned to the facet_wrap variable
#'@param stat The statistical transformation to use on the data for this layer, as a string
#'            c("count","identity")
#'@param gradient_colors A voector of color names used in function scale_fill_gradientn().
#'       Default value is c("white","steelblue")
#'@param addlabel A logical value. If TRUE, label will be added to the plot
#'@param polar A logical value. If TRUE, coord_polar() function will be added
#'@param interactive A logical value. If TRUE, an interactive plot will be returned
#'@param yangle A integer. The value will be used adjust the angle of axis.text.y
#'@param color  Color argument passed on to geom_bar_interactive.
#'@param size Size argument passed on to geom_bar_interactive.
#'@param ... other arguments passed on to geom_bar_interactive.
#'
#'@return An interactive barplot
#'
#'@examples
#'require(moonBook)
#'require(ggplot2)
#'require(ggiraph)
#'ggHeatmap(acs,"Dx","smoking",addlabel=TRUE)
#'ggHeatmap(rose,"group","Month","value",stat="identity",gradient_colors = c("white","red"))
#'ggHeatmap(rose,"group","Month","value",stat="identity",addlabel=TRUE)
#'ggHeatmap(rose,"Month","group","value",stat="identity",polar=TRUE,interactive=TRUE)
#'ggHeatmap(taco,"AgeGroup","Filling","Rating",stat="identity")
#'ggHeatmap(taco,"AgeGroup","Filling","Rating","ShellType",stat="identity")
#'ggHeatmap(taco,"AgeGroup","Filling","Rating","ShellType",stat="identity",interactive=TRUE)
ggHeatmap=function(data,xvar,yvar,fillvar=NULL,facetvar=NULL,stat="count",gradient_colors=c("white","steelblue"),
                   addlabel=FALSE,polar=FALSE,interactive=FALSE,yangle=0,color="black",size=0.1,...){

    if(stat=="count") {
        df=plyr::ddply(data,c(xvar,yvar,facetvar),"nrow")
        fillvar="nrow"
    } else {
        df=data[c(xvar,yvar,fillvar,facetvar)]
    }

    width=1
    df$xno=as.numeric(factor(df[[1]]))
    df$yno=as.numeric(factor(df[[2]]))

    df$xmin=df$xno-width/2
    df$xmax=df$xno+width/2
    df$ymin=df$yno-width/2
    df$ymax=df$yno+width/2
    df$tooltip=paste0(df[[xvar]],"<br>",df[[yvar]],"<br>",df[[fillvar]])
    df$data_id=as.character(1:nrow(df))
    #print(str(df))
    # write.csv(df,"df.csv",row.names=FALSE)

    # df=read.csv("df.csv",stringsAsFactors = FALSE)
    # head(df)
    # gradient_colors=c("white","steelblue");fillvar="value";facetvar=NULL
    # addlabel=FALSE;polar=FALSE;interactive=FALSE;yangle=0;color="black";size=0.1

    xlabels=levels(factor(df[[1]]))
    ylabels=levels(factor(df[[2]]))

    xtotal=length(xlabels)
    x=1:xtotal
    ytotal=length(ylabels)
    y=1:ytotal


    p<-ggplot(df,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,data_id=data_id,tooltip=tooltip))+
        geom_rect_interactive(aes_string(fill=fillvar),color=color,size=size,...)+
        #geom_rect_interactive(aes_string(fill=fillvar),color="black",size=0.1);p
        xlab(xvar)+ylab(yvar)
    p<-p+scale_x_continuous(breaks=x,labels=xlabels,limits = c(0.5,xtotal+0.5))
    p<-p+scale_y_continuous(breaks=y,labels=ylabels,limits = c(0.5,ytotal+0.5))
    if(yangle!=0) p<-p+theme(axis.text.y=element_text(angle=90,hjust = 0.5))
    p<- p+scale_fill_gradientn(colours=gradient_colors)

    if(addlabel)
        p<-p+geom_text(aes_string(x="xno",y="yno",label=fillvar))+guides(fill=FALSE)
    if(polar) p<-p+coord_polar()
    if(!is.null(facetvar)) {
        formula=as.formula(paste0("~",facetvar))
        p<-p+facet_wrap(formula)
    }
    if(interactive) p<-ggiraph(code=print(p))
    p
}


