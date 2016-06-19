#'Make an interactive bar plot with Errorbar
#'
#'@param x An R object to ggErrorBar
ggErrorBar<- function(x,...) UseMethod("ggErrorBar")

#'@describeIn ggCatepillar
#'Make an interactive barplot with errorbar
#'
#'@param x A formula of type y ~ x + A
#'@param data A data
ggErrorBar.formula=function(x,data,...){

    f = x

    x = as.character(f[[3]])
    x = unlist(strsplit(x, "+", fixed = TRUE))
    y = as.character(f[[2]])

    if(length(x) == 3) {
        ggErrorBar.default(data,yvar=y,xvar=x[2],group=x[3],...)
    } else{
        ggErrorBar.default(data,yvar=y,xvar=x,...)
    }
}

#'@describeIn ggErrorBar
#'Make an interactive bar plot with error bar
#'
#'@param x A data.frame
#'@param yvar A character string of "numeric" column name be used as a y-axis variable
#'@param xvar A character string of column name be used as a grouping variable. Default value os NULL
#'@param group A character string of column name be used as a x-axis variable
#'@param interactive A logical value. If TRUE, an interactive plot will be returned
#'@param digits An integer indicating the number of decimal places
#'@param mode if 2, two-sided error bar will be displayed, if 1 one-sided errorbar will be displayed
#'@return An interactive catepillar plot
#'
ggErrorBar.default=function(x,yvar,xvar,group=NULL,interactive=FALSE,digits=1,mode=2){
    df<-x
    A=yvar
    B=group
    C=xvar
    if(is.null(B)){
        dat=summarySE(df,A,C)
        dat$tooltip=""
        dat$label=paste0(dat[[C]],"<br>",round(dat[[A]],digits))
    } else if(B=="None") {
        dat=summarySE(df,A,C)
        dat$tooltip=""
        dat$label=paste0(dat[[C]],"<br>",round(dat[[A]],digits))
    } else {
        dat=summarySE(df,A,c(B,C))
        dat[[B]]=factor(dat[[B]])
        dat$tooltip=dat[[B]]
        dat$label=paste0(dat[[B]],"<br>",dat[[C]],"<br>mean:",round(dat[[A]],digits),
                         "<br>se:",round(dat$se,digits),"<br>sd:",round(dat$sd,digits))

    }
    if(length(C)>1){
        temp=Reduce(paste0,C)
        dat[[temp]]=Reduce(pastecolon,dat[C])
        C=temp
        dat[[C]]=factor(dat[[C]])
    }
    #dat

    #dat$tooltip=dat[[B]]
    #dat$label=paste0(dat[[B]],"<br>",dat[[C]],"<br>",round(dat[[A]],digits))
    dat$id=as.character(1:nrow(dat))

    # print(dat)
    # print(str(dat))


    #mywidth
    # if(is.null(B)) {
    #     p<-ggplot(data=dat,aes_string(x=C,y=A,group=1,colour=C))+xlab(Reduce(pastecolon,C))
    #
    # } else if(B=="None") {
    #     p<-ggplot(data=dat,aes_string(x=C,y=A,group=1,colour=C))+xlab(Reduce(pastecolon,C))
    #
    # } else p<-ggplot(data=dat,aes_string(x=C,y=A,group=B,colour=B))
    #
    # p<-p+ geom_path_interactive(aes(tooltip=tooltip,data_id=id),position=position_dodge(width=mywidth))+
    #     geom_point_interactive(aes(tooltip=label,data_id=id),size=4,position=position_dodge(width=mywidth))
    # p
    # p<-p+eval(parse(text=paste0("geom_errorbar(aes(ymin=",A,"-se,ymax=",
    #                             A,"+se),width=",mywidth,",
    #                             position=position_dodge(width=mywidth))")))
    # #p<-my_theme(p)
    # #p<-p+theme(legend.position="none")
    # if(interactive) p<-ggiraph(code=print(p),
    #                            hover_css="r:7px;cursor:pointer;stroke-width:6px;")
    # p
    #

    if(is.null(group)) {
        p<-ggplot(dat,aes_string(x=xvar,fill=xvar,y=yvar,tooltip="label",data_id="id"))+guides(fill=FALSE)
    } else {
        p<-ggplot(dat,aes_string(x=xvar,fill=group,y=yvar,tooltip="label",data_id="id"))
    }
    if(mode==2) p<-p+geom_bar_interactive(position="dodge",stat="identity")
    p<-p+eval(parse(text=paste0("geom_errorbar(aes(ymin=",A,"-se,ymax=",
                                 A,"+se),position=position_dodge(0.9),width=0.2)")))

    #p<-p+geom_errorbar(aes(ymin=age-se,ymax=age+se),position=position_dodge(0.9),width=0.2)+
    if(mode!=2) p<-p+geom_bar_interactive(position="dodge",stat="identity")

    if(interactive) p<-ggiraph(code=print(p))
    p
}
