require(ggplot2)
require(ggiraph)
require(moonBook)


#'Paste character vactoers with separatoe colon
#'
#'@param ... Arguments passed on to paste()
pastecolon=function(...){
    paste(...,sep=":")
}

#'Make an interactive catepillar plot
#'
#'@param x An R object to ggCatepillar
ggCatepillar<- function(x,...) UseMethod("ggCatepillar")


#'@describeIn ggCatepillar
#'Make an interactive catepillar plot
#'
#'@param x A formula of type y ~ x + A
#'@param data A data
ggCatepillar.formula=function(x,data,...){
    f = x

    x = as.character(f[[3]])
    x = unlist(strsplit(x, "+", fixed = TRUE))
    y = as.character(f[[2]])


    if(length(x) == 3) {
        ggCatepillar.default(data,yvar=y,xvar=x[2],group=x[3],...)
    } else{
        ggCatepillar.default(data,yvar=y,xvar=x,...)
    }
}

#'@describeIn ggCatepillar
#'Make an interactive catepillar plot
#'
#'@param x A data.frame
#'@param yvar A character string of "numeric" column name be used as a y-axis variable
#'@param xvar A character string of column name be used as a grouping variable. Default value os NULL
#'@param group A character string of column name be used as a x-axis variable
#'@param interactive A logical value. If TRUE, an interactive plot will be returned
#'@param digits An integer indicating the number of decimal places
#'
#'@return An interactive catepillar plot
#'
ggCatepillar.default=function(x,yvar,xvar,group=NULL,interactive=FALSE,digits=1){
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
        dat$label=paste0(dat[[B]],"<br>",dat[[C]],"<br>",round(dat[[A]],digits))

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
    dat$id=1:nrow(dat)

    #print(dat)

    if(class(dat[[C]])%in% c("numeric","integer")) {
        mywidth=max(dat[[C]])/80
    } else mywidth=0.2
    #mywidth
    if(is.null(B)) {
        p<-ggplot(data=dat,aes_string(x=C,y=A,group=1,colour=C))+xlab(Reduce(pastecolon,C))

    } else if(B=="None") {
        p<-ggplot(data=dat,aes_string(x=C,y=A,group=1,colour=C))+xlab(Reduce(pastecolon,C))

    } else p<-ggplot(data=dat,aes_string(x=C,y=A,group=B,colour=B))

    p<-p+ geom_path_interactive(aes(tooltip=tooltip,data_id=id),position=position_dodge(width=mywidth))+
        geom_point_interactive(aes(tooltip=label,data_id=id),size=4,position=position_dodge(width=mywidth))
    p
    p<-p+eval(parse(text=paste0("geom_errorbar(aes(ymin=",A,"-se,ymax=",
                                A,"+se),width=",mywidth,",
                                position=position_dodge(width=mywidth))")))
    #p<-my_theme(p)
    #p<-p+theme(legend.position="none")
    if(interactive) p<-ggiraph(code=print(p),
                               hover_css="r:7px;cursor:pointer;stroke-width:6px;",
                               zoom_max=10)
    p
}

#'ggEffect generic
#'
#'Visualize the effect of interaction between two continuous independent variables on a response variable
#'
#'@param x Object to ggEffect
#'@param ... additional arguments passed to the generic function
ggEffect <- function(x,...) UseMethod("ggEffect")


#'@describeIn ggEffect Visualize the effect of interaction between two continuous independent variables on a response variable
#'
#'@param x A name of response variable
#'@param x1 A name of one of the independent variable
#'@param x2 A name of the other independent variable
#'
#'@return An interactive plot showing interaction
ggEffect.default <-function(x,y,x1,x2,...) {

    y=as.character(substitute(y))
    x1=as.character(substitute(x1))
    x2=as.character(substitute(x2))
    formula=as.formula(paste(y,"~",x1,"*",x2))
    ggEffect.formula(formula,x,...)
}


#'@describeIn ggEffect Visualize the effect of interaction between two continuous independent variables on a response variable
#'
#'@param x A formula
#'@param data A data
#'@examples
#'#data(mtcars)
#'#fit=lm(mpg~wt*hp,data=mtcars)
#'#ggEffect(fit,use.rownames=TRUE)
#'#ggEffect(fit,no=2)
#'#require(moonBook)
#'#fit2=lm(NTAV~age*smoking,data=radial)
#'#ggEffect(fit2)
#'#fit3=lm(age~sex*smoking,data=acs)
#'#ggEffect(fit3,interactive=TRUE)
ggEffect.formula <-function(x,data,...){

    # print(df)
    formula<-x
    df=data
    fit=lm(formula,data=data)
    if(length(names(fit$model))!=3) {
        print("two independent variables are allowed")
        return
    }
    ggEffect.lm(fit,...)

}

#'Visualize the effect of interaction between two continuous independent variables on a response variable
#'
#'@param x An integer(1 or 2) indicating which independent variable is used as x-axis variable
#'@param probs A vector of probability weights for obtaining the elements of the vector being sampled.Default value is c(0.10,0.5,0.90)
#'@param point A logical value. If TRUE, draw points
#'@param xvalue A numeric vector
#'@param digits An integer indicating the number of decimal places
#'@param use.rownames If TRUE, use rownames in label
#'@param interactive A logical value. If TRUE, an interactive plot will be returned
#'
#'@return An interactive plot showing interaction
#'@import ggplot2
#'@examples
#'data(mtcars)
#'fit=lm(mpg~wt*hp,data=mtcars)
#'require(ggplot2)
#'ggEffect(fit,use.rownames=TRUE)
#'ggEffect(fit,use.rownames=TRUE,interactive=TRUE)
#'ggEffect(fit,no=2)
#'require(moonBook)
#'fit2=lm(NTAV~age*smoking,data=radial)
#'ggEffect(fit2,interactive=TRUE)
#'fit3=lm(age~sex*smoking,data=acs)
#'ggEffect(fit3,interactive=TRUE)
ggEffect.lm<-function(x,
                  no=1,
                  probs=c(0.10,0.5,0.90),
                  point=TRUE,
                  xvalue=NULL,
                  digits=2,
                  use.rownames=FALSE,
                  interactive=FALSE)
    {
    fit<-x
    df=fit$model
    coef=fit$coef
    name=colnames(df)
    count=0
    if(is.numeric(df[[2]])) count=count+1
    if(is.numeric(df[[3]])) count=count+2
    if(count==0){
        p<-ggCatepillar(df,name[1],name[1+no],name[4-no])
    } else if(count<3){
        if(use.rownames) {
            df$label=rownames(df)
        } else {
            df$id=1:nrow(df)
            df$label=paste0(df$id,"<br>",name[4-count],"=",df[[name[4-count]]],"<br>",
                            name[1+no],"=",round(df[[name[1+no]]],digits),"<br>",
                             name[1],"=",round(df[[name[1]]],digits))
        }
        df$data_id=1:nrow(df)
        # str(df)
        # coef
        # summary(fit)
        xvar=name[1+count]
        color=name[4-count]
        names=levels(df[[color]])
        # df[[xvar]]
        xmin=min(df[[xvar]])
        xmin=rep(xmin,length(names))
        xmax=max(df[[xvar]])
        xmax=rep(xmax,length(names))
        length(names)
        intercept=coef[1]
        slope=coef[2]
        for(i in 2:length(names)){
            slope=c(slope,coef[2]+coef[2+length(names)+(i-2)])
            intercept=c(intercept,coef[1]+coef[3+(i-2)])
        }
        ymin=slope*xmin+intercept
        ymax=slope*xmax+intercept
        df1=data.frame(names,slope,intercept,xmin,ymin,xmax,ymax)
        # df1
        name2=rep(df1$names,2)
        x2=c(df1$xmin,df1$xmax)
        y2=c(df1$ymin,df1$ymax)
        slope2=rep(df1$slope,2)
        intercept2=rep(df1$intercept,2)
        df2=data.frame(name2,x2,y2,slope2,intercept2)
        colnames(df2)=c(color,"x","y","slope","intercept")
        df2$tooltip=paste0("for ",color,"=",df2[[color]],"<br>y=",round(df2$slope,digits),"*x +",round(df2$intercept,digits))
        df2$data_id=1:nrow(df2)
        # str(df)
        # str(df2)
        p<-ggplot(data=df,aes_string(x=name[1+count],y=name[1],colour=color))+
            #stat_smooth(method="lm",se=se,fullrange=TRUE)+
            geom_path_interactive(data=df2,
                                  aes_string(x="x",y="y",tooltip="tooltip",data_id="data_id"),size=1)
        if(point) p<-p+ geom_point_interactive(aes(tooltip=label,data_id=data_id))

        # p1<-ggplot(data=df,aes_string(x=name[1+count],y=name[1],colour=color))+
        #     stat_smooth(method="lm",se=se,fullrange=TRUE)+
        #   #  geom_path_interactive(data=df2,
        #    #                       aes_string(x="x",y="y",tooltip="tooltip",data_id="data_id"))+
        #     geom_point_interactive(aes(tooltip=label,data_id=data_id))
        # p1
    } else {
        (z=name[4-no])
        color=name[4-count]
        df$data_id=1:nrow(df)
        if(use.rownames){
            df$label=rownames(df)
        } else {
            df$label=paste0(df$data_id,"<br>",z,"=",df[[z]],"<br>",name[1+no],"=",df[[name[1+no]]],"<br>",name[1],"=",df[[name[1]]])
        }
        #str(df)
        if(is.null(xvalue)) {
            A=quantile(df[[4-no]],probs,na.rm=TRUE)
        } else A=xvalue
        count=length(A)
        labels=as.character(A)
        intercept=coef[1]+coef[4-no]*A
        slope=coef[1+no]+coef[4]*A
        xvar=df[[name[1+no]]]

        xmin=rep(min(xvar),count)
        xmax=rep(max(xvar),count)
        ymin=xmin*slope+intercept
        ymax=xmax*slope+intercept
        df1=data.frame(A,intercept,slope,xmin,xmax,ymin,ymax)
        # print(df1)
        name2=rep(df1$A,2)
        x2=c(df1$xmin,df1$xmax)
        y2=c(df1$ymin,df1$ymax)
        slope2=rep(df1$slope,2)
        intercept2=rep(df1$intercept,2)
        df2=data.frame(name2,x2,y2,slope2,intercept2)
        colnames(df2)=c(z,"x","y","slope","intercept")
        df2[[z]]=factor(df2[[z]])
        df2$tooltip=paste0("for ",z,"=",df2[[z]],"<br>y=",round(df2$slope,digits),"*x +",round(df2$intercept,digits))
        df2$data_id=1:nrow(df2)
        # print(df2)
        # str(df)
        str(df2)
        # name
        #df
        str(df)

        if(length(unique(df[[z]]))<6) {
            df[[z]]=factor(df[[z]])
            p<-ggplot(data=df,aes_string(x=name[1+no],y=name[1],tooltip="label",
                                         data_id="data_id",colour=z))
        } else {
            p<-ggplot(data=df,aes_string(x=name[1+no],y=name[1],tooltip="label",
                                         data_id="data_id"))
        }

        p<-p+ geom_path_interactive(data=df2,
                                  aes_string(x="x",y="y",tooltip="tooltip",data_id="data_id",color=z),size=1)

        if(point) p<-p + geom_point_interactive()

    }
    if(interactive){
        tooltip_css <- "background-color:white;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;"
        hover_css="fill-opacity=.3;cursor:pointer;stroke:gold;"
        if(interactive) p<-ggiraph(code=print(p),tooltip_extra_css=tooltip_css,tooltip_opacity=.75,
                                   zoom_max=10,hover_css=hover_css)
    }
    p
}

#'Make an interactive plot for an ANCOVA model
#'
#'@param x an object
#'@param ... additional arguments passed to the generic function
ggAncova=function(x,...) UseMethod("ggAncova")

#'@describeIn ggAncova Make an interactive plot for an ANCOVA model
#'@param yvar A character string of "continuous" column name be assigned to a response variable.
#'@param xvar A character string of "continuous" column name be assigned to a covariate.
#'@param A A character string of column name be assigned to a grouping variable.
ggAncova.default=function(x,yvar,xvar,A,...){
    data<-x
    yvar=as.character(substitute(yvar))
    xvar=as.character(substitute(xvar))
    A=as.character(substitute(A))
    formula=as.formula(paste(yvar,"~",xvar,"+",A))
    ggAncova.formula(formula,data,...)
}


#'@describeIn ggAncova Make an interactive plot for an ANCOVA model
#'
#'@param data a data.frame
ggAncova.formula=function(x,data,...){

    # print(df)
    formula <- x
    df=data
    fit=lm(formula,data=df)
    #summary(fit)
    if(length(names(fit$model))!=3) {
        print("only one independent variable and one covariate are allowed")
        return
    }
    (y=names(fit$model)[1])
    (x=names(fit$model)[2])
    (A=names(fit$model)[3])
    if((!is.numeric(df[[x]])) &(is.numeric(df[[A]]))){
        temp=A
        A=x
        x=temp
    } else if((is.numeric(df[[x]])) &(is.numeric(df[[A]]))){
        df[[A]]=factor(df[[A]])
        return(ggAncova.formula(formula,df,...))
    } else if((!is.numeric(df[[x]])) &(!is.numeric(df[[A]]))){
        print("only one independent variable and one covariate are allowed")
        return
    }
    ggAncova.lm(fit,...)

}


#'@describeIn ggAncova Make an interactive plot for an ANCOVA model
#'
#'@param x An object of class "lm"
#'@param label A character string of column name be assigned to the label
#'@param digits An integer indicating the number of decimal places
#'@param interactive A logical value. If TRUE, an interactive plot will be returned
ggAncova.lm=function(x,label=NULL,digits=1,interactive=FALSE){

    # print(df)
    fit<-x
    df=fit$model
    #summary(fit)
    if(length(names(fit$model))!=3) {
        print("only one independent variable and one covariate are allowed")
        return
    }
    (y=names(fit$model)[1])
    (x=names(fit$model)[2])
    (A=names(fit$model)[3])
    if((!is.numeric(df[[x]])) &(is.numeric(df[[A]]))){
        temp=A
        A=x
        x=temp
    } else if((is.numeric(df[[x]])) &(is.numeric(df[[A]]))){
        df[[A]]=factor(df[[A]])
        formula=as.formula(paste(y,"~",x,"+",A))
        fit=lm(formula,df)
        return(ggAncova.lm(fit,label=label,digits=digits,interactive=interactive))
    } else if((!is.numeric(df[[x]])) &(!is.numeric(df[[A]]))){
        print("only one independent variable and one covariate are allowed")
        return
    }
    df$all=rep("all",nrow(df))
    df$colour=factor(df[[A]])
    if(is.null(label)) {
        df$label=paste0(df[[A]],"<br>",x,"=",round(df[[x]],1),"<br>",y,"=",round(df[[y]],digits))
    } else df$label=df[[label]]
    df$data_id=1:nrow(df)
    coef=fit$coef
    slope=rep(coef[2],length(coef)-1)
    intercept=coef[1]
    for(i in 3:length(coef)) intercept=c(intercept,coef[1]+coef[i])
    name=levels(df[[A]])
    xmin=min(df[[x]])
    xmin=rep(xmin,length(coef)-1)
    xmax=max(df[[x]])
    xmax=rep(xmax,length(coef)-1)
    ymin=xmin*slope+intercept
    ymax=xmax*slope+intercept
    df1=data.frame(name,slope,intercept,xmin,ymin,xmax,ymax)
    colnames(df1)[1]=A
    df1$colour=df1[[A]]
    # print(df1)
    name2=rep(name,2)
    x2=c(df1$xmin,df1$xmax)
    y2=c(df1$ymin,df1$ymax)
    slope2=rep(df1$slope,2)
    intercept2=rep(df1$intercept,2)
    df2=data.frame(name2,x2,y2,slope2,intercept2)
    colnames(df2)=c(A,x,y,"slope","intercept")
    df2$color=df2[[A]]
    df2$tooltip=paste0(A,"=",df2[[A]],"<br>y=",round(df2$slope,1),"*x +",round(df2$intercept,1))
    df2$data_id=1:nrow(df2)
    # print(df2)

    p<-ggplot(data=df,aes_string(x=x,y=y,colour="colour",fill=A))+
        geom_point_interactive(aes_string(tooltip="label",data_id="data_id"))+
        facet_grid(as.formula(paste(".~",A)),margins=TRUE)+
        guides(colour=FALSE,fill=FALSE,linetype=FALSE)+
        #geom_abline(data=df1,aes_string(slope="slope",intercept="intercept",
        #                                colour="colour",linetype="colour"))
        geom_path_interactive(data=df2,aes(colour=color,tooltip=tooltip,data_id=data_id,linetype=color))
    if(interactive) p<-ggiraph(code=print(p),
                               hover_css="r:4px;cursor:pointer;stroke-width:6px;",
                               zoom_max=10)
    p

}


