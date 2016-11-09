require(ggplot2)
require(ggiraph)

#' Make an interactive scatterplot
#'
#'@param x An R object to ggPoints
ggScatter<-function(x,...) UseMethod("ggScatter")


#'@describeIn ggScatter
#' Draw scatterplot
#'
#'@param data a data.frame
#'@param x A data.frame
#'@param xvar A character string of column name be used as a x-axis variable
#'@param yvar A character string of column name be used as a y-axis variable.
#'@param colorvar A character string of column name be used as a colour variable. Default value is NULL
#'@param se Logical. display confidence interval around linear regression? (TRUE by default)
#'@param addloess Logical. add loess line
#'@param loessse  Logical. display confidence interval around loess regression? (TRUE by default)
#'@param fullrange should the fit span the full range of the plot, or just the data
#'@param interactive A logical value. If TRUE, an interactive plot will be returned
#'
#'@examples
#'require(moonBook)
#'require(ggplot2)
#'require(ggiraph)
#'ggScatter(iris,yvar="Sepal.Width",xvar="Sepal.Length",se=FALSE,colorvar="Species",interactive=TRUE)
#'ggScatter(acs,yvar="weight",xvar="height",colorvar="Dx",se=FALSE,interactive=TRUE)
#'ggScatter(iris,yvar="Sepal.Width",xvar="Sepal.Length",colorvar="Species",interactive=TRUE)
#'ggScatter(radial,yvar="NTAV",xvar="age",colorvar="DM",interactive=TRUE)
ggScatter.default=function(x,yvar,xvar,colorvar=NULL,facet=FALSE,
                           se=TRUE,method="auto",fullrange=FALSE,
                           #addloess=FALSE,loessse=FALSE,
                           cut_number=3,tooltip=NULL,interactive=FALSE,...){
     # x=mtcars;xvar="wt";yvar="mpg";colorvar=NULL;se=TRUE;addloess=FALSE
     # loessse=TRUE;fullrange=FALSE;tooltip=NULL;interactive=FALSE
    df<-x
    #df=data[c(xvar,yvar,colorvar,tooltip)]

    df$id=1:nrow(df)
    if(is.null(tooltip)){
        df$tooltip=paste0(df$id,"<br>",xvar,":",df[[xvar]],"<br>",yvar,":",df[[yvar]])
    } else{
        df$tooltip=paste0(df[[tooltip]],"<br>",xvar,":",df[[xvar]],"<br>",yvar,":",df[[yvar]])
    }
    if(!is.null(colorvar)) df$tooltip=paste0(df$tooltip,"<br>",colorvar,":",df[[colorvar]])
    if(is.null(colorvar)){

        p<-ggplot(data=df,aes_string(x=xvar,y=yvar))
        p<-p+ geom_smooth(method=method,se=se,fullrange=fullrange)

        #if(addloess) p<-p+ geom_smooth(colour="red",se=loessse)
        if(method=="lm") {
            myformula=as.formula(paste0(yvar,"~",xvar))
            fit=lm(myformula,data=df)
            intercept=coef(fit)[1]
            slope=coef(fit)[2]
            xmin=min(df[[xvar]],na.rm=T)
            xmax=max(df[[xvar]],na.rm=T)
            ymin=xmin*slope+intercept
            ymax=xmax*slope+intercept
            x=c(xmin,xmax)
            y=c(ymin,ymax)
            tooltip=makeEquation(fit)[1]
            df3=data.frame(x,y,tooltip)
            df3$id=1:nrow(df3)
            #df
            #str(df3)
            p<-p+ geom_path_interactive(data=df3,aes_string(x="x",y="y",data_id="id",tooltip="tooltip"),color="blue",size=1)
        }
        #caption=df3$tooltip[1]
        #print(caption)
        #if(method=="lm") p<-p+labs(caption=caption)
        p<-p+ geom_point_interactive(aes(data_id="id",tooltip=tooltip),...)
        p

    } else {
        z=colorvar
        group=unique(df[[colorvar]])
        categorical=TRUE
        if(is.numeric(df[[colorvar]])&(length(group)<=6)){
            df[[colorvar]]<-factor(df[[colorvar]])
            group=unique(df[[colorvar]])
        } else if(is.numeric(df[[colorvar]])&(length(group)>6)){
            categorical=FALSE
            df$cut_group=cut_number(df[[colorvar]],n=cut_number)
            group=levels(df$cut_group)
        }
        p<-ggplot(data=df,aes_string(x=xvar,y=yvar,colour=colorvar))
        p<-p+ geom_smooth(method=method,se=se,fullrange=fullrange)



        # print(categorical)
        #     str(df3)
        #     str(df)

        #if(addloess) p<-p+ geom_smooth(se=loessse)
        if(method=="lm"){
            name<-intercept<-slope<-xmin<-xmax<-ymin<-ymax<-tooltip<-c()


            for(i in 1 :length(group)){
                if(categorical) subdf=df[df[[colorvar]]==group[i],]
                else subdf=df[df$cut_group==group[i],]
                myformula=as.formula(paste0(yvar,"~",xvar))
                fit=lm(myformula,data=subdf)
                # if(is.factor(df[[colorvar]])) name<-c(name,levels(df[[colorvar]])[i])
                # else name<-c(name,group[i])
                if(is.factor(df[[colorvar]])) name<-c(name,levels(group)[group[i]])
                else name<-c(name,group[i])
                intercept=c(intercept,coef(fit)[1])
                slope=c(slope,coef(fit)[2])
                if(fullrange){
                    xmin=c(xmin,min(df[[xvar]],na.rm=T))
                    xmax=c(xmax,max(df[[xvar]],na.rm=T))
                    ymin=c(ymin,min(df[[xvar]],na.rm=T)*coef(fit)[2]+coef(fit)[1])
                    ymax=c(ymax,max(df[[xvar]],na.rm=T)*coef(fit)[2]+coef(fit)[1])

                } else{
                    xmin=c(xmin,min(subdf[[xvar]],na.rm=T))
                    xmax=c(xmax,max(subdf[[xvar]],na.rm=T))
                    ymin=c(ymin,min(subdf[[xvar]],na.rm=T)*coef(fit)[2]+coef(fit)[1])
                    ymax=c(ymax,max(subdf[[xvar]],na.rm=T)*coef(fit)[2]+coef(fit)[1])

                }
                tooltip=c(tooltip,paste0("for ",colorvar,"=",group[i],"<br>",makeEquation(fit)[1]))
            }
            df2=data.frame(name,intercept,slope,xmin,xmax,ymin,ymax,tooltip)
            x=c(df2$xmin,df2$xmax)
            y=c(df2$ymin,df2$ymax)
            #color=rainbow(length(group))
            #str(df2)
            df3=data.frame(x,y,df2$name,tooltip)
            if(categorical) colnames(df3)[3]=z
            else colnames(df3)[3]="cut_group"
            df3$id=1:nrow(df3)
            if((!categorical) & (!facet)){
                myformula=as.formula(paste0(yvar,"~",xvar))
                fit=lm(myformula,data=df)
                intercept=coef(fit)[1]
                slope=coef(fit)[2]
                xmin=min(df[[xvar]],na.rm=T)
                xmax=max(df[[xvar]],na.rm=T)
                ymin=xmin*slope+intercept
                ymax=xmax*slope+intercept
                x=c(xmin,xmax)
                y=c(ymin,ymax)
                tooltip=makeEquation(fit)[1]
                df3=data.frame(x,y,tooltip)
                df3$id=1:nrow(df3)
                df
                #str(df3)
            }

            if(categorical){
                p<-p+ geom_path_interactive(data=df3,
                                            aes_string(x="x",y="y",colour=colorvar,data_id="id",tooltip="tooltip"),
                                            size=1)
            } else {

            p<-p+ geom_path_interactive(data=df3,
                                    aes_string(x="x",y="y",data_id="id",tooltip="tooltip"),color="blue",
                                    size=1)
            }
        }
        p<-p+geom_point_interactive(aes_string(data_id="id",tooltip="tooltip"),...)

        if(facet){
        if(!categorical) p<-p+facet_wrap(~cut_group)
        else p<-p+eval(parse(text=paste0("facet_wrap(~",colorvar,")")))
        }

        # caption=df3$tooltip[seq(1,nrow(df3),2)]
        # print(caption)
        # if(method=="lm") p<-p+labs(caption=caption)


        # formula=as.formula(paste0(yvar,"~",xvar,"*",colorvar))
        # p<-ggEffect(formula,data=x)
        # if(se) p<-p+ geom_smooth(method="lm",fullrange=TRUE)
        # if(addloess) p<-p+ geom_smooth(se=loessse)


    }
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


#'@describeIn ggScatter
#' Draw scatterplot
#'
#'@param x A formula of type y ~ x | B
#'@param data A data.frame
#'@param ... other arguments passed on to ggScatter.defalut
#'
#'@examples
#'require(moonBook)
#'ggScatter(weight~height|sex,data=radial,interactive=TRUE)
#'ggScatter(weight~height|sex,data=radial,fullrange=TRUE,se=FALSE,interactive=TRUE)
#'ggScatter(weight~height,data=radial,interactive=TRUE)
ggScatter.formula=function(x,data,...){
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
        temp=paste0("ggScatter(x=",substitute(data),",yvar='",y,"',xvar='",x,"',...)")
        #print(temp)

    } else if(length(a)==3) {
        #cat("\na=",a,"length(a)=",length(a))
        # cat("\nx=",a[2])
        x<-a[2]
        # cat("\ngroup=",a[3])
        group<-a[3]
        # cat("\ngroup=",group,"\n")
        temp=paste0("ggScatter(x=",substitute(data),",yvar='",y,"',xvar='",x,
                    "',colorvar='",group,"',...)")
        #print(temp)

    }
    eval(parse(text=temp))
}


