require(ggplot2)
require(plyr)
require(moonBook)
require(ggiraph)

#'Browser market share 2011
#'
#'A phony dataset measuring browser market share
#'
#'  @format A data.frame with 12 rows and 3 columns
#'  \describe{
#'     \item{browser}{browser}
#'     \item{version}{browser version}
#'     \item{share}{market share, in percentage}
#'}
"browsers"

#'Rose sales among 7 groups in a year
#'
#'A phony dataset representing rose sales
#'
#'  @format A data.frame with 84 rows and 3 columns
#'  \describe{
#'     \item{group}{group A to G}
#'     \item{Month}{Month 1 to 12}
#'     \item{value}{Rose sales amount}
#'}
"rose"

#'Draw a Pie and Donut plot
#'@param data A data.frame
#'@param pies A character string of column name be assigned to the Pies
#'@param donuts A character string of column name be assigned to the Donuts
#'@param count A character string of column name uses as count
#'@param addPieLabel A logical value. If TRUE, labels are added to the Pies
#'@param addDonutLabel A logical value. If TRUE, labels are added to the Donuts
#'@param showRatioDonut A logical value. If TRUE, Ratios are added to the DonutLabels
#'@param showRatioPie A logical value. If TRUE, Ratios are added to the PieLabels
#'@param showRatioPieAbove10 A logical value. If TRUE, labels are added to the Pies with ratio above 10.
#'@param title Plot title
#'@param labelposition A number indicating the label position
#'@param polar A logical value. If TRUE, coord_polar() function will be added
#'@param interactive A logical value. If TRUE, an interactive plot will be returned
#'
#'@return An interactive Pie and Donut plot
#'@examples
#'#ggPieDonut(mtcars,"am","cyl")
#'#p<-ggPieDonut(mtcars,"am","cyl")
#'#ggiraph(code=print(p))
#'#ggPieDonut(polar=FALSE)
#'#ggPieDonut(labelposition=0)
#'#ggPieDonut(interactive=TRUE)
#'#ggPieDonut(mtcars,"cyl","am")
#'#ggPieDonut(browsers,"browser","version","share", title="Browser market share 2011",interactive=TRUE)
ggPieDonut=function(data=acs,pies="Dx",donuts="smoking",count=NULL,
                    addPieLabel=TRUE,addDonutLabel=TRUE,
                    showRatioDonut=TRUE,showRatioPie=TRUE,
                    showRatioPieAbove10=TRUE,title="",
                    labelposition=1, polar=TRUE,interactive=FALSE){

        pies=as.character(substitute(pies))
        donuts=as.character(substitute(donuts))
        if(is.null(count)){
                dat1=ddply(data,c(pies,donuts),nrow)
                colnames(dat1)[3]="n"

                dat1$ymax=cumsum(dat1$n)
                dat1$ymin=cumsum(dat1$n)-dat1$n
                dat1$ypos=dat1$ymin+dat1$n/2
                dat1$ratio=dat1$ypos*100/sum(dat1$n)
                dat1$hjust=ifelse((dat1$ratio>25 & dat1$ratio<75),0,1)
                dat1$label=paste0(dat1[[pies]],'<br>',dat1[[donuts]],"<br>",dat1$n,"(",round(dat1$ratio,1),"%)")

                #print(dat1)

                data2=ddply(data,pies,nrow)
                colnames(data2)[2]="sum"
                #data2=data2[order(data2$sum,decreasing=TRUE),]
                data2$cumsum=cumsum(data2$sum)
                data2$pos=data2$cumsum-data2$sum/2
                data2$ymin=data2$cumsum-data2$sum
                data2$ratio=data2$sum*100/sum(data2$sum)
                data2$label=ifelse(data2$ratio>10,
                                   paste0(data2[[pies]],"<br>",data2$sum,"(",round(data2$ratio,1),"%)"),
                                   paste0(data2[[pies]]))
                data2$tooltip=paste0(data2[[pies]],"<br>",data2$sum,"(",round(data2$ratio,1),"%)")
                #print(data2)

        } else{
                dat1=data
                colnames(dat1)[colnames(dat1)==count]="n"
                dat1$ymax=cumsum(dat1$n)
                dat1$ymin=cumsum(dat1$n)-dat1$n
                dat1$ypos=dat1$ymin+dat1$n/2
                dat1$ratio=dat1$n*100/sum(dat1$n)
                dat1$cumratio=dat1$ypos*100/sum(dat1$n)
                dat1$hjust=ifelse((dat1$cumratio>25 & dat1$cumratio<75),0,1)
                dat1$label=paste0(dat1[[pies]],"<br>",dat1[[donuts]],"<br>",dat1$n,"(",round(dat1$ratio,1),"%)")


                #print(dat1)

                data2=ddply(dat1,pies,summarize,sum(n))
                colnames(data2)[2]="sum"
                data2=data2[order(data2$sum,decreasing=TRUE),]
                data2$cumsum=cumsum(data2$sum)
                data2$pos=data2$cumsum-data2$sum/2
                data2$ymin=data2$cumsum-data2$sum
                data2$ratio=data2$sum*100/sum(data2$sum)
                data2$label=ifelse(data2$ratio>10,
                                   paste0(data2[[pies]],"<br>",data2$sum,"(",round(data2$ratio,1),"%)"),
                                   paste0(data2[[pies]]))
                data2$tooltip=paste0(data2[[pies]],"<br>",data2$sum,"(",round(data2$ratio,1),"%)")
                #print(data2)


        }
        mainCol=rainbow(nrow(data2))
        subCol=subcolors(dat1,pies,mainCol)
        #subCol
        p<-ggplot(dat1) +
                geom_rect_interactive(aes_string( ymax="ymax", ymin="ymin", xmax="4", xmin="3",
                                                  tooltip="label",data_id=donuts),fill=subCol,colour="white")+

                geom_rect_interactive(aes_string(ymax="cumsum", ymin="ymin", xmax="3", xmin="0",
                                                 tooltip="tooltip",data_id=pies),data=data2,
                          fill=mainCol,colour="white",alpha=0.7) +
                theme_clean()

        if(addDonutLabel) {
                label2=dat1[[donuts]]
                if(showRatioDonut)
                        label2=paste0(label2,"\n(",round(dat1$ratio,1),"%)")
                if(polar){
                        if(labelposition==1) {
                                p<- p+ geom_text(aes(label=label2,x=4.3,y=ypos,hjust=hjust),size=3)+
                                        geom_segment(aes(x=4,xend=4.2,y=ypos,yend=ypos))
                        }  else{
                                p<- p+ geom_text(aes(label=label2,x=3.5,y=ypos),size=3)
                        }
                } else{
                        p<-p+ geom_text(aes(label=label2,x=3.5,y=ypos),size=3)

                }

        }
        if(addPieLabel) {
            Pielabel=data2[[pies]]
            if(showRatioPie) {
                if(showRatioPieAbove10) {
                    Pielabel=ifelse(data2$ratio>10,
                                       paste0(data2[[pies]],"\n(",round(data2$ratio,1),"%)"),
                                       paste0(data2[[pies]]))
                }
                else Pielabel=paste0(Pielabel,"\n(",round(data2$ratio,1),"%)")
            }
            p<-p+geom_text(data=data2,aes(label=Pielabel,x=1.5,y=pos),size=4)
        }
        if(polar) p<-p+coord_polar(theta="y",start=3*pi/2)
        if(title!="") p<-p+ggtitle(title)
        if(interactive) p<-ggiraph(code=print(p))
        p

}

#'Draw a Donut plot
#'@param data A data.frame
#'@param donuts A character string of column name be assigned to the Donuts
#'@param count A character string of column name uses as count
#'@param addDonutLabel A logical value. If TRUE, labels are added to the Donuts
#'@param showRatio A logical value. If TRUE, Ratios are added to the DonutLabels
#'@param polar A logical value. If TRUE, coord_polar() function will be added
#'@param labelposition A number indicating the label position
#'@param title Plot title
#'@param interactive A logical value. If TRUE, an interactive plot will be returned
#'
#'@return An interactive Pie and Donut plot
ggDonut=function(data=acs,donuts="Dx",count=NULL,
                 addDonutLabel=TRUE,showRatio=TRUE,
                 polar=TRUE,labelposition=1,title="",
                 interactive=FALSE){
        if(is.null(count)){
                dat1=ddply(data,donuts,nrow)
                colnames(dat1)[2]="n"
        } else{
                dat1=data
                colnames(dat1)[colnames(dat1)==count]="n"
        }
                dat1$ymax=cumsum(dat1$n)
                dat1$ymin=cumsum(dat1$n)-dat1$n
                dat1$ypos=dat1$ymin+dat1$n/2
                dat1$ratio=dat1$n*100/sum(dat1$n)
                dat1$cumratio=dat1$ypos*100/sum(dat1$n)
                dat1$hjust=ifelse((dat1$cumratio>25 & dat1$cumratio<75),0,1)
                dat1$label=paste0(dat1[[donuts]],"<br>",dat1$n,"(",round(dat1$ratio,1),"%)")

        mainCol=rainbow(nrow(dat1))
        p<-ggplot(dat1) +
                geom_rect_interactive(aes_string( ymax="ymax", ymin="ymin", xmax="4", xmin="3",
                                                  tooltip="label",data_id=donuts),fill=mainCol,colour="white",alpha=0.7)+
                coord_polar(theta="y",start=3*pi/2)+
                xlim(0,4+labelposition)+
                theme_clean()

        donutlabel=dat1[[donuts]]
        if(showRatio)
            donutlabel=paste0(donutlabel,"\n(",round(dat1$ratio,1),"%)")

        if(labelposition==1) {
                p<- p+ geom_text(aes(label=donutlabel,x=4.3,y=ypos,hjust=hjust),size=3)+
                                geom_segment(aes(x=4,xend=4.2,y=ypos,yend=ypos))
        }  else{
                p<- p+ geom_text(aes(label=donutlabel,x=3.5,y=ypos),size=3)
        }
        if(title!="") p<-p+ggtitle(title)
        if(interactive) p<-ggiraph(code=print(p))
        p

}

#'Make a subcolors according to the mainCol
#'
#'@param .dta A data.frame
#'@param main A character string of column name used as a main variable
#'@param mainCol A main color
subcolors <- function(.dta,main,mainCol){
        tmp_dta = cbind(.dta,1,'col')
        tmp1 = unique(.dta[[main]])
        for (i in 1:length(tmp1)){
                tmp_dta$"col"[.dta[[main]] == tmp1[i]] = mainCol[i]
        }
        u <- unlist(by(tmp_dta$"1",tmp_dta[[main]],cumsum))
        n <- dim(.dta)[1]
        subcol=rep(rgb(0,0,0),n);
        for(i in 1:n){
                t1 = col2rgb(tmp_dta$col[i])/256
                subcol[i]=rgb(t1[1],t1[2],t1[3],1/(1+u[i]))
        }
        return(subcol);
}


