#'Korean administative area and code
#'
#'A dataset
#'
#'  @format A data.frame with 16 rows and 3 columns
#'  \describe{
#'     \item{code}{Korean administative area code}
#'     \item{name}{Korean administative area, level 1}
#'     \item{name2}{abbreviation of area}
#'}
"areacode"

#'Korean administative map data(2010) level 1
#'
#'A dataset
#'
#'  @format A data.frame with 6928 rows and 11 columns
#'  \describe{
#'     \item{id}{Korean administative area code}
#'     \item{long}{longitude}
#'     \item{lat}{latitude}
#'     \item{order}{An integer}
#'     \item{hole}{A logical}
#'     \item{piece}{Factor with 102 levels}
#'     \item{group}{Factor with 186 levels}
#'     \item{SP_ID}{shape id}
#'     \item{FID}{An integer}
#'     \item{code}{Area code}
#'     \item{region}{Region}
#'}
"kormap1"

#'Korean administative map data(2010) level 2
#'
#'A dataset
#'
#'  @format A data.frame with 16926 rows and 11 columns
#'  \describe{
#'     \item{id}{Korean administative area code}
#'     \item{long}{longitude}
#'     \item{lat}{latitude}
#'     \item{order}{An integer}
#'     \item{hole}{A logical}
#'     \item{piece}{Factor with 37 levels}
#'     \item{group}{Factor with 401 levels}
#'     \item{SP_ID}{shape id}
#'     \item{FID}{An integer}
#'     \item{code}{Area code}
#'     \item{region}{Region}
#'}
"kormap2"

#'Korean administative map data(2010) level 3
#'
#'A dataset
#'
#'  @format A data.frame with 70711 rows and 11 columns
#'  \describe{
#'     \item{id}{Korean administative area code}
#'     \item{long}{longitude}
#'     \item{lat}{latitude}
#'     \item{order}{An integer}
#'     \item{hole}{A logical}
#'     \item{piece}{Factor with 11 levels}
#'     \item{group}{Factor with 3593 levels}
#'     \item{SP_ID}{shape id}
#'     \item{FID}{An integer}
#'     \item{code}{Area code}
#'     \item{region}{Region}
#'}
"kormap3"


#' Select subdata of data
#'
#' @param map an object of data.frame or class Shape(SpatialPolygonsDataFrame)
#' @param area a string of area looking for
#'
#' @return Subdata of data.frame orclass Shape of which code matched with area
subdata <- function(data,area){
    code<-area2code(area)
    if(length(code)>0) {
        code1=paste0("^",code)
        temp=Reduce(paste_or,code1)
        mydata<-data[grep(temp,data$code),]
        mydata
    }
}

#' Returns whether x is integer(0)
#'
#' @param x a numeric vector
is.integer0 <- function(x) { is.integer(x) && length(x) == 0L}

#' Paste '|' between vectors
#' @param ... one or more R objects, to be converted to character vectors.
paste_or <- function(...) {
    paste(...,sep="|")
}

#' Seek area from data areacode and returns the code
#'
#' @param area a string looking for
#'
#' @return a code if the area is found, else returns NA
area2code <- function(area){
    result<-c()
    for(i in 1:length(area)){
        pos<-grep(area[i],areacode[[2]])
        if(!is.integer0(pos)) temp<-areacode[pos,1]
        else {
            pos<-grep(area[i],areacode[[3]])
            if(!is.integer0(pos)) temp<-areacode[pos,1]
        }
        result=c(result,temp)
    }
    result
}

#' Draw an interactive choropleth map
#' @param data a data.frame
#' @param map a map maybe a result of map_data()
#' @param fillvar a column name assigned to a fill variable
#' @param colors A vector of colours used as a parameter of scale_fill_gradientn()
#' @param map_id a column name used as an id
#' @param tooltip a column name included in a tooltip
#' @param facetvar a column name assigned to a facet variable
#' @param subarea a name of subarea
#' @param title A title
#' @param digits An integer indicating the number of decimal places
#' @param interactive Logical. If positive an interactive map will be made
#' @param ... other arguments passed on to geom_map_interactive
#'@examples
#'crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
#'require(ggplot2)
#'states_map <- map_data("state")
#'ggChoropleth(crimes,states_map,fill="Murder",map_id="state",tooltip="state",interactive=TRUE)
#'#ggChoropleth(data,kormap1,fillvar="총인구_명",tooltip="name",interactive=TRUE)
#'#ggChoropleth(data2,kormap2,fillvar="총인구_명",tooltip="name",interactive=TRUE)
#'#ggChoropleth(data3,kormap3,fillvar="총인구_명",tooltip="name",interactive=TRUE)
#'#ggChoropleth(data3,kormap3,fillvar="총인구_명",subarea=c("전라","광주"),interactive=TRUE)
ggChoropleth=function(data,map,fillvar="총인구_명",colors=c('white','orange','red'),
                      map_id="code",tooltip=NULL,facetvar=NULL,subarea=NULL,title="",digits=1,interactive=FALSE,...){

    if(!is.null(subarea)) {
        data=subdata(data,subarea)
        map=subdata(map,subarea)
    }
    data$data_id=data[[map_id]]
    if(is.null(tooltip)) {
        if(is.numeric(data[[fillvar]])) data[[fillvar]]=round(data[[fillvar]],digits)
        data$tooltip=paste0(data[[map_id]],"<br>",
                            fillvar,"<br>",data[[fillvar]])
    } else {

        if(is.numeric(data[[fillvar]])) data[[fillvar]]=round(data[[fillvar]],digits)
        data$tooltip=paste0(data[[tooltip]],"<br>",fillvar,"<br>",data[[fillvar]])
    }
    mycolors=colors
    tooltip_css <- "background-color:white;font-style:italic;padding:10px;border-radius:20px 20px 20px 20px;"


    p<-ggplot(data=data,aes_string(map_id=map_id,fill=fillvar,
                                   data_id="data_id",tooltip="tooltip"))+
        expand_limits(x=map$long,y=map$lat)+
        geom_map_interactive(map=map,colour='black',size=0.1,...)+
        coord_map()+
        scale_fill_gradientn(colours=mycolors)
    if(!is.null(facetvar)) p<-p+facet_wrap(facetvar)
    if(title!="") p<-p+ ggtitle(title)

    if(interactive) p<-ggiraph(code=print(p),tooltip_extra_css = tooltip_css,zoom_max=10)
    p
}

