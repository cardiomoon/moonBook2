#' Draw a heatmap of correlation test
#'
#' @param df A data.frame
#' @param label if true, add label to the heatmap
#' @param colors colors for low, mid and high correlation values
#' @param title if true, add title to the heatmap
#' @param ... further arguments to be passed to cor.test
#'
#' @examples
#'# ggCor(iris)
#'# ggCor(mtcars,interactive=TRUE)
#'# ggCor(iris,method="pearson")
ggCor=function(df,label=FALSE,colors=NULL,title=FALSE,interactive=FALSE,...){
    result=mycor(df,...)

    if(is.null(colors)) colors=c("#6D9EC1","white","#E46726")
    cor_mat<-result$r
    p_mat<-result$p
    diag( cor_mat ) <- NA
    diag( p_mat ) <- NA
    var1 <- rep( row.names(cor_mat), ncol(cor_mat) )
    var2 <- rep( colnames(cor_mat), each = nrow(cor_mat) )
    cor <- as.numeric(cor_mat)
    cor_mat <- data.frame( var1 = var1, var2 = var2,
                           cor = cor, stringsAsFactors = FALSE )
    pval=as.numeric(p_mat)
    cor_mat$label=ifelse(is.na(cor_mat$cor),"",sprintf("%0.2f",cor_mat$cor))
    cor_mat$p=ifelse(is.na(pval),"",sprintf("%0.3f",pval))
    cor_mat[["tooltip"]] <-
        sprintf("<i>%s</i> vs <i>%s</i>:</br><i>r</i> = %s</br><i>p</i> = %s",
                var1, var2, cor_mat$label,cor_mat$p)

    # ggplot creation and ggiraph printing ----
    p <- ggplot(data = cor_mat, aes(x = var1, y = var2,tooltip=tooltip) ) +
        geom_tile_interactive(aes(fill = cor), colour = "white") +
        scale_fill_gradient2(low = colors[1], mid = colors[2], high = colors[3], limits = c(-1, 1)) +
        coord_equal()+
        xlab("")+ylab("")
    if(title) {
        title=paste0(result$out$method,",",result$out$alternative)
        p<-p+ggtitle(title)
    }
    if(label) p<-p+geom_text(aes(label=label))
    if(interactive) p<-ggiraph( code = print(p))
    p
}
