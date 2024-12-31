#helper functions



########################### density plotting function

denschart3 <- function (x, labels = NULL, groups = NULL, gdata = NULL, cex = par("cex"), 
    pt.cex = cex, bg = par("bg"), 
    color = "grey20", colorHPDI ="grey60", HPDI=0.9, vline = NULL, polyborder=NA, polyborderHPDI=NA,
    gcolor = par("fg"), lcolor = "gray", xlim = range(unlist(x)), yvals = 1:length(x), yextra=0.7,
    main = NULL, xlab = NULL, ylab = NULL, height=0.7 , border=NA, adjust=1, ...) 
  {
    opar <- par("mai", "mar", "cex", "yaxs")
    on.exit(par(opar))
    par(cex = cex, yaxs = "i")
    if (!is.list(x)) 
        stop("'x' must be a list of vectors or matrices")
    n <- length(x)
    glabels <- NULL
    if (is.list(x)) {
        if (is.null(labels)) 
            labels <- names(x)
        if (is.null(labels)) 
            labels <- as.character(1L:n)
        labels <- rep_len(labels, n)
        #if (is.null(groups)) 
        #    groups <- col(x, as.factor = TRUE)
        #glabels <- levels(groups)
    }
    plot.new()
    linch <- if (!is.null(labels)) 
        max(strwidth(labels, "inch"), na.rm = TRUE)
    else 0
    if (is.null(glabels)) {
        ginch <- 0
        goffset <- 0
    }
    else {
        ginch <- max(strwidth(glabels, "inch"), na.rm = TRUE)
        goffset <- 0.4
    }
    if (!(is.null(labels) && is.null(glabels))) {
        nmai <- par("mai")
        nmai[2L] <- nmai[4L] + max(linch + goffset, ginch) + 
            0.1
        par(mai = nmai)
    }
    if (is.null(groups)) {
        o <- 1L:n
        y <- yvals #o                           #vertical spacing
        ylim <- c(0.2, max(y) + yextra) #n + 1)
    }
    else {
        # sub-groups, so need more rows
        o <- sort.list(as.numeric(groups), decreasing = TRUE)
        x <- x[o]
        groups <- groups[o]
        color <- rep_len(color, length(groups))[o]
        lcolor <- rep_len(lcolor, length(groups))[o]
        offset <- cumsum(c(0, diff(as.numeric(groups)) != 0))
        y <- 1L:n + 2 * offset
        ylim <- range(0, y + 2)
    }
    plot.window(xlim = xlim, ylim = ylim, log = "")
    lheight <- par("csi")
    if (!is.null(labels)) {
        linch <- max(strwidth(labels, "inch"), na.rm = TRUE)
        loffset <- (linch + 0.1)/lheight
        labs <- labels[o]
        #mtext(labs, side = 2, line = -1 #0.4, #loffset,           #### y-labels
        #      at = y, adj = 1, 
        #      col = "black", las = 2, cex = cex, ...)
        
        #text(labels=labs, x=-5.2, y=y, pos=2, adj=1)
    }

    #vertical lines
    if ( !is.null(vline) ) {
        for ( t in 1:length(vline) ) {
            lines(x=list( x=c(vline[t],vline[t]), y=c(0,max(y)+0.5) ), lty=2, lwd=0.75)
        } # for t
    } #if


    # draw densities at each y offset
    for ( i in 1:n ) {
        a <- density( x[[i]] , adjust=adjust )
        a$y <- a$y/max(a$y) * height + y[i] - 0.3
        polygon( a$x , a$y , col=color[i] , border=polyborder[i],lwd=2 )
        Cuts <- HPDI( x[[i]] , HPDI)
        XX <- a$x[which(a$x > Cuts[1] & a$x < Cuts[2])]
        YY <- a$y[which(a$x > Cuts[1] & a$x < Cuts[2])]
        #loXX <- a$x[which(a$x < Cuts[1] )]
        #loYY <- a$y[which(a$x < Cuts[1] )]
        #hiXX <- a$x[which(a$x > Cuts[2] )]
        #hiYY <- a$y[which(a$x > Cuts[2] )]
        polygon( c(min(XX), XX, max(XX)), c(min(a$y), YY, min(a$y)),
          col=colorHPDI[i], border=polyborderHPDI[i],lwd=2 )
        #polygon( c(min(loXX), loXX, max(loXX)), c(min(a$y), loYY, min(a$y)),
        #  col=color[i], border=NA )
        #polygon( c(min(hiXX), hiXX, max(hiXX)), c(min(a$y), hiYY, min(a$y)),
        #  col=color[i], border=NA )
    }

    if (!is.null(groups)) {
        gpos <- rev(cumsum(rev(tapply(groups, groups, length)) + 
            2) - 1)
        ginch <- max(strwidth(glabels, "inch"), na.rm = TRUE)
        goffset <- (max(linch + 0.2, ginch, na.rm = TRUE) + 0.1)/lheight
        mtext(glabels, side = 2, line = goffset, at = gpos, adj = 0, 
            col = gcolor, las = 2, cex = cex, ...)
        if (!is.null(gdata)) {
            abline(h = gpos, lty = "dotted")
            points(gdata, gpos, pch = gpch, col = gcolor, bg = bg, 
                cex = pt.cex/cex, ...)
        }
    }
    #axis(side=1, at=c(-6,-4,-2,0,2,4,6))
    #box()
    #title(main = main, xlab = xlab, ylab = ylab, ...)
    invisible()
}


