


cex1 <- 2 #size adjustment for internal text

pdf(file="./combined_plots/Plots/cylinder.pdf", 
    height=4, width=8)
par(mfrow=c(1,3), oma=c(0,0,0,0), mar=c(0,0,0,0)) # margins and outer margins c(bottom, left, top, right)


#### original tube

plot(c(0, 100), c(0, 100), type = "n", xlab = "", ylab = "", axes=FALSE, asp=1) #empty plot


# outer bottom
draw.ellipse(
              x = 50, y = 26.5,
              a = 39, b = 29.25,    # a is horizontal radius, b is vertical radius
              lwd = 2
            ) 

# internal         
filledcylinder(
                  mid = c(50, 50),        # midpoint of cylinder
                  ry = 28,                # vertical radius of (laying down) cylinder: r = 28
                  rx = 21,                # horizontal radius of (laying down) cylinder: 0.75*ry
                  len = 47,               # cylinder length
                  angle = 90,             # rotation angle, degrees
                  col = "black",           # color of ex/interior
                  lcol = "black",         # line color on external surface
                  lwd = 2,                # only if lcol!=NA, width of external line
                  lcolint = "black",       # only if lcol!=NA, line color on internal (hidden) surface
                  ltyint = 1,             # only if lcol!=NA, line type on internal (hidden) surface.
                  lwdint = 2,             # only if dlcol!=NA, line width on internal (hidden) surface.
                  topcol = "black",       # color (palette) of top (right) surface.
                  botcol = "black"        # color (palette) of bottom (left) surface
               ) 

# external        
filledcylinder(
                  mid = c(50, 50),        # midpoint of cylinder
                  ry = 39,                # vertical radius of (laying down) cylinder: r + w, w = 11
                  rx = 29.25,             # horizontal radius of (laying down) cylinder: 0.75*ry
                  len = 47,               # cylinder length
                  angle = 90,             # rotation angle, degrees
                  col = rgb(255, 255, 255, max = 255, alpha = 200),           # color of ex/interior, alpha=0 is fully transparent
                  lcol = "black",         # line color on external surface
                  lwd = 2,                # only if lcol!=NA, width of external line
                  lcolint = "grey",       # only if lcol!=NA, line color on internal (hidden) surface
                  ltyint = 0,             # only if lcol!=NA, line type on internal (hidden) surface.
                  lwdint = 2,             # only if dlcol!=NA, line width on internal (hidden) surface.
                  topcol = rgb(255, 255, 255, max = 255, alpha = 200),       # color (palette) of top (right) surface.
                  #botcol = "white"        # color (palette) of bottom (left) surface
               ) 

# inner top
draw.ellipse(
              x = 50, y = 73.5,
              a = 28, b = 21,    # a is horizontal radius, b is vertical radius
              lwd = 2
            )

# outer top
draw.ellipse(
              x = 50, y = 73.5,
              a = 39, b = 29.25,    # a is horizontal radius, b is vertical radius
              lwd = 2
            ) 

lines(x=c(50,78), y=c(73.5,73.5), lwd=4, col="blue", lend="butt")
lines(x=c(78,89), y=c(73.5,73.5), lwd=4, col="red", lend="butt")

text( x=65, y=78, labels=expression(italic("r")), cex=cex1, font=1 )
text( x=82.5, y=78, labels=expression(italic("w")), cex=cex1, font=1 )
text( x=95, y=50, labels=expression(italic("h")), cex=cex1, font=1 )



#### inside-out tube

xshift <- 5 # shift tube right

plot(c(0, 100), c(0, 100), type = "n", xlab = "", ylab = "", axes=FALSE, asp=1) #empty plot


# outer bottom
draw.ellipse(
              x = 50 + xshift, y = 26.5,
              a = 28, b = 21,    # a is horizontal radius, b is vertical radius
              lwd = 2
            ) 

# internal         
filledcylinder(
                  mid = c(50 + xshift, 50), # midpoint of cylinder
                  ry = 6.86,              # vertical radius of (laying down) cylinder: r - i = 28 - 21.14 = 6.86
                  rx = 5.145,             # horizontal radius of (laying down) cylinder: 0.75*ry
                  len = 47,               # cylinder length
                  angle = 90,             # rotation angle, degrees
                  col = "black",          # color of ex/interior
                  lcol = "black",         # line color on external surface
                  lwd = 2,                # only if lcol!=NA, width of external line
                  lcolint = "black",      # only if lcol!=NA, line color on internal (hidden) surface
                  ltyint = 1,             # only if lcol!=NA, line type on internal (hidden) surface.
                  lwdint = 2,             # only if dlcol!=NA, line width on internal (hidden) surface.
                  topcol = "black",       # color (palette) of top (right) surface.
                  botcol = "black"        # color (palette) of bottom (left) surface
               ) 

# external        
filledcylinder(
                  mid = c(50 + xshift, 50), # midpoint of cylinder
                  ry = 28,                # vertical radius of (laying down) cylinder: r = 28
                  rx = 21,                # horizontal radius of (laying down) cylinder: 0.75*ry
                  len = 47,               # cylinder length
                  angle = 90,             # rotation angle, degrees
                  col = rgb(255, 255, 255, max = 255, alpha = 200),           # color of ex/interior, alpha=0 is fully transparent
                  lcol = "black",         # line color on external surface
                  lwd = 2,                # only if lcol!=NA, width of external line
                  lcolint = "grey",       # only if lcol!=NA, line color on internal (hidden) surface
                  ltyint = 0,             # only if lcol!=NA, line type on internal (hidden) surface.
                  lwdint = 2,             # only if dlcol!=NA, line width on internal (hidden) surface.
                  topcol = rgb(255, 255, 255, max = 255, alpha = 200),       # color (palette) of top (right) surface.
                  #botcol = "white"        # color (palette) of bottom (left) surface
               ) 

# inner top
draw.ellipse(
              x = 50 + xshift, y = 73.5,
              a = 6.86, b = 5.145,    # a is horizontal radius, b is vertical radius
              lwd = 2
            )

# outer top
draw.ellipse(
              x = 50 + xshift, y = 73.5,
              a = 28, b = 21,    # a is horizontal radius, b is vertical radius
              lwd = 2
            ) 

lines(x=c(50 + xshift,78 + xshift), y=c(73.5,73.5), lwd=4, col="blue", lend="butt")
lines(x=c(50-6.86 + xshift,50-28 + xshift), y=c(73.5,73.5), lwd=4, col="red", lend="butt")

text( x=65 + xshift, y=78, labels=expression(italic("r")), cex=cex1, font=1 )
text( x=35 + xshift, y=78, labels=expression(italic("i")), cex=cex1, font=1 )
text( x=84 + xshift, y=50, labels=expression(italic("h")), cex=cex1, font=1 )



#### cylinder

plot(c(0, 100), c(0, 100), type = "n", xlab = "", ylab = "", axes=FALSE, asp=1) #empty plot


# outer bottom
draw.ellipse(
              x = 50, y = 26.5,
              a = 28, b = 21,    # a is horizontal radius, b is vertical radius
              lwd = 2
            ) 

# external        
filledcylinder(
                  mid = c(50, 50),        # midpoint of cylinder
                  ry = 28,                # vertical radius of (laying down) cylinder: r = 28
                  rx = 21,                # horizontal radius of (laying down) cylinder: 0.75*ry
                  len = 47,               # cylinder length
                  angle = 90,             # rotation angle, degrees
                  col = rgb(255, 255, 255, max = 255, alpha = 200),           # color of ex/interior, alpha=0 is fully transparent
                  lcol = "black",         # line color on external surface
                  lwd = 2,                # only if lcol!=NA, width of external line
                  lcolint = "grey",       # only if lcol!=NA, line color on internal (hidden) surface
                  ltyint = 0,             # only if lcol!=NA, line type on internal (hidden) surface.
                  lwdint = 2,             # only if dlcol!=NA, line width on internal (hidden) surface.
                  topcol = rgb(255, 255, 255, max = 255, alpha = 200),       # color (palette) of top (right) surface.
                  #botcol = "white"        # color (palette) of bottom (left) surface
               ) 

# outer top
draw.ellipse(
              x = 50, y = 73.5,
              a = 28, b = 21,    # a is horizontal radius, b is vertical radius
              lwd = 2
            ) 

lines(x=c(50,78), y=c(73.5,73.5), lwd=4, col="blue", lend="butt")

text( x=65, y=78, labels=expression(italic("r")), cex=cex1, font=1 )
text( x=84, y=50, labels=expression(italic("h")), cex=cex1, font=1 )


mtext(expression(paste(bold('A.'),' Body as a tube')), side = 3, outer = TRUE, cex = 1.2, line = -3, adj=0.08) #side (1=bottom, 2=left, 3=top, 4=right)
mtext(expression(paste(bold('B.'),' Tube turned inside-out,')), side = 3, outer = TRUE, cex = 1.2, line = -3, adj=0.5)
mtext(expression(paste('therefore',italic(' i'),' > ',italic('w'))), side = 3, outer = TRUE, cex = 1.2, line = -5, adj=0.5)
mtext(expression(paste(bold('C.'),' Inside-out tube')), side = 3, outer = TRUE, cex = 1.2, line = -3, adj=0.9)
mtext(expression(paste('when ',italic('i'),' = ',italic('r'))), side = 3, outer = TRUE, cex = 1.2, line = -5, adj=0.87)


graphics.off()





