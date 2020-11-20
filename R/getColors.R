getCol <- function(colname=NULL, alpha=255, scale=1){
  # scale=1;alpha=255;colname="KU.red"
  tmp = .cols(scale,alpha)
  names(tmp)
  if(is.null(colname)){
    # Display color names
    tmp100 = .cols(1,alpha)
    tmp75  = .cols(.75,alpha)[9:16]
    tmp50  = .cols(.5,alpha)[9:16]

    par(mfrow=c(1,1),mar=c(0,0,0,0), oma=c(0,0,0,0), bg="white")
    plot(rep(1,length(tmp100)),1:length(tmp100), pch=15, cex=3.5, col=rev(as.character(tmp100)), bty='n', axes=FALSE, xlim=c(1,10),ylim=c(0,(length(tmp100)+2)), ann=FALSE)
    points(rep(2,8),1:8, pch=15, cex=3.5, col=rev(as.character(tmp75)))
    points(rep(3,8),1:8, pch=15, cex=3.5, col=rev(as.character(tmp50)))
    text(3.5,1:length(tmp),1:16,labels = rev(names(tmp)), cex=0.75, pos=4)
    text(1:3,rep(0.5,3),1:3,labels = paste0(c(100,75,50),"%"), cex=0.75, pos=1)
  }else{
   return(tmp[[which(colname==names(tmp))]])
  }
}


.cols <- function(scale=1,alpha=255){
  cols = list(KU.red         = rgb(144,25,30, alpha, maxColorValue = 255),
              KU.gray        = rgb(102,102,102, alpha, maxColorValue = 255),
              KU.teo         = rgb(102,96,158, alpha, maxColorValue = 255),
              KU.jura        = rgb(153,150,50, alpha, maxColorValue = 255),
              KU.sund        = rgb(42,33,106, alpha, maxColorValue = 255),
              KU.samf        = rgb(225,55,24, alpha, maxColorValue = 255),
              KU.hum         = rgb(54,90,165, alpha, maxColorValue = 255),
              KU.sci         = rgb(70,116,60, alpha, maxColorValue = 255),
              KU.beech1      = rgb(119, 153, 33, alpha*scale, maxColorValue = 255),
              KU.beech2      = rgb(172, 202, 79, alpha*scale, maxColorValue = 255),
              KU.verdigris1  = rgb(177, 221, 223, alpha*scale, maxColorValue = 255),
              KU.verdigris2  = rgb(121, 173, 177, alpha*scale, maxColorValue = 255),
              KU.ocher1      = rgb(155, 74, 20, alpha*scale, maxColorValue = 255),
              KU.ocher2      = rgb(212, 159, 58, alpha*scale, maxColorValue = 255),
              KU.portblue1   = rgb(66, 117, 155, alpha*scale, maxColorValue = 255),
              KU.portblue2   = rgb(150, 190, 231, alpha*scale, maxColorValue = 255))
  return(cols)
}

