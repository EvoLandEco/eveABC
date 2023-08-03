as.paleoPhylo <- function (nm, pn, st, en, xx = NA, label = nm, grp=NA) 
{
  if(length(nm)!=length(unique(nm))) stop (paste("ID codes are not unique, specifically",nm[duplicated(nm)]))
  if(!is.na(pn[st==max(st)])) 
  {
    warning(paste("The oldest species' ancestor has been changed to 'NA' from", pn[st==max(st)], "."))
    pn[st==max(st)] <- NA
  }
  
  getX <- length(xx)==1 & is.na(xx[1])
  dat<- data.frame(nm,pn,st,en,xx,label,grp)
  dat <- dat[rev(order(dat$st, dat$en)),]
  pP <- list(nm = as.character(dat$nm), pn = as.character(dat$pn), 
             st = dat$st, en = dat$en,
             xx = dat$xx, label = as.character(dat$label), grp=dat$grp)
  class(pP) <- "paleoPhylo"
  
  #if I need to find the x locations
  if(getX) 
  {
    xxloc <- getXloc(pP)[, c(1, 6)]
    wxloc <- merge(dat, xxloc, by=c("nm"))	#with xlocs in
    dat <- wxloc
    dat <- with(wxloc, data.frame(nm, pn, st, en, xx=xx.y, label, grp))
    dat <- dat[rev(order(dat$st, dat$en)),]
    pP <- list(nm = as.character(dat$nm), pn = as.character(dat$pn), 
               st = dat$st, en = dat$en,
               xx = dat$xx, label = as.character(dat$label), grp=dat$grp)
    class(pP) <- "paleoPhylo"
  }
  
  return(pP)
}


getXloc <- function (pP) 
{
  if (class(pP) != "paleoPhylo") stop("object is not of class 'paleoPhylo'")
  {
    dat <- data.frame(nm = pP$nm, pn = pP$pn, st = pP$st, en = pP$en)
    #dat <- dat[rev(order(dat$st, dat$en)),]
    pos <- 0.5
    time <- minTime <- min(-pP$st)
    ids <- pP$nm[1]
    tmFrm <- round(-dat$st, 5)
    nn <- length(pP$nm)
    dex <- numeric(nn)
    nAncs <- vector("list", nn)
    for (i in 1:nn) nAncs[[i]] <- route2root(pP, pP$nm[i])$path
    for (j in 1:nn) dex[j] <- sum(sapply(1:nn, function(k) pP$nm[j] %in% nAncs[[k]]))
    
    while (time != max(tmFrm))
    {
      time <- tmFrm[(round(tmFrm, 8) > round(time, 8)) == TRUE][1]
      event <- dat[which(-round(dat$st, 5) == round(time, 5)), ]
      parents <- unique(as.character(event$pn))
      inIds <- sapply(1:length(parents), function(i) sum(intersect(ids, parents) == as.list(parents)[[i]]) > 0)
      parents <- c(parents[inIds == TRUE], parents[inIds == FALSE])
      for (i in 1:length(parents))
      {
        singleEvent <- event[as.character(event$pn) == parents[i], ]
        if (length(singleEvent[, 1]) > 1) evnt <- 1
        if (length(singleEvent[, 1]) == 1) evnt <- 0
        focInd <- as.character(singleEvent$nm)
        pntLoc <- which(ids == parents[i])
        if (time != minTime)
        {
          fcPrnt <- as.character(unique(singleEvent$pn))
          fcSist <- as.character(unique(singleEvent$nm))
          if (length(pntLoc) == 0 & length(pos) > 1) 
            stop(paste("There is a lack of congruence in the tree around", focInd, "\nHave a look at the ancestor", 
                       fcPrnt, "but also the sister species", fcSist[1], "and", fcSist[2], "\n"))
        }
        parentPos <- pos[pntLoc]
        ids <- c(ids, focInd)
        sortPos <- sort(pos)
        if (length(pos) > 1)
        {
          whr <- which(sortPos == parentPos)
          lowPos <- (sortPos[whr - 1] + sortPos[whr])/2
          hghPos <- (sortPos[whr + 1] + sortPos[whr])/2
          if (evnt == 0) 
          {
            pta <- dex[which(pP$nm == parents[i])]
            ida <- dex[which(pP$nm == focInd)]
            
            if (pta > ida & parentPos >= 0.5) newPos <- lowPos else newPos <- hghPos
            if (pta > ida & parentPos < 0.5)  newPos <- hghPos else newPos <- lowPos
            
            if (length(newPos) == 0 & parentPos == max(pos)) newPos <- extendrange(pos)[2]
            if (length(newPos) == 0 & parentPos == min(pos)) newPos <- extendrange(pos)[1]
          }
          if (evnt == 1)
          {
            if (parentPos == min(pos)) lowPos <- extendrange(pos)[1]
            if (parentPos == max(pos)) hghPos <- extendrange(pos)[2]
            newPos <- c(lowPos, hghPos)
            if (length(focInd) > 2) 
            {for (j in 3:length(focInd)) newPos <- c(newPos, (newPos[j - 2] + parentPos)/2)}
          }
          pos <- c(pos, newPos)
        }
        
        if (length(pos) == 1)  ifelse(evnt == 0, pos <- seq(0, 1, 1), pos <- c(0.5, 0, 1))
        
        lnsp <- length(sortPos)
        lnfi <- length(focInd)
        if (evnt == 1 & lnfi > 4 & lnsp == 1)  stop("Cannot yet handle polytomies of order > 4 immediately post root.")
        if (evnt == 1 & lnfi == 3 & lnsp == 1) pos <- c(0.5, 0, 1, 0.25)
        if (evnt == 1 & lnfi == 4 & lnsp == 1) pos <- c(0.5, 0, 1, 0.25, 0.75)
        if (length(pos) != length(ids)) 
          stop(paste("There is not a position for all individuals.\n The problem is something to with", focInd))
        pos <- as.numeric(as.factor(pos))/length(pos)
      }
    }
    locDat <- data.frame(ids, pos)
    locDat$xx <- pos #as.numeric(as.factor(locDat$pos))/length(locDat$pos)
    #print(length(locDat$xx))
    #print(length(unique(locDat$xx)))
    if (length(locDat$xx) != length(unique(locDat$xx))) stop("non-unique locations")
    cmbDat <- merge(dat, locDat, by.x = c("nm"), by.y = c("ids"))
  }
  return(cmbDat)
}


route2root <- function(pP, focLin) 
{
  if (class(pP) != "paleoPhylo") stop("pP is not of class 'paleoPhylo'")
  
  x <- which(pP$st==max(pP$st))
  rtDur <- pP$st[x]-pP$en[x]
  rt <- pP$nm[x]
  
  allAnc <- immAnc <- focLin
  tm2rt  <- pP$en[pP$nm==focLin]
  
  while(immAnc != rt)
  {
    i <- which(pP$nm == immAnc)
    tm2rt <- c(tm2rt, pP$st[i])
    immAnc <- pP$pn[i]
    allAnc <- c(allAnc,immAnc)
  }
  
  if(length(allAnc)!=length(tm2rt)) stop ("Number of lineages does not match number of durations.")
  tm2rt <- diff(c(tm2rt, pP$st[x]))
  out <- list(path=rev(allAnc), duration=rev(tm2rt), nNode=length(allAnc))
  return(out)
}


`createBifurcate` <- function (pP) 
{
  mergeSplit <- function (dt, spL) 	{dt <- dt[dt$cd != spL$cd[1],]  ;  dt <- dt[dt$cd != spL$cd[3],]  ;  dt <- rbind(dt,spL)}
  
  splitLineage <- function (prnt, off, nxtCd, ltCd)
  {
    ifelse (ltCd==1 ,  
            splName<-paste(substr(prnt$nm,1,nchar(as.character(prnt$nm))),LETTERS[ltCd],sep=""),  
            splName<-paste(substr(prnt$nm,1,nchar(as.character(prnt$nm))-1),LETTERS[ltCd],sep=""))
    sp1 <- data.frame(nm=as.character(prnt$nm),pn=as.character(prnt$pn),st=prnt$st,en=off$st,cd=prnt$cd,label=prnt$label)
    sp2 <- data.frame(nm=splName,pn=sp1$nm,st=off$st,en=prnt$en,cd=nxtCd,label=prnt$label)
    sp3 <- data.frame(nm=off$nm,pn=sp1$nm,st=off$st,en=off$en,cd=off$cd,label=off$label)
    spL <- data.frame(rbind(sp1,sp2,sp3))
    return(spL)
  }
  
  if(class(pP)!="paleoPhylo") stop("object is not of class 'paleoPhylo'")
  {
    dat <- with(pP, data.frame(nm=as.character(nm),pn=as.character(pn),st=st,en=en,cd=1:length(nm),label=label))
    Nms <- data.frame(nms=as.character(pP$nm),st=pP$st)  ;  nms <- as.character(Nms[rev(order(pP$st)),1])
    
    for (k in 1:length(unique(dat$nm)))
    {
      prnt <- spL <- dat[which(dat$nm == nms[k]),]
      off <- dat[c(which(as.character(prnt$nm)==as.character(dat$pn))),]
      off <- off[rev(order(off$st)),]
      
      if (length(off[,1]) != 0)
      {
        evnts <- unique(off$st)
        for (i in 1:length(evnts))
        {
          off1 <- off[off$st == evnts[i],]
          if (sum(off$st == evnts[i]) == 2) 
          {for (j in 1:length(off1$nm)) 
          {
            dat$pn <- as.character(dat$pn)
            dat$pn[which(as.character(dat$nm) == as.character(off1$nm[j]))] <- as.character(prnt$nm)}
          }
          if (sum(off$st == evnts[i]) == 1) 
          {
            off1$pn <- prnt$nm
            spL <- splitLineage(prnt,off1,max(dat$cd)+1,i)
            prnt <- spL[2,]
            dat <- mergeSplit (dat, spL)
          }
        }
      }
    }
    dat <- dat[rev(order(dat$st)),]
    if (sum(with(dat[dat$st < max(dat$st),],tapply(st,paste(st,pn),length))!=2) !=0) {cat("splitting of lineages to form bifurcating tree not exhaustive")}
    YY <- with(dat,as.paleoPhylo(nm,pn,st,en,label=label))
    return(YY)
  }	
}


`buildApe` <- function(pP, label=TRUE)
{	
  if(class(pP)!="paleoPhylo") stop("object is not of class 'paleoPhylo'")
  {
    get.line<-function (pn,dat,stNode,enNode=stNode)
    {
      tmp <- with(dat,data.frame(nm=nm,pn=pn,st=st,en=en,Speciation=Speciation,cd=cd, label=label))
      tmp <- tmp[!is.na(tmp$pn),]
      tmp <- tmp[as.character(tmp$pn)==pn,]
      
      if(length(tmp[,1])>0) {
        vv<-data.frame(IDcode=tmp$pn, IDname=rep(dat$label[dat$nm==tmp$pn[1]],2),descName=tmp$nm,descCode=tmp$cd,duration=tmp$st-tmp$en,
                       tip=0+(tmp$Speciation==0),extant= 0 + (tmp$en==min(dat$en)),startNode=rep(stNode,2),endNode=enNode+1:2,startTime=tmp$st,endTime=tmp$en)}
      if(length(tmp[,1])==1) {vv <- vv[1,]}
      vv<-vv[order(rev(vv$startTime)),]
      return(vv)
    }
    
    pP$Speciation <- as.numeric(sapply(1:length(pP$pn),function(i) sum(match(pP$pn,pP$nm[i],nomatch=0))>0))
    pP$cd <- 1:length(pP$nm)
    stop <- noEvent <- FALSE
    pn <- as.character(pP$nm[pP$st==max(pP$st)])
    clade <- get.line(pn,pP,1000000)
    
    while(stop==FALSE)
    {
      oldLength <- length(clade[,1])
      for(i in 1:length(clade[,1]))
      {
        if (sum(na.omit(as.character(clade$descName)[i])==as.character(clade$IDcode))==0 & clade$tip[i]==0)
        {
          parent <- clade$descName[i]
          nxt <- get.line(parent,pP,clade$endNode[i],max(clade$endNode))
          clade <- rbind(clade,nxt)
        }
      }
      if(oldLength==length(clade[,1])) {stop<-TRUE}
    }
    
    clade$endNode[clade$tip==1]<-clade$descCode[clade$tip==1]
    edge<-matrix(as.numeric(as.factor(c(clade$startNode,clade$endNode))),ncol=2)
    clade$edge2 <- edge[,2]
    
    clade$descLab <- NA
    for(i in 1:length(clade[,1])) {clade$descLab[i] <- (pP$nm[pP$nm==clade$descName[i]])}
    
    ifelse(label==TRUE,tL <- data.frame(tL=as.character(clade$descLab[clade$tip==1]),ed=clade$edge2[clade$tip==1]), 				tL <- data.frame(tL=as.character(clade$descName[clade$tip==1]),ed=clade$edge2[clade$tip==1]))
    tL <- as.character(tL[order(tL$ed),1])
    ifelse(label==TRUE, nL <- c(pn,as.character(clade$descLab[clade$tip==0])), nL <- c(pn,as.character(clade$descName[clade$tip==0])))
    
    clade$nodeLabel<-edge[,1]
    if (sum(with(clade,tapply(nodeLabel,nodeLabel,length))!=2) !=0)
    {warning("bifuracting walk through has failed to yield a completely bifurcating tree")}
    
    mft <- list(edge = edge, tip.label = tL, Nnode = length(unique(clade$IDcode)), 
                node.label = nL, edge.length=clade$duration)
    class(mft) <- "phylo"	
    mft <- reorder(mft)		  
    return(mft)
  }
}
