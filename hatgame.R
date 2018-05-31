##hatgame ?
#saralamba@gmail.com

#put each individual randomly in a square unit
genPop <- function(Npop, add.infected = TRUE, id = NULL){
  posx <- runif(Npop)
  posy <- runif(Npop)
  ids <- 1:Npop
  state <- rep(0,Npop)
  pop <- data.frame(ids=ids, posx=posx,posy=posy,state=state)
  if(add.infected==TRUE)
   pop <- addInfected(pop, id)

  return(pop)
}

#move each individual randomly within a radius
movePop <- function(pop, move.radius){
  if(move.radius < 0 || move.radius >= 1){
    stop("radius must be positive and less than 1.")
  }
  npop <- nrow(pop)
  randx <- runif(npop, min = -move.radius, max = move.radius)
  randy <- runif(npop, min = -move.radius, max = move.radius)
  
  newpop.posx <- pop$posx + randx
  newpop.posy <- pop$posy + randy
  
  newpop.posx <- ifelse(newpop.posx > 1, newpop.posx - 1, newpop.posx)
  newpop.posx <- ifelse(newpop.posx < 0, newpop.posx + 1, newpop.posx)
  
  newpop.posy <- ifelse(newpop.posy > 1, newpop.posy - 1, newpop.posy)
  newpop.posy <- ifelse(newpop.posy < 0, newpop.posy + 1, newpop.posy)
  
  return(data.frame(ids=pop$ids, posx=newpop.posx,posy=newpop.posy,state=pop$state))
}

#euclidean distance between two points
distance <- function(x1,y1,x2,y2){
  return(sqrt((x1-x2)^2+(y1-y2)^2))
}

#find the distances between id and the others
findDistance <- function(id, pop){
  id.posx <- pop[id,2]
  id.posy <- pop[id,3]
  
  pop.noid <- pop[pop$ids!=id,]
  distance.from.id <- distance(id.posx,id.posy,pop.noid$posx,pop.noid$posy)
  
  return(data.frame(ids=pop.noid$ids, distance.from.id=distance.from.id))
}

#find N nearest ids 
findNearest <- function(id, pop, n.nearest, radius){
  id.dist <- findDistance(id, pop)
  ordered <- id.dist[order(id.dist$distance.from.id),]
  within.radius <- ordered[ordered$distance.from.id <= radius,]
  nnearest.within.radius <- nrow(within.radius)
  
  if(nnearest.within.radius >= n.nearest){
    ids.nearest <- within.radius[1:n.nearest,]$ids
  }else{
    ids.nearest <- within.radius[1:nnearest.within.radius,]$ids
  }
  return(pop[ids.nearest,])
}

#find nearest Infected 
is.near.infected <- function(id, pop, n.nearest, inf.radius){
  nearest <- findNearest(id, pop, n.nearest, inf.radius)
  nearest.states <- nearest$state
  if(length(nearest.states[nearest.states==1]) >= 1){
    return(TRUE)
  }else{
    return(FALSE)
  }
}


## add infected individual into pop
addInfected <- function(pop,id=NULL){
  if(is.null(id)){
    pop[sample(1:nrow(pop),1),"state"] <- 1
  }else{
    pop[id,"state"] <- 1
  }
  return(pop)
}


# state changing
# S -> I if they are near I
#this will return the state of each individual for the next time step
# S = 0, I = 1, R = 2
becomeInfected <- function(pop, n.nearest, inf.radius){
  state <- pop$state
  state.canbe.changed <- state==0
  already.infected <- state==1
  near.infected.state <- c()
  for(i in pop$ids){ 
    near.infected.state <- c(near.infected.state, is.near.infected(i,pop,n.nearest,inf.radius))
  }
  nstate <- c()
  for(i in 1:length(state)){
    nstate[i] <- ifelse(near.infected.state[i]==TRUE && state.canbe.changed[i] == TRUE, 1, state[i])
    # change I -> R
    nstate[i] <- ifelse(nstate[i]==1 && already.infected[i] == TRUE, 2, nstate[i])
  }
  return(nstate)
}

## run the simulation
HatGame <- function(npop, n.nearest, inf.radius, move.radius, ntimes){
  
  #generate population
  pop <- genPop(Npop = npop, add.infected = TRUE)
  bigpop <- cbind(pop,time=0)
  
  for(t in 1:ntimes){
    #population movement
    pop <- movePop(pop,move.radius)
    pop$state <- becomeInfected(pop = pop, n.nearest = n.nearest , inf.radius = inf.radius)
    tmppop <- cbind(pop,time=t)
    bigpop <- rbind(bigpop,tmppop)
  }
  
  return(bigpop)
}

#return numbers of SIR at each time step
HatGame.summary <- function(bigpop){
  ntimes <- length(unique(bigpop$time))
  out <- data.frame(time=NULL, S=NULL, I=NULL, R=NULL)
  for(i in 0:(ntimes-1)){
    pop <- bigpop[bigpop$time==i,]
    tmp <- data.frame(time=i,S=sum(pop$state==0), I=sum(pop$state==1), R=sum(pop$state==2))
    out <- rbind(out,tmp)
  }
  
  return(out)
}

# plot HatGame.summary
SIR.plot <- function(bigpop){
  counts <- HatGame.summary(bigpop = bigpop)
  x.max <- max(counts$time)
  y.max <- max(counts$S)
  plot(x = counts$time, y = counts$S, type='l', xlim = c(0,x.max), ylim = c(0,y.max), col='green', xlab = "time", ylab = "numbers", lwd=2.5)
  lines(x=counts$time, y= counts$I,col='red',lwd=2.5)
  lines(x=counts$time, y= counts$R,col='blue',lwd=2.5)
  legend(x.max-2.5,y.max, c("S","I","R"), lty=c(1,1,1), lwd=c(2.5,2.5,2.4),col=c("green","red","blue"))
}


# plot the positions at time nth step
movePop.plot <- function(bigpop, n.th){
  pop <- bigpop[bigpop$time==n.th,]
  cols <- state.col(pop$state)
  p <- as.character(pop$ids)
  if(length(p) > 0){
    plot(pop$posy ~ pop$posx, data = pop, type='n', ylim=c(0,1), xlim=c(0,1), xlab='x', ylab='y')
    text(pop$posx, pop$posy, labels = p, col=cols)
  }else{
    NULL
  }
}

movePop.plot2 <- function(bigpop, n.th){
  pop <- bigpop[bigpop$time==n.th,]
  cols <- state.col(pop$state)
  p <- as.character(pop$ids)
  plot(pop$posx,pop$posy,pch=p,xlab='x',ylab='y',xlim = c(0,1),ylim=c(0,1), col=cols)
  
}

# colour for each state
state.col <- function(state){
  col <- ifelse(state==0,'green',ifelse(state==1,'red',ifelse(state==2,'blue','green')))
  return(col)
}

## gen movement plots
genMovement.plots <- function(bigpop){
  ntimes <- length(unique(bigpop$time))
  path <- tempdir()
  for(i in 0:(ntimes-1)){
    png(filename = file.path(path,paste0(as.character(i),".png")))
    movePop.plot(bigpop, i)
    dev.off()
  }
}

