library(rjson)
library(ggplot2)
library(ggtext)
library(parallel)
library(tidyverse)
library(data.table)
library(tidyr)
library(gganimate)
library(av)
library(grid)

# Load Data ---------------------------------------------------------------
#Using parallel computation for quicker loading
cores <- detectCores() -1 
cluster <- makeCluster(cores)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Load data from game against Malmo
files <- list.files(paste0(getwd(),"\\2019\\Tracking Data"), 
                   pattern=".json", full.names=T)
json.file <- files[which(grepl("20191020.Hammarby-MalmöFF.1-events.json",files))]
malmo.events.1 <- fromJSON(file=json.file)
malmo.events.1 <- data.frame(do.call(rbind,malmo.events.1),stringsAsFactors = FALSE)
json.file <- files[which(grepl("20191020.Hammarby-MalmöFF.1-info_live.json",files))]
malmo.info_live.1 <- fromJSON(file=json.file)
malmo.info_live.1$home_team <- malmo.info_live.1$team_home_name
malmo.info_live.1$away_team <- malmo.info_live.1$team_away_name
json.file <- files[which(grepl("20191020.Hammarby-MalmöFF.1-stats.json",files))]
malmo.stats.1 <- fromJSON(file=json.file)
malmo.stats.1 <- data.frame(do.call(rbind,malmo.stats.1),stringsAsFactors = FALSE)
json.file <- files[which(grepl("20191020.Hammarby-MalmöFF.1-tracks.json",files))]
malmo.tracks.1 <- fromJSON(file=json.file)
malmo.tracks.1 <- data.frame(do.call(rbind,malmo.tracks.1),stringsAsFactors = FALSE)
json.file <- files[which(grepl("20191020.Hammarby-MalmöFF.2-events.json",files))]
malmo.events.2 <- fromJSON(file=json.file)
malmo.events.2 <- data.frame(do.call(rbind,malmo.events.2),stringsAsFactors = FALSE)
json.file <- files[which(grepl("20191020.Hammarby-MalmöFF.2-info_live.json",files))]
malmo.info_live.2 <- fromJSON(file=json.file)
malmo.info_live.2$home_team <- malmo.info_live.2$team_home_name
malmo.info_live.2$away_team <- malmo.info_live.2$team_away_name
json.file <- files[which(grepl("20191020.Hammarby-MalmöFF.2-stats.json",files))]
malmo.stats.2 <- fromJSON(file=json.file)
malmo.stats.2 <- data.frame(do.call(rbind,malmo.stats.2),stringsAsFactors = FALSE)
json.file <- files[which(grepl("20191020.Hammarby-MalmöFF.2-tracks.json",files))]
malmo.tracks.2 <- fromJSON(file=json.file)
malmo.tracks.2 <- data.frame(do.call(rbind,malmo.tracks.2),stringsAsFactors = FALSE)
malmo.events <- rbind(malmo.events.1, malmo.events.2)
malmo.info_live <- c(malmo.info_live.1, malmo.info_live.2)
malmo.stats <- rbind(malmo.stats.1, malmo.stats.2)
malmo.tracks <- rbind(malmo.tracks.1, malmo.tracks.2)
rm(malmo.events.1, malmo.events.2,
   malmo.stats.1, malmo.stats.2,
   malmo.info_live.1, malmo.info_live.2,
   malmo.tracks.1, malmo.tracks.2)

#Load basic pitch draw (function provided by Suds on FoT)
draw.pitch <- function(pitch.length = 120, pitch.width = 80) {
  ###Define the Pitch Visualization
  stadiumlength <- pitch.length
  stadiumwidth <- pitch.width
  linewidth <- .12
  goalboxlength <- 16.5
  goalboxpos <- (stadiumwidth-goalboxlength*2-7.32)/2
  smallboxlength <- 5.5
  smallboxpos <- goalboxpos+goalboxlength-smallboxlength
  crossbarl <- 7.32
  crossbarh <- 2.44
  goalpos <- goalboxpos+goalboxlength
  attdefzone <- 30
  channel <- 4
  channelsplit <- 25
  
  
  
  ##Define the circle function that we will be using to create the center cirlce as well as the 18 yard box circles
  circle_fun <- function(center=c(0,0), diameter=1, npoints=1000, start=0, end=2){
    tt <- seq(start*pi, end*pi, length.out=npoints)
    data.frame(
      x = center[1] + diameter / 2 * cos(tt),
      y = center[2] + diameter / 2 * sin(tt)
    )
  }
  
  
  ##everything we build on one side of the pitch should be mirrored on the other side of the pitch
  ##this assumes that our pitch diagram goes down -> up
  mirror <- function(x) stadiumlength-x
  
  
  ## To use ggplot2, we need to define all the points that will go into the shape of the field
  ## The four parameters are: x (width values), y (length values), group (for the two sides), desc (a description for ease of use and further customization)
  segment_coord <- function(x, y, group, desc){
    segment_df <- data.frame(x = x, y = y) 
    segment_df$group <- group
    segment_df$side <- 1
    group <- group + 1
    
    # The same thing for the opposite side
    segment_df2 <- data.frame(x = x, y = mirror(y))
    segment_df2$group <- group
    segment_df2$side <- 2
    group <<- group + 1
    
    # On reunit les donnees
    segment_df <- rbind(segment_df, segment_df2)
    segment_df$desc <- desc
    
    return(segment_df)
  }
  
  ####this function will rotate the pitch if you want to see the field horizontally 
  rotate_pitch <- function(pitch, theta=pi/2){
    pitch_r <- pitch
    pitch_r$x <- pitch_r$x / 180 * pi
    pitch_r$y <- pitch_r$y / 180 * pi
    matrix_r <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), ncol = 2)
    coords_r <- apply(pitch_r[,c("x","y")], 1, function(x) x %*% matrix_r)
    pitch_r$x <- coords_r[1,] ; pitch_r$y <- coords_r[2,]
    pitch_r$x <- pitch_r$x * 180 / pi
    pitch_r$y <- pitch_r$y * 180 / pi
    return(pitch_r)
  }
  
  
  
  
  ##Here we define the circles we need for our field. Our field lines have width of 12cm.
  #Center Circle -- at midpoint of the halfway line, with radius of 9.15m (10 yards)
  centercircle_outer <- circle_fun(center=c(stadiumwidth/2,stadiumlength/2),diameter = 9.15 * 2)
  centercircle_inner <- circle_fun(center=c(stadiumwidth/2,stadiumlength/2),diameter = (9.15-linewidth) * 2)
  
  #Penalty Arc -- arc from penalty spot (11 meters, 12 yards) with radius of 9.15 m (10 yards)
  penaltyarc_outer <- circle_fun(center=c(stadiumwidth/2,11),diameter = 9.15 * 2)
  penaltyarc_inner <- circle_fun(center=c(stadiumwidth/2,11),diameter = (9.15-linewidth) * 2)
  
  #corner arcs
  cornerarc_outer_r <- circle_fun(center = c(stadiumwidth,0),diameter = 1*2)
  cornerarc_inner_r <- circle_fun(center = c(stadiumwidth,0),diameter = (1-linewidth)*2)
  
  cornerarc_outer_l <- circle_fun(center = c(0,0),diameter = 1*2)
  cornerarc_inner_l <- circle_fun(center = c(0,0),diameter = (1-linewidth)*2)
  
  #penalty spot
  penaltyspot <- circle_fun(center=c(stadiumwidth/2,11),diameter=.5*2)
  
  
  ##Now, we generate our dataframe that contains the points for all of our field markings
  group <- 1
  
  pitch <- segment_coord(x=c(0-linewidth,0-linewidth,stadiumwidth+linewidth,stadiumwidth+linewidth)
                         ,y=c(0-linewidth,0,0,0-linewidth),group=group,desc = "goal line") 
  
  pitch <- rbind(pitch,segment_coord(x=c(0-linewidth,0-linewidth,0,0)
                                     ,y=c(0,stadiumlength/2-linewidth/2,stadiumlength/2-linewidth/2,0),group=group,desc = "left touch line"))
  
  pitch <- rbind(pitch,segment_coord(x=c(stadiumwidth,stadiumwidth,stadiumwidth+linewidth,stadiumwidth+linewidth)
                                     ,y=c(0,stadiumlength/2-linewidth/2,stadiumlength/2-linewidth/2,0),group=group,desc = "right touch line"))
  
  pitch <- rbind(pitch,segment_coord(x=c(goalboxpos-linewidth,goalboxpos-linewidth,goalboxpos,goalboxpos)
                                     ,y=c(0,goalboxlength-linewidth,goalboxlength-linewidth,0),group=group,desc = "left 18 yard box"))
  
  pitch <- rbind(pitch,segment_coord(x=c(stadiumwidth-goalboxpos-linewidth,stadiumwidth-goalboxpos-linewidth,stadiumwidth-goalboxpos,stadiumwidth-goalboxpos)
                                     ,y=c(0,goalboxlength-linewidth,goalboxlength-linewidth,0),group=group,desc = "right 18 yard box"))
  
  pitch <- rbind(pitch,segment_coord(x=c(goalboxpos-linewidth,goalboxpos-linewidth,stadiumwidth-goalboxpos,stadiumwidth-goalboxpos)
                                     ,y=c(goalboxlength-linewidth,goalboxlength,goalboxlength,goalboxlength-linewidth),group=group,desc = "18 yard box line"))
  
  pitch <- rbind(pitch, segment_coord(x=c(smallboxpos-linewidth,smallboxpos-linewidth,smallboxpos,smallboxpos)
                                      ,y=c(0,smallboxlength-linewidth,smallboxlength-linewidth,0),group=group,desc = "left 6 yard box"))
  
  pitch <- rbind(pitch, segment_coord(x=c(stadiumwidth-smallboxpos-linewidth,stadiumwidth-smallboxpos-linewidth,stadiumwidth-smallboxpos,stadiumwidth-smallboxpos)
                                      ,y=c(0,smallboxlength-linewidth,smallboxlength-linewidth,0),group=group,desc = "right 6 yard box"))
  
  pitch <- rbind(pitch, segment_coord(x=c(smallboxpos-linewidth,smallboxpos-linewidth,stadiumwidth-smallboxpos,stadiumwidth-smallboxpos)
                                      ,y=c(smallboxlength-linewidth,smallboxlength,smallboxlength,smallboxlength-linewidth),group=group,desc = "6 yard box line"))
  
  pitch <- rbind(pitch, segment_coord(x=c(goalpos-linewidth,goalpos-linewidth,goalpos,goalpos)
                                      ,y=c(0-crossbarh,0,0,0-crossbarh),group=group,desc = "left goal post"))
  
  pitch <- rbind(pitch, segment_coord(x=c(stadiumwidth-goalpos-linewidth,stadiumwidth-goalpos-linewidth,stadiumwidth-goalpos,stadiumwidth-goalpos)
                                      ,y=c(0-crossbarh,0,0,0-crossbarh),group=group,desc = "right goal post"))
  
  pitch <- rbind(pitch, segment_coord(x=c(goalpos-linewidth,goalpos-linewidth,stadiumwidth-goalpos,stadiumwidth-goalpos)
                                      ,y=c(0-crossbarh,0-crossbarh,0-crossbarh,0-crossbarh),group=group,desc = "crossbar"))
  
  pitch <- rbind(pitch, segment_coord(x=penaltyspot[,"x"]
                                      ,y=penaltyspot[,"y"],group=group,desc = "penalty spot"))
  
  pitch <- rbind(pitch, segment_coord(x=c(centercircle_outer[centercircle_outer$y<=stadiumlength/2,"x"],rev(centercircle_inner[centercircle_inner$y<=stadiumlength/2,"x"]))
                                      ,y=c(centercircle_outer[centercircle_outer$y<=stadiumlength/2,"y"],rev(centercircle_inner[centercircle_inner$y<=stadiumlength/2,"y"])),group=group,desc = "center circle"))
  
  
  pitch <- rbind(pitch, segment_coord(x=c(0-linewidth,0-linewidth,stadiumwidth+linewidth,stadiumwidth+linewidth)
                                      ,y=c(stadiumlength/2-linewidth/2,stadiumlength/2+linewidth/2,stadiumlength/2+linewidth/2,stadiumlength/2-linewidth/2),group=group,desc = "halfway line"))
  
  pitch <- rbind(pitch, segment_coord(x=c(penaltyarc_outer[penaltyarc_outer$y>=goalboxlength,"x"],rev(penaltyarc_inner[penaltyarc_inner$y>=goalboxlength,"x"]))
                                      ,y=c(penaltyarc_outer[penaltyarc_outer$y>=goalboxlength,"y"],rev(penaltyarc_inner[penaltyarc_inner$y>=goalboxlength,"y"])),group=group,desc = "penalty arc"))
  
  pitch <- rbind(pitch, segment_coord(x=c(cornerarc_outer_l[cornerarc_outer_l$x>=0 & cornerarc_outer_l$y>=0,"x"],rev(cornerarc_inner_l[cornerarc_inner_l$x>=0 & cornerarc_inner_l$y>=0,"x"]))
                                      ,y=c(cornerarc_outer_l[cornerarc_outer_l$x>=0 & cornerarc_outer_l$y>=0,"y"],rev(cornerarc_inner_l[cornerarc_inner_l$x>=0 & cornerarc_inner_l$y>=0,"y"])),group=group,desc = "left corner"))
  
  pitch <- rbind(pitch, segment_coord(x=c(cornerarc_outer_r[cornerarc_outer_r$x>=0&cornerarc_outer_r$x<=stadiumwidth & cornerarc_outer_r$y>=0,"x"],rev(cornerarc_inner_r[cornerarc_inner_r$x>=0&cornerarc_inner_r$x<=stadiumwidth& cornerarc_inner_r$y>=0,"x"]))
                                      ,y=c(cornerarc_outer_r[cornerarc_outer_r$x>=0&cornerarc_outer_r$x<=stadiumwidth & cornerarc_outer_r$y>=0,"y"],rev(cornerarc_inner_r[cornerarc_inner_r$x>=0&cornerarc_inner_r$x<=stadiumwidth & cornerarc_inner_r$y>=0,"y"])),group=group,desc = "right corner"))
  
  
  #######This section will add zones as per Benfica LAB's diagrams (note, it would be nice to create a zonal function so the zones can change)
  pitch <- rbind(pitch, segment_coord(x=c(0-linewidth,0-linewidth,stadiumwidth+linewidth,stadiumwidth+linewidth)
                                      ,y=c(attdefzone-linewidth/2,attdefzone+linewidth/2,attdefzone+linewidth/2,attdefzone-linewidth/2),group=group,desc = "attacking zone"))
  
  pitch <- rbind(pitch,segment_coord(x=c(goalboxpos+channel-linewidth,goalboxpos+channel-linewidth,goalboxpos+channel,goalboxpos+channel)
                                     ,y=c(0,stadiumlength/2-linewidth/2,stadiumlength/2-linewidth/2,0),group=group,desc = "left channel"))
  
  pitch <- rbind(pitch,segment_coord(x=c(stadiumwidth-goalboxpos-channel-linewidth,stadiumwidth-goalboxpos-channel-linewidth,stadiumwidth-goalboxpos-channel,stadiumwidth-goalboxpos-channel)
                                     ,y=c(0,stadiumlength/2-linewidth/2,stadiumlength/2-linewidth/2,0),group=group,desc = "right channel"))
  
  pitch <- pitch <- rbind(pitch,segment_coord(x=c(stadiumwidth/2-linewidth/2,stadiumwidth/2-linewidth/2,stadiumwidth/2+linewidth/2,stadiumwidth/2+linewidth/2)
                                              ,y=c(0,attdefzone,attdefzone,0),group=group,desc = "middle channel"))
  
  pitch <- rbind(pitch,segment_coord(x=seq(0,goalboxpos,length.out = 20)
                                     ,y=seq(channelsplit,goalboxlength,length.out = 20),group=group,desc = "center left channel"))
  
  pitch <- rbind(pitch,segment_coord(x=seq(stadiumwidth,stadiumwidth-goalboxpos,length.out = 20)
                                     ,y=seq(channelsplit,goalboxlength,length.out = 20),group=group,desc = "center right channel"))
  
  
  vert3 <- ggplot() + geom_polygon(data = pitch[pitch$group %in% seq(1:36),], aes(x = x, y = y, group = group), col = "#FFFFFFB3") +
    #geom_polygon(data = pitch[pitch$group %in% seq(from=37,to=48),], aes(x=x, y=y, group=group), col= "#FFFF00B3") + 
    geom_segment(aes(x=-5,y=100,xend=-5,yend=90),arrow=arrow(length = unit(0.03, "npc")))+
    coord_equal() +
    ylim(-20,125) +
    xlim(-20,88) +
    scale_x_continuous(breaks = NULL) +
    scale_y_continuous(breaks = NULL) +
    xlab("") + ylab("") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(), axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(), axis.title = element_blank(),
          panel.background = element_rect(fill = "#77BD77",
                                          colour = "#77BD77",
                                          size = 0.5, linetype = "solid")
    )
  
  
  
  #Rotated Pitch
  horipitch <- rotate_pitch(pitch, theta = pi/2)
  horipitch2 <- horipitch
  horipitch2$y <- horipitch2$y * -1
  
  return(horipitch2)
}

pl <- malmo.info_live[["calibration"]]$pitch_size[1] #pitch length
pw <- malmo.info_live[["calibration"]]$pitch_size[2] #pitch width
field.data <- draw.pitch(pitch.length = pl, pitch.width = pw)
field.graph <- ggplot()+ geom_polygon(data = field.data[field.data$group %in% seq(1:36),], aes(x = x, y = y, group = group), col = "#FFFFFFB3") +
coord_equal() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + ylab("") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(), axis.title = element_blank(),
        panel.background = element_rect(fill = "#77BD77",
                                        colour = "#77BD77"),
        plot.background = element_rect(fill = "black")
  )

stopCluster(cluster)

# Data Adjustments and Functions ------------------------------------------

#Get information about players: name and jersey
home_players <- data.frame(matrix(ncol=3,nrow=0))
colnames(home_players)<-c("Jersey","Name","TeamID")
track.id <- 1
for (player in malmo.info_live[['team_home_players']]){
  home_players[track.id,"Jersey"] <- player["jersey_number"]
  home_players[track.id,"Name"] <- player["name"]
  track.id <- track.id + 1
}
away_players <- data.frame(matrix(ncol=3,nrow=0))
colnames(away_players)<-c("Jersey","Name","TeamID")
track.id <- 1
for (player in malmo.info_live[['team_away_players']]){
  away_players[track.id,"Jersey"] <- player["jersey_number"]
  away_players[track.id,"Name"] <- player["name"]
  track.id <- track.id + 1
}

#Spare data from home team and away team
home.team <- malmo.tracks %>% select(home_team)
away.team <- malmo.tracks %>% select(away_team)

frame.start<-20800 #first frame
frames.to.end<-375 #range of frames to build video
dt<-0.04 #time in seconds between each frame

#Build a dataframe with information of each player on each frame
find.positions <- function(team, frameID, frames.forward = 0, get.distance.from = 'a'){
  positions<-data.frame()
  for (f in 0:frames.forward){
    team.frame <- team[[1]][[frameID+f]]
    next.frame <- team[[1]][[frameID+1+f]]
    opp.frame <- if (get.distance.from == 'a') away.team[[1]][[frameID+f]] else home.team[[1]][[frameID+f]]
    #there can be more than one player missing from one frame to the next
    #this code ignores it, but it can be upgraded con adjust.frames()
    if (abs(length(team.frame) - length(next.frame))>1){
      next
    }
    if (length(team.frame) != length(next.frame)){
      adjusted.frames <- adjust.frames(team.frame,next.frame)
      team.frame <- adjusted.frames[[1]]
      next.frame <- adjusted.frames[[2]]
    }
    #ordering data frames to make comparision by index
    team.frame<-team.frame[order(sapply(team.frame, `[[`, i=1))]
    next.frame<-next.frame[order(sapply(next.frame, `[[`, i=1))]
    nr <- nrow(positions)
    for (i in  1:length(team.frame)){
      positions[nr+i,1]<-team.frame[[i]]$position[1]+pl/2
      positions[nr+i,2]<-team.frame[[i]]$position[2]+pw/2
      dx<-(next.frame[[i]]$position[1]+pl/2) - (team.frame[[i]]$position[1]+pl/2)
      dy<-(next.frame[[i]]$position[2]+pw/2) - (team.frame[[i]]$position[2]+pw/2)
      positions[nr+i,3]<-positions[i,1]+dx/dt
      positions[nr+i,4]<-positions[i,2]+dy/dt
      positions[nr+i,5]<-frameID+f
      positions[nr+i,6]<-team.frame[[i]]$jersey_number
      positions[nr+i,7]<-team.frame[[i]]$speed
      positions[nr+i,8]<-player.acceleration(team.frame[[i]]$speed, 
                                             next.frame[[i]]$speed,dt)
      positions[nr+i,9]<-calc.distance.to.goal(positions[nr+i,1],
                                               positions[nr+i,2])
      positions[nr+i,10]<-nearest.player(team.frame, team.frame, i,FALSE)
      positions[nr+i,11]<-nearest.player(team.frame, opp.frame, i,TRUE) 
    }
  }
  names(positions)<-c("X","Y","dX","dY","Frame",
                      "Jersey","Speed","Acceleration","DistanceToGoal",
                      "NearestTeammate", "NearestOpponent")
  if (get.distance.from == 'a'){
    positions <- merge(positions,home_players,by='Jersey')
  }
  else {
    positions <- merge(positions,away_players,by='Jersey')
  }
  return(positions)
}

#Method to replicate a player position if it is missing in next or previous frame
adjust.frames <- function(frame1,frame2){
  frame.size <- max(length(frame1),length(frame2))
  jerseys1 <- map(frame1,"jersey_number")
  jerseys2 <- map(frame2,"jersey_number")
  missing.jersey <- setdiff(jerseys2,jerseys1)
  if (length(missing.jersey) > 0){
    player <- frame2[[which(sapply(frame2, "[[", "jersey_number") == missing.jersey)]]
    frame1[[frame.size]] <- player
  }
  missing.jersey <- setdiff(jerseys1,jerseys2)
  if (length(missing.jersey) > 0){
    player <- frame1[[which(sapply(frame1, "[[", "jersey_number") == missing.jersey)]]
    frame2[[frame.size]] <- player
  }
  
  return(list(frame1,frame2))
}

#When the ball information is NULL, change it to (-99,-99,-99)
no.ball<-c(-99,-99,-99)
ball.position <- malmo.tracks %>% select(ball)
ball.position<-purrr::map(ball.position[[1]],1)
for (i in 1:length(ball.position)){
  if (is.null(ball.position[[i]])){
    ball.position[[i]] <- no.ball
  }
}
ball.position.df <- data.frame(do.call(rbind,ball.position), stringsAsFactors = FALSE)
names(ball.position.df)<-c("X","Y","Z")

#calculte player acceleration, but limiting to 8m/s² to fit into the plot
player.acceleration <- function(v1, v2, dt){
  return(min((v1-v2)/dt,8))
}

calc.distance <- function(x1,y1,x2,y2){
  return(dist(rbind(c(x1,y1),c(x2,y2))))  
}

calc.distance.to.goal<-function(x,y){
  goal.reference <- c(0,pw/2) #coordinates of center point of goal
  return(calc.distance(x,y,goal.reference[1],goal.reference[2]))
}

#Find nearest player of *ref.team from *playerID of *team
#*opp indicates if *ref.team is the opponent or not
nearest.player <- function(team,ref.team,playerID,opp){
  nearest<-999
  for (i in 1:length(ref.team)){
    if (opp || team[[i]]$jersey != team[[playerID]]$jersey){
      d<-calc.distance(team[[playerID]]$position[1],
                       team[[playerID]]$position[2],
                       ref.team[[i]]$position[1],
                       ref.team[[i]]$position[2])
      nearest <- min(d,nearest)
    }
  }
  return(nearest)
}


# 1- Find frames with positions, speed and directions of all 22 pl --------

#Build objects with home, away and ball positions
frame.start<-20800
frames.to.end<-0
home.team.positions <- find.positions(home.team,frame.start,frames.to.end)
away.team.positions <- find.positions(away.team,frame.start,frames.to.end)
bp.df <- ball.position.df[c(frame.start:(frame.start+frames.to.end)),]
bp.df["X"] <- apply(bp.df["X"], 1, function(x) ifelse(x != -99, x+pl/2, -1)) 
bp.df["Y"] <- apply(bp.df["Y"], 1, function(x) ifelse(x != -99, x+pw/2, -1))
bp.df["Frame"] <- c(frame.start:(frame.start+frames.to.end))

#Plot informations
g<- field.graph + 
  geom_point(data = home.team.positions, aes(x=X,y=Y), color="forestgreen")+
  geom_point(data = away.team.positions, aes(x=X,y=Y), color="blue")+ 
  geom_segment(data = home.team.positions,
               aes(x = X, y = Y, xend = dX, yend = dY),
               arrow = arrow(length = unit(0.1, "cm")))+ 
  geom_segment(data = away.team.positions,
               aes(x = X, y = Y, xend = dX, yend = dY),
               arrow = arrow(length = unit(0.1, "cm")))+
  geom_text(data = home.team.positions, aes(x=X+2, y=Y, label=Jersey), size=3) +
  geom_point(data = bp.df, aes(x=X,y=Y))

print(g)

#uncomment code below to make videos, if there are enough frames selected
# g<- g+ 
#   transition_states(Frame)
# 
# animate(
#   g + exit_fly(y_loc = 1),
#   renderer = av_renderer()
# )


# 2- Graphs for evidence limitations on data ------------------------------

frames.span <- 375 #equivalent do 15 seconds

home.team.positions <- find.positions(home.team,frame.start,frames.span)
home.team.positions["Goal"] <- apply(home.team.positions["Jersey"], 1, 
                                     function(x) ifelse(x==20,1,0))

g2.1<-ggplot(data = home.team.positions, aes(x=Frame,y=Speed, fill=as.factor(Goal))) +
  geom_col() + theme(axis.text.x=element_blank(), legend.position = "none" ) +
  geom_vline(aes(xintercept = 21077)) +
  facet_wrap(~ Name) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  scale_fill_manual(values= c("gray","red")) +
  ggtitle(paste0("Players speed through 15 seconds, highliting ",
                 "<span style = 'color:red;'>scorer</span>"))+
  theme(plot.title = element_markdown())
g2.2<-ggplot(data = home.team.positions, aes(x=Frame,y=Acceleration, fill=as.factor(Goal))) +
  geom_col() + ylim(c(-8,8)) + theme(axis.text.x=element_blank(), legend.position = "none" ) +
  geom_vline(aes(xintercept = 21077)) +
  facet_wrap(~ Name) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  scale_fill_manual(values= c("gray","red")) +
  ggtitle(paste0("Players acceleration through 15 seconds, highliting ",
                 "<span style = 'color:red;'>scorer</span>"))+
  theme(plot.title = element_markdown())
g2.3<-ggplot(data = home.team.positions, aes(x=Frame,y=DistanceToGoal, fill=as.factor(Goal))) +
  geom_col() + theme(axis.text.x=element_blank(), legend.position = "none" ) +
  geom_vline(aes(xintercept = 21077)) +
  facet_wrap(~ Name) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  scale_fill_manual(values= c("gray","red")) +
  ggtitle(paste0("Players distance to goal through 15 seconds, highliting ",
                 "<span style = 'color:red;'>scorer</span>"))+
  theme(plot.title = element_markdown())


# Find nearest opponent and teammate --------------------------------------

home.team.positions <- find.positions(home.team, frame.start, frames.span)
away.team.positions <- find.positions(away.team, frame.start, frames.span, get.distance.from = 'h')

#using Kacaniklic as the reference player
kacaniklic <- home.team.positions[home.team.positions$Jersey==20,]
ggplot()+
  geom_line(data=kacaniklic, aes(x=Frame,y=NearestTeammate), color="forestgreen", size=2)+
  geom_line(data=kacaniklic, aes(x=Frame,y=NearestOpponent), color="blue", size=2)+ 
  geom_vline(aes(xintercept = 21077)) +
  theme(axis.text.x=element_blank(), axis.title.y = element_blank(),
        axis.title.x = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank()) +
  ggtitle(paste0("Kacaniklic distance to nearest ",
                 "<span style = 'color:forestgreen;'>teammate</span> and ",
                 "<span style = 'color:blue;'>opponent</span>"))+
  theme(plot.title = element_markdown())


# Looking for formations --------------------------------------------------

htp.no.gk <- home.team.positions[home.team.positions$Jersey != 25,]
formations <- NULL
for (i in frame.start:(frame.start+frames.span)){
  if (length(htp.no.gk[htp.no.gk$Frame == i,"X"]) < 1) next
  hlines <- kmeans(htp.no.gk[htp.no.gk$Frame == i,"X"],3)
  formations <- rbind(formations, hlines$size[order(desc(hlines$centers))])
}
formations <- as_tibble(formations)
names(formations)<-c("L1","L2","L3")
formations %>% 
  as_tibble() %>% 
  group_by(L1,L2,L3) %>% 
  summarise(n=n()/nrow(formations)) %>% 
  arrange(desc(n))

atp.no.gk <- away.team.positions[away.team.positions$Jersey != 27,]
formations <- NULL
for (i in frame.start:(frame.start+frames.span)){
  if (length(atp.no.gk[atp.no.gk$Frame == i,"X"]) < 1) next
  hlines <- kmeans(atp.no.gk[atp.no.gk$Frame == i,"X"],3)
  formations <- rbind(formations, hlines$size[order(hlines$centers)])
}
formations <- as_tibble(formations)
names(formations)<-c("L1","L2","L3")
formations %>% 
  as_tibble() %>% 
  group_by(L1,L2,L3) %>% 
  summarise(n=n()/nrow(formations)) %>% 
  arrange(desc(n))

home.team.positions <- find.positions(home.team, 20900)
away.team.positions <- find.positions(away.team, 20900)
hlines <- kmeans(htp.no.gk[htp.no.gk$Frame == 20900,"X"],3)
#goalkeeper in 9th position, receives cluster 0
home.team.positions["Cluster"] <- c(hlines$cluster[1:9],0,hlines$cluster[10])
g<- field.graph + 
  geom_point(data = home.team.positions, aes(x=X,y=Y,colour=as.factor(Cluster)), size=3)+
  scale_color_manual(values = c("#999999","#56B4E9","#E69F00","#111111")) +
  geom_point(data = away.team.positions, aes(x=X,y=Y), color="blue", alpha = 0.2)+ 
  geom_segment(data = home.team.positions,
               aes(x = X, y = Y, xend = dX, yend = dY),
               arrow = arrow(length = unit(0.1, "cm")))+ 
  geom_segment(data = away.team.positions,
               aes(x = X, y = Y, xend = dX, yend = dY),
               arrow = arrow(length = unit(0.1, "cm")))+
  geom_text(data = home.team.positions, aes(x=X+2, y=Y, label=Jersey), size=3) +
  geom_point(data = bp.df, aes(x=X,y=Y)) +
  geom_vline(aes(xintercept = hlines$centers[1])) +
  geom_vline(aes(xintercept = hlines$centers[2])) +
  geom_vline(aes(xintercept = hlines$centers[3])) + 
  theme(legend.position = "none") 
print(g)

