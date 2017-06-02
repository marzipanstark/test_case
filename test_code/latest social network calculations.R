##########################################################################################################

##sna code
##########################################################################################################
#library(igraph)
# # # ###################################################################

#find active guys
active.number <- 4
active.this.month <- find.active.guys(from.to.count.this.month,froms.this.month,active.number)
active.number <- 20
active.this.year <- find.active.guys(from.to.count.this.year,froms.this.year,active.number)
active.number <- 100
active.network <- find.active.guys(from.to.count,legit.froms,active.number)
#
write.csv(active.network,"C:/Users/Becky.Stark/Documents/Data Science Shared/active network guys.csv",row.names=FALSE)
write.csv(active.this.year,"C:/Users/Becky.Stark/Documents/Data Science Shared/active network guys_this year.csv",row.names=FALSE)
write.csv(active.this.month,"C:/Users/Becky.Stark/Documents/Data Science Shared/active network guys_this month.csv",row.names=FALSE)

# now connect the unique network users back to their network connections
nw.all.time <- connect.user2network(legit.froms,clean.nets)
nw.this.year <- connect.user2network(froms.this.year,nets.this.year)
nw.this.month <- connect.user2network(froms.this.month,nets.this.month)
#graph this month network
these.pairs.this.month <- nw.this.month[,c(1,9:17,22:27)] 
links.this.month <- these.pairs.this.month[,c(1,2,9,10)]
q <- which(is.na(links.this.month$firstname))
for (i in 1:length(q)){
  this.index <- which(users.abr$users.guid == links.this.month$from_user_guid[q[i]])
  if (length(this.index) == 1){
    links.this.month$firstname[q[i]] <- users.abr$firstname[this.index]
    links.this.month$lastname[q[i]] <- users.abr$lastname[this.index]
  }
}

nodes.this.month <- unique(c(links.this.month[,1],links.this.month[,2])) # collect the from user ids and the to user ids
# get network for the year
these.pairs.this.year <- nw.this.year[,c(1,9:17,22:27)] 
links.this.year <- these.pairs.this.year[,c(1,2,9,10)]
q <- which(is.na(links.this.year$firstname))
for (i in 1:length(q)){
  this.index <- which(users.abr$users.guid == links.this.year$from_user_guid[q[i]])
  if (length(this.index) == 1){
    links.this.year$firstname[q[i]] <- users.abr$firstname[this.index]
    links.this.year$lastname[q[i]] <- users.abr$lastname[this.index]
  }
}

nodes.this.year <- unique(c(links.this.year[,1],links.this.year[,2]))
write.csv(these.pairs.this.month,"C:/Users/Becky.Stark/Documents/Data Science Shared/this month pairs.csv",row.names=FALSE)
write.csv(links.this.month,"C:/Users/Becky.Stark/Documents/Data Science Shared/edges this month.csv",row.names=FALSE)
write.csv(nodes.this.month,"C:/Users/Becky.Stark/Documents/Data Science Shared/nodes this month.csv",row.names=FALSE)
nodes <- read.csv("C:/Users/Becky.Stark/Documents/Data Science Shared/nodes this month.csv", header=T, as.is=T)
links <- read.csv("C:/Users/Becky.Stark/Documents/Data Science Shared/edges this month.csv", header=T, as.is=T)
write.csv(links.this.year,"C:/Users/Becky.Stark/Documents/Data Science Shared/edges this year.csv",row.names=FALSE)
write.csv(nodes.this.year,"C:/Users/Becky.Stark/Documents/Data Science Shared/nodes this year.csv",row.names=FALSE)
nodes.this.year <- read.csv("C:/Users/Becky.Stark/Documents/Data Science Shared/nodes this year.csv", header=T, as.is=T)
links.this.year <- read.csv("C:/Users/Becky.Stark/Documents/Data Science Shared/edges this year.csv", header=T, as.is=T)
##############################

# install.packages("igraph")
# install.packages("network")
# install.packages("sna")
# install.packages("ndtv")
#library(igraph)
# net <- graph.data.frame(links, nodes, directed=T)
# net
# build a small network to graph - start with the top user
active.number <- 1
active.this.month <- find.active.guys(from.to.count.this.month,froms.this.month,active.number)
max.count <- max(active.this.month$`from.to.count.vector[q]`)
qcount <- which(active.this.month$`from.to.count.vector[q]` == max.count)

#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
# kick it off with a top guy
# top.guy <- active.this.month$user.guid[qcount[1]]
# new.id <- top.guy
want.job.plot <- 0
for (ijk in 1:2){ ### big outside loop
  top.guy <- active.this.month$user.guid[qcount[1]]
  new.id <- top.guy
  flag <- 0
  component.count = 0
  while (flag == 0){ ## just inside big outside loop
    if(top.guy == new.id){
      first.time.thru <- 1
    }else{
      first.time.thru <- 0
    }
    main.recip <- get.recipients(new.id,links.this.year)
    sub1 <- get.main.connections(main.recip,links.this.year)
    subcomp <- sub1[which (!duplicated(sub1)),]
    prev.subcomp <- subcomp
    ########################################
    new.guids <- unique(subcomp$to_user_guid)
    these.guids <- setdiff(new.guids,main.recip)
    if(length(these.guids) > 0){ ##########
      # if not true, this component is done
      for (i in 1:length(these.guids)){ #
        new.recip <- get.recipients(these.guids[i],links.this.year)
        rm.recip <- subcomp$from_user_guid[which(subcomp$to_user_guid == these.guids[i])]
        use.recip <- setdiff(new.recip,rm.recip) # have to remove the "from user guid" that brought in this "to user guid" in the first place
        if (length(use.recip) == 1){ ##
          sub2 <- get.one.users.connections(use.recip,links.this.year,subcomp)
          if (length(sub2) >1){ ###
            sub1 <- rbind(subcomp,sub2)
          }else{ ###
            sub1 <- subcomp
          } ###
          subcomp <- sub1[which (!duplicated(sub1)),]
        }else{ ##
          sub2 <- get.connections(use.recip,links.this.year,subcomp)
          if (nrow(sub2) >0){ ####
            sub1 <- rbind(subcomp,sub2)
          }else{ ####
            sub1 <- subcomp
          } ####
          subcomp <- sub1[which (!duplicated(sub1)),]
        } ##
      } #
      ##################################################
      if (nrow(prev.subcomp) < nrow(subcomp)){ ######
        #repeat the process... otherwise, you are done
        new.guids <- unique(subcomp$to_user_guid)
        prev.guids <- unique(prev.subcomp$to_user_guid)
        
        if (length(new.guids) > length(prev.guids)){ #####
          these.guids <- setdiff(new.guids,prev.guids)
          loop.counter = 0
          new.subcomp <- connection.guts(subcomp,these.guids,links.this.year)
        while (nrow(new.subcomp) > nrow(subcomp)){ ####
          #keep going
          loop.counter <- loop.counter + 1
          new.subcomp <- connection.guts(subcomp,these.guids,links.this.year)
          if (nrow(new.subcomp) > nrow(subcomp)){ ###
            new.guids <- unique(new.subcomp$to_user_guid)
            prev.guids <- unique(subcomp$to_user_guid)
            these.guids <- setdiff(new.guids,prev.guids)
            }else{ ###
            subcomp <- new.subcomp
          } ###
        } ####
       }   #####
      } ######
    }  ##########
    ###############################################
    # now plot it

    unique.nodes <- get.plot(subcomp,users.abr)
    if (ijk == 2){
      unique.nodes <- get.plot.jobtype.state(subcomp,users.abr,job.pref,address)
      component.count <- component.count + 1
    }
    if (ijk == 1){ #
      if (want.job.plot == 1){ # if just want to plot the restaurant as the label on the nodes, don't put "want.job.plot" == 1
        unique.nodes <- get.plot.jobtype(subcomp,users.abr,job.pref)
      }else{
        unique.nodes <- get.jobtype(subcomp,users.abr,job.pref)
      }
      recent.wh.matrix <- get.recent.job(unique.nodes,history,emer)
      recent.emer <- merge(recent.wh.matrix,emer,by.x="employer_guid",by.y="guid",all.x=TRUE)
      recent.emer <- recent.emer[,c(2:17,21,22)]#c(2:5,8:13,20,21)]
      for (kk in 1:nrow(recent.emer)){ ##
        if (is.na(recent.emer$name[kk])){ ###
          recent.emer$name[kk] <- "None"
          recent.emer$type[kk] <- "None"
        } ###
      } ##
     unique.wh.nodes <- get.plot.emer(subcomp,recent.emer)
     Sys.sleep(2)
    } #
    if (first.time.thru == 1){
      completed.nodes <- unique(unique.nodes$users.guid)
      new.id.list <- as.character(unlist(top.guy))
      new.id.workplace <- recent.emer
    }else{
      completed.nodes <- unique(c(completed.nodes,unique.nodes$users.guid))
      new.id.list <- unique(c(new.id.list,new.id))
      new.id.workplace <- rbind(new.id.workplace,recent.emer)
    }
    all.done <- length(completed.nodes)
    ###########################################################################
    still.left <- setdiff(nodes.this.month,completed.nodes)
    if (length(still.left) == "0"){ ##
      flag <- 1
    }else{ ##
      still.left.degree <- rep(0,length(still.left))
      for (ii in 1:length(still.left)){
        get.index <- which(active.this.month$user.guid == still.left[ii])
        if (length(get.index) >0){
          still.left.degree[ii] <- active.this.month[get.index,1]
        }
      }
      new.max <- max(still.left.degree)
      new.max.ind <- which(still.left.degree == new.max)
      new.id <- still.left[new.max.ind[1]]
    } ##
  } ### just inside big outside loop
} #### big outside loop

# note that the "new.id.list" is a vector of decreasingly high node degree members.

#use these to recommend connections to other members in this list.

# #recommendation algorithm 1
# # for (i in 1: length(new.id.list)){
# #     recommend to new.id.list[i] a connection to any other new.id.list member on the list.
# #     Prefer to have members connect to those in the higher positions. The earlier a member id
# #       shows up in the list, the more connected the member is.
# #}
# 

# #recommendation algorithm 2
# # for (i in 1: nrow(new.id.workplace)){
# #     recommend to these users (object is new.id.workplace$users.guid) a connection to any other member with their same job type.
# #     recommend to these users a connection to any other member with their same job location
# #     Take advantage of inviting these members to connect since they have already demonstrated 
# #       an interest in using the network functionality.
# #}
# 

# #recommendation algorithm 3
# # for (i in 1: nrow(new.id.workplace)){
# #     recommend connections to these users in the new.id.workplace object who are of these job types:
# #      * Managers * Bartenders * Bar-backs * Servers * Sommeliers
# #     These job types have demonstrated more use in our network functionality than other job types
# #       These are the more inherently "social" job types
# #}
# 

# #recommendation algorithm 4
# # 
# #     recommend connections to members in "same trajectories" according to Heather's Education track map
# #      
# #   examples: barback - server - bartender - sommelier  
# #             host - server- bartender-manager   
# #             dishwasher - prep cook - line cook - sous chef - exec chef
#  

# #recommendation algorithm 5
# # 
# #     recommend connections to members in "same responsibility positions" according to Heather's Education track map
# #      
# #   examples: server - bartender - sommelier  
# #             manager - trainer - sous chef - exec chef
#  

# #recommendation algorithm 6
# # 
# #     recommend connections to members in "social positions" to try to get them more involved in using our network
# #      
# #   examples: servers - bartenders - sommeliers - managers - barbacks
# #       These job types have demonstrated a higher likelihood of using our networks than other job types      
# 
