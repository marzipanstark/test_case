updates <- unlist(strsplit(as.character(Sys.Date()),"-"))
date.vector = as.numeric(updates)
this.year = date.vector[1]
this.month = date.vector[2] 
this.day = date.vector[3]
this.date = paste(this.month,"/",this.day,"/",this.year)
doy = calc.day.of.yr(this.month,this.day) # calc day of year
###################################################################################################################################
#install.packages("sand")
library(sand)
install_sand_packages()
library(igraph)

##########################################################################################################
########################  stats just from jan 1 this.year (2017 now)
#num.months <- 2
num.months <- 1
#
#keep.internals <- 1 # means yes, keep them
keep.internals <- 0 # means no, don't keep them
#
prev.month <- this.month - 1
usernet.this.year <- user.nets[which(user.nets$created.year == this.year),]
usernet.prev.mo <- usernet.this.year[which(usernet.this.year$created.month == prev.month),]
usernet.this.mo <- usernet.this.year[which(usernet.this.year$created.month == this.month),]
# userempnet.this.year <- user.employer.net[which(user.employer.net$created.year == this.year),]
# userempnet.this.mo <- userempnet.this.year[which(userempnet.this.year$created.month == this.month),]
# userempnet.prev.mo <- userempnet.this.year[which(userempnet.this.year$created.month == prev.month),]
# endorseemp.this.year <- endorse.employer[which(endorse.employer$created.year == this.year),]
# endorseemp.this.mo <- endorseemp.this.year[which(endorseemp.this.year$created.month == this.month),]
# endorseemp.prev.mo <- endorseemp.this.year[which(endorseemp.this.year$created.month == prev.month),]
# endorseuser.this.year <- endorse.user[which(endorse.user$created.year == this.year),]
# endorseuser.this.mo <- endorseuser.this.year[which(endorseuser.this.year$created.month == this.month),]
# endorseuser.prev.mo <- endorseuser.this.year[which(endorseuser.this.year$created.month == prev.month),]
ref.this.year <- referrals[which(referrals$created.year == this.year),]
ref.this.mo <- ref.this.year[which(ref.this.year$created.month == this.month),]
ref.prev.mo <- ref.this.year[which(ref.this.year$created.month == prev.month),]

#  
# net.stats <- c(user.net.pairs,u.user.from,u.user.to,num.followings,num.stores.followed,num.users.following,num.referrals,u.referred,u.referring,num.store.endorse,num.user.endorse,u.endorsed.user,u.endorsing.user,u.endorsed.store,u.endorsing.user2store)
# write.csv(net.stats,file.path(DASHBOARD_DATA,"network stats.csv"),row.names = FALSE)
##### just keep the ones that have status = 2 (accepted)
# these are the people who are IN a network, separated out by the list of "tos" and the list of "froms"

accepted.nets <- user.nets[which(user.nets$status == "2"),]
# clean the networks from internal users or keep them in
if (keep.internals == 0){
  clean.nets <- remove.internals(accepted.nets) # if "keep.internals" == 0, remove them
}
if (keep.internals == 1){
  clean.nets <- accepted.nets # if "keep.internals" == 1, keep them
}
# error occurs in "keep.internals" is not defined
# ###########################################################################################################
legit.froms <- connect.user(clean.nets$from_user_guid,users.oi)
legit.tos <- connect.user(clean.nets$to_user_guid,users.oi)
nets.this.year <- clean.nets[which(clean.nets$created.year == this.year),]
if (num.months == 1){
  nets.this.month <- nets.this.year[which(nets.this.year$created.month == this.month),]
}
if (num.months == 2){
  nets.prev.month <- nets.this.year[which(nets.this.year$created.month == prev.month),]
  nets.2.months <- rbind(nets.prev.month,nets.this.month)
  nets.this.month <- nets.2.months[which (!duplicated(nets.2.months)),] # NOTE: name is not accurate
}

froms.this.year <- connect.user(nets.this.year$from_user_guid,users.oi)
froms.this.month <- connect.user(nets.this.month$from_user_guid,users.oi)
tos.this.year <- connect.user(nets.this.year$to_user_guid,users.oi)
tos.this.month <- connect.user(nets.this.month$to_user_guid,users.oi)
# this next batch takes the list of "tos" (or the list of "froms) and sees how many connections each one has
# it also looks at it the opposite way, but count of how many "tos" (or "froms") have 1 connection, 2 connections, etc.
from.to.count <- conn.to.count(legit.froms,clean.nets) # returns a vector count of how many connections each initiator has
from.to.count.this.year <- conn.to.count(froms.this.year,nets.this.year)
from.to.count.this.month <- conn.to.count(froms.this.month,nets.this.month)
to.from.count <- conn.from.count(legit.tos,clean.nets) # returns a vector count of how many connections each initiator has
to.from.count.this.year <- conn.from.count(tos.this.year,nets.this.year)
to.from.count.this.month <- conn.from.count(tos.this.month,nets.this.month)
exact.count.vector.from <- exact.count(from.to.count)
exact.count.vector.this.year.from <- exact.count(from.to.count.this.year)
exact.count.vector.this.month.from <- exact.count(from.to.count.this.month)
exact.count.vector.to <- exact.count(to.from.count)
exact.count.vector.this.year.to <- exact.count(to.from.count.this.year)
exact.count.vector.this.month.to <- exact.count(to.from.count.this.month)
#
write.csv(table(from.to.count),"C:/Users/Becky.Stark/Documents/Data Science Shared/distribution of counts of accepted network initiations.csv",row.names=FALSE)
write.csv(table(from.to.count.this.year),"C:/Users/Becky.Stark/Documents/Data Science Shared/distribution of counts of accepted network initiations_this year.csv",row.names=FALSE)
write.csv(table(from.to.count.this.month),"C:/Users/Becky.Stark/Documents/Data Science Shared/distribution of counts of accepted network initiations_this month.csv",row.names=FALSE)
more.info.legit.froms <- merge(legit.froms,history,by.x="primary_work_history_guid",by.y="history.guid")
more.info.legit.tos <- merge(legit.tos,history,by.x="primary_work_history_guid",by.y="history.guid")
#######################################################
######################################################
#####################################################
##### from "latest social network calculations
######################################################
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