## functions for SNA projects

library(igraph)
# clrs <- c("lightblue","lightgoldenrod","lightgreen","magenta","orange","yellow","maroon","pink","saddlebrown","cornflowerblue","bisque4","turquoise","chartreuse","cyan","goldenrod","black","olivedrab","darkgreen","blue","snow","tomato","purple")
# plot(seq(length(clrs)),seq(length(clrs)),pch=19,col=clrs,xlab="Job Type Colors",ylab="Job Type Colors")
# force the date field to be DAY, MONTH, YEAR only
fix.date.updated <- function(data.vector) { 
  dash.dates = gsub("/","-",data.vector)
  updates <- strsplit(dash.dates,"-")
  # create a matrix of day - month - year separated out
  date.matrix = matrix(0,nrow = length(data.vector),ncol = 3)
  for (i in 1:length(data.vector)){
    if (length(as.numeric(unlist(updates[i])))==3){
      date.matrix[i,] = as.numeric(unlist(updates[i]))
    }
  }
  return(date.matrix)
}
#######################################################################
calc.day.of.yr <- function(m,d) {
  day.of.year = 0
  if (m==13) (day.of.year = 366) # leap year in 2016: change this after Dec 31 to subtract 1 from each month except "31"
  if (m==12) (day.of.year = 335)
  if (m==11) (day.of.year = 305)
  if (m==10) (day.of.year = 274)
  if (m==9) (day.of.year = 244)
  if (m==8) (day.of.year = 213)
  if (m==7) (day.of.year = 182)
  if (m==6) (day.of.year = 152)
  if (m==5) (day.of.year = 121)
  if (m==4) (day.of.year = 91)
  if (m==3) (day.of.year = 60)
  if (m==2) (day.of.year = 31)
  if (m==1) (day.of.year = 0)
  day.of.year = day.of.year + d
  return(day.of.year)
}
# connections "to" count by "froms"
conn.to.count <- function(the.froms,usernet) {
  conn.to.count.vector <- rep(0,nrow(the.froms))
  for (i in 1:nrow(the.froms)) {
    q <- which(usernet$from_user_guid == the.froms$user.guid[i])
    conn.to.count.vector[i] <- length(q)
  }
  return(conn.to.count.vector)
}
#
exact.count <- function(conn.to.count.vector){
  qmax <- max(conn.to.count.vector)
  exact.count.vector <- rep(0,qmax)
  for (i in 1:qmax){
    exact.count.vector[i] <- length(which(conn.to.count.vector == i))
  }
  return(exact.count.vector)
}
# connections "from" count by "tos"
conn.from.count <- function(the.tos,usernet) {
  conn.from.count.vector <- rep(0,nrow(the.tos))
  for (i in 1:nrow(the.tos)) {
    q <- which(usernet$to_user_guid == the.tos$user.guid[i])
    conn.from.count.vector[i] <- length(q)
  }
  return(conn.from.count.vector)
}
# remove internals from network data
remove.internals<- function(accepted.nets){
  ##### tanya
  tanya1 <- which(accepted.nets$from_user_guid == "897c63dc-6f4c-4041-8261-7c7be6d79b76")
  tanya2 <- which(accepted.nets$from_user_guid =="90d4303c-2b91-4816-bb17-517999ac9c8b")
  tanya3 <- which(accepted.nets$from_user_guid =="ed1b260c-59ec-44cf-b93f-79690c746a25") # this is 0
  tanya.all <- c(tanya1,tanya2)
  
  raechel1 <- which(accepted.nets$from_user_guid =="43160ed0-6246-4bf8-bf2f-e47a3eb727c8")
  raechel2 <- which(accepted.nets$from_user_guid =="cdfc1c4f-8717-480e-a5eb-0c48158f1d52") #this is 0
  raechel3 <- which(accepted.nets$from_user_guid =="38179886-b791-4ac5-b54a-f4cc11f98908") #this is 0
  raechel.all <- raechel1
  
  michael1 <- which(accepted.nets$from_user_guid =="1c1bee1f-3d4d-49e6-ad5b-bdc8cb69640e")
  michael2 <- which(accepted.nets$from_user_guid =="40eb88a0-12a8-4499-97ed-ae9628441fb3")
  michael3 <- which(accepted.nets$from_user_guid =="6927852e-17f7-4c18-83f6-542599e196da") #this is 0
  michael.all <- c(michael1,michael2)
  
  dwight1 <- which(accepted.nets$from_user_guid =="a7ee0837-fa4e-4876-a8cb-845b4930dc7b") #this is 0
  dwight2 <- which(accepted.nets$from_user_guid =="e7f78fcd-a642-4188-9234-c030f5f0bdbf")
  dwight.all <- dwight2
  
  heather1 <- which(accepted.nets$from_user_guid =="e411e56b-abfc-48cd-8046-ee35ab024cad")
  heather2 <- which(accepted.nets$from_user_guid =="cc014343-d978-4827-86a4-a5c6bb8253d7")
  heather.all <- c(heather1,heather2)
  
  recruit.team <- which(accepted.nets$from_user_guid =="835536f9-3162-4329-87ba-9e602a329529")
  
  kyle1 <- which(accepted.nets$from_user_guid =="86feb9c3-71f7-411e-8b49-94e5fd3ab400")
  kyle2 <- which(accepted.nets$from_user_guid =="de8400d0-052a-44e7-bd57-37925afec831")
  kyle.all <- c(kyle1,kyle2)
  
  ###########################################################################################################
  remove.these <- c(heather.all,dwight.all,michael.all,tanya.all,raechel.all,recruit.team,kyle.all)
  no.internals <- accepted.nets[-remove.these,]
  
  q11 <- which(no.internals$to_user_guid == "897c63dc-6f4c-4041-8261-7c7be6d79b76" ) # tanya
  q12 <- which(no.internals$to_user_guid == "90d4303c-2b91-4816-bb17-517999ac9c8b" ) # tanya
  q13 <- which(no.internals$to_user_guid == "43160ed0-6246-4bf8-bf2f-e47a3eb727c8" ) # raechel
  q14 <- which(no.internals$to_user_guid == "e7f78fcd-a642-4188-9234-c030f5f0bdbf" ) # dwight
  q15 <- which(no.internals$to_user_guid == "1c1bee1f-3d4d-49e6-ad5b-bdc8cb69640e" ) # michael
  q16 <- which(no.internals$to_user_guid == "40eb88a0-12a8-4499-97ed-ae9628441fb3" ) # michael
  q17 <- which(no.internals$to_user_guid == "e411e56b-abfc-48cd-8046-ee35ab024cad" ) # heather
  q18 <- which(no.internals$to_user_guid == "cc014343-d978-4827-86a4-a5c6bb8253d7" ) # heather
  q19 <- which(no.internals$to_user_guid == "835536f9-3162-4329-87ba-9e602a329529" ) # recruit team
  q20 <- which(no.internals$to_user_guid == "86feb9c3-71f7-411e-8b49-94e5fd3ab400" ) # kyle
  q21 <- which(no.internals$to_user_guid == "de8400d0-052a-44e7-bd57-37925afec831" ) # kyle
  qq3 <- c(q11,q12,q13,q14,q15,q16,q17,q18,q19,q20,q21)
  
  conn.mostguys <- no.internals[-qq3,]
  return(conn.mostguys)
}
# aggregates network data to user data
connect.user <- function(network.users,users.data){
  nw.users <- unique(network.users)
  user.details <- merge(nw.users,users.data,by.x = "x",by.y="users.guid")
  colnames(user.details) <- c("user.guid",names(user.details[2:ncol(user.details)]))
  return(user.details)
}
# provide user guid, and see how many accepted connections the user has
count.actual.connections <- function(id,network){
  connection.count <- which(network$user.guid == id)
  return(connection.count)
}
# find the active guys in the network based on accepted network initiations
find.active.guys <- function(from.to.count.vector,froms.list,n){
  q <- which(from.to.count.vector >= n)
  active.guys <- cbind(from.to.count.vector[q],froms.list[q,])
  return(active.guys)
}
# reconnect the identified network users with their network connection activity
connect.user2network <- function(froms.list,network.list){
  froms.users <- froms.list$user.guid
  network.matrix <- merge(network.list,froms.list,by.x="from_user_guid",by.y="user.guid",all.x=TRUE)
  return(network.matrix)
}
# ##################
# collect connections by specific members
get.main.connections <- function(recip,network.links){
  subnet <- network.links[1,]
  for (i in 1:length(recip)){ # gather any initiations from main user's recipients
    qs <- which(network.links$from_user_guid == recip[i])
    if (length(qs) >0){
      subnet <- rbind(subnet,network.links[qs,])
    }
  }
  subnet <- subnet[-1,] # will return a matrix with 0 rows if there aren't any
  clean.subnet <- subnet[which (!duplicated(subnet)),]
  return(clean.subnet)
}
get.connections <- function(recip,network.links,prev.subnet){
  subnet <- prev.subnet
  for (i in 1:length(recip)){ # gather any initiations from main user's recipients
    qs <- which(network.links$from_user_guid == recip[i])
    if (length(qs) >0){
      subnet <- rbind(subnet,network.links[qs,])
    }
  }
 clean.subnet <- subnet[which (!duplicated(subnet)),]
  return(clean.subnet)
}
get.one.users.connections <- function(recip,network.links,prev.subnet){
  subnet <- prev.subnet
    qs <- which(network.links$from_user_guid == recip)
    if (length(qs) >0){
      subnet <- network.links[qs,]
      clean.subnet <- subnet[which(!duplicated(subnet)),]
    }else{
      clean.subnet <- subnet
    }
  return(clean.subnet)
}

get.recipients <- function(user.id,network.links){
  q <- which(network.links$from_user_guid == user.id)
  if (length(q) >0){
    subnet <- network.links[q,]
    main.recip <- unique(subnet$to_user_guid) # these are the connections "user.id" connected to
    main.recip <- c(main.recip,unlist(as.character(user.id))) #
  }else{
    main.recip <- user.id # if coming into this branch, the user was only a "to" and not a "from" user
    q2 <- which(network.links$to_user_guid == user.id)
    if (length(q2) >0){ # do include the original "to" this user id was brought in under
      subnet <- network.links[q2,]# have to make sure "subnet" is defined
    }
  }
  q2 <- which(network.links$to_user_guid == user.id) # "user.id" is also a "to", include these
  if (length(q2) >1){ # don't include the original "to" this user id was brought in under
    subnet <- rbind(subnet,network.links[q2,])
    main.recip <- unique(c(main.recip,network.links$from_user_guid[q2])) # and add the main user's "from" to the recip list
    main.recip <- c(main.recip,unlist(as.character(user.id)))
  }
  main.recip <- unique(main.recip)
  return(main.recip)
}
 
##################
connection.guts <- function(subcomp,these.guids,links.this.year){
  ########################################
  for (i in 1:length(these.guids)){
    new.recip <- get.recipients(these.guids[i],links.this.year)
    rm.recip <- subcomp$from_user_guid[which(subcomp$to_user_guid == these.guids[i])]
    use.recip <- setdiff(new.recip,rm.recip) # have to remove the "from user guid" that brought in this "to user guid" in the first place
    if (length(use.recip) == 1){
      sub2 <- get.one.users.connections(use.recip,links.this.year,subcomp)
      if (length(sub2) >1){
        sub1 <- rbind(subcomp,sub2)
      }else{
        sub1 <- subcomp
      }
      subcomp <- sub1[which (!duplicated(sub1)),]
    }else{
      sub2 <- get.connections(use.recip,links.this.year,subcomp)
      if (nrow(sub2) >0){
        sub1 <- rbind(subcomp,sub2)
      }else{
        sub1 <- subcomp
      }
      subcomp <- sub1[which (!duplicated(sub1)),]
    }
  }
  ##################################################
  return(subcomp)
}
####################
get.plot <- function(subcomp,users.abr){
  all.froms <- subcomp[,c(1,3,4)]
  from.nodes <- all.froms[which(!duplicated(all.froms)),]
  full.from.nodes <- merge(from.nodes,users.abr[,c(1,2,3,12,13,15,16)],by.x="from_user_guid",by.y="users.guid")
  all.tos <- subcomp[,2:4]
  to.nodes <- subset(all.tos,!duplicated(all.tos$to_user_guid))
  
  colm.names <- c("users.guid","firstname","lastname","primary_work_history_guid","users.address_guid","hs_sso_user","registration_method","user.created.month" ,"user.created.yr")
  
  full.to.nodes <- merge(to.nodes,users.abr,by.x="to_user_guid",by.y="users.guid")
  complete.to.nodes <- full.to.nodes[,c(1,7,8,4,5,14,15,17,18)]
  colnames(complete.to.nodes) <- colm.names
  colnames(full.from.nodes) <- colm.names
  
  all.nodes <- rbind(full.from.nodes,complete.to.nodes)
  
  all.component.ids <- unique(c(subcomp$from_user_guid,subcomp$to_user_guid))
  unique.nodes <- subset(all.nodes,!duplicated(all.nodes$users.guid))
  this.component <- graph.data.frame(subcomp,unique.nodes,directed=T)
  this.component
  plot(this.component, edge.arrow.size =.4,vertex.label = V(this.component)$firstname)
  return(unique.nodes)
}
get.plot.jobtype <- function(subcomp,users.abr,job.pref){
  all.froms <- subcomp[,c(1,3,4)]
  from.nodes <- all.froms[which(!duplicated(all.froms)),]
  full.from.nodes <- merge(from.nodes,users.abr[,c(1,2,3,12,13,15,16)],by.x="from_user_guid",by.y="users.guid")
  complete.from.nodes <- merge(full.from.nodes,job.pref[,3:4],by.x="from_user_guid",by.y="user_guid",all.x=TRUE)
  colm.names <- c("users.guid","firstname","lastname","primary_work_history_guid","users.address_guid","hs_sso_user","registration_method","user.created.month" ,"user.created.yr","job_name")
  colnames(complete.from.nodes) <- colm.names
  for (kk in 1:nrow(complete.from.nodes)){
    if (is.na(complete.from.nodes$job_name[kk])){
      complete.from.nodes$job_name[kk] <- "None"
    }
  }
  for (kk in 1:nrow(complete.from.nodes)){
    if (is.na(complete.from.nodes$firstname[kk])){
      qno <- which(users.abr$users.guid == complete.from.nodes$users.guid[kk])
      complete.from.nodes$firstname[kk] <- users.abr$firstname[qno]
      complete.from.nodes$lastname[kk] <- users.abr$lastname[qno]
    }
  }
  #
  all.tos <- subcomp[,2:4]
  to.nodes <- subset(all.tos,!duplicated(all.tos$to_user_guid))
  full.to.nodes <- merge(to.nodes,users.abr,by.x="to_user_guid",by.y="users.guid")
  full.abr.to.nodes <- full.to.nodes[,c(1,7,8,4,5,14,15,17,18)]
  complete.to.nodes <- merge(full.abr.to.nodes,job.pref[,3:4],by.x="to_user_guid",by.y="user_guid",all.x=TRUE)
  colnames(complete.to.nodes) <- colm.names
  for (jj in 1:nrow(complete.to.nodes)){
    if (is.na(complete.to.nodes$job_name[jj])){
      complete.to.nodes$job_name[jj] <- "None"
    }
  }
  for (kk in 1:nrow(complete.to.nodes)){
    if (is.na(complete.to.nodes$firstname[kk])){
      qno <- which(users.abr$users.guid == complete.to.nodes$users.guid[kk])
      complete.to.nodes$firstname[kk] <- users.abr$firstname[qno]
      complete.to.nodes$lastname[kk] <- users.abr$lastname[qno]
    }
  }
  all.nodes <- rbind(complete.from.nodes,complete.to.nodes)
  
  #all.component.ids <- unique(c(subcomp$from_user_guid,subcomp$to_user_guid))
  unique.nodes <- subset(all.nodes,!duplicated(all.nodes$users.guid))
  #node_color <- rep("lightblue",nrow(unique.nodes))
  clrs <- c("lightblue","lightgoldenrod","lightgreen","magenta","orange","yellow","maroon","pink","saddlebrown","cornflowerblue","bisque4","turquoise","chartreuse","cyan","goldenrod","black","olivedrab","darkgreen","blue","snow","tomato","purple")
  node_color <- clrs[4]
  for (jk in 1:nrow(unique.nodes)){
    if(unique.nodes$job_name[jk] == "Manager") (node_color <- c(node_color,clrs[1])) #(unique.nodes$node_color[jk] <- clrs[1])
    if(unique.nodes$job_name[jk] == "Busser") (node_color <- c(node_color,clrs[2])) #(unique.nodes$node_color[jk] <- clrs[2])
    if(unique.nodes$job_name[jk] == "Server") (node_color <- c(node_color,clrs[3])) #(unique.nodes$node_color[jk] <- clrs[3])
    if(unique.nodes$job_name[jk] == "None") (node_color <- c(node_color,clrs[4])) #(unique.nodes$node_color[jk] <- clrs[4])
    if(unique.nodes$job_name[jk] == "Chef") (node_color <- c(node_color,clrs[5])) #(unique.nodes$node_color[jk] <- clrs[5])
    if(unique.nodes$job_name[jk] == "Cashier") (node_color <- c(node_color,clrs[6])) #(unique.nodes$node_color[jk] <- clrs[6])
    if(unique.nodes$job_name[jk] == "Bartender") (node_color <- c(node_color,clrs[7])) #(unique.nodes$node_color[jk] <- clrs[7])
    if(unique.nodes$job_name[jk] == "Bar-back") (node_color <- c(node_color,clrs[8])) #(unique.nodes$node_color[jk] <- clrs[8])
    if(unique.nodes$job_name[jk] == "Barista") (node_color <- c(node_color,clrs[9])) #(unique.nodes$node_color[jk] <- clrs[9])
    if(unique.nodes$job_name[jk] == "Baker") (node_color <- c(node_color,clrs[10])) #(unique.nodes$node_color[jk] <- clrs[10])
    if(unique.nodes$job_name[jk] == "Cook") (node_color <- c(node_color,clrs[11])) #(unique.nodes$node_color[jk] <- as.character(clrs[11]))
    if(unique.nodes$job_name[jk] == "Crew") (node_color <- c(node_color,clrs[12])) #(unique.nodes$node_color[jk] <- clrs[12])
    if(unique.nodes$job_name[jk] == "Team Member") (node_color <- c(node_color,clrs[13])) #(unique.nodes$node_color[jk] <- clrs[13])
    if(unique.nodes$job_name[jk] == "Dishwasher") (node_color <- c(node_color,clrs[14])) #(unique.nodes$node_color[jk] <- clrs[14])
    if(unique.nodes$job_name[jk] == "Driver") (node_color <- c(node_color,clrs[15])) #(unique.nodes$node_color[jk] <- clrs[15])
    if(unique.nodes$job_name[jk] == "Runner") (node_color <- c(node_color,clrs[16])) #(unique.nodes$node_color[jk] <- clrs[16])
    if(unique.nodes$job_name[jk] == "Host") (node_color <- c(node_color,clrs[17])) #(unique.nodes$node_color[jk] <- clrs[17])
    if(unique.nodes$job_name[jk] == "Prep Cook") (node_color <- c(node_color,clrs[18])) #(unique.nodes$node_color[jk] <- clrs[18])
    if(unique.nodes$job_name[jk] == "Sous Chef") (node_color <- c(node_color,clrs[19])) #(unique.nodes$node_color[jk] <- clrs[19])
    if(unique.nodes$job_name[jk] == "Trainer") (node_color <- c(node_color,clrs[20])) #(unique.nodes$node_color[jk] <- clrs[20])
    if(unique.nodes$job_name[jk] == "Drive-Thru") (node_color <- c(node_color,clrs[21])) #(unique.nodes$node_color[jk] <- clrs[21])
    if(unique.nodes$job_name[jk] == "Sommelier") (node_color <- c(node_color,clrs[22])) #(unique.nodes$node_color[jk] <- clrs[22])
  }
  node_color <- node_color[-1]
  unique.nodes <- cbind(unique.nodes,node_color)
  this.component <- graph.data.frame(subcomp,unique.nodes,directed=T)
  this.component
   plot(this.component, edge.arrow.size =.4,vertex.label = V(this.component)$firstname,vertex.color = V(this.component)$node_color)
  return(unique.nodes)
}
#
get.jobtype <- function(subcomp,users.abr,job.pref){
  all.froms <- subcomp[,c(1,3,4)]
  from.nodes <- all.froms[which(!duplicated(all.froms)),]
  full.from.nodes <- merge(from.nodes,users.abr[,c(1,2,3,12,13,15,16)],by.x="from_user_guid",by.y="users.guid")
  complete.from.nodes <- merge(full.from.nodes,job.pref[,3:4],by.x="from_user_guid",by.y="user_guid",all.x=TRUE)
  colm.names <- c("users.guid","firstname","lastname","primary_work_history_guid","users.address_guid","hs_sso_user","registration_method","user.created.month" ,"user.created.yr","job_name")
  colnames(complete.from.nodes) <- colm.names
  for (kk in 1:nrow(complete.from.nodes)){
    if (is.na(complete.from.nodes$job_name[kk])){
      complete.from.nodes$job_name[kk] <- "None"
    }
  }
  for (kk in 1:nrow(complete.from.nodes)){
    if (is.na(complete.from.nodes$firstname[kk])){
      qno <- which(users.abr$users.guid == complete.from.nodes$users.guid[kk])
      complete.from.nodes$firstname[kk] <- users.abr$firstname[qno]
      complete.from.nodes$lastname[kk] <- users.abr$lastname[qno]
    }
  }
  #
  all.tos <- subcomp[,2:4]
  to.nodes <- subset(all.tos,!duplicated(all.tos$to_user_guid))
  full.to.nodes <- merge(to.nodes,users.abr,by.x="to_user_guid",by.y="users.guid")
  full.abr.to.nodes <- full.to.nodes[,c(1,7,8,4,5,14,15,17,18)]
  complete.to.nodes <- merge(full.abr.to.nodes,job.pref[,3:4],by.x="to_user_guid",by.y="user_guid",all.x=TRUE)
  colnames(complete.to.nodes) <- colm.names
  for (jj in 1:nrow(complete.to.nodes)){
    if (is.na(complete.to.nodes$job_name[jj])){
      complete.to.nodes$job_name[jj] <- "None"
    }
  }
  for (kk in 1:nrow(complete.to.nodes)){
    if (is.na(complete.to.nodes$firstname[kk])){
      qno <- which(users.abr$users.guid == complete.to.nodes$users.guid[kk])
      complete.to.nodes$firstname[kk] <- users.abr$firstname[qno]
      complete.to.nodes$lastname[kk] <- users.abr$lastname[qno]
    }
  }
  all.nodes <- rbind(complete.from.nodes,complete.to.nodes)
  unique.nodes <- subset(all.nodes,!duplicated(all.nodes$users.guid))
  clrs <- c("lightblue","lightgoldenrod","lightgreen","magenta","orange","yellow","maroon","pink","saddlebrown","cornflowerblue","bisque4","turquoise","chartreuse","cyan","goldenrod","black","olivedrab","darkgreen","blue","snow","tomato","purple")
  node_color <- clrs[4]
  for (jk in 1:nrow(unique.nodes)){
    if(unique.nodes$job_name[jk] == "Manager") (node_color <- c(node_color,clrs[1])) #(unique.nodes$node_color[jk] <- clrs[1])
    if(unique.nodes$job_name[jk] == "Busser") (node_color <- c(node_color,clrs[2])) #(unique.nodes$node_color[jk] <- clrs[2])
    if(unique.nodes$job_name[jk] == "Server") (node_color <- c(node_color,clrs[3])) #(unique.nodes$node_color[jk] <- clrs[3])
    if(unique.nodes$job_name[jk] == "None") (node_color <- c(node_color,clrs[4])) #(unique.nodes$node_color[jk] <- clrs[4])
    if(unique.nodes$job_name[jk] == "Chef") (node_color <- c(node_color,clrs[5])) #(unique.nodes$node_color[jk] <- clrs[5])
    if(unique.nodes$job_name[jk] == "Cashier") (node_color <- c(node_color,clrs[6])) #(unique.nodes$node_color[jk] <- clrs[6])
    if(unique.nodes$job_name[jk] == "Bartender") (node_color <- c(node_color,clrs[7])) #(unique.nodes$node_color[jk] <- clrs[7])
    if(unique.nodes$job_name[jk] == "Bar-back") (node_color <- c(node_color,clrs[8])) #(unique.nodes$node_color[jk] <- clrs[8])
    if(unique.nodes$job_name[jk] == "Barista") (node_color <- c(node_color,clrs[9])) #(unique.nodes$node_color[jk] <- clrs[9])
    if(unique.nodes$job_name[jk] == "Baker") (node_color <- c(node_color,clrs[10])) #(unique.nodes$node_color[jk] <- clrs[10])
    if(unique.nodes$job_name[jk] == "Cook") (node_color <- c(node_color,clrs[11])) #(unique.nodes$node_color[jk] <- as.character(clrs[11]))
    if(unique.nodes$job_name[jk] == "Crew") (node_color <- c(node_color,clrs[12])) #(unique.nodes$node_color[jk] <- clrs[12])
    if(unique.nodes$job_name[jk] == "Team Member") (node_color <- c(node_color,clrs[13])) #(unique.nodes$node_color[jk] <- clrs[13])
    if(unique.nodes$job_name[jk] == "Dishwasher") (node_color <- c(node_color,clrs[14])) #(unique.nodes$node_color[jk] <- clrs[14])
    if(unique.nodes$job_name[jk] == "Driver") (node_color <- c(node_color,clrs[15])) #(unique.nodes$node_color[jk] <- clrs[15])
    if(unique.nodes$job_name[jk] == "Runner") (node_color <- c(node_color,clrs[16])) #(unique.nodes$node_color[jk] <- clrs[16])
    if(unique.nodes$job_name[jk] == "Host") (node_color <- c(node_color,clrs[17])) #(unique.nodes$node_color[jk] <- clrs[17])
    if(unique.nodes$job_name[jk] == "Prep Cook") (node_color <- c(node_color,clrs[18])) #(unique.nodes$node_color[jk] <- clrs[18])
    if(unique.nodes$job_name[jk] == "Sous Chef") (node_color <- c(node_color,clrs[19])) #(unique.nodes$node_color[jk] <- clrs[19])
    if(unique.nodes$job_name[jk] == "Trainer") (node_color <- c(node_color,clrs[20])) #(unique.nodes$node_color[jk] <- clrs[20])
    if(unique.nodes$job_name[jk] == "Drive-Thru") (node_color <- c(node_color,clrs[21])) #(unique.nodes$node_color[jk] <- clrs[21])
    if(unique.nodes$job_name[jk] == "Sommelier") (node_color <- c(node_color,clrs[22])) #(unique.nodes$node_color[jk] <- clrs[22])
  }
  node_color <- node_color[-1]
  unique.nodes <- cbind(unique.nodes,node_color)
  return(unique.nodes)
}
#
get.plot.jobtype.state <- function(subcomp,users.abr,job.pref,address){
  all.froms <- subcomp[,c(1,3,4)]
  from.nodes <- all.froms[which(!duplicated(all.froms)),]
  full.from.nodes <- merge(from.nodes,users.abr[,c(1,2,3,12,13,15,16)],by.x="from_user_guid",by.y="users.guid")
  complete.from.nodes <- merge(full.from.nodes,job.pref[,3:4],by.x="from_user_guid",by.y="user_guid",all.x=TRUE)
  state.complete.from.nodes <- merge(complete.from.nodes,address[,c(1,3)],by.x="users.address_guid",by.y="guid",all.x=TRUE)
  state.complete.from.nodes <- state.complete.from.nodes[,-1]
  colm.names <- c("users.guid","firstname","lastname","primary_work_history_guid","hs_sso_user","registration_method","user.created.month" ,"user.created.yr","job_name","state")
  colnames(state.complete.from.nodes) <- colm.names
  for (kk in 1:nrow(state.complete.from.nodes)){
    if (is.na(state.complete.from.nodes$job_name[kk])){
      state.complete.from.nodes$job_name[kk] <- "None"
    }
  }
  for (kk in 1:nrow(state.complete.from.nodes)){
    if (is.na(state.complete.from.nodes$firstname[kk])){
      qno <- which(users.abr$users.guid == state.complete.from.nodes$users.guid[kk])
      state.complete.from.nodes$firstname[kk] <- users.abr$firstname[qno]
      state.complete.from.nodes$lastname[kk] <- users.abr$lastname[qno]
    }
  }
  for (kk in 1:nrow(state.complete.from.nodes)){
    if (is.na(state.complete.from.nodes$state[kk])){
      state.complete.from.nodes$state[kk] <- "None"
    }
  }
  #
  all.tos <- subcomp[,2:4]
  to.nodes <- subset(all.tos,!duplicated(all.tos$to_user_guid))
  full.to.nodes <- merge(to.nodes,users.abr,by.x="to_user_guid",by.y="users.guid")
  full.abr.to.nodes <- full.to.nodes[,c(1,7,8,4,5,14,15,17,18)]
  complete.to.nodes <- merge(full.abr.to.nodes,job.pref[,3:4],by.x="to_user_guid",by.y="user_guid",all.x=TRUE)
  state.complete.to.nodes <- merge(complete.to.nodes,address[,c(1,3)],by.x="users.address_guid",by.y="guid",all.x=TRUE)
  state.complete.to.nodes <- state.complete.to.nodes[,-1]
  colm.names <- c("users.guid","firstname","lastname","primary_work_history_guid","hs_sso_user","registration_method","user.created.month" ,"user.created.yr","job_name","state")
  colnames(state.complete.to.nodes) <- colm.names
  for (kk in 1:nrow(state.complete.to.nodes)){
    if (is.na(state.complete.to.nodes$job_name[kk])){
      state.complete.to.nodes$job_name[kk] <- "None"
    }
  }
  for (kk in 1:nrow(state.complete.to.nodes)){
    if (is.na(state.complete.to.nodes$firstname[kk])){
      qno <- which(users.abr$users.guid == state.complete.to.nodes$users.guid[kk])
      state.complete.to.nodes$firstname[kk] <- users.abr$firstname[qno]
      state.complete.to.nodes$lastname[kk] <- users.abr$lastname[qno]
    }
  }
  for (kk in 1:nrow(state.complete.to.nodes)){
    if (is.na(state.complete.to.nodes$state[kk])){
      state.complete.to.nodes$state[kk] <- "None"
    }
  }
  
  all.nodes <- rbind(state.complete.from.nodes,state.complete.to.nodes)
  
  #all.component.ids <- unique(c(subcomp$from_user_guid,subcomp$to_user_guid))
  unique.nodes <- subset(all.nodes,!duplicated(all.nodes$users.guid))
  #node_color <- rep("lightblue",nrow(unique.nodes))
  clrs <- c("lightblue","lightgoldenrod","lightgreen","magenta","orange","yellow","maroon","pink","saddlebrown","cornflowerblue","bisque4","turquoise","chartreuse","cyan","goldenrod","black","olivedrab","darkgreen","blue","snow","tomato","purple")
  node_color <- clrs[4]
  for (jk in 1:nrow(unique.nodes)){
    if(unique.nodes$job_name[jk] == "Manager") (node_color <- c(node_color,clrs[1])) #(unique.nodes$node_color[jk] <- clrs[1])
    if(unique.nodes$job_name[jk] == "Busser") (node_color <- c(node_color,clrs[2])) #(unique.nodes$node_color[jk] <- clrs[2])
    if(unique.nodes$job_name[jk] == "Server") (node_color <- c(node_color,clrs[3])) #(unique.nodes$node_color[jk] <- clrs[3])
    if(unique.nodes$job_name[jk] == "None") (node_color <- c(node_color,clrs[4])) #(unique.nodes$node_color[jk] <- clrs[4])
    if(unique.nodes$job_name[jk] == "Chef") (node_color <- c(node_color,clrs[5])) #(unique.nodes$node_color[jk] <- clrs[5])
    if(unique.nodes$job_name[jk] == "Cashier") (node_color <- c(node_color,clrs[6])) #(unique.nodes$node_color[jk] <- clrs[6])
    if(unique.nodes$job_name[jk] == "Bartender") (node_color <- c(node_color,clrs[7])) #(unique.nodes$node_color[jk] <- clrs[7])
    if(unique.nodes$job_name[jk] == "Bar-back") (node_color <- c(node_color,clrs[8])) #(unique.nodes$node_color[jk] <- clrs[8])
    if(unique.nodes$job_name[jk] == "Barista") (node_color <- c(node_color,clrs[9])) #(unique.nodes$node_color[jk] <- clrs[9])
    if(unique.nodes$job_name[jk] == "Baker") (node_color <- c(node_color,clrs[10])) #(unique.nodes$node_color[jk] <- clrs[10])
    if(unique.nodes$job_name[jk] == "Cook") (node_color <- c(node_color,clrs[11])) #(unique.nodes$node_color[jk] <- as.character(clrs[11]))
    if(unique.nodes$job_name[jk] == "Crew") (node_color <- c(node_color,clrs[12])) #(unique.nodes$node_color[jk] <- clrs[12])
    if(unique.nodes$job_name[jk] == "Team Member") (node_color <- c(node_color,clrs[13])) #(unique.nodes$node_color[jk] <- clrs[13])
    if(unique.nodes$job_name[jk] == "Dishwasher") (node_color <- c(node_color,clrs[14])) #(unique.nodes$node_color[jk] <- clrs[14])
    if(unique.nodes$job_name[jk] == "Driver") (node_color <- c(node_color,clrs[15])) #(unique.nodes$node_color[jk] <- clrs[15])
    if(unique.nodes$job_name[jk] == "Runner") (node_color <- c(node_color,clrs[16])) #(unique.nodes$node_color[jk] <- clrs[16])
    if(unique.nodes$job_name[jk] == "Host") (node_color <- c(node_color,clrs[17])) #(unique.nodes$node_color[jk] <- clrs[17])
    if(unique.nodes$job_name[jk] == "Prep Cook") (node_color <- c(node_color,clrs[18])) #(unique.nodes$node_color[jk] <- clrs[18])
    if(unique.nodes$job_name[jk] == "Sous Chef") (node_color <- c(node_color,clrs[19])) #(unique.nodes$node_color[jk] <- clrs[19])
    if(unique.nodes$job_name[jk] == "Trainer") (node_color <- c(node_color,clrs[20])) #(unique.nodes$node_color[jk] <- clrs[20])
    if(unique.nodes$job_name[jk] == "Drive-Thru") (node_color <- c(node_color,clrs[21])) #(unique.nodes$node_color[jk] <- clrs[21])
    if(unique.nodes$job_name[jk] == "Sommelier") (node_color <- c(node_color,clrs[22])) #(unique.nodes$node_color[jk] <- clrs[22])
  }
  node_color <- node_color[-1]
  unique.nodes <- cbind(unique.nodes,node_color)
  this.component <- graph.data.frame(subcomp,unique.nodes,directed=T)
  this.component
  plot(this.component, edge.arrow.size =.4,vertex.label = V(this.component)$state,vertex.color = V(this.component)$node_color)
  return(unique.nodes)
}
#
get.recent.job <- function(unique.nodes,history,emer){
  users.history <- merge(unique.nodes,history,by.x="primary_work_history_guid",by.y="history.guid",all.x=TRUE)
  users.history <- users.history[,c(2:4,6,10:11,13:23)]
  colnames(users.history) <- c("users.guid","firstname","lastname","hs_sso_user","job_name","node_color","employer_guid","history.status","end.day","end.month","end.yr","start.day","start.month","start.yr","crtd.day","crtd.month","crtd.yr")
  #colnames(history) <- c("history.guid","users.guid","employer_guid","history.status","end.day","end.month","end.yr","start.day","start.month","start.yr","crtd.day","crtd.month","crtd.yr")
  
  for (i in 1:nrow(users.history)){
    if (is.na(users.history$employer_guid[i])){
      qem <- which(history$history.user_guid == users.history$users.guid[i])
      if (length(qem) > 0){
        users.history <- users.history[-i,]
        missing.history <- history[qem,]
        new.users.wh <- merge(missing.history,unique.nodes,by.x="history.user_guid",by.y="users.guid")
        new.users.history <- merge(new.users.wh,emer,by.x="history.employer_guid",by.y="guid",all.x=TRUE)
        add.users.history <- new.users.history[,c(2,14:15,18,22:23,1,4:13)]
        colnames(add.users.history) <- names(users.history)
        users.history <- rbind(users.history,add.users.history)
      }
    }
  }
  u.users <- unique(users.history$users.guid)
  recent.wh.matrix <- as.data.frame(matrix(0,nrow=length(u.users),ncol=ncol(users.history)))
  colnames(recent.wh.matrix) <- names(users.history)
  for (k in 1:length(u.users)){
    q <- which(users.history$users.guid == u.users[k])
    if (length(q) >= 1){ ###
      if (length(q) == 1){#
        recent.wh.matrix[k,] <- users.history[q,]
      }else{#
        more.than.one.job <- users.history[q,]
        still.working <- more.than.one.job[which(more.than.one.job$end.yr == 0),]
        if (nrow(still.working) >1) { ####
          max.year <- max(still.working$start.yr)
          keep.these <- still.working[which(still.working$start.yr == max.year),]
          if (nrow(keep.these) >1){######
            max.month <- max(keep.these$start.month)
            recent.wh.matrix[k,] <- keep.these[which(keep.these$start.month == max.month),]
          } ######
        }else{ ####
          recent.wh.matrix[k,] <- still.working
        } ####
      }#
    } ###
  }
  return(recent.wh.matrix)
}
#unique.wh.nodes <- get.plot.emer(subcomp,recent.emer)
get.plot.emer <- function(subcomp,recent.emer){
  all.user.ids <- unique(c(subcomp$from_user_guid,subcomp$to_user_guid))
  for (i in 1:length(all.user.ids)){
    qe <- which(recent.emer$users.guid == all.user.ids[i])
    if (length(qe) == 0){
      add.this.one <- matrix(0,nrow=1,ncol = ncol(recent.emer))
      add.this.one[1,1] <- all.user.ids[i]
      for (j in 2:ncol(add.this.one)){
        add.this.one[1,j] == "None"
      }
      recent.emer <- rbind(recent.emer,add.this.one)
    }
  }
 
  unique.nodes <- subset(recent.emer,!duplicated(recent.emer$users.guid))
  #node_color <- rep("lightblue",nrow(unique.nodes))
  clrs <- c("lightblue","lightgoldenrod","lightgreen","magenta","orange","yellow","maroon","pink","saddlebrown","cornflowerblue","bisque4","turquoise","chartreuse","cyan","goldenrod","black","olivedrab","darkgreen","blue","snow","tomato","purple")
  node_color <- clrs[4]
  for (jk in 1:nrow(unique.nodes)){
    if(unique.nodes$job_name[jk] == "Manager") (node_color <- c(node_color,clrs[1])) #(unique.nodes$node_color[jk] <- clrs[1])
    if(unique.nodes$job_name[jk] == "Busser") (node_color <- c(node_color,clrs[2])) #(unique.nodes$node_color[jk] <- clrs[2])
    if(unique.nodes$job_name[jk] == "Server") (node_color <- c(node_color,clrs[3])) #(unique.nodes$node_color[jk] <- clrs[3])
    if(unique.nodes$job_name[jk] == "None") (node_color <- c(node_color,clrs[4])) #(unique.nodes$node_color[jk] <- clrs[4])
    if(unique.nodes$job_name[jk] == "Chef") (node_color <- c(node_color,clrs[5])) #(unique.nodes$node_color[jk] <- clrs[5])
    if(unique.nodes$job_name[jk] == "Cashier") (node_color <- c(node_color,clrs[6])) #(unique.nodes$node_color[jk] <- clrs[6])
    if(unique.nodes$job_name[jk] == "Bartender") (node_color <- c(node_color,clrs[7])) #(unique.nodes$node_color[jk] <- clrs[7])
    if(unique.nodes$job_name[jk] == "Bar-back") (node_color <- c(node_color,clrs[8])) #(unique.nodes$node_color[jk] <- clrs[8])
    if(unique.nodes$job_name[jk] == "Barista") (node_color <- c(node_color,clrs[9])) #(unique.nodes$node_color[jk] <- clrs[9])
    if(unique.nodes$job_name[jk] == "Baker") (node_color <- c(node_color,clrs[10])) #(unique.nodes$node_color[jk] <- clrs[10])
    if(unique.nodes$job_name[jk] == "Cook") (node_color <- c(node_color,clrs[11])) #(unique.nodes$node_color[jk] <- as.character(clrs[11]))
    if(unique.nodes$job_name[jk] == "Crew") (node_color <- c(node_color,clrs[12])) #(unique.nodes$node_color[jk] <- clrs[12])
    if(unique.nodes$job_name[jk] == "Team Member") (node_color <- c(node_color,clrs[13])) #(unique.nodes$node_color[jk] <- clrs[13])
    if(unique.nodes$job_name[jk] == "Dishwasher") (node_color <- c(node_color,clrs[14])) #(unique.nodes$node_color[jk] <- clrs[14])
    if(unique.nodes$job_name[jk] == "Driver") (node_color <- c(node_color,clrs[15])) #(unique.nodes$node_color[jk] <- clrs[15])
    if(unique.nodes$job_name[jk] == "Runner") (node_color <- c(node_color,clrs[16])) #(unique.nodes$node_color[jk] <- clrs[16])
    if(unique.nodes$job_name[jk] == "Host") (node_color <- c(node_color,clrs[17])) #(unique.nodes$node_color[jk] <- clrs[17])
    if(unique.nodes$job_name[jk] == "Prep Cook") (node_color <- c(node_color,clrs[18])) #(unique.nodes$node_color[jk] <- clrs[18])
    if(unique.nodes$job_name[jk] == "Sous Chef") (node_color <- c(node_color,clrs[19])) #(unique.nodes$node_color[jk] <- clrs[19])
    if(unique.nodes$job_name[jk] == "Trainer") (node_color <- c(node_color,clrs[20])) #(unique.nodes$node_color[jk] <- clrs[20])
    if(unique.nodes$job_name[jk] == "Drive-Thru") (node_color <- c(node_color,clrs[21])) #(unique.nodes$node_color[jk] <- clrs[21])
    if(unique.nodes$job_name[jk] == "Sommelier") (node_color <- c(node_color,clrs[22])) #(unique.nodes$node_color[jk] <- clrs[22])
  }
  node_color <- node_color[-1]
  unique.nodes <- cbind(unique.nodes,node_color)
  qhs <- which(unique.nodes$hs_sso_user == "")
  if (length(qhs) > 0){
    unique.nodes[qhs,4] <- "false"
  }
  shps <- c("circle","sphere")
  node_shape <- shps[1]
  for (jk in 1:nrow(unique.nodes)){
    if(unique.nodes$hs_sso_user[jk] == "false") (node_shape <- c(node_shape,shps[1])) 
    if(unique.nodes$hs_sso_user[jk] == "true") (node_shape <- c(node_shape,shps[2])) 
  }
  node_shape <- node_shape[-1]
  unique.nodes <- cbind(unique.nodes,node_shape) 
  this.component <- graph.data.frame(subcomp,unique.nodes,directed=T)
 #  V(this.component)$shape <-c("square","circle")[V(this.component)$hs_sso_user]  #v.shape[V(this.component)$hs_sso_user]
  plot(this.component, edge.arrow.size =.4,vertex.label = V(this.component)$name,vertex.color = V(this.component)$node_color,vertex.shape = V(this.component)$node_shape)
  return(unique.nodes)
}
##################################################
# find indices of claimed or unclaimed stores
is.claimed <- function(data,colm.index){
  claimed.indices <- which(toupper(data[,colm.index]) == "TRUE")
  return(claimed.indices)
}
##
is.unclaimed <- function(data,colm.index){
  unclaimed.indices <- which(toupper(data[,colm.index]) == "FALSE")
  return(unclaimed.indices)
}
######################################################################################################################
LOOKUP_DF <- "C:/Users/Becky.Stark/Documents/Lookup files"
BF_DATAFILES <- "C:/Users/Becky.Stark/Downloads"
RD_DATAFILES <- "C:/Users/Becky.Stark/Documents/KPI Daily Stats Reports"
DASHBOARD_DATA <- "C:/Users/Becky.Stark/Documents/recruit dashboard/data"
# #####################################################################################################################