# KPI features for members and employers
# doesn't have any login behavior

updates <- unlist(strsplit(as.character(Sys.Date()),"-"))
date.vector = as.numeric(updates)
this.year = date.vector[1]
this.month = date.vector[2] 
this.day = date.vector[3]

this.date = paste(this.month,"/",this.day,"/",this.year)
doy = calc.day.of.yr(this.month,this.day) # calc day of year
###################################################################################################################################
referrals <- read.csv(file.path(BF_DATAFILES,"bf_referral.csv"),header=TRUE,colClasses="character",skip=0,fill=TRUE,strip.white=TRUE,blank.lines=TRUE)
user.nets <- read.csv(file.path(BF_DATAFILES,"bf_user_network.csv"),header=TRUE,colClasses="character",skip=0,fill=TRUE,strip.white=TRUE,blank.lines=TRUE)
user.employer.net <- read.csv(file.path(BF_DATAFILES,"bf_user_employer_network.csv"),header=TRUE,colClasses="character",skip=0,fill=TRUE,strip.white=TRUE,blank.lines=TRUE)
endorse.employer <- read.csv(file.path(BF_DATAFILES,"bf_employer_endorsement.csv"),header=TRUE,colClasses="character",skip=0,fill=TRUE,strip.white=TRUE,blank.lines=TRUE)
endorse.user <- read.csv(file.path(BF_DATAFILES,"bf_user_endorsement.csv"),header=TRUE,colClasses="character",skip=0,fill=TRUE,strip.white=TRUE,blank.lines=TRUE)
##################################################################################
# bring in most recent user data
users = read.csv(file.path(BF_DATAFILES,"bf_user.csv"),header=TRUE,colClasses="character",skip=0,fill=TRUE,strip.white=TRUE,blank.lines=TRUE)
users.abr = users[,c(5,12,13,3,4,8,20,7,14,15,16,21,22,2)] # fields - user[guid],user[WH guid], user[address guid],other stuff,returning_user,VC,account state,terms_accepted,email, created date
users.created.date <- fix.date.updated(users.abr$created)
created.names = c("created.day","created.month","created.year")
colnames(users.created.date)<- created.names
users.abr = cbind(users.abr[,1:13],users.created.date)
names(users.abr) = c("users.guid","primary_work_history_guid","users.address_guid","email","firstname","lastname","terms_accepted_version","returning_user","about","verification_code","account_state","hs_sso_user","registration_method","user.created.day","user.created.month","user.created.yr")
q <- which(users.abr$user.created.yr == 0)
if (length(q) > 0){
  users.abr <- users.abr[-q,]
}
users.oi <- users.abr[which(users.abr$account_state == 0),]
######################################
upref = read.csv(file.path(BF_DATAFILES,"bf_user_preference.csv"),header=TRUE,colClasses="character",skip=0,fill=TRUE,strip.white=TRUE,blank.lines=TRUE)
jobtype = read.csv(file.path(BF_DATAFILES,"bf_jobtype.csv"),header=TRUE,colClasses="character",skip=0,fill=TRUE,strip.white=TRUE,blank.lines=TRUE)
upref = upref[,c(2,3,5,6,8)]
qpref = which(upref[,3]==1)
jobsearch = upref[qpref,c(1,4,5)] # created date, job type guid, user guid
colnames(jobsearch) <- c("date","jobtype_guid","user_guid")
jobtype = jobtype[,c(4,5)]
colnames(jobtype) <- c("job_guid","job_name")
# job.merge = merge(jobtype,jobsearch,by.x="job_guid",by.y="jobtype_guid")
# jobtype.table <- table(job.merge$job_name)
# jobnames <- names(jobtype.table)
# job.preferences <- cbind(jobnames,as.numeric(jobtype.table))
# colnames(job.preferences)<- c("jobName","Frequency")
job.pref <- merge(jobsearch,jobtype,by.x="jobtype_guid",by.y="job_guid")
# write.csv(job.preferences,file.path(DASHBOARD_DATA,"job preference table.csv"),row.names=FALSE)
# num.w.pref <- length(unique(jobsearch[,3]))
########### Read in data ##################

####################################
address = read.csv(file.path(BF_DATAFILES,"bf_address.csv"),header=TRUE,colClasses="character",skip=0,fill=TRUE,strip.white=TRUE,blank.lines=TRUE)
street.address <- address$line1
address.geo <- address$address_geo_guid
address = address[,c(5,10,8,3,2)] # address[guid], zip, state, country, city
###########################################
job_postings = read.csv(file.path(BF_DATAFILES,"bf_job_posting.csv"),header=TRUE,colClasses="character",skip=0,fill=TRUE,strip.white=TRUE,blank.lines=TRUE)
job = job_postings[,c(4,5,10,11,14,17,18,19,2,22:25)] # created date is last
job.extra.fields <- job_postings[,c(4,3,7,8,12,14,16,19,22,23,25)] # for job matching algorithm
rm(job_postings)
###########################################
# users = read.csv(file.path(BF_DATAFILES,"bf_user.csv"),header=TRUE,colClasses="character",skip=0,fill=TRUE,strip.white=TRUE,blank.lines=TRUE)
# users.abr = users[,c(5,12,13,3,4,8,20,7,14,15,16,21,22,2)] # fields - user[guid],user[WH guid], user[address guid],other stuff,returning_user,VC,account state,terms_accepted,email, created date
# ##### important: changed the 7th field here from "terms_accepted" which is no longer used, to "terms_accepted_version"
# # if "terms_accepted_version" is other than 0, user has accepted some version of T&Cs
###########################################
candidates = read.csv(file.path(BF_DATAFILES,"bf_job_posting_candidate.csv"),header=TRUE,colClasses="character",skip=0,fill=TRUE,strip.white=TRUE,blank.lines=TRUE)
cand = candidates[,c(3,8,9,2,4:6)] # cand[guid],cand[user guid],cand[job guid]
cand.extra.fields = candidates[,10:15] # these are "rating", "status", and "statusdate" - probably not needed right now
rm(candidates)
###########################################
history = read.csv(file.path(BF_DATAFILES,"bf_user_workhistory.csv"),header=TRUE,colClasses="character",skip=0,fill=TRUE,strip.white=TRUE,blank.lines=TRUE)
history = history[,c(5,10,11,4,6,7,3)] # history[guid], history[user_guid], history[employer_guid], end_date, start_date, status, created date
###########################################
employers = read.csv(file.path(BF_DATAFILES,"bf_employer.csv"),header=TRUE,colClasses="character",skip=0,fill=TRUE,strip.white=TRUE,blank.lines=TRUE)
emer = employers[,c(6,12,14,17,7,15,4)] # guid, address_guid, claimed status, claim date, name, type (0-4),created date
emer.extra = employers[,c(6,14,2,3,5,8,9,12,15)]
rm(employers)
###########################################
admin = read.csv(file.path(BF_DATAFILES,"bf_employer_admin.csv"),header=TRUE,colClasses="character",skip=0,fill=TRUE,strip.white=TRUE,blank.lines=TRUE)
admin = admin[,c(3,6,7,2)] # guid, user_guid, employer_guid, created date
###########################################
# fix date fields
cand.created.date <- fix.date.updated(cand$created)# checked
job.posted.date <- fix.date.updated(job$posted)#checked
job.expires.date <- fix.date.updated(job$expires)#checked
job.created.date <- fix.date.updated(job$created)#checked
history.end.date <- fix.date.updated(history$end_date)#checked
history.start.date <- fix.date.updated(history$start_date)#checked
history.created.date <- fix.date.updated(history$created)#checked
emer.claim.date <- fix.date.updated(emer$claim_date)#checked
emer.created.date <- fix.date.updated(emer$created)#checked
admin.created.date <- fix.date.updated(admin$created)#checked
#########################################################
posted.names = c("posted.day","posted.month","posted.year")
expires.names = c("expires.day","expires.month","expires.year")
created.names = c("created.day","created.month","created.year")
# seen.names = c("seen.day","seen.month","seen.year")
updated.names = c("updated.day","updated.month","updated.year")
start.names = c("start.day","start.month","start.year")
end.names = c("end.day","end.month","end.year")
claimed.names = c("claimed.day","claimed.month","claimed.year")
############################
colnames(cand.created.date)<- created.names
# colnames(cand.seen.date)<- seen.names
# colnames(cand.updated.date)<- updated.names
colnames(users.created.date)<- created.names
colnames(job.created.date)<- created.names
colnames(history.created.date)<- created.names
colnames(emer.created.date)<- created.names
colnames(admin.created.date)<- created.names
colnames(job.posted.date)<- posted.names
colnames(job.expires.date)<- expires.names
colnames(history.start.date)<- start.names
colnames(history.end.date)<- end.names
colnames(emer.claim.date)<- claimed.names
#################################################
users.abr = cbind(users.abr[,1:13],users.created.date)
names(users.abr) = c("users.guid","primary_work_history_guid","users.address_guid","email","firstname","lastname","terms_accepted_version","returning_user","about","verification_code","account_state","hs_sso_user","registration_method","user.created.day","user.created.month","user.created.yr")
q <- which(users.abr$user.created.yr == 0)
if (length(q) > 0){
  users.abr <- users.abr[-q,]
}
history = cbind(history[,c(1:3,6)],history.end.date,history.start.date,history.created.date)
names(history) = c("history.guid","history.user_guid","history.employer_guid","history.status","end.day","end.month","end.yr","start.day","start.month","start.yr","crtd.day","crtd.month","crtd.yr")
admin = cbind(admin[,1:3],admin.created.date)
names(admin) = c("admin[guid]","admin_user_guid","admin[employer_guid]","crtd.day","crtd.month","crtd.yr")
job = cbind(job[,c(1:5,8)],job.posted.date,job.expires.date,job.created.date,job[,10:13])
names(job) = c("job guid","job name","job[owner guid]","job[createdby guid]","jobStatus","jobType","posted day","posted month","posted yr","exp day","exp month","exp yr","job crtd day","job crtd month","job crtd yr","budget","j2c_job_reference","remaining_budget","payment_model")
job.extra.fields <- cbind(job.extra.fields,job.posted.date,job.expires.date)
colnames(cand) <- c("cand_guid","user_guid","job_guid","created","seen","seendate","updated")
cand = cbind(cand[,1:3],cand.created.date)#,cand$seen,cand.seen.date,cand.updated.date) # 
##########################
# users.oi is just the opted in (account_state = 0) of all users (of users.abr)
users.oi <- users.abr[which(users.abr$account_state == 0),]
# users.since.aug17 is the subset of users.abr created since Aug 17, 2016
#  optin.users.aug17 is the subset of users.oi created since Aug 17, 2016
users16 <- users.abr[which(users.abr$user.created.yr == 2016),]
users.after.aug<- users16[which(users16$user.created.month > 8),]
users.in.aug <- users16[which(users16$user.created.month == 8),]
users.aug17 <- users.in.aug[which(users.in.aug$user.created.day>= 17),]
users.since.aug17 <-rbind(users.aug17,users.after.aug)
users17 <- users.abr[which(users.abr$user.created.yr == 2017),] # change this to "this.year" after jan 1, 2017
users.since.aug17 <-rbind(users.aug17,users.after.aug,users17)
aug17.ct <- nrow(users.since.aug17)
optin.users.aug17 <- users.since.aug17[which(users.since.aug17$account_state == 0),]
optin.users.since.nov1.1 <- optin.users.aug17[which(optin.users.aug17$user.created.yr >2016),] # keep all
optin.users.since.nov1.2 <- optin.users.aug17[which(optin.users.aug17$user.created.yr == 2016),]
optin.users.since.nov1.3 <- optin.users.since.nov1.2[which(optin.users.since.nov1.2$user.created.month > 10),] # keep all
optin.users.since.nov1 <- rbind(optin.users.since.nov1.1,optin.users.since.nov1.3)
######################################
# auto-provisioned terminateds
term.ar <- users.since.aug17[which(users.since.aug17$registration_method == 11),] # I checked and there aren't any terminateds before 2016
term.ar.oi <- term.ar[which(term.ar$account_state == 0),]

auto.prov.users <- users.since.aug17[which(users.since.aug17$registration_method == 0),]
# the other 3 types of terminated users
term.web.users <- users.since.aug17[which(users.since.aug17$registration_method == 12),]
term.ios.users <- users.since.aug17[which(users.since.aug17$registration_method == 13),]
term.android.users <- users.since.aug17[which(users.since.aug17$registration_method == 14),]
other.term.users <- rbind(term.ios.users,term.android.users,term.web.users)
other.term.users.oi <- other.term.users[which(other.term.users$account_state==0),]
all.term.oi <- rbind(term.ar.oi,other.term.users.oi)
# manually registering from HS app (this is registration_method 15,16,17,18 only)
manual.users1 <- users.abr[which(users.abr$registration_method == 15),] # hs ad ios
manual.users2 <- users.abr[which(users.abr$registration_method == 16),] # hs ad android
manual.users3 <- users.abr[which(users.abr$registration_method == 17),] # hs mobile old client
unk.reg <- users.abr[which(users.abr$registration_method == 18),] # unknown
manual.users <- rbind(manual.users1,manual.users2, manual.users3)
organic.users1 <- users.abr[which(users.abr$registration_method == 2),] # hs creds web
organic.users2 <- users.abr[which(users.abr$registration_method == 3),] # hs creds ios
organic.users3 <- users.abr[which(users.abr$registration_method == 4),] # hs creds android
organic.users4 <- users.abr[which(users.abr$registration_method == 5),] # fb web
organic.users5 <- users.abr[which(users.abr$registration_method == 6),] # fb iod
organic.users6 <- users.abr[which(users.abr$registration_method == 7),] # fb android
organic.users7 <- users.abr[which(users.abr$registration_method == 8),] # manual web
organic.users8 <- users.abr[which(users.abr$registration_method == 9),] # manual ios
organic.users9 <- users.abr[which(users.abr$registration_method == 10),] # manual android
organic.users <- rbind(organic.users1,organic.users2,organic.users3,organic.users4,organic.users5,organic.users6,organic.users7,organic.users8,organic.users9)

# unique posted job candidates
cand.job = merge(cand, job, by.x = "job_guid",by.y = "job guid")
cand.posted = cand.job[which(cand.job$jobType == 0),] # these are posted jobs (job type = 0)
draft.posted = length(which(cand.posted[,23] == 0)) # these are drafts
cand.remove.ind = which(cand.posted[,23] == 0) 
if (length(cand.remove.ind) > 0) cand.posted = cand.posted[-cand.remove.ind,]
u.cand.posted = subset(cand.posted,!duplicated(cand.posted$user_guid))
# unique posted job candidates since Aug 17
cand16 <- cand.posted[which(cand.posted$created.year == 2016),]
cand.after.aug<- cand16[which(cand16$created.month > 8),]
cand.in.aug <- cand16[which(cand16$created.month == 8),]
cand.aug17 <- cand.in.aug[which(cand.in.aug$created.day>= 17),]
cand.since.aug17 <-rbind(cand.aug17,cand.after.aug)
cand17 <- cand.posted[which(cand.posted$created.year == 2017),] # change this to "this.year" after jan 1, 2017
cand.since.aug17 <-rbind(cand.aug17,cand.after.aug,cand17)
cand.aug17.ct <- nrow(cand.since.aug17)
u.cand.since.aug17 <- subset(cand.since.aug17,!duplicated(cand.since.aug17$user_guid))
u.cand.aug17.ct <- nrow(u.cand.since.aug17)
#################################################
# Claimed Stores and Jobs Section
##############################################
claimed.ind <- is.claimed(emer,3) # column index that says "true" or "false" to claimed status
unclaimed.ind <- is.unclaimed(emer,3)
claimed.emer = as.data.frame(cbind(emer[claimed.ind,1],emer[claimed.ind,2],emer[claimed.ind,5],emer[claimed.ind,6],emer.claim.date[claimed.ind,]))
unclaimed.emer = as.data.frame(cbind(emer[unclaimed.ind,1],emer[unclaimed.ind,2],emer[unclaimed.ind,5],emer[unclaimed.ind,6],emer.created.date[unclaimed.ind,]))
names(claimed.emer) = c("guid","address guid","name", "type","claim day","claim month","claim year")
names(unclaimed.emer) = c("guid","address guid","name","type","created day","created month","created year")
claimed.emer.extra = cbind(emer.extra[claimed.ind,],emer.claim.date[claimed.ind,])
##############################################3## 
cl.store.type <- claimed.emer$type
cl.store.type.missing1 <- length(which(cl.store.type == "0"))
cl.store.type.missing2 <- length(which(cl.store.type == ""))
cl.store.type.missing <- sum(cl.store.type.missing1,cl.store.type.missing2)
cl.store.type.qsr <- length(which(cl.store.type == "1"))
cl.store.type.fc <- length(which(cl.store.type == "2"))
cl.store.type.cd <- length(which(cl.store.type == "3"))
cl.store.type.fd <- length(which(cl.store.type == "4"))
cl.store.jobs = merge(job,claimed.emer,by.x = "job[owner guid]",by.y = "guid")
#get rid of drafts before continuing
rr = which(cl.store.jobs[,11] == 0)
if (length(rr) > 0) cl.store.jobs = cl.store.jobs[-rr,]
cl.drafts = length(rr)
qp = which(cl.store.jobs$jobType == 0) # posted jobs only
cl.posted = cl.store.jobs[qp,]
c.cl.posted = unique(cl.posted[,1])
# posted jobs just since Aug 17
colnames(cl.posted) <- c("job[owner guid]","job.guid","job.name","job[createdby guid]","jobStatus","jobType","posted.day","posted.month","posted.yr","exp.day","exp.month","exp.yr","job.crtd.day","job.crtd.month","job.crtd.yr","budget","j2c_job_reference","remaining_budget","payment_model","address.guid","name","type","claim.day","claim.month","claim.year" )
jobs16 <- cl.posted[which(cl.posted$posted.yr == 2016),]
jobs.after.aug<- jobs16[which(jobs16$posted.month > 8),]
jobs.in.aug <- jobs16[which(jobs16$posted.month == 8),]
jobs.aug17 <- jobs.in.aug[which(jobs.in.aug$posted.day>= 17),]
jobs.since.aug17 <-rbind(jobs.aug17,jobs.after.aug)
jobsthisyear <- cl.posted[which(cl.posted$posted.year == this.year),] # change this to "this.year" after jan 1, 2017
jobs.since.aug17 <-rbind(jobs.aug17,jobs.after.aug,jobsthisyear)
jobs.aug17.ct <- nrow(jobs.since.aug17)
u.jobs.since.aug17 <- subset(jobs.since.aug17,!duplicated(jobs.since.aug17$job.guid))
u.jobs.aug17.ct <- nrow(u.jobs.since.aug17) # it's the same as jobs.aug17.ct
##############################################################################
photo = read.csv(file.path(BF_DATAFILES,"bf_photo.csv"),header=TRUE,colClasses="character",skip=0,fill=TRUE,strip.white=TRUE,blank.lines=TRUE)
qu = which(photo$phototype_name =="ProfilePhoto")
profphoto = photo[qu,c(2,5,7,8,10)]
u.profphoto = subset(profphoto,!duplicated(profphoto$owner_guid))# unique user versions of the photo data
q1 = which(photo$phototype_name =="EmployerLogo")
q2 = which(photo$phototype_name =="EmployerAdditionalPhoto")
q3 = which(photo$phototype_name =="GooglePlacesPhoto")
logophoto = photo[q1,c(2,5,7,8,10)]
storephoto = photo[q2,c(2,5,7,8,10)]
googlephoto = photo[q3,c(2,5,7,8,10)]
# unique user versions of the photo data
u.logophoto = subset(logophoto,!duplicated(logophoto$owner_guid))
u.storephoto = subset(storephoto,!duplicated(storephoto$owner_guid))
u.googlephoto = subset(googlephoto,!duplicated(googlephoto$owner_guid))
store.logo = merge(u.logophoto,claimed.emer,by.x = "owner_guid",by.y="guid")
store.photo = merge(u.storephoto,claimed.emer,by.x = "owner_guid",by.y="guid")
google.photo = merge(u.googlephoto,claimed.emer,by.x = "owner_guid",by.y="guid")
st.w.logo = subset(store.logo,!duplicated(store.logo$owner_guid))
st.w.photo = subset(store.photo,!duplicated(store.photo$owner_guid))
st.w.google = subset(google.photo,!duplicated(google.photo$owner_guid))
clst = nrow(claimed.emer) ############################################       number of claimed stores
store.logo.per = round((nrow(st.w.logo)/clst *100),digits=1 )
photostores = rbind(st.w.photo,st.w.google)
u.photostores = subset(photostores,!duplicated(photostores$owner_guid))
store.photo.per = round((nrow(u.photostores)/clst *100),digits=1 )
allstorephotos <- rbind(storephoto,googlephoto)
######################################################################################
# active job calculation
data = cl.store.jobs
q = which(cl.store.jobs[,11] == 0) # get rid of the drafts (no expires date)
if (length(q) >0) data = data[-q,]
qp = which(data$jobType == "0")
active.jobs.data = data[qp,]
active.posted.jobs.data <- active.jobs.data[which(active.jobs.data$jobStatus == "0"),]
##############################################################################
### profile related features ###  
# I only need members, members since Aug 17, candidates, and terminateds.... don't need the active or new users
############################################################################################## 
## photo 
qu = which(photo$phototype_name =="ProfilePhoto")
profphoto = photo[qu,c(2,5,7,8,10)]
u.profphoto = subset(profphoto,!duplicated(profphoto$owner_guid))# unique user versions of the photo data
## find photo counts for each cohort
users.oi.photo = merge(u.profphoto,users.oi,by.x = "owner_guid",by.y="users.guid")
just.photo.users.oi = users.oi.photo[,c(1,4)]
u.just.photo.users.oi = subset(just.photo.users.oi, !duplicated(just.photo.users.oi$owner_guid))
#
oi.aug17.photo = merge(u.profphoto,optin.users.aug17,by.x = "owner_guid",by.y="users.guid")
just.photo.oi.aug17 = oi.aug17.photo[,c(1,4)]
u.just.photo.oi.aug17 = subset(just.photo.oi.aug17, !duplicated(just.photo.oi.aug17$owner_guid))
#
cand.photo = merge(u.profphoto,u.cand.posted,by.x = "owner_guid",by.y="user_guid")
just.photo.cand = cand.photo[,c(1,4)]
u.just.photo.cand = subset(just.photo.cand, !duplicated(just.photo.cand$owner_guid))
#
term.photo = merge(u.profphoto,all.term.oi,by.x = "owner_guid",by.y="users.guid")
just.photo.term = term.photo[,c(1,4)]
u.just.photo.term = subset(just.photo.term, !duplicated(just.photo.term$owner_guid))
######## availability #########
avail = read.csv(file.path(BF_DATAFILES,'bf_user_availability.csv'),header=TRUE,colClasses="character",skip=0,fill=TRUE,strip.white=TRUE,blank.lines=TRUE)
avail.abr = avail[,c(2,13)]
unique.avail = unique(avail$user_guid)
# just having a record in the availability table means the user has filled out this availability
users.oi.avail = merge(users.oi,avail.abr,by.x="users.guid",by.y = "user_guid") # all unique users
just.avail.oi = users.oi.avail[,c(1,17)]
colnames(just.avail.oi) <- c("users.guid","avail.created")
#
oi.aug17.avail = merge(optin.users.aug17,avail.abr,by.x="users.guid",by.y = "user_guid") 
just.avail.oi.aug17 = oi.aug17.avail[,c(1,17)]
colnames(just.avail.oi.aug17) <- c("users.guid","avail.created")
#
cand.avail = merge(u.cand.posted,avail.abr,by="user_guid") 
just.avail.cand = cand.avail[,c(1,17)]
colnames(just.avail.cand) <- c("users.guid","avail.created")
#
term.avail = merge(all.term.oi,avail.abr,by.x="users.guid",by.y = "user_guid") 
just.avail.term = term.avail[,c(1,17)]
colnames(just.avail.term) <- c("users.guid","avail.created")
#########################################
# work history calculations
wh.verif = read.csv(file.path(BF_DATAFILES,"bf_user_workhistory_job.csv"),header=TRUE,colClasses="character",skip=0,fill=TRUE,strip.white=TRUE,blank.lines=TRUE)
wh.ff = read.csv(file.path(BF_DATAFILES,"bf_user_workhistory_freeform.csv"),header=TRUE,colClasses="character",skip=0,fill=TRUE,strip.white=TRUE,blank.lines=TRUE)
wh.users = history[,1:2]
colnames(wh.users) <-c("guid","user_guid")
ff.users = matrix(0,nrow=nrow(wh.ff),ncol=2)
ff.users[,1] = wh.ff$guid
ff.users[,2] = wh.ff$user_guid
colnames(ff.users) <-c("guid","user_guid")
wk.users = rbind(wh.users,ff.users)
wk.users = subset(wk.users, !duplicated(wk.users$user_guid))
#
wh.users.oi = merge(users.oi,wk.users,by.x="users.guid",by.y="user_guid")
u.wh.users.oi <- unique(wh.users.oi$users.guid)
ver.wh.users.oi = merge(wh.users.oi,wh.verif,by.x="primary_work_history_guid",by.y="work_history_guid")
verified.users.oi = ver.wh.users.oi[which(ver.wh.users.oi$src == "1"),]
#
wh.oi.aug17 = merge(optin.users.aug17,wk.users,by.x="users.guid",by.y="user_guid")
u.wh.oi.aug17 <- unique(wh.oi.aug17$users.guid)
ver.wh.oi.aug17 = merge(wh.oi.aug17,wh.verif,by.x="primary_work_history_guid",by.y="work_history_guid")
verified.oi.aug17 = ver.wh.oi.aug17[which(ver.wh.oi.aug17$src == "1"),]
#
wh.cand = merge(u.cand.posted,wk.users,by ="user_guid")
u.wh.cand <- unique(wh.cand$user_guid)
cand.user.merge <- merge(wh.cand,users.abr,by.x="user_guid",by.y ="users.guid") # a small number of these guys have not opted in
u.cand.users <- subset(cand.user.merge,!duplicated(cand.user.merge$user_guid))
ver.wh.cand = merge(u.cand.users,wh.verif,by.x="primary_work_history_guid",by.y="work_history_guid")
verified.cand = ver.wh.cand[which(ver.wh.cand$src == "1"),]
#
wh.term = merge(all.term.oi,wk.users,by.x="users.guid",by.y="user_guid")
u.wh.term <- unique(wh.term$users.guid)
ver.wh.term = merge(wh.term,wh.verif,by.x="primary_work_history_guid",by.y="work_history_guid")
verified.term = ver.wh.term[which(ver.wh.term$src == "1"),]
#########################################
# job preferences (skills)
u.skills <- subset(jobsearch,!duplicated(jobsearch$user_guid))
users.oi.pref <- merge(users.oi,u.skills,by.x="users.guid",by.y="user_guid")
#
oi.aug17.pref <- merge(optin.users.aug17,u.skills,by.x="users.guid",by.y="user_guid")
#
cand.pref <- merge(u.cand.posted,u.skills,by="user_guid")
#
term.pref <- merge(all.term.oi,u.skills,by.x="users.guid",by.y="user_guid")
######## applied to posted job? #########
users.oi.jobs = merge(users.oi,u.cand.posted,by.x="users.guid",by.y = "user_guid") # all unique users
just.jobs.oi = users.oi.jobs[,c(1,17)]
colnames(just.jobs.oi) <- c("users.guid","job_guid")
#
oi.aug17.jobs = merge(optin.users.aug17,u.cand.posted,by.x="users.guid",by.y = "user_guid") 
just.jobs.oi.aug17 = oi.aug17.jobs[,c(1,17)]
colnames(just.jobs.oi.aug17) <- c("users.guid","job_guid")
#
cand.jobs = merge(u.cand.posted,u.cand.posted,by="user_guid") 
just.jobs.cand = cand.jobs[,c(1,17)]
colnames(just.jobs.cand) <- c("users.guid","job_guid")
#
term.jobs = merge(all.term.oi,u.cand.posted,by.x="users.guid",by.y = "user_guid") 
just.jobs.term = term.jobs[,c(1,17)]
colnames(just.jobs.term) <- c("users.guid","job_guid")
#########################################################
referrals.date <- fix.date.updated(referrals$created)
user.nets.date <- fix.date.updated(user.nets$created)
user.employer.net.date <- fix.date.updated(user.employer.net$created)
endorse.employer.date <- fix.date.updated(endorse.employer$created)
endorse.user.date <- fix.date.updated(endorse.user$created)
created.names = c("created.day","created.month","created.year")
colnames(referrals.date) <- created.names
colnames(user.nets.date) <- created.names
colnames(user.employer.net.date) <- created.names
colnames(endorse.employer.date) <- created.names
colnames(endorse.user.date) <- created.names

referrals <- cbind(referrals,referrals.date)
user.nets <- cbind(user.nets,user.nets.date)
endorse.employer <- cbind(endorse.employer,endorse.employer.date)
user.employer.net <- cbind(user.employer.net,user.employer.net.date)
endorse.user <- cbind(endorse.user,endorse.user.date)

user.net.pairs <- nrow(user.nets)
u.user.from <- length(unique(user.nets$from_user_guid))
u.user.to <- length(unique(user.nets$to_user_guid))
num.followings <- nrow(user.employer.net)
num.stores.followed <- length(unique(user.employer.net$employer_guid))
num.users.following <- length(unique(user.employer.net$user_guid))
num.referrals <- nrow(referrals)
u.referred <- length(unique(referrals$referred_user_guid))
u.referring <- length(unique(referrals$referring_user_guid))
num.store.endorse <- nrow(endorse.employer)
num.user.endorse <- nrow(endorse.user)
u.endorsed.user <- length(unique(endorse.user$endorsed_user_guid))
u.endorsing.user <- length(unique(endorse.user$endorsing_user_guid))
recd.endorse <- length(which(endorse.user$received == "TRUE"))
u.endorsed.store <- length(unique(endorse.employer$employer_guid))
u.endorsing.user2store <- length(unique(endorse.employer$user_guid))
#

