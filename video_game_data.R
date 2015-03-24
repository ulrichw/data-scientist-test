# R program to read data of the data-scientist pre-test provided by a video game company
# This company used a SQLite database
# This database includes 3 tables
#----------------------------------------------------------------------------------------

require(RSQLite) #SQLite library for R
require(sqldf)   #to convert database tables into R data frames
require(MASS)    #modern applied statistics package
require(plotrix) #for 3D pie charts
#require(poweRlaw)#for power-law distributions

#Open a connection to the database
db <- dbConnect(SQLite(),dbname="ds_prescreen_2014/tasks.sqlite")

#Print the names of the tables into the databases
cat("This database has the following tables: \n")
cat(dbListTables(db),"\n") #names of tables

#Print the fields/attributes for each table
for(i in dbListTables(db)){
  cat("For table",i," the fields are: \n")
  print(dbListFields(db,i))
  cat("\n")
}

#How much revenue was produced on 2013/02/01?
#-----------------------------------------------------------------------------------------

#NB: here I will run SQL queries on the database, but I could also just import the tables
#as R dataframes, using dbReadTable(db,tablename)
res <- dbSendQuery(conn=db,"SELECT sum(cash_amount) FROM transactions WHERE created_time BETWEEN '2013-02-01 00:00:00' AND '2013-02-01 23:59:59.999';")
data <- fetch(res)
cat("Total revenues for 2013/02/01:",data[[1]],"\n") #data[[1]] is the total amount as a double
dbClearResult(res)

# Are there any users who use both iPads and iPhones?
#-----------------------------------------------------------------------------------------

#the following query is actually more generic than just iPad vs iPhone: it tests whether
#there are any users that use two different devices.
#since the answer is no, hen the answer to the initial question is also no
res <- dbSendQuery(conn=db,"SELECT DISTINCT C.account_id FROM (SELECT A.account_id, A.device AS device1, B.device AS device2 FROM account_device AS A, 
account_device AS B WHERE A.account_id = B.account_id) AS C WHERE device1 <> device2;")
#res <- dbSendQuery(conn=db,"SELECT count(account_id) FROM account_device GROUP BY account_id;")
data <- fetch(res)
cat("Number of users using two different device: ",dim(data),"\n")
dbClearResult(res)

#another way to do it:
#res <- sqldf("SELECT * FROM account_device",dbname="ds_prescreen_2014/tasks.sqlite")
#dupl <- duplicated(res$account_id)
#print(any(dupl)) #returns FALSE, so there are no duplicated user account: looks like every user only has one device

#could have done: res <- dbReadTable(db,"account_device")
#any(duplicated(res))

#Which country produces the most revenues?
#-----------------------------------------------------------------------------------------

res <- dbSendQuery(conn=db,"SELECT account.create_country,sum(cash_amount) FROM account, transactions WHERE account.account_id=transactions.account_id
                   GROUP BY account.create_country ORDER BY sum(cash_amount) DESC;")
data <- fetch(res)
cat("The country producing the most revenues is:",data[[1]][1],"\n")
dbClearResult(res)

#What is the iPad/iPhone split in Canada?
#-----------------------------------------------------------------------------------------

res <- dbSendQuery(conn=db,"SELECT count(B.device) FROM account AS A, account_device AS B WHERE 
                   A.account_id = B.account_id AND A.create_country = 'CA' AND B.device LIKE '%iPhone%';")
data1 <- fetch(res)
dbClearResult(res)
res <- dbSendQuery(conn=db,"SELECT count(B.device) FROM account AS A, account_device AS B WHERE 
                   A.account_id = B.account_id AND A.create_country = 'CA' AND B.device LIKE '%iPad%';")
data2 <- fetch(res)
dbClearResult(res)
cat("The iPad/iPhone split in Canada is:",data2[[1]],"iPads, and:",data1[[1]],"iPhones \n")

#What proportion of lifetime revenue is generated on the player's first week in game?
#-----------------------------------------------------------------------------------------

#res <- dbSendQuery(conn=db,"SELECT A.account_id,A.created_date, sum(B.cash_amount) FROM
#                  account AS A, transactions AS B WHERE A.account_id = B.account_id GROUP BY A.account_id;")
#data <- fetch(res)
#dbClearResult(res)
#res <- dbSendQuery(conn=db,"SELECT A.account_id,A.created_date, sum(B.cash_amount) FROM
#                  account AS A, transactions AS B WHERE A.account_id = B.account_id AND julianday(B.created_time) <= julianday(A.created_date) + 7 GROUP BY A.account_id;")
#data2 <- fetch(res)
#dbClearResult(res)

res <- dbSendQuery(conn=db,"WITH AA AS (SELECT A.account_id AS id,A.created_date, sum(B.cash_amount) AS cash FROM
                  account AS A, transactions AS B WHERE A.account_id = B.account_id GROUP BY A.account_id),
                  BB AS (SELECT A.account_id AS id,A.created_date, sum(B.cash_amount) AS cash FROM
                  account AS A, transactions AS B WHERE A.account_id = B.account_id AND julianday(B.created_time) <= julianday(A.created_date) + 7 GROUP BY A.account_id)
                  SELECT avg(BB.cash/AA.cash) FROM AA,BB WHERE AA.id=BB.id;")
data <- fetch(res)
cat("The proportion of lifetime revenue generated during the first week is:",data[[1]]*100.,"percent\n")
dbClearResult(res)

cat ("Press [enter] to continue")
line <- readline()


#A visualization:
#Let's visualize the distribution of total amount of money spent by users
#-----------------------------------------------------------------------------------------

res <- sqldf("SELECT max(session_count) AS max_sessions, sum(cash_amount) AS sum_amount, min(cash_amount) AS min_amount FROM account NATURAL JOIN transactions GROUP BY account_id;",dbname="ds_prescreen_2014/tasks.sqlite")
png('ds_prescreen_2014/histogram2.png',width=4,height=6,units="in",res=300)
hist(res$sum_amount,breaks=100,freq=FALSE,main="pdf of total spending per user",col="darkblue",xlab="Total spending")
dev.off()

# Another visualization: geographic repartition of the players
#-----------------------------------------------------------------------------------------

res <- sqldf("SELECT create_country, count(create_country) AS country_count FROM account GROUP BY create_country;",dbname="ds_prescreen_2014/tasks.sqlite")
thresh <- c(0.1)
selected <- res$country_count >= max(res$country_count)*thresh
rejected <- res$country_count < max(res$country_count)*thresh
country_count <- c(res$country_count[selected],sum(res$country_count[rejected]))
labels <- c(res$create_country[selected],"Other")
perct  <- round(country_count/sum(country_count)*100)
labels <- paste(labels,perct)
labels <- paste(labels,"%",sep="")

#pie chart (only shows countries with a count larger than 10% of the maximum count)
png('ds_prescreen_2014/pie.png',width=4,height=4.5,units="in",res=300)
pie3D(country_count,labels=labels,labelcex=0.5,col=rainbow(length(labels)),explode=0.1,main="Countries of origin of the players")
dev.off()

#Yet another visualisation: average spending per transaction by country
#-----------------------------------------------------------------------------------------

res <- sqldf("SELECT create_country, count(create_country) AS country_count, avg(cash_amount) AS cash_count 
             FROM account NATURAL JOIN transactions GROUP BY create_country ORDER BY count(create_country) DESC;"
             ,dbname="ds_prescreen_2014/tasks.sqlite")
thresh <- c(0.1)
selected <- res$country_count >= 72 #we select countries that had at least 72 transactions
rejected <- res$country_count < 72
labels <- c(res$create_country[selected],"Other")
cash_count <- c(res$cash_count[selected],mean(res$cash_count[rejected]))

png('ds_prescreen_2014/bar.png',width=6,height=4,units="in",res=300)
barplot(cash_count,names.arg=labels,las=2,main="Average spending per transaction per country",col="darkblue")
dev.off()

#Last visualisation: a boxplot of spending per country
#-----------------------------------------------------------------------------------------

res <- sqldf("SELECT cash_amount, create_country FROM account NATURAL JOIN transactions ORDER BY create_country;",dbname="ds_prescreen_2014/tasks.sqlite")
res <- unstack(res) #to have as many columns in the dataframe as there are countries
simple_res <- sapply(res,length) #total number of transations per country
selected <- simple_res >= 72
rejected <- simple_res < 72

res_rej <- stack(res[rejected])
res <- res[selected]
res$Other <- res_rej[[1]]

png('ds_prescreen_2014/boxplot.png',width=6,height=4,units="in",res=300)
boxplot(res,las=2,range=2,varwidth=TRUE,border=c("darkblue","darkgreen"),col="red",main="Box-whiskers plot of amount per transaction \n for countries with more than 50 transactions")
dev.off()

#Let's study the distribution of in-app purchases as a function of time since the user
#created his/her profile
#-----------------------------------------------------------------------------------------

#do a natural join of two tables and import the result as a R data frame
res <- sqldf("SELECT A.account_id, A.created_date,julianday(B.created_time)-julianday(A.created_date) AS days_elapsed,B.cash_amount FROM account AS A, 
             transactions AS B WHERE A.account_id=B.account_id ORDER BY A.account_id;",dbname="ds_prescreen_2014/tasks.sqlite")

png('ds_prescreen_2014/histogram.png',width=4,height=6,units="in",res=300)

#let's look at the distribution of in-app purchases
y<-hist(res$days_elapsed,freq=FALSE,breaks=100,main='pdf of days when a purchase occurs',col='lightblue',
        xlab="Days since the user profile was created")$density
#lines(density(res$days_elapsed,adjust=0.25),col="red",lwd=3)
x<-seq(0.5,length(y)-0.5,1.0) #use the mid-point of the bin
df<-data.frame(x,y)

#fit the histogram by a power law (I could use nls() but with a power law it's easier to take the
# log of x and then do a linear regression)
z <- lm(y~log(x))
lines(x,z$fitted,col="darkblue",lwd=3)
SS<-sum((z$fitted-y)^2)

#print("Fit with poweRlaw")
#mr=conpl$new(res$days_elapsed)
#est=estimate_xmin(mr)
#mr$setXmin(est)

#Or, I can just use the fitdistr() function of MASS to fit an exponential distribution to 
#the days_elapsed!
z<-fitdistr(res$days_elapsed,densfun="exponential")$estimate
y2<-dexp(x,rate=z[1])
lines(x,y2,lwd=3,col="green")
SS2<-sum((y2-y)^2)

#now, let's use the fitdistr() function of the MASS package to fit a lognormal distribution:
z<-fitdistr(res$days_elapsed,densfun="lognormal")$estimate
y3<-dlnorm(x,meanlog=z[1],sdlog=z[2])
lines(x,y3,lwd=3,col="red")
SS3<-sum((y3-y)^2)

legend("topright",c("power-law","exponential","lognormal"),fill=c("darkblue","green","red"))

cat("\n A very basic way to compare the goodness of fit of these 3 distributions: their Sums-of-squares \n")
cat("Sums of squares:",SS,SS2,SS3,"\n")

dev.off()

#Here a basic linear regression...
#-----------------------------------------------------------------------------------------

#do a natural join of two tables and import the result as a R data frame
res <- sqldf("SELECT max(session_count) AS max_sessions, sum(cash_amount) AS sum_amount, 
             min(cash_amount) AS min_amount FROM account NATURAL JOIN transactions 
             GROUP BY account_id;",dbname="ds_prescreen_2014/tasks.sqlite")

#is there a relationship between the involvement of the player (in terms of number of
#game sessions) and the amount of money he/she will spend on the game?
#i.e.: is involvement a good predictor of spending?
png('ds_prescreen_2014/corr.png',width=4,height=4,units="in",res=300)
plot(res$max_sessions,res$sum_amount,xlab="Number of sessions",pch=20,ylab="Total spending",
     col="darkred",main="Total spending per user as a function \n of their number of game sessions")
abline(lm(res$sum_amount ~ res$max_sessions),col="darkgreen",lwd=3)
print(cor(res$max_sessions,res$sum_amount))
text(3000,400,labels="corr=0.0994")
dev.off()

#Let's look at the potential relationship before the time it takes
#for a player to convert (start spending money) and the total amount of money he/she
#will spend on the game
#-----------------------------------------------------------------------------------------

res <- sqldf("SELECT account_id AS id, min(delay) AS delay, sum(cash_amount) AS cash FROM (SELECT account_id,(julianday(created_time)-julianday(created_date)) AS delay,
             cash_amount FROM account NATURAL JOIN transactions) GROUP BY account_id ORDER BY account_id;"
             ,dbname="ds_prescreen_2014/tasks.sqlite")

png('ds_prescreen_2014/histogram3.png',width=4,height=4,units="in",res=300)
plot(res$delay,res$cash,pch=20,xlab="days before conversion",ylab="Total spending",col="red")
print(cor.test(res$delay,res$cash, method="pearson"))
print(cor.test(res$delay,res$cash, method="spearman"))
dev.off()

res2 <- sqldf("SELECT account_id AS id, max(session_count) AS max_sessions, sum(cash_amount) AS cash 
             FROM account NATURAL JOIN transactions 
             GROUP BY account_id ORDER BY account_id;",dbname="ds_prescreen_2014/tasks.sqlite")

spending <- res$cash
spending[res$cash <=50] = 0
spending[res$cash >50] = 1
print(summary(glm(spending ~ res$delay + res2$max_sessions,family="binomial")))

#let's clean the workspace
#rm()
