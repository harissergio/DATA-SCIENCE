
install.packages("DBI")
install.packages("RSQLite")
install.packages("tidyverse")
library(DBI)
library(RSQLite)
library(tidyverse)

# 1. Κανουμε συνδεση στην βάση δεδομένων football-database.sqlite

# ΣΥΝΔΕΣΗ ΣΤΟ SQLite file :football-database.sqlite
football<- DBI::dbConnect(RSQLite::SQLite(), "database.sqlite")
football

#2. Διαβάζουμε όλους τους πίνακες της βάσης και ελέγξτε τη δομή τους

dbReadTable(football,"Country")
dbReadTable(football,"League")
dbReadTable(football,"Player_Attributes")
dbReadTable(football,"Player")
dbReadTable(football,"Team")
dbReadTable(football,"Team_Attributes")
dbReadTable(football,"Match")                     
                    

# ΕΛΕΓΧΟΣ ΔΟΜΗΣ ΠΙΝΑΚΩΝ

str(dbReadTable(football,"Country"))
str(dbReadTable(football,"League"))
str(dbReadTable(football,"Player_Attributes"))
str(dbReadTable(football,"Player"))
str(dbReadTable(football,"Team"))
str(dbReadTable(football,"Team_Attributes"))
str(dbReadTable(football,"Match"))

#3. Χρησιμοποιηουμε την συνάρτηση tbl() για να δημιουργήσουμε pointers για τους 7 πίνακες
#(εκτος τον πίνακα sqlite_sequence) και ακολούθως ελέγχουμε τις διαστάσεις τους. Τα
#ονόματα που θα δώσουμε στα pointers να είναι ίδια με τα ονόματα που έχουνε οι πίνακες
#στη βάση αλλά με μικρά γράμματα (lowecase)

country <- tbl(football, "Country")
country
dim(country)

league <- tbl(football, "League")
league
dim(league)

player_attributes <- tbl(football, "Player_Attributes")
player_attributes
dim(player_attributes)

player <- tbl(football, "Player")
player
dim(player)

team <- tbl(football, "Team")
team
dim(team)

team_attributes <- tbl(football, "Team_Attributes")
team_attributes
dim(team_attributes)

match <- tbl(football, "Match")
match
dim(match)

#4. Ξαναγράφουμε τα ερωτήματα SQL της Άσκησης 1 αλλά αυτή τη φορά με τον τρόπο της dplyr

#a.Βρίσκουμε όλους τους ποδοσφαιριστές με ύψος από 160cm έως 180cm.
#Ο πίνακας που θα προκύψει από το ερώτημα θα έχει 3 στήλες με το όνομα του παίκτη, το ύψος και το βάρος του καθώς.
#Τέλος οι τιμές θα είναι σε φθίνουσα σειρά βάρους.

query1Asql <- dbGetQuery(football, "SELECT player_name, height,weight  FROM Player WHERE height BETWEEN 160 AND 180 ORDER BY weight DESC")

head(query1Asql)

query1A<- player %>%
  filter(height>=160 && height<=180)%>%
  summarize(player_name,height,weight)%>%
  arrange(-weight)
head(query1A)


# b. Για έναν αγώνα της επιλογής μας (επιλέγουμε match_api_id) την ημερομηνία διεξαγωγής του, την γηπεδούχο ομάδα 
#καθώς και τον αριθμό των goal που σκόραρε η γηπεδούχος ομάδα.


query1Bsql <- dbGetQuery(football, "SELECT Match.date,Team.team_long_name AS home_team,
Match.home_team_goal
FROM Match JOIN Team on
Match.home_team_api_id  =Team.team_api_id
WHERE match_api_id=492473")

query1Bsql


query1B<- left_join(match,team,by=c("home_team_api_id"="team_api_id"))%>%
  filter(match_api_id==492473)%>%
  summarize(date,team_long_name,home_team_goal)
query1B




#c. Σε ποια χώρα ανήκει η κάθε ομάδα. Θα χρειαστεί να συνδυάσετε 3 πίνακες για να το κάνετε αυτό.
#Ο τελικός πίνακας θα περιέχει μόνο 2 στήλες (όνομα ομάδας και χώρα) και θα επιστρέφει 296 γραμμές όσες είναι και όλες οι ομάδες.

query1Csql <- dbGetQuery(football, "SELECT DISTINCT Team.team_long_name AS Team,Country.name AS Country 
FROM Team JOIN Country JOIN Match on
Team.team_api_id= Match.home_team_api_id  AND
Country.id=Match.country_id")

tail(query1Csql)

query1C<-left_join(country,match,by=c("id"="country_id"))%>%
  left_join(team,by=c("home_team_api_id"="team_api_id"))%>%
  summarise(name,team_long_name)%>%
  distinct(.keep_all = FALSE)

query1C_df<-as.data.frame(query1C)
colnames(query1C_df)<-c("Country","Team")
tail(query1C_df)



#d. Σαν συνέχεια της διαδικασιας b κάνουμε τη διαδικασια έτσι ώστε να επιστρέφει πίνακα/ πέρα από την ημερομηνία, την γηπεδούχο ομάδα 
#και τα goal που σημείωσε, την αντίπαλο ομάδα καθώς και τα goal που σημείωσε αυτή.

query1Dsql <- dbGetQuery(football, "SELECT 
Match.date,
HTEAM.team_long_name AS home_team,
ATEAM.team_long_name AS away_team,
Match.home_team_goal,Match.away_team_goal
FROM Match
JOIN Team AS HTEAM
JOIN Team AS ATEAM ON
Match.home_team_api_id  =HTEAM.team_api_id AND Match.away_team_api_id =ATEAM.team_api_id
WHERE match_api_id=492473")

query1Dsql

HTEAM<- left_join(match,team,by=c("home_team_api_id"="team_api_id"))%>%
  filter(match_api_id==492473)%>%
  summarize(date,team_long_name,home_team_goal)
HTEAM

ATEAM<- left_join(match,team,by=c("away_team_api_id"="team_api_id"))%>%
  filter(match_api_id==492473)%>%
  summarize(date,team_long_name,away_team_goal)
ATEAM

HTEAM_df<-as.data.frame(HTEAM)
ATEAM_df<-as.data.frame(ATEAM)
query1D<-cbind.data.frame(HTEAM_df,ATEAM_df)
colnames(query1D)<-c("date","home_team","home_team_goal","away_team","away_team_goal")

query1D


#5.
#a. Βρισκουμε πόσοι παίκτες είναι αριστεροπόδαροι και πόσοι δεξιοπόδαροι (Θα χρησιμοποιήσουμε την στήλη preferred_foot 
#του πίνακα player_attributes καθώς και τις συναρτήσεις group_by και summarize)


left_right_foot<- player_attributes %>%
  group_by(preferred_foot) %>%
  summarize(paratirisis=n())
left_right_foot

#ΣΥΝΕΠΩΣ ΠΑΡΑΤΗΡΟΥΜΕ ΟΤΙ ΟΙ ΑΡΙΣΤΕΡΟΠΟΔΑΡΟΙ ΕΙΝΑΙ 44733 ΕΝΩ ΟΙ ΔΕΞΙΟΠΟΔΑΡΟΙ ΕΙΝΑΙ 138409.


#b. Χρησιμοποιήουμε τη βιβλιοθήκη ggplot2 και κατασκευάζουμε ένα απλό
#ραβδόγραμμα για τη μεταβλητή preferred_foot του πίνακα player_attributes

install.packages("ggplot2")
library(ggplot2)


#Ραβδόγραμμα


preferred_footbar<-ggplot(player_attributes,aes(x=preferred_foot))+
            geom_bar()
  

  
preferred_footbar




#6.
#a. Με χρήση της ggplot2 κάνουμε ένα διάγραμμα διασποράς για τις μεταβλητές ύψος
#(height) και βάρος (weight) των παικτών (πίνακας player)

weight_height_a<-player %>%
  group_by(height,weight) %>%
  collect() %>%
  ggplot(aes(x = height, y = weight))+
  geom_point()

weight_height

#b. Χρησιμοποιήουμε το geom_jitter για να βελτιώσουμε το παραπάνω γράφημα και
#αλλάζουμε την διαφάνεια (alpha) έτσι ώστε να αποφύγουμε το overplotting

weight_height_b<-player %>%
  group_by(height,weight) %>%
  collect() %>%
  ggplot(aes(x = height, y = weight))+
  geom_point()+
  geom_jitter(alpha=0.08)

weight_height_b



#7. Ο ακόλουθος κώδικας υπολογίζει τους βαθμούς που πήρε κάθε ομάδα σε κάθε αγώνα
#και τους προσθέτει στο τέλος του πίνακα match:
 
 match_points <- match %>%
  mutate(home_team_points = if_else((home_team_goal > away_team_goal), 3,
                                    if_else((home_team_goal == away_team_goal), 1, 0))) %>%
  mutate(away_team_points = if_else((home_team_goal > away_team_goal), 0,
                                    if_else((home_team_goal == away_team_goal), 1, 3)))
 match_points
  
  
#a. Χρησιμοποιήουμε τον πίνακα match_points που ορίζεται παραπάνω και
#υπολογίζουμε τον μέσο όρο βαθμών που είχε η ομάδα εντός έδρας ανά παιχνίδι
#αποθηκεύοντας το σε μια μεταβλητή με το όνομα home_points. (Θα περιέχονται
#μόνο οι στήλες league_id, team_api_id, και η στήλη με το μέσο όρο βαθμών εντός
#έδρας).

home_points<- match_points %>%
  group_by(team_api_id=home_team_api_id)%>%
summarize(league_id,mean_home_team_points = mean(home_team_points))            

head(home_points)
 
#b. Χρησιμοποιήουμε τον πίνακα match_points που ορίζεται παραπάνω και
#υπολογίζουμε τον μέσο όρο βαθμών που είχε η ομάδα εκτός έδρας ανά παιχνίδι
#αποθηκεύοντας το σε μια μεταβλητή με το όνομα away_points. (Θα περιέχονται
#μόνο οι στήλες league_id, team_api_id, και η στήλη με το μέσο όρο βαθμών εκτός
#έδρας)

away_points<- match_points %>%
  group_by(team_api_id=away_team_api_id)%>%
  summarize(league_id,mean_away_team_points = mean(away_team_points))            

head(away_points)
 
 
#c. Ενώνουμε τους δύο πίνακες home_points και away_points σε έναν πίνακα

both_points<-left_join(home_points,away_points)
both_points


#d. Χρησιμοποιήουμε τη βιβλιοθήκη ggplot2 και κάνουμε ένα διάγραμμα διασποράς του
#μέσου όρου των βαθμών που μαζεύουν οι ομάδες εντός έδρας με το μέσο όρο
#βαθμών που μαζεύουν εκτός έδρας. Χρησιμοποιήουμε και το
#geom_smooth(method = lm) για να δουμε καλύτερα την γραμμική σχέση.

  both_points_gg<-ggplot(both_points,aes(x = mean_away_team_points, y = mean_home_team_points))+
  geom_jitter(alpha=0.4)+
  geom_smooth(method = lm)  
  both_points_gg

#ΕΙΝΑΙ ΕΜΦΑΝΕΣ ΟΤΙ ΟΙ ΔΥΟ ΣΤΗΛΕΣ ΕΧΟΥΝ ΜΕΓΑΛΗ ΓΡΑΜΙΚΗ ΣΥΣΧΕΤΗΣΗ ΜΕΤΑΞΥ ΤΟΥΣ.ΑΥΤΟ ΓΙΝΕΤΑΙ ΑΝΤΙΛΕΙΠΤΟ
#ΓΡΑΜΜΙΚΗΣ ΠΑΛΙΝΔΡΟΜΗΣΗΣ

#ΟΛΟΚΛΗΡΩΝΟΝΤΑΣ ΚΑΝΩ ΑΠΟΣΥΝΔΕΣΗ ΑΠΟ ΤΗ ΒΑΣΗ
dbDisconnect(football)
