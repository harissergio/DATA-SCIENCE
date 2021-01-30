
print("ΣΕΡΓΙΟΥ ΧΑΡΑΛΑΜΠΟΣ
      ΑΕΜ 16394")




#1. Χαρακτηρίζουμε τις μεταβλητές ως προς το είδος του (για όσα σύνολα δεδομένων
#                                                     θα χρησιμοποιήσουμε).

cane <- read.csv("cane.csv")
aids <- read.csv("aids.csv")

#install.packages("dplyr")
library(dplyr)

dim(cane)
glimpse(cane)
summary(cane)
str(cane)
class(cane)

head(cane)
tail(cane)

#αλλαγη τυπου μεταβλητης σε αυτες που χρειαζεται

cane$var<-as.factor(cane$var)
cane$block<-as.factor(cane$block)

cane<-cane[,c(2,3,4,5,6)]

str(cane)

dim(aids)
glimpse(aids)
summary(aids)
str(aids)
class(aids)


head(aids)
tail(aids)

#αλλαγη τυπου μεταβλητης σε αυτες που χρειαζεται

aids$year<-as.factor(aids$year)
aids$quarter<-as.factor(aids$quarter)
aids$dud<-as.factor(aids$dud)

aids<-aids[,c(2,3,4,5,6,7)]
aids
str(aids)

#Για το CANE εχω τις ποσοτικες  n,r,x (:numeric) και τις ποιοτικες var,block (:factor)

#Για το AIDS εχω τις ποσοτικες  delay,time,y (:numeric) και τις ποιοτικες year,quarter,dud (:factor)
 

#2. Κατασκευάζουμε τον πίνακα συχνοτήτων και σχετικών συχνοτήτων για τις ποιοτικές
#μεταβλητές και ένα ραβδόγραμμα ή ένα κυκλικό διάγραμμα.

#Επιλέγω να κάνω τη διαδικασία για τη μεταβλητή block και var του dataset cane

# Πίνακας συχνοτήτων

freq.block.16394 = table(cane$block)
freq.block.16394

freq.block.df.16394 = as.data.frame(freq.block)
freq.block.df.16394 
colnames(freq.block.df.16394)=c("Block","Frequency")
freq.block.df.16394 

freq.var.16394 = table(cane$var)
freq.var.16394
freq.var.df.16394 = as.data.frame(freq.var.16394)
freq.var.df.16394
colnames(freq.var.df.16394)=c("Variety of sugar-cane in each plot","Frequency")
freq.var.df.16394



# Πίνακας σχετικών συχνοτήτων

rel.freq.var.16394 = prop.table(freq.var.16394)
rel.freq.var.16394

rel.freq.var.df.16394 = as.data.frame(rel.freq.var.16394)
rel.freq.var.df.16394
colnames(rel.freq.var.df.16394)=c("Variety of sugar-cane in each plot","Relative_Frequency")
rel.freq.var.df.16394


rel.freq.block.16394 = prop.table(freq.block.16394)
rel.freq.block.16394

rel.freq.block.df.16394 = as.data.frame(rel.freq.block.16394)
rel.freq.block.df.16394
colnames(rel.freq.block.df.16394)=c("Block","Relative_Frequency")
rel.freq.block.df.16394



#Ραβδόγραμμα

bar.freq.block.16394<-
  barplot(
    freq.block.16394, 
    main="Block_Destribution", 
    xlab="Block", 
    ylab="Frequency", 
    horiz=FALSE, 
    cex.names=0.8)

bar.freq.var.16394<-
  barplot(
    freq.var.16394,
    main="Variety of sugar-cane", 
    xlab="Variety", 
    ylab="Frequency",
    horiz=FALSE, 
    cex.names=0.8)

#3. Κατασκευάζουμε Crosstabulation Matrix όπου στις στήλες θα έχει μια ποιοτική
#μεταβλητή και στις γραμμές μια άλλη ποιοτική μεταβλητή των δεδομένων σας.
#Επισης κατασκευάζουμε ένα συγκριτικό ραβδογράμμα. Στην συνέχεια με το chi-squared contingency
#table test εξετάζουμε αν οι δυο μεταβλητές είναι εξαρτημένες ή όχι.Οπτικοποιήουμε το
#αποτέλεσμα του ελέγχου με την gginference.
#Γράφουμε την μηδενική και την εναλλακτική υπόθεση καθώς και το συμπέρασμα του
#παραπάνω ελέγχου.

#Επιλέγω να κάνω τη διαδικασία για τη ποιοτικη μεταβλητή year και quarter του dataset aids


# Πίνακας συχνοτήτων

aids_year.freq.16394 = table(aids$year,aids$quarter)
aids_year.freq.16394



#Συγκρητικο ραβδογραμμα

barplot_year_quarter.16394<-barplot(
  height = t(aids_year.freq.16394),
  beside=FALSE,
  horiz = FALSE,
  col = rainbow(length(levels(aids$quarter))),
  main = "The year of the diagnosis by its quarter",
  xlab = "Year", 
  ylab = "Frequency of Time",
  cex.names = 0.8,
  legend.text = levels(aids$quarter))

chitest_quarter_year.16394 = chisq.test(aids$quarter,aids$year)
chitest_quarter_year.16394

#To pvalue είναι μεγαλύτερο του 0,05 άρα δέχομαι τη μηδενική υπόθεση και συνεπώς οι δύο μεταβλητές μου είναι ανεξάρτητες

# Οπτικός Έλεγχος
install.packages("gginference")
library(gginference)

ggchisqtest.16394<-ggchisqtest(chitest.16394)
ggchisqtest.16394







#4. Υπολογίζουμε για δυο ποσοτικές μεταβλητές τις εξής παραμέτρους: μέση τιμή, διάμεσο,
#τυπική απόκλιση, διασπορά, λοξότητα, κύρτωση, εύρος και ποσοστημόρια. Κάνουμε το
#αντίστοιχο θηκόγραμμα για κάθε μια ξεχωριστά καθώς και ένα σημειογραμμα (scatter plot).

##Επιλέγω να κάνω τη διαδικασία για τη μεταβλητή n και r του dataset cane

#Μέση Τιμή 
mean(cane$n)
mean(cane$r)
#Διάμεσος (Median)
median(cane$n)
median(cane$r)
#Εύρος
range(cane$n)
range(cane$r)
#Εκατοστημόρια
quantile(cane$n)
quantile(cane$r)
#Διακύμανση/διασπορά
var(cane$n)
var(cane$r)
#Τυπική απόκλιση
sd(cane$n)
sd(cane$r)

install.packages("devtools")
library(devtools)
install_github("okgreece/DescriptiveStats.OBeu")
library(DescriptiveStats.OBeu)

#Λοξότητα
ds.skewness(cane$n)
ds.skewness(cane$r)
#Κύρτωση
ds.kurtosis(cane$n)
ds.kurtosis(cane$r)

#Εναλλακτικα
install.packages("psych")
library(psych)
describe(cane$r)
describe(cane$n)

#Θηκογραμμα

bn.16394<-boxplot( 
  x=cane$n,
  main = "Boxplot of the total number of shoots in each plot", 
  col = "blue")
bn.16394

br.16394<-boxplot( 
  x=cane$r,
  main = "Boxplot of the number of diseased shoots", 
  col = "red",
  freq = TRUE)
br.16394


#Συγκρητικο σημειογραμμα-----

plot_r_n.16394<-plot( 
  formula = r~n,
  data = cane,
  main="Total number of shoots by the diseased shoots",
  xlab = "total number of shoots",
  ylab = "the number of diseased shoots")



#Παρατηρούμε ότι και στις δύο μεταβλητές ο δειγματικός μέσος είναι μεγαλύτερος από τη διάμεσο άρα έχουμε θετική ασυμμετρία (λοξότητα).
#Επίσης συντελεστής κύρτωσης στην μεταβλητή n είναι αρκετά μικρότερος από την τιμή 3 συνεπώς η κατανομή είναι πλατυκυρτη ενώ αντίθετα
#στην μεταβλητή r είναι μεγαλύτερος από 3 άρα η κατανομή είναι λεπτοκυρτη.



#5. Υπολογίζουμε για μια ποσοτική μεταβλητή σε κάθε δείγμα που προκύπτει από μια ποιοτική
#(π.χ. age με case=0 και case=1) τις εξής παραμέτρους: μέση τιμή, διάμεσο, τυπική απόκλιση,
#διασπορά, λοξότητα, κύρτωση, εύρος και ποσοστημόρια. Κάνουμε επίσης και τα αντίστοιχο
#συγκριτικό θηκόγραμμα (1 γράφημα με δυο θηκογράμματα).

library(devtools)
install_github("okgreece/DescriptiveStats.OBeu")
library(DescriptiveStats.OBeu)

compare.stats(df=aids,group_var ="quarter",values ="y", m_functions = c("mean","median","sd","var","ds.skewness","ds.kurtosis"))

#Παρατηρώ ότι το εύρος και τα ποσοστημορια δεν μπορώ να το υπολογίσω με το compare stats συνεπώς θα πρέπει να το βρω ξεχωριστά για κάθε περίπτωση

#Ευρος
#1το τριμηνο
range(aids$y[aids$quarter=="1"])
#2ρο τριμηνο
range(aids$y[aids$quarter=="2"])
#3το τριμηνο
range(aids$y[aids$quarter=="3"])
#4το τριμηνο
range(aids$y[aids$quarter=="4"])


#Ποσοστημορια
#1το τριμηνο
quantile(aids$y[aids$quarter=="1"])
#2ρο τριμηνο
quantile(aids$y[aids$quarter=="2"])
#3το τριμηνο
quantile(aids$y[aids$quarter=="3"])
#4το τριμηνο
quantile(aids$y[aids$quarter=="4"])

#συγκριτικό θηκόγραμμα

boxplot_y_n.16394<-boxplot(
  formula = y~quarter,
  data = aids,
  main = "AIDS Cases reported by Quarters of Year",
  ylab = "AIDS Cases", 
  xlab = "Quarter",col = rainbow(length(levels(aids$quarter))))


#Παρατηρούμε ότι και στις 4 περιπτωσεις ο δειγματικός μέσος είναι μεγαλύτερος από τη διάμεσο άρα έχουμε θετική ασυμμετρία (λοξότητα) σε ολες τις περιπτωσεις.
#Επίσης συντελεστής κύρτωσης και στις 4 περιπτωσεις είναι αρκετα μεγαλύτερος από 3 άρα η κατανομή είναι λεπτοκυρτη  σε ολες τις περιπτωσεις.


#6. Ελέγχουμε σε επίπεδο εμπιστοσύνης 95% με την βοήθεια του t-test τη μηδενική υπόθεση
#σύμφωνα με την οποία η μέση τιμή μιας ποσοτικής μεταβλητής δεν διαφέρει μεταξύ των δυο
#group μιας διχοτομικής ποιοτικής (δύο κατηγοριών). Οπτικοποιήουμε το αποτέλεσμα του
#ελέγχου με την gginference.

#Παρατηρούμε ότι δεν μπορούμε να χρησιμοποιήσουμε το data set cane γιατί δεν υπάρχει διχοτομική ποιοτική μεταβλητή. 
#Άρα αναγκαστικά θα πρέπει να χρησιμοποιήσουμε το data set aids με τη διχοτομικη μεταβλητη dud
#Σύμφωνα με την άσκηση υποθέτουμε ότι η ποσοτική μεταβλητή delay ακολουθεί κανονική κατανομή,
#συνεπώς μένει να ελέγξουμε την ανεξαρτησία και την ισοτητας διασπορας των μεταβλητών για να προχωρήσουμε στο t test


aids_df16394<-data.frame(aids)
dud_y_vartest= var.test(x = aids_df16394[aids_df16394$dud == "1", "y"],
                        y = aids_df16394[aids_df16394$dud == "0", "y"],alternative = "two.sided",conf.level= 0.95)
dud_y_vartest


#Το pvalue είναι αρκετα μικροτερο του 0.05 συνεπώς απορριπτω τη μηδενική υπόθεση άρα εχουν ανισες διακυμανσεις


chitest_y_dud.16394 = chisq.test(aids$y,aids$dud)
chitest_quarter_year.16394

#Το pvalue είναι μεγαλύτερο του 0.05 συνεπώς δέχομαι τη μηδενική υπόθεση άρα οι μεταβλητες ειναι ανεξαρτητες


y_dud_test= t.test(
  aids$y ~ aids$dud,
  alternative = "two.sided",
  paired = FALSE,
  var.equal = FALSE, 
  conf.level = 0.95)

y_dud_test 

#Το pvalue είναι αρκετα μικροτερο του 0.05 συνεπώς απορριπτω τη μηδενική υπόθεση (Η0=μ1-μ2=0 )άρα οι μεσες τιμες δεν ειναι ισες 


library(gginference)
ggttest.16394<-ggttest(y_dud_test)
ggttest.16394





#7. Κατασκευάζουμε το σημειόγραμμα (scatterplot) δυο μεταβλητών της επιλογής σας. Υπολογίζουμε τον συντελεστή συσχέτισης του Pearson.


#διαγραμμα διασπορας
#σημειογραμα
  
plot_delay_y.16394<-plot( 
  formula = delay~y,
  data = aids,
  ylab = "Delay",
  xlab = "aids cases")


#Για να εξετάστει αν υπάρχει γραμμική σχέση μεταξύ των τιμών της μεταβλητής delay και y
#θα πρέπει να ελέγξουμε αν υπάρχει η ευθεία ψ = α + β Χ όπου Χ=delay και ψ = y ώστε
#να προβλέπουμε την τιμή της y  (εξαρτημένη) γνωρίζοντας την τιμή της delay (ανεξάρτητη)



#Γραμμικη παλινδρομηση

delay_y_lm.16394 = lm(
  formula = delay~y,
  data = aids)

delay_y_lm.16394 


#delay= 22.9271- 0.2582 y


#ευθεια γραμμικης παλινδρομισης

abline(delay_y_lm.16394,col="red")


summary(delay_y_lm.16394)

#η μεση τιμη = -0.6593 δεν απεχει πολυ απτο 0

# τα σφάλματα που καθορίζουν την ακρίβεια των συντελεστών ειναι (Std. Error t value
#FOR delay          0.50681
#FOR y              0.01833



#Το p value είναι αρκετά μικρότερο του 0,05 και στις δύο περιπτώσεις συνεπώς απορρίπτω την μηδενική υπόθεση
#(Η0= ο συντελεστής είναι ίσος με μηδέν)

# το τυπικό σφάλμα είναι σχετικα μεγαλο, συνεπως η ευθεία παλινδρόμησης δεν δίνει καλή περιγραφή της σχέσης μεταξύ των μεταβλητών
#Residual standard error: 11.09 on 568 degrees of freedom

#Η τιμή του συντελεστή προσδιορισμού είναι αρκετά μακριά στη μονάδα άρα το μοντέλο δεν έχει καλή προσαρμογή στα δεδομένα
#Multiple R-squared:  0.2589




library(nortest)


sintelestis_pearson.16394<-cor(aids$delay,aids$y)
sintelestis_pearson.16394
#Ο συντελεστής συσχέτισης του PEARSON(περνει τιμες απο -1 μεχρι 1) είναι -0.5088282 συνεπώς βλέπουμε ότι υπάρχει αρνητική γραμμική συσχέτιση


#Από τα παραπάνω δεδομένα συμπεραίνουμε ότι εχουμε αρνητικη  γραμμική συσχέτιση οχι τοσο σημαντικη ομως, άρα δεν είναι εφικτό
#να κατασκευάζουμε μοντέλο γραμμικής παλινδρόμησης να προβλέπει μία μεταβλητή δίνοντας την άλλη.

