vscode2

datal2 <- read.csv('C:\\Users\\IBM_ADMIN\\Desktop\\R\\UDA\\stateData.csv')
fb <- read.csv('C:\\Users\\IBM_ADMIN\\Desktop\\R\\UDA\\pseudo_facebook.tsv', sep = '\t')
nci <- read.table('C:\\Users\\IBM_ADMIN\\Desktop\\R\\UDA\\nci.tsv')
load('C:\\Users\\IBM_ADMIN\\Desktop\\R\\UDA\\BigDiamonds.RDa') # load saved dataframe

library(readxl)
affected <- read_excel('C:\\Users\\IBM_ADMIN\\Desktop\\R\\UDA\\indicator_air_accident_affected.xlsx', sheet = 1, col_names = FALSE)

datal2[datal2['state.region'] == 1, 'X']
d1 = datal2[datal2['state.region'] == 1, ]
d2 = subset(datal2, state.region == 1)
length(subset(datal2, state.region == 1))
subset(diamonds, diamonds$price < 500)
subset(diamonds, diamonds$color == 'D')$price # len stlpec price
length(datal2[datal2['state.region'] == 1, ]) # length rozdielny (asi matica x vector)
length(datal2[datal2['state.region'] == 1, 'X'])

class(ld['LoanStatus'])
"data.frame"
class(ld$LoanStatus)
"factor"

head(d1, n = 2)
head(d2, n = 2)
tail(d1, n = 2)
tail(d2, n = 2)
dim(d1)
dim(d2)
nrow(x)
ncol(x)
names(datal2) # columns names
t() # transpose
deparse(substitute(x)) # variable name as string
x[[4]][[3]][[2]] # something in list, which is in list in list

table(reddit$marital.status) # hodnoty a pocet faktora
levels(reddit$marital.status) # hodnoty faktora
View(reddit) # otvori sa dataframe v novom okne
unique(reddit$age.range) # len unique hodnoty

mean(datal2['income'][datal2['income']>0])
mean(datal2['income'][datal2['income']!=0])
mean(datal2['income'][datal2['income']<Inf])


# ostane DF

datal2['population']
datal2[5]

datal2[datal2['income'] > 0, c('X', 'state.abb', 'income')]
pf[!is.na(pf['gender']),] # bez NA hodnot v gender stlpci
datal2[c(1,2),c(1,2)]
datal2[1:10, 10:12]

filter(birthdays, week_day == 'Friday')

x = subset(datal2, income > 5000, select = c('X', 'income'))
x = subset(datal2, income > 5000, select = c(X, income))
x = subset(datal2, income > 5000, select = X:income)

datal2[, c(-1, -3)]  # okrem stlpcov 1 a 3
datal2[, !(names(datal2) %in% c('X', 'state.abb', 'state.region', 'income'))] # vsetky stlpce okrem tychto
# alebo colnames()
d1[names(d1) == c('frost', 'area')] # ak su pri sebe vrati obe, ak nie, vrati posledny, inak pouzit %in%


names(x) = c('jedna', 'dva') # zmeni, ostatne nazvy stlpcov da NA, aj vektor, aj DF
colnames(x) = c('jedna', 'dva') # len DF
names(x)[2]

# bude vektor - faktor, pri cislach bude vektor integer/numeric

datal2[datal2['income'] > 0, 'X']
datal2[, 'state.abb']

# bude vektor - vsetky zvolene stlpce hodi do jedneho vektora

datal2[,5]
datal2['income'][1,1]
datal2['income'][1,]
datal2['income'][0:2,]
datal2['income'][,1]
datal2['income'][(datal2['income']>4000) & (datal2['income']<5000)]
x[1:2][x['income']>5200]

# data type change

high_income = as.factor(high_income)
xx = as.integer(xx)
xx = as.character(xx)
xx = as.numeric(xx)

  # apply function to multiple columns

affected_t[, 2:ncol(affected_t)] <- sapply(affected_t[, 2:ncol(affected_t)], as.character)
affected_t[, 2:ncol(affected_t)] <- sapply(affected_t[, 2:ncol(affected_t)], as.integer) # viacero stlpcov, najprv do char, potom do int treba
affected_t1 <- as.integer(as.character(affected_t[, 2])) # jeden stlpec z z faktora na integer

# sampling

sample(100, size = 10, replace = TRUE)
plot(rexp(1000))
plot(rnorm(1000))
hist(rnorm(1000))
hist(rexp(1000))
hist(runif(1000))

# column rename

colnames(d1)[1] = 'country'
colnames(d1)[names(d1) == 'peksa'] = 'mozno'
colnames(df)[colnames(df) == v] <- 'var'

# column reorder

group_default <- group_default[,c(10, 1:9)]

# filling

# deriving

d1['test'] <- d1['area']/d1['income']
unite(birthdays, 'dq', week_day, quarter, sep = '-', remove = F) # novy stlpec, spoji stlpce do jedneho
separate(birthdays, week_day, c('t1', 't2'), sep = 'd', remove = F) # nove stlpce z jedneho podla separatora?
yo <- transform(yo, all.purchases = strawberry + blueberry + pina.colada + plain + mixed.berry) # novy stlpec all.purchases ako sucet

# drop column

d1['state.region'] <- NULL
mtcars <- subset(mtcars, select = -year)

# conditional element selection

cond <- mtcars$wt > 3.5
mtcars$weight_class <- ifelse(cond, 'heavy', mtcars$weight_class) # condition, true, false

# aggregate

test1 <- subset(test, !is.na(test$Population))  %>% 
  group_by(Year) %>% 
  summarize(all_pop = sum(Population))
fbs <- fb %>% 
  group_by(age) %>% 
  summarize(age_mean = mean(friend_count), 
            age_median = median(friend_count), 
            count = n())
pf.fc_by_age_gender <- fb[!is.na(fb$gender),] %>% # alebo filter(fb, !is.na(gender))
  group_by(age, gender) %>% 
  summarize(mean_friend_count = mean(friend_count), median_friend_count = median(friend_count), n = n()) %>% 
  ungroup() %>% 
  arrange(age, gender)

t <- ld %>% group_by(MemberKey) %>% 
  summarize(nr_of_loans = n(), 
            minimum_apr = min(BorrowerAPR), 
            if_test = ifelse(min(BorrowerAPR) <=0.2, 1,ifelse(min(BorrowerAPR) >= 0.3, 3, 2))) %>% 
  mutate(times = if_test * minimum_apr) %>% 
  mutate(lead_test = lead(times,2)) %>% mutate(lag_test = lag(times, 2)) %>% 
  mutate(d = dense_rank(times)) %>% 
  mutate(ind = row_number(times)) %>% 
  mutate(nt = ntile(times, 5)) %>% 
  mutate(ca = cummean(times))

crime_ag <- aggregate(CrimeCount ~ Borough, FUN = sum, data = crime_theft)

# indexing

# sorting

month_count <- month_count[order(month_count$n, decreasing = T),] # descending
fbs <- arrange(fbs, age) # ascending
fbs <- arrange(fbs, desc(age)) # descending

# rescaling

ld$rescl <- rescale(ld$BorrowerAPR)

# sql

library(sqldf)
sqldf()
sqldf('select * from d1 where income > 5000')

# plotting

  # density

plot(density(d1[, 'income'])) # kernel density function graf

smoothScatter(ld$BorrowerAPR, ld$CreditScoreRangeLower, 
              colramp = colorRampPalette(c("white", 'blue', 'yellow',"orange", "red", 'darkred')))

smoothScatter(lb_before_repDate$AmountDelinquent_principal_ratio, lb_before_repDate$should_pay_toMonth, 
              colramp = colorRampPalette(brewer.pal(9, 'Reds')), xlab = 'EstimatedLoss', ylab = 'should_pay_toMonth')

ggplot(aes(x = v1, y = v2), data = bframe) +
  stat_density2d(aes(fill = ..level..), geom = 'polygon') +
  scale_fill_gradientn(colours = colorRampPalette(c('blue', 'yellow',"orange", "red", 'darkred'))(100))

  # ggplot2

library(ggplot2)
ggsave('priceHistogram.png')

    # histograms

qplot(reddit$age.range)
ggplot(pf, aes(x = pf$friend_count)) + geom_histogram(binwidth = 1.5)
ggplot(aes(x = tenure/365), data = pf) + 
  geom_histogram(binwidth = .25, color = 'black', fill = '#F79420') +
  xlim(0,7)
ggplot(aes(x = friend_count), data = pf) + geom_histogram(binwidth = 25) + 
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0,1000, 50))
ggplot(aes(x = tenure/365), data = pf) + 
  geom_histogram(binwidth = .25, color = 'black', fill = '#F79420') + 
  scale_x_continuous(limits = c(0, 7)) + xlab('Number of years using FB') + 
  ylab('Occurencies') + ggtitle('Years usage of FB by users') + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))
qplot(x = friend_count, data = pf, xlim = c(0,1000))
      
      # scale

qplot(x = friend_count, data = pf) + scale_x_continuous(limits = c(0,500))
ch3 <- ggplot(aes(x = friend_count), data = pf) + 
  geom_histogram() + 
  scale_x_sqrt() +
  xlab('friend_count_sqrt') + 
  ggtitle('Friend count sqrt histogram')
ggplot(aes(x = www_likes), data = subset(pf, !is.na(pf$gender))) + 
  geom_freqpoly(aes(color = gender)) + scale_x_log10()

      # faceting

ggplot(aes(x = friend_count), data = pf) + geom_histogram() + scale_x_continuous(limits = c(0, 1000)) + 
  facet_grid(. ~ gender)
ggplot(aes(x = friend_count), data = pf) + geom_histogram() + scale_x_continuous(limits = c(0, 1000)) + 
  facet_wrap(~ gender)
qplot(x = price, data = diamonds) + facet_wrap(~cut, scales = 'free')
ggplot(aes(x = time, y = price), data = subset(yo, id %in% sampled_ids)) + 
  geom_line() + 
  facet_wrap(~ id) + 
  geom_point(aes(size = n), pch = 1) + # pch - druh bodu
  scale_x_continuous(breaks = seq(9800, 10400, 300))

      # stats

ggplot(aes(x = friend_count, y = ..count../sum(..count..)), data = pf) + 
  geom_freqpoly(aes(color = gender))
qplot(x = friend_count, data = pf, geom = 'freqpoly', colour = gender)
ggplot(aes(x = age, y = friendships_initiated), data = subset(fb, friendships_initiated > 0)) + 
  geom_jitter(alpha = 1/30, color = '#F79420') +
  coord_trans(y = 'log10') + 
  annotation_logticks(scaled = F, sides = 'l') + 
  expand_limits(x = 0) + 
  geom_line(stat = 'summary', fun.y = mean) + 
  geom_line(stat = 'summary', fun.y = quantile, probs = .1, linetype = 2, color = 'blue') + 
  geom_line(stat = 'summary', fun.y = quantile, probs = .9, linetype = 2, color = 'blue') # neukaze legendu
ggplot(aes(x = age, y = friendships_initiated), data = subset(fb, friendships_initiated > 0)) + 
  geom_jitter(alpha = 1/30, color = '#F79420') +
  coord_trans(y = 'log10') + 
  annotation_logticks(scaled = F, sides = 'l') + 
  expand_limits(x = 0) + 
  stat_summary(aes(color = 'mean'), fun.y = mean, geom = 'line') + 
  stat_summary(aes(color = 'median'), fun.y = median, geom = 'line') + 
  stat_summary(aes(color = '1quant'), fun.y = quantile, probs = .1, geom = 'line', linetype = 2) + 
  stat_summary(aes(color = '9quant'), fun.y = quantile, probs = .9, geom = 'line', linetype = 2) + 
  scale_color_manual(values = c('black', 'black', 'blue', 'red'), name = 'Legend') # ukaze aj legendu

      # geom_quantile

ggplot(aes(x = www_likes_received, y = likes_received), data = subset(fb, likes_received > 0 & www_likes_received > 0)) + 
  geom_point(color = 'red', alpha = 1/50) + 
  coord_trans(xtrans = 'log10', ytrans = 'log10') + 
  annotation_logticks(scaled = F) + 
  geom_quantile(quantiles = c(.1, .5, .9)) + 
  theme_bw()

      # coord_trans

ggplot(aes(x = age, y = friend_count), data = subset(fb, fb$friend_count > 0)) + 
  xlim(13,90) + 
  geom_jitter(alpha = 1/20) + 
  coord_trans(y = 'sqrt')

      # geom_smooth

ggplot(aes(x = www_likes_received, y = likes_received), data = subset(fb, likes_received > 0 & www_likes_received > 0))  + 
  geom_jitter(alpha = 1/50) + 
  ylim(0, quantile(fb$likes_received, probs = .95)) + 
  xlim(0, quantile(fb$www_likes_received, probs = .95)) + 
  geom_smooth(method = 'lm', color = 'red') + 
  theme_bw() # spravi regresnu ciaru do scatter plotu

ggplot(aes(x = 30*(round(tenure/30)), y = friendships_initiated/tenure), data = fb) + 
  geom_line(aes(color = year_joined.bucket), stat = 'summary', fun.y = 'mean') # manual smoothin x without geom_smooth

ggplot(aes(x = tenure, y = friendships_initiated/tenure), data = subset(fb, tenure >=1)) + 
  geom_smooth(aes(color = year_joined.bucket)) # alternativa plotu predtymto, default smoothing

    # frequency polygons

ggplot(aes(x = friend_count), data = pf) + geom_freqpoly(aes(color = gender))
qplot(x = friend_count, data = pf, geom = 'freqpoly', colour = gender)

    # boxplots

ggplot(aes(x = gender, y = friend_count), 
       data = subset(pf, !is.na(pf$gender) & (pf$friend_count > 0 & pf$friend_count < 1000))) + 
  geom_boxplot() # alebo namiesto subset 0 - 1000 pouzit ylim(0, 1000), 
                  # alebo scale_y_continuous(limits = c(0, 1000)),
                  # alebo spravne + coord_cartesian(ylim = c(0, 1000), to zblizi, neodfiltruje
ggplot(aes(x = clarity, y = price, fill = clarity), data = diamonds) + 
  geom_boxplot() + 
  stat_summary(fun.y = mean, geom = 'point', shape = 1, size = 4) # priemer ako maly kruzok
ggplot(aes(x = Year, y = Population), data = subset(test1, test1$Year >= '1980-01-01')) + 
  geom_boxplot(aes(group = Year)) # grupne to podla Year, tzn. rovnaky Year bude v jednom boxplote

        # boxplot median ordering
ggplot(aes(x = reorder(LastLoanStatus, pay_current, FUN=median), y = pay_current), 
       data = subset(lb_before_repDate, pay_current < 3)) + 
  geom_boxplot()

    # barcharts

ggplot(aes(x = color), data = diamonds) + stat_bin(aes(x = color, y = ..count.., fill = color))

ggplot(aes(x = interaction(ORG_NAME, YEARMONTH), y = m, fill = GEO_NAME), data = df_util_test) + 
  geom_bar(stat = 'identity', position = 'dodge') + 
  annotate('text', 
           x = 1:length(unique(interaction(df_util_test$ORG_NAME, df_util_test$YEARMONTH))), 
           y = - 0.02, 
           label = rep(unique(df_util_test$ORG_NAME), length(unique(df_util_test$YEARMONTH)))) + 
  annotate('text', 
           x = (1:(length(unique(df_util_test$YEARMONTH)))) * length(unique(df_util_test$ORG_NAME)) - length(unique(df_util_test$ORG_NAME))/2 + 0.5 , 
           y = - 0.07, 
           label = unique(df_util_test$YEARMONTH)) # annotation dynamically changing

    # violin plots

ggplot(aes(x = color, y = price, fill = color), data = diamonds) + 
  geom_violin() + 
  stat_summary(fun.y = mean, geom = 'point', shape = 1, size = 2) + 
  stat_summary(fun.y = median, geom = 'point', shape = 4, size = 2)

    # dotplots

ggplot(aes(x = color, y = price), data = diamonds) + 
  geom_dotplot(stackdir = 'center', binaxis = 'y', binwidth = 20)

    # line charts

ggplot(aes(x = as.numeric(as.character(Year)), y = all_pop), data = test1) + 
  geom_point() + 
  geom_line()

    # vertical or horizontal lines

ggplot(aes(x = www_likes_received, y = likes_received), data = fb) + 
  geom_point(alpha = 1/20) + 
  coord_cartesian(ylim = c(0, 10000), xlim = c(0,10000)) + 
  geom_hline(yintercept = mean(fb$likes_received)) # ciara ako priemer

    # heatmaps

ggplot(aes(x = variable, y = gene, fill = value), data = test) + 
  geom_tile() + 
  scale_fill_gradientn(colours = colorRampPalette(c('blue', 'red'))(100))

# custom axis transformations

cuberoot_trans = function() trans_new('cuberoot', 
                                      transform = function(x) x^(1/3),
                                      inverse = function(x) x^3)

ggplot(aes(x = carat, y = price), data = diamonds) + 
  geom_point(position = 'jitter', alpha = 1/2, size = 3/4) + 
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2,3), breaks = c(0.2, 0.5, 1, 2, 3)) + # pri oboch osiach transformuje koordinaty, nie premennu
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000), breaks = c(350, 1000, 5000, 10000, 15000)) + 
  ggtitle('Price (log10) by cube root of carat')

# arrange plots to grid

library(gridExtra)
grid.arrange(ch1, ch2, ch3, ncol = 2)

# plot matrix

library(GGally)
ggpairs(fb_subset[sample.int(nrow(fb_subset), 1000),])

# legend guide, overriding aes, color brewer

ggplot(aes(x = carat, y = price, colour = clarity), data = diamonds) + 
  geom_point(alpha = 0.5, size = 1, position = 'jitter') +
  scale_color_brewer(type = 'div', palette = 9, guide = guide_legend(title = 'CLARITY', keywidth = 5, override.aes = list(size = 5, alpha = 1))) +  
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3), breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000), breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat and Clarity')

# color palettes

colorRampPalette(brewer.pal(name = 'RdYlGn', n = 12))(max(df$exp_bill))[df$exp_bill] # vytvori novy stlpec v DF s hex kodmi farieb podla zvolenej palety

display.brewer.all() # ukaze vsetky sety paliet v colorbreweri

# dates

format(strptime(population_tr$Year, '%Y'), '%y') # zmeni factor stlpec na date, a potom date zmeni na len '71', co uz bude character
test$Year <- strptime(as.character(test$Year), '%Y') # zmena z faktora Year na date so vstupnym formatom '%Y'
birthdays$dates <- as.Date(birthdays$date, format = '%m/%d/%y')

julian(birthdays$dates, origin = as.Date('2014-01-01')) # cislo dna od zaciatku roku 2014
birthdays$weekend <- is.weekend(birthdays$dates)
strftime(birthdays$dates, format = '%W') # week number

df_test$YEARMONTH2<- strptime(df_test$YEARMONTH, format = "%Y-%m-%d") # zo stringu "2014-02-28" na posixlt "2014-02-28"
str(unclass(df_test$YEARMONTH2)) # uvidim vsetky elementy (sec, min, hour, ...)

  # timeseries

df_test_spread_xts <- xts(x = df_test_spread[, -1], 
                          order.by = as.POSIXct(strptime(as.character(df_test_spread$YEARMONTH), 
                                                         format = "%Y-%m-%d")))


# other

summary(d1)
str(d1) # structure
by(pf, pf$gender, summary) # applies some function to values of some factor
by(diamonds$price, diamonds$cut, summary, digits = 10) # vysledky funkcii rozdielne, treba nastavit digits

IQR(subset(diamonds, diamonds$color == 'D')$price)
mean(d1[,names(d1) == 'income'])
mean(subset(temp1, EmploymentStatus == 'Self-employed')$should_pay_toMonth_diff) # $ spravi z toho vektor, inak error

with(subset(fb, tenure > 1), summary(friend_count/tenure)) # funkcia na subsete
with(fb, cor.test(fb$age, fb$friend_count, method = 'pearson')) # korelacie, pearson linearne vztahy, spearman monotonne
cor.test(fb$age, fb$friend_count, method = 'pearson')
cor(fb$age, fb$friend_count)

data() # all example datasets
data('women', package = 'datasets') # loads dataset
edit() # innvoke text editor

cbind(obj, obj, ...) # combines objects as columns
rbind(obj, obj, ...) # combines objects as rows
ls() # lists current objects
rm(obj, obj,...) # remove objects

floor(3.77)
ceiling(3.77)

fb$year_joined.bucket <- cut(fb$year_joined, c(2004, 2009, 2011, 2012, 2014)) # konvertuje z numeric na
            # faktor a zaroven ich rozbinuje podla hranic

scale(lb_before_repDate[, c("should_pay_toMonth_diff_relative")]) # normalizacia 0 priemer, 1 stand. odchylka
# http://stackoverflow.com/questions/15215457/standardize-data-columns-in-r

##### data wrangling

# cheatsheet
library(dplyr)
library(tidyr)

# reshape

gather(test, Year, Population) # z tabularneho do transakcneho formatu, Year - key, Population - vytvoreny stlpec
gather(test, 'variable', 'value', 1:10) # zo setu test zobere premenne zo stlpcov (v1 - v10), ich hodnoty
                          # hodi do jedneho, premenna v stlpci 11 bola cislo riadka, ta tam ostane
subset(pf.fc_by_age_gender[c('age', 'gender', 'median_friend_count')], !is.na(gender)) %>% 
  spread(gender, median_friend_count) %>% 
  mutate(ratio = male / female) # zaroven to aj grupne (odstrani riadky s NA) a median_friend_count da sum
                                # a vytvori novu premennu ratio

spread(df_test, key = GEO_NAME, value = g_mean) # vyjebany Error: All columns must be named - ak su v key stlpci NA

###########################################

library(Lahman) # baseball datasets
library(nycflights13) # cache nycflights13 db to local sqlite
library(alr3)

# DB connections

library(ibmdbR) # in database mining
library(RJDBC)

library(RODBC)

odbcDataSources() # lists available odbc data sources
con1 <- odbcConnect(dsn = 'xxx', uid = 'yyy', pwd = 'pass')
sqlTypeInfo(con1) # lists available datatypes
sqlTables(con1, schema = 'TTT', tableType = 'TABLE') # lists all tables in schema timetrac
sqlColumns(con1, 'TTT.ASSIGNMENT') # info about columns of a table
db_test <- sqlFetch(con1, 'SPSS.HANDSHAKE_INPUT') # read table to data frame
query_test <- sqlQuery(con1, 'SELECT * FROM SPSS.HANDSHAKE_INPUT WHERE ID_GEO = 1') # sql query
odbcCloseAll() # close all odbc connections

# regression

library('memisc')
library('lattice') # grafy
library('car')
library('scales')

m1 <- lm(I(log(price)) ~ I(carat^(1/3)), data = diamonds)
m2 <- update(m1, ~ . + carat)

m2 # see regression formula and model coefficients
summary(m5)
anova(m5) # analysis of variance
plot(m5) # residuals analysis 4 plots

fitted(m1)
coef(m1)
residuals(m1)
names(m1) # what it produces
est <- predict(m5, newdata = test_for_pred, interval = 'prediction', level = .95) # 

mtable(m1,m2, m3, m4, m5) # comparative table of models


# big diamonds dataset

library(RCurl) # http requests
library(bitops)
library(funModeling) http://www.r-bloggers.com/package-funmodeling-data-cleaning-importance-variable-analysis-and-model-perfomance/?utm_source=feedburner&utm_medium=feed&utm_campaign=Feed%3A+RBloggers+%28R+bloggers%29&utm_content=FaceBook
library(installr) http://www.r-statistics.com/2015/06/a-step-by-step-screenshots-tutorial-for-upgrading-r-on-windows/


####### Shiny

runGist(6571951) # spustenie shiny apky z githubu
runGitHub('GoogleAnalytics', 'ChrisBeeley')
runApp('C:/Users/IBM_ADMIN/Desktop/R/ShinyApps/GoogleAnalytics-master')

cat(file=stderr(), "selected grouping - ", input$grouping) # vypise do konzoly

library(shinyBS) - tooltipy a picovinky jendotlivo na objekty v UI?
library(rpivotTable) - interaktivna pivot tabulka
library(xtable) - brutalne moznosti formatovania tabulky
library(googleVis) - googlecharts
library(gmodels) - funkcia CrossTable, spravi akoze crosstab s row, column, total summary, ale je to asi picovina

timeseries

library(dygraphs) http://stackoverflow.com/questions/18261160/convert-date-time-format-for-use-in-xts
library(xts)

mapy

library(leaflet)
library(ggmap) #- do ggplotu
library(rgdal) #- nacita rozne shapefily
library(rgeos)
library(maptools)
library(tmap)

  # nacitanie shapefilu

lnd <- readOGR(dsn = 'C:\\Users\\IBM_ADMIN\\Desktop\\R\\Creating-maps-in-R-master\\data', layer = 'london_sport') - dir, shapefile
lnd@data - slot dat
lnd$name - stlpec name slote dat

plot(lnd, col = 'lightgrey') - zobrazi vsetky data
plot(lnd[lnd$Partic_Per > 20 & lnd$Partic_Per < 25,], col = 'turquoise', add = T) - data s podmienkou na modro

# filtrovanie podla centroidov

plot(lnd, col = "grey")
# find London's geographic centroid (add ", byid = T" for all)
cent_lnd <- gCentroid(lnd[lnd$name == "City of London",]) 
points(cent_lnd, cex = 3)
# set 10 km buffer
lnd_buffer <- gBuffer(spgeom = cent_lnd, width = 10000) 

# method 1 of subsetting selects any intersecting zones

lnd_central <- lnd[lnd_buffer,] # the selection is too big!
# test the selection for the previous method - uncomment below
plot(lnd_central, col = "lightblue", add = T)
plot(lnd_buffer, add = T) # some areas just touch the buffer

# method2 of subsetting selects only points within the buffer

lnd_cents <- SpatialPoints(coordinates(lnd),
                           proj4string = CRS(proj4string(lnd))) # create spatialpoints
sel <- lnd_cents[lnd_buffer,] # select points inside buffer
points(sel) # show where the points are located
lnd_central <- lnd[sel,] # select zones intersecting w. sel
plot(lnd_central, add = T, col = "lightslateblue", 
     border = "grey")
plot(lnd_buffer, add = T, border = "red", lwd = 2)

# Add text to the plot!
text(coordinates(cent_lnd), "Central\nLondon")

      # bacha na projection string

qtm(lnd, 'CrimeCount') # mapa z kniznice tmap, subor lnd, stlpec crimecount

lnd_f <- fortify(lnd) # len udaje o polygonoch, koordinatoch,...bez dat
lnd$id <- row.names(lnd)
lnd_f <- left_join(lnd_f, lnd@data) # doplnenie dat

ggplot(lnd_f, aes(long, lat, group = group, fill = Partic_Per)) + geom_polygon() # mapa

library(rCharts)
library(threejs)

# datatables

rowCallback = DT::JS('function(row, data) {if (data[1] == "China")$("td", row).css("background", "orange")
                 }') # oznac na oranzovo riadok, kde prvy stlpec je 'China', zatvorka musi byt na dalsom riadku,
                      # ako premenna nefunguje v ramci paste0, mozno len problem so stringom

# UI stuff

div(style = 'height: 220px; overflow:scroll', uiOutput('reac_units')) style element

# mesure time of execution

# difference between 2 proc.time() calls
# time of function calls system.time(function_name(params))

# recoding categorical variables

mapvalues(df_toKeep$cluster,
          from = unique(df_toKeep$cluster),
          to = 1:length(unique(df_toKeep$cluster)))
alebo revalue z packagu plyr


https://cran.r-project.org/web/packages/RGA/index.html # google analytics api client package
graph compedium v R/plotting zalozke - popici network graf a scatter s marginal plotmi s gridExtra
http://www.r-bloggers.com/taskscheduler-r-package-to-schedule-r-scripts-with-the-windows-task-manager/
http://blog.rstudio.org/2016/03/29/feather/ - novy super rychly format na dataframy
https://shinydata.wordpress.com/2015/01/08/front-end-for-ggplot2/ - reaktivita, a zaujimava kniznica na  robenie grafov ggplot cez shiny
http://www.kevjohnson.org/making-maps-in-r/ ggmaps v R, kratky jednoduchy vystizny navod
https://cran.r-project.org/web/packages/animation/index.html POPICI! animacie do grafov
http://www.ggplot2-exts.org/ggiraph.html extensions do ggplotu
http://www.r-bloggers.com/shinyevents-build-shiny-apps-with-event-handlers/ - kniznica na event handling v Shiny
http://deanattali.com/2015/04/23/shinyjs-r-package/ alebo http://www.r-bloggers.com/new-shinyjs-version-useful-tools-for-any-shiny-app-developer-easily-call-javascript-functions-as-r-code/ javascript do shiny appiek
http://blog.revolutionanalytics.com/2016/06/microsoft-cognitive-services.html - R package for MS cognitive services APIs
http://yulijia.net/en/howto/r-language/2015/04/19/how-to-build-shiny-app-with-animation-package.html - ukladanie grafov ako html a ich renderovanie pomocou renderUI
http://www.r-bloggers.com/animated-choropleths-using-animation-ggplot2-rcharts-googlevis-and-shiny-to-visualize-violent-crime-rates-in-different-us-states-across-5-decades/ - vytvorenie saveMTHL animovaneho grafu s controls
https://ujjwalkarn.me/2016/06/17/introducing-xda-r-package-for-exploratory-data-analysis/ small package for quick EDA
http://bafflednerd.com/learn-r-online/ list of 60 online R courses
https://www.udemy.com/r-analysis/ kurz, vela o apply, loopoch a pod.
https://www.coursera.org/learn/data-products R + Shiny kurz
https://www.udemy.com/linear-regression-glms-and-gams-with-r/ o regresii
https://www.udemy.com/the-comprehensive-programming-in-r-course/ R oop, vela o s3, s4 classach
http://www.r-bloggers.com/data-shape-transformation-with-reshape/ reshape funkcia v R aj s cviceniami
http://www.r-bloggers.com/grea-the-rstudio-add-in-to-read-all-the-data-into-r/ RStudio addin to read data
http://jangorecki.github.io/blog/2016-06-30/Boost-Your-Data-Munging-with-R.html podrobne o data.table - super
http://daattali.com/shiny/timevis-demo/ timeline visualisations in shiny - PECKA
https://www.ggplot2-exts.org/ggiraph.html ggplot extensions
https://blog.rstudio.org/2016/05/23/profiling-with-rstudio-and-profvis/ # profiling in Rstudio - able to see which part of code takes how much long
http://www.exegetic.biz/blog/2016/08/uber-a-package-for-the-uber-api/  # how to use uber API