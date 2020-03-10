#loading library

library(RMySQL)
library(DBI)
library(arules)
library(Matrix)
library(dplyr)
library(arulesViz)

#Connect to data base

mydb <-  dbConnect(RMySQL::MySQL(),
                  username = "root",
                  password = "rootpassword",
                  host = "127.0.0.1",
                  port = 3306,
                  dbname = "imarket_sql"
)

# importing data from data base

dbListTables(mydb)
dbListFields(mydb, 'line_item')
rs = dbSendQuery(mydb, "select * from line_item")
line_item = fetch(rs, n=-1)
rs = dbSendQuery(mydb, "select * from orders")
orders=fetch(rs, n=-1)
rs = dbSendQuery(mydb, "select * from products")
products=fetch(rs, n=-1)

# merge line_item and order

line_order=merge.data.frame(line_item,orders,by="id_order")

# exclude not completed order

line_order=line_order %>% 
  filter(state=='Completed')

# merge product to the new data set

line_order_product=merge(line_order,products,by="sku")

# assign the datatype

line_order_product <- line_order_product %>% 
  mutate(unit_price = as.numeric(gsub(",",".",unit_price)))

line_order_product$total_paid=as.numeric(gsub(",",".",line_order_product$total_paid))

line_order_product$price=as.numeric(gsub(",",".",line_order_product$price))

#find the differeance between the prices in line_item and order

data1=line_order_product %>% 
  group_by(id_order,total_paid,price,product_quantity,unit_price,sku) %>% 
  summarise(sum=sum(unit_price*product_quantity)) %>%
  mutate(differance=abs(total_paid-sum)) %>% 
  filter(differance<=sum*.75)

# taking id order fromm the data set

id=data.frame(data1$id_order)

#excluding id_order whhiv is not in id

line_order_product=line_order_product %>% 
  inner_join (line_order_product,id,by='id_order')
# set decimal points to the uneven prices


#line_order_product = line_order_product[1:20,]
#for (row in 1:nrow(line_order_product)) {
 # price <- line_order_product$price.x
  #unit_price  <- line_order_product$unit_price.x
  #ff=abs(price-unit_price)
  
#  if(diff > .75*(unit_price)) {
 #   x<-nchar(as.character(unit_price))
  #  y<-nchar(as.character(price))
   # if(x>y){
    #  unit_price=(unit_price/10^(x-y))
  #  } else {
   #   price=(price/10^(y-x))
  #  }
    
#  }
  
#}
#find the differance between price in products and line items

data2=line_order_product %>%select(id_order,name_en.x,unit_price.x,price.x,product_quantity.x)%>% 
  mutate(diffrence=abs(price.x-unit_price.x)) %>%  filter(diffrence <= (unit_price.x)*.5) 

data3=data.frame(data2$id_order,data2$name_en.x)

#writing a csv file

  write.csv(data3,file ="filterd_dataset" )
  
#read  the dataset with arule package

modal_set<-read.transactions(file = "filterd_dataset",format = c("single"),
                               header=TRUE,sep = ",",cols = c("data2.id_order","data2.name_en.x"))
# view the transactions. 

inspect(modal_set)

# Number of transactions.

length(modal_set) 

#Number of items per transaction 

table(size(modal_set))

# Lists the transactions by conversion (LIST must be capitalized)

LIST(modal_set) 

# To see the item labels

itemLabels(modal_set) 


#plot the top ten products

itemFrequencyPlot(modal_set,topN=10)

summary(modal_set)
# use apriori model 

RulesName <- apriori (modal_set, parameter = list(supp = 0.000270144 , conf = 0.19,minlen=2))

# view the most recomended products

inspect(RulesName)


itemFrequencyPlot(RulesName,topN=5)

top10subRules <- head(RulesName, n = 10, by = "confidence")

plot(top10subRules, method = "graph",  engine = "htmlwidget")

