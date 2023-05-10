install.packages("wiesbaden")

library(wiesbaden) 
library(dplyr) 

save_credentials(db = 'regio', user= "RE009694", password = "jSiHrju8n6M1e82")

test_login(genesis=c(db='regio', user= "RE009694", password = "jSiHrju8n6M1e82"))

d <- retrieve_datalist(tableseries="411*", 
                       genesis=c(db='regio', user= "RE009694", password = "jSiHrju8n6M1e82")) 

da <- retrieve_data(tablename="41120BJ008", 
                    genesis=c(db='regio', user= "RE009694", password = "jSiHrju8n6M1e82")) 

a <- a %>% 
  filter(STAG == '31.12.2021') %>% 
  select(GEMEIN, Flaeche = FLC006_val) 

head(a)




