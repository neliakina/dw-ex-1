#load packages
library(tidyr)
library(dplyr)


#load refine_original.csv
products <- read.csv("refine-original.csv", stringsAsFactors = FALSE)

#1: Clean up brand names
products$company <- gsub(".*lips", "philips", products$company, ignore.case = TRUE)
products$company <- gsub("ak.*", "akzo", products$company, ignore.case = TRUE)
products$company <- gsub("van houten", "van houten", products$company, ignore.case = TRUE)
products$company <- gsub("unil.*", "unilever", products$company, ignore.case = TRUE)

#2: Separate product code and number
products <- separate(products, 
         Product.code...number, 
         c("product_code", "product_number"),
         sep = "-",
         remove = TRUE)

#3: Add product categories
p_categories <- c("p" = "smartphone", 
                  "v" = "tv",
                  "x" = "laptop",
                  "q" = "tablet")
products <- mutate(products, product_category = p_categories[product_code])

#4: Add full address for geocoding
products <- unite(products, 
                  full_address, 
                  address, city, country, 
                  sep = ",",
                  remove = FALSE)

#5: Create dummy variables for company and product category
products <- mutate(products, company_philips = ifelse(products$company == "philips", 1, 0),
         company_akzo = ifelse(products$company == "akzo", 1, 0), 
         company_van_houten = ifelse(products$company == "van houten", 1, 0), 
         company_unilever = ifelse(products$company == "unilever", 1, 0))

products <- mutate(products, product_smartphone = ifelse(products$product_category == "smartphone", 1, 0),
         product_tv = ifelse(products$product_category == "tv", 1, 0), 
         product_laptop = ifelse(products$product_category == "laptop", 1, 0), 
         product_tablet = ifelse(products$product_category == "tablet", 1, 0))

# Write results to refine_clean_csv
write.csv(products, file = "refine_clean.csv")
