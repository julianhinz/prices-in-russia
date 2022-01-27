###
# section 2 - Consumer prices in Russia - plots and regressions
###

if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr)
pacman::p_load(data.table)
pacman::p_load(readxl)
pacman::p_load(magrittr)
pacman::p_load(lubridate)
pacman::p_load(stringr)
pacman::p_load(ggplot2)
pacman::p_load(scales)
pacman::p_load(lfe)
pacman::p_load(stargazer)
pacman::p_load(countrycode)
pacman::p_load(sf)
pacman::p_load_gh("thomasp85/scico")
pacman::p_load(viridis)

###
# preparations ####
###

# load and prepare data
data_prices = read_rds("data/prices.rds")
data_prices = data_prices[value > 0]
data_prices[, date := ymd(date)]

# load and prepare meta data
meta_products = read_xlsx(str_c("data/metadata.xlsx"), sheet = "products") %>% setDT()
meta_products[str_length(HS.code) %in% c(3,5,7,9), HS.code := str_pad(HS.code, str_length(HS.code)+1, "left", "0")]
meta_regions = read_xlsx(str_c("data/metadata.xlsx"), sheet = "regions") %>% setDT()

## geographical subdivisions
federal_districts = data_prices[, .(region = unique(region))][str_detect(unique(region), "ederal") & !str_detect(unique(region), "Crimea"), region]
federal_subjects = meta_regions[subject == 1]$english %>% unique()
federal_subjects = federal_subjects[federal_subjects %in% data_prices[, unique(region)]]
cities = meta_regions[city == 1, unique(english)]
cities = cities[cities %in% data_prices[, unique(region)]]

## product categories
products_targeted = meta_products[strictly.targeted == 1, english]
products_services = meta_products[service == 1, english]
products_food = meta_products[nutrition == 1, english]

# add variables
data_prices[, sanctions := as.integer(date >= ymd("2014-08-01"))]
data_prices[, targeted := as.integer(product %in% products_targeted)]
data_prices[, services := as.integer(product %in% products_services)]
data_prices[, food := as.integer(product %in% products_food)]

# create fixed effects
data_prices[, region_date := str_c(region, date), by = .(region, date)]
data_prices[, region_product_month := str_c(region, product, month(date))]

# define subsamples
data_prices[, data_districts_fnf := as.logical(region %in% federal_districts & services == F)]
data_prices[, data_districts_f := as.logical(region %in% federal_districts & food == T)]
data_prices[, data_districts_nf := as.logical(region %in% federal_districts & food == F & services == F)]

data_prices[, data_subjects_fnf := as.logical(region %in% federal_subjects & services == F)]
data_prices[, data_subjects_f := as.logical(region %in% federal_subjects & food == T)]
data_prices[, data_subjects_nf := as.logical(region %in% federal_subjects & food == F & services == F)]

data_prices[, data_cities_fnf := as.logical(region %in% cities & services == F
                                     & !region=="Anadyr" & !region=="Cherkessk" & !region=="Gatchina" & !region=="Kerch"
                                     & !region=="Kingisepp" & !region=="Kirishi"  & !region=="Novomoskovsk"   & !region=="Pskov"
                                     & !region=="Tosno" & !region=="Yalta"  & !region=="Yevpatoriya")]
data_prices[, data_cities_f := as.logical(region %in% cities & food == T
                                          & !region=="Anadyr" & !region=="Cherkessk" & !region=="Gatchina" & !region=="Kerch"
                                          & !region=="Kingisepp" & !region=="Kirishi"  & !region=="Novomoskovsk"   & !region=="Pskov"
                                          & !region=="Tosno" & !region=="Yalta"  & !region=="Yevpatoriya")]
data_prices[, data_cities_nf := as.logical(region %in% cities & food == F & services == F
                                           & !region=="Anadyr" & !region=="Cherkessk" & !region=="Gatchina" & !region=="Kerch"
                                           & !region=="Kingisepp" & !region=="Kirishi"  & !region=="Novomoskovsk"   & !region=="Pskov"
                                           & !region=="Tosno" & !region=="Yalta"  & !region=="Yevpatoriya")]

persistent_products_prices = data_prices[, .(n = uniqueN(date)), by = product][n == uniqueN(data_prices$date), product]
persistent_regions_prices = data_prices[, .(n = uniqueN(date)), by = region][n == uniqueN(data_prices$date), region]

###
# descriptive statistics ####
###

# count cities
length(unique(cities))
# count federal subjects
length(unique(federal_subjects))
# count federal districts
length(unique(federal_districts))

# count all products
data_prices[, uniqueN(product)]
# count targeted goods in dataset
data_prices[targeted == 1, uniqueN(product)]
# count food products in dataset
data_prices[food == 1, uniqueN(product)]
# count non-food products in dataset
data_prices[food == 0 & services == 0, uniqueN(product)]
# count services in dataset
data_prices[services == 1, uniqueN(product)]

# number of observations in city level
data_prices[region %in% cities] %>% nrow
# number of observations in subject level
data_prices[region %in% federal_subjects] %>% nrow
# number of observations in district level
data_prices[region %in% federal_districts] %>% nrow


###
# figure 1: evolution of prices ####
###

plot_data = data_prices[product %in% persistent_products_prices
                 & region %in% persistent_regions_prices
                 & region %in% federal_subjects]
plot_data[targeted == 1, type := "Embargoed"]
plot_data[food == 0 & services == 0, type := "Not embargoed (non-food)"]
plot_data[food == 1 & targeted == 0, type := "Not embargoed (food)"]

plot_data[, value := value / mean(value[year(date) == 2013]), by = product]
plot_data = plot_data[, .(mean_price = mean(value)), by = .(date, type)]

plot = ggplot(data = plot_data[!is.na(type)]) +
  theme_minimal() +
  geom_line(aes(x = date, y = mean_price, group = type, color = type)) +
  scale_y_continuous("Mean price (1 = average of 2013)") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  guides(color=guide_legend("Embargoed")) +
  scale_color_manual(values = c("red", "blue", "darkgreen", "goldenrod")) +
  geom_vline(aes(xintercept = ymd("2014-08-01"))) +
  annotate(geom = "text",
           x = ymd("2014-09-01") - 60, y = 0.95,
           label = "Embargo", color = "black",
           angle = 90) +
  theme(axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(r = 0.5, unit = 'cm')),
        legend.spacing.x = unit(0.25, 'cm'))
ggsave(plot, filename = "results/treatment_control.eps",
       width = 20, height = 10, units = "cm")
rm(plot_data, plot)


###
# table 1: benchmark regression ####
###
reg1_districts_prices_f = felm(log(value) ~ sanctions : targeted |
                                 region_date + region_product_month | 0 | region,
                               data_prices[data_districts_f == T])
reg1_districts_prices_nf = felm(log(value) ~ sanctions : targeted |
                                   region_date + region_product_month | 0 | region,
                                 data_prices[targeted == T | data_districts_nf == T])
reg1_subjects_prices_f = felm(log(value) ~ sanctions : targeted  |
                                region_date + region_product_month | 0 | region,
                              data_prices[data_subjects_f == T])
reg1_subjects_prices_nf = felm(log(value) ~ sanctions : targeted  |
                                  region_date + region_product_month | 0 | region,
                                data_prices[targeted == T | data_subjects_nf == T])
reg1_cities_prices_f = felm(log(value) ~ sanctions : targeted |
                              region_date + region_product_month | 0 | region,
                            data_prices[data_cities_f == T])
reg1_cities_prices_nf = felm(log(value) ~ sanctions : targeted |
                                region_date + region_product_month | 0 | region,
                              data_prices[targeted == T | data_cities_nf == T])

# output
stargazer(reg1_districts_prices_f, reg1_districts_prices_nf,
          reg1_subjects_prices_f, reg1_subjects_prices_nf,
          reg1_cities_prices_f, reg1_cities_prices_nf,
          type = "text")

# clean up
rm(reg1_districts_prices_f, # reg1_districts_prices_nf needed below
   reg1_subjects_prices_f, reg1_subjects_prices_nf,
   reg1_cities_prices_f, reg1_cities_prices_nf)


###
# figure 2 and table for appendix with monthly coefficients ####
###

data_prices[, time := as.integer(date >= ymd("2014-08-01"))]

# regressions
reg2_districts_prices_date_f = felm(log(value) ~ time : targeted : as.factor(date) |
                                      region_date + region_product_month | 0 | region,
                                    data_prices[data_districts_f == T])
reg2_districts_prices_date_nf = felm(log(value) ~ time : targeted : as.factor(date) |
                                       region_date + region_product_month | 0 | region,
                                     data_prices[targeted == T | data_districts_nf == T])
reg2_subjects_prices_date_f = felm(log(value) ~ time : targeted : as.factor(date) |
                                     region_date + region_product_month | 0 | region,
                                   data_prices[data_subjects_f == T])
reg2_subjects_prices_date_nf = felm(log(value) ~ time : targeted : as.factor(date) |
                                      region_date + region_product_month | 0 | region,
                                    data_prices[targeted == T | data_subjects_nf == T])
reg2_cities_prices_date_f = felm(log(value) ~ time : targeted : as.factor(date) |
                                   region_date + region_product_month | 0 | region,
                                 data_prices[data_cities_f == T])
reg2_cities_prices_date_nf = felm(log(value) ~ time : targeted : as.factor(date) |
                                    region_date + region_product_month | 0 | region,
                                  data_prices[targeted == T | data_cities_nf == T])

# plot coefficients
coef_plot = data.table(Date = names(coef(reg2_districts_prices_date_f)),
                       districts_f_value = summary(reg2_districts_prices_date_f)$coefficients[,1],
                       districts_f_se = summary(reg2_districts_prices_date_f)$coefficients[,2],
                       districts_nf_value = summary(reg2_districts_prices_date_nf)$coefficients[,1],
                       districts_nf_se = summary(reg2_districts_prices_date_nf)$coefficients[,2],
                       subjects_f_value = summary(reg2_subjects_prices_date_f)$coefficients[,1],
                       subjects_f_se = summary(reg2_subjects_prices_date_f)$coefficients[,2],
                       subjects_nf_value = summary(reg2_subjects_prices_date_nf)$coefficients[,1],
                       subjects_nf_se = summary(reg2_subjects_prices_date_nf)$coefficients[,2],
                       cities_f_value = summary(reg2_cities_prices_date_f)$coefficients[,1],
                       cities_f_se = summary(reg2_cities_prices_date_f)$coefficients[,2],
                       cities_nf_value = summary(reg2_cities_prices_date_nf)$coefficients[,1],
                       cities_nf_se = summary(reg2_cities_prices_date_nf)$coefficients[,2])
coef_plot[, Date := str_sub(Date, -10) %>% ymd()]
coef_plot = coef_plot[Date >= ymd("2014-08-01")]
coef_plot = melt(coef_plot, id.vars = "Date")
coef_plot[, type := "value"]
coef_plot[str_detect(variable, "_se"), type := "se"]
coef_plot[, Sample := "Food"]
coef_plot[str_detect(variable, "_nf"), Sample := "Non-food"]
coef_plot[, Region := "Districts"]
coef_plot[str_detect(variable, "subjects_"), Region := "Subjects"]
coef_plot[str_detect(variable, "cities_"), Region := "Cities"]
coef_plot = dcast(coef_plot[,list(Date, type, Sample, Region, value)], Date + Region + Sample ~ type, value.var = "value")

# district level
plot_districts = ggplot(coef_plot[Region == "Districts"]) +
  theme_minimal() +
  geom_ribbon(data = coef_plot[Region == "Districts" & Date < ymd("2014-08-01")],
              aes(x = Date, ymax = value + 1.96*se, ymin = value - 1.96*se, group = Sample, color = NULL), fill = "grey90") +
  geom_line(data = coef_plot[Region == "Districts" & Date < ymd("2014-08-01")],
            aes(x = Date, y = value, group = Sample, color = Sample)) +
  geom_ribbon(data = coef_plot[Region == "Districts" & Date >= ymd("2014-08-01")],
              aes(x = Date, ymax = value + 1.96*se, ymin = value - 1.96*se, group = Sample, color = NULL), fill = "grey90") +
  geom_line(data = coef_plot[Region == "Districts" & Date >= ymd("2014-08-01")],
            aes(x = Date, y = value, group = Sample, color = Sample)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  geom_vline(aes(xintercept = ymd("2014-08-01"))) +
  annotate(geom = "text",
           x = ymd("2014-08-01")-20, y = 0,
           label = "Embargo", color = "black",
           angle = 90) +
  scale_color_manual(labels = c("Food", "Non-food"),
                     values = c("blue", "darkgreen")) +
  scale_y_continuous(name = "Magnitude") +
  theme(axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(r = 0.5, unit = 'cm')),
        legend.spacing.x = unit(0.25, 'cm'))
ggsave(plot_districts, filename = "results/monthly_coefs_districts.eps",
       width = 10, height = 10, units = "cm")

# subject level
plot_subjects = ggplot(coef_plot[Region == "Subjects"]) +
  theme_minimal() +
  geom_ribbon(data = coef_plot[Region == "Subjects" & Date < ymd("2014-08-01")],
              aes(x = Date, ymax = value + 1.96*se, ymin = value - 1.96*se, group = Sample, color = NULL), fill = "grey90") +
  geom_line(data = coef_plot[Region == "Subjects" & Date < ymd("2014-08-01")],
            aes(x = Date, y = value, group = Sample, color = Sample)) +
  geom_ribbon(data = coef_plot[Region == "Subjects" & Date >= ymd("2014-08-01")],
              aes(x = Date, ymax = value + 1.96*se, ymin = value - 1.96*se, group = Sample, color = NULL), fill = "grey90") +
  geom_line(data = coef_plot[Region == "Subjects" & Date >= ymd("2014-08-01")],
            aes(x = Date, y = value, group = Sample, color = Sample)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  geom_vline(aes(xintercept = ymd("2014-08-01"))) +
  annotate(geom = "text",
           x = ymd("2014-08-01")-20, y = 0,
           label = "Embargo", color = "black",
           angle = 90) +
  scale_color_manual(labels = c("Food", "Non-food"),
                     values = c("blue", "darkgreen")) +
  scale_y_continuous(name = "Magnitude") +
  theme(axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(r = 0.5, unit = 'cm')),
        legend.spacing.x = unit(0.25, 'cm'))
ggsave(plot_subjects, filename = "results/monthly_coefs_subjects.eps",
       width = 10, height = 10, units = "cm")

# city level
plot_cities = ggplot(coef_plot[Region == "Cities"]) +
  theme_minimal() +
  geom_ribbon(data = coef_plot[Region == "Cities" & Date < ymd("2014-08-01")],
              aes(x = Date, ymax = value + 1.96*se, ymin = value - 1.96*se, group = Sample, color = NULL), fill = "grey90") +
  geom_line(data = coef_plot[Region == "Cities" & Date < ymd("2014-08-01")],
            aes(x = Date, y = value, group = Sample, color = Sample)) +
  geom_ribbon(data = coef_plot[Region == "Cities" & Date >= ymd("2014-08-01")],
              aes(x = Date, ymax = value + 1.96*se, ymin = value - 1.96*se, group = Sample, color = NULL), fill = "grey90") +
  geom_line(data = coef_plot[Region == "Cities" & Date >= ymd("2014-08-01")],
            aes(x = Date, y = value, group = Sample, color = Sample)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  geom_vline(aes(xintercept = ymd("2014-08-01"))) +
  annotate(geom = "text",
           x = ymd("2014-08-01")-20, y = 0,
           label = "Embargo", color = "black",
           angle = 90) +
  scale_color_manual(labels = c("Food", "Non-food"),
                     values = c("blue", "darkgreen")) +
  scale_y_continuous(name = "Magnitude") +
  theme(axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(r = 0.5, unit = 'cm')),
        legend.spacing.x = unit(0.25, 'cm'))
ggsave(plot_cities, filename = "results/monthly_coefs_cities.eps",
       width = 10, height = 10, units = "cm")

# short-run impact in Febuary 2015
coef_plot[Sample == "Food" & Date == ymd("2015-02-01")][value == min(value)]
coef_plot[Sample == "Non-food" & Date == ymd("2015-02-01")][value == max(value)]

# medium-run impact in August 2015
coef_plot[Sample == "Food" & Date == ymd("2015-08-01")][value == min(value)]
coef_plot[Sample == "Non-food" & Date == ymd("2015-08-01")][value == max(value)]

# clean up
rm(plot_cities, plot_subjects, plot_districts, coef_plot,
   reg2_districts_prices_date_f, reg2_districts_prices_date_nf,
   reg2_subjects_prices_date_f, reg2_subjects_prices_date_nf,
   reg2_cities_prices_date_f, reg2_cities_prices_date_nf)


###
# table 2 and figure 3: impact on imports ####
###

data_trade = read_rds("data/trade.rds")
data_trade[, region := reporter]
data_trade[, product := hs]

sanctioning_countries = c("Belgium", "Denmark", "France", "Germany",
                          "Greece", "Ireland", "Italy", "Luxembourg",
                          "the Netherlands", "Portugal", "Spain", "United Kingdom",
                          "Austria", "Finland", "Sweden", "Czech Republic",
                          "Cyprus", "Estonia", "Hungary", "Latvia",
                          "Lithuania", "Malta", "Poland", "Slovakia", "Slovenia",
                          "Romania", "Bulgaria", "Croatia", "United States",
                          "Japan", "Canada", "Australia", "New Zealand",
                          "Norway", "Ukraine", "Georgia", "Albania", "Montenegro") %>%
  countrycode("country.name", "iso3c")
embargoed_products = meta_products[strictly.targeted == 1 | ambiguous == 1]$HS.code %>% str_sub(1,4) %>% unique()
embargoed_products = embargoed_products[!is.na(embargoed_products)]
food_products = c("02", "03", "04", "07", "08", "09", "10", "16",
                  "17", "18", "19", "20", "21", "22", "23")

# add variables
data_trade[, food := as.numeric(str_sub(hs,1,2) %in% str_sub(unique(c(food_products, embargoed_products)),1,2))]
data_trade[, embargoed := as.numeric(hs %in% embargoed_products)]
data_trade[, sanctioning := as.numeric(partner %in% sanctioning_countries)]
data_trade[, treatment_period := as.numeric(date > ymd("2014-08-01"))]
data_trade[, treatment_products := as.numeric(embargoed)]
data_trade[, treatment_partner := as.numeric(sanctioning)]

# descriptive stats
data_trade[year(date) == 2013, .(share = sum(value[food == 1 & sanctioning == 1]) / sum(value[food == 1]))]

# create fixed effects
data_trade[, region_date := str_c(region, date), by = .(region, date)]
data_trade[, region_product := str_c(region, product), by = .(region, product)]
data_trade[, region_product_month := str_c(region, product, month(date)), by = .(region, product, date)]
data_trade[, region_partner_month := str_c(region, partner, month(date)), by = .(region, partner, date)]

# diff-in-diff: change in value / weight - across partners within embargoed
reg1_value = felm(log(value) ~ treatment_period : treatment_partner |
                    region_date + region_product_month | 0 | region,
                  data = data_trade[value > 0 & weight > 0 & embargoed == 1])
reg1_weight = felm(log(weight) ~ treatment_period : treatment_partner |
                     region_date + region_product_month | 0 | region,
                   data = data_trade[value > 0 & weight > 0 & embargoed == 1])

# diff-in-diff: change in value / weight - across products within non-sanctioning partners
reg3_value = felm(log(value) ~ treatment_period : treatment_products |
                    region_date + region_product_month | 0 | region,
                  data = data_trade[value > 0 & weight > 0 & sanctioning == 0])
reg3_weight = felm(log(weight) ~ treatment_period : treatment_products |
                     region_date + region_product_month | 0 | region,
                   data = data_trade[value > 0 & weight > 0 & sanctioning == 0])

stargazer(reg1_value, reg1_weight,
          reg3_value, reg3_weight,
          type = "text")


# plot
plot_data = data_trade[, .(value = sum(value)), by = .(date, sanctioning, embargoed)]
plot_data[, type := "Affected (sanctioning country, embargoed good)"]
plot_data[embargoed == T & sanctioning == F, type := "Not affected (non-sanctioning country, embargoed good)"]
plot_data[embargoed == F & sanctioning == T, type := "Not affected (sanctioning country, non-embargoed good)"]
plot_data[embargoed == F & sanctioning == F, type := "Not affected (non-sanctioning country, non-embargoed good)"]

plot = ggplot(plot_data) +
  theme_minimal() +
  geom_line(aes(x = date, y = value / 1000000, group = type, color = type)) +
  scale_x_date(NULL, date_labels = "%Y", date_breaks = "1 year") +
  scale_y_log10("Value of imports (in Mio. USD)") +
  scale_color_manual(values = c("red", "blue", "darkgreen", "goldenrod")) +
  geom_vline(aes(xintercept = ymd("2014-08-01"))) +
  annotate(geom = "text",
           x = ymd("2014-08-15") - 30, y = 100,
           label = "Embargo", color = "black",
           angle = 90) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(r = 0.25, unit = 'cm')),
        legend.spacing.x = unit(0.1, 'cm')) +
  guides(color = guide_legend(nrow=2,byrow=TRUE))
ggsave(plot, filename = "results/treatment_control_trade.eps",
       width = 20, height = 10, units = "cm")

rm(reg1_value, reg1_weight,
   reg3_value, reg3_weight,
   plot, plot_data)


###
# table 3 and figure 4: impact on wholesales and domestic production ####
###

# load wholesales dataset
data_wholesales = read_rds("data/wholesales.rds")

# clean up dataset
data_wholesales = data_wholesales[value > 0]

# only data at district level, subject-level data incomplete
data_wholesales = data_wholesales[region %in% federal_districts]

# only take products that are bought in each period, otherwise possibly spurious
persistent_products_wholesales = data_wholesales[, uniqueN(date), by = product][V1 >= 57, product]

# only take products which are measured in tons
persistent_products_wholesales = persistent_products_wholesales[(str_detect(persistent_products_wholesales, ", t") |
                                                                   str_detect(persistent_products_wholesales, "ton"))
                                          & !str_detect(persistent_products_wholesales, ", ths.")]

persistent_products_wholesales[!persistent_products_wholesales %in% products_targeted] %>% length()
persistent_products_wholesales[persistent_products_wholesales %in% products_targeted] %>% length()

# add variables
data_wholesales[, sanctions := as.integer(date >= ymd("2014-08-01"))]
data_wholesales[, targeted := as.integer(product %in% products_targeted)]
data_wholesales[, food := as.integer(product %in% products_food)]

# create fixed effects
data_wholesales[, region_date := str_c(region, date), by = .(region, date)]
data_wholesales[, region_product_month := str_c(region, product, month(date))]

# number of observations in city level
data_wholesales[region %in% cities] %>% nrow
# number of observations in subject level
data_wholesales[region %in% federal_subjects] %>% nrow
# number of observations in district level
data_wholesales[region %in% federal_districts] %>% nrow

# regressions
reg3_districts_f = felm(log(value) ~ sanctions : targeted |
                          region_date + region_product_month | 0 | region,
                        data_wholesales[
                          product %in% persistent_products_wholesales
                          & food == T
                          ])

reg3_districts_nf = felm(log(value) ~ sanctions : targeted |
                            region_date + region_product_month | 0 | region,
                          data_wholesales[
                            product %in% persistent_products_wholesales
                            & (food == F | targeted == T)
                          ])

# load production data
data_production = read_rds("data/production.rds")
data_production = data_production[date >= ymd("2011-01-01") & date <= ymd("2016-06-01")]

# clean up
data_production = data_production[value > 0]
data_production = data_production[unit == "ton"]
data_production = data_production[!region %in% c("Russian Federation", "Crimean Federal District")]

# persistent products
persistent_products_production = data_production[, uniqueN(date), by = product][V1 == 66, product]

# add variables
data_production[, sanctions := as.integer(date >= ymd("2014-08-01"))]
data_production[, targeted := as.integer(product %in% products_targeted)]
data_production[, food := as.integer(product %in% products_food)]

# create fixed effects
data_production[, region_date := str_c(region, date), by = .(region, date)]
data_production[, region_product_month := str_c(region, product, month(date))]

reg4_districts_f = felm(log(value) ~ sanctions : targeted | region_date + region_product_month | 0 | region,
                          data_production[
                            food == T
                            & product %in% persistent_products_production
                            ])
reg4_districts_nf = felm(log(value) ~ sanctions : targeted | region_date + region_product_month | 0 | region,
                          data_production[
                            product %in% persistent_products_production &
                              (food == F | targeted == T)
                          ])

stargazer(reg3_districts_f,
          reg3_districts_nf,
          reg4_districts_f,
          reg4_districts_nf,
          type = "text")


# plot: all products
plot_data = data_production[product %in% persistent_products_production,
                            .(value = sum(value)), by = .(date = ymd(date), targeted = as.factor(targeted), food = food == T)]
plot_data[, type := "Affected"]
plot_data[targeted == 0 & food == 0, type := "Not affected (non-food)"]
plot_data[targeted == 0 & food == 1, type := "Not affected (food)"]

plot = ggplot(plot_data) +
  theme_minimal() +
  geom_line(aes(x = date, y = value / 1000000, group = type, color = type)) +
  scale_y_log10("Production of goods (in Mio. tons)") +
  guides(color=guide_legend("Embargoed")) +
  scale_color_manual(labels = c("Affected", "Not affected (food)", "Not affected (non-food)"),
                     values = c("red", "blue", "darkgreen")) +
  geom_vline(aes(xintercept = ymd("2014-08-01"))) +
  annotate(geom = "text",
           x = ymd("2014-08-01") - 60, y = 25,
           label = "Embargo", color = "black",
           angle = 90) +
  theme(axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(r = 0.25, unit = 'cm')),
        legend.spacing.x = unit(0.1, 'cm')) +
  guides(color = guide_legend(nrow=1,byrow=TRUE))
ggsave(plot, filename = "results/treatment_control_production.eps",
       width = 20, height = 10, units = "cm")
rm(plot, plot_data,
   reg3_districts_f, # reg3_districts_nf needed below
   reg4_districts_f, reg4_districts_nf,
   data_production)


###
# table 4: interaction with import shares ####
# - note: import shares from 2012, so regressions on prices 2013 onwards
###

shares = read_rds("data/share_imp_12months.rds")
data_prices[, region_upper := toupper(region)]
data_prices = merge(data_prices[date >= ymd("2013-01-01")],
                    shares[, .(region_upper = region,
                               share_sanctioning_in_embargoed,
                               share_sanctioning_in_total)],
                    by = "region_upper", all.x = T)
data_prices[, region_upper := NULL]
rm(shares)

reg4_districts_f = felm(log(value) ~ sanctions : targeted + sanctions : targeted : share_sanctioning_in_total |
                          region_date + region_product_month | 0 | region_date,
                        data_prices[data_districts_f == T])
reg4_districts_nf = felm(log(value) ~ sanctions : targeted + sanctions : targeted : share_sanctioning_in_total |
                           region_date + region_product_month | 0 | region_date,
                         data_prices[data_districts_nf == T | targeted == T])
reg4_subjects_f = felm(log(value) ~ sanctions : targeted + sanctions : targeted : share_sanctioning_in_total |
                         region_date + region_product_month | 0 | region_date,
                       data_prices[data_subjects_f == T])
reg4_subjects_nf = felm(log(value) ~ sanctions : targeted + sanctions : targeted : share_sanctioning_in_total |
                           region_date + region_product_month | 0 | region_date,
                         data_prices[data_subjects_nf == T | targeted == T])
stargazer(reg4_districts_f, reg4_districts_nf,
          reg4_subjects_f, reg4_subjects_nf,
          type = "text")

rm(reg4_districts_f, reg4_districts_nf,
   reg4_subjects_f, reg4_subjects_nf)


###
# table 5: check movement of prices and quantities ----
###

reg1_districts_prices_non_embargoed = felm(log(value) ~ sanctions : food |
                                             region_date + region_product_month | 0 | region,
                                           data_prices[data_districts_nf == T | (food == T & targeted == F)])
reg3_districts_non_embargoed = felm(log(value) ~ sanctions : food |
                                      region_date + region_product_month | 0 | region,
                                    data_wholesales[
                                      product %in% persistent_products_wholesales
                                      & targeted == F
                                    ])

stargazer(reg1_districts_prices_nf, reg1_districts_prices_non_embargoed,
          reg3_districts_nf, reg3_districts_non_embargoed,
          type = "text")

rm(reg1_districts_prices_nf, reg1_districts_prices_non_embargoed,
   reg3_districts_nf, reg3_districts_non_embargoed,
   data_wholesales)


###
# table 6: change in prices of linked sectors ####
###
concord_product_HS_GTAP = read_rds("data/concord_product_HS_GTAP.rds")
linked_products = concord_product_HS_GTAP[GTAP_code %in% c(9, 10, 11, 12, 24), unique(product)]
data_prices[, linked_sectors := as.integer(product %in% linked_products)]

# descriptive stats
data_trade[year(date) == 2013, .(share = sum(value[food == 1 & sanctioning == 1]) / sum(value[food == 1]))]

reg6_districts_fnf = felm(log(value) ~ sanctions : linked_sectors + sanctions : targeted |
                            region_date + region_product_month | 0 | region,
                          data_prices[data_districts_fnf == T])
reg6_subjects_fnf = felm(log(value) ~ sanctions : linked_sectors + sanctions : targeted |
                           region_date + region_product_month | 0 | region,
                         data_prices[data_subjects_fnf == T])
reg6_cities_fnf = felm(log(value) ~ sanctions : linked_sectors + sanctions : targeted |
                         region_date + region_product_month | 0 | region,
                       data_prices[data_cities_fnf == T])

# output table
stargazer(reg6_districts_fnf,
          reg6_subjects_fnf,
          reg6_cities_fnf,
          type = "text")

rm(concord_product_HS_GTAP,
   reg6_districts_fnf,
   reg6_subjects_fnf,
   reg6_cities_fnf,
   data_trade)
gc()


###
# figure 5: map of average price increases of embargo products by regions, jun 14 to jun 15 ####
###

plot_data = data_prices[product %in% persistent_products_prices
                 & region %in% persistent_regions_prices
                 & region %in% federal_subjects
                 , list(date, product, targeted, value, region)]

plot_data = plot_data[(date == "2015-06-01" | date == "2014-06-01") & targeted == 1]
plot_data[date < ymd("2014-09-01"), type := "pre"]
plot_data[date > ymd("2014-09-01"), type := "post"]
plot_data = plot_data[, .(change = (value[type == "post"] - value[type == "pre"]) / value[type == "pre"]), by = .(region, product)]
plot_data = plot_data[, .(change = 100*mean(change, na.rm = T)), by = .(region)]

# map
russia_map = read_rds("data/shapefile_RUS_adm1.rds")
russia_map = st_as_sf(russia_map)
russia_map = st_simplify(russia_map, dTolerance = 0.01)
gc()

plot_data = merge(plot_data, meta_regions[!is.na(map_name), .(region = english, NAME_1 = map_name)], by = "region", all.x = T)
plot_data = merge(russia_map, plot_data, by = "NAME_1", all.x = T)

plot = ggplot() +
  theme_minimal() +
  geom_sf(data = plot_data, aes(fill = change), color = "darkgrey", size = 0.1) +
  coord_sf(crs="+proj=aea +lat_1=30 +lat_2=70 +lon_0=100") +
  scale_fill_viridis("Change\nin %", option = "plasma") +
  theme(plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title = element_text(size = 9),
        legend.margin=margin(r=0,l=0))
ggsave(plot, file = str_c("results/map_prices.png"),
       bg = "transparent",
       width = 20, height = 10, units = "cm")
rm(plot, plot_data,
   russia_map)


###
# appendix table B1: interaction with distance to Ukraine ####
###
distances_to_ukraine = fread("data/distances_to_ukraine.csv")
data_prices = merge(data_prices, distances_to_ukraine, by = "region", all.x = T)
rm(distances_to_ukraine)

reg5_districts_f = felm(log(value) ~ sanctions : targeted + sanctions : targeted : log(distance_to_ukraine) |
                          region_date + region_product_month | 0 | region,
                        data_prices[data_districts_f == T])
reg5_districts_nf = felm(log(value) ~ sanctions : targeted + sanctions : targeted : log(distance_to_ukraine) |
                           region_date + region_product_month | 0 | region,
                         data_prices[data_districts_nf == T | targeted == T])
reg5_subjects_f = felm(log(value) ~ sanctions : targeted + sanctions : targeted : log(distance_to_ukraine) |
                         region_date + region_product_month | 0 | region,
                       data_prices[data_subjects_f == T])
reg5_subjects_nf = felm(log(value) ~ sanctions : targeted + sanctions : targeted : log(distance_to_ukraine) |
                          region_date + region_product_month | 0 | region,
                        data_prices[data_subjects_nf == T | targeted == T])

# output table
stargazer(reg5_districts_f, reg5_districts_nf,
          reg5_subjects_f, reg5_subjects_nf,
          type = "text")

rm(reg5_districts_f, reg5_districts_nf,
   reg5_subjects_f, reg5_subjects_nf,
   data_prices)
gc()

### CLEAN UP
rm(list = ls())
