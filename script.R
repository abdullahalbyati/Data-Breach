library(readr)
library(tidyverse)
library(lubridate)
library(ggrepel)
library(mc2d)
library(modeest)
library(plotly)


df <- read_csv("PRC Data Breach Chronology - 1.13.20 - PRC Data Breach Chronology - 1.13.20.csv")

df <- df %>% 
  mutate(`Date Made Public` = as.Date(`Date Made Public`, format = "%m/%d/%Y"))
df$`Year of Breach` <- as.character(df$`Year of Breach`)

df <-
  filter(df, `Total Records` > 100)

Probability_of_Loss <- read_csv("Probability of Loss.csv")

summarized <- df %>% 
  summarise(
    count = n(),
    zero_records_count = nrow(filter(df, `Total Records` == 0)),
    no_record_count = sum(is.na(df$`Total Records`)),
    under_100 = nrow(filter(df, `Total Records` < 100, `Total Records` > 0 )),
  )

summarized <- summarized %>%
  tibble::rownames_to_column() %>%  
  pivot_longer(-rowname) %>% 
  pivot_wider(names_from=rowname, values_from=value) 

ggplot(data = df, mapping = aes(x = `Date Made Public`, y = `Total Records`)) +
  geom_point() +
  theme_bw(base_size = 15) +
  scale_x_date(date_labels = "%Y") +
  scale_y_continuous(labels = scales::label_number_si()) +
  ggrepel::geom_text_repel(data = df, aes(label = Company))

cost = c(10000, 100000, 1000000, 10000000, 100000000, 1000000000)
Probability_of_Loss$estimated_average_cost <- apply(Probability_of_Loss[,2:ncol(Probability_of_Loss)] * cost[col(Probability_of_Loss[,2:ncol(Probability_of_Loss)])],
                                                   1, mean)

Probability_of_Loss %>% 
  mutate(`Records Lost Range` = fct_reorder(`Records Lost Range`, desc(estimated_average_cost))) %>%
  ggplot(aes(x = `Records Lost Range`, y = estimated_average_cost)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(labels = scales::label_number_si(prefix="$")) +
  ylab("Estimated Average Cost of Breach")

df_1 <- filter(df, `Total Records` %in% 100:1000) 
df_1 <- df_1 %>% mutate(df_1, cost_per_record = Probability_of_Loss$estimated_average_cost[1]/`Total Records`)

df_2 <- filter(df, `Total Records` %in% 1000:10000) 
df_2 <- df_2 %>% mutate(df_2, cost_per_record = Probability_of_Loss$estimated_average_cost[2]/`Total Records`)

df_3 <- filter(df, `Total Records` %in% 10000:100000)
df_3 <- df_3 %>% mutate(df_3, cost_per_record = Probability_of_Loss$estimated_average_cost[3]/`Total Records`)

df_4 <- filter(df, `Total Records` %in% 100000:500000) 
df_4 <- df_4 %>% mutate(df_4, cost_per_record = Probability_of_Loss$estimated_average_cost[4]/`Total Records`)

df_5 <- filter(df, `Total Records` %in% 500000:1000000) 
df_5 <- df_5 %>% mutate(df_5, cost_per_record = Probability_of_Loss$estimated_average_cost[5]/`Total Records`)

df_6 <- filter(df, `Total Records` %in% 1000000:5000000)
df_6 <- df_6 %>% mutate(df_6, cost_per_record = Probability_of_Loss$estimated_average_cost[6]/`Total Records`)
  
df_7 <- filter(df, `Total Records` %in% 5000000:10000000) 
df_7 <- df_7 %>% mutate(df_7, cost_per_record = Probability_of_Loss$estimated_average_cost[7]/`Total Records`)

df_8 <- filter(df, `Total Records` %in% 10000000:50000000) 
df_8 <- df_8 %>% mutate(df_8, cost_per_record = Probability_of_Loss$estimated_average_cost[8]/`Total Records`)

df_9 <- filter(df, `Total Records` %in% 50000000:100000000)
df_9 <- df_9 %>% mutate(df_9, cost_per_record = Probability_of_Loss$estimated_average_cost[9]/`Total Records`)

df_10 <- filter(df, `Total Records` %in% 100000000:500000000)
df_10 <- df_10 %>% mutate(df_10, cost_per_record = Probability_of_Loss$estimated_average_cost[10]/`Total Records`)

df_11 <- filter(df, `Total Records` %in% 500000000:1000000000)
df_11 <- df_11 %>% mutate(df_11, cost_per_record = Probability_of_Loss$estimated_average_cost[11]/`Total Records`)

df_12 <- filter(df, `Total Records` > 1000000000)
df_12 <- df_12 %>% mutate(df_12, cost_per_record = Probability_of_Loss$estimated_average_cost[12]/`Total Records`)

sum(mean(df_1$cost_per_record),
    mean(df_2$cost_per_record),
    mean(df_3$cost_per_record),
    mean(df_4$cost_per_record),
    mean(df_5$cost_per_record),
    mean(df_6$cost_per_record),
    mean(df_7$cost_per_record),
    mean(df_8$cost_per_record),
    mean(df_9$cost_per_record),
    mean(df_10$cost_per_record),
    mean(df_11$cost_per_record),
    mean(df_12$cost_per_record)
    ) / 12

df_complete <- bind_rows(df_1,df_2,df_3,df_4,df_5,df_6,df_7,df_8,df_9, df_10, df_11, df_12)

df <- mutate(df, estimated_cost = df$`Total Records` * 67)

quant <- as_tibble(as.list(quantile(df$estimated_cost, probs=c(.1, .98))))

df <- df %>% filter(estimated_cost > quant$`10%` & estimated_cost < quant$`98%`)
mean(df$estimated_cost)

ggplot(data = df, mapping = aes(x = `Date Made Public`, y = estimated_cost)) +
  geom_point() +
  theme_bw(base_size = 15) +
  scale_x_date(date_labels = "%Y") +
  scale_y_continuous(labels = scales::label_number_si())

df %>% 
  group_by(`Year of Breach`) %>% 
  summarise(mean(estimated_cost)) %>%
  filter(`Year of Breach` != "2020") %>%
  ggplot(aes(x = `Year of Breach`, y = `mean(estimated_cost)`, group = 1)) +
  geom_point(size=6) +
  geom_line(color="black") +
  scale_y_continuous(labels = scales::label_number_si(prefix="$")) +
  theme_bw(base_size = 15)
  
lower <- 10000000
upper <- max(df$estimated_cost)
prob <- df %>% 
  group_by(`Year of Breach`) %>%
  summarise(prob = sum(estimated_cost > lower & estimated_cost < upper, na.rm = TRUE) / n())

RUNS <- 10000
simulations <- tibble(id = set_names(paste0("sim", 1:RUNS)),
                      probability = rpert(RUNS, min(prob$prob), mean(prob$prob), max(prob$prob), 5),
                     cost = rpert(RUNS, lower, (lower+upper/2), upper, 5),
                    risk = probability * cost
                    )

ale_frame <- mutate(simulations, prob = 1 - percent_rank(simulations$risk))
ale_frame <- ale_frame[order(simulations$risk),]
ggplotly(ggplot(ale_frame, mapping = aes(x = risk, y = prob))+
  geom_path() + 
  geom_vline(xintercept = 40000000, linetype = "dashed") +
  scale_y_continuous(labels = scales::percent)+
  labs(y = "Probability of Loss")+
  labs(x = "Annualized Loss Exposure (ALE)")+
  scale_x_continuous(labels = scales::label_number_si(prefix="$"))+
  theme_bw())


simulations %>%
  ggplot(aes(x=risk)) +
  geom_density(adjust = 2) +
  scale_x_continuous(labels = scales::label_number_si(prefix="$")) +
  labs(x = "Annualized Loss Exposure (ALE)", y="")+
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank()
        )


