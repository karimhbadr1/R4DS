
# Introduction ------------------------------------------------------------
library(tidyverse)

# Creating Factors --------------------------------------------------------
x1<-c("Dec","Apr","Jan","Mar")
x2<-c("Dec","Apr","Jam","Mar")
sort(x1)
month_levels<-c(
  "Jan","Feb","Mar","Apr","May","Jun",
  "Jul","Aug","Sep","Oct","Nov","Dec"
)
y1<-factor(x1,levels=month_levels)
y1
sort(y1)
y2 <- factor(x2, levels = month_levels)
y2
y2<-parse_factor(x2,levels=month_levels)
factor(x1)
f1<-factor(x1,levels=unique(x1))
f1
f2<-factor(x1)
f2
f2 %>% fct_inorder()
levels(f2)


# General Social Survey ---------------------------------------------------

gss_cat
head(gss_cat)
view(gss_cat)  
levels(gss_cat$race)
gss_cat %>% 
  count(race)

ggplot(gss_cat,aes(race))+
  geom_bar()+
  scale_x_discrete(drop=FALSE)

ggplot(gss_cat,aes(rincome))+
  geom_bar()+
  scale_x_discrete(drop=FALSE)+
  coord_flip()

gss_cat %>% 
  mutate(rincome = fct_recode(rincome,"Less than $1000" = "Lt $1000")) %>% 
  mutate(rincome_na= rincome %in% c("Not applicable","Refused","Don't know","No answer")) %>% 
  ggplot(aes(x=rincome,fill=rincome_na))+
  geom_bar()+
  scale_x_discrete("Respondent's Income") +
  scale_y_continuous("Number of Respondents") +
  scale_fill_manual(values = c("FALSE" = "black", "TRUE" = "gray"))+
  theme(legend.position = "None")+
  coord_flip()

gss_cat %>%
  filter(!rincome %in% c("Not applicable")) %>%
  mutate(rincome = fct_recode(rincome,
                              "Less than $1000" = "Lt $1000"
  )) %>%
  mutate(rincome_na = rincome %in% c("Refused", "Don't know", "No answer")) %>%
  ggplot(aes(x = rincome, fill = rincome_na)) +
  geom_bar() +
  coord_flip() +
  scale_y_continuous("Number of Respondents", labels = scales::comma) +
  scale_x_discrete("Respondent's Income") +
  scale_fill_manual(values = c("FALSE" = "black", "TRUE" = "gray")) +
  theme(legend.position = "None")

gss_cat %>% 
  count(relig) %>% 
  arrange(desc(n))

gss_cat %>% 
  count(partyid) %>% 
  arrange(desc(n))

levels(gss_cat$denom)

gss_cat %>%
  filter(!denom %in% c(
    "No answer", "Other", "Don't know", "Not applicable",
    "No denomination"
  )) %>% 
  count(relig)

gss_cat %>%
  count(relig, denom) %>%
  ggplot(aes(x = relig, y = denom)) +
  geom_point()+
  theme(axis.text.x = element_text(angle = 90))+
  coord_flip()


# Modifying Factor Order --------------------------------------------------

relig_summary <- gss_cat %>%
  group_by(relig) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

ggplot(relig_summary, aes(tvhours, relig)) + geom_point()

levels(relig_summary$relig)
A<-fct_reorder(relig_summary$relig,relig_summary$tvhours)

ggplot(relig_summary, aes(tvhours, fct_reorder(relig, tvhours))) +
  geom_point()

relig_summary %>%
  mutate(relig = fct_reorder(relig, tvhours)) %>%
  ggplot(aes(tvhours, relig)) +
  geom_point()

rincome_summary<-gss_cat %>% 
  group_by(rincome) %>% 
  summarise(
    age=mean(age,na.rm=TRUE)
  )

ggplot(rincome_summary,aes(age,fct_reorder(rincome,age)))+geom_point()

ggplot(rincome_summary,aes(age,rincome))+geom_point()

ggplot(rincome_summary, aes(age, fct_relevel(rincome, "Not applicable"))) +
  geom_point()

by_age <- gss_cat %>%
  filter(!is.na(age)) %>%
  count(age, marital) %>%
  group_by(age) %>%
  mutate(prop = n / sum(n))

ggplot(by_age, aes(age, prop, colour = marital)) +
  geom_line(na.rm = TRUE)

ggplot(by_age, aes(age, prop, colour = marital, age)) +
  geom_line() 

ggplot(by_age, aes(age, prop, colour = fct_reorder2(marital, age, prop))) +
  geom_line() +
  labs(colour = "marital")

gss_cat %>%
  mutate(marital = marital %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(marital)) +
  geom_bar()

gss_cat %>%
  mutate(marital = marital %>% fct_infreq()) %>%
  ggplot(aes(marital)) +
  geom_bar()

summary(gss_cat["tvhours"])

gss_cat %>%
  group_by(relig) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = median(tvhours, na.rm = TRUE),
    n = n()
  )


# Modifying Factor Levels -------------------------------------------------

gss_cat %>% count(partyid)

gss_cat %>%
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong"    = "Strong republican",
                              "Republican, weak"      = "Not str republican",
                              "Independent, near rep" = "Ind,near rep",
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak"        = "Not str democrat",
                              "Democrat, strong"      = "Strong democrat"
  )) %>%
  count(partyid)

gss_cat %>%
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong"    = "Strong republican",
                              "Republican, weak"      = "Not str republican",
                              "Independent, near rep" = "Ind,near rep",
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak"        = "Not str democrat",
                              "Democrat, strong"      = "Strong democrat",
                              "Other"                 = "No answer",
                              "Other"                 = "Don't know",
                              "Other"                 = "Other party"
  )) %>%
  count(partyid)

gss_cat %>%
  mutate(partyid = fct_collapse(partyid,
                                other = c("No answer", "Don't know", "Other party"),
                                rep = c("Strong republican", "Not str republican"),
                                ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                                dem = c("Not str democrat", "Strong democrat")
  )) %>%
  count(partyid)


gss_cat %>% 
count(relig)

gss_cat %>%
  mutate(relig = fct_lump(relig)) %>%
  count(relig)

gss_cat %>%
  mutate(relig = fct_lump(relig, n = 10)) %>%
  count(relig, sort = TRUE)

gss_cat %>%
  mutate(relig = fct_lump(relig, n = 10)) %>%
  count(relig, sort = TRUE) %>%
  print(n = Inf)


gss_cat_1<-gss_cat %>%
  mutate(partyid = fct_collapse(partyid,
                                other = c("No answer", "Don't know", "Other party"),
                                rep = c("Strong republican", "Not str republican"),
                                ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                                dem = c("Not str democrat", "Strong democrat")
  ))

gss_cat_1<-gss_cat_1 %>% 
  group_by(year) %>% 
  count(partyid)

ggplot(gss_cat_1,aes(year,n,colour=partyid))+geom_line()

gss_cat %>%
mutate(
  partyid =
    fct_collapse(partyid,
                 other = c("No answer", "Don't know", "Other party"),
                 rep = c("Strong republican", "Not str republican"),
                 ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                 dem = c("Not str democrat", "Strong democrat")
    )
) %>%
  count(year, partyid)%>%
  group_by(year) %>%
  mutate(p = n / sum(n)) %>% 
  ggplot(aes(
    x = year, y = p,
    colour = fct_reorder2(partyid, year, p)
  )) +
  geom_point() +
  geom_line() +
  labs(colour = "Party ID.")

gss_cat %>%
  mutate(
    rincome =
      fct_collapse(
        rincome,
        `Unknown` = c("No answer", "Don't know", "Refused", "Not applicable"),
        `Lt $5000` = c("Lt $1000", str_c(
          "$", c("1000", "3000", "4000"),
          " to ", c("2999", "3999", "4999")
        )),
        `$5000 to 10000` = str_c(
          "$", c("5000", "6000", "7000", "8000"),
          " to ", c("5999", "6999", "7999", "9999")
        )
      )
  ) %>%
  ggplot(aes(x = rincome)) +
  geom_bar() +
  coord_flip()
