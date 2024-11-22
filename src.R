  
# ==============================================================================
# R Script for Quarto Shiny file (slar.qmd).                                   |
# by Fazliddin Sultonov - Canada Life                                          |
# ==============================================================================
#
# Read source for Variables and functions ======================================
#rm(list = ls())
source(here::here("src","var_func.R"))
#
# Connect with DB = Reporting ==================================================
conn <- dbConnect(odbc(),
                  Driver = driver,
                  Server = server,
                  Database = db,
                  Trusted_Connection = tc)

# Table: [dbo].[CLT_Bank_Holiday] ----------------------------------------------
tblBH <- tbl(conn, "CLT_Bank_Holiday") %>% 
  select(
    Holiday_Date,
    Tenant_Id
  ) %>% dplyr::collect()
#
banc_hol <- tblBH$Holiday_Date
# tail(holidays)
rm(tblBH)
#
# Tabelle: [dbo].[CLT_Business_Case] -------------------------------------------
raw_bc <- tbl(conn, "CLT_Business_Case") %>%
  mutate(
    Creation_Date = as.Date(Creation_Date),
    Finish_Date = as.Date(Finish_Date),
    Original_Target_Date = as.Date(Original_Target_Date),
    Escalation_Date = as.Date(Escalation_Date),
    Last_Change_Date = as.Date(Last_Change_Date)
  ) %>%
  # filter(
  #   (Work_Type_Group_EN %in% admin_wtg & (is.na(Finish_Date))) |
  #   (!is.na(Finish_Date) & (Finish_Date >= start_date &  Finish_Date <= end_date))
  # ) %>%
  
  filter(
    Work_Type_Group_EN %in% admin_wtg  & 
    (CLE_Status_EN %in% c("Open", "On Hold") | 
    # & (Escalation_Date >= start_date & Escalation_Date <= end_date)) |
    (CLE_Status_EN %in% c("Closed", "Cancelled") & !is.na(Finish_Date) & Finish_Date >= start_date & Finish_Date <= end_date))
  ) %>%
  select(

    Business_Case_Id
    ,Policy_Number
    ,Creation_Date
    ,Original_Target_Date
    ,Escalation_Date
    ,Finish_Date
    ,CLE_Status_EN
    # ,CLE_Status_DE
    ,Work_Type_Group
    ,Work_Type_Group_EN
    # ,Work_Type_Group_DE
    # ,Department_Name
    # ,Department_EN
    # ,Department_DE
    ,Work_Type_EN
    # ,Work_Type_DE
    ,Last_Change_User
    ,Last_Change_Date
    ,Assigning_User
    ,Created_By_User
    ,Finished_By_User
    ,Is_Escalated
    ,Priority_EN

  )  %>%
  collect()

# Disconnect with DB = Reporting ===============================================
dbDisconnect(conn)
# 
# raw_bc %>%
#   filter(
#     (CLE_Status_EN == "Open") & (Escalation_Date >= as.Date("2024-01-15")) & (Escalation_Date <= as.Date("2024-01-20"))
#    ) %>% count() 
# 
# 
# raw_bc %>%
#   group_by(
#     CLE_Status_EN
#   ) %>%
#   summarise(
#     cnt = n()
#   ) %>%  filter(CLE_Status_EN=="Open") %>%  select(cnt) -> son
# # 
# raw_bc %>%
#   filter(
#     CLE_Status_EN == "Open"
#   ) %>% count() %>% .$n
# raw_bc %>%
#   group_by(
#     Work_Type_Group_EN, CLE_Status_EN
#   ) %>%
#   summarise(
#     cnt = n()
#   ) %>%
#   pivot_wider(
#     names_from = c( "CLE_Status_EN"),
#     values_from = "cnt"
#   ) %>%
#   mutate(
#     Total = sum(Closed, `On Hold`, Open, na.rm = TRUE)
#   ) %>%  replace(is.na(.), 0) -> tblStatus
# 
# 
# 
# raw_bc %>% 
#   group_by(
#     CLE_Status_EN
#   ) %>% summarise(cnt=n()) %>% 
#   filter(CLE_Status_EN == "Open") %>% select(cnt)

# names(raw_bc)
# setdiff(names(raw_bc), "Policy_Number") 
# names(raw_bc)[[3]]


### Plot ===============================================================

# raw_bc %>% 
#   group_by(
#     Work_Type_Group_EN, CLE_Status_EN
#   ) %>% 
#   summarise(
#     cnt = n()
#   ) %>% 
#   pivot_wider(
#     names_from = c( "CLE_Status_EN"),
#     values_from = "cnt") %>% 
#   mutate(
#     Total = sum(Closed, `On Hold`, Open, na.rm = TRUE)
#   ) %>% 
#   replace(is.na(.), 0) # -> tblStatus
# 
# # Docchart ==============
# raw_bc %>%
#   group_by(
#     Work_Type_Group_EN, CLE_Status_EN
#   ) %>%
#   summarise(
#     cnt = n()
#   ) %>%
#   mutate(
#     # Work_Type_Group_EN= as.factor(Work_Type_Group_EN),
#     cnt = as.numeric(as.character(cnt)),
#     CLE_Status_EN = as.factor(CLE_Status_EN)
#   ) %>%
#   ggdotchart(
#     x="cnt", y= "Work_Type_Group_EN",
#     group = "CLE_Status_EN", color = "CLE_Status_EN",
#     palette = pcolors,
#     shape = 18,
#     size = 2.9,
#     rotate = TRUE,
#     sorting = "descending",
#     ggtheme = theme_bw(),
#     xlab = "Count of Tasks",
#     ylab = "Departments",
#     y.text.col = TRUE,
#   ) +
#   theme(axis.text.x = element_text(angle = 25, hjust = 1)) +
#   labs(color = "Status")
# 
# 
# # Barplot ======================
# raw_bc %>% 
#   filter(
#     !Work_Type_Group_EN %in% c("COLDIS","Quality Management","Others","AGMA",
#                                "Scanning","Technical Alteration of Contract")
#   ) %>% 
#   group_by(
#     Work_Type_Group_EN, CLE_Status_EN
#   ) %>% 
#   summarise(
#     cnt = n()
#   ) %>%  
#   ggplot( aes(x = Work_Type_Group_EN, y = cnt, fill = CLE_Status_EN)) +
#   # ggplot( aes(y = Work_Type_Group_EN, x = cnt, fill = CLE_Status_EN)) +
#   geom_bar(stat = "identity", position = "fill") +  
#   guides(fill = guide_legend(title = "Status")) +
#   scale_fill_manual(values=pcolors) +
#   labs(title = "BC Status by Departments",
#        x = "Department",
#        y = "BC Status") +
#   theme(axis.text.x=element_text(angle = -15, hjust = 0)) +
#   scale_y_continuous(labels = scales::percent_format())
# 
# # Plot Closed
# raw_bc %>% 
#   filter(
#     CLE_Status_EN == "Closed"
#   ) %>%
#   group_by(
#     Work_Type_Group_EN,  CLE_Status_EN
#   ) %>% 
#   summarise(anz = n()) %>%
#   
#   ggplot(aes(y = Work_Type_Group_EN, x = sort(anz), fill = CLE_Status_EN)) +
#   geom_col() +
#   geom_text(aes(label = anz), size = 3, position = position_stack(vjust = 0.5)) +
#   scale_fill_manual(values=c("mediumseagreen","lightcoral")) +
#   labs(title = "Closed Tasks",
#        x = "Count of Closed",
#        y = "Department") # -> plot_closed
# 
# # Plot Open
# status <- c("Closed", "Open", "On Hold", "Cancelled")
# raw_bc %>%
#   filter(
#     CLE_Status_EN %in% status
#   ) %>%
#   group_by(
#     Work_Type_Group_EN,  CLE_Status_EN
#   ) %>%
#   summarise(anz = n()) %>%
# 
#   ggplot(aes(y = Work_Type_Group_EN, x = sort(anz), fill = CLE_Status_EN)) +
#   geom_col() +
#   geom_text(aes(label = anz), size = 3, position = position_stack(vjust = 0.5)) +
#   scale_fill_manual(values=pcolors) +
#   labs(title = "Closed Tasks",
#        x = "Count of Closed",
#        y = "Department") + 
#   theme(legend.position = c(0.95, 0.1))# -> plot_closed

# # 
# raw_bc %>%
#   group_by(
#     Work_Type_Group_EN, CLE_Status_EN
#   ) %>%
#   summarise(
#     cnt = as.numeric(n())
#   ) %>% 
#   mutate(
#     # Work_Type_Group_EN= as.factor(Work_Type_Group_EN),
#     cnt = cnt,
#     CLE_Status_EN = as.factor(CLE_Status_EN)
#   ) -> dotplot
# 
# dotchart(dotplot$cnt,
#          label = dotplot$Work_Type_Group_EN,
#          groups = dotplot$CLE_Status_EN,
#          color =  pcolors[dotplot$CLE_Status_EN], #"darkgrey",
#          cex = 1.0,  pch = 18, 
#          yaxt='n',
#          # xlim = c(min(dotplot$cnt), max(dotplot$cnt), by=500),
#          
#          gdata = tapply(dotplot$cnt, dotplot$CLE_Status_EN, mean), # für sum, mean, ...
#          gcolor = pcolors,
#          gpch = 19,
#          xlab = "Count of Tasks", ylab = "Departments by Tasks",
# ) + 
#   axis(
#     side = 1, 
#     at = seq(from = 0, to = max(dotplot$cnt), by=500),
#     # labels = min(t$cnt): max(t$cnt),
#     cex.axis =1)

raw_bc %>%
  filter(
    (CLE_Status_EN %in% c("Open", "On Hold")) |
    (CLE_Status_EN %in% c("Closed", "Cancelled") & !is.na(Finish_Date) & 
       (Finish_Date >= as.Date("2024-03-25") & Finish_Date <= as.Date("2024-03-31")))  
  ) -> df 

df %<>% select(1:10)


# Green(SLA) - Red(OutSLA) berechnen =====================================================================
df %>% 
  mutate(
    BC_Status = case_when(
      CLE_Status_EN == "Closed" & !is.na(Finish_Date) & 
        (Finish_Date >= as.Date("2024-03-25") & Finish_Date <= as.Date("2024-03-31") & Finish_Date <= Escalation_Date)  ~ "Closed_Green",
      CLE_Status_EN == "Closed" &  !is.na(Finish_Date) & Finish_Date > Escalation_Date  ~ "Closed_Red",
      CLE_Status_EN == "Open" & Escalation_Date >= as.Date("2024-04-31") ~ "Open_Green",
      CLE_Status_EN == "Open" & Escalation_Date <= as.Date("2024-04-31") ~ "Open_Red",
      CLE_Status_EN == "On Hold" & Escalation_Date >= as.Date("2024-03-31") ~ "On_Hold_Green",
      CLE_Status_EN == "On Hold" & Escalation_Date <= as.Date("2024-03-31") ~ "On_Hold_Red",
      TRUE ~ CLE_Status_EN)
    ) -> res  
  # group_by(
  #   Work_Type_Group_EN, BC_Status
  # ) %>%
  # summarise(
  #   cn = n()
  #   ) %>%
  # pivot_wider(
  #   names_from = "BC_Status",
  #   values_from = "cn"
  # ) %>%
  # replace(is.na(.), 0) %>% 
  # select(
  #   Open_Green,
  #   Open_Red,
  #   On_Hold_Red,
  #   On_Hold_Green,
  #   Closed_Green,
  #   Closed_Red,
  #   Cancelled
  # ) -> data_task

# Closed----------------------------------------------------------------------
res %>% 
  filter(
    BC_Status %in% c("Closed_Green", "Closed_Red")
  ) %>% 
  group_by(
    Work_Type_Group_EN,  BC_Status
  ) %>% 
  summarise(anz = n()) %>% 
  ggplot(aes(y = Work_Type_Group_EN, x = anz, fill = BC_Status)) +
  geom_col() +
  geom_text(aes(label = anz), size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values=c("mediumseagreen","lightcoral")) +
  labs(title = "Closed Tasks",
       x = "Count",
       y = "Department") +
  theme(legend.position = c(0.95, 0.06)) #-> plot_closed
# Open----------------------------------------------------------------------
res %>% 
  filter(
    BC_Status %in% c("Open_Green", "Open_Red")
  ) %>% 
  group_by(
    Work_Type_Group_EN,  BC_Status
  ) %>% 
  summarise(anz = n()) %>% 
  ggplot(aes(y = Work_Type_Group_EN, x = anz, fill = BC_Status)) +
  geom_col() +
  geom_text(aes(label = anz), size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values=c("skyblue","lightcoral")) +
  labs(title = "Open Tasks",
       x = "Count",
       y = "Department") +
  theme(legend.position = c(0.95, 0.06)) #-> plot_open

# On Hold----------------------------------------------------------------------
res %>% 
  filter(
    BC_Status %in% c("On_Hold_Green", "On_Hold_Red")
  ) %>% 
  group_by(
    Work_Type_Group_EN,  BC_Status
  ) %>% 
  summarise(anz = n()) %>% 
  ggplot(aes(y = Work_Type_Group_EN, x = anz, fill = BC_Status)) +
  geom_col() +
  geom_text(aes(label = anz), size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values=c("orange","lightcoral")) +
  labs(title = "On Hold Tasks",
       x = "Count",
       y = "Department") +
  theme(legend.position = c(0.95, 0.06)) #-> plot_on_hold

# Task Table----------------------------------------------------------------------
# install.packages("gtExtras")
# library(gtExtras)
res %>%
  group_by(
    Work_Type_Group_EN, BC_Status
  ) %>%
  summarise(
    cn = n()
  ) %>%
  pivot_wider(
    names_from = "BC_Status",
    values_from = "cn"
  ) %>%
  mutate(
    Open_Green_Per = round(Open_Green/sum(Open_Green, Open_Red, na.rm = TRUE) * 100, digits = 2),
    Open_Red_Per = round(Open_Red/sum(Open_Green, Open_Red, na.rm = TRUE) * 100, digits = 2),
    Closed_Green_Per = round(Closed_Green/sum(Closed_Green, Closed_Red, na.rm = TRUE) * 100, digits = 2),
    Closed_Red_Per = round(Closed_Red/sum(Closed_Green, Closed_Red, na.rm = TRUE) * 100, digits = 2),
    OnHold_Green_Per = round(On_Hold_Green/sum(On_Hold_Green, On_Hold_Red, na.rm = TRUE) * 100, digits = 2),
    OnHold_Red_Per = round(On_Hold_Red/sum(On_Hold_Green, On_Hold_Red, na.rm = TRUE) * 100, digits = 2)
  ) %>% 
  replace(is.na(.), 0) 
  # select(
  #   as.integer(Work_Type_Group_EN),
  #   Open_Green,
  #   Open_Red,
  #   Open_Green_Per,
  #   Open_Red_Per,
  #   On_Hold_Red,
  #   On_Hold_Green,
  #   OnHold_Green_Per,
  #   OnHold_Red_Per,
  #   Closed_Green,
  #   Closed_Red,
  #   Closed_Green_Per,
  #   Closed_Red_Per,
  #   Cancelled
  # ) %>%
  # gt() %>%
  # gt_plt_bar_pct(column = Open_Green_Per, scaled = FALSE,
  #                fill = "blue", background = "lightblue")
res %>% 
  filter(
    BC_Status %in% c("Closed_Green", "Closed_Red")
  ) %>% 
  group_by(
    Work_Type_Group_EN, BC_Status
  ) %>% 
  summarise(
    cn = n()
  ) %>% 
  mutate(
    cum_sum = sum(cn)
  ) -> clos
  # mutate(
  #   status_pct = round(cn/cum_sum * 100, digits = 0)
  # ) %>% 
  # group_by(
  #   Work_Type_Group_EN
  # ) %>% 
  # summarise(
  #   pct = list(status_pct)
  # ) 
  # mutate(
  #  pct1 = case_when(
  #    pct = 100 ~ list(100,0),
  #    pct = 0, list(0,100),
  #    TRUE ~ pct
  #  )
  # )
# 


# Closed Percent Tabelle +++++++++++++++++++++++++++++++++++++++++++++++
res %>% 
  filter(
    BC_Status %in% c("Closed_Green", "Closed_Red")
  ) %>% 
  group_by(
    Work_Type_Group_EN, BC_Status
  ) %>%
  summarise(
    cn = n()
  ) %>% 
  mutate(
    cum_sum = sum(cn)
  ) %>%
  mutate(
     status_pct = round(cn/cum_sum * 100, digits = 0)
  ) %>% 
  group_by(
    Work_Type_Group_EN
  ) %>%
  summarise(
    pct = list(status_pct)
  ) %>% rowwise() %>% 
  mutate(
    pct = list(case_when(
      pct[][1]==100 ~ as.numeric(c(100,0)),
      pct[][1]==0 ~ as.numeric(c(0,100)),
      TRUE ~ pct
    ))
  ) -> prc_clos

# Open Percent Tabelle +++++++++++++++++++++++++++++++++++++++++++++++
res %>% 
  filter(
    BC_Status %in% c("Open_Green", "Open_Red")
  ) %>% 
  group_by(
    Work_Type_Group_EN, BC_Status
  ) %>%
  summarise(
    cn = n()
  ) %>% 
  mutate(
    cum_sum = sum(cn)
  ) %>%
  mutate(
    status_pct = round(cn/cum_sum * 100, digits = 0)
  ) %>% 
  group_by(
    Work_Type_Group_EN
  ) %>%
  summarise(
    pct = list(status_pct)
  ) %>% rowwise() %>% 
  mutate(
    pct = list(case_when(
      pct[][1]==100 ~ as.numeric(c(100,0)),
      pct[][1]==0 ~ as.numeric(c(0,100)),
      TRUE ~ pct
    ))
  ) -> prc_open 


# On Hold Percent Tabelle +++++++++++++++++++++++++++++++++++++++++++++++
res %>% 
  filter(
    BC_Status %in% c("On_Hold_Green", "On_Hold_Red")
  ) %>% 
  group_by(
    Work_Type_Group_EN, BC_Status
  ) %>%
  summarise(
    cn = n()
  ) %>% 
  mutate(
    cum_sum = sum(cn)
  ) %>%
  mutate(
    status_pct = round(cn/cum_sum * 100, digits = 0)
  ) %>% 
  group_by(
    Work_Type_Group_EN
  ) %>%
  summarise(
    pct = list(status_pct)
  ) %>% rowwise() %>% 
  mutate(
    pct = list(case_when(
      pct[][1]==100 ~ as.numeric(c(100,0)),
      pct[][1]==0 ~ as.numeric(c(0,100)),
      TRUE ~ pct
    ))
  ) -> prc_onhold 

check_list(clos$pct)
clos$pct

prc_clos %>% 
  gt() %>%
  gt_plt_bar_stack(
    position = "fill",
    column = pct,
    palette = c("mediumseagreen", "lightcoral"),
    labels = c("Closed_Green", "Closed_Red"),
    fmt_fn = scales::label_percent(scale=1),
    width = 75
  )

prc_open %>% 
  gt() %>%
  gt_plt_bar_stack(
    position = "fill",
    column = pct,
    palette = c("deepskyblue", "lightcoral"),
    labels = c("Open_Green", "Open_Red"),
    fmt_fn = scales::label_percent(scale=1),
    width = 75
  )

prc_onhold %>% 
  gt() %>%
  gt_plt_bar_stack(
    position = "fill",
    column = pct,
    palette = c("orange", "lightcoral"),
    labels = c("On Hold_Green", "On Hold_Red"),
    fmt_fn = scales::label_percent(scale=1),
    width = 75
  )



clos$pct[7] 
clos$pct[7] == 100
as.numeric(unlist(clos$pct))
a =list(2,0)
a = list(as.double(as.character(c(0,2))))
a[[1]][2] = as.double(as.character(0))
a[2]
a



check_list <- function(x){
  
  for (i in 1:length(x)){
    
    if (x[[i]][1] == 100) {
      x[[i]][2] <- as.numeric(0)
    } else if (x[[i]][1] == 0){
      x[[i]][2] <- as.numeric(100)
    } else {
      
    }
  } 
}


for (i in 1:length(clos$pct)){
  if (clos$pct[[i]][1] == 100) {
    print(TRUE) 
    #clos$pct[[i]][2] <- as.numeric(0)
    
  } else if (clos$pct[[i]][1] == 0){
    print(FALSE)
  }
  print("OK")
} 

a[][1]
a

class(clos$pct[[5]][2])
clos$pct[[5]][2] == 8




case_when(
  clos$pct[][1] == 100,clos$pct[2][as.double(as.character(0))] ,clos$pct
  # clos$pct[2] == NULL ~ clos$pct[1][as.double(as.character(100))],
  # TRUE ~ clos$pct
  )
clos %>% select(pct[1])
clos$pct

# install.packages("svglite")
library(svglite)
library(gt)
library(gtExtras)
res %>%
  filter(
    CLE_Status_EN %in% c("Closed", "Open", "On Hold")
  ) %>% 
  group_by(
    Work_Type_Group_EN, CLE_Status_EN
  ) %>%
  summarise(
    cn = n()
  ) %>% 
  mutate(
    cum_sum = sum(cn)
  ) %>% 
  mutate(
    bar_col = round(cn/cum_sum * 100, digits = 0)
  ) %>% 
  group_by(
    Work_Type_Group_EN
  ) %>% 
  summarise(
    task = list(bar_col)
  ) %>% 
  rename(
    Department = Work_Type_Group_EN
  ) -> test
  # rowwise() %>% 
  # mutate(
  #   pct = list(case_when(
  #     task[][1]==100 ~ as.numeric(c(100,0)),
  #     task[][1]==0 ~ as.numeric(c(0,100)),
  #     TRUE ~ task
  #   ))) %>% 
test %>% 
  gt() %>%
  gt_plt_bar_stack(
    column = task,
    position = "fill",
    palette = c("mediumseagreen", "orange", "deepskyblue"),
    labels = c("Closed", "On Hold", "Open"),
    fmt_fn = scales::label_percent(scale=1),
    width = 75
  )
  
  

test1 <- 
test %>% 
  group_by(
    Work_Type_Group_EN
  ) %>% 
  summarise(
    task = list(bar_col)
  )      
test1 %>%   
  gt() %>% 
  gt_plt_bar_pct(
    column = task,
    scaled = FALSE,
    palette = c("mediumseagreen", "orange", "skyblue"),
    labels = c("Closed", "On Hold", "Open"),
  ) 
 
  
ex_df <- dplyr::tibble(
    x = c("Example 1","Example 1",
          "Example 1","Example 2","Example 2","Example 2",
          "Example 3","Example 3","Example 3","Example 4","Example 4",
          "Example 4"),
    measure = c("Measure 1","Measure 2",
                "Measure 3","Measure 1","Measure 2","Measure 3",
                "Measure 1","Measure 2","Measure 3","Measure 1","Measure 2",
                "Measure 3"),
    data = c(30, 20, 50, 30, 30, 40, 30, 40, 30, 30, 50, 20)
  )
tab_df <- ex_df %>%
  group_by(x) %>%
  summarise(list_data = list(data))
tab_df
test
ex_tab <- tab_df %>%
  gt() %>%
  gt_plt_bar_stack(column = list_data)

save(df, res, raw_bc, file = "C:/Users/sultonf/OneDrive - GWLE/Desktop/unterlagen_fazi/mydoc.RData")
