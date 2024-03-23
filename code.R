### Loading Packages
  
library(tidyverse)
library(readr)
library(readxl)
library(tidyr)
library(dplyr)
library(stargazer)
library(ggrepel)
library(lubridate)
library(plm)
library(tidysynth)
library(Synth)
library(WDI)
library(haven)
library(xgboost)
library(DiagrammeR)


### Data Entry

wdi_data <- read_csv("C:/Users/Shreejit/Desktop/Honors Capstone/Data/WDIData.csv")

pwt_data <- read_dta("C:/Users/Shreejit/Desktop/Honors Capstone/Data/pwt1001.dta") %>%
  filter(countrycode %in% c("AFG", "BGD", "BRN", "BTN", "KHM", "IND", "IDN", "LAO", "MYS", "MDV", "MMR", "NPL", "PAK", "PHL", "SGP", "LKA", "THA", "TLS", "VNM"))


### Data Tidying
#### Dependent Variables: 
Trade Volume (% of GDP): NE.TRD.GNFS.ZS \\
Foreign direct investment, net inflows (% of GDP): BX.KLT.DINV.WD.GD.ZS \\
Unemployment total (% of total labor force): SL.UEM.TOTL.ZS \\

#### Control Variables used:
Official exchange rate (LCU per US$, period average): PA.NUS.FCRF \\
GDP per capita growth (annual %): NY.GDP.PCAP.KD.ZG \\
Inflation, consumer prices (annual %): FP.CPI.TOTL.ZG \\
Urban Population (% of total population): SP.URB.TOTL.IN.ZS \\
GDP Growth (annual %): NY.GDP.MKTP.KD.ZG \\
Total natural resources rents (% of GDP): NY.GDP.TOTL.RT.ZS \\
Industry, value added (% of GDP): NV.IND.TOTL.ZS \\
Net ODA received (% of GNI): DT.ODA.ODAT.GN.ZS \\
Population Density: EN.POP.DNST \\
Age dependency ratio (% of working-age population): SP.POP.DPND \\
Broad money (% of GDP): FM.LBL.BMNY.GD.ZS \\
Production - Agriculture, forestry, and fishing, value added (% of GDP): NV.AGR.TOTL.ZS \\
Labor force participation rate, total (% of total population ages 15+): SL.TLF.CACT.ZS \\
Fertility rate, total (births per woman): SP.DYN.TFRT.IN \\
Households and NPISHs final consumption expenditure (% of GDP): NE.CON.PRVT.ZS \\
General government final consumption expenditure (% of GDP): NE.CON.GOVT.ZS \\
Women Business and the Law Index Score (scale 1-100): SG.LAW.INDX \\


wdi_clean <- wdi_data %>%
  filter(`Indicator Code` %in% c("NE.TRD.GNFS.ZS", "PA.NUS.FCRF", "NY.GDP.PCAP.KD.ZG", "SP.POP.TOTL", "BX.KLT.DINV.WD.GD.ZS", "FP.CPI.TOTL.ZG", "SP.URB.TOTL.IN.ZS", "NY.GDP.MKTP.KD.ZG", "SL.UEM.TOTL.ZS", "NY.GDP.TOTL.RT.ZS", "NV.IND.TOTL.ZS", "DT.ODA.ODAT.GN.ZS", "EN.POP.DNST", "SE.PRM.ENRR", "SP.POP.DPND", "FM.LBL.BMNY.GD.ZS", "NV.AGR.TOTL.ZS", "SL.TLF.CACT.ZS", "SP.DYN.TFRT.IN", "NE.CON.PRVT.ZS", "SP.POP.GROW", "NE.CON.GOVT.ZS", "SG.LAW.INDX", "SL.EMP.WORK.ZS", "GC.TAX.IMPT.ZS", "GC.TAX.INTT.RV.ZS"))%>%
  gather(year, value, "1960":"2021") %>%
  spread(`Indicator Code`, value, fill = NA) %>%
  arrange(`Country Code`) %>%
  #mutate(year = as.Date(year, format="%Y")) %>%
  #mutate(year = year(year)) %>%
  rename("cname" = `Country Name`) %>%
  rename("ccode" = `Country Code`) %>%
  rename("trade_vol" = "NE.TRD.GNFS.ZS") %>%
  rename("ex_rate" = "PA.NUS.FCRF") %>%
  rename("gdp_pc" = "NY.GDP.PCAP.KD.ZG") %>%
  rename("pop" = "SP.POP.TOTL") %>%
  rename("fdi" = "BX.KLT.DINV.WD.GD.ZS") %>%
  rename("inf_cp" = "FP.CPI.TOTL.ZG") %>%
  rename("urban" = "SP.URB.TOTL.IN.ZS") %>%
  rename("gdp_growth" = "NY.GDP.MKTP.KD.ZG") %>%
  rename("unemp" = "SL.UEM.TOTL.ZS") %>%
  rename("nat_rcs" = "NY.GDP.TOTL.RT.ZS") %>%
  rename("industry" = "NV.IND.TOTL.ZS") %>%
  rename("aid" = "DT.ODA.ODAT.GN.ZS") %>%
  rename("pop_den" = "EN.POP.DNST") %>%
  rename("prim_edu" = "SE.PRM.ENRR") %>%
  rename("age_dep" = "SP.POP.DPND") %>%
  rename("prod" = "NV.AGR.TOTL.ZS") %>%
  rename("broad" = "FM.LBL.BMNY.GD.ZS") %>%
  rename("labor" = "SL.TLF.CACT.ZS") %>%
  rename("fertility" = "SP.DYN.TFRT.IN") %>%
  rename("house" = "NE.CON.PRVT.ZS") %>%
  rename("pop_growth" = "SP.POP.GROW") %>%
  rename("gov_exp" = "NE.CON.GOVT.ZS") %>%
  rename("women" = "SG.LAW.INDX") %>%
  rename("wage_worker" = "SL.EMP.WORK.ZS") %>%
  rename("imp_duty" = "GC.TAX.IMPT.ZS") %>%
  rename("int_tax" = "GC.TAX.INTT.RV.ZS") %>%
  mutate(
    lxr = log(ex_rate),
    pop = pop/1000000,
    lxr_sqr = lxr*lxr,
    fdi_pc = fdi/gdp_pc,
    lfdi_pc = log(fdi_pc),
    lgdp_pc = log(gdp_pc),
    end_date     = if_else(year == max(year), TRUE, FALSE),
    end_label    = if_else(year == max(year), ccode, NA_character_))

wdi_main <- wdi_clean %>%
  mutate(year = as.Date(year, format="%Y"))

pwt_main <- pwt_data %>%
  select("countrycode", "country", "year", "rgdpo", "pop", "hc", "labsh", "irr", "xr") %>%
  filter(year >= 1960) %>%
  mutate(
    gdp_per_capita = rgdpo / pop,
    xr = format(xr, scientific = FALSE, na.encode = FALSE),
    year = as.Date(as.character(year), format="%Y")) %>%
  arrange(countrycode)


### Merging Datasets

all_data <- merge.data.frame(wdi_main, pwt_main, by.x = c("cname", "ccode", "year"), by.y = c("country", "countrycode", "year"), all.x = TRUE, all.y = FALSE) %>%
  mutate(
    xr = as.numeric(xr),
    ex_rate = coalesce(ex_rate, xr)
  )

all_data <- all_data %>%
  mutate(year = as.numeric(year(year)))

rm(pwt_data, pwt_main, wdi_clean, wdi_main, wdi_data)


### Exchange rate visualization 

df1_saarc <- all_data %>%
  filter(!ccode %in% c("VNM", "IDN", "LAO", "KHM", "MMR"))

ggplot(df1_saarc, mapping = aes(x = year, 
                                y = ex_rate, 
                                color = ccode, 
                                label = end_label)) +
  scale_x_discrete(breaks = c("1960", "1980", "2000", "2020")) +
  geom_point(data = df1_saarc %>% filter(end_date)) +
  geom_line(width = 0.4, size = 0.5, alpha = 1) +
  guides(color = FALSE) +
  geom_label_repel(aes(label = end_label), nudge_x = 1, size = 4, na.rm = TRUE) + 
  theme(legend.position = "none") +
  #scale_color_viridis_d() +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Exchange Rate"
  )


## Observation
Bhutan and India have the SAME exchange rate, thus the lines are overlapped. Bhutan's currency is pegged to INR.
Afghanistan's exchange rate for 2021 is missing.

df1_saarc %>%
  filter(ccode %in% c("BTN", "IND")) %>%
  ggplot(mapping = aes(x = year, 
                       y = ex_rate,
                       col = ccode,
                       label = end_label)) +
  scale_x_discrete(breaks = c("1960", "1980", "2000", "2020")) +
  geom_line(width = 0.4, size = 0.5, alpha = 1) +
  geom_point(data = df1_saarc %>% filter(end_date, ccode %in% c("BTN", "IND"))) + 
  geom_label_repel(aes(label = end_label), nudge_x = 1, size = 4, na.rm = TRUE) +
  theme(legend.position = "none") 

df1_saarc %>%
  filter(ccode %in% c("AFG")) %>%
  ggplot(mapping = aes(x = year, 
                       y = ex_rate,
                       col = ccode,
                       label = end_label)) +
  scale_x_discrete(breaks = c("1960", "1980", "2000", "2020")) +
  geom_line(width = 0.4, size = 0.5, alpha = 1) +
  geom_point(data = df1_saarc %>% filter(end_date, ccode %in% c("AFG"))) + 
  geom_label_repel(aes(label = end_label), nudge_x = 1, size = 4, na.rm = TRUE) +
  theme(legend.position = "none")  


#### Checking Cambodia's exchange rate difference
{r}
all_data %>%
  filter(ccode %in% c("KHM")) %>%
  ggplot(mapping = aes(x = year, 
                       y = ex_rate, 
                       col = ccode,
                       label = end_label))  +
  scale_x_discrete(breaks = c("1960", "1980", "2000", "2020")) +
  geom_line(width = 0.4, size = 0.5, alpha = 1) 


### Summary Statistics
{r}
stargazer(all_data, 
          summary = TRUE, 
          title = "Summary statistics for the baseline estimation",
          type = "text",
          omit = c("pop.x", "lfdi_pc", "xr", "end_date", "rgdpo", "pop.y", "gdp_per_capita", "year", "pop_den", "lgdp_pc1960", "lgdp_pc", "irr", "imp_duty", "int_tax", "house", "prim_edu", "wage", "pop_growth", "fdi_pc"),
          #covariate.labels=c("Trade Volume","Exchange Rate","FDI (per capita)","Inflation, consumer prices (annual %)","Urbanization","GDP Growth (annual %)","Unemployment (% of pop.)","Human Capital Index", "GDP per capita"),
          omit.summary.stat = "n")


_________________________________________________________________________________________________________________________________________________________________
_________________________________________________________________________________________________________________________________________________________________

#### Checking curve for Trade Volume
{r}
ggplot(all_data, aes(x = lxr)) +
  geom_histogram()

ggplot(all_data, aes(x = lxr, y = trade_vol)) +
  geom_point() +
  geom_smooth()


### Regression against Trade Volume

Official exchange rate (LCU per US$, period average): PA.NUS.FCRF \\
GDP per capita growth (annual %): NY.GDP.PCAP.KD.ZG \\
Inflation, consumer prices (annual %): FP.CPI.TOTL.ZG \\
Urban Population (% of total population): SP.URB.TOTL.IN.ZS \\
GDP Growth (annual %): NY.GDP.MKTP.KD.ZG \\
Industry, value added (% of GDP): NV.IND.TOTL.ZS \\
Broad money (% of GDP): FM.LBL.BMNY.GD.ZS \\
Production - Agriculture, forestry, and fishing, value added (% of GDP): NV.AGR.TOTL.ZS \\


reg1_tv <- plm(trade_vol ~ lxr + factor(ccode) + factor(year), data = all_data, na.rm = TRUE, method = "within", effect = "individual")

reg2_tv <- plm(trade_vol ~ lxr + lxr_sqr + factor(ccode) + factor(year), data = all_data, na.rm = TRUE, method = "within", effect = "individual")

reg3_tv <- plm(trade_vol ~ lxr + lxr_sqr + prod + inf_cp + factor(ccode) + factor(year), data = all_data, na.rm = TRUE, method = "within", effect = "individual")

reg4_tv <- plm(trade_vol ~ lxr + lxr_sqr + prod + inf_cp + industry + urban + factor(ccode) + factor(year), data = all_data, na.rm = TRUE, method = "within", effect = "individual")

reg_all_tv <- plm(trade_vol ~ lxr + lxr_sqr + prod + inf_cp + industry + urban + broad + gdp_growth + factor(ccode) + factor(year), data = all_data, na.rm = TRUE, method = "within", effect = "individual")

stargazer(reg1_tv, reg2_tv, reg3_tv, reg4_tv, reg_all_tv, 
          title = "The influence of various economic parameters on Trade Volume", 
          align = TRUE,
          type = "text",
          dep.var.labels = c("Trade Volume"),
          #covariate.labels=c("log of Exchange Rate", "log of Exchange Rate (squared)", "Production, value added (% of GDP)", "Inflation, consumer prices (annual %)", "Industry, value added (% of GDP)", "Urban Population (% of total population)", "Broad money (% of GDP)", "Government expenditure (% of GDP)", "GDP growth (annual %)"),
          omit.stat=c("LL","ser","f"),
          omit = c("year", "cname"),
          omit.labels = c("Year fixed effect", "Country fixed effect"),
          no.space = TRUE,
          header = FALSE,
          notes = "",
          omit.table.layout = "n",
          column.sep.width = "-20pt")

_________________________________________________________________________________________________________________________________________________________________
_________________________________________________________________________________________________________________________________________________________________

#### Checking curve for FDI
{r}
ggplot(all_data, aes(x = lfdi_pc)) +
  geom_histogram()

ggplot(all_data, aes(x = lxr, y = lfdi_pc)) +
  geom_point() +
  geom_smooth()


### Regression against FDI

Variables used:
  Official exchange rate (LCU per US$, period average): PA.NUS.FCRF \\
GDP per capita growth (annual %): NY.GDP.PCAP.CD \\
Inflation, consumer prices (annual %): FP.CPI.TOTL.ZG \\
Urban Population (% of total population): SP.URB.TOTL.IN.ZS \\
GDP Growth (annual %): NY.GDP.MKTP.KD.ZG \\
Total natural resources rents (% of GDP): NY.GDP.TOTL.RT.ZS \\
Tourists (total arrivals): ST.INT.ARVL \\
Primary Education: SE.PRM.ENRL \\
Broad Money: FM.LBL.BMNY.GD.ZS \\
Production - Agriculture, forestry, and fishing, value added (% of GDP): NV.AGR.TOTL.ZS \\
General government final consumption expenditure (% of GDP): NE.CON.GOVT.ZS


reg1_fdi <- plm(fdi ~ lxr + factor(ccode) + factor (year.x), data = all_data, na.rm = TRUE, method = "within", effect = "individual")

reg2_fdi <- plm(fdi ~ lxr + urban + gdp_pc + factor(ccode) + factor (year.x), data = all_data, method = "within", effect = "individual")

reg3_fdi <- plm(fdi ~ lxr + urban + gdp_pc + gov_exp + inf_cp + factor(ccode) + factor (year.x), data = all_data,  na.rm = TRUE, method = "within", effect = "individual")

reg4_fdi <- plm(fdi ~ lxr + urban + gdp_pc + gov_exp + inf_cp + pop_den + women + factor(ccode) + factor (year.x), data = all_data,  na.rm = TRUE, method = "within", effect = "individual")

reg_all_fdi <- plm(fdi ~ lxr + urban + gdp_pc + gov_exp + inf_cp + pop_den + women + nat_rcs  + prod + factor(ccode) + factor (year.x), data = all_data, na.rm = TRUE, method = "within", effect = "individual")

stargazer(reg1_fdi, reg2_fdi, reg3_fdi, reg4_fdi, reg_all_fdi, 
          title = "The influence of various economic parameters on Foreign Direct Investment", 
          align = TRUE, 
          type = "text",
          dep.var.labels=c("FDI"),
          #covariate.labels=c("log of Exchange Rate", "Urban Population (% of total population)", "GDP per capita growth (annual %)",  "Government expenditure (% of GDP)", "Inflation, consumer prices (annual %)", "Natural resources rents (% of GDP)", "GDP Growth (annual %)", "Production, value added (% of GDP)"),
          omit.stat=c("LL","ser","f"), 
          omit = c("year.x", "ccode"),
          omit.labels = c("Year fixed effect", "Country fixed effect"),
          no.space = TRUE,
          header = FALSE,
          column.sep.width = "-20pt")


_________________________________________________________________________________________________________________________________________________________________
_________________________________________________________________________________________________________________________________________________________________

#### Checking curve for Unemployment
{r}
ggplot(all_data, aes(x = unemp)) +
  geom_histogram()

ggplot(all_data, aes(x = lxr, y = unemp)) +
  geom_point() +
  geom_smooth()



### Regression against Unemployment

Variables used: 
  Official exchange rate (LCU per US$, period average): PA.NUS.FCRF \\
GDP per capita (current US$): NY.GDP.PCAP.CD \\
Inflation, consumer prices (annual %): FP.CPI.TOTL.ZG \\
Urban Population (% of total population): SP.URB.TOTL.IN.ZS \\
GDP Growth (annual %): NY.GDP.MKTP.KD.ZG \\
Net ODA received (% of GNI): DT.ODA.ODAT.GN.ZS \\
Population Density: EN.POP.DNST \\
School enrollment, primary (% gross): SE.PRM.ENRR \\
Age dependency ratio (% of working-age population): SP.POP.DPND \\
Labor force participation rate, total (% of total population ages 15+): SL.TLF.CACT.ZS \\
Fertility rate, total (births per woman): SP.DYN.TFRT.IN \\
Population growth (annual %): SP.POP.GROW \\
Women Business and the Law Index Score (scale 1-100): SG.LAW.INDX \\


reg1_unemp <- plm(unemp ~ lxr + factor(ccode) + factor (year.x), data = all_data, method = "within", effect = "individual", na.rm = TRUE)

reg2_unemp <- plm(unemp ~ lxr + labor + age_dep + factor(ccode) + factor (year.x), data = all_data, method = "within", effect = "individual", na.rm = TRUE)

reg3_unemp <- plm(unemp ~ lxr + labor + age_dep + fertility + inf_cp + factor(ccode) + factor (year.x), data = all_data, method = "within", effect = "individual", na.rm = TRUE)

reg4_unemp <- plm(unemp ~ lxr + labor + age_dep + fertility + inf_cp + pop_growth + prim_edu + factor(ccode) + factor (year.x), data = all_data, method = "within", effect = "individual", na.rm = TRUE)

reg_all_unemp <- plm(unemp ~ lxr + labor + age_dep + fertility + inf_cp + pop_growth + prim_edu + hc + factor(ccode) + factor(year.x), data = all_data, method = "within", effect = "individual", na.rm = TRUE)

stargazer(reg1_unemp, reg2_unemp, reg3_unemp, reg4_unemp, reg_all_unemp, 
          title = "The influence of various economic parameters on Unemployment", 
          align = TRUE, 
          type = "text",
          dep.var.labels=c("Unemployment"),
          #covariate.labels=c("log of Exchange Rate", "Labor force participation rate", "Age dependency ratio", "Fertility rate", "Inflation, consumer prices (annual %)", "Population Growth (annual %)", "Net ODA received ", "Economic opportunity for women", "Human Capital Index"),
          omit.stat=c("LL","ser","f"), 
          omit = c("year.x", "ccode"),
          omit.labels = c("Year fixed effect", "Country fixed effect"),
          no.space = TRUE,
          header = FALSE,
          column.sep.width = "-20pt")

# capital-to-labor ratio


_________________________________________________________________________________________________________________________________________________________________
_________________________________________________________________________________________________________________________________________________________________


### Sythetic Control Method

Afghanistan had missing data, a lot of it
Laos' data is too extreme and unmatching with that of Nepal's and causes unwanted distortion
Myanmar also has missing data (for trave_vol)
*adding Srilanka's data causes more similarity with pre-treatment Nepal.
{r}
nep_scm <- all_data %>%
  mutate(
  year.x = as.numeric(format(as.Date(year.x), "%Y")),
  unit = rep(1:22, each = 62),
  ex_rate = ifelse(is.na(ex_rate), mean(ex_rate, na.rm = TRUE), ex_rate),
  ccode = as.character(ccode),
  year = as.numeric(year)) %>%
  as.data.frame()

units <- nep_scm %>%
  select("ccode", "unit")

units <- distinct(units)
dataprep.out <- dataprep(nep_scm,
                        predictors = c("trade_vol", "fdi", "unemp", "urban", "inf_cp", "gdp_growth", "nat_rcs"),
                        dependent = c("ex_rate"),
                        unit.variable = "unit",
                        time.variable = "year.x",
                        unit.names.variable = "ccode",
                        treatment.identifier = 12,
                        controls.identifier = c(2:10, 13, 14:15, 17:18, 21), # exclude Nepal, South Asia, World and South Asia (IDA & IBRD) -- 2:11, 13:15, 18:21
                        time.predictors.prior = 1960:2001,
                        time.optimize.ssr = 1990:2001,
                        time.plot = 1960:2021) # excluded Afghanistan (1 - no data), Myanmar (11 - no data), Timor-Leste (19 - no data)

synth.out <- synth(dataprep.out, na.rm = TRUE)
synth.tables <- synth.tab(dataprep.res = dataprep.out,
                          synth.res = synth.out)
synth.tables$tab.w[1:20, ]

path.plot(
  synth.res = synth.out,
  dataprep.res = dataprep.out,
  tr.intake = c(2001.5, 2006.8),
  Ylab = c("Exchange Rate"),
  Xlab = c("Year"),
  Legend = c("Nepal Ex. Rate","Synthetic Nepal Ex. Rate"),
  Legend.position = c("topleft"),
  Main = ""
) +
abline(h = 100, col = "red")


{r}
posterior_data <- as.data.frame(dataprep.out$Y0plot)[62:62, ] %>%
  pivot_longer(cols = "2":"21") %>%
  select(value)

posterior_weights <- synth.tables$tab.w %>%
  select(w.weights)

posterior <- cbind(posterior_data, posterior_weights)

sum(posterior$value*posterior$w.weights)



{r}
stargazer(synth.tables$tab.w, 
          type = "latex", 
          summary = FALSE
          )



### Country-wise Summary Statistic

nep_scm_summary <- nep_scm %>%
  select("ccode", "ex_rate", "trade_vol")%>%
  group_by(ccode) %>%
  summarize(across(where(is.numeric), list(mean = mean, sd = sd, min = min, max = max), na.rm = TRUE))

nep_scm_fdiunemp <- nep_scm %>%
  select("ccode", "fdi", "unemp")%>%
  group_by(ccode) %>%
  summarize(across(where(is.numeric), list(mean = mean, sd = sd, min = min, max = max), na.rm = TRUE, digits = 3))

stargazer(nep_scm_summary, 
          summary = FALSE,
          header = TRUE,
          title = "Summary Statistics (country-wise)", 
          type = "latex",
          digits = 3)

stargazer(nep_scm_fdiunemp, 
          summary = FALSE,
          header = TRUE,
          title = "Summary Statistics (country-wise)", 
          type = "latex",
          digits = 3)



### Scatter plot for dependent variables

# Convert lxr and trade_vol variables to numeric if needed
nep_scm$lxr <- as.numeric(nep_scm$lxr)
nep_scm$trade_vol <- as.numeric(nep_scm$trade_vol)

par(mfrow = c(1, 3))

ggplot(nep_scm, aes(x = lxr, y = trade_vol)) +
  geom_point(col = "darkturquoise", size = 0.5) +
  geom_smooth(size = 0.5) +
  labs(y = "Trade Volume", x = "(a)") +
  theme_minimal() +
  theme(aspect.ratio = 1)

ggplot(nep_scm, aes(x = lxr, y = fdi_pc)) +
  geom_point(col="brown3", size = 0.5) +
  geom_smooth(size = 0.8)+
  labs(y = "FDI per capita", x = "(b)") +
  theme_minimal() +
  theme(aspect.ratio=1)

ggplot(nep_scm, aes(x = lxr, y = unemp)) +
  geom_point(col="olivedrab", size = 0.5) +
  geom_smooth(size = 0.8)+
  labs(y = "Uneployment", x = "(c)")+
  theme_minimal() +
  theme(aspect.ratio=1)


### Saving from R into MS Excel
{r}
write.csv(nep_scm, "nep_scm.csv")


### XGBoost
{r}
xgboost_filtered <- all_data %>%
  select(ccode, year, fdi, aid, pop_den, broad, inf_cp, trade_vol, prod, industry, gdp_growth, gdp_pc, nat_rcs, ex_rate, prim_edu, women, labor, unemp, fertility, age_dep, urban, lxr, hc, labsh) %>%
  filter(!ccode %in% c("SAS", "TSA", "WLD")) 

ccode <- model.matrix(~ ccode -1 , data = xgboost_filtered)

year <- model.matrix(~ year , data = xgboost_filtered)

xgboost_final <- cbind(xgboost_filtered, ccode, year) %>%
  select(-ccode, -year)

xgboost_matrix <- data.matrix(xgboost_final)

target_tv <- "trade_vol"
target_fdi <- "fdi"
target_unemp <- "unemp"

# Extract labels
label_tv <- xgboost_filtered[[target_tv]]
label_fdi <- xgboost_filtered[[target_fdi]]
label_unemp <- xgboost_filtered[[target_unemp]]

# Remove the target column from the feature matrix
xgboost_filtered_tv <- xgboost_filtered %>%
  select(-c(target_tv))

xgboost_filtered_fdi <- xgboost_filtered %>%
  select(-c(target_fdi))

xgboost_filtered_unemp <- xgboost_filtered %>%
  select(-c(target_unemp))

# Convert to matrix
xgboost_matrix_tv <- data.matrix(xgboost_filtered_tv)
xgboost_matrix_fdi <- data.matrix(xgboost_filtered_fdi)
xgboost_matrix_unemp <- data.matrix(xgboost_filtered_unemp)



{r}
# Calculate the number of training samples
numberOfTrainingSamples <- round(nrow(xgboost_matrix_tv) * 0.7)

# Make sure the number of training samples doesn't exceed the number of rows
numberOfTrainingSamples <- min(numberOfTrainingSamples, nrow(xgboost_matrix_tv))

# Create a sequence of indices
all_indices <- 1:nrow(xgboost_matrix_tv)

# Randomly shuffle the indices
set.seed(123)
shuffled_indices <- sample(all_indices)

# Take the first numberOfTrainingSamples for training
train_indices <- shuffled_indices[1:numberOfTrainingSamples]
test_indices <- shuffled_indices[(numberOfTrainingSamples + 1):nrow(xgboost_matrix_tv)]

# Splitting the data
train_data <- xgboost_matrix_tv[train_indices, ]
test_data <- xgboost_matrix_tv[test_indices, ]
train_labels <- label_tv[train_indices]
test_labels <- label_tv[test_indices]

# Handle missing values in labels
train_labels[is.na(train_labels)] <- -1  # Replace missing with a default value
test_labels[is.na(test_labels)] <- -1

# Convert labels to 0 and 1
train_labels <- ifelse(train_labels == "desired_positive_label", 1, 0)
test_labels <- ifelse(test_labels == "desired_positive_label", 1, 0)



{r}
# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label = train_labels)
dtest <- xgb.DMatrix(data = test_data, label = test_labels)



{r}
# train a model using our training data
model <- xgboost(data = dtrain, # the data   
                 nround = 10, # max number of boosting iterations
                 objective = "binary:logistic")  # the objective function



{r}
# generate predictions for our held-out testing data
pred <- predict(model, dtest)

# get & print the classification error
err <- mean(as.numeric(pred > 0.5) != test_labels)
print(paste("test-error=", err))


{r}
# train an xgboost model
model_tuned <- xgboost(data = dtrain, # the data           
                       max.depth = 3, # the maximum depth of each decision tree
                       nround = 10, # max number of boosting iterations
                       objective = "binary:logistic") # the objective function 

# generate predictions for our held-out testing data
pred <- predict(model_tuned, dtest)

# get & print the classification error
err <- mean(as.numeric(pred > 0.5) != test_labels)
print(paste("test-error=", err))


{r}
# get the number of negative & positive cases in our data
negative_cases <- sum(train_labels == FALSE)
postive_cases <- sum(train_labels == TRUE)

# train a model using our training data
model_tuned <- xgboost(data = dtrain, # the data           
                       max.depth = 3, # the maximum depth of each decision tree
                       nround = 10, # number of boosting rounds
                       early_stopping_rounds = 3, # if we dont see an improvement in this many rounds, stop
                       objective = "binary:logistic", # the objective function
                       scale_pos_weight = negative_cases/postive_cases) # control for imbalanced classes

# generate predictions for our held-out testing data
pred <- predict(model_tuned, dtest)

# get & print the classification error
err <- mean(as.numeric(pred > 0.5) != test_labels)
print(paste("test-error=", err))



{r}
# train a model using our training data
model_tuned <- xgboost(data = dtrain, # the data           
                       max.depth = 3, # the maximum depth of each decision tree
                       nround = 10, # number of boosting rounds
                       early_stopping_rounds = 3, # if we dont see an improvement in this many rounds, stop
                       objective = "binary:logistic", # the objective function
                       scale_pos_weight = negative_cases/postive_cases, # control for imbalanced classes
                       gamma = 1) # add a regularization term

# generate predictions for our held-out testing data
pred <- predict(model_tuned, dtest)

# get & print the classification error
err <- mean(as.numeric(pred > 0.5) != test_labels)
print(paste("test-error=", err))



{r}
# plot them features! what's contributing most to our model?
tree_dump <- xgb.dump(model = model, with_stats = TRUE)
print(tree_dump)




### CODE 2 for XGBOOST

{r}
# Remove rows with missing target values (trade_vol)
all_data <- all_data[!is.na(all_data$trade_vol), ]

# Split the data into features (X) and the target variable (y)
X <- all_data[, c("unemp", "aid", "pop_den", "broad", "inf_cp", "prod", "industry", "gdp_growth", "gdp_pc", "nat_rcs", "lxr", "gov_exp", "women", "labor", "fdi", "fertility", "age_dep", "urban", "hc")]
y <- all_data$trade_vol

# Create a matrix for the categorical variables
dtrain <- xgb.DMatrix(data = as.matrix(X), label = y)

# Set XGBoost parameters
params <- list(
  objective = "reg:squarederror",  # Specify the regression objective
  # Number of boosting rounds (you can tune this)
  eta = 0.1                        # Learning rate (you can tune this)
)

# Set the seed for reproducibility
set.seed(42)

# Train the XGBoost model
xgb_model <- xgboost(params = params, data = dtrain, nrounds = 100)

# Plot feature importances
importance_matrix <- xgb.importance(model = xgb_model)
xgb.plot.importance(importance_matrix)

table_tv <- importance_matrix %>%
  select(Feature, Importance)

stargazer(table_tv, summary = FALSE, type = "latex")


### FDI
{r}
# Remove rows with missing target values (trade_vol)
all_data <- all_data[!is.na(all_data$fdi), ]

# Split the data into features (X) and the target variable (y)
X <- all_data[, c("unemp", "aid", "pop_den", "broad", "inf_cp", "prod", "industry", "gdp_growth", "gdp_pc", "nat_rcs", "lxr", "gov_exp", "women", "labor", "trade_vol", "fertility", "age_dep", "urban", "hc")]
y <- all_data$fdi

# Create a matrix for the categorical variables
dtrain <- xgb.DMatrix(data = as.matrix(X), label = y)

# Set XGBoost parameters
params <- list(
  objective = "reg:squarederror",  # Specify the regression objective
  # Number of boosting rounds (you can tune this)
  eta = 0.1                        # Learning rate (you can tune this)
)

# Set the seed for reproducibility
set.seed(42)

# Train the XGBoost model
xgb_model <- xgboost(params = params, data = dtrain, nrounds = 100)

# Plot feature importances
importance_matrix <- xgb.importance(model = xgb_model)
xgb.plot.importance(importance_matrix)

table_fdi <- importance_matrix %>%
  select(Feature, Importance)

stargazer(table_fdi, summary = FALSE, type = "latex")



### Unemp
{r}
# Remove rows with missing target values (trade_vol)
all_data <- all_data[!is.na(all_data$unemp), ]

# Split the data into features (X) and the target variable (y)
X <- all_data[, c("fdi", "aid", "pop_den", "broad", "inf_cp", "prod", "industry", "gdp_growth", "gdp_pc", "nat_rcs", "lxr", "gov_exp", "women", "labor", "trade_vol", "fertility", "age_dep", "urban", "hc")]
y <- all_data$unemp

# Create a matrix for the categorical variables
dtrain <- xgb.DMatrix(data = as.matrix(X), label = y)

# Set XGBoost parameters
params <- list(
  objective = "reg:squarederror",  # Specify the regression objective
  # Number of boosting rounds (you can tune this)
  eta = 0.1                        # Learning rate (you can tune this)
)

# Set the seed for reproducibility
set.seed(42)

# Train the XGBoost model
xgb_model <- xgboost(params = params, data = dtrain, nrounds = 100)

# Plot feature importances
importance_matrix <- xgb.importance(model = xgb_model)
xgb.plot.importance(importance_matrix)

table_unemp <- importance_matrix %>%
  select(Feature, Importance)

stargazer(table_unemp, summary = FALSE, type = "latex")


{r}
full_imp <- merge(table_tv, table_fdi, by.x = c("Feature"), by.y = c("Feature"), all.x = TRUE, all.y = TRUE)

full_imp <- merge(full_imp, table_unemp, by.x = c("Feature"), by.y = c("Feature"), all.x = TRUE, all.y = TRUE)



### Bayesian
{r, warning=FALSE}
set.seed(1237)

nep_data <- all_data %>%
  select(ccode, year, fdi, ex_rate, unemp, trade_vol) %>%
  mutate(lfdi = log(fdi)) %>%
  as.data.frame()

# Filter out rows with missing values in the lfdi variable
nep_data <- nep_data[complete.cases(nep_data$lfdi), ]

mu_mean = mean(nep_data$fdi, na.rm = TRUE)
mu_sd = sd(nep_data$fdi, na.rm = TRUE)

plot(density(nep_data$lfdi))
lines(density(rnorm(nrow(nep_data), log(mu_mean), log(mu_sd))), col = "red")



{r}
nep <- stan_glm(fdi ~ ex_rate + factor(ccode) + factor(year),
                data = nep_data,
                family = gaussian,
                prior = normal(log(mu_mean), log(mu_sd), autoscale = TRUE),
                prior_intercept = normal(autoscale = TRUE),
                prior_aux = exponential(autoscale = TRUE),
                #prior_PD = TRUE,
                chains = 4, iter = 5000*2, refresh = 0)

summary(nep)
ggsave("mcmc_trace.png", mcmc_trace(nep, pars = c("ex_rate")), width = 6, height = 4, units = "in", dpi = 300)


{r}
plot(density(posterior_predict(nep)))



