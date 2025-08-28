
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
###            r-script 1_study_population.R          ###
###                 Replication file.                 ###
###                    2025 by TH                     ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Extract study population and construct socioeconomic features.
rm(list=ls())

# Install and load the following packages:
library(here)             # Relative file paths.
library(data.table)       # mutating and aggregating data
library(tictoc)           # timing 

# Inputs:
input_folk <- "~DATAINFRA/TK/raw/folk_yhdistetty_2023.csv"
input_kela <- "~DATAINFRA/KELA/cleaned/kela_erityiskorvausoikeudet_2024.csv"
input_hva <- "~DATAINFRA/misc/raw/kunta_hva_2023.csv"
input_distance <- "~DATAINFRA/misc/cleaned/hetu_terveysasemat_21_23_lakkautetut_VV032025AM.csv"

# Outputs:
output_population <- here('data', 'study_population.csv')

# Start timing:
tic.clearlog()
tic()

###
###


# Read FOLK data:
vars <- 
  c('shnro', 'kunta31_12', 'ika', 'sukup', 'kieli_k', 'sivs', 'posti_alue',
    'maka', 'ututku_aste', 'ptoim1', 'kturaha_ekv', 'tyke', 'petu')
dt <- data.table::fread(input_folk, select=vars, 
                        na.strings = c(NA_character_, ''))


# Read data on special reimbursement rights:
dt.rights <- data.table::fread(
  input_kela, na.strings = c(NA_character_, ''),
  select = c('shnro','ek_kela_kansansairaus','ek_kaisa_monisairas',
             'ek_kaisa_diabetes_insuliinihoito'))

# Merge to FOLK data:
dt <- merge(dt, dt.rights, by='shnro', all.x = TRUE)

# Inpute zeroes if no morbidity is observed:
dt[is.na(ek_kela_kansansairaus), ek_kela_kansansairaus := 0]
dt[is.na(ek_kaisa_monisairas), ek_kaisa_monisairas := 0]
dt[is.na(ek_kaisa_diabetes_insuliinihoito), ek_kaisa_diabetes_insuliinihoito:=0]


# Add income decile:
dt <- dt[order(kturaha_ekv)]
dt[, kturaha_decile :=
     .bincode(kturaha_ekv,
              quantile(kturaha_ekv, probs = 0:10/10, na.rm=TRUE), 
              right = FALSE, include.lowest = TRUE)]


# Wellbeing services county:
dt.hva <- data.table::fread(input_hva)
dt <- merge(dt, dt.hva[, .(kuntanro, hva_lyhenne)], 
            by.x = 'kunta31_12', by.y = 'kuntanro', all.x = TRUE)


# If not in family population, use person ID as family ID:
dt[, petu := as.character(petu)]
dt[is.na(petu), petu := shnro]

# Construct covariates for descriptive statistics statistics:
dt[, ':=' (female = as.integer(sukup == 2),
           language.fin = as.integer(kieli_k == 1),
           language.swe = as.integer(kieli_k == 2),
           language.other = as.integer(kieli_k == 3),
           relationship.or.widowed = as.integer(sivs %in% c(2, 5)),
           living.in.city = as.integer(maka %in% c('K1', 'K2', 'K3')),
           educ.tertiary = as.integer(ututku_aste %in% c(5, 6, 7, 8)),
           pensioner = as.integer(ptoim1 %in% c(24, 29)),
           in.labor.market = as.integer(ptoim1 %in% c(11, 12)),
           unemployment = as.integer(tyke %in% c(0:12)),
           income.bottom.20 = as.integer(kturaha_decile %in% c(1:2)),
           age.80.plus = as.integer(ika >= 80),
           ek_kaisa_diabetes_insuliinihoito_40 =
             as.integer(ika >= 40 & ek_kaisa_diabetes_insuliinihoito==1))]


# Read distances to the nearest health station and merge:
dt.dist <- data.table::fread(
  input_distance, na.strings = c(NA_character_, ''),
  select = c('shnro', 'vuosi', 'etaisyys_terveysasemalle_km'))
dt.dist <- dt.dist[vuosi==2023]
dt.dist[, vuosi := NULL]

dt <- merge(dt, dt.dist, by='shnro', all.x = TRUE)


# If distance to nearest health station is missing, 1) use mean distance for the
# same postal code area or 2) use mean distance for the same municipality.

dt.help <- dt[, .(etaisyys_tk_postialue =
                    mean(etaisyys_terveysasemalle_km, na.rm=TRUE)), 
              by='posti_alue'
              ][!is.na(etaisyys_tk_postialue)]

dt <- merge(dt, dt.help, by='posti_alue', all.x = TRUE)

dt[is.na(etaisyys_terveysasemalle_km), 
   etaisyys_terveysasemalle_km := etaisyys_tk_postialue]

dt.help <- dt[, .(etaisyys_tk_kunta = 
                    mean(etaisyys_terveysasemalle_km, na.rm=TRUE)), 
              by='kunta31_12'
              ][!is.na(etaisyys_tk_kunta)]

dt <- merge(dt, dt.help, by='kunta31_12', all.x = TRUE)

dt[is.na(etaisyys_terveysasemalle_km), 
   etaisyys_terveysasemalle_km := etaisyys_tk_kunta]

dt[, ':=' (etaisyys_tk_postialue=NULL, etaisyys_tk_kunta=NULL, posti_alue=NULL)]


# Missing values:
100 * colMeans(is.na(dt))
colSums(is.na(dt))

# N:
print('Sample size (individuals):')
print(dt[, uniqueN(shnro)])


# Restrict to those who will be 50 at the end of 2025:
dt <- dt[ika >= 48]
 
# Exclude those who will turn 65 in 2025 or 2026:
dt <- dt[!(ika %in% c(62:63))]

# Create an indicator for being 66 or more at the end of 2025:
dt[, age.65.plus := as.integer(ika >= 64)]

# Mutate age that it represents age at the end of 2025 instead of age at the
# end of 2023:
dt[, ika := ika + 2]


# N:
print('Sample size (individuals):')
print(dt[, uniqueN(shnro)])


# Save:
fwrite(dt, file=output_population)

# End.
rm(list=ls())
gc()
toc()
