
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
###             r-script 2_panel_data.R               ###
###                 Replication file.                 ###
###                    2025 by TH                     ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Construct outcomes and panel data for DID analyses.
rm(list=ls())

# Install and load the following packages:
library(here)             # Relative file paths.
library(data.table)       # Mutating and aggregating data
library(tictoc)           # timing 

source("~DATAINFRA/functions.R") # some helper functions

# Input folder for Avohilmo:
input_folder_thl <- "~DATAINFRA/THL/cleaned"
input_folder_kela <- "~DATAINFRA/KELA/cleaned"

# Inputs:
input_population <- here('data', 'study_population.csv')
inputs.thl <- 
  get_file_paths(input_folder_thl)[grepl('avohilmo|hilmo_tupva', file_name)
                                   ][is.na(year) | year >= 2022, file_name]
inputs.thl <- paste0(inputs.thl, '.csv')
inputs_avosh <- inputs.thl[grep('avosh_', inputs.thl)]
inputs_hta <- inputs.thl[grep('hta_', inputs.thl)]
inputs_esh <- inputs.thl[grep('hilmo_tupva', inputs.thl)]
print(c(inputs_avosh, inputs_hta, inputs_esh))
inputs.kela <- 
  get_file_paths(input_folder_kela)[grepl('laakarinpalkkiot', file_name)
                                    ][year >= 2022, file_name]
inputs.kela <- paste0(inputs.kela, '.csv')

# Outputs:
output_panel <- here('data', 'person_by_month_panel.csv')

# Start timing:
tic.clearlog()
tic()

###
###


# Study start and end date:
start.date <- as.Date('2022-03-01')
end.date <- as.Date('2025-02-28')

# Study population and their characteristics:
folk <- fread(input_population, na.strings = c(NA_character_, ''), 
              select = c('shnro', 'age.65.plus'))

# Initiate a person-month panel on healthcare use:
dt <- CJ(shnro = folk[, unique(shnro)],
         year = c(2022:2027),
         month = c(1:12))

# Drop rows for which we do not yet observe data:
dt <- dt[year <= as.integer(format(end.date, '%Y'))]
dt <- dt[!(year == as.integer(format(end.date, '%Y')) &
             month > as.integer(format(end.date, '%m')))]


### ### ### ### ### ### ### ### ### ### ### ### ###
#### 1) Data on Kela reimbursements for private doctor visits. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


kela <- lapply(inputs.kela, function(input_name) {
  print(input_name)
  source.path <- paste(input_folder_kela, input_name, sep='/')
  
  # Read the datasets and subset:
  dt <- data.table::fread(source.path, na.strings = c(NA_character_, ''))
  dt <- dt[kaynti_pv >= start.date & 
             kaynti_pv <= end.date &
             shnro %in% folk[, unique(shnro)],
           .(shnro, kaynti_pv, hoidonantaja_koodi, toimenpide_koodi, 
             toimenpidekust_eur, toimenpidekorv_eur, korvaustaksa_eur)]
  
  # The share of missing values (looks good for all covariates):
  print(nrow(dt))
  print(round(100 * colMeans(is.na(dt)), digits=1))
  
  # Create variables for year and month:
  dt[, ':=' (year = year(kaynti_pv),
             month = month(kaynti_pv))]
  
  return(dt)
})

kela <- rbindlist(kela)


# Overview on the data:

t <- kela[, .(share_percent = round(100 * .N / nrow(kela), digits=4), .N),
          by='hoidonantaja_koodi'][order(-share_percent)]
print(t)

t <- kela[, .(share_percent = round(100 * .N / nrow(kela), digits=4), .N),
          by='toimenpide_koodi'][order(-share_percent)]
print(t)


# Create an indicator for whether the contact was remote or in-person (2022-25)
# NOTE: doctor's certificates not included.
eta <- c('E10PS', 'E10VI', 'E101L', 'E101V', 'H101E', 'H101V', 'N101E', 
         'N101V', 'E101T', 'E101P')
lasna <- c('H101L', 'N101L', '0101A', '0101B', '0101C', '0101D', 
           '0101E', '0101L')
kela[toimenpide_koodi %in% eta, etapalvelu := 1]
kela[toimenpide_koodi %in% lasna, etapalvelu := 0]

t <- kela[, .(share_percent = round(100 * .N / nrow(kela), digits=4), .N),
          by='etapalvelu'][order(-share_percent)]
print(t)


# Outcomes:

# LATER (WHEN WE KNOW MORE DETAILS) CREATE AN OUTCOME FOR VISITS REIMBURSED
# UNDER THE KELA TRIAL.

# All private doctor visits reimbursed by Kela:
kela.visits.total <- 
  kela[, .(kela.visits.total = .N), by=c('shnro', 'year', 'month')]

# In-person doctor visits reimbursed by Kela:
kela.visits.inperson <- 
  kela[etapalvelu==0, 
       .(kela.visits.inperson = .N), by=c('shnro', 'year', 'month')]

# Telemedicine doctor visits reimbursed by Kela:
kela.visits.telemed <- 
  kela[etapalvelu==1, 
       .(kela.visits.telemed = .N), by=c('shnro', 'year', 'month')]

# Monthly sum of total reimbursements in euros:
kela.reimbursements.total <- 
  kela[, .(kela.reimbursements.total = sum(toimenpidekorv_eur, na.rm=TRUE)), 
           by=c('shnro', 'year', 'month')]

# Monthly sum of total costs of reimbursed visits in euros:
kela.costs.total <- 
  kela[, .(kela.costs.total = sum(toimenpidekust_eur, na.rm=TRUE)), 
           by=c('shnro', 'year', 'month')]

rm(kela)
gc()


# Merge the outcomes to the person-month panel, impute zeroes if no health care
# use is observed, and release some memory:

dt <- merge(dt, kela.visits.total, by=c('shnro', 'year', 'month'), all.x=TRUE)
dt <- merge(dt, kela.visits.inperson, by=c('shnro', 'year', 'month'), all.x=TRUE)
dt <- merge(dt, kela.visits.telemed, by=c('shnro', 'year', 'month'), all.x=TRUE)
dt <- merge(dt, kela.reimbursements.total, by=c('shnro', 'year', 'month'), all.x=TRUE)
dt <- merge(dt, kela.costs.total, by=c('shnro', 'year', 'month'), all.x=TRUE)

dt[is.na(kela.visits.total), kela.visits.total := 0]
dt[is.na(kela.visits.inperson), kela.visits.inperson := 0]
dt[is.na(kela.visits.telemed), kela.visits.telemed := 0]
dt[is.na(kela.reimbursements.total), kela.reimbursements.total := 0]
dt[is.na(kela.costs.total), kela.costs.total := 0]

rm(kela.visits.total, kela.visits.inperson, kela.visits.telemed,
   kela.reimbursements.total, kela.costs.total)
gc()


### ### ### ### ### ### ### ### ### ### ### ### ###
#### 2) Data on the utilization of private clinics (Avohilmo+Hilmo). ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Outpatient visits:

phc <- lapply(inputs_avosh, function(input_name) {
  print(input_name)
  source.path <- paste(input_folder_thl, input_name, sep='/')
  
  # Read the datasets and subset:
  dt <- data.table::fread(source.path, na.strings = c(NA_character_, ''))
  dt <- dt[kaynti_alkoi_pvm_c >= start.date & 
             kaynti_alkoi_pvm_c <= end.date &
             shnro %in% folk[, unique(shnro)] & 
             sektori==2,
           .(shnro, kaynti_palvelumuoto, kaynti_luonne, kaynti_kavijaryhma,
             kaynti_yhteystapa, kaynti_alkoi_pvm_c, 
             kaynti_ammattiluokka)]
  
  # The share of missing values (looks good for all covariates):
  print(nrow(dt))
  print(round(100 * colMeans(is.na(dt)), digits=1))
  
  # Subset and select covariates:
  dt <- dt[kaynti_palvelumuoto=='T11' &
             kaynti_luonne=='SH' &
             kaynti_kavijaryhma==1 &
             kaynti_ammattiluokka %in% c('SH', 'LK'),
           .(shnro, kaynti_yhteystapa, kaynti_alkoi_pvm_c, 
             kaynti_ammattiluokka)]
  
  print(nrow(dt))

  return(dt)
  
})
phc <- rbindlist(phc)


# Visits to specialized healthcare:

esh <- lapply(inputs_esh, function(input_name) {
  print(input_name)
  source.path <- paste(input_folder_thl, input_name, sep='/')
  
  # Read the datasets and select covariates:
  dt <- data.table::fread(source.path, na.strings = c(NA_character_, ''))
  dt <- dt[tupva >= start.date & 
             tupva <= end.date &
             tupva == lpvm &
             shnro %in% folk[, unique(shnro)] & 
             sektori==2, 
           .(shnro, kavijary, tupva, yhteystapa, ammattiluokka, 
             ammattitk, ammattioikeus)]
  
  # Note that the number of private contacts in Hilmo varies considerably by
  # year. For instance: 405k in 2022, 34k in 2023, 210k in 2024k. We believe
  # that this smoothes the variation in the number of Avohilmo contacts.
  # If both Avohilmo and Hilmo are taken into account, the number of contacts
  # is relatively steady and somewhat similar to Kela's data.
  print(nrow(dt))
  
  # The share of missing values is also high in 2023 when there were few obs.
  print(round(100 * colMeans(is.na(dt)), digits=1))
  
  dt <- dt[, .(shnro, tupva, yhteystapa, ammattiluokka)]
  
  setnames(dt, old = c('tupva', 'yhteystapa', 'ammattiluokka'), 
           new = c('kaynti_alkoi_pvm_c', 'kaynti_yhteystapa', 
                   'kaynti_ammattiluokka'))
  
  return(dt)
  
})
esh <- rbindlist(esh) 

# Bind rows:

phc[, source := 'avohilmo']
esh[, source := 'hilmo']

phc <- rbind(phc, esh)
print(phc[, .N, by='source'])
rm(esh)


# Outcomes:
# Take unique person-by-contact-date observations, group by month:

# Outcome private.total:
# Include in-person visits and telemedicine conducted by nurses or physicians.
private.total <- 
  phc[(kaynti_yhteystapa %in% c('R10', 'R50','R51','R52','R55','R56') &
         kaynti_ammattiluokka %in% c('SH', 'LK'))
      ][, .N, by=c('shnro', 'kaynti_alkoi_pvm_c')
        ][N > 0, .(shnro, kaynti_alkoi_pvm_c)
          ][, private.total := 1
            ][, year := year(kaynti_alkoi_pvm_c)
              ][, month := month(kaynti_alkoi_pvm_c)
                ][, .(private.total = .N), by=c('shnro', 'year', 'month')]

rm(phc)
gc()


# Merge the outcomes to the person-month panel, impute zeroes if no health care
# use is observed, and release some memory:
dt <- merge(dt, private.total, by=c('shnro', 'year', 'month'), all.x=TRUE)
dt[is.na(private.total), private.total := 0]
rm(private.total)
gc()


### ### ### ### ### ### ### ### ### ### ### ### ###
#### 3) Data on PPC utilization. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Outpatient visits:

avosh <- lapply(inputs_avosh, function(input_name) {
  print(input_name)
  source.path <- paste(input_folder_thl, input_name, sep='/')
  
  # Read the datasets and subset:
  dt <- data.table::fread(source.path, na.strings = c(NA_character_, ''))
  dt <- dt[kaynti_alkoi_pvm_c >= start.date & 
             kaynti_alkoi_pvm_c <= end.date &
             shnro %in% folk[, unique(shnro)] & 
             sektori==1,
           .(avohilmoid, shnro, kaynti_palvelumuoto, kaynti_luonne, 
             kaynti_kavijaryhma, kaynti_yhteystapa, kaynti_alkoi_pvm_c, 
             kaynti_ammattiluokka, kaynti_alkoi_aika)]
  
  # The share of missing values:
  # NOTE: the share of missing kaynti_kavijaryhma has increased since 9/2024,
  # we'll have to monitor this, and we do not use it here for now.
  print(round(100 * colMeans(is.na(dt)), digits=1))
  
  # Subset and select covariates:
  dt <- dt[kaynti_palvelumuoto=='T11' &
             kaynti_luonne=='SH' &
             #kaynti_kavijaryhma==1 &
             kaynti_ammattiluokka %in% c('SH', 'LK'),
           .(avohilmoid, shnro, kaynti_yhteystapa, kaynti_alkoi_pvm_c, 
             kaynti_ammattiluokka, kaynti_alkoi_aika,
             kaynti_kavijaryhma)]
  
  
})
avosh <- rbindlist(avosh)

# If kaynti_kavijaryhma is observed, it is almost always 1=single-person:
t <- avosh[, .(share_percent = round(100 * .N / nrow(avosh), digits=4), .N),
           by='kaynti_kavijaryhma'][order(-share_percent)]
print(t)


# Care needs assessments:

hta <- lapply(inputs_hta, function(input_name) {
  print(input_name)
  source.path <- paste(input_folder_thl, input_name, sep='/')
  
  # Read the datasets and select covariates:
  dt <- data.table::fread(source.path, na.strings = c(NA_character_, ''))
  dt <- dt[hta_pvm_c >= start.date & 
             hta_pvm_c <= end.date &
             shnro %in% folk[, unique(shnro)] & 
             sektori==1,
           .(avohilmoid, shnro, hta_pvm_c, hta_aika, hta_ammattiluokka)]
  dt[, kaynti_yhteystapa :='hta']
  
})
hta <- rbindlist(hta)


# We include as separate rows those care needs assessments for which:
# - we observe no timestamp for the visit (kaynti_alkoi) OR
# - we observe that the timestamp for the visit differs from the time stamp
#   for the triage.

hta <- merge(hta, avosh[, .(avohilmoid, kaynti_alkoi_aika, kaynti_alkoi_pvm_c)],
             by='avohilmoid', all.x = TRUE)

hta <- hta[is.na(kaynti_alkoi_pvm_c) |
             !(kaynti_alkoi_aika == hta_aika & kaynti_alkoi_pvm_c == hta_pvm_c)]

# Tidy columns:
hta[, ':=' (kaynti_alkoi_aika=NULL, kaynti_alkoi_pvm_c=NULL, hta_aika=NULL)]
setnames(hta, old = c('hta_pvm_c', 'hta_ammattiluokka'),
         new = c('kaynti_alkoi_pvm_c', 'kaynti_ammattiluokka'))


# Bind rows:
phc <- rbind(hta, avosh, fill=TRUE)
rm(hta, avosh)
gc()


# Overview on the data:

t <- phc[, .(share_percent = round(100 * .N / nrow(phc), digits=4), .N),
         by='kaynti_ammattiluokka'][order(-share_percent)]
print(t)

t <- phc[, .(share_percent = round(100 * .N / nrow(phc), digits=4), .N),
         by='kaynti_yhteystapa'][order(-share_percent)]
print(t)


# Outcomes:
# Take unique person-by-contact-date observations:

# Outcome public.total:
# Include in-person visits, telemedicine contacts and professional-to-
# professional interactions conducted by nurses or physicians.

public.total <- 
  phc[kaynti_yhteystapa=='hta' | # care needs assesments
        (kaynti_yhteystapa %in% c('R10', # in-person visits
                                  'R50','R51','R52','R55','R56', # telemedicine
                                  'R60', 'R71') & # prof-to-prof interactions
           kaynti_ammattiluokka %in% c('SH','LK'))
      ][, .N, by=c('shnro', 'kaynti_alkoi_pvm_c')
        ][N > 0, .(shnro, kaynti_alkoi_pvm_c)
          ][, public.total := 1
            ][, year := year(kaynti_alkoi_pvm_c)
              ][, month := month(kaynti_alkoi_pvm_c)
                ][, .(public.total = .N), by=c('shnro', 'year', 'month')]

# Outcome public.nurse.total:
# Include in-person visits, telemedicine contacts and professional-to-
# professional interactions conducted by nurses.

public.nurse.total <- 
  phc[kaynti_yhteystapa=='hta' | # care needs assesments
        (kaynti_yhteystapa %in% c('R10', # in-person visits
                                  'R50','R51','R52','R55','R56', # telemedicine
                                  'R60', 'R71') & # prof-to-prof interactions
           kaynti_ammattiluokka %in% c('SH'))
      ][, .N, by=c('shnro', 'kaynti_alkoi_pvm_c')
        ][N > 0, .(shnro, kaynti_alkoi_pvm_c)
          ][, public.nurse.total := 1
            ][, year := year(kaynti_alkoi_pvm_c)
              ][, month := month(kaynti_alkoi_pvm_c)
                ][, .(public.nurse.total = .N), by=c('shnro', 'year', 'month')]

# Outcome public.doctor.total:
# Include in-person visits, telemedicine contacts and professional-to-
# professional interactions conducted by physicians.

public.doctor.total <- 
  phc[(kaynti_yhteystapa %in% c('R10', # in-person visits
                                  'R50','R51','R52','R55','R56', # telemedicine
                                  'R60', 'R71') & # prof-to-prof interactions
           kaynti_ammattiluokka %in% c('LK'))
      ][, .N, by=c('shnro', 'kaynti_alkoi_pvm_c')
        ][N > 0, .(shnro, kaynti_alkoi_pvm_c)
          ][, public.doctor.total := 1
            ][, year := year(kaynti_alkoi_pvm_c)
              ][, month := month(kaynti_alkoi_pvm_c)
                ][, .(public.doctor.total = .N), by=c('shnro', 'year', 'month')]


# Outcome public.total.inperson:
# Include in-person visits conducted by nurses or physicians.

public.total.inperson <- 
  phc[kaynti_yhteystapa=='R10' &
        kaynti_ammattiluokka %in% c('SH','LK')
      ][, .N, by=c('shnro', 'kaynti_alkoi_pvm_c')
        ][N > 0, .(shnro, kaynti_alkoi_pvm_c)
          ][, public.total.inperson := 1
            ][, year := year(kaynti_alkoi_pvm_c)
              ][, month := month(kaynti_alkoi_pvm_c)
                ][, .(public.total.inperson = .N), 
                  by=c('shnro', 'year', 'month')]

# Outcome public.total.telemed:
# Include telemedicine contacts and professional-to-
# professional interactions conducted by nurses or physicians.

public.total.telemed <- 
  phc[kaynti_yhteystapa=='hta' | # care needs assesments
        (kaynti_yhteystapa %in% c('R50','R51','R52','R55','R56', # telemedicine
                                  'R60', 'R71') & # prof-to-prof interactions
           kaynti_ammattiluokka %in% c('SH','LK'))
      ][, .N, by=c('shnro', 'kaynti_alkoi_pvm_c')
        ][N > 0, .(shnro, kaynti_alkoi_pvm_c)
          ][, public.total.telemed := 1
            ][, year := year(kaynti_alkoi_pvm_c)
              ][, month := month(kaynti_alkoi_pvm_c)
                ][, .(public.total.telemed = .N), 
                  by=c('shnro', 'year', 'month')]

rm(phc)
gc()


# Merge the outcomes to the person-month panel, impute zeroes if no health care
# use is observed, and release some memory:

dt <- merge(dt, public.total, by=c('shnro', 'year', 'month'), all.x=TRUE)
dt <- merge(dt, public.total.telemed, by=c('shnro', 'year', 'month'), all.x=TRUE)
dt <- merge(dt, public.total.inperson, by=c('shnro', 'year', 'month'), all.x=TRUE)
dt <- merge(dt, public.nurse.total, by=c('shnro', 'year', 'month'), all.x=TRUE)
dt <- merge(dt, public.doctor.total, by=c('shnro', 'year', 'month'), all.x=TRUE)

dt[is.na(public.total), public.total := 0]
dt[is.na(public.total.telemed), public.total.telemed := 0]
dt[is.na(public.total.inperson), public.total.inperson := 0]
dt[is.na(public.nurse.total), public.nurse.total := 0]
dt[is.na(public.doctor.total), public.doctor.total := 0]

rm(public.total, public.total.telemed, public.total.inperson,
   public.nurse.total, public.doctor.total)
gc()


# Construct relative time (1 = 1/2022, 2 = 2/2022, ...):
dt.help <- CJ(year = dt[, sort(unique(year))],
              month = dt[, sort(unique(month))])
dt.help[, relative.time := 1:nrow(dt.help)]
dt <- merge(dt, dt.help, by=c('year', 'month'), all.x = TRUE)


# Save:
data.table::fwrite(dt, file=output_panel)
print(dt)

# End.
rm(list=ls())
gc()
toc()
