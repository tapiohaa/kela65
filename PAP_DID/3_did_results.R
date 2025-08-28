
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
###             r-script 3_did_results.R              ###
###                 Replication file.                 ###
###                    2025 by TH                     ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Plot trends plots and report DID results.
rm(list=ls())

# Install and load the following packages:
library(here)             # Relative file paths.
library(data.table)       # Mutating and aggregating data.
library(fixest)           # Fast fixed effects estimation.
library(ggplot2)          # Creating plots.
library(viridis)          # accessible color palette.
library(patchwork)        # Combining plots.
library(stargazer)        # Save as tex file.
library(tictoc)           # timing 

source("~DATAINFRA/functions.R") # some helper functions

# Inputs:
input_population <- here('data', 'study_population.csv')
input_panel <- here('data', 'person_by_month_panel.csv')

# Outputs:
output.plot_kela_main <- here('figures', 'placebo_trends_and_did_plot_kela_main.pdf')
output.plot_kela_contacttype <- here('figures', 'placebo_trends_and_did_plot_kela_contacttype.pdf')
output.plot_private_thl <- here('figures', 'placebo_trends_and_did_plot_private_thl.pdf')
output.plot_ppc_professions <- here('figures', 'placebo_trends_and_did_plot_ppc_professions.pdf')
output.plot_ppc_contacttype <- here('figures', 'placebo_trends_and_did_plot_ppc_contacttype.pdf')
output.table <- here('tables', 'placebo_did_results')

# Start timing:
tic.clearlog()
tic()

###
###

# Study population and their characteristics:
folk <- fread(input_population, na.strings = c(NA_character_, ''), 
              select = c('shnro', 'age.65.plus', 'ika'))

# Healthcare utilization:
dt <- fread(
  input_panel, na.strings = c(NA_character_, ''),
  select=c('shnro', 'year', 'month', 'relative.time',
           'kela.visits.total', 'kela.reimbursements.total', 'kela.costs.total',
           'kela.visits.telemed', 'kela.visits.inperson',
           'private.total', 
           'public.total', 'public.nurse.total', 'public.doctor.total',
           'public.total.telemed', 'public.total.inperson'))

# Merge treatment status to healthcare utilization:
dt <- merge(dt, folk[, .(shnro, age.65.plus, ika)], by='shnro', all.x = TRUE)
dt[, month.year := paste0(month, '/', year)]


### ### ### ### ### ### ### ### ### ### ### ### ###
#### 1) Function: aggregate data. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


### A function that aggregates data for plotting. ###

aggr.data <- function(data, otc, start.date=3, treatment=27) {
  # INPUTS:
  # data: df
  # otc: outcome as character
  # start.date: start month as relative time (3 = 3/2022)
  # treatment: treatment start month as relative time (27 = 3/2024)
  # OUTPUTS:
  # returns a list of data.tables containing aggregated data for plotting.
  
  
  # 1: Compute the mean monthly healthcare use (or costs) by age group:
  dt.groups <- data[relative.time >= start.date, 
                    .(annual.mean = 12 * mean(get(otc))),                
                    by=c('relative.time', 'month.year', 'age.65.plus')]
  
  # Create labels for age groups:
  dt.groups[age.65.plus==1, 
            treat.group := paste0(data[age.65.plus==1, min(ika)], '+')]
  dt.groups[age.65.plus==0, 
            treat.group := paste0(data[age.65.plus==0, min(ika)], '-', 
                                  data[age.65.plus==0, max(ika)])]
  

  # 2: Pivot wider and compute difference in outcomes between age groups:
  dt.diff <- 
    dcast(dt.groups, relative.time + month.year ~ 
            paste0('age.65.plus_', age.65.plus), 
          value.var = 'annual.mean')
  dt.diff[, difference := age.65.plus_1 - age.65.plus_0]
  
  
  # 3: Adjusted difference (subtract linear pre-trends and calendar month
  # effects from the differences in outcomes):
  
  # Create covariates for month and year:
  dt.diff[, c('month', 'year') := tstrsplit(month.year, '/', fixed=TRUE)]
  
  # Compute pre-treatment monthly means in the difference in outcomes between
  # the age groups:
  df.pre.means <- dt.diff[relative.time < treatment, 
                          .(pre.mean = mean(difference)), by='month']
  
  # Merge monthly pre-means and compute adjusted difference:
  dt.diff <- merge(dt.diff, df.pre.means, by='month', all.x = TRUE)
  dt.diff[, adj.difference := difference - pre.mean]
  
  # Fit OLS model based on pre-treatment trends:
  ols <- feols(adj.difference ~ relative.time, 
               data = dt.diff[relative.time < treatment])
  
  # Predict for all periods:
  preds <- data.table(
    relative.time = dt.diff[, sort(relative.time)],
    prediction =predict(ols, dt.diff[, .(relative.time)][order(relative.time)]))
  
  # Merge predictions to dt.diff:
  dt.diff <- merge(dt.diff, preds, by='relative.time', all.x = TRUE)
  
  # Update adjusted difference (subtract linear pre-trends):
  dt.diff[, adj.difference := adj.difference - prediction]
  
  # Create a variable for pre and post periods relative to the treatment:
  dt.diff[, period := fcase(relative.time >= treatment, 'post', default='pre')]
  
  
  # 4: Compute plain DID results.
  # Compute mean differences in means before and after the treatment:
  dt.did <- dt.diff[relative.time %in% c((treatment - 12) : (treatment + 11)), 
                    .(means = mean(difference)), by=period]
  dt.did[, id := 1]
  dt.did <- dcast(dt.did, id ~ paste0('period_', period), value.var = 'means')
  
  # Compute i) pre-mean for the treated, ii) the DID estimand, and iii)
  # the DID estimate relative (%) to the pre-mean:
  pre.mean.treated <- dt.diff[period=='pre' & relative.time >= (treatment - 12), 
                              mean(age.65.plus_1)]
  dt.did[, ':=' (pre.mean.treated = pre.mean.treated, 
                 estimate.did = period_post - period_pre)]
  dt.did[, estimate.did.relative := 100 * estimate.did / pre.mean.treated]
  
  
  # 5: Compute adjusted DID results:
  
  # Compute mean adjusted differences in means before and after the treatment:
  dt.did.adj <- dt.diff[, .(means = mean(adj.difference)), by=period]
  dt.did.adj[, id := 1]
  dt.did.adj <- dcast(dt.did.adj, id ~ paste0('period_', period), 
                      value.var = 'means')
  
  # Compute i) pre-mean for the treated, ii) the DID estimand, and iii)
  # the DID estimate relative (%) to the pre-mean:
  pre.mean.treated <- dt.diff[period=='pre' & relative.time >= (treatment - 12), 
                              mean(age.65.plus_1)]
  dt.did.adj[, ':=' (pre.mean.treated = pre.mean.treated, 
                     estimate.did = period_post - period_pre)]
  dt.did.adj[, estimate.did.relative := 100 * estimate.did / pre.mean.treated]
  
  
  # 6: Create a table that contains axis breaks and labels for plotting:
  
  dt.axix.breaks <- 
    unique(dt.groups[, .(relative.time, month.year)])[order(relative.time)]
  dt.axix.breaks <- dt.axix.breaks[seq(1, nrow(dt.axix.breaks), by=4)]
  
  return(
    list('dt.outcome' = otc,
         'dt.groups' = dt.groups, 
         'dt.difference' = dt.diff,
         'dt.did' = dt.did,
         'dt.did.adj' = dt.did.adj,
         'treatment' = treatment,
         'dt.axix.breaks' = dt.axix.breaks))
  
}
test <- aggr.data(data=dt, otc='public.total')
test


### ### ### ### ### ### ### ### ### ### ### ### ###
#### 2) Function: plot based on aggregated data. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


plot.trend.plots <- function(data) {
  # INPUTS:
  # data: output from the aggr.data function
  # OUTPUT:
  # a list containing ggplot objects.
  
  
  # Y-axis labels depend on the outcome:
  if(
    data$dt.outcome %in% c('kela.visits.total', 
                           'kela.visits.telemed', 
                           'kela.visits.inperson')) {
    
    y.title_p.groups <- 'Käynnit'
    y.title_p.diff <- 'Erotus käynneissä'
    y.title_p.adj.diff <- 'Oikaistu erotus käynneissä'
    
  } else if (
    data$dt.outcome %in% c('kela.reimbursements.total', 
                           'kela.costs.total')) {
    
    y.title_p.groups <- 'Euroa'
    y.title_p.diff <- 'Erotus euroissa'
    y.title_p.adj.diff <- 'Oikaistu erotus euroissa'
    
  } else {
    
    y.title_p.groups <- 'Kontaktipäivät'
    y.title_p.diff <- 'Erotus kontaktipäivissä'
    y.title_p.adj.diff <- 'Oikaistu erotus kontaktipäivissä' }
  
  
  # 1: A plot that shows monthly outcome means for age groups separately:
  
  p.groups <- 
    ggplot(data$dt.groups, aes(x=relative.time, y=annual.mean, 
                               color=treat.group, shape=treat.group)) +
    geom_vline(xintercept = (data$treatment - 0.5), linetype='dashed') + 
    geom_label(data = data.table(x = (data$treatment - 0.5), 
                                 y = 1.1 * data$dt.groups[, max(annual.mean)]),
               aes(x=x, y=y, label='Kokeilu alkaa'), size=5, 
               inherit.aes = FALSE)+
    geom_point() + 
    geom_line() +
    scale_x_continuous(breaks= data$dt.axix.breaks[, relative.time], 
                       labels = data$dt.axix.breaks[, month.year]) +
    scale_color_viridis_d() +
    labs(y = y.title_p.groups,
         color = 'Ikäryhmä:',
         shape = 'Ikäryhmä:',
         title = 'I Keskiarvot ikäryhmittäin') +
    guides(color = guide_legend(position='inside', nrow=1),
           group = guide_legend(position='inside', nrow=1),
           shape = guide_legend(position='inside', nrow=1)) +
    scale_y_continuous(expand = expansion(mult = c(0.2, 0.2))) +
    theme(text = element_text(size=20),
          legend.position.inside = c(0.25, 0.9),
          legend.background = element_rect(fill='white', color='black', 
                                           linewidth=1),
          legend.text = element_text(size=12),
          legend.title = element_blank(),
          panel.background = element_rect(fill = 'white', colour = 'white'),
          panel.grid.major = element_line(linewidth=0.25, linetype = 'solid',
                                          color = 'lightgrey'),
          panel.grid.minor = element_line(linewidth=0.25, linetype = 'solid',
                                          color = 'lightgrey'),
          panel.border = element_rect(colour='black', fill=NA, linewidth=0.5),
          axis.text.x = element_text(angle=45, hjust=1),
          axis.title.x = element_blank()) 
  
  
  # 2: A plot that shows the monthly difference in mean outcomes between
  # the age groups:
  
  # DID estimate as rounded character string:
  did.string <- 
    paste0(
      'DID-estimaatti:\n',
      format(round(data$dt.did[, estimate.did], digits=3), nsmall=3), ' [', 
      format(round(data$dt.did[, estimate.did.relative], digits=1), nsmall=1),
      ' %]')
  
  # Plot:
  p.diff <- 
    ggplot(data$dt.difference, aes(x=relative.time, y=difference)) +
    geom_hline(yintercept = 0, linetype='dashed') + 
    geom_vline(xintercept = (data$treatment - 0.5), linetype='dashed') + 
    geom_label(
      data=data.table(x = mean(c(data$dt.difference[, (min(relative.time) - 5)],
                                 data$treatment-1 - 5)), 
                      y = 1.2* data$dt.difference[, max(abs(difference))]),
      aes(x=x, y=y, label=did.string), size=5, inherit.aes = FALSE) +
    geom_label(data = data.table(x=(data$treatment - 0.5), 
                                 y=1.2* data$dt.difference[, max(abs(difference))]),
               aes(x=x, y=y, label='Kokeilu alkaa'), size=5, 
               inherit.aes = FALSE) +
    geom_segment(
      data = data.table(x=(data$treatment - 12.5),
                        xend = (data$treatment - 0.5),
                        y = data$dt.did[, period_pre],
                        yend = data$dt.did[, period_pre]), 
      aes(x=x, y=y, xend=xend, yend=yend), color = '#440154FF', linewidth=2) +
    geom_segment(
      data = data.table(
        x = (data$treatment - 0.5),
        xend = (data$treatment + 11.5),
        y = data$dt.did[, period_post],
        yend = data$dt.did[, period_post]), 
      aes(x=x, y=y, xend=xend, yend=yend), color = '#440154FF', linewidth=2) +
    geom_point() + 
    geom_line() +
    scale_x_continuous(breaks = data$dt.axix.breaks[, relative.time], 
                       labels = data$dt.axix.breaks[, month.year]) +
    scale_y_continuous(expand = expansion(mult = c(0.2, 0.2))) +
    labs(y = y.title_p.diff,
         title = 'II raaka-DID') +
   theme(text = element_text(size=20),
          legend.position = 'bottom',
          panel.background = element_rect(fill = 'white', colour = 'white'),
          panel.grid.major = element_line(linewidth=0.25, linetype = 'solid',
                                          color = 'lightgrey'),
          panel.grid.minor = element_line(linewidth=0.25, linetype = 'solid',
                                          color = 'lightgrey'),
          panel.border = element_rect(colour='black', fill=NA, linewidth=0.5),
          axis.text.x = element_text(angle=45, hjust=1),
          axis.title.x = element_blank()) 
  
  
  # 3: A plot that shows the adjusted monthly difference in outcomes between
  # the age groups.
  
  # DID estimate as rounded character string:
  did.string.adj <- 
    paste0(
      'DID-estimaatti:\n',
      format(round(data$dt.did.adj[, estimate.did], digits=3), nsmall=3), ' [', 
      format(round(data$dt.did.adj[, estimate.did.relative], digits=1), 
             nsmall=1), ' %]')
  
  # Plot:
  p.adj.diff <- 
    ggplot(data$dt.difference, aes(x=relative.time, y=adj.difference)) +
    geom_hline(yintercept = 0, linetype='dashed') + 
    geom_vline(xintercept = (data$treatment - 0.5), linetype='dashed') + 
    geom_label(
      data=data.table(x = mean(c(data$dt.difference[, (min(relative.time) - 5)],
                                 data$treatment-1 - 5)), 
                      y = 1.2* data$dt.difference[, max(abs(adj.difference))]),
      aes(x=x, y=y, label=did.string.adj), size=5, inherit.aes = FALSE) +
    geom_label(data = 
                 data.table(x = (data$treatment - 0.5), 
                            y = 1.2*
                              data$dt.difference[, max(abs(adj.difference))]),
               aes(x=x, y=y, label='Kokeilu alkaa'), 
               size=5, inherit.aes = FALSE) +
    geom_segment(
      data = data.table(x=data$dt.difference[period=='pre', min(relative.time)],
                        xend = (data$treatment-0.5),
                        y = data$dt.did.adj[, period_pre],
                        yend = data$dt.did.adj[, period_pre]), 
      aes(x=x, y=y, xend=xend, yend=yend), color = '#440154FF', linewidth=2) +
    geom_segment(
      data = data.table(
        x = (data$treatment-0.5),
        xend = data$dt.difference[period=='post', max(relative.time)],
        y = data$dt.did.adj[, period_post],
        yend = data$dt.did.adj[, period_post]), 
      aes(x=x, y=y, xend=xend, yend=yend), color = '#440154FF', linewidth=2) +
    geom_point() + 
    geom_line() +
    scale_x_continuous(breaks= data$dt.axix.breaks[, relative.time], 
                       labels = data$dt.axix.breaks[, month.year]) +
    scale_y_continuous(expand = expansion(mult = c(0.2, 0.2))) +
    labs(y = y.title_p.adj.diff,
         title = 'III oikaistu DID') +
    theme(text = element_text(size=20),
          legend.position = 'bottom',
          panel.background = element_rect(fill = 'white', colour = 'white'),
          panel.grid.major = element_line(linewidth=0.25, linetype = 'solid',
                                          color = 'lightgrey'),
          panel.grid.minor = element_line(linewidth=0.25, linetype = 'solid',
                                          color = 'lightgrey'),
          panel.border = element_rect(colour='black', fill=NA, linewidth=0.5),
          axis.text.x = element_text(angle=45, hjust=1),
          axis.title.x = element_blank()) 
  
  return(list(p.groups = p.groups,
              p.diff = p.diff,
              p.adj.diff = p.adj.diff))
  
  
}

test.plots <- plot.trend.plots(data=test)
wrap_plots(test.plots)


### ### ### ### ### ### ### ### ### ### ### ### ###
#### 3) Draw the plots and save. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# loop over the following outcomes:
outcomes <- c(
  'kela.visits.total', 'kela.reimbursements.total', 'kela.costs.total',
  'kela.visits.telemed', 'kela.visits.inperson', 'private.total', 
  'public.total', 'public.nurse.total', 'public.doctor.total',
  'public.total.telemed', 'public.total.inperson'
)

plots <- lapply(outcomes, function(outcome) {
  
  print(outcome)
  dt <- aggr.data(data=dt, otc=outcome)
  plot <- plot.trend.plots(data=dt)
    
  return(list(data=dt, plot=plot))
  
})
names(plots) <- outcomes


# Save plots:

ggsave(
  filename = output.plot_kela_main, width = 15, height = 16, 
  plot = wrap_elements(panel = plots$kela.visits.total$plot$p.groups + 
                         plots$kela.visits.total$plot$p.diff + 
                         plots$kela.visits.total$plot$p.adj.diff) + 
    ggtitle('A. Korvatut yksityislääkärikäynnit (per hlö per vuosi)') + 
    plot_layout(ncol=1) +
    theme(plot.title = element_text(size=35, hjust=0.5)) +
    wrap_elements(panel = plots$kela.reimbursements.total$plot$p.groups + 
                    plots$kela.reimbursements.total$plot$p.diff + 
                    plots$kela.reimbursements.total$plot$p.adj.diff) +
    ggtitle('B. Maksetut korvaukset (per hlö per vuosi)') + 
    plot_layout(ncol=1) +
    theme(plot.title = element_text(size=35, hjust=0.5)) +
    wrap_elements(panel = plots$kela.costs.total$plot$p.groups + 
                    plots$kela.costs.total$plot$p.diff + 
                    plots$kela.costs.total$plot$p.adj.diff) +
    ggtitle('C. Käyntien kokonaiskustannukset (per hlö per vuosi)') + 
    plot_layout(ncol=1) +
    theme(plot.title = element_text(size=35, hjust=0.5)) )

ggsave(
  filename = output.plot_kela_contacttype, width = 15, height = 10.666, 
  plot = wrap_elements(panel = plots$kela.visits.telemed$plot$p.groups + 
                         plots$kela.visits.telemed$plot$p.diff + 
                         plots$kela.visits.telemed$plot$p.adj.diff) + 
    ggtitle('A. Korvatut etäasioinnit (per hlö per vuosi)') + 
    plot_layout(ncol=1) +
    theme(plot.title = element_text(size=35, hjust=0.5)) +
    wrap_elements(panel = plots$kela.visits.inperson$plot$p.groups + 
                    plots$kela.visits.inperson$plot$p.diff + 
                    plots$kela.visits.inperson$plot$p.adj.diff) +
    ggtitle('B. Korvatut läsnäkäynnit (per hlö per vuosi)') + 
    plot_layout(ncol=1) +
    theme(plot.title = element_text(size=35, hjust=0.5)) )

ggsave(
  filename = output.plot_private_thl, width = 15, height = 5.33, 
  plot = wrap_elements(panel = plots$private.total$plot$p.groups + 
                         plots$private.total$plot$p.diff + 
                         plots$private.total$plot$p.adj.diff) + 
    ggtitle('A. Yksityiset kontaktit (per hlö per vuosi)') + 
    plot_layout(ncol=1) +
    theme(plot.title = element_text(size=35, hjust=0.5)) )

ggsave(
  filename = output.plot_ppc_professions, width = 15, height = 16, 
  plot = wrap_elements(panel = plots$public.total$plot$p.groups + 
                         plots$public.total$plot$p.diff + 
                         plots$public.total$plot$p.adj.diff) + 
    ggtitle('A. Julkiset kontaktit (per hlö per vuosi)') + 
    plot_layout(ncol=1) +
    theme(plot.title = element_text(size=35, hjust=0.5)) +
    wrap_elements(panel = plots$public.nurse.total$plot$p.groups + 
                    plots$public.nurse.total$plot$p.diff + 
                    plots$public.nurse.total$plot$p.adj.diff) +
    ggtitle('B. Julkiset hoitajakontaktit (per hlö per vuosi)') + 
    plot_layout(ncol=1) +
    theme(plot.title = element_text(size=35, hjust=0.5)) +
    wrap_elements(panel = plots$public.doctor.total$plot$p.groups + 
                    plots$public.doctor.total$plot$p.diff + 
                    plots$public.doctor.total$plot$p.adj.diff) +
    ggtitle('C. Julkiset lääkärikontaktit (per hlö per vuosi)') + 
    plot_layout(ncol=1) +
    theme(plot.title = element_text(size=35, hjust=0.5)) )

ggsave(
  filename = output.plot_ppc_contacttype, width = 15, height = 10.666, 
  plot = wrap_elements(panel = plots$public.total.telemed$plot$p.groups + 
                         plots$public.total.telemed$plot$p.diff + 
                         plots$public.total.telemed$plot$p.adj.diff) + 
    ggtitle('A. Julkiset etäasioinnit (per hlö per vuosi)') + 
    plot_layout(ncol=1) +
    theme(plot.title = element_text(size=35, hjust=0.5)) +
    wrap_elements(panel = plots$public.total.inperson$plot$p.groups + 
                    plots$public.total.inperson$plot$p.diff + 
                    plots$public.total.inperson$plot$p.adj.diff) +
    ggtitle('B. Julkiset läsnäkäynnit (per hlö per vuosi)') + 
    plot_layout(ncol=1) +
    theme(plot.title = element_text(size=35, hjust=0.5)) )


### ### ### ### ### ### ### ### ### ### ### ### ###
#### 4) Create the results table. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Select and order outcomes to be presented in the paper:
outcomes <- c(
  'kela.visits.total', 'kela.reimbursements.total', 'kela.costs.total',
  'kela.visits.telemed', 'kela.visits.inperson', 
  'public.total', 'public.nurse.total', 'public.doctor.total',
  'public.total.telemed', 'public.total.inperson'
)


# Collect results:
table <- lapply(outcomes, function(outcome) {
  
  dt.did <- plots[[outcome]]$data$dt.did
  dt.did.adj <- plots[[outcome]]$data$dt.did.adj
  
  data.table(
    outcome = outcome, 
    pre.mean.treated = dt.did[, pre.mean.treated],
    estimate.did = dt.did[, estimate.did],
    estimate.did.relative = dt.did[, estimate.did.relative],
    estimate.did.adj = dt.did.adj[, estimate.did],
    estimate.did.adj.relative = dt.did.adj[, estimate.did.relative],
    estimate.did.mean = mean(c(dt.did[, estimate.did], 
                               dt.did.adj[, estimate.did])),
    estimate.did.relative.mean = mean(c(dt.did[, estimate.did.relative], 
                                        dt.did.adj[, estimate.did.relative])))
  
})
table <- rbindlist(table)

# Round and format:

table[, pre.mean.treated := format(round(pre.mean.treated, digits=3), nsmall=3)]

table[, estimate.did :=
        paste0(format(round(estimate.did, digits=3), nsmall=3), ' [', 
               format(round(estimate.did.relative, digits=1), nsmall=1), '%]')]

table[, estimate.did.adj :=
        paste0(format(round(estimate.did.adj, digits=3), nsmall=3), ' [', 
               format(round(estimate.did.adj.relative, digits=1), nsmall=1), 
               '%]')]

table[, estimate.did.mean :=
        paste0(format(round(estimate.did.mean, digits=3), nsmall=3), ' [', 
               format(round(estimate.did.relative.mean, digits=1), nsmall=1), 
               '%]')]

# Order columns:
table <- table[, .(outcome, pre.mean.treated, estimate.did.mean, estimate.did, 
                   estimate.did.adj)]

# Tidy:
table[, estimate.did := gsub('^ ', '+', estimate.did)]
table[, estimate.did := gsub('\\[ ', '\\[+', estimate.did)]
table[, estimate.did.adj := gsub('^ ', '+', estimate.did.adj)]
table[, estimate.did.adj := gsub('\\[ ', '\\[+', estimate.did.adj)]
table[, estimate.did.mean := gsub('^ ', '+', estimate.did.mean)]
table[, estimate.did.mean := gsub('\\[ ', '\\[+', estimate.did.mean)]

# Kela-korvatut lääkärinpalkkiot:
table[outcome == 'kela.visits.total', 
      outcome := 'Kaikki lääkärikäynnit']
table[outcome == 'kela.reimbursements.total', 
      outcome := 'Maksetut korvaukset']
table[outcome == 'kela.costs.total', 
      outcome := 'Kokonaiskustannukset']
table[outcome == 'kela.visits.telemed', 
      outcome := 'Etäasioinnit']
table[outcome == 'kela.visits.inperson', 
      outcome := 'Läsnäasioinnit']

# Julkisen avoisairaanhoidon kontaktit:
table[outcome == 'public.total', 
      outcome := 'Käyntipäivät']
table[outcome == 'public.nurse.total', 
      outcome := 'Käyntipäivät hoitajalla']
table[outcome == 'public.doctor.total', 
      outcome := 'Käyntipäivät lääkärillä']
table[outcome == 'public.total.telemed', 
      outcome := 'Etäasiointipäivät']
table[outcome == 'public.total.inperson', 
      outcome := 'Läsnäasiointipäivät']

# Tidy column names:
setnames(table, old='outcome', new='Vaste (per asukas per vuosi)')
setnames(table, old='pre.mean.treated', 
         new='Keskiarvo koeryhmässä ennen kokeilua')
setnames(table, old='estimate.did.mean', new='DID-estimaattien ka.')
setnames(table, old='estimate.did', new='raaka DID-estimaatti')
setnames(table, old='estimate.did.adj', new='oikaistu DID-estimaatti')

# Save:
stargazer::stargazer(
  table, out=paste0(output.table, '.tex'),
  type='text', summary=F, rownames=F, header=F)
fwrite(table, file=paste0(output.table, '.csv'))


# End.
rm(list=ls())
gc()
toc()
