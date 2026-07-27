#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(data.table); library(dplyr); library(ggplot2); library(patchwork)
  library(scales); library(grid); library(svglite); library(ragg)
})
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
tab <- file.path(root, "07_results", "tables")
fig <- file.path(root, "07_results", "figures")
dir.create(fig, recursive = TRUE, showWarnings = FALSE)
ink <- "#202124"; gray <- "#737373"; navy <- "#294C7A"; teal <- "#138A81"; coral <- "#C8574D"; gold <- "#B98522"
theme_policy <- function(size = 7) theme_classic(base_size = size, base_family = "Arial") +
  theme(text = element_text(colour = ink), axis.line = element_line(linewidth=.35),
        axis.ticks = element_line(linewidth=.35), axis.text = element_text(size=size-.35),
        axis.title = element_text(size=size), legend.title = element_blank(),
        legend.text = element_text(size=size-.6), plot.title = element_text(face="bold", size=size+.4),
        plot.subtitle = element_text(size=size-.4, colour=gray), plot.tag = element_text(face="bold", size=size+1),
        panel.grid.major.y = element_line(colour="#EAEAEA", linewidth=.25), panel.grid.minor = element_blank(),
        plot.margin = margin(4,5,4,5))
save_policy <- function(p, name, w=183, h=118) {
  base <- file.path(fig, name); wi <- w/25.4; hi <- h/25.4
  svglite::svglite(paste0(base,".svg"), width=wi, height=hi); print(p); dev.off()
  grDevices::cairo_pdf(paste0(base,".pdf"), width=wi, height=hi, family="Arial"); print(p); dev.off()
  ragg::agg_tiff(paste0(base,".tiff"), width=wi, height=hi, units="in", res=600, compression="lzw"); print(p); dev.off()
  ragg::agg_png(paste0(base,".png"), width=wi, height=hi, units="in", res=300); print(p); dev.off()
}

# Figure 1 was intentionally removed at the author's request. Figure 2 is the
# first retained main figure.
if (FALSE) {
timeline <- data.frame(cohort=c(rep("CFPS",3),rep("CHARLS",4),rep("CLASS",3)),
  year=c(2012,2014,2018,2011,2013,2015,2018,2018,2020,2023),
  role=c(rep("Pilot-area quasi-experiment",3),rep("National expansion-period distributional analysis",4),rep("Post-policy within-person validation",3)))
role_cols <- c("Pilot-area quasi-experiment"=coral,"National expansion-period distributional analysis"=navy,"Post-policy within-person validation"=teal)
p1a <- ggplot(timeline,aes(year,cohort,colour=role,group=cohort)) +
  geom_line(linewidth=1.05, colour="#B8B8B8") + geom_point(size=2.8) +
  geom_vline(xintercept=2016,linetype="dashed",colour=coral,linewidth=.55) +
  annotate("label",x=2016,y=3.55,label="2016 national expansion",size=2.25,label.size=.2,colour=coral,fill="white") +
  scale_colour_manual(values=role_cols) + scale_x_continuous(breaks=c(2011,2012,2013,2014,2015,2016,2018,2020,2023)) +
  labs(x="Survey year",y=NULL,title="Policy timing and evidence roles") + theme_policy() + theme(legend.position="bottom",legend.box="vertical")
sample_roles <- data.frame(x=1,y=c(3,2,1),heading=c("CFPS", "CHARLS", "CLASS"),
  detail=c("Pilot versus non-pilot areas\nDID, DDD and event-study estimates", "National expansion period\n2015-2018 incident ADL and age-group differential change", "Post-policy longitudinal follow-up\n2018, 2020 and 2023 within-person changes"), col=c(coral,navy,teal))
p1b <- ggplot(sample_roles) + geom_segment(aes(x=0,xend=0,y=y-.28,yend=y+.28,colour=col),linewidth=1.5,show.legend=FALSE) +
  geom_text(aes(x=.12,y=y+.12,label=heading),hjust=0,fontface="bold",size=3.1) +
  geom_text(aes(x=.12,y=y-.12,label=detail),hjust=0,vjust=1,size=2.45,lineheight=.98,colour=gray) +
  scale_colour_identity() + coord_cartesian(xlim=c(-.1,2.2),ylim=c(.5,3.5),clip="off") +
  labs(title="Three complementary policy-evidence layers") + theme_void(base_family="Arial") + theme(plot.title=element_text(face="bold",size=7.4,hjust=0),plot.margin=margin(8,5,5,5))
save_policy(p1a + p1b + plot_annotation(tag_levels="a") & theme(plot.tag=element_text(face="bold",size=9)), "Figure1_policy_evidence_framework_v5",183,75)
}

# Figure 2: CFPS pilot-area quasi-experimental estimates.
main <- fread(file.path(tab,"r_corrected_main_results.csv"))
cfps <- main |> filter(grepl("CFPS pilot",analysis)) |> mutate(
  label=case_when(grepl("DID$",analysis)~"Pilot-area DID",grepl("activity limitation",estimand)~"High-need DDD",TRUE~"Age 75+ DDD"),
  estimate=100*estimate,conf_low=100*conf_low,conf_high=100*conf_high)
p2a <- ggplot(cfps,aes(estimate,reorder(label,estimate),colour=label)) +
  geom_vline(xintercept=0,colour=gray,linewidth=.35) + geom_errorbarh(aes(xmin=conf_low,xmax=conf_high),height=.14,linewidth=.55) +
  geom_point(size=2) + scale_colour_manual(values=c(coral,gold,teal)) +
  labs(x="Risk difference in poor self-rated health (percentage points)",y=NULL,title="CFPS pilot-area policy estimates") + theme_policy() + theme(legend.position="none")
ev <- fread(file.path(tab,"r_cfps_event_study.csv")) |> filter(analysis=="CFPS pilot-area event study") |> mutate(year=as.integer(year),estimate=100*estimate,conf_low=100*conf_low,conf_high=100*conf_high)
p2b <- ggplot(ev,aes(year,estimate)) + geom_hline(yintercept=0,colour=gray,linewidth=.35) +
  geom_vline(xintercept=2016,linetype="dashed",colour=coral,linewidth=.55) +
  geom_errorbar(aes(ymin=conf_low,ymax=conf_high),width=.12,colour=navy) + geom_line(colour=navy,linewidth=.65) + geom_point(colour=navy,size=2) +
  scale_x_continuous(breaks=c(2012,2014,2018)) + labs(x="Survey year",y="Pilot-area contrast (percentage points)",title="Event-study diagnostic") + theme_policy()
save_policy(p2a + p2b + plot_layout(widths=c(1,1.05)) + plot_annotation(tag_levels="a") & theme(plot.tag=element_text(face="bold",size=9)),"Figure2_CFPS_policy_estimates_v5",183,78)

# Figure 3: aligned policy-period and post-policy estimates on one metric.
placebo <- fread(file.path(tab,"r_v4_charls_placebo_periods.csv")) |> mutate(label=specification)
p3a <- ggplot(placebo,aes(estimate,reorder(label,wave),colour=analysis)) +
  geom_vline(xintercept=0,colour=gray,linewidth=.35) + geom_errorbarh(aes(xmin=conf_low,xmax=conf_high),height=.14,linewidth=.55) + geom_point(size=2) +
  scale_colour_manual(values=c("Observed 2015-2018 period"=navy,"Placebo period"=gray)) +
  coord_cartesian(xlim=c(-15,17)) + labs(x="Change (percentage points)",y=NULL,title="CHARLS: age-group differential change",subtitle="Blue: 2015-2018; grey: placebo periods") + theme_policy() + theme(legend.position="none")
class_change <- fread(file.path(tab,"r_class_longitudinal_changes.csv")) |> filter(year %in% c(2020,2023),outcome %in% c("Poor self-rated health","Need help with activities of daily living")) |> mutate(estimate=100*estimate,conf_low=100*conf_low,conf_high=100*conf_high,outcome=factor(outcome,levels=rev(c("Poor self-rated health","Need help with activities of daily living"))))
p3b <- ggplot(class_change,aes(estimate,outcome,colour=factor(year),shape=factor(year))) +
  geom_vline(xintercept=0,colour=gray,linewidth=.35) + geom_errorbarh(aes(xmin=conf_low,xmax=conf_high),height=.14,position=position_dodge(width=.45),linewidth=.5) +
  geom_point(position=position_dodge(width=.45),size=2) + scale_colour_manual(values=c("2020"=teal,"2023"=navy),labels=c("2020 vs 2018","2023 vs 2018")) +
  scale_shape_discrete(labels=c("2020 vs 2018","2023 vs 2018")) + coord_cartesian(xlim=c(-15,17)) + labs(x="Change (percentage points)",y=NULL,title="CLASS: post-policy within-person change",subtitle="Circle: 2020 vs 2018; triangle: 2023 vs 2018") + theme_policy() + theme(legend.position="none")
hetero <- fread(file.path(tab,"r_class_postpolicy_age_heterogeneity.csv")) |> filter(outcome=="Need help with activities of daily living", grepl("age75",term)) |> mutate(label=ifelse(grepl("2020",term),"2020 vs 2018","2023 vs 2018"),estimate=100*estimate,conf_low=100*conf_low,conf_high=100*conf_high)
p3c <- ggplot(hetero,aes(estimate,reorder(label,estimate))) + geom_vline(xintercept=0,colour=gray,linewidth=.35) +
  geom_errorbarh(aes(xmin=conf_low,xmax=conf_high),height=.14,colour=teal,linewidth=.55) + geom_point(colour=teal,size=2) +
  coord_cartesian(xlim=c(-15,17)) + labs(x="Additional change for age 75+ (percentage points)",y=NULL,title="CLASS: age heterogeneity in ADL help") + theme_policy()
fig3 <- (p3a / p3b / p3c) + plot_layout(heights=c(1,1,1)) + plot_annotation(tag_levels="a") & theme(plot.tag=element_text(face="bold",size=9),legend.position="none")
save_policy(fig3,"Figure3_policy_period_and_postpolicy_changes_v5",150,168)

# Figure 4: secondary trajectories. These panels describe heterogeneous burden.
char_traj <- fread(file.path(tab,"r_charls_lcga_trajectory_profiles.csv"))
class_traj <- fread(file.path(tab,"r_class_lcga_profiles.csv"))
p4a <- ggplot(char_traj,aes(year,mean_z,group=class_label,colour=class_label)) + geom_ribbon(aes(ymin=conf_low,ymax=conf_high,fill=class_label),alpha=.12,colour=NA) + geom_line(linewidth=.7) + geom_point(size=1.5) + labs(x="Survey year",y="Mean standardized cognitive score",title="CHARLS cognitive trajectories") + theme_policy() + theme(legend.position="bottom")
p4b <- ggplot(class_traj,aes(year,mean_score,group=class_label,colour=class_label)) + geom_ribbon(aes(ymin=conf_low,ymax=conf_high,fill=class_label),alpha=.12,colour=NA) + geom_line(linewidth=.7) + geom_point(size=1.5) + labs(x="Survey year",y="Mean 9-item depressive symptom score",title="CLASS depressive symptom trajectories") + theme_policy() + theme(legend.position="bottom")
save_policy(p4a + p4b + plot_annotation(tag_levels="a") & theme(plot.tag=element_text(face="bold",size=9)),"Figure4_secondary_trajectory_heterogeneity_v5",183,78)
cat("Policy-centred figures written to ",fig,"\n",sep="")
