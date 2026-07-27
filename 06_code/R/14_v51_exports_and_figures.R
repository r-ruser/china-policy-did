#!/usr/bin/env Rscript
# V5.1 auditable exports: all values are read from R-produced tables/models.
suppressPackageStartupMessages({library(data.table);library(dplyr);library(ggplot2);library(patchwork);library(svglite);library(ragg)})
root<-normalizePath(getwd(),winslash="/",mustWork=TRUE); tab<-file.path(root,"07_results","tables"); diag<-file.path(root,"07_results","diagnostics"); fig<-file.path(root,"07_results","figures"); dir.create(fig,showWarnings=FALSE,recursive=TRUE)
write_csv<-function(x,n) fwrite(as.data.table(x),file.path(tab,n))
main<-fread(file.path(tab,"r_corrected_main_results.csv")); placebo<-fread(file.path(tab,"r_v4_charls_placebo_periods.csv")); trend<-fread(file.path(tab,"r_v4_charls_differential_trend_adjusted.csv"))
charls_crosswalk<-bind_rows(
 main|>filter(analysis=="CHARLS target-group period change",outcome%in%c("Incident ADL","Incident ADL (IPCW)"))|>transmute(estimand=ifelse(outcome=="Incident ADL","2018 incident ADL risk difference by baseline age group","2018 incident ADL risk difference by baseline age group: IPCW 1/99"),sample="2015 age >=65 and no baseline ADL limitation",outcome,model="Individual and year fixed effects",estimate_pp=100*estimate,ci_low_pp=100*conf_low,ci_high_pp=100*conf_high,p_value,n_obs,n_id),
 placebo|>transmute(estimand=paste0("Repeated ADL status: ",specification),sample="Repeated observations with non-missing ADL status",outcome="Repeated ADL status",model="Individual and year fixed effects",estimate_pp=estimate,ci_low_pp=conf_low,ci_high_pp=conf_high,p_value,n_obs,n_id=NA_integer_),
 trend|>transmute(estimand="Trend-adjusted additional 2018 deviation",sample="Repeated observations, 2011-2018",outcome="Repeated ADL status",model="Age-group linear differential trend plus 2018 deviation",estimate_pp=estimate,ci_low_pp=conf_low,ci_high_pp=conf_high,p_value,n_obs,n_id=NA_integer_))
write_csv(charls_crosswalk,"CHARLS_estimand_crosswalk.csv")
cfps_robust<-bind_rows(
 main|>filter(grepl("CFPS pilot",analysis))|>transmute(component="Conventional city-clustered estimate",estimand,outcome,estimate=100*estimate,ci_low=100*conf_low,ci_high=100*conf_high,p_value,details=notes),
 fread(file.path(tab,"r_v4_cfps_wild_cluster_bootstrap.csv"))|>transmute(component="999-replicate wild cluster bootstrap",estimand,outcome="Poor self-rated health",estimate=100*estimate,ci_low=NA_real_,ci_high=NA_real_,p_value=wild_cluster_p,details=paste0(clusters," city clusters")),
 fread(file.path(tab,"r_v4_cfps_ps_weighted.csv"))|>transmute(component="ATT weighted DID",estimand=analysis,outcome="Poor self-rated health",estimate=100*estimate,ci_low=100*conf_low,ci_high=100*conf_high,p_value,details=paste0("ESS=",round(effective_sample_size))),
 fread(file.path(tab,"r_v4_cfps_equivalence_mde.csv"))|>transmute(component="Equivalence and MDE",estimand="Primary DID",outcome="Poor self-rated health",estimate=100*estimate,ci_low=100*ci90_low,ci_high=100*ci90_high,p_value=NA_real_,details=paste0("±5pp equivalent=",equivalent_within_5pp,"; MDE80=",round(100*minimum_detectable_effect_80pct,2),"pp")))
write_csv(cfps_robust,"CFPS_robustness_results.csv")

# CLASS contrasts use the saved FE covariance matrices, so 75+ group CIs are not approximated.
mods<-readRDS(file.path(root,"07_results","models","r_class_primary_models.rds"))$longitudinal_models$age_heterogeneity
labs<-c(poor_srh="Poor self-rated health",adl_help="Need help with activities of daily living",depression9="Common 9-item depressive symptom score")
class_out<-bind_rows(lapply(names(mods),function(nm){m<-mods[[nm]]; b<-coef(m); V<-vcov(m); do.call(rbind,lapply(c("2020","2023"),function(yr){p<-paste0("post",yr); it<-paste0("post",yr,":age75_2018"); mk<-function(label,w){e<-sum(w*b[names(w)]); s<-sqrt(as.numeric(t(w)%*%V[names(w),names(w),drop=FALSE]%*%w)); data.frame(outcome=labs[[nm]],wave=as.integer(yr),contrast=label,estimate=e,std_error=s,conf_low=e-1.96*s,conf_high=e+1.96*s,p_value=2*pnorm(-abs(e/s)))}; bind_rows(mk("65-74 change",setNames(1,p)),mk("75+ change",setNames(c(1,1),c(p,it))),mk("75+ minus 65-74 change",setNames(1,it))) }))}))|>group_by(outcome)|>mutate(q_value=p.adjust(p_value,"BH"))|>ungroup()
write_csv(class_out,"CLASS_age_interaction_results.csv")
attr<-fread(file.path(diag,"r_v4_attrition_rates_by_age_group.csv")); wdiag<-fread(file.path(diag,"r_v4_ipcw_weight_diagnostics.csv")); ipcw<-fread(file.path(tab,"r_v4_ipcw_trim_sensitivity.csv")); mi<-fread(file.path(tab,"r_v4_multiple_imputation_adl.csv")); ext<-fread(file.path(tab,"r_v4_extreme_scenario_sensitivity.csv"))
missing<-bind_rows(attr|>mutate(section="Attrition rate")|>rename(estimate=attrition_rate),wdiag|>mutate(section="IPCW diagnostics")|>rename(estimate=effective_sample_size),ipcw|>mutate(section="IPCW effect")|>rename(estimate=estimate),mi|>mutate(section="Multiple imputation")|>rename(estimate=estimate),ext|>mutate(section="Extreme scenario")|>rename(estimate=risk_difference))
write_csv(missing,"attrition_and_missingness_results.csv")
traj<-bind_rows(fread(file.path(diag,"r_charls_lcga_model_selection.csv"))|>mutate(cohort="CHARLS"),fread(file.path(diag,"r_class_lcga_model_selection.csv"))|>mutate(cohort="CLASS")); quality<-bind_rows(fread(file.path(diag,"r_charls_lcga_classification_quality.csv"))|>mutate(cohort="CHARLS"),fread(file.path(diag,"r_class_lcga_quality.csv"))|>mutate(cohort="CLASS")); write_csv(bind_rows(traj|>mutate(record_type="model_selection"),quality|>mutate(record_type="classification_quality")),"trajectory_model_quality.csv")

themev<-theme_classic(base_family="Arial",base_size=7)+theme(plot.title=element_text(face="bold"),plot.tag=element_text(face="bold",size=9),legend.position="bottom",axis.line=element_line(linewidth=.35))
pA<-ggplot(charls_crosswalk|>filter(grepl("Repeated|Trend-adjusted",estimand)),aes(estimate_pp,reorder(estimand,estimate_pp)))+geom_vline(xintercept=0,colour="grey50")+geom_errorbarh(aes(xmin=ci_low_pp,xmax=ci_high_pp),height=.14)+geom_point(colour="#2D5686",size=2)+labs(title="CHARLS repeated ADL status",x="Age-group differential change (percentage points)",y=NULL)+themev
overall<-fread(file.path(tab,"r_class_longitudinal_changes.csv"))|>filter(year%in%c(2020,2023),outcome!="Common 9-item depressive symptom score")|>mutate(estimate=100*estimate,conf_low=100*conf_low,conf_high=100*conf_high)
pB<-ggplot(overall,aes(estimate,reorder(outcome,estimate),shape=factor(year),colour=factor(year)))+geom_vline(xintercept=0,colour="grey50")+geom_errorbarh(aes(xmin=conf_low,xmax=conf_high),height=.12,position=position_dodge(.45))+geom_point(position=position_dodge(.45),size=2)+labs(title="CLASS binary outcomes",x="Within-person change (percentage points)",y=NULL,shape=NULL,colour=NULL)+themev
pC<-ggplot(class_out|>filter(contrast=="75+ minus 65-74 change"),aes(estimate,reorder(outcome,estimate),shape=factor(wave),colour=factor(wave)))+geom_vline(xintercept=0,colour="grey50")+geom_errorbarh(aes(xmin=conf_low,xmax=conf_high),height=.12,position=position_dodge(.45))+geom_point(position=position_dodge(.45),size=2)+labs(title="CLASS age 75+ interaction",x="Additional change for age 75+",y=NULL,shape=NULL,colour=NULL)+themev
g3<-pA/pB/pC+plot_annotation(tag_levels="a")
for(extn in c("png","pdf","svg","tiff")){
  f<-file.path(fig,paste0("Figure3_policy_distributional_evidence_V5.1.",extn))
  if(extn=="png") { ggsave(f,g3,width=150/25.4,height=175/25.4,dpi=300) }
  else if(extn=="tiff") { ragg::agg_tiff(f,width=150/25.4,height=175/25.4,units="in",res=600); print(g3); dev.off() }
  else if(extn=="svg") { svglite(f,width=150/25.4,height=175/25.4); print(g3); dev.off() }
  else { cairo_pdf(f,width=150/25.4,height=175/25.4); print(g3); dev.off() }
}
cat("V5.1 exports complete\n")
