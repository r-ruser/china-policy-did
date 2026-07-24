# ============================================================
# 02_build_policy_crosswalk.R
# 构建政策主表和CFPS地理交叉对照
# ============================================================

source("00_config.R")
log_msg("Starting 02_build_policy_crosswalk.R")

library(data.table)
library(haven)
library(readxl)

# ----------------------------------------------------------
# 1. 读取政策主表
# ----------------------------------------------------------
log_msg("Reading policy unit table...")
policy <- fread(file.path(PATH_POLICY, "policy_unit_table.csv"))
log_msg(paste("Policy units:", nrow(policy)))
log_msg(paste("Batch 1:", sum(policy$batch == 1)))
log_msg(paste("Batch 2:", sum(policy$batch == 2)))

# 验证
assert(nrow(policy) == 90, "Policy table must have 90 units")
assert(all(policy$post_start_year == 2017), "All post_start_year must be 2017")

# ----------------------------------------------------------
# 2. 读取CFPS交叉对照文件
# ----------------------------------------------------------
log_msg("Reading CFPS crosswalk...")
cfps_xwalk <- as.data.table(read_dta(file.path(
  PATH_CFPS, "1⭐cfps顺序码匹配", "顺序码匹配.dta"
)))
log_msg(paste("CFPS crosswalk counties:", nrow(cfps_xwalk)))
log_msg(paste("Unique provinces in crosswalk:", cfps_xwalk[, uniqueN(provname)]))

# 检查code列
assert(all(!is.na(cfps_xwalk$code)), "Crosswalk code must not have NAs")
cfps_xwalk[, code_str := sprintf("%06d", code)]
cfps_xwalk[, prov_code := as.integer(substr(code_str, 1, 2))]
cfps_xwalk[, city_code := as.integer(paste0(substr(code_str, 1, 4), "00"))]

# ----------------------------------------------------------
# 3. 匹配试点地区
# ----------------------------------------------------------
log_msg("Matching pilot cities to CFPS crosswalk...")

# 构建处理组标识
policy[, county_code_str := sprintf("%06d", county_code)]
policy[, prov_code := as.integer(substr(county_code_str, 1, 2))]
policy[, city_code := as.integer(paste0(substr(county_code_str, 1, 4), "00"))]

# 对于区县级试点（直辖市辖区、县级市），直接匹配
# 对于地级市试点，匹配该市下辖的所有CFPS县区
cfps_xwalk[, treat := 0L]
cfps_xwalk[, treat_policy_id := NA_character_]
cfps_xwalk[, treat_batch := NA_integer_]

for (i in 1:nrow(policy)) {
  p <- policy[i]
  pcode <- p$county_code
  plevel <- p$policy_level

  if (plevel %in% c("municipal_district", "county", "county_level_city")) {
    # 直接匹配
    idx <- cfps_xwalk$code == pcode
    if (any(idx)) {
      cfps_xwalk[idx, treat := 1L]
      cfps_xwalk[idx, treat_policy_id := p$policy_unit_id]
      cfps_xwalk[idx, treat_batch := p$batch]
    }
  } else if (plevel %in% c("prefecture_city", "sub_provincial_city", "autonomous_prefecture")) {
    # 匹配该市下辖的所有县区（前4位相同）
    prefix <- substr(sprintf("%06d", pcode), 1, 4)
    idx <- substr(cfps_xwalk$code_str, 1, 4) == prefix
    if (any(idx)) {
      cfps_xwalk[idx, treat := 1L]
      cfps_xwalk[idx, treat_policy_id := p$policy_unit_id]
      cfps_xwalk[idx, treat_batch := p$batch]
    }
  }
}

n_treated <- cfps_xwalk[treat == 1, uniqueN(code)]
n_treated_counties <- cfps_xwalk[treat == 1, .N]
log_msg(paste("CFPS counties in pilot cities:", n_treated_counties))
log_msg(paste("Unique pilot city codes covered:", n_treated))

# ----------------------------------------------------------
# 4. 统计每个试点地区的CFPS样本
# ----------------------------------------------------------
log_msg("Pilot city coverage summary:")
treat_summary <- cfps_xwalk[treat == 1, .(
  n_counties = .N,
  counties = paste(code_str, collapse = ", ")
), by = .(treat_policy_id, treat_batch)]
print(treat_summary)

# ----------------------------------------------------------
# 5. 保存交叉对照结果
# ----------------------------------------------------------
saveRDS(cfps_xwalk, file.path(PATH_CLEAN, "cfps_crosswalk_treat.rds"))
fwrite(cfps_xwalk, file.path(PATH_CLEAN, "cfps_crosswalk_treat.csv"))

log_msg("Crosswalk saved.")

# ----------------------------------------------------------
# 6. 输出匹配失败记录
# ----------------------------------------------------------
# 检查哪些试点地区没有CFPS覆盖
covered_policies <- unique(cfps_xwalk[treat == 1]$treat_policy_id)
not_covered <- policy[!policy_unit_id %in% covered_policies]
if (nrow(not_covered) > 0) {
  log_msg(paste("Pilot cities NOT covered by CFPS:", nrow(not_covered)))
  fwrite(not_covered, file.path(PATH_LOGS, "cfps_pilot_not_covered.csv"))
}

log_msg("02_build_policy_crosswalk.R completed.")
