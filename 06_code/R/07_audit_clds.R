# ============================================================
# 07_audit_clds.R
# CLDS数据库审计 - 地理编码和结局变量
# ============================================================

source("00_config.R")
log_msg("Starting 07_audit_clds.R")

library(data.table)
library(haven)

# ----------------------------------------------------------
# 1. 读取CLDS各波次数据
# ----------------------------------------------------------

# 2012波次（含标准地理编码）
log_msg("Reading CLDS 2012 individual...")
clds12 <- as.data.table(read_dta(file.path(
  PATH_CLDS, "CLDS2012年数据", "individual2012转码后.dta"
)))
log_msg(paste("CLDS 2012:", nrow(clds12), "individuals"))

# 检查地理编码
assert("PROVINCE" %in% names(clds12), "CLDS 2012 must have PROVINCE")
assert("CITY" %in% names(clds12), "CLDS 2012 must have CITY")
assert("COUNTY" %in% names(clds12), "CLDS 2012 must have COUNTY")

clds12[, province_code := as.integer(PROVINCE)]
clds12[, city_code := as.integer(CITY)]
clds12[, county_code := as.integer(COUNTY)]

log_msg(paste("Provinces:", clds12[, uniqueN(province_code)]))
log_msg(paste("Cities:", clds12[, uniqueN(city_code, na.rm = TRUE)]))
log_msg(paste("Counties:", clds12[, uniqueN(county_code, na.rm = TRUE)]))

# ----------------------------------------------------------
# 2. 匹配试点城市
# ----------------------------------------------------------
log_msg("Matching CLDS cities to pilot cities...")

policy <- fread(file.path(PATH_POLICY, "policy_unit_table.csv"))

# CLDS城市代码是6位标准代码
clds_cities <- unique(clds12$city_code[!is.na(clds12$city_code)])

# 对于地级市试点，直接匹配城市代码
# 对于直辖市区县，需要匹配区县代码
pilot_city_codes <- policy$county_code  # 使用county_code作为匹配键

matched <- intersect(clds_cities, pilot_city_codes)
log_msg(paste("CLDS cities matching pilot cities:", length(matched)))

# 显示匹配结果
for (code in matched) {
  pname <- policy[county_code == code, policy_unit_name_standard]
  log_msg(paste("  Matched:", code, "=", pname))
}

# ----------------------------------------------------------
# 3. 检查各波次可用性
# ----------------------------------------------------------

# 2016波次
log_msg("Reading CLDS 2016 individual...")
clds16 <- as.data.table(read_dta(file.path(
  PATH_CLDS, "CLDS2016年数据", "individual2016转码后.dta"
)))
log_msg(paste("CLDS 2016:", nrow(clds16), "individuals"))

# 2018波次
log_msg("Reading CLDS 2018 individual...")
clds18 <- as.data.table(read_dta(file.path(
  PATH_CLDS, "CLDS2018年数据", "individual2018转码后.dta"
)))
log_msg(paste("CLDS 2018:", nrow(clds18), "individuals"))

# ----------------------------------------------------------
# 4. 检查变量名一致性
# ----------------------------------------------------------
log_msg("Checking variable consistency across waves...")

# 检查关键变量在各波次是否存在
key_vars <- c("gender", "birthyear", "birthmonth", "marriage")
for (v in key_vars) {
  in12 <- v %in% names(clds12)
  in16 <- v %in% names(clds16)
  in18 <- v %in% names(clds18)
  log_msg(paste(v, "- 2012:", in12, "2016:", in16, "2018:", in18))
}

# ----------------------------------------------------------
# 5. 保存审计结果
# ----------------------------------------------------------
audit_result <- data.table(
  wave = c(2012, 2016, 2018),
  n_individuals = c(nrow(clds12), nrow(clds16), nrow(clds18)),
  n_provinces = c(
    clds12[, uniqueN(province_code)],
    clds16[, uniqueN(as.integer(PROVINCE), na.rm = TRUE)],
    clds18[, uniqueN(as.integer(PROVINCE), na.rm = TRUE)]
  )
)
saveRDS(audit_result, file.path(PATH_AUDIT, "clds_wave_summary.rds"))

log_msg("07_audit_clds.R completed.")
