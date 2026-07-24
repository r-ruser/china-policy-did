# ============================================================
# 00_config.R
# 医养结合政策准实验研究 - 全局配置
# ============================================================

# --- 项目根目录 ---
PROJECT_ROOT <- "E:/公共数据库/中国数据库/医养结合政策DID_CHFS_CFPS"

# --- 数据路径（只读） ---
PATH_CFPS <- "E:/公共数据库/中国数据库/CFPS"
PATH_CLDS <- "E:/公共数据库/中国数据库/CLDS"
PATH_CHFS <- "E:/公共数据库/中国数据库/CHFS"
PATH_GDP  <- "E:/公共数据库/中国数据库/31省名义、实际GDP及GDP平减指数数据（2000-2024年）"

# --- 输出路径 ---
PATH_PROTOCOL   <- file.path(PROJECT_ROOT, "00_protocol")
PATH_AUDIT      <- file.path(PROJECT_ROOT, "01_database_audit")
PATH_POLICY     <- file.path(PROJECT_ROOT, "02_policy_mapping")
PATH_DICT       <- file.path(PROJECT_ROOT, "03_data_dictionary")
PATH_CLEAN      <- file.path(PROJECT_ROOT, "04_clean_data")
PATH_ANALYSIS   <- file.path(PROJECT_ROOT, "05_analysis_data")
PATH_CODE_R     <- file.path(PROJECT_ROOT, "06_code/R")
PATH_TABLES     <- file.path(PROJECT_ROOT, "07_results/tables")
PATH_FIGURES    <- file.path(PROJECT_ROOT, "07_results/figures")
PATH_MODELS     <- file.path(PROJECT_ROOT, "07_results/models")
PATH_DIAG       <- file.path(PROJECT_ROOT, "07_results/diagnostics")
PATH_MANUSCRIPT <- file.path(PROJECT_ROOT, "08_manuscript")
PATH_SUPPLEMENT <- file.path(PROJECT_ROOT, "09_supplement")
PATH_LOGS       <- file.path(PROJECT_ROOT, "10_logs")

# --- 随机种子 ---
set.seed(20260723)

# --- 政策时间定义 ---
POLICY_ANNOUNCEMENT_1 <- as.Date("2016-06-16")
POLICY_ANNOUNCEMENT_2 <- as.Date("2016-09-14")
POST_START_YEAR <- 2017
TRANSITION_YEAR <- 2016

# --- CFPS 波次定义 ---
CFPS_WAVES <- c(2010, 2012, 2014, 2016, 2018, 2020)
CFPS_PRE_WAVES <- c(2010, 2012, 2014)
CFPS_POST_WAVES <- c(2018, 2020)
CFPS_TRANSITION <- 2016  # 主分析删除

# --- CLDS 波次定义 ---
CLDS_WAVES <- c(2011, 2012, 2014, 2016, 2018)
CLDS_PRE_WAVES <- c(2011, 2012, 2014)
CLDS_POST_WAVES <- c(2018)
CLDS_TRANSITION <- 2016  # 主分析删除

# --- CHFS 波次定义 ---
CHFS_WAVES <- c(2011, 2013, 2015, 2017, 2019, 2021)
CHFS_PRE_WAVES <- c(2011, 2013, 2015)
CHFS_POST_WAVES <- c(2019, 2021)
CHFS_EARLY_POST <- 2017  # 早期政策实施期

# --- 老年年龄阈值 ---
AGE_THRESHOLD <- 60

# --- 灾难性卫生支出阈值 ---
CHE_THRESHOLDS <- c(0.10, 0.25, 0.40)

# --- 货币平减基准年 ---
BASE_YEAR <- 2015

# --- 日志函数 ---
log_msg <- function(msg) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(sprintf("[%s] %s\n", timestamp, msg))
}

# --- 断言函数 ---
assert <- function(condition, msg) {
  if (!condition) {
    stop(sprintf("ASSERTION FAILED: %s", msg))
  }
}

# --- 加载必要包 ---
required_packages <- c(
  "data.table", "haven", "readxl", "arrow",
  "dplyr", "tidyr", "stringr", "labelled",
  "fixest", "did", "WeightIt", "cobalt",
  "MatchIt", "ggplot2", "modelsummary",
  "sf", "mice"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    log_msg(paste("Installing package:", pkg))
    install.packages(pkg, repos = "https://cran.r-project.org")
  }
}

log_msg("Config loaded successfully.")
log_msg(paste("Project root:", PROJECT_ROOT))
