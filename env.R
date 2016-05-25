# todo: options could be user-specific
set_options <- function() {
  options(java.parameters = "-Xmx10G")

  options(mc.cores = 6)
  options(error = stop)
  # set to recover can be useful for long-running tasks
  # options(error = recover)

  options(show.signif.stars = FALSE)
}


# default environment definition
default_env <- function(mode = "interactive") list(

  data = switch(mode,
                interactive =
                  # dataframes and variables are defined globally
                  # this is more convenient for an interactive environment
                  globalenv(),

                production =
                  # create a new environment to keep dataframes and
                  # variables from filling up the default environment
                  # this is meant for a production environment
                  new.env()),

  jar_path     = file.path(getwd(), "sqljdbc4.jar"),
  analytics_dir= getwd(),
  output_dir   = file.path(getwd(), "output"),
  shared_obj   = "/Volumes/QB Shared/",
  odbc_driver  = "/usr/local/lib/libtdsodbc.so",
  db_username  = "usr",
  db_password  = "pwd",
  db_host      = "x.x.x.x",
  db_port      = 1433,
  db_dbname    = "DB",
  db_uri       = "jdbc:sqlserver://x.x.x.x;DatabaseName=DBname",
  mongo_db_url = "x.x.x.x",
  db_schema    = "project_template",
  db_force_load= FALSE,
  isWindows    = Sys.info()["sysname"] == "Windows"
)



# user-specific settings
# override defaults here
local_env <- function(person = Sys.info()["effective_user"]) {

  switch(person,
         ioan.stanculescu = list(
           basePath =  "/Users/ioan.stanculescu/Documents/misc/know/Beat_the_KNOW_search/",
           basePath2 =  "/Users/ioan.stanculescu/Documents/misc/know/Beat the Know seacrh - new data 04-08/",
           basePath3 =  "/Users/ioan.stanculescu/Documents/misc/know/Beat the Know seacrh - new data 04-14//",
           obbc_driver = "/usr/local/lib/libtdsodbc.so"
         ),
         maren.eckhoff = list(
           basePath = "/Users/maren.eckhoff/Documents/learning/Anaconda\ competition/data/Beat_the_KNOW_search/",
           basePath2 = "/Users/maren.eckhoff/Documents/learning/Anaconda\ competition/data/Beat the Know seacrh - new data 04-08/",
           basePath3 = "/Users/maren.eckhoff/Documents/learning/Anaconda\ competition/data/Beat the Know seacrh - new data 04-14/"
         )
  )
}


# construct an environment from the local and default variants
get_env <- function(mode   = "interactive",
                    person = Sys.info()["effective_user"]) {

  specific_env <- local_env(person)
  if (!(is.null(specific_env)))
    message("using non-default settings for ", person, ": ", paste(names(specific_env), collapse=", "))

  # overlay specific on default environment
  final_env <- default_env(mode)
  for (i in names(specific_env)) {
    final_env[i] <- specific_env[i]
  }
  return(final_env)
}


# load libraries and install if necessary
# supports bioclite, cran, github
# todo: define the list of package seperately from the loading logic
load_libs <- function() {

  installed_pkgs <- installed.packages()[,"Package"]

  # avoid questions when loading R.cache library
  dir.create(path="~/.Rcache", showWarnings=F)

  # bioc packages http://www.bioconductor.org/
  required_bioc_packages <- list("Rgraphviz")
  for (pkg in required_bioc_packages) {
    if (!(pkg %in% installed_pkgs))
      biocLite(pkg, suppressUpdates = TRUE)
  }

  # cran packages
  required_cran_packages <- list(
    "tm",
    "stringr",

    "RUnit",
    "git2r",
    "httr",
    "devtools",
    "parallel",
    "R.cache",
    "MASS",
    "RJDBC",
    "RODBC",
    "xlsx",

    "shiny",
    "shinythemes",
    "extrafont",
    "corrplot",
    "lattice",
    "ggplot2",
    "scales",
    "RColorBrewer",

    "stringr",
    "hash",
    "dplyr",
    "reshape2",
    "gdata",
    "data.table",
    "lubridate",

    "igraph",
    "network",
    "grid",

    "car",
    "moments",
    "DT",
    "tools",
    "corrplot",
    "jsonlite",
    "randomForest",
    "glmnet",
    "topicmodels",
    "PearsonDS",
    "zoo",
    "rmongodb",
    "plyr",
    "XML",

    "ROCR",
    "pROC",
    "doMC"
  )

  # load libraries and install if necessary
  if (!require("pacman")) install.packages("pacman")
  do.call("p_load", required_cran_packages)

  # Create .Rcache at current working dir
  setCacheRootPath(path = file.path(getwd(), ".Rcache"))

  # github packages
  required_github_packages <- list("mjkallen/rlogging")

  for (author_pkg in required_github_packages) {
    pkg <- unlist(strsplit(author_pkg, "/"))[2]
    if (!(pkg %in% installed_pkgs))
      p_install_gh(author_pkg)
  }

  if(!require("QBlib")) {
    message("Installing QBlib from server...")
    install.packages("QBlib", repos="https://qb:qu4ntum8@shinyr.quantumblack.com/Rpkgs/", type="source")
  }

  library("QBlib")
}


# initialisation
init <- function(mode = "interactive") {

  if(!file.exists(".git") && !any(grepl("\\.Rproj$", dir())))
    stop("expected a working directory with .git or .Rproj")

  assign("env", get_env(mode), globalenv())
  #source_all_functions()
  set_options()
  load_libs()

  # prefer ODBC
  # ensure_odbc_conn()

  # uncomment this only if you need both data access methods
  #ensure_jdbc_conn()

  #ensure_mongo_conn()

  # create output folder if necessary
  if (!file.exists(env$output_dir)){
    dir.create(env$output_dir, recursive = TRUE)
  }

  #init_complete := TRUE
}



# INIT
init(mode = "interactive")