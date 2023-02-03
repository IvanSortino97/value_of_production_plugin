# Steps -------------------------------------------------------------------

# 1: Connect to Pulse via home.fao.org ------------------------------------

# 2: Access RStudio -------------------------------------------------------

####################### a) RStudio Workbench #######################
# https://rstudios.fao.org
# Work environment

####################### b) RStudio Connect ####################### 
# https://rstudioc.fao.org/
# Access Shiny Apps, Presentations ...

####################### c) Package Manager ####################### 
# https://rstudiopm.fao.org/


# 3: Create token ---------------------------------------------------------

##################### a) Associate a plugin to a dataset #####################

##################### b) Create a session ####################################

##################### c) Run plugin ##########################################

##################### d) Generate token ######################################


# 4: Connect to SWS through R ---------------------------------------------

##################### a) Load packages #######################################

##################### b) Setup the environment (yml file) ####################

# certificates
# server
# token

# 5: Session info ---------------------------------------------------------

##################### a) Domain #############################################

swsContext.datasets[[1]]@domain

##################### b) Dataset ############################################

swsContext.datasets[[1]]@dataset

##################### c) Param chosen #######################################

swsContext.computationParams

##################### d) Domain names #######################################

GetDomainNames()

##################### e) Dataset in specific domain #########################

GetDatasetNames("disseminated")

##################### f) Dim of specific dataset ############################

GetDatasetConfig("disseminated", "fbs_balanced_diss")

##################### g) Code lists #########################################

GetCodeList("disseminated", "fbs_balanced_diss", "geographicAreaM49")


# 6: Read in datasets -----------------------------------------------------

##################### a) Define key ########################################

key_ <- DatasetKey(
  domain = "disseminated",
  dataset = "fbs_balanced_diss",
  dimensions = list(
    Dimension(name = "geographicAreaM49",
              keys = "784"),
    Dimension(name = "measuredElementSuaFbs",
              keys = c("511", "664")),
    Dimension(name = "measuredItemFbsSua",
              keys = GetCodeList("disseminated", "fbs_balanced_diss", "measuredItemFbsSua")[, code]),
    Dimension(name = "timePointYears",
              keys = as.character(2010:2019)
    )
  ))

##################### b) GetData function ###################################

data <- GetData(key_)



##################### c) Get names ##########################################

nameData("disseminated", "fbs_balanced_diss", data)


# 7: Read in datatables ---------------------------------------------------

ReadDatatable("mapping_datasets_qi")
