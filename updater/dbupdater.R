##
# Updater
tmp <- readxl::read_excel('./updater/source.xlsx', sheet = 'AssetSector')
WriteDataToSS(db_obj, tmp, "MyBroKe_AssetSector")

tmp <- readxl::read_excel('./updater/source.xlsx', sheet = 'AssetCountry')
WriteDataToSS(db_obj, tmp, "MyBroKe_AssetCountry")

tmp <- readxl::read_excel('./updater/source.xlsx', sheet = 'FundTransferHistory')
WriteDataToSS(db_obj, tmp, "MyBroKe_FundTransferHistory")

tmp <- readxl::read_excel('./updater/source.xlsx', sheet = 'STWatchlist')
WriteDataToSS(db_obj, tmp, "MyBroKe_STWatchlist")