##
# Updater
tmp <- readxl::read_excel('./updater/source.xlsx', sheet = 'AssetSector')
WriteDataToSS(db_obj, tmp, "MyBroKe_AssetSector")

tmp <- readxl::read_excel('./updater/source.xlsx', sheet = 'AssetCountry')
WriteDataToSS(db_obj, tmp, "MyBroKe_AssetCountry")

tmp <- readxl::read_excel('./updater/source.xlsx', sheet = 'STWatchlist')
WriteDataToSS(db_obj, tmp, "MyBroKe_STWatchlist")

tmp <- readxl::read_excel('./updater/source.xlsx', sheet = 'AssetChar')
WriteDataToSS(db_obj, tmp, "MyBroKe_AssetChar")

tmp <- readxl::read_excel('./updater/source.xlsx', sheet = 'FundTransferHistory')
WriteDataToSS(db_obj, tmp, "MyBroKe_FundTransferHistory")

tmp <- readxl::read_excel('./updater/source.xlsx', sheet = 'FundTransferHistoryOA')
WriteDataToSS(db_obj, tmp, "MyBroKe_FundTransferHistoryOA", apd = TRUE)

tmp <- readxl::read_excel('./updater/source.xlsx', sheet = 'MyBrokerAccounts')
WriteDataToSS(db_obj, tmp, "MyBroKe_Accounts")
