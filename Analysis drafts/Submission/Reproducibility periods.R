gift_new = readRDS(paste0(getwd(), "/Results/big_grid_gift.RData"))
el_new = readRDS(paste0(getwd(), "/Results/big_grid_el.RData"))

gift_old = readRDS(paste0(getwd(), "/Results - Kopie/big_grid_gift.RData"))
el_old = readRDS(paste0(getwd(), "/Results - Kopie/big_grid_el.RData"))

gift_new == gift_old
el_new == el_old

if (!exists("repr")){
  repr = list()
}

repr$gift_periods = gift_new == gift_old
repr$el_periods = el_new == el_old
