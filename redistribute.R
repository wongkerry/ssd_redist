library(raster)
library(sf)
library(data.table)
library(exactextractr)

# setwd("C:/Users/wkerr/Dropbox/who2023/south_sudan_pop_2023")

pop2020 <- raster("https://www.dropbox.com/scl/fi/if7873ad0thvbn352lvic/ssd_ppp_2020_UNadj.tif?rlkey=d73xi9fo29go2mc9s4gnffmuk&dl=1")

pop2023 <- as.data.table(readxl::read_excel("ssd_2023_population_estimates_data.xlsx"))
names(pop2023)[6] <- "pop2024"
pop2023 <- pop2023[pop_2024<10000000] #remove total row
pop2023 <- pop2023[Admin2_Pcode!="SS0001"]
pop2023 <- pop2023[, .(Admin2_Pcode, pop2024)]
names(pop2023)[1] <- "ADM2_PCODE"

adm2 <- st_read(dsn = "SSD_AdminBoundaries_candidate.gdb", layer = "ssd_admbnda_adm2_imwg_nbs_20230829")
adm2 <- adm2 %>% subset(ADM2_PCODE!="SS0001")
adm2 <- st_transform(adm2, st_crs(pop2020))
adm2$pop2020 <- exact_extract(pop2020, adm2, "sum")
adm2 <- merge(adm2, pop2023, all = TRUE)

pts <- rasterToPoints(pop2020)

spdf <- SpatialPointsDataFrame(coords = pts[, 1:2], data = data.frame(pts),
                               proj4string = crs(pop2020))

spdf$ADM2_PCODE <- over(spdf[], as_Spatial(adm2))$ADM2_PCODE 

spdt <- as.data.table(spdf)
spdt <- merge(spdt, adm2 %>% 
                    data.frame() %>% 
                    dplyr::select(ADM2_PCODE, ADM2_EN, pop2020, pop_2024), all.x = TRUE)
spdt[, perc := ssd_ppp_2020_UNadj/pop2020]
spdt[, ssd_ppp_2023 := pop2024*perc]
              
newras <- rasterize(spdt[, .(x, y)], pop2020, field=spdt$ssd_ppp_2023, fun=mean)

writeRaster(newras, "new_pop_ssd.tif", overwrite=TRUE)
