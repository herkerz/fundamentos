df <- read.table("C:\\Users\\hgker\\Desktop\\Master DS\\fundamentos_r\\data\\marambio_2007.dat")

colnames(df)

head(df)
tail(df)

df$ukmo[is.na(df$ukmo)] <- mean(df$ukmo, na.rm = T)
df$ncep[is.na(df$ncep)] <- mean(df$ncep, na.rm = T)
apply(df,2,mean)

max(df$cmam)
min(df$cmam)