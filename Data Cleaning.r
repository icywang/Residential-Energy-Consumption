#correlation
##########Dummy variable creation for PCA analysis########################
####Energy Table Starts######
state.other <- ifelse(final.merge$LRGSTATE == 0,1,0)
state.ny <- ifelse(final.merge$LRGSTATE == 1,1,0)
state.ca <- ifelse(final.merge$LRGSTATE == 2,1,0)
state.tx <- ifelse(final.merge$LRGSTATE == 3,1,0)
state.fl <- ifelse(final.merge$LRGSTATE == 4,1,0)


home.ty.mob <- ifelse(final.merge$TYPEHUQ == 1,1,0)
home.ty.det <- ifelse(final.merge$TYPEHUQ == 2,1,0)
home.ty.att <- ifelse(final.merge$TYPEHUQ == 3,1,0)
home.ty.2to4 <- ifelse(final.merge$TYPEHUQ == 4,1,0)
home.ty.5plus <- ifelse(final.merge$TYPEHUQ == 5,1,0)

pca.energy.df <- cbind(state.other,state.ny,state.ca,state.tx,state.fl,
                       home.ty.mob,home.ty.det,home.ty.att,home.ty.2to4,home.ty.5plus)

####Energy Table Ends######


####House Table Starts######

wall.brick <- ifelse(final.merge$WALLTYPE == 1,1,0)
wall.wood <- ifelse(final.merge$WALLTYPE == 2,1,0)
wall.siding <- ifelse(final.merge$WALLTYPE == 3,1,0)
wall.stucco <- ifelse(final.merge$WALLTYPE == 4,1,0)
wall.shingle <- ifelse(final.merge$WALLTYPE == 5,1,0)
wall.stone <- ifelse(final.merge$WALLTYPE == 6,1,0)
wall.concrete <- ifelse(final.merge$WALLTYPE == 7,1,0)
wall.glass <- ifelse(final.merge$WALLTYPE == 8,1,0)
wall.other <- ifelse(final.merge$WALLTYPE == 9,1,0)
wall.indesc <- ifelse(final.merge$WALLTYPE == 10,1,0)


yearmade.b1940 <- ifelse(final.merge$YEARMADE == 1,1,0)
yearmade.1940plus <- ifelse(final.merge$YEARMADE == 2,1,0)
yearmade.1950plus <- ifelse(final.merge$YEARMADE == 3,1,0)
yearmade.1960plus <- ifelse(final.merge$YEARMADE == 4,1,0)
yearmade.1970plus <- ifelse(final.merge$YEARMADE == 5,1,0)
yearmade.1980plus <- ifelse(final.merge$YEARMADE == 6,1,0)
yearmade.1985plus <- ifelse(final.merge$YEARMADE == 7,1,0)
yearmade.1990plus <- ifelse(final.merge$YEARMADE == 8,1,0)
yearmade.1995plus <- ifelse(final.merge$YEARMADE == 9,1,0)
yearmade.2000plus <- ifelse(final.merge$YEARMADE == 10,1,0)
yearmade.2003 <- ifelse(final.merge$YEARMADE == 11,1,0)
yearmade.2004 <- ifelse(final.merge$YEARMADE == 12,1,0)
yearmade.2005 <- ifelse(final.merge$YEARMADE == 13,1,0)


neigh.city <- ifelse(final.merge$URBRUR == 1,1,0)
neigh.town <- ifelse(final.merge$URBRUR == 2,1,0)
neigh.suburbs <- ifelse(final.merge$URBRUR == 3,1,0)
neigh.rural <- ifelse(final.merge$URBRUR == 4,1,0)
pca.house.df <- cbind(wall.brick,wall.wood,wall.siding,wall.stucco,wall.shingle,wall.stone,
                      wall.concrete,wall.glass,wall.other,wall.indesc,
                      yearmade.b1940,yearmade.1940plus,yearmade.1950plus,yearmade.1960plus,yearmade.1970plus,
                      yearmade.1980plus,yearmade.1985plus,yearmade.1990plus,yearmade.1995plus,
                      yearmade.2000plus,yearmade.2003,yearmade.2004,yearmade.2005,
                      neigh.city,neigh.town,neigh.suburbs,neigh.rural)

####House Table Ends######

####Household Table Starts######

homebusiness.yes <- ifelse(final.merge$HBUSNESS == 1,1,0)
homebusiness.no <- ifelse(final.merge$HBUSNESS == 0,1,0)

otherwork.yes <- ifelse(final.merge$OTHWORK == 1,1,0)
otherwork.no <- ifelse(final.merge$OTHWORK == 0,1,0)

athome.yes <- ifelse(final.merge$ATHOME == 1,1,0)
athome.no <- ifelse(final.merge$ATHOME == 0,1,0)


gender.male <- ifelse(final.merge$HHSEX == 2,1,0)
gender.female <- ifelse(final.merge$HHSEX == 1,1,0)

employment.unemp <- ifelse(final.merge$EMPLOYHH == 0,1,0)
employment.empft <- ifelse(final.merge$EMPLOYHH == 1,1,0)
employment.emppt <- ifelse(final.merge$EMPLOYHH == 2,1,0)


spouse.yes <- ifelse(final.merge$SPOUSE == 1,1,0)
spouse.no <- ifelse(final.merge$SPOUSE == 0,1,0)

racehisp.yes <- ifelse(final.merge$SDESCENT == 1,1,0)
racehisp.no <- ifelse(final.merge$SDESCENT == 0,1,0)

workpay.yes <- ifelse(final.merge$WORKPAY == 1,1,0)
workpay.no <- ifelse(final.merge$WORKPAY == 0,1,0)

retirepay.yes <- ifelse(final.merge$RETIREPY == 1,1,0)
retirepay.no <- ifelse(final.merge$RETIREPY == 0,1,0)

cashben.yes <- ifelse(final.merge$CASHBEN == 1,1,0)
cashben.no <- ifelse(final.merge$CASHBEN == 0,1,0)

ncashben.yes <- ifelse(final.merge$NCASHBEN == 1,1,0)
ncashben.no <- ifelse(final.merge$NCASHBEN == 0,1,0)

moneypy.l2500 <- ifelse(final.merge$MONEYPY == 1,1,0)
moneypy.2500plus <- ifelse(final.merge$MONEYPY == 2,1,0)
moneypy.5000plus <- ifelse(final.merge$MONEYPY == 3,1,0)
moneypy.7500plus <- ifelse(final.merge$MONEYPY == 4,1,0)
moneypy.10000plus <- ifelse(final.merge$MONEYPY == 5,1,0)
moneypy.15000plus <- ifelse(final.merge$MONEYPY == 6,1,0)
moneypy.20000plus <- ifelse(final.merge$MONEYPY == 7,1,0)
moneypy.25000plus <- ifelse(final.merge$MONEYPY == 8,1,0)
moneypy.30000plus <- ifelse(final.merge$MONEYPY == 9,1,0)
moneypy.35000plus <- ifelse(final.merge$MONEYPY == 10,1,0)
moneypy.40000plus <- ifelse(final.merge$MONEYPY == 11,1,0)
moneypy.45000plus <- ifelse(final.merge$MONEYPY == 12,1,0)
moneypy.50000plus <- ifelse(final.merge$MONEYPY == 13,1,0)
moneypy.55000plus <- ifelse(final.merge$MONEYPY == 14,1,0)
moneypy.60000plus <- ifelse(final.merge$MONEYPY == 15,1,0)
moneypy.65000plus <- ifelse(final.merge$MONEYPY == 16,1,0)
moneypy.70000plus <- ifelse(final.merge$MONEYPY == 17,1,0)
moneypy.75000plus <- ifelse(final.merge$MONEYPY == 18,1,0)
moneypy.80000plus <- ifelse(final.merge$MONEYPY == 19,1,0)
moneypy.85000plus <- ifelse(final.merge$MONEYPY == 20,1,0)
moneypy.90000plus <- ifelse(final.merge$MONEYPY == 21,1,0)
moneypy.95000plus <- ifelse(final.merge$MONEYPY == 22,1,0)
moneypy.100000plus <- ifelse(final.merge$MONEYPY == 23,1,0)
moneypy.120000plus <- ifelse(final.merge$MONEYPY == 24,1,0)


pca.household.df <- cbind(homebusiness.yes,homebusiness.no,otherwork.yes,otherwork.no,
                          athome.yes,athome.no,gender.male,gender.female,
                          employment.unemp,employment.empft,employment.emppt,
                          spouse.yes,spouse.no,racehisp.yes,racehisp.no,workpay.yes,workpay.no,
                          retirepay.yes,retirepay.no,cashben.yes,cashben.no,ncashben.yes,ncashben.no,
                          moneypy.l2500,moneypy.2500plus,moneypy.5000plus,moneypy.7500plus,
                          moneypy.10000plus,moneypy.15000plus,moneypy.20000plus,moneypy.25000plus,
                          moneypy.30000plus,moneypy.35000plus,moneypy.40000plus,moneypy.45000plus,
                          moneypy.50000plus,moneypy.55000plus,moneypy.60000plus,moneypy.65000plus,
                          moneypy.70000plus,moneypy.75000plus,moneypy.80000plus,moneypy.85000plus,
                          moneypy.90000plus,moneypy.95000plus,moneypy.100000plus,moneypy.120000plus)

####Household Table Ends######

pca.df <- cbind.data.frame(pca.energy.df,pca.house.df,pca.household.df)

num.merge <- cbind.data.frame(final.merge,pca.df)
drops <- c("LRGSTATE", "TYPEHUQ", "WALLTYPE", "YEARMADE", "URBRUR",
           "HBUSNESS", "OTHWORK", "ATHOME", "HHSEX", "EMPLOYHH", 
           "SPOUSE", "SDESCENT", "WORKPAY", "RETIREPY", "CASHBEN",
           "NCASHBEN", "MONEYPY")
num.merge <- num.merge[ , !(names(num.merge) %in% drops)]
correlation <- cor(num.merge, use = "complete.obs", method="pearson")
corr.column <- c("AGEHHMEM2",	
                 "AGEHHMEM3",
                 "AGEHHMEMY",
                 "athome.no",
                 "athome.yes",
                 "ATTHSQFT",
                 "ATTUCSQFT",
                 "ATTUSQFT",
                 "BASHSQFT",
                 "BASUCSQFT",
                 "BASUSQFT",
                 "cashben.no",
                 "cashben.yes",
                 "CD65",
                 "employment.empft",
                 "employment.emppt",
                 "GARCSQFT",
                 "GARUCSQFT",
                 "GARUSQFT",
                 "gender.female",
                 "gender.male",
                 "HD65",
                 "HHAGE",
                 "home.ty.5plus",
                 "home.ty.att",
                 "home.ty.det",
                 "home.ty.mob",
                 "homebusiness.no",
                 "NHSLDMEM",
                 "otherwork.no",
                 "otherwork.yes",
                 "racehisp.no",
                 "racehisp.yes",
                 "retirepay.no",
                 "retirepay.yes",
                 "RHMHSQFT",
                 "RHMUCSQFT",
                 "RHMUSQFT",
                 "spouse.no",
                 "spouse.yes",
                 "state.ca",
                 "state.fl",
                 "state.ny",
                 "state.other",
                 "state.tx",
                 "TOTATTCSQFT",
                 "TOTBASESQFT",
                 "TOTCSQFT",
                 "TOTGARGSQFT",
                 "TOTHSQFT",
                 "TOTRHMSQFT",
                 "TOTSQFT",
                 "TOTUCSQFT",
                 "TOTUSQFT",
                 "wall.brick",
                 "wall.concrete",
                 "wall.glass",
                 "wall.shingle",
                 "wall.stucco",
                 "workpay.no",
                 "workpay.yes")
pc.merge <- num.merge[, names(num.merge) %in% corr.column]
pc.result <- prcomp(pc.merge, retx = TRUE, center = TRUE, scale = FALSE)
screeplot(pc.result, type = "lines")
View(pc.result$x[1:6])
