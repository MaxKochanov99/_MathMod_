#������� ������ - ��� ������� 31 ������������ �������
#�������: ����������� ����������� ������� � ������ � 2000 �� 2003 ��� ���� ��� �������� ������� ����� �������� ���������� �� ��� ����, � ������������ �� ���������� �� 60 �� 150 ��
#��������� � �������� ������� ����������
setwd("D:/MathMod")

#��������� ����������� �������
library (tidyverse)
library (rnoaa)
library (lubridate)

#��������� ������ ������������
station_data = ghcnd_stations()
write.csv(station_data,"station_data21.csv")
station_data = read.csv("station_data21.csv")

#������������ ������ ������������
Belgorod=data.frame(id  = "Belgorod",latitude= 50.46, longitude = 37.27)

# �������� ������������ � ������������� ������� �� ��������� ��� �������� ����� �������,������� ����� ����������� ������ � �������� ��������� ������
Belgorod_around=meteo_nearby_stations(lat_lon_df = Belgorod, station_data = station_data, var = c("TAVG"),
                                      year_min = 2000, year_max = 2003)

#�������� �������������� ������������ ��������
Belgorod_id=Belgorod_around[["Belgorod"]][["id"]][1]

summary(Belgorod_id)
Belgorod_table=Belgorod_around[[1]]
summary(Belgorod_table)

#��������� ������� ��������� ������������
Belgorod_table = data.frame(Belgorod_around)
summary(Belgorod_table)

# ����������� ��� �������, �� ���������� �� 60 �� 150 ��
Belgorod_stations= Belgorod_table[Belgorod_table$Belgorod.distance > 60 & Belgorod_table$Belgorod.distance < 150,]
str(Belgorod_stations)
Belgorod_stations$Belgorod.id

# �������� �����, � ������� ����������� ����������� ������ � ������������ 

# ������������� ������, ���� ����������� ������ � ��������� ������������
all_i = data.frame()

# ������ ���� ����������� ��� ������ �� ���� ������������
all_Belgorod_meteodata = data.frame()

# ���� ��� ���� ������������
for(i in 1:12)
{
  Belgorod_id =  Belgorod_around[["Belgorod"]] [["id"]] [ i]
  data = meteo_tidy_ghcnd(stationid = Belgorod_id,
                          var = "TAVG",
                          date_min = "2000-01-01",
                          date_max = "2003-12-31")
  all_Belgorod_meteodata =  bind_rows(all_Belgorod_meteodata, data)
}

# ������ ���������� ������ � ����
write.csv(all_Belgorod_meteodata, "all_Belgorod_meteodata.csv")
all_Belgorod_meteodata

# ��������� ������ �� ����� all_Belgorod_meteodata.csv
all_Belgorod_meteodata = read.csv("all_Belgorod_meteodata.csv")

# ��������� �� ������
str(all_Belgorod_meteodata)

#  ������� ���, �����,����
all_Belgorod_meteodata = mutate(all_Belgorod_meteodata, year = year(date), month = month(date), day = day(date))
str(all_Belgorod_meteodata)

# ����������� ������ �� 2000 - 2003 ����
years_Belgorod_meteodata = filter(all_Belgorod_meteodata, year %in% c( 2000:2003))

#  �������� ���������
str(years_Belgorod_meteodata)
summary(years_Belgorod_meteodata)

#������� (�� ����� � �������������) ����� �������� ���������� �� �����
# ���������� ������� ����� ���������� � ���������� �����, ��� ������ ������� �� 10
years_Belgorod_meteodata[,"tavg"] = years_Belgorod_meteodata$tavg/10

# ��������� � ���� ��� NA � ��� 5<tavg>30
years_Belgorod_meteodata[is.na(years_Belgorod_meteodata$tavg),"tavg"] = 0
years_Belgorod_meteodata[years_Belgorod_meteodata$tavg<5, "tavg"] = 0
summary(years_Belgorod_meteodata)

#���������� �� �������������, ����� � �������
#����������� �� �������������, ����� � ������� ��� ������ ������� group_by
alldays = group_by(years_Belgorod_meteodata, id, year, month)

#����� ����������� �� ���� ������� � ������� sum 
sumT_alldays_Belgorod = summarize(alldays, tsum = sum(tavg))
summary(sumT_alldays_Belgorod)

#����������� ������ �� �������  
groups_Belgorod_months = group_by(sumT_alldays_Belgorod, month); groups_Belgorod_months

#������ �������� �� ������� ��� ���� ������������ � ���� ���
sumT_months=summarize(groups_Belgorod_months,St=mean(tsum))
sumT_months

# ���������� � ������� �� ������� ������

#���� �������� ��� ������� �����������
afi = c(0.00, 0.00, 0.00, 32.11, 26.31, 25.64, 23.20, 18.73, 16.30, 13.83, 0.00, 0.00)
bfi = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03, 8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
di = c(0.00, 0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)

#����������� ��� ���������� ������ - �������, ��� ��� ���� �������� ������
y = 1.0

#����������� ������������� ��� �������
Kf = 300

#������������ ������ ��������
Qj = 1600

#����������� "����� ������ �������� � �������� ���������"
Lj = 2.2

#����������� "����������� ��������� ��������"
Ej = 25 

#������� Fi �� �������
sumT_months = mutate(sumT_months, Fi = afi + bfi * y * St)

#������� Yi
sumT_months = mutate(sumT_months, Yi = ((Fi * di) * Kf) / (Qj * Lj * (100 - Ej)))

#������ ������, ��� ����� �� �������
Yield = sum(sumT_months$Yi)
Yield

# ��� ������� 31 ����������� ������� � ������ � 2000 �� 2003 ��� ��������� 19,17 �/�� 

