@echo off
cd /d "C:\Users\rewhe\Desktop\pravosudie\vcode"

start "Центральный"       cmd /k "python main.py Tsentralny_FO"
start "Северо-Западный"   cmd /k "python main.py Severo_Zapadny_FO"
start "Южный"            cmd /k "python main.py Yuzhny_FO"
start "Приволжский"      cmd /k "python main.py Privolzhsky_FO"
start "Уральский"        cmd /k "python main.py Uralsky_FO"
start "Сибирский"         cmd /k "python main.py Sibirsky_FO"
start "Дальневосточный"   cmd /k "python main.py Dalnevostochny_FO"
start "Северо-Кавказский" cmd /k "python main.py Severo_Kavkazsky_FO"
start "Крымский"          cmd /k "python main.py Krymsky_FO"