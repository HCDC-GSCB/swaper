@echo off
chcp 65001 >nul

echo [1/4] Di chuyen den thu muc du an...
:: Lệnh này tự động lấy đường dẫn của thư mục chứa file .bat và di chuyển vào đó
cd /d "%~dp0"

echo [2/4] Dang chay file R cap nhat du lieu...
Rscript dashboard/backend.R

echo [3/4] Dang render website Quarto...
quarto render

echo [4/4] Dang day len GitHub...
git add .
git commit -m "Auto update Epidemic Channel"
git push origin main

echo ✅ HOAN TAT QUY TRINH DEPLOY!