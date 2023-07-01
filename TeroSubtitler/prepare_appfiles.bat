@echo off
xcopy /E /Y ".\bin\Languages" ".\bin\tero.app\Contents\Resources\Languages"
xcopy /E /Y ".\bin\Dictionaries" ".\bin\tero.app\Contents\Resources\Dictionaries"
xcopy /E /Y ".\bin\OCR" ".\bin\tero.app\Contents\Resources\OCR"
xcopy /E /Y ".\bin\CustomFormat" ".\bin\tero.app\Contents\Resources\CustomFormat"
xcopy /Y ".\bin\libhunspellx64.dylib" ".\bin\tero.app\Contents\Frameworks\libhunspellx64.dylib"
xcopy /Y ".\bin\tero.mac" ".\bin\tero.app\Contents\Resources\ShortCuts\Tero.key"
xcopy /Y ".\bin\tero.cfs" ".\bin\tero.app\Contents\Resources\tero.cfs"
xcopy /Y ".\src\tero.icns" ".\bin\tero.app\Contents\Resources\tero.icns"
