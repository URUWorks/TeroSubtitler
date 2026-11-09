; Languages and texts of the Tero Subtitler installer for Windows.
; Included by installscript_win.iss (#include), so it is not compiled on its own.
;
; To add a language:
;   1. Add its line to [Languages] with the Inno Setup .isl file (compiler:Languages\...).
;   2. Translate every message of [CustomMessages] with the same name as prefix
;      (for example "french.TypeFull=..."). A message without translation shows the English text.
;
; Messages without prefix are the English (default) text. %1, %2... are replaced by the script.
; Keep this file in UTF-8 with BOM, so Inno Setup reads the accented characters correctly.

[Languages]
Name: "english";             MessagesFile: "compiler:Default.isl"
Name: "spanish";             MessagesFile: "compiler:Languages\Spanish.isl"
Name: "brazilianportuguese"; MessagesFile: "compiler:Languages\BrazilianPortuguese.isl"
Name: "bulgarian";           MessagesFile: "compiler:Languages\Bulgarian.isl"
Name: "german";              MessagesFile: "compiler:Languages\German.isl"
Name: "italian";             MessagesFile: "compiler:Languages\Italian.isl"
Name: "russian";             MessagesFile: "compiler:Languages\Russian.isl"
Name: "slovenian";           MessagesFile: "compiler:Languages\Slovenian.isl"
Name: "turkish";             MessagesFile: "compiler:Languages\Turkish.isl"

[CustomMessages]
english.Dictionaries=Spell checking dictionaries
english.OtherTasks=Other tasks:
english.AssocSubtitles=Associate subtitle files with %1
english.TypeCompact=Compact installation
english.TypeFull=Full installation
english.TypeCustom=Custom installation
english.DownloadingFile=Downloading file %1 of %2
english.DownloadRetry=Downloading file %1 of %2 (try %3)

spanish.Dictionaries=Diccionarios ortográficos
spanish.OtherTasks=Otras tareas:
spanish.AssocSubtitles=Asociar los archivos de subtítulos con %1
spanish.TypeCompact=Instalación compacta
spanish.TypeFull=Instalación completa
spanish.TypeCustom=Instalación personalizada
spanish.DownloadingFile=Descargando el archivo %1 de %2
spanish.DownloadRetry=Descargando el archivo %1 de %2 (intento %3)

brazilianportuguese.Dictionaries=Dicionários de verificação ortográfica
brazilianportuguese.OtherTasks=Outras tarefas:
brazilianportuguese.AssocSubtitles=Associar os arquivos de legenda ao %1
brazilianportuguese.TypeCompact=Instalação compacta
brazilianportuguese.TypeFull=Instalação completa
brazilianportuguese.TypeCustom=Instalação personalizada
brazilianportuguese.DownloadingFile=Baixando o arquivo %1 de %2
brazilianportuguese.DownloadRetry=Baixando o arquivo %1 de %2 (tentativa %3)

bulgarian.Dictionaries=Речници за проверка на правописа
bulgarian.OtherTasks=Други задачи:
bulgarian.AssocSubtitles=Асоцииране на файловете със субтитри с %1
bulgarian.TypeCompact=Компактна инсталация
bulgarian.TypeFull=Пълна инсталация
bulgarian.TypeCustom=Инсталация по избор
bulgarian.DownloadingFile=Изтегляне на файл %1 от %2
bulgarian.DownloadRetry=Изтегляне на файл %1 от %2 (опит %3)

german.Dictionaries=Wörterbücher für die Rechtschreibprüfung
german.OtherTasks=Weitere Aufgaben:
german.AssocSubtitles=Untertiteldateien mit %1 verknüpfen
german.TypeCompact=Minimale Installation
german.TypeFull=Vollständige Installation
german.TypeCustom=Benutzerdefinierte Installation
german.DownloadingFile=Datei %1 von %2 wird heruntergeladen
german.DownloadRetry=Datei %1 von %2 wird heruntergeladen (Versuch %3)

italian.Dictionaries=Dizionari per il controllo ortografico
italian.OtherTasks=Altre attività:
italian.AssocSubtitles=Associa i file di sottotitoli a %1
italian.TypeCompact=Installazione compatta
italian.TypeFull=Installazione completa
italian.TypeCustom=Installazione personalizzata
italian.DownloadingFile=Download file %1 / %2
italian.DownloadRetry=Download del file %1 / %2 (tentativo %3)
italian.NameAndVersion={#MyAppName} {#AppVersion}
italian.LaunchProgram=Esegui {#MyAppName}
italian.AdditionalIcons=Collegamenti:
italian.CreateDesktopIcon=Crea collegamento programma sul &desktop
italian.CreateQuickLaunchIcon=Crea collegamento programma nella &barra 'Avvio veloce'
italian.AssocFileExtension=&Associa i file con estensione '%2' a '%1'
italian.AssocingFileExtension=Associazione file con estensione '%2' a '%1'...

russian.Dictionaries=Словари для проверки орфографии
russian.OtherTasks=Другие задачи:
russian.AssocSubtitles=Связать файлы субтитров с %1
russian.TypeCompact=Компактная установка
russian.TypeFull=Полная установка
russian.TypeCustom=Выборочная установка
russian.DownloadingFile=Загрузка файла %1 из %2
russian.DownloadRetry=Загрузка файла %1 из %2 (попытка %3)

slovenian.Dictionaries=Slovarji za preverjanje črkovanja
slovenian.OtherTasks=Druga opravila:
slovenian.AssocSubtitles=Poveži datoteke podnapisov s programom %1
slovenian.TypeCompact=Osnovna namestitev
slovenian.TypeFull=Polna namestitev
slovenian.TypeCustom=Namestitev po meri
slovenian.DownloadingFile=Prenašanje datoteke %1 od %2
slovenian.DownloadRetry=Prenašanje datoteke %1 od %2 (poskus %3)

turkish.Dictionaries=Yazım denetimi sözlükleri
turkish.OtherTasks=Diğer görevler:
turkish.AssocSubtitles=Altyazı dosyalarını %1 ile ilişkilendir
turkish.TypeCompact=Küçük kurulum
turkish.TypeFull=Tam kurulum
turkish.TypeCustom=Özel kurulum
turkish.DownloadingFile=Dosya indiriliyor: %1 / %2
turkish.DownloadRetry=Dosya indiriliyor: %1 / %2 (deneme %3)
