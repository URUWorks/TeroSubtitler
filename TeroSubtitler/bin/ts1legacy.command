#!/bin/bash
# Arma "Tero Subtitler.app" en el Escritorio a partir de los archivos de esta carpeta:
#   Languages/     idiomas, van a Contents/Resources/Languages
#   Dictionaries/ OCR/ CustomFormat/   copias de bin, van a Contents/Resources/
#   tero.app            plantilla del bundle (Info.plist, icono, recursos)
#   tero                binario
#   frameworks-legacy/  dylibs que van a Contents/Frameworks
set -euo pipefail

FRAMEWORKS_DIR="frameworks-legacy"
HERE="$(cd "$(dirname "$0")" && pwd)"
SRC_APP="$HERE/tero.app"
DEST_APP="$HOME/Desktop/Tero Subtitler.app"

die() { echo "ERROR: $*" >&2; echo; read -n 1 -s -r -p "Presioná una tecla para cerrar..."; exit 1; }

[ -d "$SRC_APP" ] || die "No encuentro tero.app en $HERE"
[ -d "$HERE/$FRAMEWORKS_DIR" ] || die "No encuentro la carpeta $FRAMEWORKS_DIR en $HERE"
[ -d "$HERE/Languages" ] || die "No encuentro la carpeta Languages en $HERE"
for d in Dictionaries OCR CustomFormat; do
  [ -d "$HERE/$d" ] || die "No encuentro la carpeta $d en $HERE"
done
[ -f "$HERE/tero" ] || die "No encuentro el binario tero en $HERE"

# Nombre del ejecutable según el Info.plist del bundle (Lazarus usa "tero")
EXE="$(/usr/libexec/PlistBuddy -c 'Print :CFBundleExecutable' "$SRC_APP/Contents/Info.plist" 2>/dev/null || echo tero)"

echo "==> Copiando tero.app a \"$DEST_APP\""
rm -rf "$DEST_APP"
ditto "$SRC_APP" "$DEST_APP"

echo "==> Copiando el binario a Contents/MacOS/$EXE"
mkdir -p "$DEST_APP/Contents/MacOS"
# Lazarus deja Contents/MacOS/tero como enlace simbólico al binario: se reemplaza por el archivo real
rm -f "$DEST_APP/Contents/MacOS/$EXE"
cp "$HERE/tero" "$DEST_APP/Contents/MacOS/$EXE"
chmod +x "$DEST_APP/Contents/MacOS/$EXE"
echo "    arquitecturas: $(lipo -archs "$DEST_APP/Contents/MacOS/$EXE")"

echo "==> Copiando $FRAMEWORKS_DIR a Contents/Frameworks"
rm -rf "$DEST_APP/Contents/Frameworks"
ditto "$HERE/$FRAMEWORKS_DIR" "$DEST_APP/Contents/Frameworks"
find "$DEST_APP/Contents/Frameworks" -name '.DS_Store' -delete

# En macOS Tero busca los idiomas en Contents/Resources/Languages (LanguageFolder en procConfig.pas)
echo "==> Copiando Languages a Contents/Resources/Languages"
mkdir -p "$DEST_APP/Contents/Resources"
rm -rf "$DEST_APP/Contents/Resources/Languages"
ditto "$HERE/Languages" "$DEST_APP/Contents/Resources/Languages"
find "$DEST_APP/Contents/Resources/Languages" -name '.DS_Store' -delete

# Diccionarios, OCR y formatos personalizados también desde bin: las copias de la
# plantilla tero.app no están al día (faltaba en_GB)
for d in Dictionaries OCR CustomFormat; do
  echo "==> Copiando $d a Contents/Resources/$d"
  rm -rf "$DEST_APP/Contents/Resources/$d"
  ditto "$HERE/$d" "$DEST_APP/Contents/Resources/$d"
  find "$DEST_APP/Contents/Resources/$d" -name '.DS_Store' -delete
done

echo "==> Quitando cuarentena y firmando ad-hoc"
xattr -cr "$DEST_APP"
codesign --force --deep --sign - "$DEST_APP" || die "Falló codesign"

echo
echo "Listo: $DEST_APP"
read -n 1 -s -r -p "Presioná una tecla para cerrar..."
echo
