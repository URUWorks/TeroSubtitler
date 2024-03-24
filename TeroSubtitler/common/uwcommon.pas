{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit UWCommon;

{$warn 5023 off : no warning about unused units}
interface

uses
  UWSystem.Encoding, UWSystem.FileUtils, UWSystem.StrUtils, UWSystem.SysUtils, 
  UWSystem.TimeUtils, UWSystem.InetUtils, UWSystem.Globalization, 
  UWSystem.Process, UWSubtitles.OCR, UWSubtitles.Utils, UWFiles.MRU, 
  UWSpellcheck.Hunspell, UWTranslateAPI.Google, UWWindows.MenuTheming, 
  UWWindows.DarkTheme, UWSystem.ThreadProcess, UWSystem.TextToSpeech, 
  UWFiles.MPEGAudio, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('UWCommon', @Register);
end.
