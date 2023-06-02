{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit SubtitleAPI;

{$warn 5023 off : no warning about unused units}
interface

uses
  UWSubtitleAPI, UWSubtitleAPI.Tags, UWSubtitleAPI.ExtraInfo, 
  UWSubtitleAPI.TimeCode, UWSubtitleAPI.TMX, UWSubtitleAPI.TBX, 
  UWSubtitleAPI.EDL, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('SubtitleAPI', @Register);
end.
