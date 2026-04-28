{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit SubtitleAPI;

{$warn 5023 off : no warning about unused units}
interface

uses
  UWSubtitleAPI, UWSubtitleAPI.Tags, UWSubtitleAPI.ExtraInfo, 
  UWSubtitleAPI.TimeCode, UWSubtitleAPI.EDL, UWSubtitleAPI.TBX, 
  UWSubtitleAPI.TMX, UWSubtitleAPI.CustomFormat, UWSubtitleAPI.Formats, 
  UWSubtitleAPI.Formats.ABCiView, UWSubtitleAPI.Formats.AdobeEncoreDVD, 
  UWSubtitleAPI.Formats.AdvancedSubstationAlpha, 
  UWSubtitleAPI.Formats.AdvancedSubtitles, UWSubtitleAPI.Formats.AQTitle, 
  UWSubtitleAPI.Formats.AvidCaption, UWSubtitleAPI.Formats.Captions32, 
  UWSubtitleAPI.Formats.CaptionsInc, UWSubtitleAPI.Formats.Cavena890, 
  UWSubtitleAPI.Formats.Cavena890.Types, UWSubtitleAPI.Formats.Cheetah, 
  UWSubtitleAPI.Formats.CheetahCaption, 
  UWSubtitleAPI.Formats.CheetahCaption.Types, UWSubtitleAPI.Formats.CPC600, 
  UWSubtitleAPI.Formats.CSV, UWSubtitleAPI.Formats.DKS, 
  UWSubtitleAPI.Formats.DRTIC, UWSubtitleAPI.Formats.DVDJunior, 
  UWSubtitleAPI.Formats.DVDSubtitle, UWSubtitleAPI.Formats.DVDSubtitleSystem, 
  UWSubtitleAPI.Formats.EBU, UWSubtitleAPI.Formats.EBU.Types, 
  UWSubtitleAPI.Formats.FABSubtitler, UWSubtitleAPI.Formats.GPACTTXT, 
  UWSubtitleAPI.Formats.IAuthor, UWSubtitleAPI.Formats.InscriberCG, 
  UWSubtitleAPI.Formats.ITunesTimedText, UWSubtitleAPI.Formats.JACOSub, 
  UWSubtitleAPI.Formats.KaraokeLyricsLRC, 
  UWSubtitleAPI.Formats.KaraokeLyricsVKT, 
  UWSubtitleAPI.Formats.MacDVDStudioPro, UWSubtitleAPI.Formats.MacSUB, 
  UWSubtitleAPI.Formats.MicroDVD, UWSubtitleAPI.Formats.MPlayer, 
  UWSubtitleAPI.Formats.MPlayer2, UWSubtitleAPI.Formats.NetflixTimedText, 
  UWSubtitleAPI.Formats.SoftNI, UWSubtitleAPI.Formats.Spreadsheet, 
  UWSubtitleAPI.Formats.SpruceSubtitleFile, 
  UWSubtitleAPI.Formats.SpruceSubtitleScript, UWSubtitleAPI.Formats.SubRip, 
  UWSubtitleAPI.Formats.SubViewer, UWSubtitleAPI.Formats.TeroSubtitler, 
  UWSubtitleAPI.Formats.TimedText, UWSubtitleAPI.Formats.TSV, 
  UWSubtitleAPI.Formats.WebVTT, UWSubtitleAPI.Utils, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('SubtitleAPI', @Register);
end.
