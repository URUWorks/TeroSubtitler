{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit SubtitleAPI;

{$warn 5023 off : no warning about unused units}
interface

uses
  UWSubtitleAPI, UWSubtitleAPI.Tags, UWSubtitleAPI.ExtraInfo, 
  UWSubtitleAPI.TimeCode, UWSubtitleAPI.CustomFormat, UWSubtitleAPI.EDL, 
  UWSubtitleAPI.Formats, UWSubtitleAPI.Utils, UWSubtitleAPI.TBX, 
  UWSubtitleAPI.TMX, ABCiView, AdobeEncoreDVD, AdvancedSubstationAlpha, 
  AdvancedSubtitles, AQTitle, AvidCaption, Captions32, CaptionsInc, Cavena890, 
  Cavena890.Types, Cheetah, CheetahCaption, CheetahCaption.Types, CPC600, CSV, 
  DKS, DRTIC, DVDJunior, DVDSubtitle, DVDSubtitleSystem, EBU, EBU.Types, 
  FABSubtitler, GPACTTXT, IAuthor, InscriberCG, ITunesTimedText, JACOSub, 
  KaraokeLyricsLRC, KaraokeLyricsVKT, MacDVDStudioPro, MacSUB, MicroDVD, 
  MPlayer, MPlayer2, NetflixTimedText, SoftNI, Spreadsheet, 
  SpruceSubtitleFile, SpruceSubtitleScript, SubRip, SubViewer, TeroSubtitler, 
  TimedText, TSV, WebVTT, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('SubtitleAPI', @Register);
end.
