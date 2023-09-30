{*
 *  URUWorks
 *
 *  The contents of this file are used with permission, subject to
 *  the Mozilla Public License Version 2.0 (the "License"); you may
 *  not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *  http://www.mozilla.org/MPL/2.0.html
 *
 *  Software distributed under the License is distributed on an
 *  "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 *  implied. See the License for the specific language governing
 *  rights and limitations under the License.
 *
 *  Copyright (C) 2023 URUWorks, uruworks@gmail.com.
 *}

unit procConfig;

{$mode ObjFPC}{$H+}
{$IFDEF DARWIN}
{$modeswitch objectivec1}
{$ENDIF}

interface

uses
  Classes, Controls, SysUtils, Menus, Forms, procTypes, UWSubtitleAPI,
  UWHotKey, formCustomQuestionDlg
  {$IFNDEF WINDOWS}, UWCheckBox, UWRadioButton{$ENDIF}
  {$IFDEF DARWIN}, CocoaAll, CocoaUtils{$ENDIF};

{ Settings }

procedure DefaultValues;
procedure LoadSettings;
procedure SaveSettings;

function LoadFormSettings(const AForm: TForm): String;
procedure SaveFormSettings(const AForm: TForm; const AExtra: String = '');

procedure LoadActors;
procedure SaveActors;

procedure UpdateValuesFromDefaultConvention;

{ Language }

procedure LoadLanguage(const AForm: TForm; const ACommonControls: Boolean = True);
function GetLangString(const S: String): String;
function GetCommonString(const S: String; const ASection: String = 'CommonStrings'): String;
procedure UpdateCommonActionString;
function GetRandomTipString: String;
procedure ApplyCommonControlsString(const AForm: TForm; const ASection: String = 'CommonControls');

{ Special folders }

function GetCustomFolderPath(const SubFolder: String): String;
function GetCustomUserPath(const AFolder: String): String;
function GetCustomFilePath(const FileName: String): String;
function SettingsFileName: String;
function CurrentWorkFileName: String;
function LanguageFolder: String;
function LanguageFileName: String;
function ShortCutFolder: String;
function ShortCutFileName: String;
function OCRFolder: String;
function IconsFolder: String;
function ExtensionsFolder: String;
function WaveformsFolder: String;
function ShotChangesFolder: String;
function MRUFileName: String;
function ConventionsFileName: String;
function StylesFileName: String;
function ActorsFileName: String;
function HunspellFileName: String;
function libMPVFileName(const AFullRoot: Boolean = True): String;
function YTDLPFileName(const AFullRoot: Boolean = True): String;
function ffmpegFileName: String;
function SceneDetectFileName: String;
function LogMPVFileName: String;
function MPVTempSubFileName: String;
function WebPreviewTempFileName: String;
function DictionariesFolder: String;
function BackupFolder: String;
function ProjectsFolder: String;
function CustomFormatFolder: String;
function TranslationMemoryFolder: String;
function TerminologyFolder: String;
function WhisperFolder: String;
function WhisperFileName: String;
function WhisperModelsFolder: String;
function WhisperTranscriptionsFolder: String;
function libmpvFolder: String;
function YTDLPFolder: String;
function ffmpegFolder: String;

function GetDictionaryNameFromCaption(const AText: String): String;
function GetExtractAppFile(const FFmpeg: Boolean = True): String;
function GetAudioToTextAppFile: String;
function GetAudioToTextParams: String;
{$IFDEF UNIX}
function GetInstallFolder(const AFileName: String): String;
{$ENDIF}

{ Check for updates}

procedure CheckForUpdates;

// -----------------------------------------------------------------------------

implementation

uses
  RegExpr, formMain, XMLConf, UWSystem.Encoding, UWSystem.SysUtils, Dialogs,
  procDialogs, UWSubtitleAPI.Formats, UWLayout, UWSystem.XMLLang, MPVPlayer,
  procWorkspace, procColorTheme, LCLProc, UWSystem.Globalization,
  UWSystem.TimeUtils, libMPV.Client, UWSpellcheck.Hunspell, procConventions,
  UWSystem.InetUtils, fileinfo, winpeimagereader, elfreader, machoreader,
  LCLIntf, UWSystem.StrUtils;

// -----------------------------------------------------------------------------

{ Settings }

// -----------------------------------------------------------------------------

procedure DefaultValues;
begin
  LastTickCount := 0;

  FillByte(Tools, SizeOf(TTools), 0);
  with Tools do
  begin
    {$IFDEF WINDOWS}
    if DirectoryExists(GetCustomUserPath('ffmpeg')) then
      FFmpeg := ConcatPaths([GetCustomUserPath('ffmpeg'), FFMPEG_EXE]);

    if DirectoryExists(GetCustomUserPath('PySceneDetect')) then
      PySceneDetect := ConcatPaths([GetCustomUserPath('PySceneDetect'), SCENEDETECT_EXE]);

    if DirectoryExists(GetCustomUserPath('Whisper')) then
    begin
      WhisperCPP    := ConcatPaths([GetCustomUserPath('Whisper'), WHISPER_EXE]);
      FasterWhisper := ConcatPaths([GetCustomUserPath('Whisper'), FASTERWHISPER_EXE]);
    end;

    if FileExists(GetCustomFilePath(YTDLP_EXE)) then
      YTDLP := GetCustomFilePath(YTDLP_EXE)
    else
      YTDLP := ConcatPaths([YTDLPFolder, YTDLP_EXE]);
    {$ELSE}
    if DirectoryExists(GetCustomUserPath('ffmpeg')) then
      FFmpeg := ConcatPaths([GetCustomUserPath('ffmpeg'), FFMPEG_EXE])
    else
      FFmpeg := GetInstallFolder(FFMPEG_EXE);

    if DirectoryExists(GetCustomUserPath('PySceneDetect')) then
      PySceneDetect := ConcatPaths([GetCustomUserPath('PySceneDetect'), SCENEDETECT_EXE])
    else
      PySceneDetect := GetInstallFolder(SCENEDETECT_EXE);

    if DirectoryExists(GetCustomUserPath('Whisper')) then
    begin
      WhisperCPP    := ConcatPaths([GetCustomUserPath('Whisper'), WHISPER_EXE]);
      FasterWhisper := ConcatPaths([GetCustomUserPath('Whisper'), FASTERWHISPER_EXE]);
    end
    else
    begin
      WhisperCPP    := GetInstallFolder(WHISPER_EXE);
      FasterWhisper := GetInstallFolder(FASTERWHISPER_EXE);
    end;

    if DirectoryExists(GetCustomUserPath('ytdlp')) then
      YTDLP := ConcatPaths([GetCustomUserPath('ytdlp'), YTDLP_EXE])
    else
      YTDLP := GetInstallFolder(YTDLP_EXE);
    {$ENDIF}

    FFmpeg_ParamsForAudioExtract := FFMPEG_Params;
    FFmpeg_ParamsForShotChanges  := FFMPEG_SCParams;
    FFmpeg_ParamsForWhisper      := WHISPER_ffParams;
    PySceneDetect_Params         := SCENEDETECT_SCParams;
    WhisperCPP_Params            := WHISPER_Params;
  end;
  Tools.FasterWhisper_Params     := FASTERWHISPER_Params;

  with VSTOptions do
  begin
    BackgroundBlock  := NIL;
    DrawMode         := dmList;
    RepaintBckgBlock := False;
    DrawErrors       := True;
    DrawTags         := True;
  end;

  FillByte(MPVOptions, SizeOf(TMPVOptions), 0);
  with MPVOptions do
  begin
    SubtitleHandleByMPV    := False;
    TextColor              := '#FFFFFF';
    TextBorderColor        := '#000000';
    TextShadowColor        := '#000000';
    UseTextShadowColor     := True;
    TextBackgroundColor    := '#000000';
    UseTextBackgroundColor := False;
    TextPosition           := 'bottom';
    TextSize               := 32;
    TextShadowOffset       := 2;
    AutoStartPlaying       := False;
    SeekTime               := 5000;
    FrameStep              := 1;
    Volume.Percent         := 75;
    Volume.Mute            := False;
  end;

  FillByte(WAVEOptions, SizeOf(TWAVEOptions), 0);
  with WAVEOptions do
  begin
    LoopCount := 3;
  end;

  FillByte(Workspace, SizeOf(TWorkspace), 0);
  with Workspace do
  begin
    SMPTE         := False;
    FPS.InputFPS  := 23.976;
    FPS.OutputFPS := FPS.InputFPS;
    DefEncoding   := MaxEncodings-1; // utf-8
    DefFormat     := sfSubRip;
    Transcription.Memo := NIL;
  end;

  FillByte(SubtitleInfo, SizeOf(TSubtitleInfo), 0);

  with AppOptions do
  begin
    FillByte(Conventions, SizeOf(TProfileItem), 0);
    with Conventions do
    begin
      Name               := 'Netflix (English) [adults]';
      RepeatableChars    := '-¡!¿?";\/_[]=';
      ProhibitedChars    := '@,http,www,#,*,–,—';
      CPSLineLenStrategy := '';
      NewSubtitleMS      := 833;
      MinDurationPerWord := 0;
      MinDuration        := 833;
      MaxDuration        := 7000;
      MaxLines           := 2;
      MinPause           := 2;
      PauseInFrames      := True;
      MaxCPS             := 20;
      RepeatedTolerance  := 100;
      WPM                := 0;
      CPL                := 42;
      ShotcutSnapArea    := 7;
      ShotcutThreshold   := 12;
      ShotcutInCues      := 0;
      ShotcutOutCues     := 2;
      Chaining           := 12;
      DotsOnSplit        := True;

      frmMain.mmoText.CPSBar.Max := MaxCPS;
    end;

    CommonErrors := [etBadValues, etTimeTooLong, etTimeTooShort, etPauseTooShort,
      etMaxCPS, etOverlapping, etFixTags, etEmpty, etUnbreak, etUnnecessarySpaces,
      etUnnecessaryDots, etRepeatedChars, etProhibitedChars, etHearingImpaired,
      etBreakLongLines, etRepeatedSubtitle, etEllipsesSingleSmartCharacter,
      etMaxLines, etOCR];

    DefChangePlayRate      := 50;
    DialogSegmentThreshold := 6000;
    ShiftTimeMS            := 500;
    AutoBackupSeconds      := 300;

    AutoLengthChar       := 60;
    AutoLengthWord       := 50;
    AutoLengthLine       := 50;
    ExpandMs             := 1500;
    ExpandChar           := 40;
    ExpandLen            := 1000;

    ShowWelcomeAtStartup  := True;
    UseOwnFileDialog      := False;
    AutoCheckErrors       := True;
    AskForDeleteLines     := True;
    AskForInputFPS        := False;
    CheckErrorsBeforeSave := True;
    TextToFind            := '';
    WebSearchURL          := URL_WordReference;
    Language              := GetOSLanguage;
    HunspellLanguage      := 'en_US';
    ShortCutPreset        := 'Tero.key';
    // Our FormatSettings
    FormatSettings       := DefaultFormatSettings;
    FormatSettings.DecimalSeparator  := '.';
    FormatSettings.ThousandSeparator := FormatSettings.DecimalSeparator;
  end;

  with frmMain do
  begin
    actShowColumnNumber.Checked    := True;
    actShowColumnTimes.Checked     := True;
    actShowColumnDuration.Checked  := True;
    //actViewShotChange.Checked      := False;
    //actViewShotChangeExecute(NIL);
    actCenterWaveform.Checked      := True;
    actCenterWaveformExecute(NIL);
    actShowToolBarMain.Checked     := True;
    actShowToolBarFPS.Checked      := True;
    actShowToolBarFormat.Checked   := True;
    actShowToolBarEncoding.Checked := True;
  end;

  {$IFDEF WINDOWS}
  frmMain.MPV.RendererMode := rmEmbedding;
  {$ELSE}
  frmMain.MPV.RendererMode := rmOpenGL;
  {$ENDIF}
  ColorThemeInstance.ColorMode := cmAuto;
end;

// -----------------------------------------------------------------------------

procedure LoadSettings;
var
  i, j, c: Integer;
  bands: array of Byte;
begin
  // Possibly first start
  if not FileExists(SettingsFileName) then
  begin
    SetVideoPreview(False);
    SetWaveformPreview(False);

    if not FileExists(LanguageFileName) then
    begin
      if AppOptions.Language.StartsWith('es-') then
        AppOptions.Language := 'es-UY'
      else
        AppOptions.Language := 'en-US';
    end;

    UpdateValuesFromDefaultConvention;

    with frmMain do
    begin
      tlb9.Hide;
      tlb11.Hide;
      tlb12.Hide;
      tlb13.Hide;
      tlb14.Hide;
      tlb15.Hide;
      tlb16.Hide;
      tlb17.Hide;
      tlb18.Hide;
      etlb5.Hide;
      vtlb8.Hide;
      vtlb9.Hide;
      vtlb10.Hide;
      vtlb11.Hide;
      wtlb1.Hide;
      wtlb2.Hide;
      wtlb3.Hide;
      wtlb7.Hide;
      wtlb8.Hide;
      wtlb9.Hide;
      wtlb10.Hide;

      UpdateToolBarButtons(True);
      CoolBarMain.Bands[0].AutosizeWidth;
      CoolBarMain.Bands[1].AutosizeWidth;
      CoolBarMain.Bands[2].AutosizeWidth;

      LayoutVideo.Width := Width div 2;
    end;

    Exit;
  end;

  with TXMLConfig.Create(NIL) do
  try
    FileName := SettingsFileName;
    with AppOptions do
    begin
      OpenKey('Settings');
      Language := GetValue('Language', Language);
      HunspellLanguage := GetValue('HunspellLanguage', HunspellLanguage);
      ShortCutPreset := GetValue('ShortCutFile', ShortCutPreset);
      ShowWelcomeAtStartup := GetValue('ShowWelcomeAtStartup', ShowWelcomeAtStartup);
      AutoBackupSeconds := GetValue('AutoBackupSeconds', AutoBackupSeconds);
      ColorThemeInstance.ColorMode := TColorMode(GetValue('ColorThemeMode', Integer(ColorThemeInstance.ColorMode)));
      AutoCheckErrors := GetValue('AutoCheckErrors', AutoCheckErrors);
      DefChangePlayRate := GetValue('DefChangePlayRate', DefChangePlayRate);
      DialogSegmentThreshold := GetValue('DialogSegmentThreshold', DialogSegmentThreshold);
      ShiftTimeMS := GetValue('ShiftTimeMS', ShiftTimeMS);
      AutoLengthChar := GetValue('AutoLengthChar', AutoLengthChar);
      AutoLengthWord := GetValue('AutoLengthWord', AutoLengthWord);
      AutoLengthLine := GetValue('AutoLengthLine', AutoLengthLine);
      ExpandMs := GetValue('ExpandMs', ExpandMs);
      ExpandChar := GetValue('ExpandChar', ExpandChar);
      ExpandLen := GetValue('ExpandLen', ExpandLen);
      UseOwnFileDialog := GetValue('UseOwnFileDialog', UseOwnFileDialog);
      AskForDeleteLines := GetValue('AskForDeleteLines', AskForDeleteLines);
      AskForInputFPS := GetValue('AskForInputFPS', AskForInputFPS);
      CheckErrorsBeforeSave := GetValue('CheckErrorsBeforeSave', CheckErrorsBeforeSave);
      WebSearchURL := GetValue('WebSearchURL', WebSearchURL);
      TextToFind := GetValue('TextToFind', TextToFind);
      CommonErrors := TSubtitleErrorTypeSet(GetValue('CommonErrors', Integer(CommonErrors)));
      frmMain.mmoText.CPSBar.Visible := GetValue('ShowCPSBar', True);
      frmMain.mmoTranslation.CPSBar.Visible := frmMain.mmoText.CPSBar.Visible;
      CloseKey;

      with Workspace, frmMain do
      begin
        OpenKey('Workspace');
        SetViewMode(TViewMode(GetValue('ViewMode', 0)));
        WorkMode := TWorkMode(GetValue('WorkMode', Integer(WorkMode)));
        TranslatorMode := GetValue('TranslatorMode', TranslatorMode);
        SetTranslatorMode(TranslatorMode);
        FPS.InputFPS := StrToSingle(GetValue('DefFPS', SingleToStr(FPS.InputFPS, FormatSettings)), 25, FormatSettings);
        DefEncoding  := GetValue('DefEncoding', DefEncoding);
        DefFormat    := TUWSubtitleFormats(GetValue('DefFormat', Integer(DefFormat)));
        SetVideoPreview(GetValue('VideoPreview', False));
        SetWaveformPreview(GetValue('AudioPreview', False));
        SetDockVideoWindow(GetValue('DockVideoControls', True));
        SetDockWaveformWindow(GetValue('DockAudioControls', True));
        actSwapWorkspace.Checked := GetValue('SwapWorkspace', False);
        SwapWorkspaceLayout;
        CloseKey;
      end;

      with Conventions do
      begin
        OpenKey('CommonErrorsCfg');
        Name := GetValue('Profile', Name);
        RepeatableChars := GetValue('RepeatableChars', RepeatableChars);
        ProhibitedChars := GetValue('ProhibitedChars', ProhibitedChars);
        CPSLineLenStrategy := GetValue('CPSLineLenStrategy', CPSLineLenStrategy);
        NewSubtitleMS := GetValue('NewSubtitleMS', NewSubtitleMS);
        MinDuration := GetValue('MinDuration', MinDuration);
        MinDurationPerWord := GetValue('MinDurationPerWord', MinDurationPerWord);
        MaxDuration := GetValue('MaxDuration', MaxDuration);
        MaxLines := GetValue('MaxLines', MaxLines);
        MinPause := GetValue('MinPause', MinPause);
        PauseInFrames := GetValue('PauseInFrames', PauseInFrames);
        MaxCPS := GetValue('MaxCPS', MaxCPS);
        RepeatedTolerance := GetValue('RepeatedTolerance', RepeatedTolerance);
        WPM := GetValue('WPM', WPM);
        CPL := GetValue('CPL', CPL);
        ShotcutSnapArea := GetValue('ShotcutSnapArea', ShotcutSnapArea);
        ShotcutThreshold := GetValue('ShotcutThreshold', ShotcutThreshold);
        ShotcutInCues := GetValue('ShotcutInCues', ShotcutInCues);
        ShotcutOutCues := GetValue('ShotcutOutCues', ShotcutOutCues);
        Chaining := GetValue('Chaining', Chaining);
        DotsOnSplit := GetValue('DotsOnSplit', DotsOnSplit);
        CloseKey;

        if PauseInFrames then
          frmMain.WAVE.MinimumBlank := FramesToTime(MinPause, Workspace.FPS.OutputFPS)
        else
          frmMain.WAVE.MinimumBlank := MinPause;

        frmMain.mmoText.CPSBar.Max := MaxCPS;
      end;

      with VSTOptions, frmMain do
      begin
        OpenKey('VST');
        DrawErrors := GetValue('DrawErrors', DrawErrors);
        DrawTags := GetValue('DrawTags', DrawTags);
        DrawMode := TVSTDrawMode(GetValue('DrawMode', Integer(DrawMode)));
        actShowColumnNumber.Checked := GetValue('ColumnNumber', actShowColumnNumber.Checked);
        actShowColumnTimes.Checked := GetValue('ColumnTimes', actShowColumnTimes.Checked);
        actShowColumnDuration.Checked := GetValue('ColumnDuration', actShowColumnDuration.Checked);
        actShowColumnStyleAndActor.Checked := GetValue('ColumnStyleAndActor', actShowColumnStyleAndActor.Checked);
        actShowColumnCPS.Checked := GetValue('ColumnCPS', actShowColumnCPS.Checked);
        actShowColumnWPM.Checked := GetValue('ColumnWPM', actShowColumnWPM.Checked);
        actShowColumnCPL.Checked := GetValue('ColumnCPL', actShowColumnCPL.Checked);
        CloseKey;
      end;

      with MPVOptions, frmMain do
      begin
        OpenKey('MPV');
        SubtitleHandleByMPV := GetValue('SubtitleHandleByMPV', SubtitleHandleByMPV);
        AutoStartPlaying := GetValue('AutoStartPlaying', AutoStartPlaying);
        SubtitleToShow := TSubtitleMode(GetValue('SubtitleToShow', Integer(SubtitleToShow)));
        actMediaAutoScroll.Checked := GetValue('UpdateListOnPreview', actMediaAutoScroll.Checked);
        actMediaChangePlayRate.Checked := GetValue('MediaChangePlayRate', actMediaChangePlayRate.Checked);
        actShowActorOnPreview.Checked := GetValue('ShowActorOnPreview', actShowActorOnPreview.Checked);
        TextBorderColor := GetValue('TextBorderColor', TextBorderColor);
        TextColor := GetValue('TextColor', TextColor);
        TextShadowColor := GetValue('TextShadowColor', TextShadowColor);
        TextBackgroundColor := GetValue('TextBackgroundColor', TextBackgroundColor);
        UseTextShadowColor := GetValue('UseTextShadowColor', UseTextShadowColor);
        UseTextBackgroundColor := GetValue('UseTextBackgroundColor', UseTextBackgroundColor);
        TextPosition := GetValue('TextPosition', TextPosition);
        TextSize := GetValue('TextSize', TextSize);
        TextShadowOffset := GetValue('TextShadowOffset', TextShadowOffset);
        MPV.RendererMode := TMPVPlayerRenderMode(GetValue('RendererMode', Integer({$IFDEF WINDOWS}rmEmbedding{$ELSE}rmOpenGL{$ENDIF})));
        SeekTime         := GetValue('SeekTime', 5000);
        FrameStep        := GetValue('FrameStep', 1);
        Volume.Percent   := GetValue('Volume', 75);
        Volume.Mute      := GetValue('Mute', False);
        actMediaVolumeMute.Checked := Volume.Mute;
        CloseKey;
      end;

      with WAVEOptions do
      begin
        OpenKey('WAVE');
        LoopCount := GetValue('LoopCount', LoopCount);
        frmMain.WAVE.DrawGAP := GetValue('DrawGAP', False);
        frmMain.actViewShotChange.Checked := GetValue('ViewShotChanges', False);
        frmMain.actViewShotChangeExecute(NIL);
        frmMain.actCenterWaveform.Checked := GetValue('CenterWaveform', False);
        frmMain.actCenterWaveformExecute(NIL);
        CloseKey;
      end;

      with Tools do
      begin
        OpenKey('Tools');
        FFmpeg                       := GetValue('FFmpeg', FFmpeg);
        FFmpeg_ParamsForAudioExtract := GetValue('FFmpeg_ParamsForAudioExtract', FFmpeg_ParamsForAudioExtract);
        FFmpeg_ParamsForShotChanges  := GetValue('FFmpeg_ParamsForShotChanges', FFmpeg_ParamsForShotChanges);
        FFmpeg_ParamsForWhisper      := GetValue('FFmpeg_ParamsForWhisper', FFmpeg_ParamsForWhisper);
        PySceneDetect        := GetValue('PySceneDetect', PySceneDetect);
        PySceneDetect_Params := GetValue('PySceneDetect_Params', PySceneDetect_Params);
        WhisperCPP        := GetValue('WhisperCPP', WhisperCPP);
        WhisperCPP_Params := GetValue('WhisperCPP_Params', WhisperCPP_Params);
        FasterWhisper        := GetValue('FasterWhisper', FasterWhisper);
        FasterWhisper_Params := GetValue('FasterWhisper_Params', FasterWhisper_Params);
        WhisperEngine := TWhisperEngine(GetValue('WhisperEngine', 0));
        YTDLP := GetValue('YTDLP', YTDLP);
        CloseKey;
      end;

      with frmMain do
      begin
        OpenKey(Name);
        SetBounds(GetValue('X', Left), GetValue('Y', Top), GetValue('W', Width), GetValue('H', Height));
        LayoutVideo.Width := GetValue('VW', LayoutVideo.Width);
        LayoutWaveform.Height := GetValue('WF', LayoutWaveform.Height);
        CloseKey;

        OpenKey('ToolBars');
          OpenKey('Main');
          for i := 0 to ToolBarMain.ButtonCount-1 do
            ToolBarMain.Buttons[i].Visible := GetValue('id'+i.ToString, True);
          CloseKey;
          OpenKey('Editor');
          for i := 0 to ToolBarEditor.ButtonCount-1 do
            ToolBarEditor.Buttons[i].Visible := GetValue('id'+i.ToString, True);
          CloseKey;
          OpenKey('Video');
          for i := 0 to ToolBarVideo.ButtonCount-1 do
            ToolBarVideo.Buttons[i].Visible := GetValue('id'+i.ToString, True);
          CloseKey;
          OpenKey('Waveform');
          for i := 0 to ToolBarWaveform.ButtonCount-1 do
            ToolBarWaveform.Buttons[i].Visible := GetValue('id'+i.ToString, True);
          CloseKey;
        CloseKey;
        UpdateToolBarButtons(True);

        OpenKey('CoolBars');
        c := CoolBarMain.Bands.Count;
        SetLength(bands, c);
        for i := 0 to c-1 do
        begin
          OpenKey('Id'+CoolBarMain.Bands[i].ID.ToString);
          CoolBarMain.Bands[i].Break := GetValue('Break', False);
          CoolBarMain.Bands[i].Visible := GetValue('Visible', True);
          CoolBarMain.Bands[i].Width := GetValue('Width', CoolBarMain.Bands[i].Width);
          bands[i] := GetValue('Pos', i);
          CloseKey;
        end;
        actShowToolBarMain.Checked     := GetValue('Main', True);
        actShowToolBarFPS.Checked      := GetValue('FPS', True);
        actShowToolBarFormat.Checked   := GetValue('Format', True);
        actShowToolBarEncoding.Checked := GetValue('Encoding', True);
        CloseKey;
        for i := 0 to c-1 do
          for j := 0 to c-1 do
            if bands[j] = i then
            begin
              CoolBarMain.Bands.FindItemID(j).Index := i;
              Break;
            end;
        SetLength(bands, 0);
        UpdateCoolBar(-1, False);
      end;
    end;
    // Format properties
    OpenKey('FormatProperties');
      //
      OpenKey(FormatToName(sfAdvancedSubtitles, True));
      with Subtitles.FormatProperties^.AdvancedSubtitles do
      begin
        Language  := GetValue('Language', Language);
        FontName  := GetValue('FontName', FontName);
        FontSize  := GetValue('FontSize', FontSize);
        FontColor := GetValue('FontColor', FontColor);
        X         := GetValue('X', X);
        Y         := GetValue('Y', Y);
        W         := GetValue('W', W);
        H         := GetValue('H', H);;
        Alignment := GetValue('Alignment', Alignment);
      end;
      CloseKey;
      //
      OpenKey(FormatToName(sfCavena890, True));
      with Subtitles.FormatProperties^.Cavena890 do
      begin
        AnsiStringToAnsiChar(TapeNumber, GetValue('TapeNumber', TapeNumber));
        AnsiStringToAnsiChar(TranslatedTitle, GetValue('TranslatedTitle', TranslatedTitle));
        AnsiStringToAnsiChar(Translator, GetValue('Translator', Translator));
        AnsiStringToAnsiChar(TranslatedEpisode, GetValue('TranslatedEpisode', TranslatedEpisode));
        AnsiStringToAnsiChar(Comments, GetValue('Comments', Comments));
        AnsiStringToAnsiChar(PrimaryFont, GetValue('PrimaryFont', PrimaryFont));
        AnsiStringToAnsiChar(OriginalTitle, GetValue('OriginalTitle', OriginalTitle));
        AnsiStringToAnsiChar(SecondaryFont, GetValue('SecondaryFont', SecondaryFont));
        AnsiStringToAnsiChar(StartTime, GetValue('StartTime', StartTime));
        AnsiStringToAnsiChar(Producer, GetValue('Producer', Producer));
        AnsiStringToAnsiChar(EpisodeTitle, GetValue('EpisodeTitle', EpisodeTitle));
        PrimaryLanguage := GetValue('PrimaryLanguage', PrimaryLanguage);
      end;
      CloseKey;
      //
      OpenKey(FormatToName(sfDVDSubtitle, True));
      with Subtitles.FormatProperties^.DVDSubtitle do
      begin
        Assigned := GetValue('Assigned', Assigned);
        DiskId   := GetValue('DiskId', DiskId);
        DVDTitle := GetValue('DVDTitle', DVDTitle);
        Language := GetValue('Language', Language);
        Author   := GetValue('Author', Author);
        Web      := GetValue('Web', Web);
        Info     := GetValue('Info', Info);
        License  := GetValue('License', License);
      end;
      CloseKey;
      //
      OpenKey(FormatToName(sfEBU, True));
      with Subtitles.FormatProperties^.EBU do
      begin
        DiskFormatCode            := GetValue('DiskFormatCode', DiskFormatCode);
        CodePageNumber            := GetValue('CodePageNumber', CodePageNumber);
        DisplayStandardCode       := GetValue('DisplayStandardCode', DisplayStandardCode);
        CharCodeTableNumber       := GetValue('CharCodeTableNumber', CharCodeTableNumber);
        LanguageCode              := GetValue('LanguageCode', LanguageCode);
        CountryOrigin             := GetValue('CountryOrigin', CountryOrigin);
        MaxNumberDisplayableChars := GetValue('MaxNumberDisplayableChars', MaxNumberDisplayableChars);
        MaxNumberDisplayableRows  := GetValue('MaxNumberDisplayableRows', MaxNumberDisplayableRows);
      end;
      CloseKey;
      //
      OpenKey(FormatToName(sfAdvancedSubStationAlpha, True));
      with Subtitles.FormatProperties^.SSA do
      begin
        DefaultStyleSettings := GetValue('DefaultStyleSettings', DefaultStyleSettings);
      end;
      CloseKey;
      //
      OpenKey(FormatToName(sfWebVTT, True));
      with Subtitles.FormatProperties^.WebVTT do
      begin
        WriteCueIdentifiers := GetValue('WriteCueIdentifiers', WriteCueIdentifiers);
        UseXTIMESTAMP       := GetValue('UseXTIMESTAMP', UseXTIMESTAMP);
        MPEGTS              := GetValue('MPEGTS', MPEGTS);
        LOCAL               := GetValue('LOCAL', LOCAL);
      end;
      CloseKey;
      //
    CloseKey;
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure SaveSettings;
var
  i: Integer;
begin
  with TXMLConfig.Create(NIL) do
  try
    FileName := SettingsFileName;
    //Clear;
    RootName := RootCfg;
    with AppOptions do
    begin
      OpenKey('Settings');
      SetValue('Language', Language);
      SetValue('HunspellLanguage', HunspellLanguage);
      SetValue('ShortCutFile', ShortCutPreset);
      SetValue('ShowWelcomeAtStartup', ShowWelcomeAtStartup);
      SetValue('AutoBackupSeconds', AutoBackupSeconds);
      SetValue('ColorThemeMode', Integer(ColorThemeInstance.ColorMode));
      SetValue('AutoCheckErrors', AutoCheckErrors);
      SetValue('DefChangePlayRate', DefChangePlayRate);
      SetValue('DialogSegmentThreshold', DialogSegmentThreshold);
      SetValue('ShiftTimeMS', ShiftTimeMS);
      SetValue('AutoLengthChar', AutoLengthChar);
      SetValue('AutoLengthWord', AutoLengthWord);
      SetValue('AutoLengthLine', AutoLengthLine);
      SetValue('ExpandMs', ExpandMs);
      SetValue('ExpandChar', ExpandChar);
      SetValue('ExpandLen', ExpandLen);
      SetValue('UseOwnFileDialog', UseOwnFileDialog);
      SetValue('AskForDeleteLines', AskForDeleteLines);
      SetValue('AskForInputFPS', AskForInputFPS);
      SetValue('CheckErrorsBeforeSave', CheckErrorsBeforeSave);
      SetValue('WebSearchURL', WebSearchURL);
      SetValue('TextToFind', TextToFind);
      SetValue('CommonErrors', Integer(CommonErrors));
      SetValue('ShowCPSBar', frmMain.mmoText.CPSBar.Visible);
      CloseKey;

      with Conventions do
      begin
        OpenKey('CommonErrorsCfg');
        SetValue('Profile', Name);
        SetValue('RepeatableChars', RepeatableChars);
        SetValue('ProhibitedChars', ProhibitedChars);
        SetValue('MaxLines', MaxLines);
        SetValue('MaxDuration', MaxDuration);
        SetValue('MinDuration', MinDuration);
        SetValue('MinDurationPerWord', MinDurationPerWord);
        SetValue('MinPause', MinPause);
        SetValue('PauseInFrames', PauseInFrames);
        SetValue('MaxCPS', MaxCPS);
        SetValue('RepeatedTolerance', RepeatedTolerance);
        SetValue('WPM', WPM);
        SetValue('CPL', CPL);
        SetValue('CPSLineLenStrategy', CPSLineLenStrategy);
        SetValue('NewSubtitleMS', NewSubtitleMS);
        SetValue('ShotcutSnapArea', ShotcutSnapArea);
        SetValue('ShotcutThreshold', ShotcutThreshold);
        SetValue('ShotcutInCues', ShotcutInCues);
        SetValue('ShotcutOutCues', ShotcutOutCues);
        SetValue('Chaining', Chaining);
        SetValue('DotsOnSplit', DotsOnSplit);
        CloseKey;
      end;

      with Workspace, frmMain do
      begin
        OpenKey('Workspace');
        SetValue('ViewMode', Integer(ViewMode));
        SetValue('WorkMode', Integer(WorkMode));
        SetValue('TranslatorMode', TranslatorMode);
        SetValue('DefFPS', SingleToStr(FPS.InputFPS, FormatSettings));
        SetValue('DefEncoding', DefEncoding);
        SetValue('DefFormat', Integer(DefFormat));
        SetValue('VideoPreview', actVideoPreview.Checked);
        SetValue('AudioPreview', actWaveformPreview.Checked);
        SetValue('DockVideoControls', actUnDockVideo.Checked);
        SetValue('DockAudioControls', actUnDockWaveform.Checked);
        SetValue('SwapWorkspace', actSwapWorkspace.Checked);
        CloseKey;
      end;

      with VSTOptions, frmMain do
      begin
        OpenKey('VST');
        SetValue('DrawErrors', DrawErrors);
        SetValue('DrawTags', DrawTags);
        SetValue('DrawMode', Integer(DrawMode));
        SetValue('ColumnNumber', actShowColumnNumber.Checked);
        SetValue('ColumnTimes', actShowColumnTimes.Checked);
        SetValue('ColumnDuration', actShowColumnDuration.Checked);
        SetValue('ColumnStyleAndActor', actShowColumnStyleAndActor.Checked);
        SetValue('ColumnCPS', actShowColumnCPS.Checked);
        SetValue('ColumnWPM', actShowColumnWPM.Checked);
        SetValue('ColumnCPL', actShowColumnCPL.Checked);
        CloseKey;
      end;

      with MPVOptions, frmMain do
      begin
        OpenKey('MPV');
        SetValue('SubtitleHandleByMPV', SubtitleHandleByMPV);
        SetValue('AutoStartPlaying', AutoStartPlaying);
        SetValue('SubtitleToShow', Integer(SubtitleToShow));
        SetValue('UpdateListOnPreview', actMediaAutoScroll.Checked);
        SetValue('MediaChangePlayRate', actMediaChangePlayRate.Checked);
        SetValue('ShowActorOnPreview', actShowActorOnPreview.Checked);
        SetValue('TextBorderColor', TextBorderColor);
        SetValue('TextColor', TextColor);
        SetValue('TextShadowColor', TextShadowColor);
        SetValue('TextBackgroundColor', TextBackgroundColor);
        SetValue('UseTextShadowColor', UseTextShadowColor);
        SetValue('UseTextBackgroundColor', UseTextBackgroundColor);
        SetValue('TextPosition', TextPosition);
        SetValue('TextSize', TextSize);
        SetValue('TextShadowOffset', TextShadowOffset);
        SetValue('RendererMode', Integer(MPV.RendererMode));
        SetValue('SeekTime', SeekTime);
        SetValue('FrameStep', FrameStep);
        SetValue('Volume', Volume.Percent);
        SetValue('Mute', Volume.Mute);
        CloseKey;
      end;

      with WAVEOptions do
      begin
        OpenKey('WAVE');
        SetValue('LoopCount', LoopCount);
        SetValue('ViewShotChanges', frmMain.actViewShotChange.Checked);
        SetValue('CenterWaveform', frmMain.actCenterWaveform.Checked);
        SetValue('DrawGAP', frmMain.WAVE.DrawGAP);
        CloseKey;
      end;

      with Tools do
      begin
        OpenKey('Tools');
        SetValue('FFmpeg', FFmpeg);
        SetValue('FFmpeg_ParamsForAudioExtract', FFmpeg_ParamsForAudioExtract);
        SetValue('FFmpeg_ParamsForShotChanges', FFmpeg_ParamsForShotChanges);
        SetValue('FFmpeg_ParamsForWhisper', FFmpeg_ParamsForWhisper);
        SetValue('PySceneDetect', PySceneDetect);
        SetValue('PySceneDetect_Params', PySceneDetect_Params);
        SetValue('WhisperCPP', WhisperCPP);
        SetValue('WhisperCPP_Params', WhisperCPP_Params);
        SetValue('FasterWhisper', FasterWhisper);
        SetValue('FasterWhisper_Params', FasterWhisper_Params);
        SetValue('WhisperEngine', Integer(WhisperEngine));
        SetValue('YTDLP', YTDLP);
        CloseKey;
      end;

      with frmMain do
      begin
        OpenKey(Name);
        SetValue('X', Left);
        SetValue('Y', Top);
        SetValue('W', Width);
        SetValue('H', Height);
        SetValue('VW', LayoutVideo.Width);
        SetValue('WF', LayoutWaveform.Height);
        SetValue('State', Integer(WindowState));
        CloseKey;

        OpenKey('ToolBars');
          OpenKey('Main');
          for i := 0 to ToolBarMain.ButtonCount-1 do
            SetValue('id'+i.ToString, ToolBarMain.Buttons[i].Visible);
          CloseKey;
          OpenKey('Editor');
          for i := 0 to ToolBarEditor.ButtonCount-1 do
            SetValue('id'+i.ToString, ToolBarEditor.Buttons[i].Visible);
          CloseKey;
          OpenKey('Video');
          for i := 0 to ToolBarVideo.ButtonCount-1 do
            SetValue('id'+i.ToString, ToolBarVideo.Buttons[i].Visible);
          CloseKey;
          OpenKey('Waveform');
          for i := 0 to ToolBarWaveform.ButtonCount-1 do
            SetValue('id'+i.ToString, ToolBarWaveform.Buttons[i].Visible);
          CloseKey;
        CloseKey;

        OpenKey('CoolBars');
        for i := 0 to CoolBarMain.Bands.Count-1 do
        begin
          OpenKey('Id'+CoolBarMain.Bands[i].ID.ToString);
          SetValue('Break', CoolBarMain.Bands[i].Break);
          SetValue('Visible', CoolBarMain.Bands[i].Visible);
          SetValue('Width', CoolBarMain.Bands[i].Width);
          SetValue('Pos', i);
          CloseKey;
        end;
        SetValue('Main', actShowToolBarMain.Checked);
        SetValue('FPS', actShowToolBarFPS.Checked);
        SetValue('Format', actShowToolBarFormat.Checked);
        SetValue('Encoding', actShowToolBarEncoding.Checked);
        CloseKey;
      end;
    end;
    // Format properties
    OpenKey('FormatProperties');
      //
      OpenKey(FormatToName(sfAdvancedSubtitles, True));
      with Subtitles.FormatProperties^.AdvancedSubtitles do
      begin
        SetValue('Language', Language);
        SetValue('FontName', FontName);
        SetValue('FontSize', FontSize);
        SetValue('FontColor', FontColor);
        SetValue('X', X);
        SetValue('Y', Y);
        SetValue('W', W);
        SetValue('H', H);
        SetValue('Alignment', Alignment);
      end;
      CloseKey;
      //
      OpenKey(FormatToName(sfCavena890, True));
      with Subtitles.FormatProperties^.Cavena890 do
      begin
        SetValue('TapeNumber', TapeNumber);
        SetValue('TranslatedTitle', TranslatedTitle);
        SetValue('Translator', Translator);
        SetValue('TranslatedEpisode', TranslatedEpisode);
        SetValue('Comments', Comments);
        SetValue('PrimaryFont', PrimaryFont);
        SetValue('OriginalTitle', OriginalTitle);
        SetValue('SecondaryFont', SecondaryFont);
        SetValue('Producer', Producer);
        SetValue('EpisodeTitle', EpisodeTitle);
        SetValue('PrimaryLanguage', PrimaryLanguage);
      end;
      CloseKey;
      //
      OpenKey(FormatToName(sfDVDSubtitle, True));
      with Subtitles.FormatProperties^.DVDSubtitle do
      begin
        SetValue('Assigned', Assigned);
        SetValue('DiskId', DiskId);
        SetValue('DVDTitle', DVDTitle);
        SetValue('Language', Language);
        SetValue('Author', Author);
        SetValue('Web', Web);
        SetValue('Info', Info);
        SetValue('License', License);
      end;
      CloseKey;
      //
      OpenKey(FormatToName(sfEBU, True));
      with Subtitles.FormatProperties^.EBU do
      begin
        SetValue('DiskFormatCode', DiskFormatCode);
        SetValue('CodePageNumber', CodePageNumber);
        SetValue('DisplayStandardCode', DisplayStandardCode);
        SetValue('CharCodeTableNumber', CharCodeTableNumber);
        SetValue('LanguageCode', LanguageCode);
        SetValue('CountryOrigin', CountryOrigin);
        SetValue('MaxNumberDisplayableChars', MaxNumberDisplayableChars);
        SetValue('MaxNumberDisplayableRows', MaxNumberDisplayableRows);
      end;
      CloseKey;
      //
      OpenKey(FormatToName(sfAdvancedSubStationAlpha, True));
      with Subtitles.FormatProperties^.SSA do
      begin
        SetValue('DefaultStyleSettings', DefaultStyleSettings);
      end;
      CloseKey;
      //
      OpenKey(FormatToName(sfWebVTT, True));
      with Subtitles.FormatProperties^.WebVTT do
      begin
        SetValue('WriteCueIdentifiers', WriteCueIdentifiers);
        SetValue('UseXTIMESTAMP', UseXTIMESTAMP);
        SetValue('MPEGTS', MPEGTS);
        SetValue('LOCAL', LOCAL);
      end;
      CloseKey;
      //
    CloseKey;
    //
    Flush;
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

function LoadFormSettings(const AForm: TForm): String;
var
  ws: TWindowState;
begin
  if not FileExists(SettingsFileName) then Exit;

  with TXMLConfig.Create(NIL) do
  try
    FileName := SettingsFileName;
    with AForm do
    begin
      OpenKey(Name);
      ws := TWindowState(GetValue('State', Integer(WindowState)));
      if ws = wsNormal then
        SetBounds(GetValue('X', Left), GetValue('Y', Top), GetValue('W', Width), GetValue('H', Height))
      else
        AForm.WindowState := ws;

      Result := GetValue('Extra', '');
      CloseKey;
    end;
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure SaveFormSettings(const AForm: TForm; const AExtra: String = '');
begin
  with TXMLConfig.Create(NIL) do
  try
    FileName := SettingsFileName;
    RootName := RootCfg;
    with AForm do
    begin
      OpenKey(Name);
      SetValue('X', Left);
      SetValue('Y', Top);
      SetValue('W', Width);
      SetValue('H', Height);
      SetValue('State', Integer(WindowState));
      SetValue('Extra', AExtra);
      CloseKey;
    end;
    Flush;
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure LoadActors;
begin
  with frmMain.cboActor do
    if FileExists(ActorsFileName) then
    try
      Items.LoadFromFile(ActorsFileName);
    except
    end;
end;

// -----------------------------------------------------------------------------

procedure SaveActors;
begin
  with frmMain.cboActor do
    if Items.Count > 0 then
    begin
      try
        Items.SaveToFile(ActorsFileName);
      except
      end;
    end
    else if FileExists(ActorsFileName) then
      DeleteFile(ActorsFileName);
end;

// -----------------------------------------------------------------------------

procedure UpdateValuesFromDefaultConvention;
var
  FProfiles: TProfiles;
  i: Integer;
begin
  FProfiles := TProfiles.Create(ConventionsFileName);
  try
    i := FProfiles.FindItemIndex(AppOptions.Conventions.Name);
    if i >= 0 then
      with AppOptions, FProfiles.Items[i]^ do
      begin
        Conventions.RepeatableChars    := RepeatableChars;
        Conventions.ProhibitedChars    := ProhibitedChars;
        Conventions.NewSubtitleMS      := NewSubtitleMS;
        Conventions.MinDuration        := MinDuration;
        Conventions.MinDurationPerWord := MinDurationPerWord;
        Conventions.MaxDuration        := MaxDuration;
        Conventions.MaxLines           := MaxLines;
        Conventions.MinPause           := MinPause;
        Conventions.PauseInFrames      := PauseInFrames;
        Conventions.MaxCPS             := MaxCPS;
        Conventions.WPM                := WPM;
        Conventions.CPL                := CPL;
        Conventions.ShotcutSnapArea    := ShotcutSnapArea;
        Conventions.ShotcutThreshold   := ShotcutThreshold;
        Conventions.ShotcutInCues      := ShotcutInCues;
        Conventions.ShotcutOutCues     := ShotcutOutCues;
        Conventions.Chaining           := Chaining;
        Conventions.DotsOnSplit        := DotsOnSplit;
        Conventions.AddHyphenSpace     := AddHyphenSpace;
      end;
  finally
    FProfiles.Free;
  end;
end;

// -----------------------------------------------------------------------------

{ Languages }

// -----------------------------------------------------------------------------

procedure LoadLanguage(const AForm: TForm; const ACommonControls: Boolean = True);
{$IFNDEF WINDOWS}
var
  C: TComponent;
{$ENDIF}
begin
  with LanguageManager do
    ApplyLanguage(GetLangIndexByName(AppOptions.Language), AForm);

  if ACommonControls then
    ApplyCommonControlsString(AForm);

  {$IFNDEF WINDOWS}
  for C in AForm do
    if (C is TUWCheckBox) then
      (C as TUWCheckBox).AutoSize := True
    else if (C is TUWRadioButton) then
      (C as TUWRadioButton).AutoSize := True;
  {$ENDIF}

  if AForm = frmMain then
    frmMain.actAbout.Caption := Format(frmMain.actAbout.Caption, [ProgramName]);
end;

// -----------------------------------------------------------------------------

function GetLangString(const S: String): String;
begin
  with LanguageManager do
    Result := GetAppString(S);
end;

// -----------------------------------------------------------------------------

function GetCommonString(const S: String; const ASection: String = 'CommonStrings'): String;
var
  sl: TAppStringList = NIL;
begin
  if LanguageManager.GetAppStringList(ASection, sl) then
  begin
    try
      Result := GetString(sl, S)
    finally
      sl.Free;
    end;
  end
  else
    Result := '';
end;

// -----------------------------------------------------------------------------

procedure UpdateCommonActionString;
var
  sl: TAppStringList = NIL;
begin
  if LanguageManager.GetAppStringList('CommonStrings', sl) then
  try
    with frmMain do
    begin
      actShiftTimeMore.Caption := Format(GetString(sl, 'ShiftTimeMore'), [AppOptions.ShiftTimeMS]);
      actShiftTimeLess.Caption := Format(GetString(sl, 'ShiftTimeLess'), [AppOptions.ShiftTimeMS]);
    end;
  finally
    sl.Free;
  end;
end;

// -----------------------------------------------------------------------------

function GetRandomTipString: String;
var
  sl: TAppStringList = NIL;
  rndTip: Integer;
begin
  if LanguageManager.GetAppStringList('TipStrings', sl) then
  begin
    try
      rndTip := Random(sl.Count)+1;
      Result := GetString(sl, 'Tip' + rndTip.ToString);
      with frmMain do
        case rndTip of
          1: Result := Format(Result, [ShortCutToTextEx(actPreviousSubtitle.ShortCut), ShortCutToTextEx(actNextSubtitle.ShortCut)]);
          2: Result := Format(Result, [ShortCutToTextEx(actWebReference.ShortCut)]);
          3: Result := Format(Result, [ShortCutToTextEx(actUnDockVideo.ShortCut)]);
          4: Result := Format(Result, [ShortCutToTextEx(actUnDockWaveform.ShortCut)]);
        end;
      SetStatusBarText(Result);
    finally
      sl.Free;
    end;
  end
  else
    Result := '';
end;

// -----------------------------------------------------------------------------

procedure ApplyCommonControlsString(const AForm: TForm; const ASection: String = 'CommonControls');
var
  sl : TAppStringList = NIL;
  i  : Integer;
  s  : String;

  procedure CheckControl(const ACtrl: TControl);
  var
    c: Integer;
  begin
    if ACtrl is TUWLayout then
    begin
      with (ACtrl as TUWLayout) do
      for c := 0 to ControlCount-1 do
        CheckControl(Controls[c]);
    end
    else
    begin
      s := GetString(sl, ACtrl.Name);
      if s <> '' then
        ACtrl.Caption := s;
    end;
  end;

begin
  if LanguageManager.GetAppStringList(ASection, sl) then
    try
      for i := 0 to AForm.ControlCount-1 do
        CheckControl(AForm.Controls[i]);
    finally
      sl.Free;
    end;
end;

// -----------------------------------------------------------------------------

{ Special folders }

// -----------------------------------------------------------------------------

function GetCustomFolderPath(const SubFolder: String): String;
begin
  {$IFDEF DARWIN}
  Result := IncludeTrailingPathDelimiter(ConcatPaths([NSStringToString(NSBundle.mainBundle.bundlePath), 'Contents', SubFolder]));
  {$ELSE}
  Result := IncludeTrailingPathDelimiter(ConcatPaths([ExtractFilePath(ParamStr(0)), SubFolder]));
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function GetCustomUserPath(const AFolder: String): String;
begin
  {$IFDEF DARWIN}
  Result := Format('%s%s/%s/', [GetUserDir, RootCfg, AFolder]);
  {$ELSE}
  Result := GetCustomFolderPath(AFolder);
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function GetCustomFilePath(const FileName: String): String;
begin
  {$IFDEF DARWIN}
  Result := ConcatPaths([NSStringToString(NSBundle.mainBundle.bundlePath), 'Contents', 'Frameworks', FileName]);
  {$ELSE}
  Result := ConcatPaths([ExtractFilePath(ParamStr(0)), FileName]);
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function SettingsFileName: String;
begin
  {$IFDEF DARWIN}
  Result := GetUserDir + RootCfg+'/'+RootCfg+'.cfg';
  {$ELSE}
  Result := GetCustomFilePath(RootCfg+'.cfg');
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function CurrentWorkFileName: String;
begin
  {$IFDEF DARWIN}
  Result := GetUserDir + RootCfg+'/'+RootCfg+'.wsf';
  {$ELSE}
  Result := GetCustomFilePath(RootCfg+'.wsf');
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function LanguageFolder: String;
begin
  {$IFDEF DARWIN}
  Result := GetCustomFolderPath('Resources/Languages/');
  {$ELSE}
  Result := GetCustomFolderPath('Languages');
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function LanguageFileName: String;
begin
  Result := LanguageFolder + AppOptions.Language + '.lng';
end;

// -----------------------------------------------------------------------------

function ShortCutFolder: String;
begin
  {$IFDEF DARWIN}
  Result := GetCustomFolderPath('Resources/ShortCuts/');
  {$ELSE}
  Result := GetCustomFolderPath('ShortCuts');
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function ShortCutFileName: String;
begin
  Result := ConcatPaths([ShortCutFolder, AppOptions.ShortCutPreset]);
end;

// -----------------------------------------------------------------------------

function OCRFolder: String;
begin
  {$IFDEF DARWIN}
  Result := GetCustomFolderPath('Resources/OCR/');
  {$ELSE}
  Result := GetCustomFolderPath('OCR');
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function IconsFolder: String;
begin
  {$IFDEF DARWIN}
  Result := GetCustomFolderPath('Resources/Icons/');
  {$ELSE}
  Result := GetCustomFolderPath('Icons');
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function ExtensionsFolder: String;
begin
  {$IFDEF DARWIN}
  Result := GetCustomFolderPath('Resources/Extensions/');
  {$ELSE}
  Result := GetCustomFolderPath('Extensions');
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function WaveformsFolder: String;
begin
  Result := GetCustomUserPath('Waveforms');
end;

// -----------------------------------------------------------------------------

function ShotChangesFolder: String;
begin
  Result := GetCustomUserPath('ShotChanges');
end;

// -----------------------------------------------------------------------------

function MRUFileName: String;
begin
  {$IFDEF DARWIN}
  Result := GetUserDir + RootCfg+'/'+RootCfg+'.mru';
  {$ELSE}
  Result := GetCustomFilePath(RootCfg+'.mru');
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function ConventionsFileName: String;
begin
  {$IFDEF DARWIN}
  Result := GetCustomFolderPath('Resources') + RootCfg+'.cfs';
  {$ELSE}
  Result := GetCustomFilePath(RootCfg+'.cfs');
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function StylesFileName: String;
begin
  {$IFDEF DARWIN}
  Result := GetUserDir + RootCfg+'/'+RootCfg+'.sts';
  {$ELSE}
  Result := GetCustomFilePath(RootCfg+'.sts');
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function ActorsFileName: String;
begin
  {$IFDEF DARWIN}
  Result := GetUserDir + RootCfg+'/'+RootCfg+'.act';
  {$ELSE}
  Result := GetCustomFilePath(RootCfg+'.act');
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function HunspellFileName: String;
begin
  Result := GetCustomFilePath(HunspellInstance.GetCorrectLibrayFileName);
end;

// -----------------------------------------------------------------------------

function libMPVFileName(const AFullRoot: Boolean = True): String;
begin
  if AFullRoot then
    {$IFDEF LINUX}
    Result := GetInstallFolder(LIBMPV_DLL_NAME)
    {$ELSE}
    Result := ConcatPaths([libmpvFolder, LIBMPV_DLL_NAME])
    {$ENDIF}
  else
    Result := LIBMPV_DLL_NAME;
end;

// -----------------------------------------------------------------------------

function YTDLPFileName(const AFullRoot: Boolean = True): String;
begin
  if AFullRoot then
    Result := Tools.YTDLP
  else
    Result := ExtractFileName(Tools.YTDLP);
end;

// -----------------------------------------------------------------------------

function ffmpegFileName: String;
begin
  Result := Tools.FFmpeg;
end;

// -----------------------------------------------------------------------------

function SceneDetectFileName: String;
begin
  Result := Tools.PySceneDetect;
end;

// -----------------------------------------------------------------------------

function LogMPVFileName: String;
begin
  {$IFDEF DARWIN}
  Result := GetUserDir + RootCfg+'/'+RootCfg+'_mpv.log';
  {$ELSE}
  Result := GetCustomFilePath(RootCfg+'_mpv.log');
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function MPVTempSubFileName: String;
begin
  Result := ConcatPaths([GetTempDir, '_tssubtmp.ass']);
end;

// -----------------------------------------------------------------------------

function WebPreviewTempFileName: String;
begin
  Result := ConcatPaths([GetTempDir, '_tswebprevtmp.html']);
end;

// -----------------------------------------------------------------------------

function DictionariesFolder: String;
begin
  {$IFDEF DARWIN}
  Result := GetCustomFolderPath('Resources/Dictionaries/');
  {$ELSE}
  Result := GetCustomFolderPath('Dictionaries');
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function BackupFolder: String;
begin
  Result := GetCustomUserPath('Backup');
end;

// -----------------------------------------------------------------------------

function ProjectsFolder: String;
begin
  Result := GetCustomUserPath('Projects');
end;

// -----------------------------------------------------------------------------

function CustomFormatFolder: String;
begin
  {$IFDEF DARWIN}
  Result := GetCustomFolderPath('Resources/CustomFormat/');
  {$ELSE}
  Result := GetCustomFolderPath('CustomFormat');
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function TranslationMemoryFolder: String;
begin
  Result := GetCustomUserPath('TM');
end;

// -----------------------------------------------------------------------------

function TerminologyFolder: String;
begin
  Result := GetCustomUserPath('Terminology');
end;

// -----------------------------------------------------------------------------

function WhisperFolder: String;
begin
  Result := GetCustomUserPath('Whisper');
end;

// -----------------------------------------------------------------------------

function WhisperFileName: String;
begin
  Result := Tools.WhisperCPP;
end;

// -----------------------------------------------------------------------------

function WhisperModelsFolder: String;
begin
  Result := IncludeTrailingPathDelimiter(ConcatPaths([WhisperFolder, 'Models']));
end;

// -----------------------------------------------------------------------------

function WhisperTranscriptionsFolder: String;
begin
  Result := IncludeTrailingPathDelimiter(ConcatPaths([WhisperFolder, 'Transcriptions']));
end;

// -----------------------------------------------------------------------------

function libmpvFolder: String;
begin
  {$IFDEF DARWIN}
  Result := GetCustomFolderPath('Frameworks');
  //Result := '/usr/local/lib/';
  {$ELSE}
  Result := GetCustomFolderPath('');
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function YTDLPFolder: String;
begin
  {$IFDEF DARWIN}
  Result := GetCustomUserPath('ytdlp');
  {$ELSE}
  Result := libmpvFolder;
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function ffmpegFolder: String;
begin
  {$IFDEF DARWIN}
  Result := GetCustomUserPath('ffmpeg');
  {$ELSE}
  Result := GetCustomFolderPath('ffmpeg');
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function GetDictionaryNameFromCaption(const AText: String): String;
begin
  with TRegExpr.Create do
  try
    Expression := '\[(.*?)\]';
    Exec(AText);
    Result := Match[1];
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

function GetExtractAppFile(const FFmpeg: Boolean = True): String;
begin
  if FFmpeg then
    Result := Tools.FFmpeg
  else
    Result := Tools.PySceneDetect;
end;

// -----------------------------------------------------------------------------

function GetAudioToTextAppFile: String;
begin
  if Tools.WhisperEngine = TWhisperEngine.WhisperCPP then
    Result := Tools.WhisperCPP
  else
    Result := Tools.FasterWhisper;
end;

// -----------------------------------------------------------------------------

function GetAudioToTextParams: String;
begin
  if Tools.WhisperEngine = TWhisperEngine.WhisperCPP then
    Result := Tools.WhisperCPP_Params
  else
    Result := Tools.FasterWhisper_Params;
end;

// -----------------------------------------------------------------------------

{$IFDEF UNIX}
function GetInstallFolder(const AFileName: String): String;
const
  {$IFDEF LINUX}
  pathLst : array[0..8] of string = (
    '/usr/bin',
    '/bin',
    '/usr/local/bin',
    '/usr/lib',
    '/lib',
    '/usr/local/lib',
    '/lib64',
    '/usr/lib64',
    '/usr/lib/x86_64-linux-gnu'
  );
  {$ELSE}
  pathLst : array[0..3] of string = (
    '/usr/local/bin',
    '/opt/local/bin',
    '/usr/local/lib',
    '/opt/local/lib'
    );
  {$ENDIF}
var
  pathIdx : Integer;
  pathStr : string;
  sr      : TSearchRec;
  re      : Integer;
begin
  for pathIdx := Low(pathLst) to High(pathLst) do
  begin
    pathStr := pathLst[pathIdx];
    if not DirectoryExists(pathStr) then continue;
    // look for file
    if FileExists(pathStr + PathDelim + AFileName) then
    begin
      Result := pathStr + PathDelim + AFileName;
      Exit;
    end;
    {$IFDEF LINUX}
    // look for .so.x
    re := SysUtils.FindFirst(pathStr + PathDelim + AFileName + '.*', faAnyFile, sr);
    {$ELSE}
    // look for x.dylib
    re := SysUtils.FindFirst(pathStr + PathDelim + ChangeFileExt(AFileName, '') + '.*.dylib', faAnyFile, sr);
    {$ENDIF}
    FindClose(sr);
    if (re = 0) then
    begin
      Result := pathStr + PathDelim + sr.Name;
      Exit;
    end;
  end;

  {$IFDEF DARWIN}
  Result := '/Applications/' + AFileName + '.app/Contents/MacOS/' + AFileName;
  if FileExists(Result) then
    Exit
  else
  begin
    Result := '~' + Result;
    if FileExists(Result) then
      Exit;
  end;
  {$ENDIF}

  Result := '';
end;
{$ENDIF}

// -----------------------------------------------------------------------------

{ Check for updates}

// -----------------------------------------------------------------------------

procedure CheckForUpdates;
var
  sNew, sOld: String;
  FileVerInfo: TFileVersionInfo;
begin
  with TDownloader.Create do
  try
    if DownloadToString(ProgramUpdateURL, sNew) then
    begin
      FileVerInfo := TFileVersionInfo.Create(NIL);
      try
        FileVerInfo.ReadFileInfo;
        sOld := FileVerInfo.VersionStrings.Values['FileVersion'];
        if (sNew.Trim > sOld.Trim) then // New version available
        begin
          if CustomQuestionDialog('CommonStrings', 'NewVersionFound', 'SeeChangeList', [dbYes, dbNo]) = mrYes then
            OpenURL(ProgramReleaseURL);
        end
        else // you're using latest version
          ShowMessageDialog(GetCommonString('NoNewVersion'));
      finally
        FileVerInfo.Free;
      end;
    end
    else // no internet or file lost?
      ShowErrorMessageDialog(GetCommonString('FailedToDownload'));
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

end.

