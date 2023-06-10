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

unit procCommon;

{$mode ObjFPC}{$H+}
{$IFDEF DARWIN}
{$modeswitch objectivec1}
{$ENDIF}

interface

uses
  Classes, Controls, SysUtils, Menus, Forms, procTypes, UWSubtitleAPI,
  formCustomQuestionDlg, formCustomMessageDlg
  {$IFNDEF WINDOWS}, UWCheckBox, UWRadioButton{$ENDIF}
  {$IFDEF DARWIN}, CocoaAll, CocoaUtils{$ENDIF};

{ Settings }

procedure DefaultValues;
procedure LoadSettings;
procedure SaveSettings;

procedure LoadFormSettings(const AForm: TForm);
procedure SaveFormSettings(const AForm: TForm);

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
function GetCustomFilePath(const FileName: String): String;
function SettingsFileName: String;
function CurrentWorkFileName: String;
function LanguageFolder: String;
function LanguageFileName: String;
function ShortCutFolder: String;
function ShortCutFileName: String;
function OCRFolder: String;
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
function LogMPVFileName: String;
function DictionariesFolder: String;
function BackupFolder: String;
function ProjectsFolder: String;
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
function GetExtractAppFile: String;
function GetAudioToTextAppFile: String;
{$IFDEF UNIX}
function GetInstallFolder(const AFileName: String): String;
{$ENDIF}

{ Custom inputbox dialog }

function InputDialog(const ACaption, APrompt, ADefault: String; const AHeight: Integer = 93): String;

{ Custom message dialogs }

procedure ShowErrorMessageDialog(const AMessage: String; const ACaption: String = '');
procedure ShowMessageDialog(const AMessage: String; const ACaption: String = '');

{ Custom question dialogs }

function CustomQuestionDialog(const ASectionName, Title, Caption: String; const AButtons: TCustomDlgButtons = []): Integer;
function MsgSaveSubtitle(FileName: String; const ASubtitleMode: TSubtitleMode = smText): Integer;
function MsgDeleteFiles: Integer;
function MsgFolderNotEmpty: Integer;
function MsgExportTextOnlyFormat: Integer;

{ Check for updates}

procedure CheckForUpdates;

// -----------------------------------------------------------------------------

implementation

uses
  RegExpr, formMain, XMLConf, UWSystem.Encoding, UWSystem.SysUtils,
  UWSubtitleAPI.Formats, UWLayout, UWSystem.XMLLang, MPVPlayer, procWorkspace,
  procColorTheme, LCLProc, formCustomInputDlg, UWSystem.Globalization,
  UWSystem.TimeUtils, libMPV.Client, UWSpellcheck.Hunspell, procConventions,
  UWSystem.InetUtils, fileinfo, winpeimagereader, elfreader, machoreader,
  LCLIntf;

// -----------------------------------------------------------------------------

{ Settings }

// -----------------------------------------------------------------------------

procedure DefaultValues;
begin
  LastTickCount := 0;

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
    LoopCount         := 3;
    ExtractApp        := FFMPEG_EXE;
    ExtractParams     := FFMPEG_Params;
    AudioToTextApp    := WHISPER_EXE;
    AudioToTextParams := WHISPER_Params;
    {$IFDEF WINDOWS}
    if DirectoryExists('c:\ffmpeg\bin') then
      ffmpegFolder := 'c:\ffmpeg\bin';
    {$ELSE}
    ffmpegFolder := ExtractFilePath(GetInstallFolder(FFMPEG_EXE));
    {$ENDIF}
  end;

  FillByte(Workspace, SizeOf(TWorkspace), 0);
  with Workspace do
  begin
    FPS.DefFPS    := 23.976;
    FPS.OutputFPS := FPS.DefFPS;
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
      ProhibitedChars    := '@,http,www,#,*';
      MaxLines           := 2;
      MaxDuration        := 8000;
      MinDuration        := 1000;
      MinDurationPerWord := 300;
      MinPause           := 200;
      PauseInFrames      := False;
      MaxCPS             := 15;
      RepeatedTolerance  := 100;
      WPM                := 180;
      CPL                := 37;
      CPSLineLenStrategy := '';
      NewSubtitleMS      := 1000;
      DotsOnSplit        := True;

      frmMain.mmoText.CPSBar.Max := MaxCPS;
    end;

    CommonErrors := [etBadValues, etTimeTooLong, etTimeTooShort, etPauseTooShort,
      etMaxCPS, etOverlapping, etFixTags, etEmpty, etUnbreak, etUnnecessarySpaces,
      etUnnecessaryDots, etRepeatedChars, etProhibitedChars, etHearingImpaired,
      etBreakLongLines, etRepeatedSubtitle, etEllipsesSingleSmartCharacter,
      etMaxLines, etOCR];

    DefChangePlayRate    := 50;
    ShiftTimeMS          := 500;
    AutoBackupSeconds    := 300;

    AutoLengthChar       := 60;
    AutoLengthWord       := 50;
    AutoLengthLine       := 50;
    ExpandMs             := 1500;
    ExpandChar           := 40;
    ExpandLen            := 1000;

    ShowWelcomeAtStartup := True;
    UseOwnFileDialog     := False;
    AutoCheckErrors      := True;
    AskForDeleteLines    := True;
    TextToFind           := '';
    WebSearchURL         := URL_WordReference;
    Language             := GetOSLanguage;
    HunspellLanguage     := 'en_US';
    ShortCutPreset       := 'Tero.key';
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
      etlb6.Hide;
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
      ShiftTimeMS := GetValue('ShiftTimeMS', ShiftTimeMS);
      AutoLengthChar := GetValue('AutoLengthChar', AutoLengthChar);
      AutoLengthWord := GetValue('AutoLengthWord', AutoLengthWord);
      AutoLengthLine := GetValue('AutoLengthLine', AutoLengthLine);
      ExpandMs := GetValue('ExpandMs', ExpandMs);
      ExpandChar := GetValue('ExpandChar', ExpandChar);
      ExpandLen := GetValue('ExpandLen', ExpandLen);
      UseOwnFileDialog := GetValue('UseOwnFileDialog', UseOwnFileDialog);
      AskForDeleteLines := GetValue('AskForDeleteLines', AskForDeleteLines);
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
        FPS.DefFPS  := StrToSingle(GetValue('DefFPS', SingleToStr(FPS.DefFPS, FormatSettings)), 25, FormatSettings);
        DefEncoding := GetValue('DefEncoding', DefEncoding);
        DefFormat   := TUWSubtitleFormats(GetValue('DefFormat', Integer(DefFormat)));
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
        MaxLines := GetValue('MaxLines', MaxLines);
        MaxDuration := GetValue('MaxDuration', MaxDuration);
        MinDurationPerWord := GetValue('MinDurationPerWord', MinDurationPerWord);
        MinDuration := GetValue('MinDuration', MinDuration);
        PauseInFrames := GetValue('PauseInFrames', PauseInFrames);
        MinPause := GetValue('MinPause', MinPause);
        MaxCPS := GetValue('MaxCPS', MaxCPS);
        RepeatedTolerance := GetValue('RepeatedTolerance', RepeatedTolerance);
        WPM := GetValue('WPM', WPM);
        CPL := GetValue('CPL', CPL);
        CPSLineLenStrategy := GetValue('CPSLineLenStrategy', CPSLineLenStrategy);
        NewSubtitleMS := GetValue('NewSubtitleMS', NewSubtitleMS);
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
        //actShowColumnStyle.Checked := GetValue('ColumnStyle', actShowColumnStyle.Checked);
        actShowColumnCPS.Checked := GetValue('ColumnCPS', actShowColumnCPS.Checked);
        actShowColumnWPM.Checked := GetValue('ColumnWPM', actShowColumnWPM.Checked);
        actShowColumnCPL.Checked := GetValue('ColumnCPL', actShowColumnCPL.Checked);
        CloseKey;
      end;

      with MPVOptions, frmMain do
      begin
        OpenKey('MPV');
        AutoStartPlaying := GetValue('AutoStartPlaying', AutoStartPlaying);
        SubtitleToShow := TSubtitleMode(GetValue('SubtitleToShow', Integer(SubtitleToShow)));
        actMediaAutoScroll.Checked := GetValue('UpdateListOnPreview', actMediaAutoScroll.Checked);
        actMediaChangePlayRate.Checked := GetValue('MediaChangePlayRate', actMediaChangePlayRate.Checked);
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
        actSMPTE.Checked := GetValue('SMPTE', False);
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
        ExtractApp    := GetValue('ExtractApp', ExtractApp);
        ExtractParams := GetValue('ExtractParams', ExtractParams);
        AudioToTextApp    := GetValue('AudioToTextApp', AudioToTextApp);
        AudioToTextParams := GetValue('AudioToTextParams', AudioToTextParams);
        ffmpegFolder := GetValue('ffmpegFolder', ffmpegFolder);
        frmMain.WAVE.DrawGAP := GetValue('DrawGAP', False);
        frmMain.actViewShotChange.Checked := GetValue('ViewShotChanges', False);
        frmMain.actViewShotChangeExecute(NIL);
        frmMain.actCenterWaveform.Checked := GetValue('CenterWaveform', True);
        frmMain.actCenterWaveformExecute(NIL);
        CloseKey;

        if ExtractApp.IsEmpty then ExtractApp := FFMPEG_EXE;
        if ExtractParams.IsEmpty then ExtractParams := FFMPEG_Params;
        if AudioToTextApp.IsEmpty then AudioToTextApp := WHISPER_EXE;
        if AudioToTextParams.IsEmpty then AudioToTextParams := WHISPER_Params;
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
    OpenKey(FormatToName(sfWebVTT));
    Subtitles.FormatProperties^.WebVTT.WriteCueIdentifiers := GetValue('WriteCueIdentifiers', False);
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
      SetValue('ShiftTimeMS', ShiftTimeMS);
      SetValue('AutoLengthChar', AutoLengthChar);
      SetValue('AutoLengthWord', AutoLengthWord);
      SetValue('AutoLengthLine', AutoLengthLine);
      SetValue('ExpandMs', ExpandMs);
      SetValue('ExpandChar', ExpandChar);
      SetValue('ExpandLen', ExpandLen);
      SetValue('UseOwnFileDialog', UseOwnFileDialog);
      SetValue('AskForDeleteLines', AskForDeleteLines);
      SetValue('WebSearchURL', WebSearchURL);
      SetValue('TextToFind', TextToFind);
      SetValue('Profile', Name);
      SetValue('CommonErrors', Integer(CommonErrors));
      SetValue('ShowCPSBar', frmMain.mmoText.CPSBar.Visible);
      CloseKey;

      with Conventions do
      begin
        OpenKey('CommonErrorsCfg');
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
        SetValue('DotsOnSplit', DotsOnSplit);
        CloseKey;
      end;

      with Workspace, frmMain do
      begin
        OpenKey('Workspace');
        SetValue('ViewMode', Integer(ViewMode));
        SetValue('WorkMode', Integer(WorkMode));
        SetValue('TranslatorMode', TranslatorMode);
        SetValue('DefFPS', SingleToStr(FPS.DefFPS, FormatSettings));
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
        //SetValue('ColumnStyle', actShowColumnStyle.Checked);
        SetValue('ColumnCPS', actShowColumnCPS.Checked);
        SetValue('ColumnWPM', actShowColumnWPM.Checked);
        SetValue('ColumnCPL', actShowColumnCPL.Checked);
        CloseKey;
      end;

      with MPVOptions, frmMain do
      begin
        OpenKey('MPV');
        SetValue('AutoStartPlaying', AutoStartPlaying);
        SetValue('SubtitleToShow', Integer(SubtitleToShow));
        SetValue('UpdateListOnPreview', actMediaAutoScroll.Checked);
        SetValue('MediaChangePlayRate', actMediaChangePlayRate.Checked);
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
        SetValue('SMPTE', actSMPTE.Checked);
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
        SetValue('ExtractApp', ExtractApp);
        SetValue('ExtractParams', ExtractParams);
        SetValue('AudioToTextApp', AudioToTextApp);
        SetValue('AudioToTextParams', AudioToTextParams);
        SetValue('ffmpegFolder', ffmpegFolder);
        SetValue('ViewShotChanges', frmMain.actViewShotChange.Checked);
        SetValue('CenterWaveform', frmMain.actCenterWaveform.Checked);
        SetValue('DrawGAP', frmMain.WAVE.DrawGAP);
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
    // WebVTT
    OpenKey(FormatToName(sfWebVTT));
    SetValue('WriteCueIdentifiers', Subtitles.FormatProperties^.WebVTT.WriteCueIdentifiers);
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

procedure LoadFormSettings(const AForm: TForm);
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

      CloseKey;
    end;
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure SaveFormSettings(const AForm: TForm);
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
      CloseKey;
    end;
    Flush;
  finally
    Free;
  end;
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
  try
    if LanguageManager.GetAppStringList(ASection, sl) then
      Result := GetString(sl, S)
    else
      Result := '';
  finally
    sl.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure UpdateCommonActionString;
var
  sl: TAppStringList = NIL;
begin
  try
    if LanguageManager.GetAppStringList('CommonStrings', sl) then
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
  try
    if LanguageManager.GetAppStringList('TipStrings', sl) then
    begin
      rndTip := Random(sl.Count)+1;
      Result := GetString(sl, 'Tip' + rndTip.ToString);
      with frmMain do
        case rndTip of
          1: Result := Format(Result, [ShortCutToText(actPreviousSubtitle.ShortCut), ShortCutToText(actNextSubtitle.ShortCut)]);
          2: Result := Format(Result, [ShortCutToText(actWebReference.ShortCut)]);
          3: Result := Format(Result, [ShortCutToText(actUnDockVideo.ShortCut)]);
          4: Result := Format(Result, [ShortCutToText(actUnDockWaveform.ShortCut)]);
        end;
      SetStatusBarText(Result);
    end
    else
      Result := '';
  finally
    sl.Free;
  end;
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
  try
    if LanguageManager.GetAppStringList(ASection, sl) then
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
  {$IFDEF DARWIN}
  Result := GetUserDir + RootCfg+'/Waveforms/';
  {$ELSE}
  Result := GetCustomFolderPath('Waveforms');
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function ShotChangesFolder: String;
begin
  {$IFDEF DARWIN}
  Result := GetUserDir + RootCfg+'/ShotChanges/';
  {$ELSE}
  Result := GetCustomFolderPath('ShotChanges');
  {$ENDIF}
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
    {$IFDEF LINUX}
    Result := GetInstallFolder(YTDLP_EXE)
    {$ELSE}
    Result := ConcatPaths([YTDLPFolder, YTDLP_EXE])
    {$ENDIF}
  else
    Result := YTDLP_EXE;
end;

// -----------------------------------------------------------------------------

function ffmpegFileName: String;
begin
  {$IFDEF LINUX}
  Result := GetInstallFolder(FFMPEG_EXE);
  {$ELSE}
  Result := ConcatPaths([ffmpegFolder, FFMPEG_EXE]);
  {$ENDIF}
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
  {$IFDEF DARWIN}
  Result := GetUserDir + RootCfg+'/Backup/';
  {$ELSE}
  Result := GetCustomFolderPath('Backup');
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function ProjectsFolder: String;
begin
  {$IFDEF DARWIN}
  Result := GetUserDir + RootCfg+'/Projects/';
  {$ELSE}
  Result := GetCustomFolderPath('Projects');
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function TranslationMemoryFolder: String;
begin
  {$IFDEF DARWIN}
  Result := GetUserDir + RootCfg+'/TM/';
  {$ELSE}
  Result := GetCustomFolderPath('TM');
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function TerminologyFolder: String;
begin
  {$IFDEF DARWIN}
  Result := GetUserDir + RootCfg+'/Terminology/';
  {$ELSE}
  Result := GetCustomFolderPath('Terminology');
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function WhisperFolder: String;
begin
  {$IFDEF DARWIN}
  Result := GetUserDir + RootCfg+'/Whisper/';
  {$ELSE}
  Result := GetCustomFolderPath('Whisper');
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function WhisperFileName: String;
begin
  {$IFDEF LINUX}
  Result := GetInstallFolder(WHISPER_EXE);
  {$ELSE}
  Result := ConcatPaths([WhisperFolder, WHISPER_EXE]);
  {$ENDIF}
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
  Result := GetUserDir + RootCfg+'/ytdlp/';
  {$ELSE}
  Result := libmpvFolder;
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function ffmpegFolder: String;
begin
  {$IFDEF DARWIN}
  Result := GetUserDir + RootCfg+'/ffmpeg/';
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

function GetExtractAppFile: String;
begin
  Result := ConcatPaths([WAVEOptions.ffmpegFolder, WAVEOptions.ExtractApp]);
end;

// -----------------------------------------------------------------------------

function GetAudioToTextAppFile: String;
begin
  Result := ConcatPaths([WhisperFolder, WAVEOptions.AudioToTextApp]);
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

{ Custom inputbox dialog }

// -----------------------------------------------------------------------------

function InputDialog(const ACaption, APrompt, ADefault: String; const AHeight: Integer = 93): String;
begin
  with TfrmCustomInputDlg.Create(NIL) do
  try
    Result := Execute(ACaption, APrompt, ADefault, AHeight);
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

{ Custom Message dialog }

// -----------------------------------------------------------------------------

procedure ShowErrorMessageDialog(const AMessage: String; const ACaption: String = '');
begin
  with TfrmCustomMessageDlg.Create(NIL) do
  try
    Execute(AMessage, ACaption);
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure ShowMessageDialog(const AMessage: String; const ACaption: String = '');
begin
  with TfrmCustomMessageDlg.Create(NIL) do
  try
    Execute(AMessage, ACaption, imInformation);
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

{ Custom question dialogs }

// -----------------------------------------------------------------------------

function CustomQuestionDialog(const ASectionName, Title, Caption: String; const AButtons: TCustomDlgButtons = []): Integer;
var
  sl: TAppStringList = NIL;
begin
  if LanguageManager.GetAppStringList(ASectionName, sl) then
    Result := formCustomQuestionDlg.ShowQuestionDialog(GetString(sl, Title), GetString(sl, Caption), ProgramName, AButtons);
  FreeAndNil(sl);
end;

// -----------------------------------------------------------------------------

function MsgSaveSubtitle(FileName: String; const ASubtitleMode: TSubtitleMode = smText): Integer;
var
  sl : TAppStringList = NIL;
  sm : String;
begin
  if LanguageManager.GetAppStringList('CommonStrings', sl) then
  begin
    if ASubtitleMode = smText then
      sm := GetString(sl, 'FileChanged')
    else
      sm := GetString(sl, 'TranslationFileChanged');

    if FileName.IsEmpty then FileName := GetString(sl, 'NoName');
    Result := formCustomQuestionDlg.ShowQuestionDialog(sm, Format(GetString(sl, 'AskToSave'), [FileName]), ProgramName);
  end;
  FreeAndNil(sl);
end;

// -----------------------------------------------------------------------------

function MsgDeleteFiles: Integer;
begin
  Result := CustomQuestionDialog('CommonStrings', 'PromptForDeleteLines', 'ContinueAnyway', [mbYes, mbNo]);
end;

// -----------------------------------------------------------------------------

function MsgFolderNotEmpty: Integer;
begin
  Result := CustomQuestionDialog('CommonStrings', 'FolderNotEmpty', 'ContinueAnyway', [mbYes, mbNo]);
end;

// -----------------------------------------------------------------------------

function MsgExportTextOnlyFormat: Integer;
begin
  Result := CustomQuestionDialog('CommonStrings', 'TextFormatted', '', [mbYes, mbNo]);
end;

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
          if CustomQuestionDialog('CommonStrings', 'NewVersionFound', 'SeeChangeList', [mbYes, mbNo]) = mrYes then
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

