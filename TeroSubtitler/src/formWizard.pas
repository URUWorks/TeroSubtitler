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

unit formWizard;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  UWLayout, UWRadioButton, LCLTranslator, procLocalize, UWSystem.Globalization;

type

  { TfrmWizard }

  TfrmWizard = class(TForm)
    btnDownloadFFMPEG: TButton;
    btnDownloadWhisper: TButton;
    btnDownloadFasterWhisper: TButton;
    btnDownloadYTDLP: TButton;
    btnNext: TButton;
    btnDownload: TButton;
    btnPrevious: TButton;
    cboLanguage: TComboBox;
    cboTimeCode: TComboBox;
    imgLayout0: TImage;
    imgLayout1: TImage;
    imgLayout2: TImage;
    imgLogo: TImage;
    lblFasterWhisper: TLabel;
    lblFasterWhisper_Hint: TLabel;
    lblFFMPEG_Hint: TLabel;
    lblLanguage: TLabel;
    lbllibMPV_Hint: TLabel;
    lblChooseLayout: TLabel;
    lblWhisper_Hint: TLabel;
    lblWhisperStatus: TLabel;
    lblWhisper: TLabel;
    lblFasterWhisperStatus: TLabel;
    lblYTDLP_Hint: TLabel;
    lblYTDLPStatus: TLabel;
    lblYTDLP: TLabel;
    lblLibMPVStatus: TLabel;
    lblFFMPEGStatus: TLabel;
    lblFFMPEG: TLabel;
    lblRequiredFiles: TLabel;
    lbllibMPV: TLabel;
    lblExperience: TLabel;
    lblWelcome: TLabel;
    lyoPage0: TUWLayout;
    lyoPage1: TUWLayout;
    lyoPage2: TUWLayout;
    rdoLayout0: TUWRadioButton;
    rdoLayout1: TUWRadioButton;
    rdoLayout2: TUWRadioButton;
    procedure btnDownloadClick(Sender: TObject);
    procedure btnDownloadFFMPEGClick(Sender: TObject);
    procedure btnDownloadYTDLPClick(Sender: TObject);
    procedure btnDownloadWhisperClick(Sender: TObject);
    procedure btnDownloadFasterWhisperClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnPreviousClick(Sender: TObject);
    procedure cboLanguageChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FIndex: Integer;
    FLngList: TStrings;
    procedure SetPageStep(const AForward: Boolean = True);
    function CheckState_libMPV: Boolean;
    function CheckState_ytdlp: Boolean;
    function CheckState_ffmpeg: Boolean;
    function CheckState_whispercpp: Boolean;
    function CheckState_fasterwhisper: Boolean;
  public
  end;

// -----------------------------------------------------------------------------

var
  frmWizard: TfrmWizard;

implementation

uses
  procTypes, procWorkspace, procConfig, procColorTheme, formDownload,
  UWSystem.SysUtils, UWSpellcheck.Hunspell, formMain;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmWizard }

// -----------------------------------------------------------------------------

procedure TfrmWizard.FormCreate(Sender: TObject);
begin
  FLngList := TStringList.Create;
  FIndex   := 0;

  btnDownloadYTDLP.Caption   := btnDownload.Caption;
  btnDownloadFFMPEG.Caption  := btnDownload.Caption;
  btnDownloadWhisper.Caption := btnDownload.Caption;

  {$IFDEF LINUX}
  btnDownload.Visible              := False;
  btnDownloadYTDLP.Visible         := False;
  btnDownloadFFMPEG.Visible        := False;
  btnDownloadWhisper.Visible       := False;
  btnDownloadFasterWhisper.Visible := False;
  {$ENDIF};

  CheckState_libMPV;
  CheckState_ytdlp;
  CheckState_ffmpeg;
  CheckState_whispercpp;
  CheckState_fasterwhisper;

  lblLanguage.Visible := False;
  cboLanguage.Visible := False;

  AppOptions.GUILanguage := LanguageIDFromFileName(LanguageFileName(True));
  FillComboWithLanguages(cboLanguage, FLngList);
  cboTimeCode.Items.Add('');
  cboTimeCode.Items.Add('');
  cboLanguageChange(NIL);
  cboTimeCode.ItemIndex := Integer(Workspace.WorkMode);

  {$IFNDEF WINDOWS}
  PrepareCustomControls(Self);
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

procedure TfrmWizard.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i : Integer;
begin
  if rdoLayout1.Checked then
    Workspace.Layout := 1
  else if rdoLayout2.Checked then
    Workspace.Layout := 2
  else
    Workspace.Layout := 0;

  SetWorkspaceLayout(Workspace.Layout);

  if cboTimeCode.ItemIndex > 0 then
  begin
    if Workspace.WorkMode <> wmFrames then
      SetWorkMode(wmFrames);
  end
  else
  begin
    if Workspace.WorkMode <> wmTime then
      SetWorkMode(wmTime);
  end;

  if (AppOptions.HunspellLanguage <> AppOptions.GUILanguage) and FileExists(DictionariesFolder+AppOptions.GUILanguage + '.dic') and FileExists(DictionariesFolder+AppOptions.GUILanguage + '.aff') then
  begin
    AppOptions.HunspellLanguage := AppOptions.GUILanguage;
    UpdateStatusBar(True);
    HunspellInstance.LoadDictionary(DictionariesFolder+AppOptions.HunspellLanguage+'.aff', DictionariesFolder+AppOptions.HunspellLanguage+'.dic');

    with frmMain do
      for i := 0 to popDictionaries.Items.Count-1 do
        popDictionaries.Items[i].Checked := (AppOptions.HunspellLanguage = GetDictionaryNameFromCaption(popDictionaries.Items[i].Caption));
  end;

  CloseAction := caFree;
  FLngList.Free;
  frmWizard := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmWizard.FormShow(Sender: TObject);
var
  png: TPortableNetworkGraphic;
begin
  CheckColorTheme(Self);
  png := TPortableNetworkGraphic.Create;
  try
    if ColorThemeInstance.GetRealColorMode = cmDark then
    begin
      png.LoadFromLazarusResource('terosubtitler_dark');
      imgLogo.Picture.Graphic := png;
      png.LoadFromLazarusResource('guilayout0_dark');
      imgLayout0.Picture.Graphic := png;
      png.LoadFromLazarusResource('guilayout1_dark');
      imgLayout1.Picture.Graphic := png;
      png.LoadFromLazarusResource('guilayout2_dark');
      imgLayout2.Picture.Graphic := png;
    end
    else
    begin
      png.LoadFromLazarusResource('terosubtitler');
      imgLogo.Picture.Graphic := png;
      png.LoadFromLazarusResource('guilayout0');
      imgLayout0.Picture.Graphic := png;
      png.LoadFromLazarusResource('guilayout1');
      imgLayout1.Picture.Graphic := png;
      png.LoadFromLazarusResource('guilayout2');
      imgLayout2.Picture.Graphic := png;
    end;
  finally
    png.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmWizard.SetPageStep(const AForward: Boolean = True);
var
  i: Integer;
begin
  if AForward then
    Inc(FIndex)
  else
    Dec(FIndex);

  if FIndex = 3 then Close;

  for i := 0 to ControlCount-1 do
    if Controls[i] is TUWLayout then
      with TUWLayout(Controls[i]) do
      begin
        if Tag = FIndex then
          Visible := True
        else
          Visible := False
      end;

  btnPrevious.Visible := (FIndex > 0);
  lblLanguage.Visible := btnPrevious.Visible;
  cboLanguage.Visible := lblLanguage.Visible;
end;

// -----------------------------------------------------------------------------

procedure TfrmWizard.btnNextClick(Sender: TObject);
begin
  SetPageStep;
end;

// -----------------------------------------------------------------------------

procedure TfrmWizard.btnPreviousClick(Sender: TObject);
begin
  SetPageStep(False);
end;

// -----------------------------------------------------------------------------

procedure TfrmWizard.cboLanguageChange(Sender: TObject);

  procedure FixStateLang(const ALabel: TLabel);
  begin
    ALabel.Caption := iff(ALabel.Tag = 1, lngSuccess, lngFailed);
  end;

var
  langs : TStringArray;
  currIdx : Integer;
begin
  langs := FLngList.Strings[cboLanguage.ItemIndex].Split([';']);
  if Length(langs) > 0 then
    AppOptions.GUILanguage := langs[0]
  else
    AppOptions.GUILanguage := 'en_US';

  currIdx := cboTimeCode.ItemIndex;
  SetGUILanguage;

  if lngwizLanguage <> 'Language' then
    lblLanguage.Caption := 'Language / ' + lngwizLanguage
  else
    lblLanguage.Caption := lngwizLanguage;

  cboTimeCode.Items[0] := lngHHMMSSZZZ;
  cboTimeCode.Items[1] := lngHHMMSSFF;
  cboTimeCode.ItemIndex := currIdx;

  FixStateLang(lblLibMPVStatus);
  FixStateLang(lblYTDLPStatus);
  FixStateLang(lblFFMPEGStatus);
  FixStateLang(lblWhisperStatus);
  FixStateLang(lblFasterWhisperStatus);
end;

// -----------------------------------------------------------------------------

procedure TfrmWizard.btnDownloadClick(Sender: TObject);
begin
  ShowDownloadDialog(URL_LIBMPV, ConcatPaths([libmpvFolder, 'libmpv.zip']));
  CheckState_libMPV;
end;

// -----------------------------------------------------------------------------

procedure TfrmWizard.btnDownloadYTDLPClick(Sender: TObject);
begin
  {$IFDEF DARWIN}
  ShowDownloadDialog(URL_YTDLP, ConcatPaths([YTDLPFolder, 'ytdlp.zip']));
  {$ELSE}
  ShowDownloadDialog(URL_YTDLP, YTDLPFileName);
  {$ENDIF}
  if FileExists(ConcatPaths([YTDLPFolder, YTDLP_EXE])) then
  begin
    Tools.YTDLP := ConcatPaths([YTDLPFolder, YTDLP_EXE]);
    CheckState_ytdlp;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmWizard.btnDownloadFFMPEGClick(Sender: TObject);
begin
//  {$IFDEF DARWIN}
//  ShowDownloadDialog(StringReplace(URL_FFMPEG, '%cpu', GetCPUArchitectureString, []), ConcatPaths([ffmpegFolder, 'ffmpeg.zip']));
//  {$ELSE}
//  ShowDownloadDialog(URL_FFMPEG, ConcatPaths([ffmpegFolder, 'ffmpeg.zip']));
//  {$ENDIF}
  ShowDownloadDialog(URL_FFMPEG, ConcatPaths([ffmpegFolder, 'ffmpeg.zip']));
  if FileExists(ConcatPaths([ffmpegFolder, FFMPEG_EXE])) then
  begin
    Tools.FFmpeg := ConcatPaths([ffmpegFolder, FFMPEG_EXE]);
    CheckState_ffmpeg;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmWizard.btnDownloadWhisperClick(Sender: TObject);
begin
  {$IFDEF DARWIN}
  ShowDownloadDialog(StringReplace(URL_WHISPER, '%cpu', GetCPUArchitectureString, []), ConcatPaths([WhisperFolder, 'whisper.zip']));
  {$ELSE}
  ShowDownloadDialog(URL_WHISPER, ConcatPaths([WhisperFolder, 'whisper.zip']));
  {$ENDIF}
  if FileExists(ConcatPaths([WhisperFolder, WHISPER_EXE])) then
  begin
    Tools.WhisperCPP := ConcatPaths([WhisperFolder, WHISPER_EXE]);
    CheckState_whispercpp;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmWizard.btnDownloadFasterWhisperClick(Sender: TObject);
begin
  {$IFDEF DARWIN}
  ShowDownloadDialog(StringReplace(URL_FASTERWHISPER, '%cpu', GetCPUArchitectureString, []), ConcatPaths([WhisperFolder, 'whisper.zip']));
  {$ELSE}
  ShowDownloadDialog(URL_FASTERWHISPER, ConcatPaths([WhisperFolder, 'whisper.zip']));
  ShowDownloadDialog(URL_FASTERWHISPERCUDA, ConcatPaths([WhisperFolder, 'cuda.zip']));
  {$ENDIF}
  if FileExists(ConcatPaths([WhisperFolder, FASTERWHISPER_EXE])) then
  begin
    Tools.FasterWhisper := ConcatPaths([WhisperFolder, FASTERWHISPER_EXE]);
    CheckState_fasterwhisper;
  end;
end;

// -----------------------------------------------------------------------------

function TfrmWizard.CheckState_libMPV: Boolean;
begin
  // libMPV
  if FileExists(libMPVFileName) then
  begin
    btnDownload.Enabled := False;
    lblLibMPVStatus.Caption := lngSuccess;
    lblLibMPVStatus.Tag := 1;
  end
  else
  begin
    btnDownload.Enabled := True;
    lblLibMPVStatus.Caption := lngFailed;
    lblLibMPVStatus.Tag := -1;
  end;
  Result := not btnDownload.Enabled;
end;

// -----------------------------------------------------------------------------

function TfrmWizard.CheckState_ytdlp: Boolean;
begin
  // yt-dlp
  if FileExists(YTDLPFileName) then
  begin
    btnDownloadYTDLP.Enabled := False;
    lblYTDLPStatus.Caption := lngSuccess;
    lblYTDLPStatus.Tag := 1;
  end
  else
  begin
    btnDownloadYTDLP.Enabled := True;
    lblYTDLPStatus.Caption := lngFailed;
    lblYTDLPStatus.Tag := -1;
  end;
  Result := not btnDownloadYTDLP.Enabled;
end;

// -----------------------------------------------------------------------------

function TfrmWizard.CheckState_ffmpeg: Boolean;
begin
  // ffmpeg
  if FileExists(ffmpegFileName) then
  begin
    btnDownloadFFMPEG.Enabled := False;
    lblFFMPEGStatus.Caption := lngSuccess;
    lblFFMPEGStatus.Tag := 1;
  end
  else
  begin
    btnDownloadFFMPEG.Enabled := True;
    lblFFMPEGStatus.Caption := lngFailed;
    lblFFMPEGStatus.Tag := -1;
  end;
  Result := not btnDownloadFFMPEG.Enabled;
end;

// -----------------------------------------------------------------------------

function TfrmWizard.CheckState_whispercpp: Boolean;
begin
  // whisper.cpp
  if FileExists(WhisperFileName) then
  begin
    btnDownloadWhisper.Enabled := False;
    lblWhisperStatus.Caption := lngSuccess;
    lblWhisperStatus.Tag := 1;
  end
  else
  begin
    btnDownloadWhisper.Enabled := True;
    lblWhisperStatus.Caption := lngFailed;
    lblWhisperStatus.Tag := -1;
  end;
  Result := not btnDownloadWhisper.Enabled;
end;

// -----------------------------------------------------------------------------

function TfrmWizard.CheckState_fasterwhisper: Boolean;
begin
  // faster-whisper
  if FileExists(Tools.FasterWhisper) then
  begin
    btnDownloadFasterWhisper.Enabled := False;
    lblFasterWhisperStatus.Caption := lngSuccess;
    lblFasterWhisperStatus.Tag := 1;
  end
  else
  begin
    btnDownloadFasterWhisper.Enabled := True;
    lblFasterWhisperStatus.Caption := lngFailed;
    lblFasterWhisperStatus.Tag := -1;
  end;
  Result := not btnDownloadFasterWhisper.Enabled;
end;

// -----------------------------------------------------------------------------

end.

