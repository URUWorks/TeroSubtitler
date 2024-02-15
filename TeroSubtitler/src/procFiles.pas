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

unit procFiles;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Dialogs, formMain, procTypes, UWSubtitleAPI,
  UWSubtitleAPI.Formats, procLocalize;

{ Helpers }

function GetDefaultExtFromFilter(const AIndex: Integer; const AFilter: String): String;

{ Subtitle files }

function CloseSubtitle(const AKeepVideoOpen: Boolean): Boolean;
procedure NewSubtitle(const InsertEmptySubtitle: Boolean = True);
procedure LoadSubtitle(const FileName: String; const AFormat: TUWSubtitleFormats = sfInvalid; const AEncoding: TEncoding = NIL; const AFPS: Single = -1; const AKeepVideoOpen: Boolean = False; const AVideoFile: String = ''; const AddToMRU: Boolean = True);
procedure ImportSubtitle(const FileName: String; const AFormat: TUWSubtitleFormats = sfInvalid; const AEncoding: TEncoding = NIL; const AFPS: Single = -1);
procedure SaveSubtitle(const FileName: String; const Format: TUWSubtitleFormats; const SubtitleMode: TSubtitleMode; const AEncoding: TEncoding = NIL; const AFPS: Single = -1);
procedure SaveSubtitleAutoBackup;
procedure SaveMarkedSubtitle(const FileName: String; const Format: TUWSubtitleFormats; const SubtitleMode: TSubtitleMode; const AEncoding: TEncoding = NIL; const AFPS: Single = -1);
procedure SaveTextOnlySubtitle(const FileName: String; const Format: TUWSubtitleFormats; const SubtitleMode: TSubtitleMode; const Formatted: Boolean; const AEncoding: TEncoding = NIL; const AFPS: Single = -1);
procedure LoadVideo(const FileName: String; const Pos: Int64 = 0);
procedure LoadAudio(const FileName: String);
function GetMediaFileNameIfExists(const FileName: String; const Exts: array of String): String;
function IsValidMediaFileName(const FileName: String; const Exts: array of String): Boolean;
function LoadShotChangesFileName(const FileName: String): Boolean;

{ Subtitle files with dialogs }

procedure LoadSubtitleWithDialog(const SubtitleMode: TSubtitleMode = smText);
procedure SaveSubtitleWithDialog(const SubtitleMode: TSubtitleMode = smText);
procedure SaveCurrentSubtitle(const SubtitleMode: TSubtitleMode = smText);
procedure LoadVideoWithDialog;
procedure ReadSubtitleData(const AFileName: String; const OnlyTimings: Boolean; const SubtitleMode: TSubtitleMode = smText; const AFormat: TUWSubtitleFormats = sfInvalid; const AEncoding: TEncoding = NIL; const AFPS: Single = -1);
procedure ReadTimingsFromFileWithDialog;
procedure ReadTextsFromFileWithDialog;
procedure ImportSubtitleWithDialog;
procedure ExportMarkedSubtitleWithDialog(const SubtitleMode: TSubtitleMode = smText);
procedure ExportTextOnlySubtitleWithDialog(const SubtitleMode: TSubtitleMode = smText);

{ Project files }

procedure LoadProject(const FileName: String);
procedure SaveProject(const FileName: String);

{ Project files with dialogs }

procedure LoadProjectWithDialog;
procedure SaveProjectWithDialog;

{ Transcription files }

procedure LoadTranscription(const FileName: String);
procedure SaveTranscription(const FileName: String);

{ Transcription files with dialogs }

procedure LoadTranscriptionWithDialog;
procedure SaveTranscriptionWithDialog;

{ WebPreview }

procedure SaveForWebPreview;

{ Drop files }

procedure DropFilesProcess(const FileNames: array of String);
procedure DropFilesProcessDir(const Dir: String);
procedure DropFilesProcessFile(const FileName: String);

{ CommandLine }

procedure CommandLineProcess;

{ Initialize App }

procedure InitializeApp;

// -----------------------------------------------------------------------------

implementation

uses
  procConfig, procDialogs, procWorkspace, procVST, procSubtitle, procUndo,
  UWSystem.Encoding, formCustomFileDlg, UWSystem.SysUtils, Forms, procMRU,
  UWSystem.StrUtils, procForms, procProjectFile, formCustomSelectDlg,
  procFixSubtitles, LCLIntf, Base64, fpsTypes, UWSubtitleAPI.Utils,
  UWSubtitleAPI.Tags
  {$IFDEF DARWIN}
  , formWelcome
  {$ENDIF};

// -----------------------------------------------------------------------------

{ Helpers }

// -----------------------------------------------------------------------------

function GetSuggestedFileNameForSave: String;
begin
  Result := ChangeFileExt(ExtractFileName(SubtitleInfo.Text.FileName), '');
  if Result.IsEmpty and not frmMain.MPV.FileName.IsEmpty then
    Result := ChangeFileExt(ExtractFileName(frmMain.MPV.FileName), '');
end;

// -----------------------------------------------------------------------------

function GetDefaultExtFromFilter(const AIndex: Integer; const AFilter: String): String;
var
  sa : TStringArray;
  s : String;
  index, a, b : Integer;
begin
  if AFilter.IsEmpty then Exit;
  sa := AFilter.Split('|');
  index := (AIndex-1) * 2;
  s := sa[index];
  a := Pos('*.', s);
  if a > 0 then
  begin
    b := Pos(';', s);
    if b > 0 then
      s := Copy(s, a+1, b-a-1)
    else
      s := Copy(s, a+1, s.Length-a-1);
  end;
  Result := s;
end;

// -----------------------------------------------------------------------------

{ Subtitle files }

// -----------------------------------------------------------------------------

function CloseSubtitle(const AKeepVideoOpen: Boolean): Boolean;
var
  r: Integer;
begin
  Result := False;

  with frmMain do
  begin
    if VST.Enabled then // ask to save if needed
    begin
      if SubtitleInfo.Text.Changed then
      begin
        r := MsgSaveSubtitle(ExtractFileName(SubtitleInfo.Text.FileName));
        case r of
          mrYes    : actSaveSubtitle.Execute;
          mrCancel : Abort;
        end;
      end;

      if actTranslatorMode.Checked then
      begin
        if SubtitleInfo.Translation.Changed then
        begin
          r := MsgSaveSubtitle(ExtractFileName(SubtitleInfo.Translation.FileName), smTranslation);
          case r of
            mrYes    : actSaveTranslation.Execute;
            mrCancel : Abort;
          end;
        end;
      end;
    end;

    with frmMain do
      MRU.Update(SubtitleInfo.Text.FileName, MPV.FileName, WAVE.FileName, VSTFocusedNode(VST), MPV.GetMediaPosInMs, WAVE.GetPlayCursorMS, MPV.IsPlaying);

    if not AKeepVideoOpen then
      actCloseVideo.Execute;

    SubtitleInfo.Text.FileName := '';
    SubtitleInfo.Translation.FileName := '';
    SubtitleChangedReset;
    VST.RootNodeCount := 0;
    Subtitles.Clear;
    mmoSourceView.Lines.Clear;
    mmoText.Clear;
    mmoTranslation.Clear;
    UndoInstance.Clear;
    EnableWorkArea(False);
    UpdateValues(True);
    RefreshAppTitle;
  end;

  Result := True;
end;

// -----------------------------------------------------------------------------

procedure NewSubtitle(const InsertEmptySubtitle: Boolean = True);
begin
  with frmMain do
    if LayoutVST.Visible then
    begin
      if CloseSubtitle(False) then
      begin
        if Workspace.WorkMode = wmFrames then // ask for FPS?
          with frmMain.cboInputFPS do
          begin
            ItemIndex := formCustomSelectDlg.ExecuteDialog('', lngSelectFPSToUse, Items, ItemIndex, True);
            frmMain.cboInputFPSSelect(NIL);
          end;

        EnableWorkArea;
        if InsertEmptySubtitle then VSTInsertEntries(VST);
        VSTFocusChanged(NIL, NIL, 0);
      end;
    end
    else if LayoutSource.Visible then
    begin
      if CloseSubtitle(False) then
      begin
        EnableWorkArea;
        mmoSourceView.Clear;
        mmoSourceView.SetFocus;
      end;
    end
    else if LayoutTranscription.Visible then
    begin
    end;
end;

// -----------------------------------------------------------------------------

procedure LoadSubtitle(const FileName: String; const AFormat: TUWSubtitleFormats = sfInvalid; const AEncoding: TEncoding = NIL; const AFPS: Single = -1; const AKeepVideoOpen: Boolean = False; const AVideoFile: String = ''; const AddToMRU: Boolean = True);
var
  _FPS: Single;
  MRUInfoObject: TMRUInfoObject;
  VFisLoaded: Boolean;
  _Encoding: TEncoding = NIL;
  _EncIndex: Integer;
  IsBinary: Boolean;
begin
  {$IFDEF DARWIN}
  if not frmMain.Visible then
  begin
    frmMain.Show;
    Application.ProcessMessages;
  end;
  {$ENDIF}
  if not CloseSubtitle(AKeepVideoOpen) then Exit;

  MPVOptions.KeepVideoOpen := AKeepVideoOpen;

  _FPS := AFPS;
  if _FPS = -1 then _FPS := Workspace.FPS.DefFPS;

  if not FileExists(FileName) then
  begin
    ShowErrorMessageDialog(lngFileNotFound);
    Exit;
  end;

  _Encoding := AEncoding;
  IsBinary := IsBinaryFormat(FileName);

  if (_Encoding = NIL) and AppOptions.AskForInputEncoding and not AppOptions.UseOwnFileDialog and not IsBinary then
  begin
    _Encoding := GetEncodingFromFile(FileName, False);

    if _Encoding = NIL then
    begin
      _Encoding := GetEncodingFromFile(FileName, True);

      with frmMain.cboEncoding do
      begin
        if _Encoding <> NIL then
          _EncIndex := GetEncodingIndex(_Encoding.CodePage)
        else
          _EncIndex := 43; //Win1252

        _EncIndex := formCustomSelectDlg.ExecuteDialog('', lngSelectEncodingToUse, Items, _EncIndex, True);
        _Encoding := TEncoding.GetEncoding(Encodings[_EncIndex].CPID);
      end;
    end;
  end;

  if AppOptions.AskForInputFPS and (Subtitles.GetTimeBaseFromFile(FileName) = stbMedia) and not AppOptions.UseOwnFileDialog and not IsBinary then //not Subtitles.IsSMPTESupported(Subtitles.GetFormatFromFile(FileName)) then
  begin
    with frmMain.cboInputFPS do
    begin
      ItemIndex := formCustomSelectDlg.ExecuteDialog('', lngSelectFPSToUse, Items, ItemIndex, True);
      _FPS := DefFPSList[ItemIndex];
      frmMain.cboInputFPSSelect(NIL);
    end;
  end;

  if Subtitles.LoadFromFile(FileName, _Encoding, _FPS, AFormat) then
  begin
    frmMain.VST.RootNodeCount := Subtitles.Count;
    frmMain.cboEncoding.ItemIndex := GetEncodingIndex(Subtitles.CodePage);
    frmMain.cboFormat.ItemIndex := Integer(Subtitles.Format)-1;
    if (Subtitles.FrameRate > 0) then
    begin
      frmMain.cboInputFPS.ItemIndex := frmMain.cboInputFPS.Items.IndexOf(SingleToStr(Subtitles.FrameRate, FormatSettings));
      _FPS := Subtitles.FrameRate;
    end;
    frmMain.cboInputFPSSelect(NIL);

    if not Subtitles.IsSMPTESupported then
    begin
      if (Workspace.WorkMode = wmFrames) and not IsInteger(Subtitles.FrameRate) then
      begin
        Subtitles.ConvertTimesToSMPTE(True);
        SetSMPTEMode(True);
      end
      else
        SetSMPTEMode(False);
    end
    else
    begin
      if (Subtitles.TimeBase = stbSMPTE) then
      begin
        if (Workspace.WorkMode = wmTime) then
        begin
          //if not IsInteger(Subtitles.FrameRate) then
          //  Subtitles.ConvertTimesToSMPTE(False);

          SetSMPTEMode(False);
        end
        else
        begin
          if not IsInteger(Subtitles.FrameRate) then
            Subtitles.ConvertTimesToSMPTE(True);

          SetSMPTEMode(not IsInteger(Subtitles.FrameRate));
        end;
      end
      else if (Subtitles.TimeBase = stbMedia) then
        SetSMPTEMode(False);
    end;

    DoAutoCheckErrors;

    SubtitleInfo.Text.FileName := FileName;
    RefreshAppTitle;

    if frmMain.LayoutSource.Visible then
    begin
      frmMain.mmoSourceView.Tag := TAG_CONTROL_UPDATE;
      frmMain.mmoSourceView.Text := Subtitles.SaveToString(Workspace.FPS.OutputFPS, NIL, TUWSubtitleFormats(frmMain.cboFormat.ItemIndex+1), smText);
      frmMain.mmoSourceView.Tag := TAG_CONTROL_NORMAL;
    end;

    EnableWorkArea;

    VFisLoaded := False;
    MRUInfoObject := MRU.GetValues(FileName);
    if Assigned(MRUInfoObject) then
    begin
      VSTSelectNode(frmMain.VST, MRUInfoObject.SelectedLine, True, True);
      if not MRUInfoObject.VideoFile.IsEmpty and not AKeepVideoOpen then
      begin
        VFisLoaded := True;
        LoadVideo(MRUInfoObject.VideoFile, MRUInfoObject.MPVPosition);
      end;
      if not MRUInfoObject.WaveformFile.IsEmpty then LoadAudio(MRUInfoObject.WaveformFile);
    end;

    if not AVideoFile.IsEmpty then
      LoadVideo(AVideoFile)
    else if not AKeepVideoOpen and not VFisLoaded then LoadVideo(GetMediaFileNameIfExists(FileName, TVideoExts));

    if AddToMRU then
      with frmMain do
        MRU.Add(FileName, MPV.FileName, WAVE.FileName, VSTFocusedNode(VST), MPV.GetMediaPosInMs, WAVE.GetPlayCursorMS, MPVOptions.AutoStartPlaying);

    if frmMain.WindowState = wsMinimized then
      frmMain.WindowState := wsNormal;
  end
  else
    ShowErrorMessageDialog(lngUnableToLoad);
end;

// -----------------------------------------------------------------------------

procedure ImportSubtitle(const FileName: String; const AFormat: TUWSubtitleFormats = sfInvalid; const AEncoding: TEncoding = NIL; const AFPS: Single = -1);
var
  _FPS : Single;
  Subs : TUWSubtitles;
  i, x : Integer;
begin
  if Subtitles.Count = 0 then Exit;

  _FPS := AFPS;
  if _FPS = -1 then _FPS := Workspace.FPS.DefFPS;

  Subs := TUWSubtitles.Create;
  try
    if Subs.LoadFromFile(FileName, AEncoding, _FPS, AFormat) then
    begin
      for i := 0 to Subs.Count-1 do
      begin
        x := Subtitles.FindInsertPos(Subs[i]);
        InsertSubtitle(x, Subs[i], False, False);
      end;
      UndoInstance.IncrementUndoGroup;
      UpdateValues(True);
    end
    else
      ShowErrorMessageDialog(lngUnableToLoad);
  finally
    Subs.Free;
  end;
end;

// -----------------------------------------------------------------------------

function AllowSaveFormat(const AFormat: TUWSubtitleFormats): Boolean;
var
  F : AnsiString;
  i : Integer;
begin
  Result := False;

  if AFormat = sfITunesTimedText then
  begin
    F := SingleToStr(Workspace.FPS.OutputFPS, FormatSettings).Replace(FormatSettings.DecimalSeparator, '.');
    if ((F <> '29.97') and (F <> '25') and (F <> '24') and (F <> '23.976')) then
    begin
      ShowErrorMessageDialog(Format('%s%s%s', [lngSubtitleSpecificationError, LineEnding+LineEnding, lngSubtitleSpecificationErrorFPS]), '', False, False);
      Exit;
    end;

    for i := 0 to Subtitles.Count-1 do
      if LineCount(Subtitles[i].Text, LineEnding) > 2 then
      begin
        ShowErrorMessageDialog(Format('%s%s%s', [lngSubtitleSpecificationError, LineEnding+LineEnding, lngSubtitleSpecificationErrorBR]), '', False, False);
        Exit;
      end
      else if (Subtitles[i].VAlign = svaCenter) then
      begin
        ShowErrorMessageDialog(Format('%s%s%s', [lngSubtitleSpecificationError, LineEnding+LineEnding, lngSubtitleSpecificationErrorAlign]), '', False, False);
        Exit;
      end;
  end;

  Result := True;
end;

// -----------------------------------------------------------------------------

procedure SaveSubtitle(const FileName: String; const Format: TUWSubtitleFormats; const SubtitleMode: TSubtitleMode; const AEncoding: TEncoding = NIL; const AFPS: Single = -1);
var
  _FPS      : Single;
  _Encoding : TEncoding;
  S, ext : String;
begin
  _FPS := AFPS;
  if _FPS = -1 then _FPS := Workspace.FPS.OutputFPS;
  _Encoding := AEncoding;
  if _Encoding = NIL then _Encoding := TEncoding.GetEncoding(Encodings[frmMain.cboEncoding.ItemIndex].CPID);

  if Workspace.SMPTE then
    Subtitles.TimeBase := stbSMPTE
  else
    Subtitles.TimeBase := stbMedia;

  S := FileName;
  if Format = sfSpreadsheet then
  begin
    ext := LowerCase(ExtractFileExt(FileName));
    if (ext <> STR_EXCEL_EXTENSION) and (ext <> STR_OOXML_EXCEL_EXTENSION) and (ext <> STR_OPENDOCUMENT_CALC_EXTENSION) then
      S := ChangeFileExt(FileName, STR_EXCEL_EXTENSION)
  end;

  if Subtitles.SaveToFile(S, _FPS, _Encoding, Format, SubtitleMode) then
  begin
    if SubtitleMode = smText then
      SubtitleInfo.Text.FileName := S
    else
      SubtitleInfo.Translation.FileName := S;

    SubtitleChangedReset(SubtitleMode);
    RefreshAppTitle;

    with frmMain do
      MRU.Add(S, MPV.FileName, WAVE.FileName, VSTFocusedNode(VST), MPV.GetMediaPosInMs, WAVE.GetPlayCursorMS, MPV.IsPlaying);
  end
  else
    ShowErrorMessageDialog(lngSaveSubtitleError);
end;

// -----------------------------------------------------------------------------

procedure SaveSubtitleAutoBackup;
var
  AFPS      : Single;
  AFileName : String;
  ATimeDate : String;
begin
  AFPS := Workspace.FPS.OutputFPS;

  if SubtitleInfo.Text.FileName.IsEmpty then
    AFileName := 'noname'
  else
    AFileName := ExtractFileName(SubtitleInfo.Text.FileName);

  ATimeDate := FormatDateTime('yyyy-mm-dd_hh-nn-ss_', Now);

  if Subtitles.SaveToFile(BackupFolder+ATimeDate+ChangeFileExt(AFileName, '.tero'), AFPS, TEncoding.UTF8, sfTeroSubtitler, smText) then
    SetStatusBarText(lngBackupSaved);
end;

// -----------------------------------------------------------------------------

procedure SaveMarkedSubtitle(const FileName: String; const Format: TUWSubtitleFormats; const SubtitleMode: TSubtitleMode; const AEncoding: TEncoding = NIL; const AFPS: Single = -1);
var
  _FPS      : Single;
  _Encoding : TEncoding;
  Subs      : TUWSubtitles;
  i         : Integer;
begin
  _FPS := AFPS;
  if _FPS = -1 then _FPS := Workspace.FPS.OutputFPS;
  _Encoding := AEncoding;
  if _Encoding = NIL then  _Encoding := TEncoding.GetEncoding(Encodings[frmMain.cboEncoding.ItemIndex].CPID);

  Subs := TUWSubtitles.Create;
  try
    Subs.FormatProperties := Subtitles.FormatProperties;

    for i := 0 to Subtitles.Count-1 do
      if Subtitles[i].Marked then
        Subs.Add(Subtitles[i]);

    if Subs.Count > 0 then
    begin
      if Subs.SaveToFile(FileName, _FPS, _Encoding, Format, SubtitleMode) then
      begin
        with frmMain do
          MRU.Add(FileName, MPV.FileName, WAVE.FileName, VSTFocusedNode(VST), MPV.GetMediaPosInMs, WAVE.GetPlayCursorMS, MPV.IsPlaying);
      end
      else
        ShowErrorMessageDialog(lngSaveSubtitleError);
    end;
  finally
    Subs.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure SaveTextOnlySubtitle(const FileName: String; const Format: TUWSubtitleFormats; const SubtitleMode: TSubtitleMode; const Formatted: Boolean; const AEncoding: TEncoding = NIL; const AFPS: Single = -1);
var
  _FPS      : Single;
  _Encoding : TEncoding;
  i         : Integer;
  txt       : TUWStringList;
begin
  if Subtitles.Count = 0 then Exit;

  _FPS := AFPS;
  if _FPS = -1 then _FPS := Workspace.FPS.OutputFPS;
  _Encoding := AEncoding;
  if _Encoding = NIL then  _Encoding := TEncoding.GetEncoding(Encodings[frmMain.cboEncoding.ItemIndex].CPID);

  txt := TUWStringList.Create;
  try
    for i := 0 to Subtitles.Count-1 do
      txt.Add(iff(SubtitleMode = smText, Subtitles[i].Text, Subtitles[i].Translation) + sLineBreak);

    if not Formatted then
      txt.Text := ReplaceEnters(RemoveTSTags(txt.Text), sLineBreak, ' ');

    txt.SaveToFile(FileName, AEncoding);
  finally
    txt.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure LoadVideo(const FileName: String; const Pos: Int64 = 0);
begin
  with frmMain do
    if FileExists(FileName) then
    begin
      if not MPVOptions.KeepVideoOpen then
        actCloseVideoExecute(NIL)
      else
        MPVOptions.KeepVideoOpen := False;

      if not actVideoPreview.Checked then
        actVideoPreview.Execute;

      MPV.Play(FileName, Pos);
      actMediaChangePlayRateExecute(NIL);
    end;
end;

// -----------------------------------------------------------------------------

procedure LoadAudio(const FileName: String);
begin
  with frmMain do
  begin
    if FileName = '' then
      Exit
    else if FileExists(FileName) then
      WAVE.LoadWaveFromFile(FileName)
    else
    begin // wave/peak file not found
      WAVE.LoadWaveFromFile(GetMediaFileNameIfExists(FileName, TWaveformAudioExts));
    end;

    if WAVE.IsPeakDataLoaded then
    begin
      LoadShotChangesFileName(FileName);
      EnableActionsByTag([TAG_ACTION_AUDIO], True);
    end;

    if (WAVE.IsPeakDataLoaded and not actWaveformPreview.Checked) or
       (not WAVE.IsPeakDataLoaded and actWaveformPreview.Checked) then
      actWaveformPreview.Execute;
  end;
end;

// -----------------------------------------------------------------------------

function GetMediaFileNameIfExists(const FileName: String; const Exts: array of String): String;
var
  i: Integer;
  s1, s2: String;
begin
  Result := '';

  for i := 0 to Length(Exts)-1 do
  begin
    s1 := ChangeFileExt(FileName, Exts[i]);
    s2 := ConcatPaths([WaveformsFolder, ExtractFileName(ChangeFileExt(FileName, Exts[i]))]);

    if FileExists(s1) then
      Exit(s1)
    else if FileExists(s2) then
      Exit(s2);
  end;
end;

// -----------------------------------------------------------------------------

function IsValidMediaFileName(const FileName: String; const Exts: array of String): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Length(Exts)-1 do
    if LowerCase(ExtractFileExt(FileName)) = Exts[i] then
    begin
      Result := True;
      Break;
    end;
end;

// -----------------------------------------------------------------------------

function LoadShotChangesFileName(const FileName: String): Boolean;
var
  s: String;
  i: Integer;
begin
  Result := False;

  for i := 0 to Length(TShotChangeExts)-1 do
  begin
    s := ConcatPaths([ShotChangesFolder, ExtractFileName(ChangeFileExt(FileName, TShotChangeExts[i]))]);
    if FileExists(s) then
    begin
      frmMain.WAVE.LoadSceneChangeFile(s, (Workspace.WorkMode = wmTime));
      Result := True;
      Break;
    end;
  end;
end;

// -----------------------------------------------------------------------------

{ Subtitle files with dialogs }

// -----------------------------------------------------------------------------

procedure LoadSubtitleWithDialog(const SubtitleMode: TSubtitleMode = smText);
var
  OD : TOpenDialog;
  CD : TfrmCustomFileDlg;
begin
  if AppOptions.UseOwnFileDialog then
  begin
    CD := TfrmCustomFileDlg.Create(NIL);
    try
      CD.FileName := SubtitleInfo.Text.FileName;
      if CD.Execute then
      begin
        if SubtitleMode = smText then
          LoadSubtitle(CD.FileName, CD.Format, CD.Encoding, CD.FPS)
        else
        begin
          if Subtitles.Count > 0 then
            ReadSubtitleData(CD.FileName, False, smTranslation, CD.Format, CD.Encoding, CD.FPS);
        end;
      end;
    finally
      CD.Free;
    end;
  end
  else
  begin
    OD := TOpenDialog.Create(NIL);
    try
      OD.Title   := lngOpenFile;
      OD.Filter  := Subtitles.FillDialogFilter(lngAllSupportedFiles);
      OD.Options := OD.Options + [ofFileMustExist];

      if OD.Execute then
      begin
        if SubtitleMode = smText then
          LoadSubtitle(OD.FileName)
        else
        begin
          if Subtitles.Count > 0 then
            ReadSubtitleData(OD.FileName, False, smTranslation);
        end;
      end;
    finally
      OD.Free;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure SaveSubtitleWithDialog(const SubtitleMode: TSubtitleMode = smText);
var
  SD : TSaveDialog;
  CD : TfrmCustomFileDlg;
begin
  if AppOptions.CheckErrorsBeforeSave and NeedToCheckErrors(Subtitles) then
    if MsgSaveWithErrors = mrNo then
      Exit;

  if AppOptions.UseOwnFileDialog then
  begin
    CD := TfrmCustomFileDlg.Create(NIL);
    try
      CD.FileName := GetSuggestedFileNameForSave;
      if CD.Execute(dmSave) and AllowSaveFormat(TUWSubtitleFormats(SD.FilterIndex)) then
        SaveSubtitle(CD.FileName, CD.Format, SubtitleMode, CD.Encoding, CD.FPS)
    finally
      CD.Free;
    end;
  end
  else
  begin
    SD := TSaveDialog.Create(NIL);
    try
      SD.Title  := lngSaveFile;
      SD.Filter := Subtitles.FillDialogFilter('');
      SD.FilterIndex := frmMain.cboFormat.ItemIndex+1;
      SD.FileName := {$IFDEF DARWIN}ChangeFileExt({$ENDIF}GetSuggestedFileNameForSave{$IFDEF DARWIN}, GetDefaultExtFromFilter(SD.FilterIndex, SD.Filter)){$ENDIF};
      {$IFDEF DARWIN}
      SD.OnTypeChange := @frmMain.DoSaveDialogTypeChange;
      {$ENDIF}

      SD.Options := [ofOverwritePrompt, ofEnableSizing];
      if SD.Execute and AllowSaveFormat(TUWSubtitleFormats(SD.FilterIndex)) then
        SaveSubtitle(SD.FileName, TUWSubtitleFormats(SD.FilterIndex), SubtitleMode);
    finally
      SD.Free;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure SaveCurrentSubtitle(const SubtitleMode: TSubtitleMode = smText);
var
  s: String;
begin
  if SubtitleMode = smText then
    s := SubtitleInfo.Text.FileName
  else
    s := SubtitleInfo.Translation.FileName;

  if s.IsEmpty then
    SaveSubtitleWithDialog(SubtitleMode)
  else
    SaveSubtitle(s, Subtitles.Format, SubtitleMode);
end;

// -----------------------------------------------------------------------------

procedure LoadVideoWithDialog;
var
  OD : TOpenDialog;
  i  : Integer;
  s  : String;
begin
  OD := TOpenDialog.Create(NIL);
  try
    s := '';
    for i := 0 to Length(TVideoExts)-1 do s := s + '*' + TVideoExts[i] + ';';
    for i := 0 to Length(TAudioExts)-1 do s := s + '*' + TAudioExts[i] + ';';
    SetLength(s, s.Length-1);

    OD.Title   := lngOpenFile;
    OD.Filter  := lngAllSupportedFiles + '|' + s;
    OD.Options := OD.Options + [ofFileMustExist];

    if OD.Execute then
    begin
      LoadVideo(OD.FileName);
    end;
  finally
    OD.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure ReadSubtitleData(const AFileName: String; const OnlyTimings: Boolean; const SubtitleMode: TSubtitleMode = smText; const AFormat: TUWSubtitleFormats = sfInvalid; const AEncoding: TEncoding = NIL; const AFPS: Single = -1);
var
  Sub  : TUWSubtitles;
  i, c : Integer;
begin
  if not AFileName.IsEmpty then
  begin
    Sub := TUWSubtitles.Create;
    try
      if Sub.LoadFromFile(AFileName, NIL, Workspace.FPS.DefFPS) then
      begin
        if Sub.Count > Subtitles.Count then
          c := Subtitles.Count
        else
          c := Sub.Count;

        for i := 0 to c-1 do
          if OnlyTimings then
            SetSubtitleTimes(i, Sub[i].InitialTime, Sub[i].FinalTime, False, False)
          else
            SetSubtitleText(i, Sub[i].Text, SubtitleMode, False, False, False);

        UndoInstance.IncrementUndoGroup;
        UpdateValues(True);

        if (SubtitleMode = smTranslation) then
        begin
          SubtitleInfo.Translation.FileName := AFileName;
          if not Workspace.TranslatorMode then SetTranslatorMode(True);
        end;
      end;
    finally
      Sub.Free;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure ReadTimingsFromFileWithDialog;
var
  OD: TOpenDialog;
begin
  OD := TOpenDialog.Create(NIL);
  try
    OD.Title   := lngOpenFile;
    OD.Filter  := Subtitles.FillDialogFilter(lngAllSupportedFiles);
    OD.Options := OD.Options + [ofFileMustExist];

    if OD.Execute then
      ReadSubtitleData(OD.FileName, True);
  finally
    OD.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure ReadTextsFromFileWithDialog;
var
  OD : TOpenDialog;
  sm : TSubtitleMode;
begin
  OD := TOpenDialog.Create(NIL);
  try
    OD.Title   := lngOpenFile;
    OD.Filter  := Subtitles.FillDialogFilter(lngAllSupportedFiles);
    OD.Options := OD.Options + [ofFileMustExist];

    if Workspace.TranslatorMode then
      sm := smTranslation
    else
      sm := smText;

    if OD.Execute then
      ReadSubtitleData(OD.FileName, False, sm);
  finally
    OD.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure ImportSubtitleWithDialog;
var
  OD : TOpenDialog;
  CD : TfrmCustomFileDlg;
begin
  if AppOptions.UseOwnFileDialog then
  begin
    CD := TfrmCustomFileDlg.Create(NIL);
    try
      CD.FileName := SubtitleInfo.Text.FileName;
      if CD.Execute then
      begin
        ImportSubtitle(CD.FileName, CD.Format, CD.Encoding, CD.FPS);
      end;
    finally
      CD.Free;
    end;
  end
  else
  begin
    OD := TOpenDialog.Create(NIL);
    try
      OD.Title   := lngOpenFile;
      OD.Filter  := Subtitles.FillDialogFilter(lngAllSupportedFiles);
      OD.Options := OD.Options + [ofFileMustExist];

      if OD.Execute then
      begin
        ImportSubtitle(OD.FileName);
      end;
    finally
      OD.Free;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure ExportMarkedSubtitleWithDialog(const SubtitleMode: TSubtitleMode = smText);
var
  SD : TSaveDialog;
  CD : TfrmCustomFileDlg;
begin
  if GetSubtitleMarkedCount = 0 then
  begin
    ShowErrorMessageDialog(lngNoEntriesMarked);
    Exit;
  end;

  if AppOptions.UseOwnFileDialog then
  begin
    CD := TfrmCustomFileDlg.Create(NIL);
    try
      CD.FileName := SubtitleInfo.Text.FileName;
      if CD.Execute(dmSave) then
      begin
        SaveMarkedSubtitle(CD.FileName, CD.Format, SubtitleMode, CD.Encoding, CD.FPS);
      end;
    finally
      CD.Free;
    end;
  end
  else
  begin
    SD := TSaveDialog.Create(NIL);
    try
      SD.Title  := lngSaveFile;
      SD.Filter := Subtitles.FillDialogFilter('');
      SD.FilterIndex := frmMain.cboFormat.ItemIndex+1;
      SD.FileName := ChangeFileExt(ExtractFileName(SubtitleInfo.Text.FileName), '');
      SD.Options := [ofOverwritePrompt, ofEnableSizing];
      if SD.Execute then
      begin
        SaveMarkedSubtitle(SD.FileName, TUWSubtitleFormats(SD.FilterIndex), SubtitleMode);
      end;
    finally
      SD.Free;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure ExportTextOnlySubtitleWithDialog(const SubtitleMode: TSubtitleMode = smText);
var
  SD : TSaveDialog;
  CD : TfrmCustomFileDlg;
  Formatted: Boolean;
  r : Integer;
begin
  if Subtitles.Count = 0 then Exit;

  r := MsgExportTextOnlyFormat;
  if r = mrCancel then
    Exit
  else
    Formatted := (r = mrYes);

  if AppOptions.UseOwnFileDialog then
  begin
    CD := TfrmCustomFileDlg.Create(NIL);
    try
      CD.FileName := SubtitleInfo.Text.FileName;
      if CD.Execute(dmSave) then
      begin
        SaveTextOnlySubtitle(CD.FileName, CD.Format, SubtitleMode, Formatted, CD.Encoding, CD.FPS);
      end;
    finally
      CD.Free;
    end;
  end
  else
  begin
    SD := TSaveDialog.Create(NIL);
    try
      SD.Title  := lngSaveFile;
      SD.Filter := lngscShotChanges + ' (*.txt)|*.txt';
      SD.FilterIndex := 0;
      SD.FileName := ChangeFileExt(ExtractFileName(SubtitleInfo.Text.FileName), '.txt');
      SD.Options := [ofOverwritePrompt, ofEnableSizing];
      if SD.Execute then
      begin
        SaveTextOnlySubtitle(SD.FileName, TUWSubtitleFormats(SD.FilterIndex), SubtitleMode, Formatted);
      end;
    finally
      SD.Free;
    end;
  end;
end;

// -----------------------------------------------------------------------------

{ Project files }

// -----------------------------------------------------------------------------

procedure LoadProject(const FileName: String);
begin
  if FileName.IsEmpty then Exit;
  with TProjectFile.Create(FileName) do
  try
    if Ready then
    begin
      LoadSubtitle(Original, sfInvalid, NIL, -1, False, Movie, False);
      if Subtitles.Count > 0 then
        ReadSubtitleData(Translation, False, smTranslation);

        with frmMain do
          MRU.Add(FileName, '', '', VSTFocusedNode(VST), MPV.GetMediaPosInMs, WAVE.GetPlayCursorMS, MPVOptions.AutoStartPlaying);
    end;
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure SaveProject(const FileName: String);
begin
  if FileName.IsEmpty then Exit;
  with TProjectFile.Create(FileName, False) do
  try
    Original    := SubtitleInfo.Text.FileName;
    Translation := SubtitleInfo.Translation.FileName;
    SaveProject;

    with frmMain do
      MRU.Add(FileName, '', '', VSTFocusedNode(VST), MPV.GetMediaPosInMs, WAVE.GetPlayCursorMS, MPVOptions.AutoStartPlaying);
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

{ Project files with dialogs }

// -----------------------------------------------------------------------------

procedure LoadProjectWithDialog;
var
  OD : TOpenDialog;
begin
  OD := TOpenDialog.Create(NIL);
  try
    OD.Title   := lngOpenFile;
    OD.Filter  := lngProjectFile + '|*' + TProjectExt;
    OD.Options := OD.Options + [ofFileMustExist];

    if OD.Execute then
      LoadProject(OD.FileName);
  finally
    OD.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure SaveProjectWithDialog;
var
  SD : TSaveDialog;
begin
  if SubtitleInfo.Text.FileName.IsEmpty then
    SaveSubtitleWithDialog;

  if SubtitleInfo.Translation.FileName.IsEmpty then
    SaveSubtitleWithDialog(smTranslation);

  SD := TSaveDialog.Create(NIL);
  try
    SD.Title   := lngSaveFile;
    SD.Filter  := lngProjectFile + '|*' + TProjectExt;
    SD.Options := [ofOverwritePrompt, ofEnableSizing];
    SD.FileName := ChangeFileExt('Project_' + ExtractFileName(SubtitleInfo.Text.FileName), '');

    if SD.Execute then
      SaveProject(SD.FileName);
  finally
    SD.Free;
  end;
end;

// -----------------------------------------------------------------------------

{ Transcription files}

// -----------------------------------------------------------------------------

procedure LoadTranscription(const FileName: String);
begin
  SetViewMode(vmTranscription);
  with Workspace.Transcription do
    if Assigned(Memo) then
    begin
      Memo.LoadFromFile(FileName, []);
      Memo.Modified := False;
    end;
end;

// -----------------------------------------------------------------------------

procedure SaveTranscription(const FileName: String);
begin
  with Workspace.Transcription do
    if Assigned(Memo) then
    begin
      Memo.SaveToFile(FileName);
      Memo.Modified := False;
    end;
end;

// -----------------------------------------------------------------------------

{ Transcription files with dialogs }

// -----------------------------------------------------------------------------

procedure LoadTranscriptionWithDialog;
var
  OD : TOpenDialog;
begin
  OD := TOpenDialog.Create(NIL);
  try
    OD.Title   := lngOpenFile;
    OD.Filter  := lngAllSupportedFiles + ' (*.txt)|' + '*.txt';
    OD.Options := OD.Options + [ofFileMustExist];

    if OD.Execute then
    begin
      LoadTranscription(OD.FileName);
    end;
  finally
    OD.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure SaveTranscriptionWithDialog;
var
  SD : TSaveDialog;
begin
  SD := TSaveDialog.Create(NIL);
  try
    SD.Title    := lngSaveFile;
    SD.Filter   := lngAllSupportedFiles + ' (*.txt)|' + '*.txt';
    SD.FileName := '';
    SD.Options  := [ofOverwritePrompt, ofEnableSizing];
    if SD.Execute then
    begin
      SaveTranscription(SD.FileName);
    end;
  finally
    SD.Free;
  end;
end;

// -----------------------------------------------------------------------------

{ WebPreview }

// -----------------------------------------------------------------------------

procedure SaveForWebPreview;

const HTMLFormat: String =
  '<!doctype html>' + sLineBreak +
  '<html lang="en">' + sLineBreak +
  '  <head>' + sLineBreak +
  '    <meta charset="utf-8">' + sLineBreak +
  '    <title>' + ProgramName + ' (Web Preview)</title>' + sLineBreak +
  '  </head>' + sLineBreak +
  '  <body style="background-color:Black;">' + sLineBreak +
  '    <center>' + sLineBreak +
  '      <video controls preload="metadata">' + sLineBreak +
  '        <source src="file://[VIDEO]" type="video/[EXT]">' + sLineBreak +
  '        <track label="' + ProgramName + '" kind="subtitles" srclang="en" src="data:text/vtt;base64, [BASE64]" default>' + sLineBreak +
  '      </video>' + sLineBreak +
  '    </center>' + sLineBreak +
  '  </body>' + sLineBreak +
  '</html>';

var
  ts : TStrings;
  i : Integer;
  SubText : String;
  VideoExt : String;
  VideoSupport : Boolean;
begin
  if (Subtitles.Count > 0) and frmMain.MPV.IsMediaLoaded then
  begin
    VideoExt := ExtractFileExt(frmMain.MPV.FileName.ToLower);
    VideoSupport := False;
    for i := 0 to High(TVideoWebExts) do
      if VideoExt = TVideoWebExts[i] then
      begin
        VideoSupport := True;
        Break;
      end;

    if not VideoSupport then
    begin
      ShowErrorMessageDialog(lngWebVideoUnsupported);
      Exit;
    end;

    ts := TStringList.Create;
    try
      ts.Text := HTMLFormat;
      SubText := EncodeStringBase64(Subtitles.SaveToString(Workspace.FPS.OutputFPS, NIL, sfWebVTT, smText));
      Delete(VideoExt, 1, 1);
      ts.Text := ts.Text.Replace('[VIDEO]', frmMain.MPV.FileName);
      ts.Text := ts.Text.Replace('[EXT]', VideoExt);
      ts.Text := ts.Text.Replace('[BASE64]', SubText);

      ts.SaveToFile(WebPreviewTempFileName);
      OpenURL({$IFDEF DARWIN}'file://' + {$ENDIF}WebPreviewTempFileName);
    finally
      ts.Free;
    end;
  end;
end;

// -----------------------------------------------------------------------------

{ Drop files }

// -----------------------------------------------------------------------------

procedure DropFilesProcess(const FileNames: array of String);
var
  c: Integer;
begin
  {$IFDEF DARWIN}
  if frmWelcome <> NIL then frmWelcome.Close;
  {$ENDIF}

  for c := 0 to Length(FileNames)-1 do
  begin
    if DirectoryExists(FileNames[c]) then
      DropFilesProcessDir(FileNames[c])
    else
      DropFilesProcessFile(FileNames[c]);
  end;
end;

// -----------------------------------------------------------------------------

procedure DropFilesProcessDir(const Dir: String);
var
  DirInfo: TSearchRec;
begin
  if FindFirst(Dir+DirectorySeparator+'*', faAnyFile and faDirectory, DirInfo) = 0 then
  repeat
    if (DirInfo.Attr and faDirectory) = faDirectory then
    begin
      if (DirInfo.Name<>'.') and (DirInfo.Name<>'..') then
        DropFilesProcessDir(Dir+DirectorySeparator+DirInfo.Name);
    end
    else
      DropFilesProcessFile(Dir+DirectorySeparator+DirInfo.Name);
  until FindNext(DirInfo) <> 0;
  FindClose(DirInfo);
end;

// -----------------------------------------------------------------------------

procedure DropFilesProcessFile(const FileName: String);
begin
  if IsValidMediaFileName(FileName, TVideoExts) or IsValidMediaFileName(FileName, TAudioExts) then
    LoadVideo(FileName)
  else if IsValidMediaFileName(FileName, TWaveformAudioExts) then
    LoadAudio(FileName)
  else
    LoadSubtitle(FileName);
end;

// -----------------------------------------------------------------------------

{ CommandLine }

// -----------------------------------------------------------------------------

procedure CommandLineProcess;
var
  i: Byte;
begin
  // Command line parameters reading
  if ParamCount > 0 then
  begin
    // Subtitle file
    if FileExists(ParamStr(1)) then
      LoadSubtitle(ParamStr(1));

    // Others params
    for i := 1 to ParamCount do
    begin
      //ParamStr(i);
    end;
  end;
end;

// -----------------------------------------------------------------------------

{ Initialize App }

// -----------------------------------------------------------------------------

procedure InitializeApp;
begin
  if not FileExists(SettingsFileName) then // first start? show wizard!
  begin
    ShowWizard;
    Application.ShowMainForm := True;
  end
  else
  begin
    // libMPV
    with frmMain do
      if not MPV.IsLibMPVAvailable then
      begin
        if MPV.Error = -20 then
          ShowErrorMessageDialog(lnglibMPVError) // dll not found
        else
          ShowErrorMessageDialog(lnglibMPVVersionError);
      end;

    {$IFNDEF DARWIN}
    // check commandline
    CommandLineProcess;
    {$ENDIF}

    // Welcome form
    if AppOptions.ShowWelcomeAtStartup and not frmMain.VST.Enabled then
      ShowWelcome
    {$IFDEF DARWIN}
    else
    {$ELSE};
    {$ENDIF}
    Application.ShowMainForm := True;
  end;
end;

// -----------------------------------------------------------------------------

end.
