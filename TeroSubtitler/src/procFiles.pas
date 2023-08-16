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
  UWSubtitleAPI.Formats;

{ Subtitle files }

function CloseSubtitle(const ACloseVideo: Boolean): Boolean;
procedure NewSubtitle(const InsertEmptySubtitle: Boolean = True);
procedure LoadSubtitle(const FileName: String; const AFormat: TUWSubtitleFormats = sfInvalid; const AEncoding: TEncoding = NIL; const AFPS: Single = -1; const AutoLoadVideoFile: Boolean = True; const AddToMRU: Boolean = True);
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
  procCommon, procWorkspace, procVST, procSubtitle, procUndo, UWSystem.Encoding,
  formCustomFileDlg, UWSystem.XMLLang, UWSystem.SysUtils, Forms, procMRU,
  UWSystem.StrUtils, procForms, procProjectFile
  {$IFDEF DARWIN}
  , formWelcome
  {$ENDIF};

// -----------------------------------------------------------------------------

{ Subtitle files }

// -----------------------------------------------------------------------------

function CloseSubtitle(const ACloseVideo: Boolean): Boolean;
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

    if ACloseVideo then
      actCloseVideo.Execute;

    SubtitleInfo.Text.FileName := '';
    SubtitleInfo.Translation.FileName := '';
    SubtitleChanged(False, False);
    VST.RootNodeCount := 0;
    Subtitles.Clear;
    mmoSourceView.Lines.Clear;
    UndoInstance.Clear;
    EnableWorkArea(not ACloseVideo);
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
      if CloseSubtitle(True) then
      begin
        EnableWorkArea;
        if InsertEmptySubtitle then VSTInsertSubtitles(VST);
      end;
    end
    else if LayoutSource.Visible then
    begin
      if CloseSubtitle(True) then
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

procedure LoadSubtitle(const FileName: String; const AFormat: TUWSubtitleFormats = sfInvalid; const AEncoding: TEncoding = NIL; const AFPS: Single = -1; const AutoLoadVideoFile: Boolean = True; const AddToMRU: Boolean = True);
var
  _FPS: Single;
  MRUInfoObject: TMRUInfoObject;
  VFisLoaded: Boolean;
begin
  {$IFDEF DARWIN}
  if not frmMain.Visible then frmMain.Show;
  {$ENDIF}
  if not CloseSubtitle(AutoLoadVideoFile) then Exit;

  _FPS := AFPS;
  if _FPS = -1 then _FPS := Workspace.FPS.DefFPS;

  if not FileExists(FileName) then
  begin
    ShowErrorMessageDialog(GetCommonString('FileNotFound'));
  end
  else if Subtitles.LoadFromFile(FileName, AEncoding, _FPS, AFormat) then
  begin
    frmMain.VST.RootNodeCount := Subtitles.Count;
    frmMain.cboEncoding.ItemIndex := GetEncodingIndex(Subtitles.CodePage);
    frmMain.cboFormat.ItemIndex := Integer(Subtitles.Format)-1;
    if Subtitles.FrameRate > 0 then
    begin
      frmMain.cboInputFPS.ItemIndex := frmMain.cboInputFPS.Items.IndexOf(SingleToStr(Subtitles.FrameRate, AppOptions.FormatSettings));
      _FPS := Subtitles.FrameRate;
    end;
    Workspace.FPS.OutputFPS := _FPS;
    frmMain.cboFPS.ItemIndex := frmMain.cboInputFPS.ItemIndex;
    SetTimeFPStoTimeEditCtrls;

    if not Subtitles.IsSMPTESupported then
    begin
      if (Workspace.WorkMode = wmFrames) and not IsInteger(Subtitles.FrameRate) then
      begin
        Subtitles.ConvertTimesToSMPTE(True);
        SetSMPTEMode(True);
      end;
    end
    else
    begin
      if (Subtitles.TimeBase = stbSMPTE) then
        SetSMPTEMode(not IsInteger(Subtitles.FrameRate))
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
      VSTSelectNode(frmMain.VST, MRUInfoObject.SelectedLine, True);
      if not MRUInfoObject.VideoFile.IsEmpty then
      begin
        VFisLoaded := True;
        LoadVideo(MRUInfoObject.VideoFile, MRUInfoObject.MPVPosition);
      end;
      if not MRUInfoObject.WaveformFile.IsEmpty then LoadAudio(MRUInfoObject.WaveformFile);
    end;

    if AutoLoadVideoFile and not VFisLoaded then LoadVideo(GetMediaFileNameIfExists(FileName, TVideoExts));

    if AddToMRU then
      with frmMain do
        MRU.Add(FileName, MPV.FileName, WAVE.FileName, VSTFocusedNode(VST), MPV.GetMediaPosInMs, WAVE.GetPlayCursorMS, MPVOptions.AutoStartPlaying);

    if frmMain.WindowState = wsMinimized then
      frmMain.WindowState := wsNormal;
  end
  else
    ShowErrorMessageDialog(GetCommonString('UnableToLoad'));
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
      ShowErrorMessageDialog(GetCommonString('UnableToLoad'));
  finally
    Subs.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure SaveSubtitle(const FileName: String; const Format: TUWSubtitleFormats; const SubtitleMode: TSubtitleMode; const AEncoding: TEncoding = NIL; const AFPS: Single = -1);
var
  _FPS      : Single;
  _Encoding : TEncoding;
begin
  _FPS := AFPS;
  if _FPS = -1 then _FPS := Workspace.FPS.OutputFPS;
  _Encoding := AEncoding;
  if _Encoding = NIL then  _Encoding := TEncoding.GetEncoding(Encodings[frmMain.cboEncoding.ItemIndex].CPID);

  if Workspace.SMPTE then
    Subtitles.TimeBase := stbSMPTE
  else
    Subtitles.TimeBase := stbMedia;

  if Subtitles.SaveToFile(FileName, _FPS, _Encoding, Format, SubtitleMode) then
  begin
    if SubtitleMode = smText then
      SubtitleInfo.Text.FileName := FileName
    else
      SubtitleInfo.Translation.FileName := FileName;

    SubtitleChangedReset(SubtitleMode);
    RefreshAppTitle;

    with frmMain do
      MRU.Add(FileName, MPV.FileName, WAVE.FileName, VSTFocusedNode(VST), MPV.GetMediaPosInMs, WAVE.GetPlayCursorMS, MPV.IsPlaying);
  end
  else
    ShowErrorMessageDialog(GetCommonString('SaveSubtitleError'));
end;

// -----------------------------------------------------------------------------

procedure SaveSubtitleAutoBackup;
var
  AFPS      : Single;
  AEncoding : TEncoding;
  AFileName : String;
  ATimeDate : String;
begin
  AFPS      := Workspace.FPS.OutputFPS;
  AEncoding := TEncoding.GetEncoding(Encodings[Workspace.DefEncoding].CPID);

  if SubtitleInfo.Text.FileName.IsEmpty then
    AFileName := 'noname'
  else
    AFileName := ExtractFileName(SubtitleInfo.Text.FileName);

  ATimeDate := FormatDateTime('yyyy-mm-dd_hh-nn-ss_', Now);

  if Subtitles.SaveToFile(BackupFolder+ATimeDate+ChangeFileExt(AFileName, '.tero'), AFPS, AEncoding, sfTeroSubtitler, smText) then
    SetStatusBarText(GetCommonString('BackupSaved'));
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
        ShowErrorMessageDialog(GetLangString('SaveSubtitleError'));
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
      txt.Text := ReplaceEnters(txt.Text, sLineBreak, ' ');

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
      actCloseVideoExecute(NIL);

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
      WAVE.LoadWaveFromFile(GetMediaFileNameIfExists(FileName, TAudioExts));
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
      frmMain.WAVE.LoadSceneChangeFile(s);
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
      OD.Title   := GetCommonString('OpenFile');
      OD.Filter  := Subtitles.FillDialogFilter(GetCommonString('AllSupportedFiles'));
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
  if AppOptions.UseOwnFileDialog then
  begin
    CD := TfrmCustomFileDlg.Create(NIL);
    try
      CD.FileName := SubtitleInfo.Text.FileName;
      if CD.Execute(dmSave) then
      begin
        SaveSubtitle(CD.FileName, CD.Format, SubtitleMode, CD.Encoding, CD.FPS);
      end;
    finally
      CD.Free;
    end;
  end
  else
  begin
    SD := TSaveDialog.Create(NIL);
    try
      SD.Title  := GetCommonString('SaveFile');
      SD.Filter := Subtitles.FillDialogFilter('');
      SD.FilterIndex := frmMain.cboFormat.ItemIndex+1;
      SD.FileName := ChangeFileExt(ExtractFileName(SubtitleInfo.Text.FileName), '');
      SD.Options := [ofOverwritePrompt, ofEnableSizing];
      if SD.Execute then
      begin
        SaveSubtitle(SD.FileName, TUWSubtitleFormats(SD.FilterIndex), SubtitleMode);
      end;
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

    OD.Title   := GetCommonString('OpenFile');
    OD.Filter  := GetCommonString('AllSupportedFiles') + '|' + s;
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
    OD.Title   := GetCommonString('OpenFile');
    OD.Filter  := Subtitles.FillDialogFilter(GetCommonString('AllSupportedFiles'));
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
    OD.Title   := GetCommonString('OpenFile');
    OD.Filter  := Subtitles.FillDialogFilter(GetCommonString('AllSupportedFiles'));
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
      OD.Title   := GetCommonString('OpenFile');
      OD.Filter  := Subtitles.FillDialogFilter(GetCommonString('AllSupportedFiles'));
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
    ShowErrorMessageDialog(GetCommonString('NoSubtitlesMarked'));
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
      SD.Title  := GetCommonString('SaveFile');
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
begin
  if Subtitles.Count = 0 then Exit;

  Formatted := (MsgExportTextOnlyFormat = mrYes);

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
      SD.Title  := GetCommonString('SaveFile');
      SD.Filter := 'TXT|*.txt';
      SD.FilterIndex := frmMain.cboFormat.ItemIndex+1;
      SD.FileName := ChangeFileExt(ExtractFileName(SubtitleInfo.Text.FileName), '');
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
      LoadSubtitle(Original, sfInvalid, NIL, -1, False, False);
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
    OD.Title   := GetCommonString('OpenFile');
    OD.Filter  := GetCommonString('ProjectFile') + '|*' + TProjectExt;
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
    SD.Title   := GetCommonString('SaveFile');
    SD.Filter  := GetCommonString('ProjectFile') + '|*' + TProjectExt;
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
    OD.Title   := GetCommonString('OpenFile');
    OD.Filter  := GetCommonString('AllSupportedFiles') + ' (*.txt)|' + '*.txt';
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
    SD.Title    := GetCommonString('SaveFile');
    SD.Filter   := GetCommonString('AllSupportedFiles') + ' (*.txt)|' + '*.txt';
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
  if IsValidMediaFileName(FileName, TVideoExts) then
    LoadVideo(FileName)
  else if IsValidMediaFileName(FileName, TAudioExts) then
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
          ShowErrorMessageDialog(GetCommonString('libMPVError')) // dll not found
        else
          ShowErrorMessageDialog(GetCommonString('libMPVVersionError'));
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
