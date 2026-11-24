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
 *  Copyright (C) 2023-2026 URUWorks, uruworks@gmail.com.
 *}

unit procFiles;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Dialogs, formMain, procTypes, LazFileUtils,
  UWSubtitleAPI, UWSubtitleAPI.Formats, procLocalize, UWSystem.TimeUtils;

{ Helpers }

function GetDefaultExtFromFilter(const AIndex: Integer; const AFilter: String): String;
function GetSubtitleFormatExt(const AFormat: TUWSubtitleFormats): String;
function EnsureSaveFileExt(const AFileName, AExt: String; const AKnownFilter: String = ''): String;

{ Subtitle files }

function CloseSubtitle(const AKeepVideoOpen: Boolean): Boolean;
procedure NewSubtitle(const InsertEmptySubtitle: Boolean = True);
procedure LoadSubtitle(const FileName: String; const AFormat: TUWSubtitleFormats = sfInvalid; const AEncoding: TEncoding = NIL; const AFPS: Single = -1; const AKeepVideoOpen: Boolean = False; const AVideoFile: String = ''; const AddToMRU: Boolean = True);
procedure CheckRoundTimesOnLoad;
procedure ImportSubtitle(const FileName: String; const AFormat: TUWSubtitleFormats = sfInvalid; const AEncoding: TEncoding = NIL; const AFPS: Single = -1);
procedure SaveSubtitle(const FileName: String; const Format: TUWSubtitleFormats; const SubtitleMode: TSubtitleMode; const AEncoding: TEncoding = NIL; const AFPS: Single = -1);
procedure SaveSubtitleAutoBackup;
procedure SaveMarkedSubtitle(const FileName: String; const Format: TUWSubtitleFormats; const SubtitleMode: TSubtitleMode; const AEncoding: TEncoding = NIL; const AFPS: Single = -1);
procedure SaveTextOnlySubtitle(const FileName: String; const Format: TUWSubtitleFormats; const SubtitleMode: TSubtitleMode; const Formatted: Boolean; const AEncoding: TEncoding = NIL; const AFPS: Single = -1);
function LoadVideo(const FileName: String; const Pos: Int64 = 0): Boolean;
function LoadAudio(const FileName: String): Boolean;
function GetMediaFileNameIfExists(const FileName: String; const Exts: array of String): String;
function IsValidMediaFileName(const FileName: String; const Exts: array of String): Boolean;
function LoadShotChangesFileName(const FileName: String): Boolean;
procedure GetShotChangesForCache(const AList: TStrings);
procedure SaveShotChangesCache;

{ Subtitle files with dialogs }

procedure LoadSubtitleWithDialog(const SubtitleMode: TSubtitleMode = smText);
procedure SaveSubtitleWithDialog(const SubtitleMode: TSubtitleMode = smText);
procedure SaveCurrentSubtitle(const SubtitleMode: TSubtitleMode = smText; const AUpdateProject: Boolean = True);
function LoadVideoWithDialog: boolean;
procedure ReadSubtitleData(const AFileName: String; const OnlyTimings: Boolean; const SubtitleMode: TSubtitleMode = smText; const AFormat: TUWSubtitleFormats = sfInvalid; const AEncoding: TEncoding = NIL; const AFPS: Single = -1);
procedure ReadTimingsFromFileWithDialog;
procedure ReadTextsFromFileWithDialog;
procedure ImportSubtitleWithDialog;
procedure ImportPlainTextWithDialog;
procedure ExportMarkedSubtitleWithDialog(const SubtitleMode: TSubtitleMode = smText);
procedure ExportTextOnlySubtitleWithDialog(const SubtitleMode: TSubtitleMode = smText);

{ Project files }

procedure LoadProject(const FileName: String);
procedure SaveProject(const FileName: String);
// File > Save project: the open project (and its subtitle/translation if
// they were changed), or Save project as if there is none
procedure SaveCurrentProject;
// the state of the player saved in the project, once its video is loaded
// (called from MPVFileLoaded)
procedure ApplyProjectPlayerState;

{ Project files with dialogs }

procedure LoadProjectWithDialog;
procedure SaveProjectWithDialog; // File > Save project as...

var
  // the project (.tero1) open or saved last, '' if none: File > Save and
  // Save project update it. Closing/opening another subtitle forgets it
  CurrentProjectFile: String = '';

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
  Types, procMPV,
  procConfig, procDialogs, procWorkspace, procVST, procSubtitle, procUndo,
  UWSystem.Encoding, formCustomFileDlg, UWSystem.SysUtils, Forms, procMRU,
  UWSystem.StrUtils, procForms, procProjectFile, formCustomSelectDlg,
  procFixSubtitles, LCLIntf, Base64, fpsTypes, UWSubtitleAPI.Utils, LCLStrConsts,
  RegExpr,
  UWSubtitleAPI.Tags, UWTranslateAPI.Google, formCustomSelectDlgWithPreview,
  FileUtil, Math, URIParser, formCustomQuestionDlg, DolbyVision, ITunesTimedText, procThumbnails,
  LazUTF8, MPVPlayer, ActnList
  {$IFDEF DARWIN}
  , formWelcome
  {$ENDIF};

var
  // LoadProject is running: the subtitle it closes/opens doesn't forget the
  // project, nor the state of the player waiting for the video
  LoadingProject: Boolean = False;
  // the state of the player of the project and its video (see
  // ApplyProjectPlayerState)
  PendingProjectState: TStringList = NIL;
  PendingProjectVideo: String = '';

var
  // a subtitle in milliseconds was opened in frames mode: once the video is
  // loaded (its fps is known) we offer to round its timings to frames
  RoundTimesOnLoadPending: Boolean = False;

// -----------------------------------------------------------------------------

{ Helpers }

// -----------------------------------------------------------------------------

function CheckIfValidFileName(const AFileName: String): Boolean;
var
  d: String;
begin
  Result := False;
  if not AFileName.IsEmpty then
  begin
    d := ExtractFileDir(AFileName);
    if DirectoryExists(d) then
      Exit(True)
    else
      Exit(CreateDir(d));
  end;
end;

// -----------------------------------------------------------------------------

function GetSuggestedFileNameForSave: String;
begin
  Result := ChangeFileExt(ExtractFileName(SubtitleInfo.Text.FileName), '');
  if Result.IsEmpty and not frmMain.MPV.FileName.IsEmpty then
    Result := MPVMediaBaseName;
end;

// -----------------------------------------------------------------------------

function GetDefaultExtFromFilter(const AIndex: Integer; const AFilter: String): String;
var
  sa : TStringArray;
  s : String;
  index, a, b : Integer;
begin
  Result := '';
  if AFilter.IsEmpty then Exit;
  sa := AFilter.Split('|');
  index := (AIndex-1) * 2;
  if (index < 0) or (index >= Length(sa)) then Exit;
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
          mrYes    : begin
                       actSaveSubtitle.Execute;
                       // save dialog cancelled or save failed, don't lose changes
                       if SubtitleInfo.Text.Changed then Abort;
                     end;
          mrCancel : Abort;
        end;
      end;

      if actTranslatorMode.Checked then
      begin
        if SubtitleInfo.Translation.Changed then
        begin
          r := MsgSaveSubtitle(ExtractFileName(SubtitleInfo.Translation.FileName), smTranslation);
          case r of
            mrYes    : begin
                         actSaveTranslation.Execute;
                         if SubtitleInfo.Translation.Changed then Abort;
                       end;
            mrCancel : Abort;
          end;
        end;
      end;
    end;

    with frmMain do
      MRU.Update(SubtitleInfo.Text.FileName, MPV.FileName, WAVE.FileName, VSTFocusedNode(VST), MPV.GetMediaPosInMs, WAVE.GetPlayCursorMS, MPV.IsPlaying);

    if not AKeepVideoOpen then
      actCloseMedia.Execute;

    // keep the translation memory for the next subtitle, only save it
    TMX.Save;
    CloseTerminology;

    // the video offset of a Dolby Vision XML (the record timecode of the
    // first frame of the video) belongs to it: not to the next file
    if DoViIsEditable(Subtitles) then
      MPVSetVideoOffset(0);

    SubtitleInfo.Text.FileName := '';
    SubtitleInfo.Translation.FileName := '';
    if not LoadingProject then
    begin
      CurrentProjectFile := '';
      FreeAndNil(PendingProjectState);
    end;
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
        //if Workspace.WorkMode = wmFrames then // ask for FPS?
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

{ Notes of non Tero formats (SRT, etc. can't store them) }

// -----------------------------------------------------------------------------

const
  NotesFileHeader = '# Tero Subtitler notes';

// hidden file next to the subtitle: ".name.srt.notes"
function NotesFileName(const FileName: String): String;
begin
  Result := ExtractFilePath(FileName) + '.' + ExtractFileName(FileName) + '.notes';
end;

// -----------------------------------------------------------------------------

function EscapeNote(const S: String): String;
begin
  Result := S.Replace('\', '\\', [rfReplaceAll]);
  Result := Result.Replace(#13#10, '\n', [rfReplaceAll]);
  Result := Result.Replace(#10, '\n', [rfReplaceAll]);
  Result := Result.Replace(#13, '\n', [rfReplaceAll]);
  Result := Result.Replace(#9, '\t', [rfReplaceAll]);
end;

// -----------------------------------------------------------------------------

function UnescapeNote(const S: String): String;
var
  i: Integer;
begin
  Result := '';
  i := 1;
  while i <= Length(S) do
  begin
    if (S[i] = '\') and (i < Length(S)) then
    begin
      Inc(i);
      case S[i] of
        'n': Result := Result + LineEnding;
        't': Result := Result + #9;
      else
        Result := Result + S[i];
      end;
    end
    else
      Result := Result + S[i];
    Inc(i);
  end;
end;

// -----------------------------------------------------------------------------

procedure SaveNotesFile(const FileName: String);
var
  sl: TStringList;
  f: String;
  i: Integer;
begin
  f := NotesFileName(FileName);
  sl := TStringList.Create;
  try
    for i := 0 to Subtitles.Count-1 do
      if not Subtitles[i].Notes.IsEmpty then
        sl.Add(Format('%d'#9'%d'#9'%s', [i, Subtitles[i].InitialTime, EscapeNote(Subtitles[i].Notes)]));

    if sl.Count = 0 then
    begin
      if FileExists(f) then DeleteFile(f);
      Exit;
    end;

    sl.Insert(0, NotesFileHeader);
    try
      {$IFDEF WINDOWS}
      if FileExists(f) then FileSetAttr(f, 0); // a hidden file can't be overwritten
      {$ENDIF}
      sl.SaveToFile(f);
      {$IFDEF WINDOWS}
      FileSetAttr(f, faHidden);
      {$ENDIF}
    except
    end;
  finally
    sl.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure LoadNotesFile(const FileName: String);
const
  MaxDiff = 100; // ms, times can change a bit after a save/load round trip
var
  sl: TStringList;
  f: String;
  i, x, idx, t, d, best: Integer;
  parts: TStringArray;

  function TimeMatches(const AIndex: Integer): Boolean;
  begin
    Result := Subtitles.ValidIndex(AIndex) and (Abs(Subtitles[AIndex].InitialTime - t) <= MaxDiff);
  end;

begin
  f := NotesFileName(FileName);
  if not FileExists(f) then Exit;

  sl := TStringList.Create;
  try
    try
      sl.LoadFromFile(f);
    except
      Exit;
    end;
    if (sl.Count = 0) or (sl[0] <> NotesFileHeader) then Exit;

    for i := 1 to sl.Count-1 do
    begin
      parts := sl[i].Split([#9], 3);
      if Length(parts) < 3 then Continue;
      idx := StrToIntDef(parts[0], -1);
      t   := StrToIntDef(parts[1], -1);
      if t < 0 then Continue;

      // same entry, or the nearest one with that initial time (lines added/removed outside)
      if not TimeMatches(idx) then
      begin
        idx := -1;
        best := MaxDiff + 1;
        for x := 0 to Subtitles.Count-1 do
        begin
          d := Abs(Subtitles[x].InitialTime - t);
          if d < best then
          begin
            best := d;
            idx := x;
          end;
        end;
      end;

      if Subtitles.ValidIndex(idx) then
        Subtitles.ItemPointer[idx]^.Notes := UnescapeNote(parts[2]);
    end;
  finally
    sl.Free;
  end;
end;

// -----------------------------------------------------------------------------

// try to guess the fps of a subtitle with times in ms (not frames or smpte),
// if all times fall on the frames of one rate, returns its DefFPSList index
function GuessFPSFromFile(const FileName: String; const AEncoding: TEncoding): Integer;
const
  Candidates : array[0..9] of Integer = (0, 1, 2, 3, 5, 6, 7, 8, 9, 11); // DefFPSList
  MinTimes   = 10;
var
  Sub1, Sub2 : TUWSubtitles;
  FPSA, FPSB : TFPSInfo;
  Times      : array of Integer;
  i, c, n, Matches : Integer;
  Tolerance, f, Err : Double;
  OnlyCS : Boolean;

  function FrameError(const ATime: Integer; const AFPS: Double): Double;
  begin
    f := ATime * AFPS / 1000;
    Result := Abs(ATime - Round(f) * 1000 / AFPS);
  end;

begin
  Result := -1;
  Times  := NIL;
  Sub1 := TUWSubtitles.Create;
  Sub2 := TUWSubtitles.Create;
  try
    // load twice with different fps, if the times change they are in frames
    FPSA.FPS := 25; FPSA.Mode := tcNDF;
    FPSB.FPS := 30; FPSB.Mode := tcNDF;
    if not Sub1.LoadFromFile(FileName, AEncoding, FPSA) or
      not Sub2.LoadFromFile(FileName, AEncoding, FPSB) or (Sub1.Count <> Sub2.Count) then
      Exit;

    SetLength(Times, Sub1.Count * 2);
    n := 0;
    for i := 0 to Sub1.Count-1 do
    begin
      if (Sub1[i].InitialTime <> Sub2[i].InitialTime) or (Sub1[i].FinalTime <> Sub2[i].FinalTime) then
        Exit;
      // time 0 is a frame in any rate
      if Sub1[i].InitialTime > 0 then
      begin
        Times[n] := Sub1[i].InitialTime;
        Inc(n);
      end;
      if Sub1[i].FinalTime > 0 then
      begin
        Times[n] := Sub1[i].FinalTime;
        Inc(n);
      end;
    end;
    SetLength(Times, n);

    if Length(Times) < MinTimes then Exit;

    // centiseconds (e.g. ass) are rounded to 10 ms
    OnlyCS := True;
    for i := 0 to High(Times) do
      if Times[i] mod 10 <> 0 then
      begin
        OnlyCS := False;
        Break;
      end;

    if OnlyCS then
      Tolerance := 5.01
    else
      Tolerance := 1.01;

    // lower rates first (all the times of 25 fps are also frames of 50 fps)
    for c := 0 to High(Candidates) do
    begin
      n := Candidates[c];
      Matches := 0;
      for i := 0 to High(Times) do
      begin
        Err := FrameError(Times[i], DefFPSList[n].FPS);
        // ntsc rates are 24000/1001, 30000/1001...
        if not IsInteger(DefFPSList[n].FPS) then
          Err := Min(Err, FrameError(Times[i], Round(DefFPSList[n].FPS) * 1000 / 1001));
        if Err <= Tolerance then Inc(Matches);
      end;

      if Matches >= Length(Times) * 0.95 then
        Exit(n);
    end;
  finally
    Sub1.Free;
    Sub2.Free;
  end;
end;

// -----------------------------------------------------------------------------

// Opened in frames mode (SMPTE/frames is the default now), a file in
// milliseconds (SRT, WebVTT...) can have timings between two frames: they
// show the frame they round to, but the gaps/durations calculated in ms can
// be one frame off (e.g. chaining). We offer to round them, only:
// - in frames mode, with a file in ms (not one in frames/SMPTE),
// - when some timing is not already on a frame,
// - with the fps of the video, once it's loaded: rounding with the wrong fps
//   would move every timing. If the fps of the video and the project's differ
//   nothing is done (the user changes the fps first and uses Round times)

procedure ApplyRoundTimesOnLoad(const Item: PUWSubtitleItem; const Index: Integer);
var
  it, ft: Integer;
begin
  with Item^ do
  begin
    // to the frame the list already shows (MSToHHMMSSFFTime rounds the same)
    it := RoundTimeWithFrames(InitialTime, Workspace.FPS.OutputFPS.FPS);
    ft := RoundTimeWithFrames(FinalTime, Workspace.FPS.OutputFPS.FPS);
    if (it <> InitialTime) or (ft <> FinalTime) then
      SetSubtitleTimes(Index, it, ft, False, False);
  end;
end;

procedure CheckRoundTimesOnLoad;
var
  i, n, it, ft: Integer;
  FPS, VideoFPS: Double;
begin
  if not RoundTimesOnLoadPending then Exit;
  RoundTimesOnLoadPending := False;

  if (Workspace.WorkMode <> wmFrames) or (Subtitles.Count = 0) then Exit;
  FPS := Workspace.FPS.OutputFPS.FPS;
  if FPS <= 0 then Exit;

  if frmMain.MPV.IsMediaLoaded then
  begin
    VideoFPS := frmMain.MPV.GetVideoFPS;
    if (VideoFPS > 0) and (Abs(VideoFPS - FPS) > 0.01) then Exit;
  end;

  n := 0;
  for i := 0 to Subtitles.Count-1 do
  begin
    it := RoundTimeWithFrames(Subtitles[i].InitialTime, FPS);
    ft := RoundTimeWithFrames(Subtitles[i].FinalTime, FPS);
    // 1 ms: the ms of a frame are rounded too
    if (Abs(it - Subtitles[i].InitialTime) > 1) or (Abs(ft - Subtitles[i].FinalTime) > 1) then
      Inc(n);
  end;
  if n = 0 then Exit;

  if CustomQuestionDialog(lngRoundTimesOnLoadTitle, Format(lngRoundTimesOnLoad,
    [n, SingleToStr(FPS, FormatSettings)]), [dbYes, dbNo]) = mrYes then
    VSTDoLoop(frmMain.VST, @ApplyRoundTimesOnLoad, dlAll, True, True);
end;

// -----------------------------------------------------------------------------

procedure LoadSubtitle(const FileName: String; const AFormat: TUWSubtitleFormats = sfInvalid; const AEncoding: TEncoding = NIL; const AFPS: Single = -1; const AKeepVideoOpen: Boolean = False; const AVideoFile: String = ''; const AddToMRU: Boolean = True);
var
  _FPS: TFPSInfo;
  MRUInfoObject: TMRUInfoObject;
  VFisLoaded: Boolean;
  _Encoding: TEncoding = NIL;
  _EncIndex: Integer;
  IsBinary: Boolean;
  i, _FPSIndex: Integer;
  _Format: TUWSubtitleFormats;
  _TimeBase: TSubtitleTimeBase;
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

  _FPS := Workspace.FPS.InputFPS;
  _FPS.FPS := AFPS;
  if _FPS.FPS = -1 then _FPS := Workspace.FPS.DefFPS;

  if not FileExists(FileName) then
  begin
    ShowErrorMessageDialog(lngFileNotFound);
    Exit;
  end;

  _Encoding := AEncoding;
  IsBinary := IsBinaryFormat(FileName);

  if (_Encoding = NIL) and AppOptions.AskForInputEncoding and not IsBinary then
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

        //_EncIndex := formCustomSelectDlg.ExecuteDialog('', lngSelectEncodingToUse, Items, _EncIndex, True);
        _EncIndex := formCustomSelectDlgWithPreview.ExecuteDialog('', lngSelectEncodingToUse, Items, FileName,_EncIndex, True);
        _Encoding := GetCachedEncoding(Encodings[_EncIndex].CPID);
      end;
    end;
  end;

  if AppOptions.AskForInputFPS and Subtitles.GetFormatInfoFromFile(FileName, _Format, _TimeBase) and ((_TimeBase = stbMedia) and (_Format <> sfSoftNI) and (_Format <> sfDolbyVision)) and (AFPS = -1) and // not chosen in our file dialog
    (not IsBinary or SameText(ExtractFileExt(FileName), '.890') or SameText(ExtractFileExt(FileName), '.cap') or
    SameText(ExtractFileExt(FileName), '.asc')) then
  begin
    with frmMain.cboInputFPS do
    begin
      // times in ms that match the frames of a rate, suggest it
      if not IsBinary then
        _FPSIndex := GuessFPSFromFile(FileName, _Encoding)
      else
        _FPSIndex := -1;

      if (_FPSIndex >= 0) and (CustomQuestionDialog(lngSelectFPSToUse,
        Format(lngFPSGuessed, [Items[_FPSIndex]]), [dbYes, dbNo]) = mrYes) then
        ItemIndex := _FPSIndex
      else
        ItemIndex := formCustomSelectDlg.ExecuteDialog('', lngSelectFPSToUse, Items, ItemIndex, True);
      _FPS := DefFPSList[ItemIndex];
      frmMain.cboInputFPSSelect(NIL);
    end;
  end;

  if Subtitles.LoadFromFile(FileName, _Encoding, _FPS, AFormat) then
  begin
    // Dolby Vision XML: the entries are in record time, the video starts at
    // the first shot (it can be changed in Video > Dolby Vision metadata)
    if DoViIsEditable(Subtitles) and (Subtitles.Count > 0) then
      MPVSetVideoOffset(Subtitles[0].InitialTime);
    if Subtitles.Format <> sfTeroSubtitler then
      LoadNotesFile(FileName);
    frmMain.VST.RootNodeCount := Subtitles.Count;
    frmMain.cboEncoding.ItemIndex := GetEncodingIndex(Subtitles.CodePage);
    frmMain.cboFormat.ItemIndex := Integer(Subtitles.Format)-1;
    if (Subtitles.FrameRate > 0) then
    begin
      _FPSIndex := -1;
      for i := 0 to High(DefFPSList) do
        if SameValue(DefFPSList[i].FPS, Subtitles.FrameRate, 0.0001) then
        begin
          // same fps and timecode mode, or at least the same fps
          if (DefFPSList[i].Mode = Subtitles.TimecodeMode) then
          begin
            _FPSIndex := i;
            Break;
          end
          else if _FPSIndex < 0 then
            _FPSIndex := i;
        end;

      if _FPSIndex >= 0 then
      begin
        frmMain.cboInputFPS.ItemIndex := _FPSIndex;
        _FPS := DefFPSList[_FPSIndex];
      end;
    end;
    frmMain.cboInputFPSSelect(NIL);

    if not Subtitles.IsSMPTESupported then
    begin
      // the times are not converted (x1.001 here, the inverse of the /1.001
      // of the load in drop frame): they are real, also in SMPTE (see
      // TimecodeLabelToMS)
      if (Workspace.WorkMode = wmFrames) and (Workspace.FPS.InputFPS.Mode = tcDF) and not IsInteger(Subtitles.FrameRate) then
        SetSMPTEMode(True)
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
          {if (Workspace.FPS.InputFPS.Mode = tcDF) and not IsInteger(Subtitles.FrameRate) then
          begin
            Subtitles.ConvertTimesToSMPTE(False);
          end;}

          SetSMPTEMode(not IsInteger(Subtitles.FrameRate));
        end;
      end
      else if (Subtitles.TimeBase = stbMedia) then
        SetSMPTEMode(False);
    end;

    DoAutoCheckErrors;

    // timings in ms opened in frames mode: offer to round them to frames
    // (with the fps of the video: after it's loaded, see below)
    RoundTimesOnLoadPending := (Workspace.WorkMode = wmFrames) and
      (Subtitles.TimeBase = stbMedia) and not (Subtitles.Format in [sfTeroSubtitler, sfDolbyVision]);

    SubtitleInfo.Text.FileName := FileName;
    RefreshAppTitle;

    if frmMain.LayoutSource.Visible then
    begin
      frmMain.mmoSourceView.Tag := TAG_CONTROL_UPDATE;
      frmMain.mmoSourceView.Text := Subtitles.SaveToString(Workspace.FPS.OutputFPS.FPS, NIL, TUWSubtitleFormats(frmMain.cboFormat.ItemIndex+1), smText);
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
      VFisLoaded := LoadVideo(AVideoFile) or VFisLoaded
    else if not AKeepVideoOpen and not VFisLoaded then
    begin
      if not LoadVideo(GetMediaFileNameIfExists(FileName, TVideoExts)) then
        VFisLoaded := LoadVideo(GetMediaFileNameIfExists(FileName, TAudioExts))
      else
        VFisLoaded := True;
    end;

    // without a video to load the fps is already the final one (with one,
    // MPVFileLoaded asks once it's loaded)
    if RoundTimesOnLoadPending and not VFisLoaded then
      Application.QueueAsyncCall(@frmMain.DoCheckRoundTimesOnLoad, 0);

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
  _FPS : TFPSInfo;
  Subs : TUWSubtitles;
  i, x : Integer;
begin
  if Subtitles.Count = 0 then Exit;

  _FPS.FPS := AFPS;
  _FPS.Mode := tcNDF;
  if _FPS.FPS = -1 then _FPS := Workspace.FPS.DefFPS;

  Subs := TUWSubtitles.Create;
  try
    if Subs.LoadFromFile(FileName, AEncoding, _FPS, AFormat) then
    begin
      for i := 0 to Subs.Count-1 do
      begin
        x := Subtitles.FindInsertPos(Subs[i]);
        // the extra info is imported only when both are of the same type
        if Subs.ExtraInfoType = Subtitles.ExtraInfoType then
          InsertSubtitle(x, Subs[i], False, False, Subs.CopyExtraInfo(i))
        else
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

type
  // formato con tags TS de una entrada, para validar contra la spec
  TSpecTag = (stBold, stUnderline, stStrikeout, stColor);
  TSpecTags = set of TSpecTag;

// Qué formato usa el texto ({\b1}, {\u1}, {\s1}, {\c&BBGGRR&}; el blanco es el
// color por defecto y no cuenta)
function SpecTagsUsed(const Txt: String): TSpecTags;
var
  i, j : Integer;
  Tag, Hex : String;
begin
  Result := [];
  i := Pos('{\', Txt);
  while i > 0 do
  begin
    j := Pos('}', Txt, i+2);
    if j = 0 then Break;
    for Tag in Copy(Txt, i+2, j-i-2).Split(['\']) do
      if Tag <> '' then
      begin
        if (Length(Tag) >= 2) and (Tag[1] in ['b', 'u', 's']) and (Tag[2] in ['1'..'9']) then
          case Tag[1] of
            'b': Include(Result, stBold);
            'u': Include(Result, stUnderline);
            's': Include(Result, stStrikeout);
          end
        else if (Copy(Tag, 1, 2) = 'c&') or (Copy(Tag, 1, 3) = '1c&') then
        begin
          Hex := UpperCase(Copy(Tag, Pos('&', Tag)+1, MaxInt).Replace('&', '').Replace('H', ''));
          if (Hex <> '') and (Hex <> 'FFFFFF') then Include(Result, stColor);
        end;
      end;
    i := Pos('{\', Txt, j+1);
  end;
end;

// -----------------------------------------------------------------------------

// Antes de guardar en un formato con especificación de entrega (iTT de Apple,
// Netflix Timed Text, EBU STL): lo que la spec no permite y no tiene arreglo
// (frame rate, más de dos líneas) impide guardar; lo que el exportador cambia
// o quita para cumplirla se avisa todo junto, con las entradas, y se puede
// guardar igual. Antes solo iTT se validaba: la alineación vertical al medio
// impedía guardar con un mensaje que decía que la alineación vertical no
// estaba permitida (arriba y abajo sí lo están), y la alineación
// izquierda/derecha y el tachado se perdían sin ningún aviso, igual que todo
// el formato salvo la cursiva en Netflix y la negrita y el tachado en EBU

function AllowSaveFormat(const AFormat: TUWSubtitleFormats; const SubtitleMode: TSubtitleMode = smText): Boolean;
const
  MaxNumbers = 20;

  // los números de las entradas (1, 2, 3, ... (n)) para el mensaje
  function EntriesToStr(const AIdxs: TIntegerDynArray): String;
  var
    j : Integer;
  begin
    Result := '';
    for j := 0 to Min(High(AIdxs), MaxNumbers-1) do
    begin
      if j > 0 then Result += ', ';
      Result += IntToStr(AIdxs[j]+1);
    end;
    if Length(AIdxs) > MaxNumbers then
      Result += Format(', ... (%d)', [Length(AIdxs)]);
  end;

  // which entries: their numbers in the message and selected in the list,
  // to find them (before, only "there are entries...")
  function CheckEntries(const AIdxs: TIntegerDynArray; const AError: String): Boolean;
  begin
    Result := Length(AIdxs) = 0;
    if Result then Exit;

    VSTSelectNodes(frmMain.VST, AIdxs, True);
    ShowErrorMessageDialog(lngSubtitleSpecificationError + LineEnding + LineEnding +
      AError + LineEnding + Format(lngSubtitleSpecificationEntries, [EntriesToStr(AIdxs)]), '', False, False);
  end;

  procedure AddIdx(var AIdxs: TIntegerDynArray; const AIdx: Integer);
  begin
    if (Length(AIdxs) = 0) or (AIdxs[High(AIdxs)] <> AIdx) then
      Insert(AIdx, AIdxs, Length(AIdxs));
  end;

  // la línea más larga, en caracteres visibles (sin los tags)
  function LongestLine(const Txt: String): Integer;
  var
    Line : String;
  begin
    Result := 0;
    for Line in RemoveTSTags(Txt).Split([LineEnding]) do
      Result := Max(Result, UTF8Length(Line));
  end;

type
  TSpecWarning = (swMiddleToBottom, swMiddleKept, swHAlignCentered, swBold,
    swPartialBold, swUnderline, swStrikeout, swColor, swPartialColor, swLineLength);

var
  F, Msg, Txt, SpecName : String;
  i, MaxChars : Integer;
  Partial : TITTPartialStyles;
  BR, All : TIntegerDynArray;
  W : array[TSpecWarning] of TIntegerDynArray;
  Affected : array of Boolean;
  Lost : TSpecTags;
  sw : TSpecWarning;
  WText : array[TSpecWarning] of String;

begin
  Result := False;
  if not (AFormat in [sfITunesTimedText, sfNetflixTimedText, sfEBU]) then Exit(True);

  if AFormat = sfITunesTimedText then
  begin
    F := SingleToStr(Workspace.FPS.OutputFPS.FPS, FormatSettings).Replace(FormatSettings.DecimalSeparator, '.');
    // 30 NDF (frameRate 30, frameRateMultiplier "1 1") is also accepted by Apple
    if ((F <> '29.97') and (F <> '30') and (F <> '25') and (F <> '24') and (F <> '23.976')) then
    begin
      ShowErrorMessageDialog(Format('%s%s%s', [lngSubtitleSpecificationError, LineEnding+LineEnding, lngSubtitleSpecificationErrorFPS]), '', False, False);
      Exit;
    end;
  end;

  // lo que cada exportador cambia o quita (ver sus SaveSubtitle):
  // - iTT: solo regiones top/bottom (el medio va abajo), tts:textAlign solo
  //   center, sin tachado, y la negrita y el color solo en toda la entrada:
  //   los parciales se quitan, como en Final Cut Pro (#510)
  // - Netflix: arriba o abajo y centrado (Timed Text Style Guide: General
  //   Requirements; el medio se escribe igual), solo cursiva
  //   (StripNonItalicTSFormatting quita negrita, subrayado, tachado y color)
  // - EBU STL: sin negrita ni tachado (no tienen código teletexto), y el
  //   máximo de caracteres por fila (MNC) de las propiedades del formato
  MaxChars := 0;
  if AFormat = sfEBU then
    MaxChars := StrToIntDef(Trim(String(Subtitles.FormatProperties^.EBU.MaxNumberDisplayableChars)), 0);

  BR := NIL; All := NIL;
  for sw := Low(TSpecWarning) to High(TSpecWarning) do W[sw] := NIL;
  Affected := NIL;
  SetLength(Affected, Subtitles.Count);

  for i := 0 to Subtitles.Count-1 do
  begin
    Txt := iff(SubtitleMode = smText, Subtitles[i].Text, Subtitles[i].Translation);

    // iTT y Netflix: dos líneas como máximo
    if (AFormat <> sfEBU) and (LineCount(Txt, LineEnding) > 2) then
      AddIdx(BR, i);

    if AFormat <> sfEBU then
    begin
      if Subtitles[i].VAlign = svaCenter then
        if AFormat = sfITunesTimedText then
          AddIdx(W[swMiddleToBottom], i)
        else
          AddIdx(W[swMiddleKept], i);
      if Subtitles[i].Align in [shaLeft, shaRight] then
        AddIdx(W[swHAlignCentered], i);
    end;

    case AFormat of
      sfITunesTimedText  : Lost := SpecTagsUsed(Txt) * [stStrikeout];
      sfNetflixTimedText : Lost := SpecTagsUsed(Txt);
      sfEBU              : Lost := SpecTagsUsed(Txt) * [stBold, stStrikeout];
    else
      Lost := [];
    end;
    if stBold in Lost then AddIdx(W[swBold], i);
    if stUnderline in Lost then AddIdx(W[swUnderline], i);
    if stStrikeout in Lost then AddIdx(W[swStrikeout], i);
    if stColor in Lost then AddIdx(W[swColor], i);

    if AFormat = sfITunesTimedText then
    begin
      Partial := ITTPartialStyles(Txt);
      if ipsBold in Partial then AddIdx(W[swPartialBold], i);
      if ipsColor in Partial then AddIdx(W[swPartialColor], i);
    end;

    if (MaxChars > 0) and (LongestLine(Txt) > MaxChars) then
      AddIdx(W[swLineLength], i);
  end;

  if not CheckEntries(BR, lngSubtitleSpecificationErrorBR) then Exit;

  WText[swMiddleToBottom] := lngSpecMiddleToBottom;
  WText[swMiddleKept]     := lngSpecMiddleKept;
  WText[swHAlignCentered] := lngSpecHAlignCentered;
  WText[swBold]           := lngSpecBold;
  WText[swPartialBold]    := lngSpecPartialBold;
  WText[swUnderline]      := lngSpecUnderline;
  WText[swStrikeout]      := lngSpecStrikeout;
  WText[swColor]          := lngSpecColor;
  WText[swPartialColor]   := lngSpecPartialColor;
  WText[swLineLength]     := Format(lngSpecLineLength, [MaxChars]);

  Msg := '';
  for sw := Low(TSpecWarning) to High(TSpecWarning) do
    if Length(W[sw]) > 0 then
    begin
      Msg += LineEnding + LineEnding + '- ' + WText[sw] + LineEnding +
        Format(lngSpecEntries, [EntriesToStr(W[sw])]);
      for i in W[sw] do Affected[i] := True;
    end;

  if Msg <> '' then
  begin
    // todas las afectadas seleccionadas en la lista, para encontrarlas
    for i := 0 to High(Affected) do
      if Affected[i] then AddIdx(All, i);
    VSTSelectNodes(frmMain.VST, All, True);

    case AFormat of
      sfITunesTimedText  : SpecName := 'iTunes Timed Text (iTT)';
      sfNetflixTimedText : SpecName := 'Netflix Timed Text';
    else
      SpecName := 'EBU STL';
    end;
    if CustomQuestionDialog(lngDoViSaveAnyway, Format(lngSpecWillChange, [SpecName]) + Msg, [dbYes, dbNo]) <> mrYes then
      Exit;
  end;

  Result := True;
end;

// -----------------------------------------------------------------------------

function AllowSaveDolbyVision: Boolean;
var
  Problems: TDoViProblems;
begin
  Result := False;
  if not DoViIsEditable(Subtitles) then
  begin
    ShowErrorMessageDialog(lngDoViSaveOnlyDoVi);
    Exit;
  end;

  // the shots of the track must be contiguous
  Problems := DoViCheckProblems(Subtitles);
  if Problems.First >= 0 then
  begin
    VSTSelectNode(frmMain.VST, Problems.First, True, True);
    if CustomQuestionDialog(lngDoViSaveAnyway, SysUtils.Format(lngDoViSaveProblems, [Problems.Gaps,
      Problems.Overlaps, Problems.Empty, Problems.NoShot, Problems.First+1]), [dbYes, dbNo]) <> mrYes then
      Exit;
  end;

  Result := True;
end;

// -----------------------------------------------------------------------------

procedure SaveSubtitle(const FileName: String; const Format: TUWSubtitleFormats; const SubtitleMode: TSubtitleMode; const AEncoding: TEncoding = NIL; const AFPS: Single = -1);
var
  _FPS      : TFPSInfo;
  _Encoding : TEncoding;
  S, ext : String;
begin
  _FPS.FPS := AFPS;
  _FPS.Mode := tcNDF;
  if _FPS.FPS = -1 then
    _FPS := Workspace.FPS.OutputFPS
  else if SameValue(_FPS.FPS, Workspace.FPS.OutputFPS.FPS, 0.001) then
    // the timecode mode (drop frame) of the hh:mm:ss:ff of the format
    _FPS.Mode := Workspace.FPS.OutputFPS.Mode;
  _Encoding := AEncoding;
  //if _Encoding = NIL then _Encoding := GetCachedEncoding(Encodings[frmMain.cboEncoding.ItemIndex].CPID);
  if _Encoding = NIL then _Encoding := GetCachedEncoding(Encodings[Workspace.DefEncoding].CPID); // default encoding

  if Workspace.SMPTE then
    Subtitles.TimeBase := stbSMPTE
  else
    Subtitles.TimeBase := stbMedia;

  // Dolby Vision XML: only the times of the shots of an opened XML are written
  if (Format = sfDolbyVision) and not AllowSaveDolbyVision then Exit;

  S := FileName;
  if (Format = sfSpreadsheet) then
  begin
    ext := LowerCase(ExtractFileExt(FileName));
    if (ext <> STR_EXCEL_EXTENSION) and (ext <> STR_OOXML_EXCEL_EXTENSION) and (ext <> STR_OPENDOCUMENT_CALC_EXTENSION) then
      S := ChangeFileExt(FileName, STR_EXCEL_EXTENSION)
  end;

  if CheckIfValidFileName(S) and Subtitles.SaveToFile(S, _FPS, _Encoding, Format, SubtitleMode) then
  begin
    if (SubtitleMode = smText) then
    begin
      SubtitleInfo.Text.FileName := S;
      if Format <> sfTeroSubtitler then
        SaveNotesFile(S);
    end
    else
      SubtitleInfo.Translation.FileName := S;

    SubtitleChangedReset(SubtitleMode);
    RefreshAppTitle;

    with frmMain do
      MRU.Add(S, MPV.FileName, WAVE.FileName, VSTFocusedNode(VST), MPV.GetMediaPosInMs, WAVE.GetPlayCursorMS, MPV.IsPlaying);
    // keep the format of what was written, so Save doesn't write another
    // format into this file (it happened after a "Save as" in another format)
    if SubtitleMode = smText then
      Subtitles.Format := Format;
  end
  else
    ShowErrorMessageDialog(lngSaveSubtitleError);
end;

// -----------------------------------------------------------------------------

procedure SaveSubtitleAutoBackup;
var
  AFPS      : TFPSInfo;
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
  _FPS      : TFPSInfo;
  _Encoding : TEncoding;
  Subs      : TUWSubtitles;
  i         : Integer;
begin
  _FPS.FPS := AFPS;
  _FPS.Mode := tcNDF;
  if _FPS.FPS = -1 then
    _FPS := Workspace.FPS.OutputFPS
  else if SameValue(_FPS.FPS, Workspace.FPS.OutputFPS.FPS, 0.001) then
    // the timecode mode (drop frame) of the hh:mm:ss:ff of the format
    _FPS.Mode := Workspace.FPS.OutputFPS.Mode;
  _Encoding := AEncoding;
  //if _Encoding = NIL then  _Encoding := GetCachedEncoding(Encodings[frmMain.cboEncoding.ItemIndex].CPID);
  if _Encoding = NIL then _Encoding := GetCachedEncoding(Encodings[Workspace.DefEncoding].CPID); // default encoding

  Subs := TUWSubtitles.Create;
  try
    // copy the values, Subs owns (and disposes) its own pointer
    Subs.FormatProperties^ := Subtitles.FormatProperties^;

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
  if _FPS = -1 then _FPS := Workspace.FPS.OutputFPS.FPS;
  _Encoding := AEncoding;
  //if _Encoding = NIL then  _Encoding := GetCachedEncoding(Encodings[frmMain.cboEncoding.ItemIndex].CPID);
  if _Encoding = NIL then _Encoding := GetCachedEncoding(Encodings[Workspace.DefEncoding].CPID); // default encoding

  txt := TUWStringList.Create;
  try
    for i := 0 to Subtitles.Count-1 do
      txt.Add(iff(SubtitleMode = smText, Subtitles[i].Text, Subtitles[i].Translation));

    // without formatting: the whole text in a single block. Any kind of line
    // break is removed (texts may have #10 or #13#10 regardless of the OS,
    // e.g. iTT read from XML), and repeated spaces are collapsed
    if not Formatted then
      txt.Text := Trim(ReplaceRegExpr('[ \t]{2,}', ReplaceRegExpr('\r\n|\r|\n', RemoveTSTags(txt.Text), ' ', False), ' ', False));

    if CheckIfValidFileName(FileName) then
      txt.SaveToFile(FileName, _Encoding)
    else
      ShowErrorMessageDialog(lngSaveSubtitleError);
  finally
    txt.Free;
  end;
end;

// -----------------------------------------------------------------------------

function LoadVideo(const FileName: String; const Pos: Int64 = 0): Boolean;
begin
  Result := False;
  with frmMain do
    if FileExists(FileName) then
    begin
      Result := True;

      if not MPVOptions.KeepVideoOpen then
        actCloseMediaExecute(NIL)
      else
        MPVOptions.KeepVideoOpen := False;

      if not actVideoPreview.Checked then
        actVideoPreview.Execute;

      MPV.Play(FileName, Pos);
      actMediaChangePlayRateExecute(NIL);
    end;
end;

// -----------------------------------------------------------------------------

function LoadAudio(const FileName: String): Boolean;
begin
  Result := False;
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
      Result := True;
      LoadShotChangesFileName(FileName);
      EnableActionsByTag([TAG_ACTION_AUDIO], True);
    end;

    if (WAVE.IsPeakDataLoaded and not actTimeline.Checked) or
       (not WAVE.IsPeakDataLoaded and actTimeline.Checked) then
      actTimeline.Execute;
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
    if FileName.ToLower.StartsWith('http://') or FileName.ToLower.StartsWith('https://') then
    begin
      // online media: only the waveform extracted before
      s1 := '';
      s2 := ConcatPaths([WaveformsFolder, OnlineMediaFileName(FileName, Exts[i])]);
    end
    else
    begin
      s1 := ChangeFileExt(FileName, Exts[i]);
      s2 := ConcatPaths([WaveformsFolder, ExtractFileName(ChangeFileExt(FileName, Exts[i]))]);
    end;

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
  s, Newest: String;
  i: Integer;
  Age, NewestAge: Int64;
begin
  Result := False;
  // the newest of the files of the video (.edl, .shotchanges, .xml): the
  // first one found was loaded and an old .edl/.xml left in the folder was
  // always loaded instead of the .shotchanges written later (Video > Shot
  // changes, a project, the ones inserted/deleted by hand)
  Newest := '';
  NewestAge := -1;
  for i := 0 to Length(TShotChangeExts)-1 do
  begin
    s := ShotChangesFileName(FileName, TShotChangeExts[i]);
    if FileExists(s) then
    begin
      Age := FileAgeUTF8(s);
      if (Newest = '') or (Age > NewestAge) then
      begin
        Newest := s;
        NewestAge := Age;
      end;
    end;
  end;

  if Newest <> '' then
  begin
    frmMain.WAVE.LoadSceneChangeFile(Newest, (Workspace.WorkMode = wmTime));
    Result := True;
  end;
end;

// -----------------------------------------------------------------------------

// the shot changes of the timeline in the format of their cache: video time
// in ms, as they are (they are not converted with the SMPTE mode). Without
// "-1" first: that one was written with the SMPTE mode, and its times were
// divided by 1.001 in time mode (WAVE.LoadSceneChangeFile still reads those
// files)
procedure GetShotChangesForCache(const AList: TStrings);
var
  sc : TIntegerDynArray;
  i  : Integer;
begin
  AList.Clear;
  sc := frmMain.WAVE.GetSceneChangeList;
  for i := 0 to High(sc) do
    AList.Add((sc[i] - frmMain.WAVE.OffsetMs).ToString);
end;

// -----------------------------------------------------------------------------

// the shot changes inserted/deleted by hand, to the cache of the video: only
// Video > Shot changes (Apply) wrote it, and the next time the video (or a
// subtitle with it) was opened the shot changes were the ones before the
// changes. Only a cache: an error here must not stop anything
procedure SaveShotChangesCache;
var
  s  : String;
  sl : TStringList;
begin
  if (frmMain.MPV.FileName = '') or not FileExists(frmMain.MPV.FileName) then Exit;
  s := ShotChangesFileName(frmMain.MPV.FileName, '.shotchanges');
  sl := TStringList.Create;
  try
    try
      // also empty (all of them were deleted): as the newest file it is
      // the one loaded, not an old .edl/.xml of the same video
      GetShotChangesForCache(sl);
      ForceDirectories(ExtractFileDir(s));
      sl.SaveToFile(s);
    except
    end;
  finally
    sl.Free;
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

function FileExtMatchesFormat(const AFileName: String; const AFormat: TUWSubtitleFormats): Boolean;
var
  ext, exts: String;
begin
  ext  := LowerCase(ExtractFileExt(AFileName));
  exts := LowerCase(Subtitles.FormatExtensions(AFormat)) + ';';
  // unknown format info: don't block the save
  Result := (exts = ';') or (ext = '') or (Pos('*' + ext + ';', exts) > 0);
end;

// -----------------------------------------------------------------------------

function GetSubtitleFormatExt(const AFormat: TUWSubtitleFormats): String;
begin
  Result := ExtractFileExt(Subtitles.FixFileNameExtension('a', AFormat));
end;

// -----------------------------------------------------------------------------

function EnsureSaveFileExt(const AFileName, AExt: String; const AKnownFilter: String = ''): String;
var
  Ext, Known, Inner: String;

  function IsKnownExt(const AValue: String): Boolean;
  begin
    Result := (AValue.Length > 1) and not Known.IsEmpty and
      ((Pos('*' + AValue + '|', Known) > 0) or (Pos('*' + AValue + ';', Known) > 0) or
      (Pos('*' + AValue + ')', Known) > 0) or Known.EndsWith('*' + AValue));
  end;

begin
  // Native save dialogs (macOS in particular) don't always append the
  // extension of the selected filter. Append it when the name has none, or
  // when its "extension" is not a known one (e.g. "Movie.S01E02").
  // Returns '' if the user cancels overwriting an existing file.
  Result := AFileName;
  if AFileName.IsEmpty or AExt.IsEmpty then Exit;

  Known := LowerCase(AKnownFilter);
  Ext := LowerCase(ExtractFileExt(AFileName));
  if Ext = LowerCase(AExt) then
  begin
    // the suggested name kept the extension of the file (or of the format
    // before changing it) and the dialog added the new one: "movie.vtt.itt",
    // "movie.itt.itt" -> "movie.itt"
    Inner := LowerCase(ExtractFileExt(ChangeFileExt(AFileName, '')));
    if (Inner = Ext) or IsKnownExt(Inner) then
    begin
      Result := ChangeFileExt(ChangeFileExt(AFileName, ''), AExt);
      if FileExists(Result) and (MessageDlg(rsfdOverwriteFile,
        Format(rsfdFileAlreadyExists, [Result]), mtConfirmation, [mbOK, mbCancel], 0) <> mrOK) then
        Result := '';
    end;
    Exit;
  end;
  if IsKnownExt(Ext) then
    Exit;

  if Result.EndsWith('.') then
    SetLength(Result, Result.Length-1);
  Result := Result + AExt;

  // the dialog only asked about the name without extension
  if FileExists(Result) and (MessageDlg(rsfdOverwriteFile,
    Format(rsfdFileAlreadyExists, [Result]), mtConfirmation, [mbOK, mbCancel], 0) <> mrOK) then
    Result := '';
end;

procedure SaveSubtitleWithDialog(const SubtitleMode: TSubtitleMode = smText);
var
  SD : TSaveDialog;
  CD : TfrmCustomFileDlg;
  s  : String;
begin
  if AppOptions.CheckErrorsBeforeSave and NeedToCheckErrors(Subtitles) then
    if MsgSaveWithErrors = mrNo then
      Exit;

  if AppOptions.UseOwnFileDialog then
  begin
    CD := TfrmCustomFileDlg.Create(NIL);
    try
      CD.FileName := GetSuggestedFileNameForSave;
      if CD.Execute(dmSave) and AllowSaveFormat(CD.Format, SubtitleMode) then
      begin
        s := EnsureSaveFileExt(CD.FileName, GetSubtitleFormatExt(CD.Format), Subtitles.FillDialogFilter(''));
        if not s.IsEmpty then
          SaveSubtitle(s, CD.Format, SubtitleMode, CD.Encoding, CD.FPS);
      end;
    finally
      CD.Free;
    end;
  end
  else
  begin
    SD := TSaveDialog.Create(NIL);
    try
      if (SubtitleMode = smText)
        then SD.Title  := lngSaveSubtitles
        else SD.Title  := lngSaveTranslation;
      if (MRU.Items.Count > 0) then
        SD.InitialDir := ExtractFileDir(MRU.Items[0{MRU.Items.Count -1}]);
      SD.Filter := Subtitles.FillDialogFilter('');
      SD.FilterIndex := frmMain.cboFormat.ItemIndex+1;
      SD.DefaultExt := GetDefaultExtFromFilter(SD.FilterIndex, SD.Filter);
      SD.FileName := GetSuggestedFileNameForSave{$IFDEF DARWIN}+SD.DefaultExt{$ENDIF};
      {$IFDEF UNIX}
      SD.OnTypeChange := @frmMain.DoSaveDialogTypeChange;
      {$ENDIF}

      SD.Options := [ofOverwritePrompt, ofEnableSizing];
      if SD.Execute and AllowSaveFormat(TUWSubtitleFormats(SD.FilterIndex), SubtitleMode) then
      begin
        s := EnsureSaveFileExt(SD.FileName, GetSubtitleFormatExt(TUWSubtitleFormats(SD.FilterIndex)), SD.Filter);
        if not s.IsEmpty then
          SaveSubtitle(s, TUWSubtitleFormats(SD.FilterIndex), SubtitleMode);
       end;
    finally
      frmMain.cboFormat.ItemIndex := Integer(Subtitles.Format)-1;
      SD.Free;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure SaveCurrentSubtitle(const SubtitleMode: TSubtitleMode = smText; const AUpdateProject: Boolean = True);
var
  s: String;
begin
  if (SubtitleMode = smText) then
    s := SubtitleInfo.Text.FileName
  else
    s := SubtitleInfo.Translation.FileName;

  // ask if the current format doesn't match the file extension (e.g. the
  // format combo was changed), instead of writing srt data into a .ass file
  if s.IsEmpty or not FileExtMatchesFormat(s, Subtitles.Format) then
    SaveSubtitleWithDialog(SubtitleMode)
  // the same checks as Save as (iTT/Netflix/EBU): saving again over an .itt
  // didn't warn about the formatting the format can't keep
  else if AllowSaveFormat(Subtitles.Format, SubtitleMode) then
    SaveSubtitle(s, Subtitles.Format, SubtitleMode);

  // a project open: File > Save saves it too (the work: shot changes,
  // thumbnails, state of the player...), if the subtitle was saved
  if AUpdateProject and not CurrentProjectFile.IsEmpty and
    not SubtitleInfo.Text.FileName.IsEmpty and
    (((SubtitleMode = smText) and not SubtitleInfo.Text.Changed) or
     ((SubtitleMode = smTranslation) and not SubtitleInfo.Translation.Changed)) then
    SaveProject(CurrentProjectFile);
end;

// -----------------------------------------------------------------------------

function LoadVideoWithDialog : boolean;
var
  OD : TOpenDialog;
  i  : Integer;
  s  : String;
begin
  Result := False;
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
      Result := True;
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
  AFPSInfo : TFPSInfo;
begin
  if not AFileName.IsEmpty then
  begin
    Sub := TUWSubtitles.Create;
    try
      AFPSInfo := Workspace.FPS.DefFPS;
      if AFPS > 0 then AFPSInfo.FPS := AFPS;
      if Sub.LoadFromFile(AFileName, AEncoding, AFPSInfo, AFormat) then
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

procedure ImportPlainTextWithDialog;
var
  OD : TOpenDialog;
begin
  // a transcription without times: the entries are made from its lines,
  // paragraphs or sentences (to synchronize them with the audio later)
  OD := TOpenDialog.Create(NIL);
  try
    OD.Title   := lngOpenFile;
    OD.Filter  := lngPlainTextFiles + ' (*.txt)|*.txt|' + lngPlainTextAllFiles + ' (*)|*';
    OD.Options := OD.Options + [ofFileMustExist];

    if OD.Execute then
      ShowImportPlainText(OD.FileName);
  finally
    OD.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure ExportMarkedSubtitleWithDialog(const SubtitleMode: TSubtitleMode = smText);
var
  SD : TSaveDialog;
  CD : TfrmCustomFileDlg;
  s  : String;
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
        s := EnsureSaveFileExt(CD.FileName, GetSubtitleFormatExt(CD.Format), Subtitles.FillDialogFilter(''));
        if not s.IsEmpty then
          SaveMarkedSubtitle(s, CD.Format, SubtitleMode, CD.Encoding, CD.FPS);
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
      SD.DefaultExt := GetDefaultExtFromFilter(SD.FilterIndex, SD.Filter);
      SD.FileName := ChangeFileExt(ExtractFileName(SubtitleInfo.Text.FileName), '');
      SD.Options := [ofOverwritePrompt, ofEnableSizing];
      if SD.Execute then
      begin
        s := EnsureSaveFileExt(SD.FileName, GetSubtitleFormatExt(TUWSubtitleFormats(SD.FilterIndex)), SD.Filter);
        if not s.IsEmpty then
          SaveMarkedSubtitle(s, TUWSubtitleFormats(SD.FilterIndex), SubtitleMode);
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
  s : String;
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
        s := EnsureSaveFileExt(CD.FileName, GetSubtitleFormatExt(CD.Format), Subtitles.FillDialogFilter(''));
        if not s.IsEmpty then
          SaveTextOnlySubtitle(s, CD.Format, SubtitleMode, Formatted, CD.Encoding, CD.FPS);
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
      SD.DefaultExt := '.txt';
      SD.FileName := ChangeFileExt(ExtractFileName(SubtitleInfo.Text.FileName), '.txt');
      SD.Options := [ofOverwritePrompt, ofEnableSizing];
      if SD.Execute then
      begin
        s := EnsureSaveFileExt(SD.FileName, '.txt');
        if not s.IsEmpty then
          SaveTextOnlySubtitle(s, TUWSubtitleFormats(SD.FilterIndex), SubtitleMode, Formatted);
      end;
    finally
      SD.Free;
    end;
  end;
end;

// -----------------------------------------------------------------------------

{ Project files }

// -----------------------------------------------------------------------------

// the state of the work saved in the project (Name=Value, all optional):
//  VideoOffset, ViewMode, VideoPreview: applied when it's opened
//  AspectRatio, PanAndScan, Volume, AudioTrack, Filters, WaveformPosition and
//  the position of the video: when its video is loaded
procedure GetProjectState(const AState: TStrings);
var
  tl : TMPVPlayerTrackList;
  i  : Integer;
  s  : String;
begin
  AState.Clear;
  with frmMain do
  begin
    AState.Values['VideoOffset']  := Workspace.VideoOffset.ToString;
    AState.Values['ViewMode']     := Integer(Workspace.ViewMode).ToString;
    AState.Values['VideoPreview'] := SysUtils.BoolToStr(actVideoPreview.Checked, '1', '0');

    if MPV.IsMediaLoaded then
    begin
      AState.Values['AspectRatio'] := Integer(MPV.AspectRatio).ToString;
      AState.Values['PanAndScan']  := SysUtils.BoolToStr(actPanAndScan.Checked, '1', '0');
      AState.Values['Volume']      := MPVOptions.Volume.Percent.ToString;

      tl := MPV.TrackList;
      for i := 0 to High(tl) do
        if (tl[i].Kind = ttAudio) and tl[i].Selected then
        begin
          AState.Values['AudioTrack'] := tl[i].ID.ToString;
          Break;
        end;

      // the video/audio filters checked (by the name of their action)
      s := '';
      for i := 0 to ComponentCount-1 do
        if (Components[i] is TAction) and TAction(Components[i]).Checked and
          (Components[i].Name.StartsWith('actVideoFilter') or Components[i].Name.StartsWith('actAudioFilter')) then
          s := s + Components[i].Name + ';';
      // ';' when none (an empty value would remove the key: "none" must
      // uncheck the filters when it's opened)
      if s = '' then s := ';';
      AState.Values['Filters'] := s;
    end;

    if WAVE.IsPeakDataLoaded then
      AState.Values['WaveformPosition'] := WAVE.GetPlayCursorMS.ToString;
  end;
end;

// -----------------------------------------------------------------------------

procedure ApplyProjectPlayerState;
var
  st : TStringList;
  tl : TMPVPlayerTrackList;
  i, x : Integer;
  s  : String;
begin
  if not Assigned(PendingProjectState) then Exit;
  // the video of the project (the recent files can open another one first)
  if not SameFileName(frmMain.MPV.FileName, PendingProjectVideo) then Exit;

  st := PendingProjectState;
  PendingProjectState := NIL;
  try
    with frmMain do
    begin
      if st.IndexOfName('AspectRatio') >= 0 then
      begin
        x := StrToIntDef(st.Values['AspectRatio'], 0);
        if (x >= Ord(Low(TMPVPlayerVideoAspectRatio))) and (x <= Ord(High(TMPVPlayerVideoAspectRatio))) then
          MPVSetVideoAspectRatio(TMPVPlayerVideoAspectRatio(x));
      end;

      if st.IndexOfName('PanAndScan') >= 0 then
        MPVSetPanAndScan(st.Values['PanAndScan'] = '1');

      if st.IndexOfName('Filters') >= 0 then
      begin
        s := ';' + st.Values['Filters'];
        for i := 0 to ComponentCount-1 do
          if (Components[i] is TAction) and
            (Components[i].Name.StartsWith('actVideoFilter') or Components[i].Name.StartsWith('actAudioFilter')) then
            TAction(Components[i]).Checked := Pos(';' + Components[i].Name + ';', s) > 0;
        MPVSetFilters;
      end;

      if st.IndexOfName('Volume') >= 0 then
      begin
        x := StrToIntDef(st.Values['Volume'], -1);
        if (x >= 0) and (x <= 100) then
          sbrVolume.Position := x; // sbrVolumeChange sets it to mpv
      end;

      if st.IndexOfName('AudioTrack') >= 0 then
      begin
        x := StrToIntDef(st.Values['AudioTrack'], -1);
        tl := MPV.TrackList;
        for i := 0 to High(tl) do
          if (tl[i].Kind = ttAudio) and (tl[i].ID = x) then
          begin
            if not tl[i].Selected then MPV.SetTrack(ttAudio, x);
            Break;
          end;
      end;

      if st.IndexOfName('VideoPreview') >= 0 then
        if (st.Values['VideoPreview'] = '0') and actVideoPreview.Checked then
          actVideoPreview.Execute;

      x := StrToIntDef(st.Values['Position'], 0);
      if x > 0 then
        MPVSeekTo(x);

      if st.IndexOfName('WaveformPosition') >= 0 then
        WAVE.SetPlayCursorMS(StrToIntDef(st.Values['WaveformPosition'], 0));
    end;
  finally
    st.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure LoadProject(const FileName: String);
var
  MRUInfoObject : TMRUInfoObject;
  AVideo, s     : String;
  x             : Integer;
begin
  if FileName.IsEmpty then Exit;
  with TProjectFile.Create(FileName) do
  try
    if Ready then
    begin
      LoadingProject := True;
      try
        FreeAndNil(PendingProjectState);
        AVideo := '';
        if not Movie.IsEmpty and FileExists(Movie) then
        begin
          // the thumbnails saved in the project (.tero1) go to the cache of the
          // video before it's opened, so they are not generated with ffmpeg
          RestoreThumbnails(GetThumbnailsCacheDir(Movie), Movie);
          // the same with the shot changes: to their cache file, where they are
          // loaded from with the waveform (it's the file Video > Shot changes
          // writes; the ones of the project are the last ones the user had)
          RestoreShotChanges(ShotChangesFileName(Movie, '.shotchanges'), Movie);
          // and the waveform, if there's none for the video (so it's not
          // extracted again)
          if GetMediaFileNameIfExists(Movie, TWaveformAudioExts).IsEmpty then
            RestoreWaveform(ConcatPaths([WaveformsFolder, ExtractFileName(ChangeFileExt(Movie, '.peak'))]), Movie);

          // the state of the player, when the video is loaded (before it's
          // opened: dialogs while the subtitle is loaded can let it load)
          PendingProjectState := TStringList.Create;
          PendingProjectState.Assign(State);
          PendingProjectState.Values['Position'] := MoviePos.ToString;
          PendingProjectVideo := Movie;

          // the video of the project, unless the recent files already open it
          // (it was opened twice)
          MRUInfoObject := MRU.GetValues(Original);
          if not Assigned(MRUInfoObject) or not SameFileName(MRUInfoObject.VideoFile, Movie) then
            AVideo := Movie;
        end;

        LoadSubtitle(Original, sfInvalid, NIL, -1, False, AVideo, False);
        if Subtitles.Count > 0 then
        begin
          // a project without translation (saved outside translator mode)
          if not Translation.IsEmpty then
            ReadSubtitleData(Translation, False, smTranslation);

          if (FocusedNode >= 0) and (FocusedNode < Subtitles.Count) then
            VSTSelectNode(frmMain.VST, FocusedNode, True, True);

          // the offset of the video (a Dolby Vision XML sets its own when
          // it's loaded: the one of the project is the last one the user had)
          if State.IndexOfName('VideoOffset') >= 0 then
            MPVSetVideoOffset(StrToIntDef(State.Values['VideoOffset'], 0));

          s := State.Values['ViewMode'];
          if s <> '' then
          begin
            x := StrToIntDef(s, -1);
            // the list or the source view (the transcription is another thing)
            if ((x = Ord(vmList)) or (x = Ord(vmSource))) and (TViewMode(x) <> Workspace.ViewMode) then
              SetViewMode(TViewMode(x));
          end;

          CurrentProjectFile := FileName;
          RefreshAppTitle;
        end
        else
          FreeAndNil(PendingProjectState);

        with frmMain do
          MRU.Add(FileName, '', '', VSTFocusedNode(VST), MPV.GetMediaPosInMs, WAVE.GetPlayCursorMS, MPVOptions.AutoStartPlaying);
      finally
        LoadingProject := False;
      end;

      // the video may already be loaded (dialogs while the subtitle loaded)
      if Assigned(PendingProjectState) and frmMain.MPV.IsMediaLoaded then
        ApplyProjectPlayerState;
    end
    else
      ShowErrorMessageDialog(lngUnableToLoad);
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure SaveProject(const FileName: String);
var
  PrjFileName : String;
begin
  if FileName.IsEmpty then Exit;
  PrjFileName := ChangeFileExt(FileName, TProjectExt);
  with TProjectFile.Create(PrjFileName, False) do
  try
    Original    := SubtitleInfo.Text.FileName;
    if Workspace.TranslatorMode then
      Translation := SubtitleInfo.Translation.FileName;
    FocusedNode := VSTFocusedNode(frmMain.VST);
    GetProjectState(State);

    // the video (it wasn't saved), its thumbnails, waveform and shot changes
    with frmMain do
      if MPV.IsMediaLoaded and FileExists(MPV.FileName) then
      begin
        Movie         := MPV.FileName;
        MoviePos      := MPV.GetMediaPosInMs;
        ThumbnailsDir := GetThumbnailsCacheDir(Movie);
        // the .peak (a .wav makes one next to it)
        if WAVE.IsPeakDataLoaded and not WAVE.FileName.IsEmpty then
          WaveformFile := ChangeFileExt(WAVE.FileName, '.peak');

        // the shot changes of the timeline (also the ones inserted/deleted
        // by hand), in the format of their cache
        GetShotChangesForCache(ShotChanges);
      end;

    try
      SaveProject;
    except
      on E: Exception do
      begin
        ShowErrorMessageDialog(E.Message);
        Exit;
      end;
    end;

    CurrentProjectFile := PrjFileName;
    RefreshAppTitle;

    with frmMain do
      MRU.Add(PrjFileName, '', '', VSTFocusedNode(VST), MPV.GetMediaPosInMs, WAVE.GetPlayCursorMS, MPVOptions.AutoStartPlaying);
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure SaveCurrentProject;
begin
  if CurrentProjectFile.IsEmpty or not FileExists(CurrentProjectFile) then
  begin
    SaveProjectWithDialog;
    Exit;
  end;

  // the subtitle and the translation first, if changed (the project only
  // has their file names). Without updating the project, it's saved after
  if SubtitleInfo.Text.Changed or SubtitleInfo.Text.FileName.IsEmpty then
  begin
    SaveCurrentSubtitle(smText, False);
    if SubtitleInfo.Text.Changed or SubtitleInfo.Text.FileName.IsEmpty then Exit; // cancelled
  end;

  if Workspace.TranslatorMode and (SubtitleInfo.Translation.Changed or SubtitleInfo.Translation.FileName.IsEmpty) then
  begin
    SaveCurrentSubtitle(smTranslation, False);
    if SubtitleInfo.Translation.Changed or SubtitleInfo.Translation.FileName.IsEmpty then Exit;
  end;

  SaveProject(CurrentProjectFile);
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
    // .tero1 and the old projects (.stp)
    OD.Filter  := lngProjectFile + '|*' + TProjectExt + ';*' + TProjectLegacyExt;
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
  // the project needs the subtitle saved first. The translation only in
  // translator mode: before it always asked to save a second file, the
  // translation, even when there was none
  if SubtitleInfo.Text.FileName.IsEmpty then
  begin
    SaveSubtitleWithDialog;
    if SubtitleInfo.Text.FileName.IsEmpty then Exit; // cancelled
  end;

  if Workspace.TranslatorMode and SubtitleInfo.Translation.FileName.IsEmpty then
  begin
    SaveSubtitleWithDialog(smTranslation);
    if SubtitleInfo.Translation.FileName.IsEmpty then Exit; // cancelled
  end;

  SD := TSaveDialog.Create(NIL);
  try
    SD.Title   := lngSaveFile;
    SD.Filter  := lngProjectFile + '|*' + TProjectExt;
    SD.Options := [ofOverwritePrompt, ofEnableSizing];
    SD.DefaultExt := TProjectExt;
    if not CurrentProjectFile.IsEmpty then
    begin
      SD.InitialDir := ExtractFileDir(CurrentProjectFile);
      SD.FileName := ExtractFileName(CurrentProjectFile);
    end
    else
      SD.FileName := ChangeFileExt(ExtractFileName(SubtitleInfo.Text.FileName), TProjectExt);

    if SD.Execute then
    begin
      SD.FileName := EnsureSaveFileExt(SD.FileName, TProjectExt);
      if not SD.FileName.IsEmpty then
        SaveProject(SD.FileName);
    end;
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
    SD.DefaultExt := '.txt';
    SD.FileName := '';
    SD.Options  := [ofOverwritePrompt, ofEnableSizing];
    if SD.Execute then
    begin
      SD.FileName := EnsureSaveFileExt(SD.FileName, '.txt');
      if not SD.FileName.IsEmpty then
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
  '        <source src="[VIDEO]" type="video/[EXT]">' + sLineBreak +
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
  OldTimeBase  : TSubtitleTimeBase;
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
      // the browser plays media time, like mpv, and the stored times are
      // already media times, so they are written without the SMPTE x1.001
      // conversion (same as MPVSaveSubtitleTempTrack). Before, an iTT/TTML at
      // 23.976 in frames mode was ~3.6 s late per hour in the web preview.
      OldTimeBase := Subtitles.TimeBase;
      Subtitles.TimeBase := stbMedia;
      try
        SubText := EncodeStringBase64(Subtitles.SaveToString(Workspace.FPS.OutputFPS.FPS, NIL, sfWebVTT, smText));
      finally
        Subtitles.TimeBase := OldTimeBase;
      end;
      Delete(VideoExt, 1, 1);
      // a proper file:/// URL (percent-encoded) escaped for an HTML attribute,
      // so paths with spaces, # or quotes work, also on Windows
      ts.Text := ts.Text.Replace('[VIDEO]', FilenameToURI(frmMain.MPV.FileName, True)
        .Replace('&', '&amp;').Replace('"', '&quot;').Replace('<', '&lt;').Replace('>', '&gt;'));
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
  Application.ProcessMessages;
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
  if IsProjectFileName(FileName) then // also when opened from the system (.tero1)
    LoadProject(FileName)
  else if IsValidMediaFileName(FileName, TVideoExts) or IsValidMediaFileName(FileName, TAudioExts) then
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
const
  csPrefix  = '-';
  csHelp = 'help';
  csConvert = 'convert';
  csTranslate = 'translate';

var
  s : String;
  fps : TFPSInfo;
  i, c : Integer;
  Sub : TUWSubtitles;
  sl : TStringList;
begin
  // Command line parameters reading
  if ParamCount > 0 then
  begin
    // Subtitle file
    if FileExists(ParamStr(1)) then
    begin
      if IsProjectFileName(ParamStr(1)) then
        LoadProject(ParamStr(1))
      else
        LoadSubtitle(ParamStr(1));
    end
    else
    begin // commands
      Sub := TUWSubtitles.Create;
      try
        s := ParamStr(1).ToLower;
        if (s = csPrefix+csHelp) then // -help
        begin
          WriteLn( 'Usage:' + LineEnding + LineEnding +
            csPrefix+csConvert + ' inputfile outputfile format [fps]' + LineEnding +
            csPrefix+csTranslate + ' inputfile outputfile srclang dstlang format [fps]' + LineEnding
          );
        end
        else if (s = csPrefix+csConvert) then // -convert inputfile outputfile format [fps]
        begin
          if (ParamCount > 3) then
          begin
            fps := Workspace.FPS.DefFPS; // also initializes fps.Mode
            if ParamCount = 5 then
              fps.FPS := StrToSingle(ParamStr(5), Workspace.FPS.DefFPS.FPS);

            s := ExtractFileName(ParamStr(2));
            if s.StartsWith('*') then
            begin
              sl := FindAllFiles(ExtractFileDir(ParamStr(2)), ExtractFileName(ParamStr(2)));
              try
                if sl.Count > 0 then
                begin
                  WriteLn('Convert files: ' + IntToStr(sl.Count));
                  for i := 0 to sl.Count-1 do
                  begin
                    WriteLn(' i: ' + sl[i]);
                    if Sub.LoadFromFile(sl[i], NIL, fps) then
                    begin
                      s := ConcatPaths([ParamStr(3), ChangeFileExt(ExtractFileName(sl[i]), '_'+ParamStr(4)) + '.x']);
                      if Sub.SaveToFile(s, fps, TEncoding.UTF8, NameToFormat(ParamStr(4), True), smText, -1, -1, True) then
                      begin
                        //WriteLn(' o: ' + s);
                        WriteLn(lngSuccess);
                      end
                      else
                        WriteLn(lngFailed);
                    end
                    else
                      WriteLn(lngFailed);
                  end;
                end;
              finally
                sl.Free;
              end;
            end
            else if Sub.LoadFromFile(ParamStr(2), NIL, fps) then
              Sub.SaveToFile(ParamStr(3), fps, TEncoding.UTF8, NameToFormat(ParamStr(4), True), smText);
          end;
        end
        else if (s = csPrefix+csTranslate) then // -translate inputfile outputfile srclang dstlang format [fps]
        begin
          if (ParamCount > 5) then
          begin
            fps := Workspace.FPS.DefFPS; // also initializes fps.Mode
            if ParamCount = 7 then // [fps] is the 7th parameter, the 5th is dstlang
              fps.FPS := StrToSingle(ParamStr(7), Workspace.FPS.DefFPS.FPS);

            s := ExtractFileName(ParamStr(2));
            if s.StartsWith('*') then
            begin
              sl := FindAllFiles(ExtractFileDir(ParamStr(2)), ExtractFileName(ParamStr(2)));
              try
                if sl.Count > 0 then
                begin
                  WriteLn('Translate files: ' + IntToStr(sl.Count));
                  for i := 0 to sl.Count-1 do
                  begin
                    WriteLn(' i: ' + sl[i]);
                    if Sub.LoadFromFile(sl[i], NIL, fps) then
                    begin
                      for c := 0 to Sub.Count-1 do
                        Sub.ItemPointer[c]^.Translation := GoogleTranslateText(Sub[c].Text, ParamStr(4), ParamStr(5));

                      s := ConcatPaths([ParamStr(3), ChangeFileExt(ExtractFileName(sl[i]), '') + '_' + UpperCase(ParamStr(5)) + '.x']);
                      if Sub.SaveToFile(s, fps, TEncoding.UTF8, NameToFormat(ParamStr(6), True), smTranslation, -1, -1, True) then
                      begin
                        //WriteLn(' o: ' + s);
                        WriteLn(lngSuccess);
                      end
                      else
                        WriteLn(lngFailed);
                    end
                    else
                      WriteLn(lngFailed);
                  end;
                end;
              finally
                sl.Free;
              end;
            end
            else if Sub.LoadFromFile(ParamStr(2), NIL, fps) then
            begin
              for i := 0 to Sub.Count-1 do
                Sub.ItemPointer[i]^.Translation := GoogleTranslateText(Sub[i].Text, ParamStr(4), ParamStr(5));

              Sub.SaveToFile(ParamStr(3), fps, TEncoding.UTF8, NameToFormat(ParamStr(6), True), smTranslation);
            end;
          end;
        end;
      finally
        Sub.Free;
        Application.Terminate;
      end;
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

    if not Application.Terminated then
    begin
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
end;

// -----------------------------------------------------------------------------

end.
