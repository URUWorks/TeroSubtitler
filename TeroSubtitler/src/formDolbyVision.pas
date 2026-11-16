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

unit formDolbyVision;

// -----------------------------------------------------------------------------
//
//  Dolby Vision metadata: the shots of a Dolby Vision XML (opened as a
//  subtitle, one entry per shot) follow a new edit of the video.
//
//   - Delete/insert N frames at a record frame: the shot that has them is
//     shorter/longer (its per frame metadata follows its frames) and all the
//     next shots move. E.g. a duplicated frame removed from the master.
//   - The cuts are checked against the shot changes of the video (Video >
//     Shot changes), a cut can be moved to its shot change.
//   - The record timecode of the first frame of the video (by default the
//     first shot).
//
//  Everything in whole frames of the <EditRate> of the XML. The entries can
//  also be edited as always (timeline, shift times, split...), see
//  DoViNormalizeEdits in the format.
//
// -----------------------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  UWRadioButton, UWCheckBox, LCLTranslator, DolbyVision;

type

  { TfrmDolbyVision }

  TfrmDolbyVision = class(TForm)
    btnApplyEdit: TButton;
    btnApplyOrigin: TButton;
    btnCheck: TButton;
    btnClose: TButton;
    btnMoveCut: TButton;
    btnVideoPosition: TButton;
    chkExtendPrevious: TUWCheckBox;
    edtOrigin: TEdit;
    edtPosition: TEdit;
    lblCheckTitle: TLabel;
    lblCount: TLabel;
    lblEditTitle: TLabel;
    lblInfo: TLabel;
    lblOrigin: TLabel;
    lblPosition: TLabel;
    lblPositionInfo: TLabel;
    lblTolerance: TLabel;
    lstResults: TListBox;
    rbnDelete: TUWRadioButton;
    rbnInsert: TUWRadioButton;
    spnCount: TSpinEdit;
    spnTolerance: TSpinEdit;
    procedure btnApplyEditClick(Sender: TObject);
    procedure btnApplyOriginClick(Sender: TObject);
    procedure btnCheckClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnMoveCutClick(Sender: TObject);
    procedure btnVideoPositionClick(Sender: TObject);
    procedure edtPositionChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lstResultsDblClick(Sender: TObject);
    procedure lstResultsSelectionChange(Sender: TObject; User: Boolean);
  private
    FNum, FDen : Integer;
    FChecks    : TDoViCutCheckArray;
    function Ready(const AShowMessage: Boolean = True): Boolean;
    function TC(const AFrame: Integer): String;
    function EntryName(const AIndex: Integer): String;
    function CurrentVideoFrame: Integer;
    procedure UpdateInfo;
    procedure ApplyChanges(const AChanges: TDoViChangeArray);
    procedure GoToFrame(const AFrame: Integer; const AIndex: Integer = -1);
  public

  end;

var
  frmDolbyVision: TfrmDolbyVision;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, procWorkspace, procSubtitle, procVST, procUndo, procMPV,
  procDialogs, procLocalize, formMain, UWSubtitleAPI, UWSubtitleAPI.ExtraInfo,
  Math;

{$R *.lfm}

// -----------------------------------------------------------------------------

function SignedInt(const AValue: Integer): String;
begin
  if AValue > 0 then
    Result := '+' + IntToStr(AValue)
  else
    Result := IntToStr(AValue);
end;

// -----------------------------------------------------------------------------

{ TfrmDolbyVision }

// -----------------------------------------------------------------------------

procedure TfrmDolbyVision.FormCreate(Sender: TObject);
begin
  FChecks := NIL;
  FNum := 24;
  FDen := 1;
  rbnDelete.Checked := True;
  lblPositionInfo.Caption := '';
  {$IFNDEF WINDOWS}
  PrepareCustomControls(Self);
  {$ENDIF}
  UpdateInfo;
  if Ready(False) then
    edtPosition.Text := TC(CurrentVideoFrame);
end;

// -----------------------------------------------------------------------------

procedure TfrmDolbyVision.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  frmDolbyVision := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmDolbyVision.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmDolbyVision.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

function TfrmDolbyVision.Ready(const AShowMessage: Boolean = True): Boolean;
begin
  // the document can change while the window is open
  Result := DoViIsEditable(Subtitles) and (Subtitles.Count > 0);
  if Result then
    DoViGetRate(Subtitles, FNum, FDen)
  else if AShowMessage then
    ShowErrorMessageDialog(lngDoViNotLoaded);
end;

// -----------------------------------------------------------------------------

function TfrmDolbyVision.TC(const AFrame: Integer): String;
begin
  Result := DoViFramesToTC(AFrame, FNum, FDen);
end;

// -----------------------------------------------------------------------------

function TfrmDolbyVision.EntryName(const AIndex: Integer): String;
begin
  if Subtitles.ValidIndex(AIndex) then
    Result := Subtitles.Text[AIndex]
  else
    Result := '';
end;

// -----------------------------------------------------------------------------

function TfrmDolbyVision.CurrentVideoFrame: Integer;
begin
  // the frame on screen, in record frames
  if frmMain.MPV.IsMediaLoaded then
    Result := DoViRecordFrame(Subtitles, frmMain.MPV.GetMediaPosInMs + Workspace.VideoOffset)
  else
    Result := Subtitles.SourceOrigin;
end;

// -----------------------------------------------------------------------------

procedure TfrmDolbyVision.UpdateInfo;
var
  a, b, c, d: Integer;
begin
  if not Ready(False) then
  begin
    lblInfo.Caption := lngDoViNotLoaded;
    Exit;
  end;

  DoViEntryFrames(Subtitles, 0, a, b);
  DoViEntryFrames(Subtitles, Subtitles.Count-1, c, d);
  if FDen = 1 then
    lblInfo.Caption := Format(lngDoViInfo, [Subtitles.Count, IntToStr(FNum), TC(a), TC(d)])
  else
    lblInfo.Caption := Format(lngDoViInfo, [Subtitles.Count, Format('%d/%d', [FNum, FDen]), TC(a), TC(d)]);
  edtOrigin.Text := TC(Subtitles.SourceOrigin);
end;

// -----------------------------------------------------------------------------

procedure TfrmDolbyVision.ApplyChanges(const AChanges: TDoViChangeArray);
var
  i, Deleted: Integer;
  P: PDolbyVision_ExtraInfo;
begin
  if Length(AChanges) = 0 then Exit;

  Deleted := 0;
  frmMain.VST.BeginUpdate;
  try
    // first the times (with the indexes of now), then the shots that were
    // left without frames, from the last one: undo restores them in reverse
    for i := 0 to High(AChanges) do
      with AChanges[i] do
      begin
        UndoInstance.AddUndoWithExtra(utSubtitleChange, Index, Subtitles[Index], Subtitles.CopyExtraInfo(Index), False);
        Subtitles.ItemPointer[Index]^.InitialTime := DoViRecordToMS(Subtitles, RecordIn);
        Subtitles.ItemPointer[Index]^.FinalTime   := DoViRecordToMS(Subtitles, RecordOut);
        New(P);
        P^ := Info;
        Subtitles.ReplaceExtraInfo(Index, P);
      end;

    for i := High(AChanges) downto 0 do
      if AChanges[i].Delete then
      begin
        DeleteSubtitle(AChanges[i].Index, False, False);
        Inc(Deleted);
      end;

    UndoInstance.IncrementUndoGroup;
  finally
    frmMain.VST.EndUpdate;
  end;

  SubtitleChanged(True, True);
  UpdateValues(True);
  frmMain.WAVE.DoUpdate;
  DoAutoCheckErrors;
  SetStatusBarText(Format(lngDoViApplied, [Length(AChanges) - Deleted, Deleted]));
end;

// -----------------------------------------------------------------------------

procedure TfrmDolbyVision.GoToFrame(const AFrame: Integer; const AIndex: Integer = -1);
begin
  // +1 ms: inside of the frame, never at the end of the previous one
  if frmMain.MPV.IsMediaLoaded then
    MPVSeekTo(DoViRecordToMS(Subtitles, AFrame) - Workspace.VideoOffset + 1);
  if Subtitles.ValidIndex(AIndex) then
    VSTSelectNode(frmMain.VST, AIndex, True, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmDolbyVision.btnApplyOriginClick(Sender: TObject);
var
  NewOrigin, i, a, b: Integer;
  Frames: array of array[0..1] of Integer;
begin
  // the record timecode of the first frame of the video: the entries move, the
  // record frames of the shots stay the same (only how they are shown)
  if not Ready then Exit;
  if not DoViTCToFrames(edtOrigin.Text, FNum, FDen, NewOrigin) then
  begin
    ShowErrorMessageDialog(lngDoViBadTimecode);
    Exit;
  end;
  if NewOrigin = Subtitles.SourceOrigin then Exit;

  Frames := NIL;
  SetLength(Frames, Subtitles.Count);
  for i := 0 to Subtitles.Count-1 do
  begin
    DoViEntryFrames(Subtitles, i, a, b);
    Frames[i][0] := a;
    Frames[i][1] := b;
  end;

  Subtitles.SourceOrigin := NewOrigin;
  for i := 0 to Subtitles.Count-1 do
  begin
    Subtitles.ItemPointer[i]^.InitialTime := DoViRecordToMS(Subtitles, Frames[i][0]);
    Subtitles.ItemPointer[i]^.FinalTime   := DoViRecordToMS(Subtitles, Frames[i][1]);
  end;

  // the undo items have the times of the old origin
  UndoInstance.Clear;
  SubtitleChanged(True, True);
  UpdateValues(True);
  frmMain.WAVE.DoUpdate;
  UpdateInfo;
  ShowMessageDialog(lngDoViOriginChanged);
end;

// -----------------------------------------------------------------------------

procedure TfrmDolbyVision.btnVideoPositionClick(Sender: TObject);
begin
  if not Ready then Exit;
  edtPosition.Text := TC(CurrentVideoFrame);
end;

// -----------------------------------------------------------------------------

procedure TfrmDolbyVision.edtPositionChange(Sender: TObject);
var
  f, i, a, b: Integer;
begin
  // where the frame is, before applying
  lblPositionInfo.Caption := '';
  if not Ready(False) or not DoViTCToFrames(edtPosition.Text, FNum, FDen, f) then Exit;

  for i := 0 to Subtitles.Count-1 do
  begin
    DoViEntryFrames(Subtitles, i, a, b);
    if (f >= a) and (f < b) then
    begin
      if f = a then
        lblPositionInfo.Caption := Format(lngDoViPosAtCut, [TC(f), EntryName(i)])
      else
        lblPositionInfo.Caption := Format(lngDoViPosInShot, [TC(f), f - a + 1, b - a, EntryName(i)]);
      Exit;
    end;
  end;
  lblPositionInfo.Caption := Format(lngDoViPosOutside, [TC(f)]);
end;

// -----------------------------------------------------------------------------

procedure TfrmDolbyVision.btnApplyEditClick(Sender: TObject);
var
  f: Integer;
begin
  if not Ready then Exit;
  if not DoViTCToFrames(edtPosition.Text, FNum, FDen, f) then
  begin
    ShowErrorMessageDialog(lngDoViBadTimecode);
    Exit;
  end;

  if rbnDelete.Checked then
    ApplyChanges(DoViPlanDeleteFrames(Subtitles, f, spnCount.Value))
  else
    ApplyChanges(DoViPlanInsertFrames(Subtitles, f, spnCount.Value, chkExtendPrevious.Checked));

  edtPositionChange(NIL);
  UpdateInfo;
  GoToFrame(f);
end;

// -----------------------------------------------------------------------------

procedure TfrmDolbyVision.btnCheckClick(Sender: TObject);
var
  i: Integer;
  s: String;
begin
  if not Ready then Exit;
  lstResults.Clear;
  FChecks := NIL;

  if frmMain.WAVE.GetSceneChangeCount = 0 then
  begin
    ShowErrorMessageDialog(lngDoViNoShotChanges);
    Exit;
  end;

  FChecks := DoViCheckCuts(Subtitles, frmMain.WAVE.GetSceneChangeList, spnTolerance.Value);

  lstResults.Items.BeginUpdate;
  try
    for i := 0 to High(FChecks) do
      with FChecks[i] do
      begin
        case Kind of
          cckMoved        : s := Format(lngDoViCheckMoved, [TC(CutFrame), EntryName(Index), TC(ShotFrame), SignedInt(ShotFrame - CutFrame)]);
          cckNoShotChange : s := Format(lngDoViCheckNoShotChange, [TC(CutFrame), EntryName(Index)]);
          cckNoCut        : s := Format(lngDoViCheckNoCut, [TC(CutFrame), EntryName(Index)]);
          else
            s := '';
        end;
        lstResults.Items.Add(s);
      end;
  finally
    lstResults.Items.EndUpdate;
  end;

  if Length(FChecks) = 0 then
    lstResults.Items.Add(lngDoViCheckOK)
  else
    SetStatusBarText(Format(lngDoViCheckCount, [Length(FChecks)]));

  lstResultsSelectionChange(NIL, False);
end;

// -----------------------------------------------------------------------------

procedure TfrmDolbyVision.lstResultsSelectionChange(Sender: TObject; User: Boolean);
var
  i: Integer;
begin
  i := lstResults.ItemIndex;
  btnMoveCut.Enabled := (i >= 0) and (i <= High(FChecks)) and (FChecks[i].Kind = cckMoved);
end;

// -----------------------------------------------------------------------------

procedure TfrmDolbyVision.lstResultsDblClick(Sender: TObject);
var
  i: Integer;
begin
  i := lstResults.ItemIndex;
  if not Ready(False) or (i < 0) or (i > High(FChecks)) then Exit;

  with FChecks[i] do
    if Kind = cckMoved then
      GoToFrame(ShotFrame, Index)
    else
      GoToFrame(CutFrame, Index);
end;

// -----------------------------------------------------------------------------

procedure TfrmDolbyVision.btnMoveCutClick(Sender: TObject);
var
  i, Idx, a, b, pa, pb: Integer;
  Old: TUWSubtitleItem;
begin
  // the end of the previous shot and the start of this one go to the shot
  // change: as edited in the timeline (the frames at the cut change of shot)
  i := lstResults.ItemIndex;
  if not Ready or (i < 0) or (i > High(FChecks)) or (FChecks[i].Kind <> cckMoved) then Exit;

  Idx := FChecks[i].Index;
  if not Subtitles.ValidIndex(Idx) or (Idx = 0) then Exit;
  DoViEntryFrames(Subtitles, Idx, a, b);
  DoViEntryFrames(Subtitles, Idx-1, pa, pb);
  if (pb <> a) or (a <> FChecks[i].CutFrame) or (FChecks[i].ShotFrame <= pa) or (FChecks[i].ShotFrame >= b) then
  begin
    ShowErrorMessageDialog(lngDoViMoveCutNotAdjacent);
    Exit;
  end;

  Old := Subtitles[Idx-1];
  UndoInstance.AddUndo(utSubtitleChange, Idx-1, Old, False);
  Subtitles.ItemPointer[Idx-1]^.FinalTime := DoViRecordToMS(Subtitles, FChecks[i].ShotFrame);
  Old := Subtitles[Idx];
  UndoInstance.AddUndo(utSubtitleChange, Idx, Old, False);
  Subtitles.ItemPointer[Idx]^.InitialTime := DoViRecordToMS(Subtitles, FChecks[i].ShotFrame);
  UndoInstance.IncrementUndoGroup;

  SubtitleChanged(True, True);
  UpdateValues(True);
  frmMain.WAVE.DoUpdate;

  // done, out of the list
  FChecks[i].Kind := cckDone;
  lstResults.Items[i] := '✓ ' + lstResults.Items[i];
  lstResultsSelectionChange(NIL, False);
  GoToFrame(FChecks[i].ShotFrame, Idx);
end;

// -----------------------------------------------------------------------------

end.
