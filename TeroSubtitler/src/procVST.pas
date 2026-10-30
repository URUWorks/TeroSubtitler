{*
 *  URUWorks Tero Subtitler
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

unit procVST;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Types, Forms, laz.VirtualTrees, procTypes;

type
  TFindMode = (fmBegin, fmCurrent, fmNext, fmBackward);
  TPasteSpecialMode = (psmTexts, psmTranslations, psmAsTranslations, psmTimes);

function VSTUpdating(const AVST: TLazVirtualStringTree): Boolean;
procedure VSTBeginUpdate(const AVST: TLazVirtualStringTree);
procedure VSTEndUpdate(const AVST: TLazVirtualStringTree);

procedure VSTAddColumn(const AVST: TLazVirtualStringTree; const AText: String; const AWidth: Integer; const AAlignment: TAlignment = taCenter; const AVisible: Boolean = True);

procedure VSTPaintCell(const TargetCanvas: TCanvas; const CellRect: TRect; const Color: TColor);
procedure VSTSetNodeHeight(const AVST: TLazVirtualStringTree; const AHeight: Integer);
procedure VSTShowColumn(const AVST: TLazVirtualStringTree; const Index: Integer; const Visible: Boolean);

function VSTFocusedNode(const AVST: TLazVirtualStringTree): Integer;
function VSTLastSelectedNode(const AVST: TLazVirtualStringTree): PVirtualNode;
function VSTLastSelectedNodeIndex(const AVST: TLazVirtualStringTree): Integer;
function VSTGetNodeAtIndex(const AVST: TLazVirtualStringTree; const AIndex: Integer): PVirtualNode;
procedure VSTSelectNode(const AVST: TLazVirtualStringTree; const AIndex: Integer; const AClear: Boolean; const AForceUpdate: Boolean = False; const ACenterOnlyIfHidden: Boolean = False);
procedure VSTSelectNode(const AVST: TLazVirtualStringTree; const ANode: PVirtualNode; const AClear: Boolean; const AForceUpdate: Boolean = False; const ACenterOnlyIfHidden: Boolean = False); overload;
procedure VSTSelectNodes(const AVST: TLazVirtualStringTree; const AIdxs: TIntegerDynArray; const AClear: Boolean); overload;
procedure VSTSelectNodes(const AVST: TLazVirtualStringTree; const AFrom, ATo: Integer; const AClear: Boolean); overload;
procedure VSTMarkEntries(const AVST: TLazVirtualStringTree; const Value: Boolean = True);
procedure VSTJumpToNextMarkedEntry(const AVST: TLazVirtualStringTree; const AForward: Boolean = True);
procedure VSTJumpToNextEmptyEntry(const AVST: TLazVirtualStringTree; const AForward: Boolean = True);
procedure VSTJumpToNextNoteEntry(const AVST: TLazVirtualStringTree; const AForward: Boolean = True);
procedure VSTCopyEntriesToClipboard(const AVST: TLazVirtualStringTree; const ACut: Boolean = False);
procedure VSTPasteEntriesFromClipboard(const AVST: TLazVirtualStringTree);
procedure VSTPasteSpecialFromClipboard(const AVST: TLazVirtualStringTree; const AMode: TPasteSpecialMode);
procedure VSTInsertEntries(const AVST: TLazVirtualStringTree; const Before: Boolean = False);
procedure VSTDeleteEntries(const AVST: TLazVirtualStringTree);
procedure VSTCombineEntries(const AVST: TLazVirtualStringTree);
procedure VSTDivideEntry(const AVST: TLazVirtualStringTree);

function VSTFind(const FindText: String; const CaseSensitive: Boolean; const FindMode: TFindMode; const FindLocation: TFindSource; const Replace: Boolean = False; const NewText: String = ''; const ReplaceAll: Boolean = False; const CasePreserve: Boolean = False; const WholeWord: Boolean = False; const RE: Boolean = False): Boolean;
procedure VSTFindPrevious(const CaseSensitive: Boolean = False; const WholeWord: Boolean = False; const RE: Boolean = False);
procedure VSTFindNext(const CaseSensitive: Boolean = False; const WholeWord: Boolean = False; const RE: Boolean = False);
procedure VSTDoLoop(const AVST: TLazVirtualStringTree; Proc: TVSTDoLoopProc; const Selection: TVSTDoLoopSelection = dlSelected; const Refresh: Boolean = True; const IncrementUndo: Boolean = False; const CBProc: TVSTDoLoopProcCB = NIL; const AFrom: Integer = 0; const ATo: Integer = 0);
procedure VSTMoveEntry(const AVST: TLazVirtualStringTree; const Refresh: Boolean = True; const IncrementUndo: Boolean = False; const CBProc: TVSTDoLoopProcCB = NIL; const AFrom: Integer = 0; const ATo: Integer = 0);

procedure VSTAdjustSubtitles(const AdjSub: TAdjustSubtitles);
procedure VSTSort(const AVST: TLazVirtualStringTree);

procedure VSTDeleteLineFromEntry(const AVST: TLazVirtualStringTree; const ALines: TStringArray);
procedure VSTTextEffect(const AVST: TLazVirtualStringTree; const AEffect: TTextEffect; const AParam1, AParam2: Integer);

procedure VSTDistributeEntriesEvenly(const AVST: TLazVirtualStringTree);
procedure VSTSetPauses(const AVST: TLazVirtualStringTree; const Pause: Integer; const Mode: TVSTSetPausesMode; const Selection: TVSTDoLoopSelection = dlSelected; const AFrom: Integer = 0; const ATo: Integer = 1);

var
  // True while VSTSelectNode changes the focus/selection: VSTFocusChanged
  // waits and VSTSelectNode updates the values once at the end
  VSTSelectingNode: Boolean = False;

// -----------------------------------------------------------------------------

implementation

uses procWorkspace, procUndo, procSubtitle, Clipbrd, RegExpr, UWSubtitles.Utils,
  UWSubtitleAPI, UWSystem.SysUtils, UWSystem.StrUtils, formMain, Math, lazUTF8,
  UWSubtitleAPI.Tags, formFindAndReplace;

// -----------------------------------------------------------------------------

function VSTUpdating(const AVST: TLazVirtualStringTree): Boolean;
begin
  Result := AVST.Tag > TAG_CONTROL_NORMAL;
end;

// -----------------------------------------------------------------------------

procedure VSTBeginUpdate(const AVST: TLazVirtualStringTree);
begin
  AVST.Tag := AVST.Tag + TAG_CONTROL_UPDATE;
end;

// -----------------------------------------------------------------------------

procedure VSTEndUpdate(const AVST: TLazVirtualStringTree);
begin
  AVST.Tag := AVST.Tag - TAG_CONTROL_UPDATE;
end;

// -----------------------------------------------------------------------------

procedure VSTAddColumn(const AVST: TLazVirtualStringTree; const AText: String; const AWidth: Integer; const AAlignment: TAlignment = taCenter; const AVisible: Boolean = True);
begin
  with AVST.Header.Columns.Add do
  begin
     Text      := AText;
     Width     := AWidth;
     Alignment := AAlignment;
     Options   := [coAllowFocus, coEnabled, coParentBidiMode, coParentColor, coResizable];
     if AVisible then Options := Options + [coVisible];
  end;
end;

// -----------------------------------------------------------------------------

procedure VSTPaintCell(const TargetCanvas: TCanvas; const CellRect: TRect; const Color: TColor);
var
  AColor   : TColor;
  PenStyle : TPenStyle;
begin
  AColor   := TargetCanvas.Brush.Color;
  PenStyle := TargetCanvas.Pen.Style;
  TargetCanvas.Pen.Style   := psClear;
  TargetCanvas.Brush.Color := Color;
  TargetCanvas.FillRect(CellRect);
  TargetCanvas.Pen.Style   := PenStyle;
  TargetCanvas.Brush.Color := AColor;
end;

// -----------------------------------------------------------------------------

procedure VSTSetNodeHeight(const AVST: TLazVirtualStringTree; const AHeight: Integer);
var
  Run : PVirtualNode;
begin
  with AVST do
    if TotalCount > 0 then
    begin
      Run := GetFirst;
      while Assigned(Run) do
      begin
        Run^.NodeHeight := AHeight;
        Run := GetNext(Run);
      end;
    end;
end;

// -----------------------------------------------------------------------------

procedure VSTShowColumn(const AVST: TLazVirtualStringTree; const Index: Integer; const Visible: Boolean);
begin
  if Index < AVST.Header.Columns.Count then
    with AVST.Header.Columns[Index] do
      if Visible then
        Options := Options + [coVisible]
      else
        Options := Options - [coVisible];
end;

// -----------------------------------------------------------------------------

function VSTFocusedNode(const AVST: TLazVirtualStringTree): Integer;
begin
  if Assigned(AVST.FocusedNode) then
    Result := AVST.FocusedNode^.Index
  else
    Result := -1;
end;

// -----------------------------------------------------------------------------

function VSTLastSelectedNode(const AVST: TLazVirtualStringTree): PVirtualNode;
var
  Node: PVirtualNode;
begin
  Node := AVST.GetFirstSelected;
  if Assigned(Node) then
    while Assigned(AVST.GetNextSelected(Node)) do
      Node := AVST.GetNextSelected(Node);

  Result := Node;
end;

// -----------------------------------------------------------------------------

function VSTLastSelectedNodeIndex(const AVST: TLazVirtualStringTree): Integer;
var
  Node: PVirtualNode;
begin
  Node := VSTLastSelectedNode(AVST);
  if Assigned(Node) then
    Result := Node^.Index
  else
    Result := -1;
end;

// -----------------------------------------------------------------------------

function VSTGetNodeAtIndex(const AVST: TLazVirtualStringTree; const AIndex: Integer): PVirtualNode;
var
  Run: PVirtualNode;
begin
  Result := NIL;

  if (AIndex < 0) or (AIndex >= AVST.RootNodeCount) then Exit;

  with AVST do
  begin
    if AIndex > (RootNodeCount div 2) then
    begin
      // --- BÚSQUEDA INVERSA
      Run := GetLast;
      while Assigned(Run) do
      begin
        if Run^.Index = AIndex then
          Exit(Run)
        else if Run^.Index < AIndex then
          Exit(NIL);

        Run := GetPrevious(Run);
      end;
    end
    else
    begin
      // --- BÚSQUEDA NORMAL
      Run := GetFirst;
      while Assigned(Run) do
      begin
        if Run^.Index = AIndex then
          Exit(Run)
        else if Run^.Index > AIndex then
          Exit(NIL);

        Run := GetNext(Run);
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure VSTSelectNode(const AVST: TLazVirtualStringTree; const AIndex: Integer; const AClear: Boolean; const AForceUpdate: Boolean = False; const ACenterOnlyIfHidden: Boolean = False);
begin
  VSTSelectNode(AVST, VSTGetNodeAtIndex(AVST, AIndex), AClear, AForceUpdate, ACenterOnlyIfHidden);
end;

// -----------------------------------------------------------------------------

procedure VSTSelectNode(const AVST: TLazVirtualStringTree; const ANode: PVirtualNode; const AClear: Boolean; const AForceUpdate: Boolean = False; const ACenterOnlyIfHidden: Boolean = False);

  function NodeFullyVisible: Boolean;
  var
    R: TRect;
  begin
    // same test as TBaseVirtualTree.ScrollIntoView
    R := AVST.GetDisplayRect(ANode, NoColumn, False);
    if hoVisible in AVST.Header.Options then
      OffsetRect(R, 0, -AVST.Header.Height);
    Result := (R.Top >= 0) and (R.Bottom <= AVST.ClientHeight);
  end;

var
  FocusChanged: Boolean;
begin
  FocusChanged := False;
  if Assigned(ANode) then
    with AVST, frmMain do
    begin
      // setting FocusedNode runs VSTFocusChanged (UpdateValues) before the
      // node is selected, and AForceUpdate ran it again: twice on every new
      // subtitle while playing. Now it runs once, with the final selection.
      FocusChanged := (FocusedNode <> ANode);
      VSTSelectingNode := True;
      try
        if AClear then ClearSelection;
        FocusedNode     := ANode;
        Selected[ANode] := True;
      finally
        VSTSelectingNode := False;
      end;
      // ACenterOnlyIfHidden (following the playback): centering on every
      // new entry scrolled and repainted the whole list each time, now only
      // when the entry goes out of view (then it is centered again)
      if not ACenterOnlyIfHidden or not NodeFullyVisible then
        ScrollIntoView(ANode, True);
    end;

  if AForceUpdate or FocusChanged then
    frmMain.VSTFocusChanged(NIL, NIL, 0);
end;

// -----------------------------------------------------------------------------

procedure VSTSelectNodes(const AVST: TLazVirtualStringTree; const AIdxs: TIntegerDynArray; const AClear: Boolean);
var
  Run: PVirtualNode;
  i, LastIdx: Integer;
  NodeToFocus: PVirtualNode;
  Wanted: array of Boolean;
begin
  if Length(AIdxs) = 0 then Exit;
  Wanted := NIL;

  AVST.BeginUpdate;
  try
    if AVST.TotalCount > 0 then
    begin
      if AClear then AVST.ClearSelection;

      NodeToFocus := nil;

      // one pass over the tree with a lookup table of the wanted indexes,
      // instead of comparing every node with every index (O(n*m))
      SetLength(Wanted, AVST.RootNodeCount);
      for i := Low(AIdxs) to High(AIdxs) do
        if (AIdxs[i] >= 0) and (AIdxs[i] < Length(Wanted)) then
          Wanted[AIdxs[i]] := True;
      LastIdx := AIdxs[High(AIdxs)];

      Run := AVST.GetFirst;
      while Assigned(Run) do
      begin
        if (Run^.Index < Cardinal(Length(Wanted))) and Wanted[Run^.Index] then
        begin
          AVST.Selected[Run] := True;
          if Integer(Run^.Index) = LastIdx then
            NodeToFocus := Run;
        end;
        Run := AVST.GetNext(Run);
      end;

      if Assigned(NodeToFocus) then
      begin
        AVST.FocusedNode := NodeToFocus;
        AVST.ScrollIntoView(NodeToFocus, True);
      end;
    end;
  finally
    AVST.EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

procedure VSTSelectNodes(const AVST: TLazVirtualStringTree; const AFrom, ATo: Integer; const AClear: Boolean);
var
  Run : PVirtualNode;
begin
  with AVST do
    if TotalCount > 0 then
    begin
      if AClear then ClearSelection;
      Run := GetFirst;
      while Assigned(Run) do
      begin
        if InRange(Run^.Index, AFrom, ATo) then
          Selected[Run] := True;
        Run := GetNext(Run);
      end;
    end;
end;

// -----------------------------------------------------------------------------

procedure VSTMarkEntries(const AVST: TLazVirtualStringTree; const Value: Boolean = True);
var
  Run : PVirtualNode;
begin
  with AVST do
    if SelectedCount > 0 then
    begin
      Run := GetFirstSelected;
      while Assigned(Run) do
      begin
        UndoInstance.AddUndo(utSubtitleChange, Run^.Index, Subtitles[Run^.Index], False);
        Subtitles.ItemPointer[Run^.Index]^.Marked := Value;
        Run := GetNextSelected(Run);
      end;
      Invalidate;
      UndoInstance.IncrementUndoGroup;
    end;
end;

// -----------------------------------------------------------------------------

procedure VSTJumpToNextMarkedEntry(const AVST: TLazVirtualStringTree; const AForward: Boolean = True);
var
  Run : PVirtualNode;
begin
  with AVST do
    if TotalCount > 0 then
    begin
      Run := GetFirstSelected;
      if not Assigned(Run) then Exit;

      if AForward then
        Run := GetFirstSelected^.NextSibling
      else
        Run := GetFirstSelected^.PrevSibling;

      while Assigned(Run) do
      begin
        if Subtitles[Run^.Index].Marked then
        begin
          VSTSelectNode(AVST, Run, True, True);
          Break;
        end;

        if AForward then
          Run := GetNext(Run)
        else
          Run := GetPrevious(Run);
      end;
    end;
end;

// -----------------------------------------------------------------------------

procedure VSTJumpToNextEmptyEntry(const AVST: TLazVirtualStringTree; const AForward: Boolean = True);
var
  Run : PVirtualNode;
begin
  with AVST do
    if TotalCount > 0 then
    begin
      Run := GetFirstSelected;
      if not Assigned(Run) then Exit;

      if AForward then
        Run := GetFirstSelected^.NextSibling
      else
        Run := GetFirstSelected^.PrevSibling;

      while Assigned(Run) do
      begin
        if Subtitles[Run^.Index].Text.IsEmpty then
        begin
          VSTSelectNode(AVST, Run, True, True);
          Break;
        end;

        if AForward then
          Run := GetNext(Run)
        else
          Run := GetPrevious(Run);
      end;
    end;
end;

// -----------------------------------------------------------------------------

procedure VSTJumpToNextNoteEntry(const AVST: TLazVirtualStringTree; const AForward: Boolean = True);
var
  Run : PVirtualNode;
begin
  with AVST do
    if TotalCount > 0 then
    begin
      Run := GetFirstSelected;
      if not Assigned(Run) then Exit;

      if AForward then
        Run := GetFirstSelected^.NextSibling
      else
        Run := GetFirstSelected^.PrevSibling;

      while Assigned(Run) do
      begin
        if not Subtitles[Run^.Index].Notes.IsEmpty then
        begin
          VSTSelectNode(AVST, Run, True, True);
          Break;
        end;

        if AForward then
          Run := GetNext(Run)
        else
          Run := GetPrevious(Run);
      end;
    end;
end;

// -----------------------------------------------------------------------------

procedure VSTCopyEntriesToClipboard(const AVST: TLazVirtualStringTree; const ACut: Boolean = False);
var
  Nodes : TNodeArray;
  I     : Integer;
  s     : String;
begin
  s := '';
  Nodes := NIL;
  if AVST.SelectedCount > 0 then
  begin
    AVST.BeginUpdate;
    try
      Nodes := AVST.GetSortedSelection(True);
      for I := 0 to High(Nodes) do
        with Subtitles[Nodes[I]^.Index] do
        begin
          s := Format('%s%d||%d||%s||%s||', [s, InitialTime, FinalTime,
            ReplaceRegExpr('\'+sLineBreak, Text, '\~', False),
            ReplaceRegExpr('\'+sLineBreak, Translation, '\~', False)]);
        end;
    finally
      Clipboard.AsText := s;
      if ACut then VSTDeleteEntries(AVST);
      AVST.EndUpdate;
      UpdateValues(True);
    end;
  end;
end;

// ------------------------------------------------------------------------------

procedure VSTPasteEntriesFromClipboard(const AVST: TLazVirtualStringTree);
var
  s      : TStringList;
  i, x   : Integer;
  t1, t2 : Integer;
  Idxs   : TIntegerDynArray;
begin
  // pasting is allowed with no selection (e.g. an empty list): append at the end
  begin
    AVST.BeginUpdate;
    s := TStringList.Create;
    Idxs := NIL;
    try
      if AVST.SelectedCount > 0 then
        x := VSTFocusedNode(AVST)
      else
        x := -1;
      if (x < 0) or (x > Subtitles.Count) then x := Subtitles.Count;
      AVST.ClearSelection;
      SplitRegExpr('\|\|', Clipboard.AsText, s);
      while s.Count > 4 do
      begin
        // validate the block: both times must be numbers, otherwise this is
        // not our clipboard format
        if not TryStrToInt(s[0], t1) or not TryStrToInt(s[1], t2) then Break;
        InsertSubtitle(x, t1, t2,
          ReplaceRegExpr('\\~', s[2], sLineBreak, False),
          ReplaceRegExpr('\\~', s[3], sLineBreak, False), False, False);
        SetLength(Idxs, Length(Idxs)+1);
        Idxs[High(Idxs)] := x;
        inc(x);
        for i := 1 to 4 do s.Delete(0);
      end;
      if Length(Idxs) > 0 then
        VSTSelectNodes(AVST, Idxs, True);
    finally
      s.Free;
      AVST.EndUpdate;
      UndoInstance.IncrementUndoGroup;
      UpdateValues(True);
      DoAutoCheckErrors;
    end;
  end;
end;

// ------------------------------------------------------------------------------

// paste only a part of the copied entries (texts, translations or times) over
// the existing ones: over the selected entries when more than one is selected,
// or over the consecutive entries from the focused one. Plain text (not copied
// from tero) is pasted one line per entry.
procedure VSTPasteSpecialFromClipboard(const AVST: TLazVirtualStringTree; const AMode: TPasteSpecialMode);
var
  s       : TStringList;
  Items   : array of record
    InTime, OutTime : Integer;
    Text, Tr        : String;
  end;
  Targets : TIntegerDynArray;
  Nodes   : TNodeArray;
  i, x, n : Integer;
  t1, t2  : Integer;
  IsOurs  : Boolean;
begin
  if Subtitles.Count = 0 then Exit;

  Items := NIL;
  s := TStringList.Create;
  try
    // our clipboard format: initial||final||text||translation||...
    SplitRegExpr('\|\|', Clipboard.AsText, s);
    IsOurs := (s.Count > 4) and TryStrToInt(s[0], t1) and TryStrToInt(s[1], t2);
    if IsOurs then
    begin
      while s.Count > 4 do
      begin
        if not TryStrToInt(s[0], t1) or not TryStrToInt(s[1], t2) then Break;
        SetLength(Items, Length(Items)+1);
        with Items[High(Items)] do
        begin
          InTime  := t1;
          OutTime := t2;
          Text := ReplaceRegExpr('\\~', s[2], sLineBreak, False);
          Tr   := ReplaceRegExpr('\\~', s[3], sLineBreak, False);
        end;
        for i := 1 to 4 do s.Delete(0);
      end;
    end
    else
    begin
      // plain text, there are no times to paste
      if AMode = psmTimes then Exit;
      s.Text := Clipboard.AsText;
      for i := 0 to s.Count-1 do
        if s[i].Trim <> '' then
        begin
          SetLength(Items, Length(Items)+1);
          with Items[High(Items)] do
          begin
            InTime  := -1;
            OutTime := -1;
            Text := s[i].Trim;
            Tr   := Text;
          end;
        end;
    end;
  finally
    s.Free;
  end;

  if Length(Items) = 0 then Exit;

  Targets := NIL;
  Nodes   := NIL;
  if AVST.SelectedCount > 1 then
  begin
    Nodes := AVST.GetSortedSelection(True);
    n := Min(Length(Nodes), Length(Items));
    SetLength(Targets, n);
    for i := 0 to n-1 do
      Targets[i] := Nodes[i]^.Index;
  end
  else
  begin
    x := Max(0, VSTFocusedNode(AVST));
    n := Min(Subtitles.Count - x, Length(Items));
    SetLength(Targets, n);
    for i := 0 to n-1 do
      Targets[i] := x + i;
  end;

  if (AMode in [psmTranslations, psmAsTranslations]) and not Workspace.TranslatorMode then
    SetTranslatorMode(True);

  AVST.BeginUpdate;
  try
    for i := 0 to High(Targets) do
      case AMode of
        psmTexts          : SetSubtitleText(Targets[i], Items[i].Text, smText, False, False, False);
        psmTranslations   : SetSubtitleText(Targets[i], Items[i].Tr, smTranslation, False, False, False);
        psmAsTranslations : SetSubtitleText(Targets[i], Items[i].Text, smTranslation, False, False, False);
        psmTimes          : SetSubtitleTimes(Targets[i], Items[i].InTime, Items[i].OutTime, False, False);
      end;

    if Length(Targets) > 0 then
      VSTSelectNodes(AVST, Targets, True);
  finally
    AVST.EndUpdate;
    UndoInstance.IncrementUndoGroup;
    UpdateValues(True);
    DoAutoCheckErrors;
  end;
end;

// ------------------------------------------------------------------------------

procedure VSTInsertEntries(const AVST: TLazVirtualStringTree; const Before: Boolean = False);
var
  Run       : PVirtualNode;
  Item      : PUWSubtitleItem;
  ti, tf    : Integer;
  pos, i    : Integer;
  Pause     : Integer;
  SelArray  : TIntegerDynArray;
begin
  Pause := GetCorrectTime(AppOptions.Conventions.MinPause, AppOptions.Conventions.PauseInFrames);
  // Multiple subtitles selected
  if AVST.SelectedCount > 1 then
  begin
    SelArray := NIL;
    SetLength(SelArray, AVST.SelectedCount);
    try
      if Before then
      begin
        i := AVST.SelectedCount-1;
        Run := VSTLastSelectedNode(AVST);
        while Assigned(Run) do
        begin
          Item := Subtitles.ItemPointer[Run^.Index];
          if Assigned(Item) then
          begin
            ti  := Range(Item^.InitialTime - AppOptions.Conventions.NewSubtitleMs - Pause, 0, Item^.InitialTime);
            tf  := ti + AppOptions.Conventions.NewSubtitleMs;
            SelArray[i] := InsertSubtitle(Run^.Index, ti, tf, '', '', False, False);
            Dec(i);
          end;
          Run := AVST.GetPreviousSelected(Run);
        end;
      end
      else
      begin
        // walk from the bottom up, like the "before" branch: inserting first
        // would shift the indexes of the selected nodes still to process
        i := AVST.SelectedCount-1;
        Run := VSTLastSelectedNode(AVST);
        while Assigned(Run) do
        begin
          Item := Subtitles.ItemPointer[Run^.Index];
          if Assigned(Item) then
          begin
            ti  := Item^.FinalTime + Pause;
            tf  := ti + AppOptions.Conventions.NewSubtitleMs;
            SelArray[i] := InsertSubtitle(Run^.Index+1, ti, tf, '', '', False, False);
            Dec(i);
          end;
          Run := AVST.GetPreviousSelected(Run);
        end;
      end;
      // every entry inserted later (above) shifted the previous ones down by one
      for i := 0 to High(SelArray) do
        SelArray[i] := SelArray[i] + i;
      VSTSelectNodes(AVST, SelArray, True);
      UpdateValues(True);
    finally
      SetLength(SelArray, 0);
      UndoInstance.IncrementUndoGroup;
    end;
  end
  else // only one
  begin
    pos := -1;
    i   := VSTFocusedNode(AVST);

    if i >= 0 then
    begin
      if Before then
      begin
        ti := Range(Subtitles[i].InitialTime - AppOptions.Conventions.NewSubtitleMs - Pause, 0, Subtitles[i].InitialTime);
        pos := i;
      end
      else
      begin
        ti := Subtitles[i].FinalTime + Pause;
        pos := i+1;
      end;
    end
    else if (i < 0) and (Subtitles.Count > 0) then
      ti := Subtitles.Items[Subtitles.Count-1].FinalTime + Pause
    else
      ti := 0;

    tf := ti + AppOptions.Conventions.NewSubtitleMs;
    VSTSelectNode(AVST, InsertSubtitle(pos, ti, tf, '', ''), True, True);
  end;
  DoAutoCheckErrors;
end;

// -----------------------------------------------------------------------------

procedure VSTDeleteEntries(const AVST: TLazVirtualStringTree);
var
  Nodes : TNodeArray;
  I     : Integer;
begin
  Nodes := NIL;
  if AVST.SelectedCount > 0 then
  begin
    AVST.BeginUpdate;
    try
      Nodes := AVST.GetSortedSelection(True);
      for I := High(Nodes) downto 0 do
        DeleteSubtitle(Nodes[I]^.Index, False, False);
    finally
      AVST.ClearSelection;
      AVST.EndUpdate;
      UndoInstance.IncrementUndoGroup;
      UpdateValues(True);
    end;
  end;
  DoAutoCheckErrors;
end;

// -----------------------------------------------------------------------------

procedure VSTCombineEntries(const AVST: TLazVirtualStringTree);
var
  Nodes  : TNodeArray;
  I, x   : Integer;
  s, st  : String;
  it, ft : Integer;
begin
  Nodes := NIL;
  if AVST.SelectedCount > 1 then
  begin
    AVST.BeginUpdate;
    try
      Nodes := AVST.GetSortedSelection(True);

      x  := Nodes[0]^.Index;
      s  := '';
      st := '';
      it := Subtitles[Nodes[0]^.Index].InitialTime;
      ft := Subtitles[Nodes[High(Nodes)]^.Index].FinalTime;

      for I := 0 to High(Nodes) do
        with Subtitles[Nodes[I]^.Index] do
        begin
          if s = '' then
            s := Text
          else
            s  := s + sLineBreak + Text;

          if st = '' then
            st := Translation
          else
            st := st + sLineBreak + Translation;
        end;

      for I := High(Nodes) downto 0 do
        DeleteSubtitle(Nodes[I]^.Index, False, False);

      s  := CleanupTags(s, [tst_Bold, tst_Italic, tst_Underline, tst_Strikeout]);
      st := CleanupTags(st, [tst_Bold, tst_Italic, tst_Underline, tst_Strikeout]);

      InsertSubtitle(x, it, ft, s, st);
    finally
      AVST.ClearSelection;
      AVST.EndUpdate;
    end;
  end;
  DoAutoCheckErrors;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure VSTDivideEntry(const AVST: TLazVirtualStringTree);
var
  s, sTrans : TStringList;
  NodeArray : TNodeArray;
  Item : PUWSubtitleItem;
  i, c, x, t1, t2, p : Integer;
  changed, UseTextAsMaster : Boolean;
  idxs : TIntegerDynArray;
  TextPart, TransPart, TempTrans : String;
begin
  if AVST.SelectedCount > 0 then
  begin
    changed := False;
    p := 0;
    s := TStringList.Create;
    sTrans := TStringList.Create;
    try
      s.SkipLastLineBreak := True;
      sTrans.SkipLastLineBreak := True;

      NodeArray := AVST.GetSortedSelection(False);
      if NodeArray = NIL then Exit;

      for i := High(NodeArray) downto Low(NodeArray) do
      begin
        x    := AVST.AbsoluteIndex(NodeArray[i]);
        Item := Subtitles.ItemPointer[x];

        if Assigned(Item) then
        begin
          // 1. Guardamos la Traducción
          TempTrans := Item^.Translation;

          // Verificamos si el TEXTO o la TRADUCCIÓN superan el límite
          if (GetTextLength(Item^.Text) > AppOptions.Conventions.CPL) or
             (GetTextLength(TempTrans) > AppOptions.Conventions.CPL) then
          begin
            s.Clear;
            sTrans.Clear;

            // 2. Generamos las divisiones
            // A. Dividimos el Texto
            SplitRegExpr('\|\|', DivideLines(Item^.Text, Item^.InitialTime, Item^.FinalTime, AppOptions.Conventions.DotsOnSplit, AppOptions.Conventions.CPL, GetCorrectTime(AppOptions.Conventions.MinPause, AppOptions.Conventions.PauseInFrames)), s);

            // B. Dividimos la Traducción (si existe)
            if TempTrans <> '' then
            begin
               SplitRegExpr('\|\|', DivideLines(TempTrans, Item^.InitialTime, Item^.FinalTime, AppOptions.Conventions.DotsOnSplit, AppOptions.Conventions.CPL, GetCorrectTime(AppOptions.Conventions.MinPause, AppOptions.Conventions.PauseInFrames)), sTrans);
            end;

            // 3. Verificamos si realmente hubo una división en alguno de los dos
            // (Count > 3 significa que hay más de 1 bloque, ya que el formato es T1||T2||Texto)
            if (s.Count > 3) or (sTrans.Count > 3) then
            begin
              changed := True;

              // El "Maestro" es quien tenga más cortes
              // Si el texto se divide en 2 y la traducción en 1, el texto manda
              UseTextAsMaster := s.Count >= sTrans.Count;

              // Borramos el subtítulo original
              DeleteSubtitle(x, False, False);

              while (s.Count >= 3) or (sTrans.Count >= 3) do
              begin
                Inc(p);
                SetLength(idxs, p);

                // A. OBTENER TIEMPOS
                // Si el texto manda y tiene datos, usamos sus tiempos
                if UseTextAsMaster and (s.Count >= 3) then
                begin
                  t1 := StrToIntDef(s[0], 0);
                  t2 := StrToIntDef(s[1], 0);
                end
                // Si la traducción manda (o el texto se termino), usamos tiempos de traducción
                else if (sTrans.Count >= 3) then
                begin
                  t1 := StrToIntDef(sTrans[0], 0);
                  t2 := StrToIntDef(sTrans[1], 0);
                end
                else
                begin
                   t1 := 0; t2 := 0;
                end;

                // B. OBTENER PARTE DE TEXTO
                TextPart := '';
                if s.Count >= 3 then
                begin
                  TextPart := FixIncompleteTags(s[2]);
                  for c := 1 to 3 do s.Delete(0);
                end;

                // C. OBTENER PARTE DE TRADUCCION
                TransPart := '';
                if sTrans.Count >= 3 then
                begin
                  TransPart := FixIncompleteTags(sTrans[2]);
                  for c := 1 to 3 do sTrans.Delete(0);
                end;

                // D. INSERTAR EL NUEVO SUBTITULO
                idxs[p-1] := InsertSubtitle(x, t1, t2, TextPart, TransPart, False, False);

                Inc(x);
              end;

              // Limpieza
              s.Clear;
              sTrans.Clear;
            end;
          end;
        end;
      end;
      SetLength(NodeArray, 0);
    finally
      s.Free;
      sTrans.Free;
    end;

    if changed then
    begin
      VSTSelectNodes(AVST, idxs, True);
      SetLength(idxs, 0);
      UndoInstance.IncrementUndoGroup;
      SubtitleChanged(True, True);
      UpdateValues(True);
      DoAutoCheckErrors;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function VSTFind(const FindText: String; const CaseSensitive: Boolean; const FindMode: TFindMode; const FindLocation: TFindSource; const Replace: Boolean = False; const NewText: String = ''; const ReplaceAll: Boolean = False; const CasePreserve: Boolean = False; const WholeWord: Boolean = False; const RE: Boolean = False): Boolean;

  function FoundText(const S: String): Boolean;
  var
    rs: String;
  begin
    if not RE then
    begin
      if not WholeWord then
        Result := (CaseSensitive and (Pos(FindText, S) > 0)) or
                  (not CaseSensitive and (PosCS(FindText, S) > 0))
      else
        Result := FindWord(S, FindText, CaseSensitive) <> '';
    end
    else
    begin
      rs := '';
      if not CaseSensitive and not Contains('(?i)', FindText) then
        rs := '(?i)';
      if WholeWord and not Contains('\b', FindText) then
        rs += '\b'+FindText+'\b'
      else
        rs += FindText;

      Result := RE_Pos(rs, S) > -1;
    end;
  end;


  function ReplaceFound(const S: String): String;
  var
    Pattern, Rep: String;
    Rx: TRegExpr;
    LastPos: Integer;
  begin
    // one regex for every mode, so case-insensitive, whole word and
    // "replace all occurrences" behave the same as the search does
    if RE then
      Pattern := FindText
    else
      Pattern := QuoteRegExprMetaChars(FindText);
    if WholeWord and not (RE and Contains('\b', FindText)) then
      Pattern := '\b' + Pattern + '\b';
    if not CaseSensitive and not (RE and Contains('(?i)', FindText)) then
      Pattern := '(?i)' + Pattern;

    Result  := '';
    LastPos := 1;
    Rx := TRegExpr.Create(Pattern);
    try
      if Rx.Exec(S) then
        repeat
          if RE then
            Rep := Rx.Substitute(NewText)
          else
            Rep := NewText;
          // preserve the case of the text actually found, not the searched one
          if CasePreserve then
            Rep := PreserveCase(Rx.Match[0], Rep);
          Result  := Result + Copy(S, LastPos, Rx.MatchPos[0] - LastPos) + Rep;
          LastPos := Rx.MatchPos[0] + Rx.MatchLen[0];
          if Rx.MatchLen[0] = 0 then Break; // avoid looping on empty matches
        until not Rx.ExecNext;
      Result := Result + Copy(S, LastPos, MaxInt);
    finally
      Rx.Free;
    end;
  end;

  function Found(const Location: TFindSource; const Text: string; const Translation: string): Boolean;
  begin
    Result := False;
    if (Location = fsText) or (Location = fsTextAndTranslation)
      then if FoundText(Text) then Exit(True);
    if (Location = fsTranslation) or (Location = fsTextAndTranslation)
          then if FoundText(Translation) then Exit(True);
  end;

var
  Item : PUWSubtitleItem;
  i, c : Integer;
  s    : String;
begin
  Result := False;
  AppOptions.TextToFind := FindText;
  if FindText = '' then exit;

  if Subtitles.Count > 0 then
  begin
    if FindMode = fmBackward then
    begin
      for i := (VSTFocusedNode(frmMain.VST)-1) downto 0 do
      begin
        Item := Subtitles.ItemPointer[i];
        if Assigned(Item) then
          with Item^ do
          begin
            if Found(FindLocation,Text,Translation) then
            begin
              Result := True;
              VSTSelectNode(frmMain.VST, i, True, True);
              Break;
            end;
          end;
      end;
      Exit;
    end;

    if FindMode = fmBegin then
      c := 0
    else
    begin
      i := VSTFocusedNode(frmMain.VST);
      if FindMode = fmNext then
        Inc(i);
      c := Range(i, 0, Subtitles.Count-1);
    end;

    for i := c to Subtitles.Count-1 do
    begin
      Item := Subtitles.ItemPointer[i];
      if Assigned(Item) then
        with Item^ do
        begin
          if Found(FindLocation,Text,Translation) then
          begin
            Result := True;
            VSTSelectNode(frmMain.VST, i, True, True);
            if not Replace then // Only Find...
              Break
            else
            begin // Find & Replace...
              if (FindLocation = fsText) or (FindLocation = fsTextAndTranslation) then
                if FoundText(Text) then
                begin
                  s := ReplaceFound(Text);
                  SetSubtitleText(i, s, smText, False, False, False);
                end; //if FoundText(Text)
              if (FindLocation = fsTranslation) or (FindLocation = fsTextAndTranslation) then
                if FoundText(Translation) then
                begin
                  s := ReplaceFound(Translation);
                  SetSubtitleText(i, s, smTranslation, False, False, False);
                end; //FoundText(Translation)
              if not ReplaceAll then Break;
            end; // Find & Replace...
          end;
        end;
    end;
  end;

  if Replace then
  begin
    UpdateValues(True);
    DoAutoCheckErrors;
  end;

  with frmMain do
  begin
    actFindNext.Enabled     := not AppOptions.TextToFind.IsEmpty;
    actFindPrevious.Enabled := actFindNext.Enabled;
  end;
end;

// -----------------------------------------------------------------------------

procedure VSTFindPrevious(const CaseSensitive: Boolean = False; const WholeWord: Boolean = False; const RE: Boolean = False);
begin
  with AppOptions do
    if TextToFind <> '' then VSTFind(TextToFind, CaseSensitive, fmBackward, frmFindAndReplace.FindSource, False, '', False, False, WholeWord, RE);
end;

// -----------------------------------------------------------------------------

procedure VSTFindNext(const CaseSensitive: Boolean = False; const WholeWord: Boolean = False; const RE: Boolean = False);
begin
  with AppOptions do
    if TextToFind <> '' then VSTFind(TextToFind, CaseSensitive, fmNext, frmFindAndReplace.FindSource, False, '', False, False, WholeWord, RE);
end;

// -----------------------------------------------------------------------------

procedure VSTDoLoop(const AVST: TLazVirtualStringTree; Proc: TVSTDoLoopProc; const Selection: TVSTDoLoopSelection = dlSelected; const Refresh: Boolean = True; const IncrementUndo: Boolean = False; const CBProc: TVSTDoLoopProcCB = NIL; const AFrom: Integer = 0; const ATo: Integer = 0);
var
  Item    : PUWSubtitleItem;
  i, c, x : Integer;
  Run     : PVirtualNode;
  Cancel  : Boolean;
begin
  Cancel := False;
  if Selection <> dlSelected then
  begin
    c := 0;
    x := Subtitles.Count-1;

    if (Selection = dlRange) then
    begin
      c := AFrom;
      x := ATo;
    end
    else if (Selection = dlCurrentToLast) then
      c := Range(VSTFocusedNode(AVST), 0, Subtitles.Count-1);

    for i := c to x do
    begin
      Item := Subtitles.ItemPointer[i];
      if Assigned(Item) then
      begin
        if (Selection <> dlMarked) or ((Selection = dlMarked) and Item^.Marked) then
        begin
          Proc(Item, i);
          if Assigned(CBProc) then
          begin
            CBProc(i, Subtitles.Count-1, Cancel);
            Application.ProcessMessages;
            if Cancel then Break;
          end;
        end;
      end;
    end;
  end
  else
  begin
    i := 0;
    Run := AVST.GetFirstSelected;
    while Assigned(Run) do
    begin
      Item := Subtitles.ItemPointer[Run^.Index];
      if Assigned(Item) then
      begin
        Proc(Item, Run^.Index);
        if Assigned(CBProc) then
        begin
          Inc(i);
          CBProc(i, AVST.SelectedCount-1, Cancel);
          Application.ProcessMessages;
          if Cancel then Break;
        end;
      end;
      Run := AVST.GetNextSelected(Run);
    end;
  end;

  if IncrementUndo then
  begin
    SubtitleChanged(True, True);
    UndoInstance.IncrementUndoGroup;
  end;

  if Refresh then
  begin
    DoAutoCheckErrors;
    UpdateValues(True);
  end;
end;

// -----------------------------------------------------------------------------

procedure VSTMoveEntry(const AVST: TLazVirtualStringTree; const Refresh: Boolean = True; const IncrementUndo: Boolean = False; const CBProc: TVSTDoLoopProcCB = NIL; const AFrom: Integer = 0; const ATo: Integer = 0);
var
  Item  : PUWSubtitleItem;
  Delay : Integer;
  Run   : PVirtualNode;
begin
  Run := AVST.GetFirstSelected;
  if Assigned(Run) then
  begin
    Delay := frmMain.MPV.GetMediaPosInMs - Subtitles.ItemPointer[Run^.Index]^.InitialTime;
    while Assigned(Run) do
    begin
      Item := Subtitles.ItemPointer[Run^.Index];
      if Assigned(Item) then
        with Item^ do
        begin
          SetSubtitleTime(Run^.Index, SetDelay(InitialTime, Delay), frmMain.tedInitial.Tag, False, False);
          SetSubtitleTime(Run^.Index, SetDelay(FinalTime, Delay), frmMain.tedFinal.Tag, False, False);
        end;
      Run := AVST.GetNextSelected(Run);
    end;

    if IncrementUndo then
    begin
      UndoInstance.IncrementUndoGroup;
      SubtitleChanged(True, True);
    end;

    if Refresh then
    begin
      UpdateValues(True);
      DoAutoCheckErrors;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure VSTAdjustSubtitles(const AdjSub: TAdjustSubtitles);
var
  l, i, j, x : Integer;
  Delta1     : Integer;          // difference of the starting synch point
  Delta2     : Integer;          // difference of the ending synch point including the delta1
  DeltaX     : Extended;         // difference per single frame
  diff       : Integer;          // the difference (position in the interval)
  Shift      : Integer;          // resulting shift
  Deltas     : array of Integer; // store for the deltas
  Starts     : array of Integer; // store for old starts
begin
  l := Length(AdjSub);
  if (l < 2) or (Subtitles.Count < 2) then Exit;

  Deltas := NIL;
  Starts := NIL;
  SetLength(Deltas, l);
  SetLength(Starts, l);
  try
    for i := 0 to l-1 do
    begin
      if (AdjSub[i].SubtitleIndex < 0) or (AdjSub[i].SubtitleIndex >= Subtitles.Count) then Exit;
      Deltas[i] := AdjSub[i].NewInitialTime - Subtitles[AdjSub[i].SubtitleIndex].InitialTime;
      Starts[i] := Subtitles[AdjSub[i].SubtitleIndex].InitialTime;
    end;

    try
      for i := 0 to l-2 do
      begin // all synch points, start subtitle difference
        Delta1 := Deltas[i];
        Delta2 := Deltas[i+1]-Delta1;
        x      := Subtitles[AdjSub[i+1].SubtitleIndex].InitialTime;
        // two synch points at the same initial time would divide by zero
        if x <> Subtitles[AdjSub[i].SubtitleIndex].InitialTime then
          DeltaX := Delta2 / (x-Subtitles[AdjSub[i].SubtitleIndex].InitialTime)
        else
          DeltaX := 0;

        if (i=(l-2)) then
        begin // we have left one subtitle: at the last synch point
          j     := AdjSub[i+1].SubtitleIndex;
          Shift := Deltas[i+1];

          SetSubtitleTimes(j, Subtitles[j].InitialTime + Shift, Subtitles[j].FinalTime + Shift, False, False);
        end;

        for j := AdjSub[i].SubtitleIndex to AdjSub[i+1].SubtitleIndex-1 do
        begin // for all subs in the interval
          with Subtitles[j] do
          begin
            diff  := InitialTime - Starts[i];       // position in the interval
            Shift := Delta1 + Round(diff * DeltaX); // this is the final value of the shift

            SetSubtitleTimes(j, Subtitles[j].InitialTime + Shift, Subtitles[j].FinalTime + Shift, False, False);
          end;
        end;
      end;
    finally
      UndoInstance.IncrementUndoGroup;
      SubtitleChanged(True, True);
      UpdateValues(True);
      DoAutoCheckErrors;
    end;
  finally
    SetLength(Deltas, 0);
    SetLength(Starts, 0);
  end;
end;

// -----------------------------------------------------------------------------

procedure VSTSort(const AVST: TLazVirtualStringTree);
var
  i       : Integer;
  OldPtrs : array of PUWSubtitleItem;
  OldItems: array of TUWSubtitleItem;
  Changed : Boolean;
begin
  // O(n log n) sort in the API (keeps ExtraInfo aligned) and one undo entry
  // per index that actually changed, instead of a swap sort with 2 undos per swap
  if (AVST.RootNodeCount > 0) and (Subtitles.Count > 1) then
  begin
    SetLength(OldPtrs, Subtitles.Count);
    SetLength(OldItems, Subtitles.Count);
    for i := 0 to Subtitles.Count-1 do
    begin
      OldPtrs[i]  := Subtitles.ItemPointer[i];
      OldItems[i] := OldPtrs[i]^;
    end;

    Subtitles.Sort;

    Changed := False;
    for i := 0 to Subtitles.Count-1 do
      if Subtitles.ItemPointer[i] <> OldPtrs[i] then
      begin
        UndoInstance.AddUndo(utSubtitleChange, i, OldItems[i], False);
        Changed := True;
      end;

    if Changed then
    begin
      UndoInstance.IncrementUndoGroup;
      SubtitleChanged(True, True);
      DoAutoCheckErrors(False);
      AVST.Invalidate;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure VSTDeleteLineFromEntry(const AVST: TLazVirtualStringTree; const ALines: TStringArray);
var
  Nodes : TNodeArray;
  I, c, n, t : Integer;
  Lines : array of Integer;
begin
  Nodes := NIL;
  if (AVST.SelectedCount > 0) and (Length(ALines) > 0) then
  begin
    // delete from the highest line number down, otherwise each deletion
    // shifts the lines still to delete ("1,2" deleted original lines 1 and 3)
    Lines := NIL;
    for c := 0 to High(ALines) do
    begin
      n := StrToIntDef(ALines[c], 0);
      if n > 0 then
      begin
        SetLength(Lines, Length(Lines)+1);
        Lines[High(Lines)] := n;
      end;
    end;
    for c := 0 to High(Lines)-1 do
      for n := c+1 to High(Lines) do
        if Lines[n] > Lines[c] then
        begin
          t := Lines[c]; Lines[c] := Lines[n]; Lines[n] := t;
        end;

    AVST.BeginUpdate;
    try
      Nodes := AVST.GetSortedSelection(True);

      for I := 0 to High(Nodes) do
        for c := 0 to High(Lines) do
        begin
          if (c > 0) and (Lines[c] = Lines[c-1]) then Continue; // duplicated
          SetSubtitleText(Nodes[I]^.Index, DeleteLineFromText(Nodes[I]^.Index, Lines[c]-1), smText, I=High(Nodes), I=High(Nodes), False);
        end;
    finally
      AVST.EndUpdate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TextEffect(const Index, InitialTime, FinalTime: Integer; const Text, Translation: String; const Effect: TTextEffect; const Param1: Integer; Param2: Integer): Integer;
var
  x       : Integer;
  ITime   : Integer;
  FTime   : Integer;
  CurTime : Integer;
  CharDur : Integer;
  Pos1    : Integer;
  SumLen  : Integer;
  VisLen  : Integer;
  s       : String;
begin
  x      := Index;
  ITime  := InitialTime;
  FTime  := FinalTime;
  Result := 0;

  case Effect of
    fxFlash:
    begin
      if (Param1 + Param2) <= 0 then Exit; // would never end
      repeat
        Inc(x);
        CurTime := ITime + Param1;
        if CurTime > FTime then CurTime := FTime;
        Result := InsertSubtitle(x, ITime, CurTime, Text, Translation, False, False);
        ITime := ITime + Param1 + Param2;
      until ITime >= FTime;
    end;

    fxTypewriter:
    begin
      VisLen := UTF8Length(RemoveTSTags(Text));
      if VisLen = 0 then Exit; // only tags or empty: nothing to type
      CharDur := Rnd((FTime - ITime) / VisLen);
      SumLen  := UTF8Length(Text);

      Pos1 := 0;
      while (Pos1 < SumLen) do
      begin
        Inc(Pos1);
        if((UTF8CompareStr(UTF8Copy(Text, Pos1, 1), #13) <> 0) and
           (UTF8CompareStr(Copy(Text, Pos1, 1), #10) <> 0) and
           (UTF8CompareStr(Copy(Text, Pos1, 1), ' ') <> 0)
           ) or (Pos1 = SumLen) then // and (not IsTagPart(Text, Pos1))) or (Pos1 = SumLen) then
        begin
          Inc(x);
          s := FixTags(UTF8Copy(Text, 1, Pos1), tst_StartTag, tst_EndTag);
          Result := InsertSubtitle(x, ITime, ITime+CharDur, s, Translation, False, False);

          if s = Text then Break; //avoid creating repeated subtitles

          ITime := ITime + CharDur;
        end;
      end;
      SetSubtitleTime(Result, FinalTime, TAG_CONTROL_FINALTIME, False, False);
    end;
  end;
end;

// -----------------------------------------------------------------------------

{procedure VSTTextEffect(const AVST: TLazVirtualStringTree; const AEffect: TTextEffect; const AParam1, AParam2: Integer);
var
  FocNode : Integer;
  NewNode : Integer;
begin
  FocNode := VSTFocusedNode(AVST);

  if (FocNode >= 0) and not Subtitles.Text[FocNode].IsEmpty then
  begin
   NewNode := TextEffect(FocNode,
                         Subtitles[FocNode].InitialTime,
                         Subtitles[FocNode].FinalTime,
                         Subtitles[FocNode].Text,
                         Subtitles[FocNode].Translation,
                         AEffect,
                         AParam1,
                         AParam2);

    DeleteSubtitle(FocNode);
    VSTSelectNode(AVST, NewNode-1, True);
  end;
end;}

// -----------------------------------------------------------------------------

procedure VSTTextEffect(const AVST: TLazVirtualStringTree; const AEffect: TTextEffect; const AParam1, AParam2: Integer);
var
  FocNode, i, x, n : Integer;
  OneSel : Boolean;
  NodeArray : TNodeArray;
begin
  if AVST.SelectedCount > 0 then
  begin
    NodeArray := AVST.GetSortedSelection(False);
    if NodeArray = NIL then Exit;
    OneSel := (AVST.SelectedCount = 1);

    FocNode := 0;
    for i := High(NodeArray) downto Low(NodeArray) do
    begin
      x := AVST.AbsoluteIndex(NodeArray[i]);
      if not Subtitles.Text[x].IsEmpty then
      begin
        n := TextEffect(x,
               Subtitles[x].InitialTime,
               Subtitles[x].FinalTime,
               Subtitles[x].Text,
               Subtitles[x].Translation,
               AEffect,
               AParam1,
               AParam2);

        DeleteSubtitle(x, False, False);
        FocNode += n - 1;
      end;
    end;
    SetLength(NodeArray, 0);

    if OneSel then
      VSTSelectNode(AVST, FocNode, True)
    else
      AVST.ClearSelection;

    UndoInstance.IncrementUndoGroup;
    SubtitleChanged(True, True);
    UpdateValues(True);
    DoAutoCheckErrors;
  end;
end;

// -----------------------------------------------------------------------------

procedure VSTDistributeEntriesEvenly(const AVST: TLazVirtualStringTree);
var
  NodeArray : TNodeArray;
  i, x, gap, dur, newDur, totLen, prevTime : Integer;
begin
  if AVST.SelectedCount > 0 then
  begin
    NodeArray := AVST.GetSortedSelection(False);
    if NodeArray = NIL then Exit;

    gap := GetCorrectTime(AppOptions.Conventions.MinPause, AppOptions.Conventions.PauseInFrames);
    dur := (Subtitles[AVST.AbsoluteIndex(NodeArray[High(NodeArray)])].FinalTime - Subtitles[AVST.AbsoluteIndex(NodeArray[Low(NodeArray)])].InitialTime) - (gap * High(NodeArray));
    totLen := 0;

    for i := Low(NodeArray) to High(NodeArray) do
    begin
      x := AVST.AbsoluteIndex(NodeArray[i]);
      if not Subtitles.Text[x].IsEmpty then
        totLen += GetTextLength(Subtitles.Text[x]);
    end;

    if totLen <= 0 then Exit; // all texts empty, nothing to distribute

    prevTime := Subtitles[AVST.AbsoluteIndex(NodeArray[Low(NodeArray)])].InitialTime - gap;

    for i := Low(NodeArray) to High(NodeArray) do
    begin
      x := AVST.AbsoluteIndex(NodeArray[i]);
      if not Subtitles.Text[x].IsEmpty then
      begin
        newDur := Round( (GetTextLength(Subtitles.Text[x]) / totLen) * dur );
        SetSubtitleTimes(x, prevTime + gap, prevTime + gap + newDur, False, False);
        prevTime := prevTime + gap + newDur;
      end;
    end;

    SetLength(NodeArray, 0);

    UndoInstance.IncrementUndoGroup;
    SubtitleChanged(True, True);
    UpdateValues(True);
    DoAutoCheckErrors;
  end;
end;

// -----------------------------------------------------------------------------

procedure VSTSetPauses(const AVST: TLazVirtualStringTree; const Pause: Integer; const Mode: TVSTSetPausesMode; const Selection: TVSTDoLoopSelection = dlSelected; const AFrom: Integer = 0; const ATo: Integer = 1);

  procedure SetPauseValue(const AIndex: Integer);
  var
    IT2, FT1, d : Integer;
  begin
    // the last entry has no next one (selection mode or a range up to Count-1)
    if (AIndex < 0) or (AIndex+1 >= Subtitles.Count) then Exit;

    FT1 := Subtitles[AIndex].FinalTime;
    IT2 := Subtitles[AIndex+1].InitialTime;

    if (IT2 - FT1) < Pause then
      case Mode of
        spmFirst  : SetSubtitleTime(AIndex, IT2-Pause, TAG_CONTROL_FINALTIME, False, False);
        spmSecond : SetSubtitleTime(AIndex+1, FT1+Pause, TAG_CONTROL_INITIALTIME, False, False);
        spmBoth   : begin
                      // split what is missing (not the existing gap) between both
                      d := Pause - (IT2 - FT1);
                      SetSubtitleTime(AIndex, FT1 - (d div 2), TAG_CONTROL_FINALTIME, False, False);
                      SetSubtitleTime(AIndex+1, FT1 - (d div 2) + Pause, TAG_CONTROL_INITIALTIME, False, False);
                    end;
        spmCPS    : if Subtitles.TextCPS[AIndex+1, AppOptions.Conventions.CPSLineLenStrategy] < Subtitles.TextCPS[AIndex, AppOptions.Conventions.CPSLineLenStrategy] then
                      SetSubtitleTime(AIndex, IT2-Pause, TAG_CONTROL_FINALTIME, False, False)
                    else
                      SetSubtitleTime(AIndex+1, FT1+Pause, TAG_CONTROL_INITIALTIME, False, False);
      end;
  end;

var
  Item    : PUWSubtitleItem;
  i, c, x : Integer;
  Run     : PVirtualNode;
begin
  if AVST.RootNodeCount < 2 then Exit;

  if Selection <> dlSelected then
  begin
    c := 0;
    x := Subtitles.Count-2;

    if (Selection = dlRange) then
    begin
      c := AFrom;
      x := ATo;
    end
    else if (Selection = dlCurrentToLast) then
      c := Range(VSTFocusedNode(AVST), 0, Subtitles.Count-2);

    if x < c then
      x := AVST.RootNodeCount-2;

    for i := c to x do
    begin
      Item := Subtitles.ItemPointer[i];
      if Assigned(Item) then
      begin
        if (Selection <> dlMarked) or ((Selection = dlMarked) and Item^.Marked) then
          SetPauseValue(i);
      end;
    end;
  end
  else
  begin
    i := 0;
    Run := AVST.GetFirstSelected;
    while Assigned(Run) do
    begin
      Item := Subtitles.ItemPointer[Run^.Index];
      if Assigned(Item) then
        SetPauseValue(Run^.Index);

      Run := AVST.GetNextSelected(Run);
    end;
  end;

  SubtitleChanged(True, True);
  UndoInstance.IncrementUndoGroup;
  DoAutoCheckErrors;
  UpdateValues(True);
end;

// -----------------------------------------------------------------------------

end.

