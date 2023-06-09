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

unit procVST;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Types, Forms, laz.VirtualTrees, procTypes;

function VSTUpdating(const AVST: TLazVirtualStringTree): Boolean;
procedure VSTBeginUpdate(const AVST: TLazVirtualStringTree);
procedure VSTEndUpdate(const AVST: TLazVirtualStringTree);

procedure VSTAddColumn(const AVST: TLazVirtualStringTree; const AText: String; const AWidth: Integer; const AAlignment: TAlignment = taCenter; const AVisible: Boolean = True);

procedure VSTPaintCell(const TargetCanvas: TCanvas; const CellRect: TRect; const Color: TColor);
procedure VSTSetNodeHeight(const AVST: TLazVirtualStringTree; const AHeight: Integer);
procedure VSTShowColumn(const AVST: TLazVirtualStringTree; const Index: Integer; const Visible: Boolean);

function VSTFocusedNode(const AVST: TLazVirtualStringTree): Integer;
function VSTLastSelectedNode(const AVST: TLazVirtualStringTree): Integer;
function VSTGetNodeAtIndex(const AVST: TLazVirtualStringTree; const AIndex: Integer): PVirtualNode;
procedure VSTSelectNode(const AVST: TLazVirtualStringTree; const AIndex: Integer; const AClear: Boolean); overload;
procedure VSTSelectNode(const AVST: TLazVirtualStringTree; const ANode: PVirtualNode; const AClear: Boolean); overload;
procedure VSTSelectNodes(const AVST: TLazVirtualStringTree; const AIdxs: TIntegerDynArray; const AClear: Boolean); overload;
procedure VSTSelectNodes(const AVST: TLazVirtualStringTree; const AFrom, ATo: Integer; const AClear: Boolean); overload;
procedure VSTMarkSubtitles(const AVST: TLazVirtualStringTree; const Value: Boolean = True);
procedure VSTJumpToNextMarkedSubtitle(const AVST: TLazVirtualStringTree; const AForward: Boolean = True);
procedure VSTCopySubtitlesToClipboard(const AVST: TLazVirtualStringTree; const ACut: Boolean = False);
procedure VSTPasteSubtitlesFromClipboard(const AVST: TLazVirtualStringTree);
procedure VSTInsertSubtitles(const AVST: TLazVirtualStringTree; const Before: Boolean = False);
procedure VSTDeleteSubtitles(const AVST: TLazVirtualStringTree);
procedure VSTCombineSubtitles(const AVST: TLazVirtualStringTree);
procedure VSTDivideSubtitles(const AVST: TLazVirtualStringTree);

function VSTFind(const FindText: String; const CaseSensitive: Boolean; const FromBegining: Boolean; const Backward: Boolean = False; const Replace: Boolean = False; const NewText: String = ''; const ReplaceAll: Boolean = False; const CasePreserve: Boolean = False; const WholeWord: Boolean = False; const RE: Boolean = False): Boolean;
procedure VSTFindPrevious;
procedure VSTFindNext;
procedure VSTDoLoop(const AVST: TLazVirtualStringTree; Proc: TVSTDoLoopProc; const Selection: TVSTDoLoopSelection = dlSelected; const Refresh: Boolean = True; const IncrementUndo: Boolean = False; const CBProc: TVSTDoLoopProcCB = NIL; const AFrom: Integer = 0; const ATo: Integer = 0);

procedure VSTAdjustSubtitles(const AdjSub: TAdjustSubtitles);
procedure VSTSort(const AVST: TLazVirtualStringTree);

// -----------------------------------------------------------------------------

implementation

uses procWorkspace, procUndo, procSubtitle, Clipbrd, RegExpr, UWSubtitles.Utils,
  UWSubtitleAPI, UWSystem.SysUtils, UWSystem.StrUtils, formMain, Math;

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

function VSTLastSelectedNode(const AVST: TLazVirtualStringTree): Integer;
var
  Node: PVirtualNode;
begin
  Result := -1;
  Node   := AVST.GetFirstSelected;
  if Assigned(Node) then
  begin
    while Assigned(AVST.GetNextSelected(Node)) do
      Node := AVST.GetNextSelected(Node);

    Result := Node^.Index;
  end;
end;

// -----------------------------------------------------------------------------

function VSTGetNodeAtIndex(const AVST: TLazVirtualStringTree; const AIndex: Integer): PVirtualNode;
var
  Run : PVirtualNode;
begin
  Result := NIL;

  with AVST do
    if TotalCount > 0 then
    begin
      Run := GetFirst;
      while Assigned(Run) do
      begin
        if AIndex = Run^.Index then
        begin
          Result := Run;
          Break;
        end;
        Run := GetNext(Run);
      end;
    end;
end;

// -----------------------------------------------------------------------------

procedure VSTSelectNode(const AVST: TLazVirtualStringTree; const AIndex: Integer; const AClear: Boolean);
begin
  VSTSelectNode(AVST, VSTGetNodeAtIndex(AVST, AIndex), AClear);
end;

// -----------------------------------------------------------------------------

procedure VSTSelectNode(const AVST: TLazVirtualStringTree; const ANode: PVirtualNode; const AClear: Boolean);
begin
  if Assigned(ANode) then
    with AVST do
    begin
      if AClear then ClearSelection;
      FocusedNode     := ANode;
      Selected[ANode] := True;
      ScrollIntoView(ANode, True);
    end;
end;

// -----------------------------------------------------------------------------

procedure VSTSelectNodes(const AVST: TLazVirtualStringTree; const AIdxs: TIntegerDynArray; const AClear: Boolean);
var
  Run : PVirtualNode;
  i   : Integer;
begin
  if Length(AIdxs) = 0 then Exit;

  with AVST do
    if TotalCount > 0 then
    begin
      if AClear then ClearSelection;
      Run := GetFirst;
      while Assigned(Run) do
      begin
        for i := Low(AIdxs) to High(AIdxs) do
        if AIdxs[i] = Run^.Index then
        begin
          Selected[Run] := True;
        end;
        Run := GetNext(Run);
      end;
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

procedure VSTMarkSubtitles(const AVST: TLazVirtualStringTree; const Value: Boolean = True);
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

//----------------------------------------------------------------------------------------------------------------------

procedure VSTJumpToNextMarkedSubtitle(const AVST: TLazVirtualStringTree; const AForward: Boolean = True);
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
          ClearSelection;
          Selected[Run] := True;
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

procedure VSTCopySubtitlesToClipboard(const AVST: TLazVirtualStringTree; const ACut: Boolean = False);
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
      if ACut then VSTDeleteSubtitles(AVST);
      AVST.EndUpdate;
      UpdateValues(True);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure VSTPasteSubtitlesFromClipboard(const AVST: TLazVirtualStringTree);
var
  s      : TStringList;
  i, x   : Integer;
  t1, t2 : Integer;
begin
  if AVST.SelectedCount > 0 then
  begin
    AVST.BeginUpdate;
    s := TStringList.Create;
    try
      x := VSTFocusedNode(AVST);
      AVST.ClearSelection;
      SplitRegExpr('\|\|', Clipboard.AsText, s);
      while s.Count > 4 do
      begin
        t1 := StrToIntDef(s[0], 0);
        t2 := StrToIntDef(s[1], 0);
        InsertSubtitle(x, t1, t2,
          ReplaceRegExpr('\\~', s[2], sLineBreak, False),
          ReplaceRegExpr('\\~', s[3], sLineBreak, False), False);
        AVST.Selected[VSTGetNodeAtIndex(AVST, x)] := True;
        inc(x);
        for i := 1 to 4 do s.Delete(0);
      end;
    finally
      s.Free;
      AVST.EndUpdate;
      UndoInstance.IncrementUndoGroup;
      UpdateValues(True);
      DoAutoCheckErrors;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure VSTInsertSubtitles(const AVST: TLazVirtualStringTree; const Before: Boolean = False);
var
  Run       : PVirtualNode;
  Item      : PUWSubtitleItem;
  ti, tf    : Integer;
  pos, i    : Integer;
  SelArray  : TIntegerDynArray;
begin
  // Multiple subtitles selected
  if AVST.SelectedCount > 1 then
  begin
    i := 0;
    SelArray := NIL;
    SetLength(SelArray, AVST.SelectedCount);
    try
      Run := AVST.GetFirstSelected;
      while Assigned(Run) do
      begin
        Item := Subtitles.ItemPointer[Run^.Index];
        if Assigned(Item) then
        begin
          ti  := (Item^.FinalTime + 1);
          tf  := ti + AppOptions.Conventions.NewSubtitleMs;
          SelArray[i] := InsertSubtitle(Run^.Index+1, ti, tf, '', '', False);
          Inc(i);
        end;
        Run := AVST.GetNextSelected(Run);
      end;
      VSTSelectNodes(AVST, SelArray, True);
    finally
      SetLength(SelArray, 0);
      UndoInstance.IncrementUndoGroup;
    end;
  end
  else // only one ?
  begin
    pos := -1;
    i   := VSTFocusedNode(AVST);

    if i >= 0 then
    begin
      if frmMain.MPV.GetMediaLenInMs = 0 then
        ti := Subtitles[i].FinalTime + 1
      else
        ti := frmMain.MPV.GetMediaPosInMs;

      if not Before then
        pos := i+1
      else
        pos := i;
    end
    else if Subtitles.Count > 0 then
      ti := Subtitles.Items[Subtitles.Count].FinalTime + 1
    else
      ti := 0;

    //ti := ti + Options.FakeStart;
    tf := ti + AppOptions.Conventions.NewSubtitleMs;
    VSTSelectNode(AVST, InsertSubtitle(pos, ti, tf, '', ''), True);
  end;
  DoAutoCheckErrors;
end;

// -----------------------------------------------------------------------------

procedure VSTDeleteSubtitles(const AVST: TLazVirtualStringTree);
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

//----------------------------------------------------------------------------------------------------------------------

procedure VSTCombineSubtitles(const AVST: TLazVirtualStringTree);
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

      InsertSubtitle(x, it, ft, s, st);
    finally
      AVST.ClearSelection;
      AVST.EndUpdate;
    end;
  end;
  DoAutoCheckErrors;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure VSTDivideSubtitles(const AVST: TLazVirtualStringTree);
var
  s : TStringList;
  NodeArray : TNodeArray;
  Item : PUWSubtitleItem;
  i, c, x, t1,t2 : Integer;
begin
  if AVST.SelectedCount > 0 then
  begin
    s := TStringList.Create;
    try
      NodeArray := AVST.GetSortedSelection(False);
      if NodeArray = NIL then Exit;
      for i := High(NodeArray) downto Low(NodeArray) do
      begin
        x    := frmMain.VST.AbsoluteIndex(NodeArray[i]);
        Item := Subtitles.ItemPointer[x];
        if Assigned(Item) then
        with Item^, AppOptions.Conventions do
        begin
          SplitRegExpr('\|\|', DivideLines(Text, InitialTime, FinalTime, AppOptions.Conventions.DotsOnSplit, AppOptions.Conventions.CPL), s);
          DeleteSubtitle(x, False, False);

          while s.Count >= 3 do
          begin
            t1 := StrToIntDef(s[0], 0);
            t2 := StrToIntDef(s[1], 0);
            InsertSubtitle(x, t1, t2, s[2], '', False, False);
            for c := 1 to 3 do s.Delete(0);
            inc(x);
          end;
        end;
      end;
      NodeArray := NIL;
    finally
      s.Free;

      UndoInstance.IncrementUndoGroup;
      SubtitleChanged(True, True);
      UpdateValues(True);
    end;
  end;
  DoAutoCheckErrors;
end;

// -----------------------------------------------------------------------------

function VSTFind(const FindText: String; const CaseSensitive: Boolean; const FromBegining: Boolean; const Backward: Boolean = False; const Replace: Boolean = False; const NewText: String = ''; const ReplaceAll: Boolean = False; const CasePreserve: Boolean = False; const WholeWord: Boolean = False; const RE: Boolean = False): Boolean;

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

var
  Item : PUWSubtitleItem;
  i, c : Integer;
  s    : String;
begin
  Result := False;
  AppOptions.TextToFind := FindText;

  if Subtitles.Count > 0 then
  begin
    if Backward then
    begin
      for i := (VSTFocusedNode(frmMain.VST)-1) downto 0 do
      begin
        Item := Subtitles.ItemPointer[i];
        if Assigned(Item) then
          with Item^ do
          begin
            if FoundText(Text) or FoundText(Translation) then
            begin
              Result := True;
              VSTSelectNode(frmMain.VST, i, True);
              Break;
            end;
          end;
      end;
      Exit;
    end;

    if FromBegining then
      c := 0
    else
      c := Range(VSTFocusedNode(frmMain.VST)+1, 0, Subtitles.Count-1);

    for i := c to Subtitles.Count-1 do
    begin
      Item := Subtitles.ItemPointer[i];
      if Assigned(Item) then
        with Item^ do
        begin
          if FoundText(Text) or FoundText(Translation) then
          begin
            Result := True;
            VSTSelectNode(frmMain.VST, i, True);
            if not Replace then // Only Find...
              Break
            else
            begin // Find & Replace...
              if FoundText(Text) then
              begin
                if not RE then
                  s := ReplaceString(Text, FindText, iff(CasePreserve, PreserveCase(FindText, NewText), NewText), False)
                else
                  s := ReplaceRegExpr(FindText, Text, iff(CasePreserve, PreserveCase(FindText, NewText), NewText), False);

                SetSubtitleText(i, s, smText, False);
              end;
              if FoundText(Translation) then
              begin
                if not RE then
                  s := ReplaceString(Translation, FindText, iff(CasePreserve, PreserveCase(FindText, NewText), NewText), False)
                else
                  s := ReplaceRegExpr(FindText, Translation, iff(CasePreserve, PreserveCase(FindText, NewText), NewText), False);

                SetSubtitleText(i, s, smTranslation, False);
              end;
              if not ReplaceAll then Break;
            end;
          end;
        end;
    end;
  end;

  if Replace then UpdateValues(True);

  with frmMain do
  begin
    actFindNext.Enabled     := not AppOptions.TextToFind.IsEmpty;
    actFindPrevious.Enabled := actFindNext.Enabled;
  end;
end;

// -----------------------------------------------------------------------------

procedure VSTFindPrevious;
begin
  with AppOptions do
    if TextToFind <> '' then VSTFind(TextToFind, False, False, True);
end;

// -----------------------------------------------------------------------------

procedure VSTFindNext;
begin
  with AppOptions do
    if TextToFind <> '' then VSTFind(TextToFind, False, False);
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
    else if (Selection = dlSelected) or (Selection = dlCurrentToLast) then
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
    UndoInstance.IncrementUndoGroup;
    SubtitleChanged(True, True);
  end;
  if Refresh then UpdateValues(True);
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
        DeltaX := Delta2 / (x-Subtitles[AdjSub[i].SubtitleIndex].InitialTime);

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
  i, a, c : Integer;
  tmp     : TUWSubtitleItem;
  List    : array of TUWSubtitleItem;
begin
  if AVST.RootNodeCount > 0 then
  begin
    SetLength(List, Subtitles.Count);
    try
      for i := 0 to Subtitles.Count-1 do
        List[i] := Subtitles.Items[i];

      c := UndoInstance.UndoCount;
      for i := 0 to High(List) do
        for a := i to High(List) do
          if List[i].InitialTime > List[a].InitialTime then
          begin
            UndoInstance.AddUndo(utSubtitleChange, i, List[i], False);
            UndoInstance.AddUndo(utSubtitleChange, a, List[a], False);

            tmp     := List[i];
            List[i] := List[a];
            List[a] := tmp;
          end;

      if UndoInstance.UndoCount <> c then
      begin
        for i := 0 to Subtitles.Count-1 do
          Subtitles.Items[i] := List[i];

        UndoInstance.IncrementUndoGroup;
        DoAutoCheckErrors(False);
        AVST.Invalidate;
      end;
    finally
      SetLength(List, 0);
    end;
  end;
end;

// -----------------------------------------------------------------------------

end.

