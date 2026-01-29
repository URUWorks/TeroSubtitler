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

type
  TFindMode = (fmBegin, fmCurrent, fmNext, fmBackward);

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
procedure VSTSelectNode(const AVST: TLazVirtualStringTree; const AIndex: Integer; const AClear: Boolean; const AForceUpdate: Boolean = False);
procedure VSTSelectNode(const AVST: TLazVirtualStringTree; const ANode: PVirtualNode; const AClear: Boolean; const AForceUpdate: Boolean = False); overload;
procedure VSTSelectNodes(const AVST: TLazVirtualStringTree; const AIdxs: TIntegerDynArray; const AClear: Boolean); overload;
procedure VSTSelectNodes(const AVST: TLazVirtualStringTree; const AFrom, ATo: Integer; const AClear: Boolean); overload;
procedure VSTMarkEntries(const AVST: TLazVirtualStringTree; const Value: Boolean = True);
procedure VSTJumpToNextMarkedEntry(const AVST: TLazVirtualStringTree; const AForward: Boolean = True);
procedure VSTJumpToNextEmptyEntry(const AVST: TLazVirtualStringTree; const AForward: Boolean = True);
procedure VSTJumpToNextNoteEntry(const AVST: TLazVirtualStringTree; const AForward: Boolean = True);
procedure VSTCopyEntriesToClipboard(const AVST: TLazVirtualStringTree; const ACut: Boolean = False);
procedure VSTPasteEntriesFromClipboard(const AVST: TLazVirtualStringTree);
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

procedure VSTSelectNode(const AVST: TLazVirtualStringTree; const AIndex: Integer; const AClear: Boolean; const AForceUpdate: Boolean = False);
begin
  VSTSelectNode(AVST, VSTGetNodeAtIndex(AVST, AIndex), AClear, AForceUpdate);
end;

// -----------------------------------------------------------------------------

procedure VSTSelectNode(const AVST: TLazVirtualStringTree; const ANode: PVirtualNode; const AClear: Boolean; const AForceUpdate: Boolean = False);
begin
  if Assigned(ANode) then
    with AVST, frmMain do
    begin
      if AClear then ClearSelection;
      FocusedNode     := ANode;
      Selected[ANode] := True;
      ScrollIntoView(ANode, True);
    end;

  if AForceUpdate then
    frmMain.VSTFocusChanged(NIL, NIL, 0);
end;

// -----------------------------------------------------------------------------

procedure VSTSelectNodes(const AVST: TLazVirtualStringTree; const AIdxs: TIntegerDynArray; const AClear: Boolean);
var
  Run: PVirtualNode;
  i: Integer;
  NodeToFocus: PVirtualNode;
begin
  if Length(AIdxs) = 0 then Exit;

  AVST.BeginUpdate;
  try
    if AVST.TotalCount > 0 then
    begin
      if AClear then AVST.ClearSelection;

      NodeToFocus := nil;
      Run := AVST.GetFirst;

      while Assigned(Run) do
      begin
        for i := Low(AIdxs) to High(AIdxs) do
        begin
          if AIdxs[i] = Run^.Index then
          begin
            AVST.Selected[Run] := True;

            if i = High(AIdxs) then
              NodeToFocus := Run;

            Break;
          end;
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

//----------------------------------------------------------------------------------------------------------------------

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

//----------------------------------------------------------------------------------------------------------------------

procedure VSTPasteEntriesFromClipboard(const AVST: TLazVirtualStringTree);
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
            SelArray[i] := InsertSubtitle(Run^.Index, ti, tf, '', '', False);
            Dec(i);
          end;
          Run := AVST.GetPreviousSelected(Run);
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
            ti  := Item^.FinalTime + Pause;
            tf  := ti + AppOptions.Conventions.NewSubtitleMs;
            SelArray[i] := InsertSubtitle(Run^.Index+1, ti, tf, '', '', False);
            Inc(i);
          end;
          Run := AVST.GetNextSelected(Run);
        end;
      end;
      VSTSelectNodes(AVST, SelArray, True);
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

//----------------------------------------------------------------------------------------------------------------------

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
                  if not RE then
                    s := ReplaceString(Text, FindText, iff(CasePreserve, PreserveCase(FindText, NewText), NewText), False)
                  else
                    s := ReplaceRegExpr(FindText, Text, iff(CasePreserve, PreserveCase(FindText, NewText), NewText), False);
                  SetSubtitleText(i, s, smText, False, False, False);
                end; //if FoundText(Text)
              if (FindLocation = fsTranslation) or (FindLocation = fsTextAndTranslation) then
                if FoundText(Translation) then
                begin
                  if not RE then
                    s := ReplaceString(Translation, FindText, iff(CasePreserve, PreserveCase(FindText, NewText), NewText), False)
                  else
                    s := ReplaceRegExpr(FindText, Translation, iff(CasePreserve, PreserveCase(FindText, NewText), NewText), False);
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

procedure VSTDeleteLineFromEntry(const AVST: TLazVirtualStringTree; const ALines: TStringArray);
var
  Nodes : TNodeArray;
  I, c, n : Integer;
begin
  Nodes := NIL;
  if (AVST.SelectedCount > 0) and (Length(ALines) > 0) then
  begin
    AVST.BeginUpdate;
    try
      Nodes := AVST.GetSortedSelection(True);

      for I := 0 to High(Nodes) do
        for c := 0 to Length(ALines)-1 do
        begin
          n := StrToIntDef(ALines[c], 0);
          if n > 0 then
            SetSubtitleText(Nodes[I]^.Index, DeleteLineFromText(Nodes[I]^.Index, n-1), smText, I=High(Nodes), I=High(Nodes), False);
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
  Pos1    : Byte;
  SumLen  : Byte;
  s       : String;
begin
  x      := Index;
  ITime  := InitialTime;
  FTime  := FinalTime;
  Result := 0;

  case Effect of
    fxFlash:
    begin
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
      CharDur := Rnd((FTime - ITime) / UTF8Length(RemoveTSTags(Text)));
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

//----------------------------------------------------------------------------------------------------------------------

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
    IT2, FT1 : Integer;
  begin
    FT1 := Subtitles[AIndex].FinalTime;
    IT2 := Subtitles[AIndex+1].InitialTime;

    if (IT2 - FT1) < Pause then
      case Mode of
        spmFirst  : SetSubtitleTime(AIndex, IT2-Pause, TAG_CONTROL_FINALTIME, False, False);
        spmSecond : SetSubtitleTime(AIndex+1, FT1+Pause, TAG_CONTROL_INITIALTIME, False, False);
        spmBoth   : begin
                      SetSubtitleTime(AIndex, FT1-((IT2-FT1) div 2), TAG_CONTROL_FINALTIME, False, False);
                      SetSubtitleTime(AIndex+1, Subtitles[AIndex].FinalTime + Pause, TAG_CONTROL_INITIALTIME, False, False);
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

