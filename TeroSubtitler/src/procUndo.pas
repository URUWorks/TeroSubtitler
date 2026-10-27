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

unit procUndo;

// -----------------------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  UWSubtitleAPI, FGL, LCLIntf, laz.VirtualTrees, UWSystem.TimeUtils;

type

  { TUndoItem }

  TUndoType = (utInsertLine, utDeleteLine, utSubtitleChange, utFPSChange);

  PUndoItem = ^TUndoItem;
  TUndoItem = record
    UndoType : TUndoType;
    Index    : Integer;
    Subtitle : TUWSubtitleItem;
    Group    : Byte;
    OldFPS   : TFPSInfo;
    NewFPS   : TFPSInfo;
  end;

  { TUndoList }

  TUndoList = specialize TFPGList<PUndoItem>;

  { TUndo }

  TUndoChangeType = (uctCount, uctItems, uctReIndex);
  TUndoChangeEvent = procedure(const ChangeType: TUndoChangeType) of object;

  TUndo = class
  private
    FUndoList      : TUndoList;
    FRedoList      : TUndoList;
    FMax           : Integer;
    FLastUndoGroup : Byte;
    FLastTickCount : Cardinal;
    FLastResentIndex : Integer;
    FLastResentType  : TUndoType;
    FOnChange      : TUndoChangeEvent;
    procedure TrimUndoList;
    function GetGroupChangeType(const AList: TUndoList): TUndoChangeType;
    procedure Changed(const ChangeType: TUndoChangeType);
    procedure SetOnChanged(const AValue: TUndoChangeEvent);
  public
    constructor Create;
    destructor Destroy; override;
    function CanUndo: Boolean;
    function CanRedo: Boolean;
    function UndoCount: Integer;
    function RedoCount: Integer;
    procedure Clear;
    procedure AddUndo(const UndoType: TUndoType; const Index: Integer; const Subtitle: TUWSubtitleItem; const IncrementGroup: Boolean = True; const ClearRedo: Boolean = True);
    procedure AddUndoIfNotResent(const UndoType: TUndoType; const Index: Integer; const Subtitle: TUWSubtitleItem; const IncrementGroup: Boolean = True; const ClearRedo: Boolean = True);
    procedure AddRedo(const UndoType: TUndoType; const Index: Integer; const Subtitle: TUWSubtitleItem; const Group: Byte);
    procedure AddUndoFPS(const OldFPS, NewFPS: TFPSInfo; const InputFPS: Boolean);
    procedure Undo(const VST: TLazVirtualStringTree);
    procedure Redo(const VST: TLazVirtualStringTree);
    procedure IncrementUndoGroup;
    property Max: Integer read FMax write FMax;
    property OnChange: TUndoChangeEvent read FOnChange write SetOnChanged;
  end;

var
  UndoInstance: TUndo;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, procVST, procWorkspace, formMain;

// -----------------------------------------------------------------------------

{ Helpers }

// -----------------------------------------------------------------------------

function NewPUndo(const AUndoType: TUndoType; const AIndex: Integer; const ASubtitle: TUWSubtitleItem; const AGroup: Byte): PUndoItem;
begin
  New(Result);
  Result^.UndoType := AUndoType;
  Result^.Index    := AIndex;
  Result^.Subtitle := ASubtitle;
  Result^.Group    := AGroup;
  //Result^.OldFPS   := NIL;
  //Result^.NewFPS   := NIL;
end;

// -----------------------------------------------------------------------------

function NewPUndoFPS(const AGroup: Byte; const AOldFPS, ANewFPS: TFPSInfo; const AInputFPS: Boolean): PUndoItem;
begin
  New(Result);
  if AInputFPS then
    Result^.Index := -1
  else
    Result^.Index := -2;

  Result^.UndoType := utFPSChange;
  Result^.Subtitle := Default(TUWSubtitleItem);
  Result^.Group    := AGroup;
  Result^.OldFPS   := AOldFPS;
  Result^.NewFPS   := ANewFPS;
end;

// -----------------------------------------------------------------------------

procedure ClearList(var AList: TUndoList);
var
  i: Integer;
begin
  if not Assigned(AList) then Exit;

  if AList.Count > 0 then
  begin
    for i := AList.Count-1 downto 0 do
    begin
      Dispose(PUndoItem(AList.Items[i]));
      AList.Items[i] := NIL;
    end;
    AList.Clear;
  end;
end;

// -----------------------------------------------------------------------------

procedure DeleteLastItem(var AList: TUndoList);
var
  i: Integer;
begin
  if not Assigned(AList) then Exit;

  i := AList.Count-1;
  Dispose(PUndoItem(AList.Items[i]));
  AList.Items[i] := NIL;
  AList.Delete(i);
end;

// -----------------------------------------------------------------------------

procedure CheckList(const VST: TLazVirtualStringTree; var FromList, ToList: TUndoList);
var
  group: Byte;
  Item: PUndoItem;
  i, it, ft: Integer;
begin
  if not Assigned(VST) or not Assigned(FromList) or not Assigned(ToList) or (FromList.Count = 0) then Exit;

  VST.BeginUpdate;
  try
    VST.ClearSelection;
    group := FromList.Last^.Group;

    while (FromList.Count > 0) and (FromList.Last^.Group = group) do
    begin
      Item := FromList.Last;

      case Item^.UndoType of
        utSubtitleChange:
          if Subtitles.ValidIndex(Item^.Index) then
          begin
            ToList.Add(NewPUndo(utSubtitleChange, Item^.Index, Subtitles.Items[Item^.Index], Item^.Group));
            Subtitles.Items[Item^.Index] := Item^.Subtitle;
            VSTSelectNode(VST, Item^.Index, False, True);
          end;

        utInsertLine:
          if Subtitles.ValidIndex(Item^.Index) then
          begin
            ToList.Add(NewPUndo(utDeleteLine, Item^.Index, Subtitles.Items[Item^.Index], Item^.Group));
            Subtitles.Delete(Item^.Index);
          end;

        utDeleteLine:
          begin
            ToList.Add(NewPUndo(utInsertLine, Item^.Index, Item^.Subtitle, Item^.Group));
            Subtitles.Insert(Item^.Index, Item^.Subtitle, NIL);
            VSTSelectNode(VST, Item^.Index, False, True);
          end;

        utFPSChange:
        begin
          ToList.Add(NewPUndoFPS(Item^.Group, Item^.NewFPS, Item^.OldFPS, Item^.Index = -1));

          for i := 0 to Subtitles.Count - 1 do
          begin
            if Item^.Index = -1 then
            begin
              it := TimeToFrames(Subtitles.Items[i].InitialTime, Item^.OldFPS.FPS);
              ft := TimeToFrames(Subtitles.Items[i].FinalTime, Item^.OldFPS.FPS);
              Subtitles.ItemPointer[i]^.InitialTime := FramesToTime(it, Item^.NewFPS.FPS);
              Subtitles.ItemPointer[i]^.FinalTime   := FramesToTime(ft, Item^.NewFPS.FPS);
            end
            else
            begin
              it := TimeToFrames(Subtitles.Items[i].InitialTime, Item^.NewFPS.FPS);
              ft := TimeToFrames(Subtitles.Items[i].FinalTime, Item^.NewFPS.FPS);
              Subtitles.ItemPointer[i]^.InitialTime := FramesToTime(it, Item^.OldFPS.FPS);
              Subtitles.ItemPointer[i]^.FinalTime   := FramesToTime(ft, Item^.OldFPS.FPS);
            end;
          end;

          if Item^.Index = -1 then
          begin
            Workspace.FPS.InputFPS  := Item^.OldFPS;
            Workspace.FPS.OutputFPS := Item^.OldFPS;

            frmMain.cboInputFPS.ItemIndex := FPSInfoToIndex(Workspace.FPS.InputFPS);
            frmMain.cboFPS.ItemIndex := frmMain.cboInputFPS.ItemIndex;
          end
          else
          begin
            Workspace.FPS.OutputFPS := Item^.OldFPS;
            frmMain.cboFPS.ItemIndex := FPSInfoToIndex(Workspace.FPS.OutputFPS);
          end;

          SetTimeFPStoTimeEditCtrls;
        end;
      end;

      DeleteLastItem(FromList);
    end;
  finally
    VST.EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

{ TUndo }

// -----------------------------------------------------------------------------

constructor TUndo.Create;
begin
  FUndoList      := TUndoList.Create;
  FRedoList      := TUndoList.Create;
  FMax           := 20000; // maximum number of undo items kept
  FLastUndoGroup := 0;
  FLastTickCount := GetTickCount;
  FLastResentIndex := -1;
  FLastResentType  := utSubtitleChange;
end;

// -----------------------------------------------------------------------------

destructor TUndo.Destroy;
begin
  FOnChange := NIL;
  Clear;
  FUndoList.Free;
  FRedoList.Free;

  inherited Destroy;
end;

// -----------------------------------------------------------------------------

function TUndo.CanUndo: Boolean;
begin
  Result := UndoCount > 0;
end;

// -----------------------------------------------------------------------------

function TUndo.CanRedo: Boolean;
begin
  Result := RedoCount > 0;
end;

// -----------------------------------------------------------------------------

function TUndo.UndoCount: Integer;
begin
  Result := FUndoList.Count;
end;

// -----------------------------------------------------------------------------

function TUndo.RedoCount: Integer;
begin
  Result := FRedoList.Count;
end;

// -----------------------------------------------------------------------------

procedure TUndo.Clear;
begin
  ClearList(FUndoList);
  ClearList(FRedoList);
end;

// -----------------------------------------------------------------------------

procedure TUndo.Changed(const ChangeType: TUndoChangeType);
begin
  if Assigned(FOnChange) then FOnChange(ChangeType);
end;

// -----------------------------------------------------------------------------

procedure TUndo.SetOnChanged(const AValue: TUndoChangeEvent);
begin
  FOnChange := AValue;
  Changed(uctCount);
end;

// -----------------------------------------------------------------------------

procedure TUndo.AddUndo(const UndoType: TUndoType; const Index: Integer; const Subtitle: TUWSubtitleItem; const IncrementGroup: Boolean = True; const ClearRedo: Boolean = True);
begin
  FUndoList.Add(NewPUndo(UndoType, Index, Subtitle, FLastUndoGroup));
  TrimUndoList;
  if IncrementGroup then IncrementUndoGroup;
  if ClearRedo then ClearList(FRedoList);
  Changed(uctCount);
end;

// -----------------------------------------------------------------------------

procedure TUndo.AddUndoIfNotResent(const UndoType: TUndoType; const Index: Integer; const Subtitle: TUWSubtitleItem; const IncrementGroup: Boolean = True; const ClearRedo: Boolean = True);
begin
  // merge with the previous undo only when it was for the same entry and the
  // same kind of change, sent less than 500ms ago (e.g. typing in the memo)
  if ((GetTickCount - FLastTickCount) > 500) or
     (Index <> FLastResentIndex) or (UndoType <> FLastResentType) then
  begin
    AddUndo(UndoType, Index, Subtitle, False, ClearRedo);
    if IncrementGroup then IncrementUndoGroup;
    FLastResentIndex := Index;
    FLastResentType  := UndoType;
    FLastTickCount   := GetTickCount;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUndo.TrimUndoList;
var
  Group: Byte;
begin
  // drop whole groups from the oldest end so memory doesn't grow forever
  if (FMax <= 0) or (FUndoList.Count <= FMax) then Exit;

  while (FUndoList.Count > FMax) do
  begin
    Group := FUndoList.First^.Group;
    while (FUndoList.Count > 1) and (FUndoList.First^.Group = Group) do
    begin
      Dispose(FUndoList.First);
      FUndoList.Delete(0);
    end;
  end;
end;

// -----------------------------------------------------------------------------

function TUndo.GetGroupChangeType(const AList: TUndoList): TUndoChangeType;
var
  i: Integer;
  Group: Byte;
begin
  // reindex if any item of the last group inserts or deletes lines
  Result := uctItems;
  if AList.Count = 0 then Exit;
  Group := AList.Last^.Group;
  for i := AList.Count-1 downto 0 do
  begin
    if AList[i]^.Group <> Group then Break;
    if AList[i]^.UndoType in [utInsertLine, utDeleteLine] then
      Exit(uctReIndex);
  end;
end;

// -----------------------------------------------------------------------------

procedure TUndo.AddRedo(const UndoType: TUndoType; const Index: Integer; const Subtitle: TUWSubtitleItem; const Group: Byte);
begin
  FRedoList.Add(NewPUndo(UndoType, Index, Subtitle, Group));
  Changed(uctCount);
end;

// -----------------------------------------------------------------------------

procedure TUndo.AddUndoFPS(const OldFPS, NewFPS: TFPSInfo; const InputFPS: Boolean);
begin
  FUndoList.Add(NewPUndoFPS(FLastUndoGroup, OldFPS, NewFPS, InputFPS));
  IncrementUndoGroup;
  ClearList(FRedoList);
  Changed(uctCount);
end;

// -----------------------------------------------------------------------------

procedure TUndo.Undo(const VST: TLazVirtualStringTree);
var
  ChangeType: TUndoChangeType;
begin
  if not CanUndo or (VST = NIL) then Exit;

  ChangeType := GetGroupChangeType(FUndoList);

  CheckList(VST, FUndoList, FRedoList);
  Changed(ChangeType);
  VST.Invalidate;
end;

// -----------------------------------------------------------------------------

procedure TUndo.Redo(const VST: TLazVirtualStringTree);
var
  ChangeType: TUndoChangeType;
begin
  if not CanRedo or (VST = NIL) then Exit;

  ChangeType := GetGroupChangeType(FRedoList);

  CheckList(VST, FRedoList, FUndoList);
  Changed(ChangeType);
  VST.Invalidate;
end;

// -----------------------------------------------------------------------------

procedure TUndo.IncrementUndoGroup;
begin
  if FLastUndoGroup < 100 then
    Inc(FLastUndoGroup)
  else
    FLastUndoGroup := 0;
end;

// -----------------------------------------------------------------------------

initialization
  UndoInstance := TUndo.Create;

// -----------------------------------------------------------------------------

finalization
  UndoInstance.Free;

// -----------------------------------------------------------------------------

end.
