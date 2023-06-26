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

unit procUndo;

// -----------------------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  UWSubtitleAPI, FGL, LCLIntf, laz.VirtualTrees;

type

  { TUndoItem }

  TUndoType = (utInsertLine, utDeleteLine, utSubtitleChange);

  PUndoItem = ^TUndoItem;
  TUndoItem = record
    UndoType : TUndoType;
    Index    : Integer;
    Subtitle : TUWSubtitleItem;
    Group    : Byte;
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
    FMax           : Byte;
    FLastUndoGroup : Byte;
    FLastTickCount : Cardinal;
    FOnChange      : TUndoChangeEvent;
    procedure Changed(const ChangeType: TUndoChangeType);
    procedure SetOnChanged(const AValue: TUndoChangeEvent);
  public
    constructor Create;
    destructor Destroy; override;
    function CanUndo: Boolean;
    function CanRedo: Boolean;
    function UndoCount: Integer;
    function RedoCount: Integer;
    procedure AddUndo(const UndoType: TUndoType; const Index: Integer; const Subtitle: TUWSubtitleItem; const IncrementGroup: Boolean = True; const ClearRedo: Boolean = True);
    procedure AddUndoIfNotResent(const UndoType: TUndoType; const Index: Integer; const Subtitle: TUWSubtitleItem; const IncrementGroup: Boolean = True; const ClearRedo: Boolean = True);
    procedure AddRedo(const UndoType: TUndoType; const Index: Integer; const Subtitle: TUWSubtitleItem; const Group: Byte);
    procedure Undo(const VST: TLazVirtualStringTree);
    procedure Redo(const VST: TLazVirtualStringTree);
    procedure IncrementUndoGroup;
    property Max: Byte read FMax write FMax;
    property OnChange: TUndoChangeEvent read FOnChange write SetOnChanged;
  end;

var
  UndoInstance: TUndo;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, procVST;

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
      AList.Delete(i);
    end;
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
  i     : Integer;
  group : Byte;
begin
  if not Assigned(VST) or not Assigned(FromList) or not Assigned(ToList) then Exit;

  VST.ClearSelection;

  group := FromList.Last^.Group;
  if (FromList.Last^.UndoType = utSubtitleChange) and Subtitles.ValidIndex(FromList.Last^.Index) then
  begin
    ToList.Add(NewPUndo(utSubtitleChange, FromList.Last^.Index, Subtitles.Items[FromList.Last^.Index], FromList.Last^.Group));
    Subtitles.Items[FromList.Last^.Index] := FromList.Last^.Subtitle;
    VSTSelectNode(VST, FromList.Last^.Index, False);
  end
  else if (FromList.Last^.UndoType = utInsertLine) and Subtitles.ValidIndex(FromList.Last^.Index) then
  begin
    ToList.Add(NewPUndo(utDeleteLine, FromList.Last^.Index, Subtitles.Items[FromList.Last^.Index], FromList.Last^.Group));
    Subtitles.Delete(FromList.Last^.Index);
  end
  else if (FromList.Last^.UndoType = utDeleteLine) then
  begin
    ToList.Add(NewPUndo(utInsertLine, FromList.Last^.Index, Subtitles.Items[FromList.Last^.Index], FromList.Last^.Group));
    Subtitles.Insert(FromList.Last^.Index, FromList.Last^.Subtitle, NIL);
    VSTSelectNode(VST, FromList.Last^.Index, False);
  end;

  DeleteLastItem(FromList);
  for i := FromList.Count-1 downto 0 do
  begin
    if group = FromList[i]^.Group then
    begin
      if (FromList.Last^.UndoType = utSubtitleChange) and Subtitles.ValidIndex(FromList[i]^.Index) then
      begin
        ToList.Add(NewPUndo(utSubtitleChange, FromList[i]^.Index, Subtitles.Items[FromList.Last^.Index], FromList[i]^.Group));
        Subtitles.Items[FromList[i]^.Index] := FromList[i]^.Subtitle;
        VSTSelectNode(VST, FromList.Last^.Index, False);
      end
      else if (FromList.Last^.UndoType = utInsertLine) and Subtitles.ValidIndex(FromList[i]^.Index) then
      begin
        ToList.Add(NewPUndo(utDeleteLine, FromList[i]^.Index, Subtitles.Items[FromList.Last^.Index], FromList[i]^.Group));
        Subtitles.Delete(FromList[i]^.Index);
      end
      else if FromList.Last^.UndoType = utDeleteLine then
      begin
        ToList.Add(NewPUndo(utInsertLine, FromList[i]^.Index, Subtitles.Items[FromList.Last^.Index], FromList[i]^.Group));
        Subtitles.Insert(FromList[i]^.Index, FromList[i]^.Subtitle, NIL);
        VSTSelectNode(VST, FromList[i]^.Index, False);
      end;
      DeleteLastItem(FromList);
    end
    else
      Break;
  end;
end;

// -----------------------------------------------------------------------------

{ TUndo }

// -----------------------------------------------------------------------------

constructor TUndo.Create;
begin
  FUndoList      := TUndoList.Create;
  FRedoList      := TUndoList.Create;
  FMax           := 100;
  FLastUndoGroup := 0;
  FLastTickCount := GetTickCount;
end;

// -----------------------------------------------------------------------------

destructor TUndo.Destroy;
begin
  ClearList(FUndoList);
  ClearList(FRedoList);
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
  if FUndoList.Count >= FMax then FUndoList.Delete(0);

  FUndoList.Add(NewPUndo(UndoType, Index, Subtitle, FLastUndoGroup));
  if IncrementGroup then IncrementUndoGroup;
  if ClearRedo then ClearList(FRedoList);
  Changed(uctCount);
end;

// -----------------------------------------------------------------------------

procedure TUndo.AddUndoIfNotResent(const UndoType: TUndoType; const Index: Integer; const Subtitle: TUWSubtitleItem; const IncrementGroup: Boolean = True; const ClearRedo: Boolean = True);
begin
  if ((GetTickCount - FLastTickCount) > 500) then // add undo only if > 500ms
  begin
    AddUndo(UndoType, Index, Subtitle, ClearRedo);
    if IncrementGroup then IncrementUndoGroup;
    FLastTickCount := GetTickCount;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUndo.AddRedo(const UndoType: TUndoType; const Index: Integer; const Subtitle: TUWSubtitleItem; const Group: Byte);
begin
  FRedoList.Add(NewPUndo(UndoType, Index, Subtitle, Group));
  Changed(uctCount);
end;

// -----------------------------------------------------------------------------

procedure TUndo.Undo(const VST: TLazVirtualStringTree);
begin
  if not CanUndo then Exit;

  case FUndoList.Last^.UndoType of
    utInsertLine: begin // then delete!
                    if (Subtitles.Count > 0) and (FUndoList.Last^.Index <= Subtitles.Count) then
                    begin
                      CheckList(VST, FUndoList, FRedoList);
                      Changed(uctReIndex);
                    end;
                  end;
    utDeleteLine: begin // then insert!
                    CheckList(VST, FUndoList, FRedoList);
                    Changed(uctReIndex);
                  end;
    else // utSubtitleChange then Undo!
      if (Subtitles.Count > 0) and (FUndoList.Count > 0) and (FUndoList.Last^.Index <= Subtitles.Count) then
      begin
        CheckList(VST, FUndoList, FRedoList);
        Changed(uctItems);
      end;
    end;

  VST.Invalidate;
end;

// -----------------------------------------------------------------------------

procedure TUndo.Redo(const VST: TLazVirtualStringTree);
begin
  if not CanRedo then Exit;

  case FRedoList.Last^.UndoType of
    utInsertLine: begin // then Insert!
                    if (Subtitles.Count > 0) and (FRedoList.Last^.Index <= Subtitles.Count) then
                    begin
                      CheckList(VST, FRedoList, FUndoList);
                      Changed(uctReIndex);
                    end;
                  end;
    utDeleteLine: begin // then Delete!
                    CheckList(VST, FRedoList, FUndoList);
                    Changed(uctReIndex);
                  end;
    else // utSubtitleChange then Redo!
      if (Subtitles.Count > 0) and (FRedoList.Count > 0) and (FRedoList.Last^.Index <= Subtitles.Count) then
      begin
        CheckList(VST, FRedoList, FUndoList);
        Changed(uctItems);
      end;
    end;

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
