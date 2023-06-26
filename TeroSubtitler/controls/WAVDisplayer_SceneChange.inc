{*
 *  URUWorks Waveform Displayer Control
 *
 *  Copyright (C) 2021-2023 URUWorks, uruworks@gmail.com.
 *
 *  Based on the great work of:
 * -----------------------------------------------------------------------------
 *  VisualSubSync
 * -----------------------------------------------------------------------------
 *  Copyright (C) 2003 Christophe Paris
 * -----------------------------------------------------------------------------
 *  This Program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This Program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with GNU Make; see the file COPYING.  If not, write to
 *  the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 *  http://www.gnu.org/copyleft/gpl.html
 * -----------------------------------------------------------------------------
 *}

//------------------------------------------------------------------------------

{ SceneChange }

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.SetSceneChangeEnabled(const AValue: Boolean);
begin
  FSceneChangeEnabled := AValue;
  if IsTimeLineEnabled and (GetSceneChangeCount > 0) then
    UpdateView([uvfSubtitle]);
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.LoadSceneChangeFile(const AFileName: String);
var
  i : Integer;
  s : TStrings;
begin
  if FileExists(AFileName) then
  begin
    s := TStringList.Create;
    try
      s.LoadFromFile(AFileName);
      if s.Count > 0 then
      begin
        SetLength(FSceneChangeList, s.Count);
        for i := 0 to s.Count-1 do
          FSceneChangeList[i] := StrToIntDef(s[i], 0);

        SetSceneChangeEnabled(FSceneChangeEnabled);
      end;
    finally
      s.Free;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.SetSceneChangeList(const SceneChangeList: TIntegerDynArray);
begin
  if (Length(SceneChangeList) > 0) then
  begin
    SetLength(FSceneChangeList, Length(SceneChangeList));
    Move(SceneChangeList[0], FSceneChangeList[0], Length(SceneChangeList) * SizeOf(Integer));
    SetSceneChangeEnabled(FSceneChangeEnabled);
  end;
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.ClearSceneChange;
begin
  SetLength(FSceneChangeList, 0);
  UpdateView([uvfSubtitle]);
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplayer.GetSceneChangeCount: Integer;
begin
  Result := Length(FSceneChangeList);
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplayer.GetSceneChangeAt(const Index: Integer): Integer;
begin
  if (Index >= Low(FSceneChangeList)) and (Index <= High(FSceneChangeList)) then
    Result := FSceneChangeList[Index]
  else
    Result := -1;
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplayer.GetNextSceneChangeIndex(const ATimeMS: Integer; const ABackward: Boolean = False): Integer;
begin
  Result := BinarySearchSC(FSceneChangeList, ATimeMS, ABackward);
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplayer.GetNextSceneChange(const ATimeMS: Integer): Integer;
begin
  Result := GetSceneChangeAt(GetNextSceneChangeIndex(ATimeMS));
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplayer.GetPreviousSceneChange(const ATimeMS: Integer): Integer;
begin
  Result := GetSceneChangeAt(GetNextSceneChangeIndex(ATimeMS, True));
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplayer.ContainsSceneChange(const AStart, AStop: Integer): Boolean;
var
  I : Integer;
begin
  I := GetNextSceneChangeIndex(AStart);
  Result := (I <= High(FSceneChangeList)) and (FSceneChangeList[I] <= AStop);
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplayer.DeleteSceneChange(const AInitialTimeMS, AFinalTimeMS: Integer): TIntegerDynArray;
var
  StartIdx, StopIdx, Count, I, J: Integer;
begin
  StartIdx := GetNextSceneChangeIndex(AInitialTimeMS);
  StopIdx  := GetNextSceneChangeIndex(AFinalTimeMS, True);
  if (StopIdx = -1) then
  begin
    SetLength(Result, 0);
    Exit;
  end;
  Count := StopIdx - StartIdx + 1;

  SetLength(Result, Count);
  J := 0;
  for I := StartIdx to StopIdx do
  begin
    Result[J] := FSceneChangeList[I];
    Inc(J);
  end;

  for I := (StopIdx + 1) to High(FSceneChangeList) do
  begin
    FSceneChangeList[I - Count] := FSceneChangeList[I];
  end;
  SetLength(FSceneChangeList, Length(FSceneChangeList) - Count);
  UpdateView([uvfSubtitle]);
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.InsertSceneChange(const Src: TIntegerDynArray);
var
  NewSCArray : TIntegerDynArray;
  I, J, K : Integer;
begin
  I := 0;
  J := 0;
  K := 0;
  SetLength(NewSCArray, Length(FSceneChangeList) + Length(Src));
  while (I < Length(FSceneChangeList)) and (J < Length(Src))  do
  begin
    while (I < Length(FSceneChangeList)) and (FSceneChangeList[I] < Src[J]) do
    begin
      NewSCArray[K] := FSceneChangeList[I];
      Inc(K);
      Inc(I);
    end;
    while (J < Length(Src)) and (FSceneChangeList[I] > Src[J]) do
    begin
      NewSCArray[K] := Src[J];
      Inc(K);
      Inc(J);
    end;
    while (J < Length(Src)) and (I < Length(FSceneChangeList)) and (FSceneChangeList[I] = Src[J]) do
    begin
      NewSCArray[K] := FSceneChangeList[I];
      Inc(K);
      Inc(J);
      Inc(I);
    end;
  end;
  while (I < Length(FSceneChangeList)) do
  begin
    NewSCArray[K] := FSceneChangeList[I];
    Inc(K);
    Inc(I);
  end;
  while (J < Length(Src)) do
  begin
    NewSCArray[K] := Src[J];
    Inc(K);
    Inc(J);
  end;
  // Re-adjust space
  SetLength(NewSCArray, K);
  SetSceneChangeList(NewSCArray);
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.InsertSceneChange(const ATimeInMS: Integer);
var
  InsertSC: TIntegerDynArray;
begin
  SetLength(InsertSC, 1);
  InsertSC[0] := ATimeInMS;
  InsertSceneChange(InsertSC);
  SetLength(InsertSC, 0);
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.SetSceneChangeTimeMode(const ASMPTE: Boolean);
var
  i: Integer;
begin
  if Length(FSceneChangeList) > 0 then
  begin
    for i := 0 to High(FSceneChangeList) do
    begin
      if ASMPTE then
        FSceneChangeList[i] := Round(FSceneChangeList[i] / 1.001)
      else
        FSceneChangeList[i] := Round(FSceneChangeList[i] * 1.001);
    end;
    DoUpdate;
  end;
end;

//------------------------------------------------------------------------------

