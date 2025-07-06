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

{ Peak }

//------------------------------------------------------------------------------

function TUWWaveformDisplayer.IsPeakDataLoaded: Boolean;
begin
  Result := FPeakDataLoaded;
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.ClearPeakData;
begin
  FPeakDataLoaded := False;
  FPeakTab        := NIL;
  FPeakTabSize    := 0;
  FSamplesPerPeak := 0;
  FillByte(FWaveFormat, SizeOf(FWaveFormat), 0);
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.CreatePeakTab(WAVFile: TUWWAVEFile);
var
  Buffer8   : TByteDynArray;
  Buffer16  : TSmallIntDynArray;
  Buffer32  : TSingleDynArray;
  i, j      : Integer;
  PeakMax, PeakMin       : SmallInt;
  PeakMax8, PeakMin8     : Byte;
  PeakMax32, PeakMin32   : Single;
  PeakMaxMax, PeakMinMin : SmallInt;
  MaxAbsoluteValue       : Integer;
  NormFactor             : Double;
  ReadSize               : Integer;
begin
  if Assigned(FOnPeakCreation) then FOnPeakCreation(Self, pcetStart, 0);

  FSamplesPerPeak := WAVFile.SamplesPerSecond div 100;
  FPeakTabSize := Ceil((WAVFile.SamplesCount / FSamplesPerPeak) / WAVFile.Channels);
  SetLength(FPeakTab, FPeakTabSize);

  PeakMaxMax := -32768;
  PeakMinMin := 32767;

  if WAVFile.BitsPerSample = 8 then
  begin
    SetLength(Buffer8, FSamplesPerPeak * WAVFile.Channels);
    for i := 0 to FPeakTabSize - 1 do
    begin
      ReadSize := WAVFile.Read(@Buffer8[0], Length(Buffer8));
      if ReadSize <= 0 then Break;

      PeakMax8 := 0;
      PeakMin8 := 255;
      for j := 0 to ReadSize - 1 do
      begin
        if Buffer8[j] > PeakMax8 then PeakMax8 := Buffer8[j];
        if Buffer8[j] < PeakMin8 then PeakMin8 := Buffer8[j];
      end;

      PeakMax := (PeakMax8 - 128) shl 8;
      PeakMin := (PeakMin8 - 128) shl 8;

      FPeakTab[i].Max := PeakMax;
      FPeakTab[i].Min := PeakMin;

      if PeakMax > PeakMaxMax then PeakMaxMax := PeakMax;
      if PeakMin < PeakMinMin then PeakMinMin := PeakMin;

      if Assigned(FOnPeakCreation) then
        FOnPeakCreation(Self, pcetProgress, (i * 100) div FPeakTabSize);
    end;
  end
  else if WAVFile.BitsPerSample = 16 then
  begin
    SetLength(Buffer16, FSamplesPerPeak * WAVFile.Channels);
    for i := 0 to FPeakTabSize - 1 do
    begin
      ReadSize := WAVFile.Read(@Buffer16[0], Length(Buffer16) * SizeOf(SmallInt));
      if ReadSize <= 0 then Break;

      PeakMax := -32768;
      PeakMin := 32767;
      for j := 0 to (ReadSize div SizeOf(SmallInt)) - 1 do
      begin
        if Buffer16[j] > PeakMax then PeakMax := Buffer16[j];
        if Buffer16[j] < PeakMin then PeakMin := Buffer16[j];
      end;

      FPeakTab[i].Max := PeakMax;
      FPeakTab[i].Min := PeakMin;

      if PeakMax > PeakMaxMax then PeakMaxMax := PeakMax;
      if PeakMin < PeakMinMin then PeakMinMin := PeakMin;

      if Assigned(FOnPeakCreation) then
        FOnPeakCreation(Self, pcetProgress, (i * 100) div FPeakTabSize);
    end;
  end
  else if WAVFile.BitsPerSample = 32 then
  begin
    SetLength(Buffer32, FSamplesPerPeak * WAVFile.Channels);
    for i := 0 to FPeakTabSize - 1 do
    begin
      ReadSize := WAVFile.Read(@Buffer32[0], Length(Buffer32) * SizeOf(Single));
      if ReadSize <= 0 then Break;

      PeakMax32 := -1.0;
      PeakMin32 := 1.0;
      for j := 0 to (ReadSize div SizeOf(Single)) - 1 do
      begin
        if Buffer32[j] > PeakMax32 then PeakMax32 := Buffer32[j];
        if Buffer32[j] < PeakMin32 then PeakMin32 := Buffer32[j];
      end;

      PeakMax := Single2SmallInt(PeakMax32);
      PeakMin := Single2SmallInt(PeakMin32);

      FPeakTab[i].Max := PeakMax;
      FPeakTab[i].Min := PeakMin;

      if PeakMax > PeakMaxMax then PeakMaxMax := PeakMax;
      if PeakMin < PeakMinMin then PeakMinMin := PeakMin;

      if Assigned(FOnPeakCreation) then
        FOnPeakCreation(Self, pcetProgress, (i * 100) div FPeakTabSize);
    end;
  end;

  MaxAbsoluteValue := Max(Abs(PeakMaxMax), Abs(PeakMinMin));
  if MaxAbsoluteValue > 0 then
    NormFactor := 32768.0 / MaxAbsoluteValue
  else
    NormFactor := 1.0;

  NormalizePeakTab(NormFactor);

  Buffer8 := nil;
  Buffer16 := nil;
  Buffer32 := nil;

  if Assigned(FOnPeakCreation) then FOnPeakCreation(Self, pcetStop, 0);
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplayer.NormalizePeakTab(NormFactor: Double): Boolean;
var
  i, MaxAbsoluteValue, Value: Integer;
begin
  Result := False;

  if NormFactor = -1 then
  begin
    MaxAbsoluteValue := 0;

    // First pass, calculate normalization factor
    for i := 0 to FPeakTabSize - 1 do
    begin
      Value := Abs(FPeakTab[i].Max);
      if Value > MaxAbsoluteValue then
        MaxAbsoluteValue := Value;

      Value := Abs(FPeakTab[i].Min);
      if Value > MaxAbsoluteValue then
        MaxAbsoluteValue := Value;
    end;

    if MaxAbsoluteValue = 0 then
      Exit(False);

    NormFactor := 32768.0 / MaxAbsoluteValue;
  end;

  // Normalize if the factor differs significantly from 1
  if Abs(NormFactor - 1.0) > 0.01 then
  begin
    for i := 0 to FPeakTabSize - 1 do
    begin
      FPeakTab[i].Max := Round(FPeakTab[i].Max * NormFactor);
      FPeakTab[i].Min := Round(FPeakTab[i].Min * NormFactor);
    end;

    Result := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.GenerateDummyPeakTab(const LengthMS: Cardinal);
begin
  FPageSizeMS     := 5000;
  FSamplesPerPeak := 1;
  FPeakTabSize    := 1;
  FPeakTab        := NIL;
  SetLength(FPeakTab, FPeakTabSize);
  FillByte(FPeakTab[0], 0, 0);
  SetLengthMS(LengthMS);

  UpdateView([uvfPageSize]);
end;

//------------------------------------------------------------------------------

