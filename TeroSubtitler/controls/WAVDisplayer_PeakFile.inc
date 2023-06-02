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
  Buffer8                : TByteDynArray;
  Buffer16               : TSmallIntDynArray;
  Buffer32               : TSingleDynArray;
  i, j                   : Integer;
  PeakMax, PeakMin       : SmallInt;
  PeakMax8, PeakMin8     : Byte;
  PeakMax32, PeakMin32   : Single;
  PeakMaxMax, PeakMinMin : SmallInt;
  MaxAbsoluteValue       : Integer;
  NormFactor             : Double;
begin
  if Assigned(FOnPeakCreation) then FOnPeakCreation(Self, pcetStart, 0);

  // Get 1 peak value every ~10ms
  FSamplesPerPeak := WAVFile.SamplesPerSecond div 100;
  FPeakTabSize    := Ceil((WAVFile.SamplesCount / FSamplesPerPeak) / WAVFile.Channels);
  // Allocate the big peak tab
  FPeakTab := NIL;
  SetLength(FPeakTab, FPeakTabSize);

// WAV Data format  Maximum value    Minimum value	    Midpoint value
//     8-bit PCM	  255 (0xFF)       0	                128 (0x80)
//     16-bit PCM	  32,767 (0x7FFF)  - 32,768 (0x8000)	0

  PeakMaxMax := -32768;
  PeakMinMin := 32767;

  if (WAVFile.BitsPerSample = 8) then
  begin
    // Allocate the small buffer
    SetLength(Buffer8, FSamplesPerPeak * WAVFile.Channels);
    for i := 0 to FPeakTabSize-1 do
    begin
      FillByte(Buffer8[0], FSamplesPerPeak * SizeOf(Shortint) * WAVFile.Channels, 0);
      WAVFile.Read(@Buffer8[0], FSamplesPerPeak * SizeOf(Shortint) * WAVFile.Channels);
      PeakMax8 := 0;
      PeakMin8 := 255;
      for j := 0 to (FSamplesPerPeak * WAVFile.Channels)-1 do
      begin
        if Buffer8[j] > PeakMax8 then
          PeakMax8 := Buffer8[j];
        if Buffer8[j] < PeakMin8 then
          PeakMin8 := Buffer8[j];
      end;
      // Convert 8 bits to 16 bits
      PeakMax := ((PeakMax8 - 128) shl 8);
      PeakMin := ((PeakMin8 - 128) shl 8);

      FPeakTab[i].Max := PeakMax;
      FPeakTab[i].Min := PeakMin;

      if (PeakMax > PeakMaxMax) then
        PeakMaxMax := PeakMax;
      if (PeakMin < PeakMinMin) then
        PeakMinMin := PeakMin;

      if Assigned(FOnPeakCreation) then
        FOnPeakCreation(Self, pcetProgress, (i*100) div Integer(FPeakTabSize));
    end;
  end
  else if (WAVFile.BitsPerSample = 16) then
  begin
    // Allocate the small buffer
    SetLength(Buffer16, FSamplesPerPeak * WAVFile.Channels);
    for i := 0 to FPeakTabSize-1 do
    begin
      FillByte(Buffer16[0], FSamplesPerPeak * SizeOf(SmallInt) * WAVFile.Channels, 0);
      WAVFile.Read(@Buffer16[0], FSamplesPerPeak * SizeOf(SmallInt) * WAVFile.Channels);
      PeakMax := -32768;
      PeakMin := 32767;
      for j := 0 to (FSamplesPerPeak * WAVFile.Channels)-1 do
      begin
        if Buffer16[j] > PeakMax then
          PeakMax := Buffer16[j];
        if Buffer16[j] < PeakMin then
          PeakMin := Buffer16[j];
      end;
      FPeakTab[i].Max := PeakMax;
      FPeakTab[i].Min := PeakMin;

      if PeakMax > PeakMaxMax then
        PeakMaxMax := PeakMax;
      if PeakMin < PeakMinMin then
        PeakMinMin := PeakMin;

      if Assigned(FOnPeakCreation) then
        FOnPeakCreation(Self, pcetProgress, (i*100) div Integer(FPeakTabSize));
    end;
  end
  else if (WAVFile.BitsPerSample = 32) then
  begin
    // Allocate the small buffer
    SetLength(Buffer32, FSamplesPerPeak * WAVFile.Channels);
    for i := 0 to FPeakTabSize-1 do
    begin
      FillByte(Buffer32[0], FSamplesPerPeak * SizeOf(SmallInt) * WAVFile.Channels, 0);
      WAVFile.Read(@Buffer32[0], FSamplesPerPeak * SizeOf(Single) * WAVFile.Channels);
      PeakMax32 := -1.0;
      PeakMin32 := 1.0;
      for j:=0 to (FSamplesPerPeak * WAVFile.Channels)-1 do
      begin
        if Buffer32[j] > PeakMax32 then
          PeakMax32 := Buffer32[j];
        if Buffer32[j] < PeakMin32 then
          PeakMin32 := Buffer32[j];
      end;
      // Convert 32 bits float to 16 bits integer
      PeakMax := Single2SmallInt(PeakMax32);
      PeakMin := Single2SmallInt(PeakMin32);

      FPeakTab[i].Max := PeakMax;
      FPeakTab[i].Min := PeakMin;

      if PeakMax > PeakMaxMax then
        PeakMaxMax := PeakMax;
      if PeakMin < PeakMinMin then
        PeakMinMin := PeakMin;

      if Assigned(FOnPeakCreation) then
        FOnPeakCreation(Self, pcetProgress, (i*100) div Integer(FPeakTabSize));
    end;
  end;
  // Calc. normalize factor
  MaxAbsoluteValue := Max(Abs(PeakMaxMax), Abs(PeakMinMin));
  NormFactor := 32768.0 / MaxAbsoluteValue;
  // Normalize peak tab
  NormalizePeakTab(NormFactor);
  Buffer8  := NIL;
  Buffer16 := NIL;
  if Assigned(FOnPeakCreation) then FOnPeakCreation(Self, pcetStop, 0);
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplayer.NormalizePeakTab(NormFactor: Double): Boolean;
var
  i, MaxAbsoluteValue, Value : Integer;
begin
  Result := False;

  if (NormFactor = -1) then
  begin
    MaxAbsoluteValue := 0;
    // First pass, calculate normalization factor
    for i := 0 to FPeakTabSize-1 do
    begin
      Value := FPeakTab[i].Max;
      if (Value > MaxAbsoluteValue) then
        MaxAbsoluteValue := Value;

      Value := -FPeakTab[i].Min;
      if (Value > MaxAbsoluteValue) then
        MaxAbsoluteValue := Value;
    end;
    NormFactor := 32768.0 / MaxAbsoluteValue;
  end;

  if (NormFactor > 1.1) then
  begin
    // Apply normalization factor
    for i := 0 to FPeakTabSize-1 do
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

