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

{ SilentZone }

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.ClearZoneList(var AZoneList: TZonesList);
var
  i: Integer;
begin
  if Length(AZoneList) > 0 then
  begin
    for i := 0 to Length(AZoneList)-1 do
    begin
      Dispose(AZoneList[i]);
      AZoneList[i] := NIL;
    end;
    SetLength(AZoneList, 0);
  end;
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplayer.DetectZoneList(var AZoneList: TZonesList; const AFindSilent: Boolean = True; const Threshold: Integer = 100; const WinSizeMS: Integer = 100): Boolean;
var
  WinPeaks, i, j : Integer;
  SampleLen, PeakLen, Sum, Rms : Double;
  Start, Stop : Int64;
  ZoneRange : PZoneRange;
begin
  Result := False;
  ClearZoneList(AZoneList);
  ZoneRange := NIL;
  SampleLen := (1.0 / FWaveFormat.nSamplesPerSec);
  PeakLen := SampleLen * FSamplesPerPeak;
  WinPeaks := Ceil((WinSizeMS / 1000.0) / PeakLen);
  i := 0;
  j := 0;
  Sum := 0;
  while (i < FPeakTabSize) do
  begin
    Sum := Sum + (FPeakTab[i].Max * FPeakTab[i].Max);
    if (j = (WinPeaks-1)) then
    begin
      Rms := Sqrt(Sum / j);
      if (AFindSilent and (Rms < Threshold)) or (not AFindSilent and (Rms > Threshold)) then
      begin
        Start := Round((i - j) * PeakLen * 1000.0);
        Stop  := Round(i * PeakLen * 1000.0);

        if Assigned(ZoneRange) then
        begin
          // Simply increase previous zone
          ZoneRange^.Stop   := Stop;
          ZoneRange^.RmsSum := ZoneRange^.RmsSum + Rms;
          Inc(ZoneRange^.RmsCount);
        end
        else
        begin
          // Add new zone
          New(ZoneRange);
          ZoneRange^.Start    := Start;
          ZoneRange^.Stop     := Stop;
          ZoneRange^.RmsSum   := Rms;
          ZoneRange^.RmsCount := 1;
          SetLength(AZoneList, Length(AZoneList)+1);
          AZoneList[Length(AZoneList)-1] := ZoneRange;
        end;
      end
      else
        ZoneRange := NIL;

      j := 0;
      Sum := 0;
    end;
    Inc(j);
    Inc(i);
  end;
  Result := Length(AZoneList) > 0;
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplayer.DetectSilentZone(const Threshold: Integer = 100; const WinSizeMS: Integer = 100): Boolean;
begin
  Result := DetectZoneList(FSilentZones, True, Threshold, WinSizeMS);
end;

//------------------------------------------------------------------------------

