{*
 *  URUWorks Lazarus TimeUtils
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
 *  Copyright (C) 2001-2023 URUWorks, uruworks@gmail.com.
 *}

unit UWSystem.TimeUtils;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, StrUtils, UWSystem.StrUtils, UWSystem.SysUtils;

// -----------------------------------------------------------------------------

type
  TRoundMode = (rmFloor, rmCeil, rmNearest);

function SMPTEConversion(const ATimeMS: Integer; const AValue: Boolean = True; const Mode: TRoundMode = rmNearest): Integer;
function NormalizeFPS(const FPS: Double): Double;
function EncodeTime(const Hour, Min, Secs, MSecs: Word): Integer;
procedure DecodeTime(const Time: Cardinal; out Hour, Min, Secs, MSecs: Word);
function TimeToFrames(const Time: Cardinal; const FPS: Double): Cardinal;
function TimeToFramesMaxFPS(const Time: Cardinal; const FPS: Double): Cardinal;
function FramesToTime(const Frames: Cardinal; FPS: Double): Cardinal;
function RoundTimeWithFrames(const ATimeMS: Integer; const AFPS: Double; const AAddFrame: Integer = 0): Integer;
function IncDecFrame(const ATimeMS: Integer; const AFPS: Double; const AStep: Byte = 1; const ASMPTE: Boolean = False; const AIncrement: Boolean = True): Integer;
function TimeToString(const Time: Cardinal; TimeFormat: String = 'hh:mm:ss'; const FPS: Double = 25; const ATrim: Boolean = False): String;
function StringToTime(const Time: String; const NoHours: Boolean = False; const FPS: Double = 0): Cardinal;
function TimeInFormat(const Time, Format: String): Boolean;
function TrimTimeString(Text: String): String;
function MSToHHMMSSFFTime(const Time: Integer; const FPS: Double; const FramesSeparator: Char = ':'): String;
function MSToHHMMSSFFMax(const Time: Integer; const FPS: Double; const FramesSeparator: Char = ':'): String;
function HHMMSSFFTimeToMS(const Time: String; const FPS: Double): Integer;
function RefTimeToMSecs(const RefTime: Int64): Cardinal;
function MSecsToRefTime(const MSecs: Cardinal): Int64;
function TicksToMSecs(const Ticks: Int64; const TicksPerSecond: Integer = 10000): Integer;
function StrSecsToMSecs(const ASeconds: String): Integer; // 25.1
function GetHours(const Time: Cardinal): Integer;
function GetMinutes(const Time: Cardinal): Integer;
function GetSeconds(const Time: Cardinal): Integer;
function GetMSecs(const Time: Cardinal): Integer;
function GetMSecsInFrames(const Time: Cardinal; const FPS: Double): Integer;
function GetDateAndTime(Format: String = 'hh:mm:ss, dd/mm/yyyy'): String;
function TimeMSToShortString(const TimeMS: Cardinal; const Precision: Cardinal): String;
function TimeMSToShortStringFrames(const TimeMS: Cardinal; const Precision: Cardinal; const AFPS: Double = 25): String;
function CalculateOptimalDisplayMS(const Text: String): Cardinal;
function CalculateOptimalDisplayMSEx(const Text: String; const CPS: Double = 14.7; const MinDisplay: Cardinal = 1000; const MaxDisplay: Cardinal = 8000): Double;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

// -------------------------------------------------------------
// Helpers MulDiv
// -------------------------------------------------------------

function MulDivFloorI64(const A, B, C: Int64): Int64; inline;
begin
  // Floor((A*B)/C)
  Result := (A * B) div C;
end;

// -----------------------------------------------------------------------------

function MulDivCeilI64(const A, B, C: Int64): Int64; inline;
begin
  // Ceil((A*B)/C)
  Result := (A * B + C - 1) div C;
end;

// -----------------------------------------------------------------------------

function MulDivRoundI64(const A, B, C: Int64): Int64; inline;
begin
  // Round((A*B)/C)
  Result := (A * B + C div 2) div C;
end;

// -----------------------------------------------------------------------------

function SMPTEConversion(const ATimeMS: Integer; const AValue: Boolean = True; const Mode: TRoundMode = rmNearest): Integer;
begin
  case Mode of
    rmFloor : begin
                if AValue then
                  Result := MulDivFloorI64(ATimeMS, 1001, 1000)
                else
                  Result := MulDivFloorI64(ATimeMS, 1000, 1001);
              end;
    rmCeil  : begin
                if AValue then
                  Result := MulDivCeilI64(ATimeMS, 1001, 1000)
                else
                  Result := MulDivCeilI64(ATimeMS, 1000, 1001);
              end;
  else // rmNearest
    if AValue then
      Result := MulDivRoundI64(ATimeMS, 1001, 1000) // *1.001
    else
      Result := MulDivRoundI64(ATimeMS, 1000, 1001); // /1.001
  end;
end;

// -----------------------------------------------------------------------------

procedure FPSToRational(const FPS: Double; out N, D: Int64);
const
  TOL = 1e-3; // 0.001
begin
  if Abs(FPS - 24000/1001)  < TOL then begin N := 24000;  D := 1001 end else // 23.976
  if Abs(FPS - 30000/1001)  < TOL then begin N := 30000;  D := 1001 end else // 29.970
  if Abs(FPS - 60000/1001)  < TOL then begin N := 60000;  D := 1001 end else // 59.94
  if Abs(FPS - 120000/1001) < TOL then begin N := 120000; D := 1001 end else
  if Abs(FPS - 240000/1001) < TOL then begin N := 240000; D := 1001 end else
  begin
    N := Round(FPS);
    D := 1;
  end;
end;

// -----------------------------------------------------------------------------

function TimeToFramesI64(const TimeMS: Int64; const N, D: Int64; const Mode: TRoundMode): Int64; inline;
var
  Den: Int64;
begin
  Den := 1000 * D; // (TimeMS * N) / (1000*D)
  case Mode of
    rmFloor : Result := MulDivFloorI64(TimeMS, N, Den);
    rmCeil  : Result := MulDivCeilI64 (TimeMS, N, Den);
  else // rmNearest
    Result := MulDivRoundI64(TimeMS, N, Den);
  end;
end;

// -----------------------------------------------------------------------------

function FramesToTimeI64(const Frames: Int64; const N, D: Int64; const Mode: TRoundMode): Int64; inline;
var
  Num: Int64;
begin
  Num := 1000 * D; // (frames * 1000*D) / N
  case Mode of
    rmFloor : Result := MulDivFloorI64(Frames, Num, N); // inicio del frame
    rmCeil  : Result := MulDivCeilI64 (Frames, Num, N); // fin del frame
  else // rmNearest
    Result := MulDivRoundI64(Frames, Num, N);
  end;
end;

// -----------------------------------------------------------------------------

function NormalizeFPS(const FPS: Double): Double;
const
  TOL = 0.02; // tolerancia
begin
  Result := FPS; // por defecto sin cambios

  // NTSC -> nominal
  if Abs(FPS - 23.976) <= TOL then Exit(24.0);
  if Abs(FPS - 29.97)  <= TOL then Exit(30.0);
  if Abs(FPS - 59.94)  <= TOL then Exit(60.0);
  if Abs(FPS - 119.88) <= TOL then Exit(120.0);
  if Abs(FPS - 239.76) <= TOL then Exit(240.0);

  // Si ya es entero dentro de tolerancia
  if Abs(FPS - Round(FPS)) <= TOL then
    Exit(Round(FPS));
end;

// -----------------------------------------------------------------------------

function EncodeTime(const Hour, Min, Secs, MSecs: Word): Integer;
begin
  Result := (Hour * (MinsPerHour * SecsPerMin   * MSecsPerSec)) +
            (Min  * SecsPerMin   * MSecsPerSec) +
            (Secs * MSecsPerSec) +
             MSecs;
end;

// -----------------------------------------------------------------------------

procedure DecodeTime(const Time: Cardinal; out Hour, Min, Secs, MSecs: Word);
var
  h, m, x: Integer;
begin
  Hour  := Time div 3600000;
  h     := Time - (Hour*3600000);

  Min   := h div 60000;
  m     := Min * 60000;
  x     := h - m;

  Secs  := x div MSecsPerSec;
  MSecs := x - (Secs*MSecsPerSec);
end;

// -----------------------------------------------------------------------------

function TimeToFrames(const Time: Cardinal; const FPS: Double): Cardinal;
begin
  Result := Round((Time * FPS) / 1000.0);
end;

// -----------------------------------------------------------------------------

function TimeToFramesMaxFPS(const Time: Cardinal; const FPS: Double): Cardinal;
begin
  Result := Round((Time * FPS) / 1000.0);
  if Result >= Trunc(FPS) then
    Result := Trunc(FPS - 0.001);
end;

// -----------------------------------------------------------------------------

function FramesToTime(const Frames: Cardinal; FPS: Double): Cardinal;
begin
  if FPS > 0 then
    Result := Round((Frames * 1000.0) / FPS)
  else
    Result := 0;
end;

{function FramesToTime(const Frames: Cardinal; FPS: Double): Cardinal;
begin
  if Abs(FPS - 23.976) < 0.001 then
    Result := Round((Frames * 1001.0) / 24.0)
  else if Abs(FPS - 29.97) < 0.001 then
    Result := Round((Frames * 1001.0) / 30.0)
  else if Abs(FPS - 59.94) < 0.001 then
    Result := Round((Frames * 1001.0) / 60.0)
  else
  begin
    if FPS > 0 then
      Result := Round((Frames * 1000.0) / FPS)
    else
      Result := 0;
  end;
end;}

// -----------------------------------------------------------------------------

function RoundTimeWithFrames(const ATimeMS: Integer; const AFPS: Double; const AAddFrame: Integer = 0): Integer;
//var
//  tmp: Double;
begin
  Result := FramesToTime(TimeToFrames(ATimeMS, AFPS) + AAddFrame, AFPS);
//  tmp := (ATimeMS / 1000.0 * AFPS) + AAddFrame;
//  tmp := tmp / AFPS * 1000.0;
//  Result := Round(tmp);
end;

// -----------------------------------------------------------------------------

function IncDecFrame(const ATimeMS: Integer; const AFPS: Double; const AStep: Byte = 1; const ASMPTE: Boolean = False; const AIncrement: Boolean = True): Integer;
var
  tmp, frames, scaleFactor: Double;
begin
  if ASMPTE then
    scaleFactor := 1001.0 / 1000.0
  else
    scaleFactor := 1.0;

  tmp := ATimeMS * scaleFactor;
  frames := tmp / 1000.0 * AFPS;

  if AIncrement then
    frames := frames + AStep
  else
  begin
    if frames >= AStep then
      frames := frames - AStep
    else
      frames := 0;
  end;

  tmp := frames / AFPS * 1000.0;
  Result := Round(tmp / scaleFactor);
end;
// -----------------------------------------------------------------------------

function TimeToString(const Time: Cardinal; TimeFormat: String = 'hh:mm:ss'; const FPS: Double = 25; const ATrim: Boolean = False): String;
var
  Hour, Min, Sec, MSec : Word;
  Count, tmp           : Cardinal;
begin
  DecodeTime(Time, Hour, Min, Sec, MSec);

  if TimeFormat = 'hh:mm:ss' then
    Result := Format('%.2d:%.2d:%.2d', [Hour, Min, Sec])
  else
  begin
    Count := StringCount('h', TimeFormat);
    if Count > 0 then
      TimeFormat := ReplaceString(TimeFormat, StringOfChar('h', Count), AddChar('0', IntToStr(Hour), Count))
    else
      Min := Min + Hour * 60;

    Count := StringCount('m', TimeFormat);
    if Count > 0 then
      TimeFormat := ReplaceString(TimeFormat, StringOfChar('m', Count), AddChar('0', IntToStr(Min), Count))
    else
      Sec := Sec + Min * 60;

    Count := StringCount('s', TimeFormat);
    if Count > 0 then
      TimeFormat := ReplaceString(TimeFormat, StringOfChar('s', Count), AddChar('0', IntToStr(Sec), Count));

    Count := StringCount('z', TimeFormat);
    if Count > 0 then
    begin
      tmp := Pos('z', TimeFormat);
      TimeFormat := Copy(ReplaceString(TimeFormat, StringOfChar('z', Count), Copy(AddChar('0', IntToStr(MSec), 3), 0, Count)), 0, tmp + Count-1);
    end;

    Count := StringCount('f', TimeFormat);
    if Count > 0 then
    begin
      tmp := TimeToFrames(MSec, FPS);
      Constrain(tmp, 0, Round(FPS)-1);
      TimeFormat := ReplaceString(TimeFormat, StringOfChar('f', Count), AddChar('0', IntToStr(tmp), Count));
    end;

    if ATrim then
      TimeFormat := TrimTimeString(TimeFormat);

    Result := TimeFormat;
  end;
end;

// -----------------------------------------------------------------------------

function StringToTime(const Time: String; const NoHours: Boolean = False; const FPS: Double = 0): Cardinal;
var
  H, M, S, Z, i                : Integer;
  PCount, PFirst, PSec, PThird : Integer;
begin
  Result := 0;
  if (Time = '') then Exit;

  H      := 0;
  M      := 0;
  S      := 0;
  Z      := 0;
  PCount := 0;
  PFirst := 0;
  PSec   := 0;
  PThird := 0;

  for i := 1 to Length(Time) do
  begin
    if not CharInSet(Time[i], ['0'..'9']) then
      if CharInSet(Time[i], [':', ',', '.', ';']) then
      begin
        if (i = 1) or (i = Length(Time)) then Exit;

        case PCount of
          0 : PFirst := i;
          1 : PSec   := i;
          2 : PThird := i;
        end;

        Inc(PCount);
      end
      else
        Exit;
  end;

  try
    if PFirst > 0 then
    begin
      if NoHours then
      begin
        M := StringToInt(Copy(Time, 0, PFirst - 1));
        if PSec = 0 then
          S := StringToInt(Copy(Time, PFirst + 1, Length(Time)-PFirst));
      end
      else
      begin
        H := StringToInt(Copy(Time, 0, PFirst - 1));
        if PSec = 0 then
          M := StringToInt(Copy(Time, PFirst + 1, Length(Time)-PFirst));
      end;

      if PSec > 0 then
      begin
        if NoHours then
        begin
          S := StringToInt(Copy(Time, PFirst + 1, PSec - PFirst - 1));
          Z := StringToInt(AddCharR('0', Copy(Time, PSec + 1, Length(Time)), 3));
        end
        else
          M := StringToInt(Copy(Time, PFirst + 1, PSec - PFirst - 1));

        if PThird > 0 then
        begin
          S := StringToInt(Copy(Time, PSec + 1, PThird - PSec - 1));

          if FPS = 0 then
            Z := StringToInt(AddCharR('0', Copy(Time, PThird + 1, Length(Time)), 3))
          else
            Z := FramesToTime( StringToInt(Copy(Time, PThird + 1, Length(Time))), FPS);
        end
        else
          if not NoHours then
            S := StringToInt(Copy(Time, PSec + 1, Length(Time)));
      end;

      Result := ((H*3600)*1000) + ((M*60)*1000) + (S*1000) + Z;
    end;
  except
    Result := 0;
  end;
end;

// -----------------------------------------------------------------------------

function TimeInFormat(const Time, Format: String): Boolean;
begin
  Result := (Pos(':', Time) = Pos(':', Format)) and
    (Pos('.', Time) = Pos('.', Format)) and
    (Pos(',', Time) = Pos(',', Format)) and
    (StringCount(':', Time) = StringCount(':', Format)) and
    (StringCount('.', Time) = StringCount('.', Format)) and
    (StringCount(',', Time) = StringCount(',', Format)) and
    (Length(Time) = Length(Format));
end;

// -----------------------------------------------------------------------------

function TrimTimeString(Text: String): String;
begin
  while (Length(Text) > 1) and (Text[1] in [Char('0'), Char(':')]) do
    if (Text.CountChar(FormatSettings.DecimalSeparator) = 1) and (Text[3] <> Char(FormatSettings.DecimalSeparator)) or (Text.CountChar(':') > 1) then
      Delete(Text, 1, 1)
    else
      Break;

  Result := Text;
end;

// -----------------------------------------------------------------------------

function MSToHHMMSSFFTime(const Time: Integer; const FPS: Double; const FramesSeparator: Char = ':'): String;
begin
  Result := TimeToString(Time, 'hh' + FramesSeparator + 'mm' + FramesSeparator + 'ss');
  Result := Result + FramesSeparator + AddChar('0', IntToStr(TimeToFrames(Time - StringToTime(Result), FPS)), 2);
end;

// -----------------------------------------------------------------------------

function MSToHHMMSSFFMax(const Time: Integer; const FPS: Double; const FramesSeparator: Char = ':'): String;
begin
  Result := TimeToString(Time, 'hh' + FramesSeparator + 'mm' + FramesSeparator + 'ss');
  Result := Result + FramesSeparator + AddChar('0', IntToStr(TimeToFramesMaxFPS(Time - StringToTime(Result), FPS)), 2);
end;

// -----------------------------------------------------------------------------

function HHMMSSFFTimeToMS(const Time: String; const FPS: Double): Integer;
begin
  if StringToTime(Time) = 0 then
    Result := 0
  else
    Result := StringToTime(Copy(Time, 1, 8)) + Integer(FramesToTime(StrToIntDef(Copy(Time, 10, 2), 0), FPS));
end;

// -----------------------------------------------------------------------------

function RefTimeToMSecs(const RefTime: Int64): Cardinal;
begin
  Result := Cardinal(RefTime div 10000);
end;

// -----------------------------------------------------------------------------

function MSecsToRefTime(const MSecs: Cardinal): Int64;
begin
  Result := Int64(MSecs * 10000);
end;

// -----------------------------------------------------------------------------

function TicksToMSecs(const Ticks: Int64; const TicksPerSecond: Integer = 10000): Integer;
begin
  Result := Integer(Ticks div TicksPerSecond);
end;

// -----------------------------------------------------------------------------

function StrSecsToMSecs(const ASeconds: String): Integer; // 25.1
var
  FormatSettings : TFormatSettings;
begin
  FormatSettings := DefaultFormatSettings;
  FormatSettings.DecimalSeparator  := '.';
  FormatSettings.ThousandSeparator := FormatSettings.DecimalSeparator;
  Result := Trunc( StrToFloatDef(ASeconds, 0, FormatSettings) * 1000 );
end;

// -----------------------------------------------------------------------------

function GetHours(const Time: Cardinal): Integer;
var
  HH, MM, SS, MS: Word;
begin
  DecodeTime(Time, HH, MM, SS, MS);
  Result := HH;
end;

// -----------------------------------------------------------------------------

function GetMinutes(const Time: Cardinal): Integer;
var
  HH, MM, SS, MS: Word;
begin
  DecodeTime(Time, HH, MM, SS, MS);
  Result := MM;
end;

// -----------------------------------------------------------------------------

function GetSeconds(const Time: Cardinal): Integer;
var
  HH, MM, SS, MS: Word;
begin
  DecodeTime(Time, HH, MM, SS, MS);
  Result := SS;
end;

// -----------------------------------------------------------------------------

function GetMSecs(const Time: Cardinal): Integer;
var
  HH, MM, SS, MS: Word;
begin
  DecodeTime(Time, HH, MM, SS, MS);
  Result := MS;
end;

// -----------------------------------------------------------------------------

function GetMSecsInFrames(const Time: Cardinal; const FPS: Double): Integer;
var
  HH, MM, SS, MS: Word;
begin
  DecodeTime(Time, HH, MM, SS, MS);
  Result := TimeToFrames(MS, FPS);
end;

// -----------------------------------------------------------------------------

function GetDateAndTime(Format: String = 'hh:mm:ss, dd/mm/yyyy'): String;
begin
  DateTimeToString(Result, Format, Now);
end;

// -----------------------------------------------------------------------------

function TimeMSToShortString(const TimeMS: Cardinal; const Precision: Cardinal): String;
var
  hour, min, sec, ms: Cardinal;
begin
  Result := '';

  ms   := TimeMs div 1000;
  hour := ms div 3600;
  min  := (ms div 60) - (hour * 60);
  sec  := ms mod 60;
  ms   := (TimeMS - (hour * 3600 * 1000) - (min * 60 * 1000) - (sec * 1000)) div Precision;

  if (hour > 0) then
  begin
    if (ms > 0) then
      Result := Format('%d:%.2d:%.2d.%d', [hour, min, sec, ms])
    else
      Result := Format('%d:%.2d:%.2d', [hour, min, sec])
  end
  else if (min > 0) then
  begin
    if (ms > 0) then
      Result := Result + Format('%d:%.2d.%d', [min, sec, ms])
    else
      Result := Result + Format('%d:%.2d', [min, sec]);
  end
  else
    Result := Result +  Format('%d.%d', [sec, ms]);
end;

// -----------------------------------------------------------------------------

function TimeMSToShortStringFrames(const TimeMS: Cardinal; const Precision: Cardinal; const AFPS: Double = 25): String;
var
  hour, min, sec, ms: Cardinal;
begin
  Result := '';

  ms   := TimeMs div 1000;
  hour := ms div 3600;
  min  := (ms div 60) - (hour * 60);
  sec  := ms mod 60;

  ms   := TimeToFrames((TimeMS - (hour * 3600 * 1000) - (min * 60 * 1000) - (sec * 1000)) div Precision, AFPS);

  if (hour > 0) then
    Result := Format('%d:%.2d:%.2d:%.2d', [hour, min, sec, ms])
  else if (min > 0) then
    Result := Result + Format('%d:%.2d:%.2d', [min, sec, ms])
  else
    Result := Result +  Format('%d:%.2d', [sec, ms]);
end;

// -----------------------------------------------------------------------------

function CalculateOptimalDisplayMS(const Text: String): Cardinal;
begin
  Result := Round(CalculateOptimalDisplayMSEx(Text));
end;

// -----------------------------------------------------------------------------

function CalculateOptimalDisplayMSEx(const Text: String; const CPS: Double = 14.7; const MinDisplay: Cardinal = 1000; const MaxDisplay: Cardinal = 8000): Double;
var
  dCPS: Double;
begin
  dCPS := CPS;
  if (dCPS < 2) or (dCPS > 100) then dCPS := 14.7;

  Result := (Length(Text) / dCPS) * 1000.0;

  if Result < MinDisplay then Result := MinDisplay;
  if Result > MaxDisplay then Result := MaxDisplay;
end;

// -----------------------------------------------------------------------------

end.
