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
  SysUtils, StrUtils, Math, UWSystem.StrUtils, UWSystem.SysUtils;

// -----------------------------------------------------------------------------

type
  TRoundMode = (rmFloor, rmCeil, rmNearest);
  TTimecodeMode = (tcNDF, tcDF);

  TFPSInfo = record
    FPS  : Double;
    Mode : TTimecodeMode;
  end;

function SMPTEConversion(const ATimeMS: Integer; const AValue: Boolean = True; const Mode: TRoundMode = rmNearest): Integer;
function NormalizeFPS(const FPS: Double): Double;
function EncodeTime(const Hour, Min, Secs, MSecs: Word): Integer;
procedure DecodeTime(const Time: Cardinal; out Hour, Min, Secs, MSecs: Word);
function TimeToFrames(const Time: Cardinal; const FPS: Double): Cardinal;
function TimeToFramesMaxFPS(const Time: Cardinal; const FPS: Double): Cardinal;
function FramesToTime(const Frames: Cardinal; FPS: Double): Cardinal;
function RoundTimeWithFrames(const ATimeMS: Integer; const AFPS: Double; const AAddFrame: Integer = 0): Integer;
function IncDecFrame(const ATimeMS: Integer; const AFPS: Double; const AStep: Byte = 1; const IsDropFrame: Boolean = False; const AIncrement: Boolean = True): Integer;
function TimeToString(const Time: Cardinal; TimeFormat: String = 'hh:mm:ss'; const FPS: Double = 25; const ATrim: Boolean = False): String;
function StringToTime(const Time: String; const NoHours: Boolean = False; const FPS: Double = 0): Cardinal;
function TimeInFormat(const Time, Format: String): Boolean;
function TrimTimeString(Text: String): String;
function MSToHHMMSSFFTime(const TimeMS: Integer; const FPS: Double; const Mode: TTimecodeMode = tcNDF): String;
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
function SMPTEStringToMS(const ASMPTE: String; const AFPS: Double; const ADropFrame: Boolean = False): Integer;
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

function IncDecFrame(const ATimeMS: Integer; const AFPS: Double; const AStep: Byte = 1; const IsDropFrame: Boolean = False; const AIncrement: Boolean = True): Integer;
var
  FrameDuration: Double;
  AbsFrame, DropFrames, Minutes, TenMinutes: Int64;
  NominalFPS: Integer;
begin
  if AFPS <= 0 then Exit(ATimeMS);

  NominalFPS := Round(AFPS); // 30 o 60
  FrameDuration := 1000.0 / AFPS;

  // 1. Frame físico actual (lineal)
  AbsFrame := Round(ATimeMS / FrameDuration);

  // 2. Si es Drop Frame, ajustamos la cuenta de frames como lo hace un reloj SMPTE
  if IsDropFrame and ((NominalFPS = 30) or (NominalFPS = 60)) then
  begin
    DropFrames := Round(NominalFPS * 0.066666); // 2 para 29.97, 4 para 59.94

    // Calculamos cuántos números se han "saltado" hasta este punto
    Minutes := AbsFrame div Round(AFPS * 60);
    TenMinutes := Minutes div 10;

    // Ajustamos el contador para que coincida con la etiqueta del TC
    AbsFrame := AbsFrame + (DropFrames * Minutes) - (DropFrames * TenMinutes);
  end;

  // 3. Aplicamos el incremento/decremento sobre la "etiqueta"
  if AIncrement then
    Inc(AbsFrame, AStep)
  else
    AbsFrame := Max(0, AbsFrame - AStep);

  // 4. Si era DF, debemos revertir el ajuste para volver al tiempo real
  if IsDropFrame and ((NominalFPS = 30) or (NominalFPS = 60)) then
  begin
    DropFrames := Round(NominalFPS * 0.066666);
    TenMinutes := AbsFrame div (Round(AFPS * 60 * 10) - (DropFrames * 9));
    Minutes := (AbsFrame + (DropFrames * 9 * TenMinutes)) div Round(AFPS * 60);

    AbsFrame := AbsFrame - (DropFrames * (Minutes - TenMinutes));
  end;

  // 5. milisegundos
  Result := Round(AbsFrame * FrameDuration);
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

function MSToHHMMSSFFTime(const TimeMS: Integer; const FPS: Double; const Mode: TTimecodeMode = tcNDF): String;
var
  totalFrames, D, M: Integer;
  fpsNom, dropFrames, framesPer10Min, framesPerMin: Integer;
  HH, MM, SS, FF: Integer;
  sep: String;
  isNegative, isNTSC: Boolean;
begin
  fpsNom := Round(FPS);

  // NTSC
  isNTSC := (Mode = tcDF) and (Abs(FPS * 1.001 - fpsNom) < 0.02);

  // ms a frames
  isNegative := TimeMS < 0;
  totalFrames := Trunc(Abs(TimeMS) * FPS / 1000 + 0.5);

  // SMPTE drop-frame
  if isNTSC then
  begin
    dropFrames := fpsNom div 15; // 2 para 30, 4 para 60
    framesPerMin := (fpsNom * 60) - dropFrames;
    framesPer10Min := (framesPerMin * 10) + dropFrames;

    D := totalFrames div framesPer10Min;
    M := totalFrames mod framesPer10Min;

    if M >= dropFrames then
      totalFrames := totalFrames
        + (dropFrames * 9 * D)
        + (dropFrames * ((M - dropFrames) div framesPerMin));

    sep := ';';
  end
  else
    sep := ':';

  FF := totalFrames mod fpsNom;
  totalFrames := totalFrames div fpsNom;

  SS := totalFrames mod 60;
  totalFrames := totalFrames div 60;

  MM := totalFrames mod 60;
  HH := totalFrames div 60;

  Result := Format('%0.2d:%0.2d:%0.2d%s%0.2d', [HH, MM, SS, sep, FF]);
  if isNegative then Result := '-' + Result;
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

{function SMPTEStringToMS(const ASMPTE: String; const AFPS: Double; const ADropFrame: Boolean = False): Integer;
var
  DropFrame : Boolean;
  ArraySMPTE : TStringArray;
  DropFrames : Integer;
  TotalFrames : Integer;
  h, m, s, f : Integer;
  TotalMinutes : Integer;
begin
  Result := 0;
  DropFrame := ADropFrame;

  ArraySMPTE := ASMPTE.Split(';');
  if Length(ArraySMPTE) >= 2 then
  begin
    DropFrame := True;
    f := ArraySMPTE[1].ToInteger;
    ArraySMPTE := ASMPTE.Split(':');
    h := ArraySMPTE[0].ToInteger;
    m := ArraySMPTE[1].ToInteger;
    s := Copy(ArraySMPTE[2], 1, Pos(';', ArraySMPTE[2])).ToInteger - 1;
  end
  else
  begin
    ArraySMPTE := ASMPTE.Split(':');
    h := ArraySMPTE[0].ToInteger;
    m := ArraySMPTE[1].ToInteger;
    s := ArraySMPTE[2].ToInteger;
    f := ArraySMPTE[3].ToInteger;
  end;

  // Drop frames is the 6% of the framerate rounded to the nearest number.
  // I get the 0.06666 * framerate to calculate the drop frames from here https://www.davidheidelberger.com/2010/06/10/drop-frame-timecode/
  if DropFrame then
    DropFrames := Round(AFPS * 0.0666666)
  else
    DropFrames := 0;

  if m > 0 then
    TotalMinutes := (60 * h) div m
  else
    TotalMinutes := 0;

  TotalFrames := (((h * 3600) + (m * 60) + s) * Round(AFPS)) + f - (DropFrames * (TotalMinutes - (Trunc(TotalMinutes / 10))));
  Result := Round((TotalFrames / AFPS ) * 1000);
end;}

function SMPTEStringToMS(const ASMPTE: String; const AFPS: Double; const ADropFrame: Boolean = False): Integer;
const
  EPS = 1e-6;
var
  s: String;
  p: TStringArray;
  h, m, sec, f: Integer;
  df: Boolean;
  fpsNom, dropsPerMin: Integer;
  totalMin: Int64;
  framesTC: Int64;
  is1000_1001, validDFNom: Boolean;
begin
  df := (Pos(';', ASMPTE) > 0) or ADropFrame;

  s := StringReplace(ASMPTE, ';', ':', [rfReplaceAll]);
  p := s.Split(':');
  if Length(p) <> 4 then Exit(0);

  h := StrToIntDef(p[0], 0);
  m := StrToIntDef(p[1], 0);
  sec := StrToIntDef(p[2], 0);
  f := StrToIntDef(p[3], 0);

  fpsNom := Round(AFPS);
  is1000_1001 := Abs(AFPS - (fpsNom * 1000.0 / 1001.0)) <= EPS;
  validDFNom := (fpsNom mod 30) = 0; // 30, 60, 120...

  if df and is1000_1001 and validDFNom then
    dropsPerMin := (fpsNom div 30) * 2 // 30->2, 60->4, 120->8
  else
  begin
    dropsPerMin := 0;
    df := False; // no DF para 24/23.976
  end;

  if (m >= 60) or (sec >= 60) or (f < 0) or (f >= fpsNom) then Exit(0);

  totalMin := Int64(h) * 60 + m;
  framesTC := ((Int64(h) * 3600 + Int64(m) * 60 + sec) * fpsNom) + f
    - Int64(dropsPerMin) * (totalMin - totalMin div 10) * Ord(df);

  Result := Round(framesTC * 1000.0 / AFPS);
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
