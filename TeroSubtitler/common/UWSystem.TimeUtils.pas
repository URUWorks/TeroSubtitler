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

function EncodeTime(const Hour, Min, Secs, MSecs: Word): Integer;
procedure DecodeTime(const Time: Cardinal; out Hour, Min, Secs, MSecs: Word);
function TimeToFrames(const Time: Cardinal; const FPS: Single): Cardinal;
function TimeToFramesMaxFPS(const Time: Cardinal; const FPS: Single): Cardinal;
function FramesToTime(const Frames, FPS: Single): Cardinal;
function TimeToString(const Time: Cardinal; TimeFormat: String = 'hh:mm:ss'; const FPS: Single = 25; const ATrim: Boolean = False): String;
function StringToTime(const Time: String; const NoHours: Boolean = False; const FPS: Single = 0): Cardinal;
function TimeInFormat(const Time, Format: String): Boolean;
function TrimTimeString(Text: String): String;
function MSToHHMMSSFFTime(const Time: Integer; const FPS: Single; const FramesSeparator: Char = ':'): String;
function MSToHHMMSSFFMax(const Time: Integer; const FPS: Single; const FramesSeparator: Char = ':'): String;
function HHMMSSFFTimeToMS(const Time: String; const FPS: Single): Integer;
function RefTimeToMSecs(const RefTime: Int64): Cardinal;
function MSecsToRefTime(const MSecs: Cardinal): Int64;
function TicksToMSecs(const Ticks: Int64; const TicksPerSecond: Integer = 10000): Integer;
function GetHours(const Time: Cardinal): Integer;
function GetMinutes(const Time: Cardinal): Integer;
function GetSeconds(const Time: Cardinal): Integer;
function GetMSecs(const Time: Cardinal): Integer;
function GetMSecsInFrames(const Time: Cardinal; const FPS: Single): Integer;
function GetDateAndTime(Format: String = 'hh:mm:ss, dd/mm/yyyy'): String;
function TimeMSToShortString(const TimeMS: Cardinal; const Precision: Cardinal): String;
function TimeMSToShortStringFrames(const TimeMS: Cardinal; const Precision: Cardinal; const AFPS: Single = 25): String;
function CalculateOptimalDisplayMS(const Text: String): Cardinal;
function CalculateOptimalDisplayMSEx(const Text: String; const CPS: Double = 14.7; const MinDisplay: Cardinal = 1000; const MaxDisplay: Cardinal = 8000): Double;

// -----------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------

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

function TimeToFrames(const Time: Cardinal; const FPS: Single): Cardinal;
begin
  Result := Round((Time / 1000.0) * FPS);
end;

// -----------------------------------------------------------------------------

function TimeToFramesMaxFPS(const Time: Cardinal; const FPS: Single): Cardinal;
begin
  Result := Round(Time / (1000.0 / FPS));
  if Result >= FPS then Result := Trunc(FPS - 0.001);
end;

// -----------------------------------------------------------------------------

function FramesToTime(const Frames, FPS: Single): Cardinal;
begin
  if FPS > 0 then
    Result := Round((Frames / FPS) * 1000.0)
  else
    Result := 0;
end;

// -----------------------------------------------------------------------------

function TimeToString(const Time: Cardinal; TimeFormat: String = 'hh:mm:ss'; const FPS: Single = 25; const ATrim: Boolean = False): String;
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

function StringToTime(const Time: String; const NoHours: Boolean = False; const FPS: Single = 0): Cardinal;
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
  Result := False;

  if StringToTime(Time) > 0 then
  begin
    if (Pos(':', Time) = Pos(':', Format)) and
       (Pos('.', Time) = Pos('.', Format)) and
       (Pos(',', Time) = Pos(',', Format)) and
       (StringCount(':', Time) = StringCount(':', Format)) and
       (StringCount('.', Time) = StringCount('.', Format)) and
       (StringCount(',', Time) = StringCount(',', Format)) and
       (Length(Time) = Length(Format)) then
    Result := True;
  end;
end;

// -----------------------------------------------------------------------------

function TrimTimeString(Text: String): String;
begin
  while (Length(Text) > 1) and (Text[1] in [Char('0'), Char(':')]) do
    if (Text.CountChar('.') = 1) and (Text[3] <> Char('.')) or (Text.CountChar(':') > 1) then
      Delete(Text, 1, 1)
    else
      Break;

  Result := Text;
end;

// -----------------------------------------------------------------------------

function MSToHHMMSSFFTime(const Time: Integer; const FPS: Single; const FramesSeparator: Char = ':'): String;
begin
  Result := TimeToString(Time, 'hh' + FramesSeparator + 'mm' + FramesSeparator + 'ss');
  Result := Result + FramesSeparator + AddChar('0', IntToStr(TimeToFrames(Time - StringToTime(Result), FPS)), 2);
end;

// -----------------------------------------------------------------------------

function MSToHHMMSSFFMax(const Time: Integer; const FPS: Single; const FramesSeparator: Char = ':'): String;
begin
  Result := TimeToString(Time, 'hh' + FramesSeparator + 'mm' + FramesSeparator + 'ss');
  Result := Result + FramesSeparator + AddChar('0', IntToStr(TimeToFramesMaxFPS(Time - StringToTime(Result), FPS)), 2);
end;

// -----------------------------------------------------------------------------

function HHMMSSFFTimeToMS(const Time: String; const FPS: Single): Integer;
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

function GetMSecsInFrames(const Time: Cardinal; const FPS: Single): Integer;
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

function TimeMSToShortStringFrames(const TimeMS: Cardinal; const Precision: Cardinal; const AFPS: Single = 25): String;
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
