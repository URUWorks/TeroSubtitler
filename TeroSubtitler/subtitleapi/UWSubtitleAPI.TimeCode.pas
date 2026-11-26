{*
 *  URUWorks Subtitle API
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
 *  Copyright (C) 2001-2026 URUWorks, uruworks@gmail.com.
 *}

unit UWSubtitleAPI.TimeCode;

// -----------------------------------------------------------------------------

{$modeswitch advancedrecords}
{$modeswitch typehelpers}

interface

uses
  SysUtils;

type

  { UWTimeCode }

  TUWTimeCode       = Integer; // MS
  TUWTimeCodeHelper = type helper for TUWTimeCode
    procedure EncodeTime(const Hour, Min, Secs, MSecs: Word); // to MS
    procedure DecodeTime(out Hour, Min, Secs, MSecs: Word);   // from MS
    function ToFrames(const FPS: Single): Cardinal;
    function ToSeconds: Cardinal;
  end;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

procedure TUWTimeCodeHelper.EncodeTime(const Hour, Min, Secs, MSecs: Word);
begin
  Self := (Hour * (MinsPerHour * SecsPerMin   * MSecsPerSec)) +
          (Min  * SecsPerMin   * MSecsPerSec) +
          (Secs * MSecsPerSec) +
           MSecs;
end;

//------------------------------------------------------------------------------

procedure TUWTimeCodeHelper.DecodeTime(out Hour, Min, Secs, MSecs: Word);
var
  x: Integer;
begin
  // before, Hour and Min (out) were used before they were set
  x     := Self;
  if x < 0 then x := 0;
  Hour  := x div 3600000;
  x     := x mod 3600000;
  Min   := x div 60000;
  x     := x mod 60000;
  Secs  := x div 1000;
  MSecs := x mod 1000;
end;

// -----------------------------------------------------------------------------

function TUWTimeCodeHelper.ToFrames(const FPS: Single): Cardinal;
begin
  Result := 0;

  if (Self > 0) and (FPS > 0) then
    Result := Round((Self / 1000) * FPS);
end;

//------------------------------------------------------------------------------

function TUWTimeCodeHelper.ToSeconds: Cardinal;
begin
  Result := 0;

  if (Result > 0) then
    Result := Trunc(Self / 1000);
end;

//------------------------------------------------------------------------------

end.
