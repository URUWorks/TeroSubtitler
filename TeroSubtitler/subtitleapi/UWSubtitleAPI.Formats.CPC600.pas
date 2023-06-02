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
 *  Copyright (C) 2001-2023 URUWorks, uruworks@gmail.com.
 *}

unit UWSubtitleAPI.Formats.CPC600;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSubtitleAPI.Formats;

type

  { TUWCPC600 }

  TUWCPC600 = class(TUWSubtitleCustomFormat)
  public
    function Name: String; override;
    function Format: TUWSubtitleFormats; override;
    function Extension: String; override;
    function HasStyleSupport: Boolean; override;
    function IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean; override;
    function LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean; override;
    function SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean; override;
  end;

// -----------------------------------------------------------------------------

implementation

uses UWSubtitleAPI.Tags, UWSystem.StrUtils, UWSystem.SysUtils;

// -----------------------------------------------------------------------------

function TUWCPC600.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWCPC600.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfCPC600;
end;

// -----------------------------------------------------------------------------

function TUWCPC600.Extension: String;
begin
  Result := '*.txt';
end;

// -----------------------------------------------------------------------------

function TUWCPC600.HasStyleSupport: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWCPC600.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  if (TimeInFormat(Copy(SubtitleFile[Row], 1, 11), 'hh:mm:ss:zz')) and
     (TimeInFormat(Copy(SubtitleFile[Row+1], 1, 11), 'hh:mm:ss:zz')) and
     (Pos('_0NEN_', SubtitleFile[Row]) = 12) and
     (Pos('_0NEN_', SubtitleFile[Row+1]) = 12) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWCPC600.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
begin
  Result := False;
  try
    for i := 0 to SubtitleFile.Count-2 do
    begin
      if (TimeInFormat(Copy(SubtitleFile[i], 1, 11), 'hh:mm:ss:zz')) and
         (TimeInFormat(Copy(SubtitleFile[i+1], 1, 11), 'hh:mm:ss:zz')) and
         (Pos('_0NEN_', SubtitleFile[i]) = 12) and
         (Pos('_0NEN_', SubtitleFile[i+1]) = 12) then
      begin
        InitialTime := StringToTime(Copy(SubtitleFile[i], 1, 11));
        FinalTime   := StringToTime(Copy(SubtitleFile[i+1], 1, 11));
        Text        := ReplaceString(Copy(SubtitleFile[i], 18, Length(SubtitleFile[i])), '\', LineEnding);

        Subtitles.Add(InitialTime, FinalTime, Text, '');
      end;
    end;
  finally
    Result := Subtitles.Count > 0;
  end;
end;

// -----------------------------------------------------------------------------

function TUWCPC600.SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  i    : Integer;
  Text : String;
begin
  Result  := False;

  StringList.Add('~CPCC6.38~;UpperLower;PopOn;01;', False);
  for i := FromItem to ToItem do
  begin
    Text := RemoveTSTags(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]));
    StringList.Add(TimeToString(Subtitles[i].InitialTime, 'hh:mm:ss:zz') + '_0NEN_' + ReplaceString(Text, LineEnding, '\'), False);
    StringList.Add(TimeToString(Subtitles[i].FinalTime, 'hh:mm:ss:zz') + '_0NEN_');
  end;

  try
    StringList.SaveToFile(FileName, Encoding);
    Result := True;
  except
  end;
end;

// -----------------------------------------------------------------------------

end.
