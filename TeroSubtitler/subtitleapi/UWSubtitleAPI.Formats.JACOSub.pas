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

unit UWSubtitleAPI.Formats.JACOSub;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSubtitleAPI.Formats;

type

  { TUWJACOSub }

  TUWJACOSub = class(TUWSubtitleCustomFormat)
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

function TUWJACOSub.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWJACOSub.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfJACOSub;
end;

// -----------------------------------------------------------------------------

function TUWJACOSub.Extension: String;
begin
  Result := '*.js;*.jss';
end;

// -----------------------------------------------------------------------------

function TUWJACOSub.HasStyleSupport: Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------

function TUWJACOSub.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  if (TimeInFormat(Copy(SubtitleFile[Row], 1, 10), 'h:mm:ss.zz')) and
     (TimeInFormat(Copy(SubtitleFile[Row], 12, 10), 'h:mm:ss.zz')) and
     (Pos(',', SubtitleFile[Row]) <> 12) and
     (Pos(',', SubtitleFile[Row]) <> 11) and
     (Pos(#9, SubtitleFile[Row]) = 0) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWJACOSub.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
begin
  Result := False;
  try
    for i := 0 to SubtitleFile.Count-1 do
    begin
      InitialTime := StringToTime(Copy(SubtitleFile[i], 1, 10));
      FinalTime   := StringToTime(Copy(SubtitleFile[i], 12, 10));
      Text        := ReplaceString(Trim(Copy(SubtitleFile[i], Pos('}', SubtitleFile[i]) + 2, Length(SubtitleFile[i]))), '\n', LineEnding);

      if (InitialTime > -1) and (FinalTime > -1) then
        Subtitles.Add(InitialTime, FinalTime, Text, '');
    end;
  finally
    Result := Subtitles.Count > 0;
  end;
end;

// -----------------------------------------------------------------------------

function TUWJACOSub.SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  i    : Integer;
  Text : String;
begin
  Result  := False;

  StringList.Add('#T100', False);
  StringList.Add('', False);
  StringList.Add('# Directive entries', False);
  StringList.Add('#D', False);
  for i := 1 to 29 do StringList.Add('#D'+IntToStr(i), False);
  StringList.Add('', False);

  for i := FromItem to ToItem do
  begin
    Text := RemoveTSTags(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]));
    StringList.Add(TimeToString(Subtitles[i].InitialTime, 'h:mm:ss.zz') + ' ' + TimeToString(Subtitles[i].FinalTime, 'h:mm:ss.zz') + ' {NTP} ' + ReplaceEnters(Text,'\n'), False);
  end;

  try
    StringList.SaveToFile(FileName, Encoding);
    Result := True;
  except
  end;
end;

// -----------------------------------------------------------------------------

end.
