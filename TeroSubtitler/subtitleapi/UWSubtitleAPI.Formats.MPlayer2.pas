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

unit UWSubtitleAPI.Formats.MPlayer2;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, UWSubtitleAPI, UWSystem.StrUtils, UWSystem.SysUtils, UWSubtitleAPI.Formats;

type

  { TUWMPlayer2 }

  TUWMPlayer2 = class(TUWSubtitleCustomFormat)
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

uses UWSubtitleAPI.Tags;

// -----------------------------------------------------------------------------

function TUWMPlayer2.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWMPlayer2.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfMPlayer;
end;

// -----------------------------------------------------------------------------

function TUWMPlayer2.Extension: String;
begin
  Result := '*.mpl';
end;

// -----------------------------------------------------------------------------

function TUWMPlayer2.HasStyleSupport: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWMPlayer2.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  if (IsInteger(Copy(SubtitleFile[Row], 2, Pos(']', SubtitleFile[Row]) - 2))) and
     (IsInteger(Copy(SubtitleFile[Row], Pos('[', SubtitleFile[Row], 2) + 1, Pos(']', SubtitleFile[Row], Pos(']', SubtitleFile[Row]) + 1) - (Pos('[', SubtitleFile[Row], 2) + 1)))) and
     (StringCount('[', SubtitleFile[Row]) >= 2) and
     (StringCount('[', SubtitleFile[Row]) >= 2) and
     (Pos('[', SubtitleFile[Row]) = 1) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWMPlayer2.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean;
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
      if (Pos('[', SubtitleFile[i]) = 1) and
         (Pos(']', SubtitleFile[i]) > 1) and
         (StringCount('[',SubtitleFile[i]) >= 2) and
         (StringCount(']',SubtitleFile[i]) >= 2) and
         (IsInteger(Copy(SubtitleFile[i], 2, Pos(']', SubtitleFile[i]) - 2))) then
      begin
        InitialTime := StrToInt(Copy(SubtitleFile[i], 2, Pos(']', SubtitleFile[i]) - 2)) * 100;
        Text := Copy(SubtitleFile[i], Pos('[', SubtitleFile[i], 2) + 1, Pos(']', SubtitleFile[i], Pos(']', SubtitleFile[i]) + 1) - (Pos('[', SubtitleFile[i], 2) + 1));
        if IsInteger(Text) then
          FinalTime := StrToInt(Text) * 100
        else
          FinalTime := InitialTime + 2000;

        Text := ReplaceString(Copy(SubtitleFile[i], Pos(']', SubtitleFile[i], Pos(']', SubtitleFile[i]) + 1) + 1, Length(SubtitleFile[i])), '|', LineEnding);

      if (InitialTime > -1) and (FinalTime > -1) then
        Subtitles.Add(InitialTime, FinalTime, Text, '');
      end;
    end;
  finally
    Result := Subtitles.Count > 0;
  end;
end;

// -----------------------------------------------------------------------------

function TUWMPlayer2.SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  i    : Integer;
  Text : String;
begin
  Result  := False;

  for i := FromItem to ToItem do
  begin
    Text := RemoveTSTags(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]));
    StringList.Add('[' + IntToStr(Subtitles[i].InitialTime div 100) + '][' + IntToStr(Subtitles[i].FinalTime div 100) + ']' + ReplaceEnters(Text), False);
  end;

  try
    StringList.SaveToFile(FileName, Encoding);
    Result := True;
  except
  end;
end;

// -----------------------------------------------------------------------------

end.
