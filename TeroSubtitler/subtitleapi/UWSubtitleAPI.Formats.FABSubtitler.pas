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

unit UWSubtitleAPI.Formats.FABSubtitler;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSystem.SysUtils,
  UWSubtitleAPI.Formats, StrUtils;

type

  { TUWFABSubtitler }

  TUWFABSubtitler = class(TUWSubtitleCustomFormat)
  public
    function Name: String; override;
    function Format: TUWSubtitleFormats; override;
    function Extension: String; override;
    function IsTimeBased: Boolean; override;
    function HasStyleSupport: Boolean; override;
    function IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean; override;
    function LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean; override;
    function SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean; override;
  end;

// -----------------------------------------------------------------------------

implementation

uses UWSubtitleAPI.Tags;

// -----------------------------------------------------------------------------

function TUWFABSubtitler.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWFABSubtitler.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfFABSubtitler;
end;

// -----------------------------------------------------------------------------

function TUWFABSubtitler.Extension: String;
begin
  Result := '*.txt';
end;

// -----------------------------------------------------------------------------

function TUWFABSubtitler.IsTimeBased: Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------

function TUWFABSubtitler.HasStyleSupport: Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------

function TUWFABSubtitler.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  if (TimeInFormat(Copy(SubtitleFile[Row], 1, 11), 'hh:mm:ss:zz')) and
     (TimeInFormat(Copy(SubtitleFile[Row], 14, 11), 'hh:mm:ss:zz')) and
     (Pos('  ', SubtitleFile[Row]) = 12) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWFABSubtitler.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean;
var
  i, c        : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
begin
  Result := False;
  try
    for i := 0 to SubtitleFile.Count-1 do
    begin
      InitialTime := StringToTime(Copy(SubtitleFile[i], 1, 8));
      FinalTime   := StringToTime(Copy(SubtitleFile[i], 14, 8));

      if IsInteger(Copy(SubtitleFile[i], 10, 2)) then
        InitialTime := InitialTime + FramesToTime(StrToInt(Copy(SubtitleFile[i], 10, 2)), FPS);
      if IsInteger(Copy(SubtitleFile[i], 23, 2)) then
        FinalTime := FinalTime + FramesToTime(StrToInt(Copy(SubtitleFile[i], 23, 2)), FPS);

      c    := 1;
      Text := '';
      while (i+c <= (SubtitleFile.Count-1)) and
            (StringToTime(Copy(SubtitleFile[i+c], 1, 8)) = -1) and
            (StringToTime(Copy(SubtitleFile[i+c], 14, 8)) = -1) do
      begin
        if Text <> '' then
          Text := Text + LineEnding + SubtitleFile[i+c]
        else
          Text := SubtitleFile[i+c];
        Inc(c);
      end;

      if (InitialTime > -1) and (FinalTime > -1) then
        Subtitles.Add(InitialTime, FinalTime, Text, '');
    end;
  finally
    Result := Subtitles.Count > 0;
  end;
end;

// -----------------------------------------------------------------------------

function TUWFABSubtitler.SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  InitialTime : String;
  FinalTime   : String;
  i           : Integer;
  Text        : String;
begin
  Result  := False;

  for i := FromItem to ToItem do
  begin
    Text := RemoveTSTags(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]));

    // Time format is hh:mm:ss:ff
    InitialTime := TimeToString(Subtitles[i].InitialTime, 'hh:mm:ss:') +
                     AddChar('0', IntToStr(GetMSecsInFrames(Subtitles[i].InitialTime, FPS)), 2);

    FinalTime := TimeToString(Subtitles[i].FinalTime, 'hh:mm:ss:') +
                   AddChar('0', IntToStr(GetMSecsInFrames(Subtitles[i].FinalTime, FPS)), 2);

    StringList.Add(InitialTime + '  ' + FinalTime);
    StringList.Add(Text, False);
    StringList.Add('', False);
  end;

  try
    StringList.SaveToFile(FileName, Encoding);
    Result := True;
  except
  end;
end;

// -----------------------------------------------------------------------------

end.
