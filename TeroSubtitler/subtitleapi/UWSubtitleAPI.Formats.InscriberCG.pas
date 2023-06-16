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

unit UWSubtitleAPI.Formats.InscriberCG;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSubtitleAPI.Formats;

type

  { TUWInscriberCG }

  TUWInscriberCG = class(TUWSubtitleCustomFormat)
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

uses UWSubtitleAPI.Tags, UWSystem.StrUtils,
  UWSystem.SysUtils;

// -----------------------------------------------------------------------------

function TUWInscriberCG.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWInscriberCG.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfInscriberCG;
end;

// -----------------------------------------------------------------------------

function TUWInscriberCG.Extension: String;
begin
  Result := '*.txt';
end;

// -----------------------------------------------------------------------------

function TUWInscriberCG.HasStyleSupport: Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------

function TUWInscriberCG.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  if (Pos('@@9', SubtitleFile[Row]) = 1) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWInscriberCG.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean;
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
      if TimeInFormat(Copy(SubtitleFile[i], Length(SubtitleFile[i])-24, 11), 'hh:mm:ss:zz') and
         TimeInFormat(Copy(SubtitleFile[i], Length(SubtitleFile[i])-11, 11), 'hh:mm:ss:zz') then
      begin
        InitialTime := StringToTime(Copy(SubtitleFile[i], Length(SubtitleFile[i])-24, 11));
        FinalTime   := StringToTime(Copy(SubtitleFile[i], Length(SubtitleFile[i])-11, 11));
        Text        := Copy(SubtitleFile[i], 5, Length(SubtitleFile[i])-30);

        c := 1;
        while not TimeInFormat(Copy(SubtitleFile[i-c], Length(SubtitleFile[i-c])-24, 11), 'hh:mm:ss:zz') and
          (i-c > 0) do
        begin
          if SubtitleFile[i-c] = '@@9' then Break;
          Text := Copy(SubtitleFile[i-c], 5, Length(SubtitleFile[i-c])) + LineEnding + Text;
          Inc(c);
        end;

        if (InitialTime >= 0) and (FinalTime > 0) and not Text.IsEmpty then
          Subtitles.Add(InitialTime, FinalTime, Text, '');
      end;
    end;
  finally
    Result := Subtitles.Count > 0;
  end;
end;

// -----------------------------------------------------------------------------

function TUWInscriberCG.SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  i : Integer;
begin
  Result  := False;

  StringList.Add('@@9 Title', False);
  StringList.Add('@@9 Created by URUWorks Tero Subtitler (uruworks.net)', False);
  StringList.Add('@@9 STORY:', False);
  StringList.Add('@@9 LANG: ENG', False);
  for i := 1 to 4 do StringList.Add('@@9', False);

  for i := FromItem to ToItem do
  begin
    Subtitles.Text[i] := RemoveTSTags(Subtitles.Text[i]);

    StringList.Add('@@9 ' + ReplaceEnters(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]), LineEnding, LineEnding + '@@9 ') +
                '<' + TimeToString(Subtitles[i].InitialTime, 'hh:mm:ss:zz') + '><'  +
                TimeToString(Subtitles[i].FinalTime, 'hh:mm:ss:zz') + '>');

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
