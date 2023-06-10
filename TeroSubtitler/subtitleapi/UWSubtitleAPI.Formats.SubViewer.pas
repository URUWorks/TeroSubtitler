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

unit UWSubtitleAPI.Formats.SubViewer;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSubtitleAPI.Formats;

type

  { TUWSubViewer }

  TUWSubViewer = class(TUWSubtitleCustomFormat)
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

uses UWSubtitleAPI.Tags, UWSystem.SysUtils;

// -----------------------------------------------------------------------------

function TUWSubViewer.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWSubViewer.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfSubViewer;
end;

// -----------------------------------------------------------------------------

function TUWSubViewer.Extension: String;
begin
  Result := '*.sbv';
end;

// -----------------------------------------------------------------------------

function TUWSubViewer.HasStyleSupport: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWSubViewer.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  if (TimeInFormat(Copy(SubtitleFile[Row], 1, 11), 'h:mm:ss.zzz')) and
     (TimeInFormat(Copy(SubtitleFile[Row], 13, 11), 'h:mm:ss.zzz')) and
     (Pos(',', SubtitleFile[Row]) = 12) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWSubViewer.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
begin
  Result := False;
  try
    i := 0;
    while i < SubtitleFile.Count do
    begin
      if (TimeInFormat(Copy(SubtitleFile[i], 1, 11), 'h:mm:ss.zzz')) and
        (Pos(',', SubtitleFile[i]) = 12) then
      begin
        InitialTime := StringToTime(Copy(SubtitleFile[i], 1, 11));
        FinalTime   := StringToTime(Copy(SubtitleFile[i], 13, 11));

        if (InitialTime > -1) and (FinalTime > -1) then
        begin
          Inc(i);
          Text := '';
          while (i < SubtitleFile.Count) and
            (not TimeInFormat(Copy(SubtitleFile[i], 1, 11), 'h:mm:ss.zzz')) do
          begin
            if Text <> '' then
              Text := Text + LineEnding + SubtitleFile[i]
            else
              Text := SubtitleFile[i];

            Inc(i);
          end;
          Dec(i);

          Text := HTMLTagsToTS(Text);
          Subtitles.Add(InitialTime, FinalTime, Text, '', NIL, False);
        end;
      end;
      Inc(i);
    end;
  finally
    Result := Subtitles.Count > 0;
  end;
end;

// -----------------------------------------------------------------------------

function TUWSubViewer.SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  i    : Integer;
  Text : String;
begin
  Result  := False;

  for i := FromItem to ToItem do
  begin
    StringList.Add(TimeToString(Subtitles.InitialTime[i], 'h:mm:ss.zzz') + ',' + TimeToString(Subtitles.FinalTime[i], 'h:mm:ss.zzz'), False);
    Text := TSTagsToHTML(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]));
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
