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

unit UWSubtitleAPI.Formats.Sofni;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSubtitleAPI.Formats;

type

  { TUWSofni }

  TUWSofni = class(TUWSubtitleCustomFormat)
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

function TUWSofni.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWSofni.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfSofni;
end;

// -----------------------------------------------------------------------------

function TUWSofni.Extension: String;
begin
  Result := '*.sub';
end;

// -----------------------------------------------------------------------------

function TUWSofni.HasStyleSupport: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWSofni.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  if (Pos('\', SubtitleFile[Row]) = 12) or SubtitleFile[Row].StartsWith('*PART 1*', True) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWSofni.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean;
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
      if SubtitleFile[i].StartsWith('*PART 1*', True) then Inc(i, 2)
      else if SubtitleFile[i].StartsWith('*END*', True) then Break;

      if Pos('\', SubtitleFile[i]) <> 12 then
      begin
        Text := '';
        while (i < SubtitleFile.Count) and (Pos('\', SubtitleFile[i]) <> 12) do
        begin
          if Text <> '' then
            Text := Text + LineEnding + SubtitleFile[i]
          else
            Text := SubtitleFile[i];

          Inc(i);
        end;
        InitialTime := StringToTime(Copy(SubtitleFile[i], 1, 11), False, FPS);
        FinalTime   := StringToTime(Copy(SubtitleFile[i], 13, 11), False, FPS);

        Text := HTMLTagsToTS(Text);
        if (InitialTime >= 0) and (FinalTime > 0) then
          Subtitles.Add(InitialTime, FinalTime, Text, '', NIL);
      end;

      Inc(i);
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

// -----------------------------------------------------------------------------

function TUWSofni.SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  i    : Integer;
  Text : String;
begin
  Result  := False;

  StringList.Add('*PART 1*');
  StringList.Add('00:00:00.00\00:00:00.00');
  for i := FromItem to ToItem do
  begin
    Text := TSTagsToHTML(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]));
    StringList.Add(Text);
    StringList.Add(TimeToString(Subtitles.InitialTime[i], 'hh:mm:ss.ff', FPS) + '\' + TimeToString(Subtitles.FinalTime[i], 'hh:mm:ss.ff', FPS));
  end;
  StringList.Add('*END*');
  StringList.Add('...........\...........');
  StringList.Add('*CODE*');
  StringList.Add('0000000000000000');
  StringList.Add('*CAST*');
  StringList.Add('*GENERATOR*');
  StringList.Add('*FONTS*');
  StringList.Add('*READ*');
  StringList.Add('0.300 15.000 130.000 100.000 25.000');
  StringList.Add('*TIMING*');
  StringList.Add('1 30 1 1 1');
  StringList.Add('*TIMED BACKUP NAME*');
  StringList.Add('C:\');
  StringList.Add('*FORMAT SAMPLE ÅåÉéÌìÕõÛûÿ*');
  StringList.Add('*READ ADVANCED*');
  StringList.Add('< > 1 1 0.300');
  StringList.Add('*MARKERS*');

  try
    StringList.SaveToFile(FileName, Encoding);
    Result := True;
  except
  end;
end;

// -----------------------------------------------------------------------------

end.
