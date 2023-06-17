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

unit UWSubtitleAPI.Formats.AdobeEncoreDVD;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSystem.StrUtils,
  UWSystem.SysUtils, UWSubtitleAPI.Formats;

type

  { TUWAdobeEncoreDVD }

  TUWAdobeEncoreDVD = class(TUWSubtitleCustomFormat)
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

const
  ATimeFormat = 'hh:mm:ss:ff';

// -----------------------------------------------------------------------------

function TUWAdobeEncoreDVD.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWAdobeEncoreDVD.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfAdobeEncoreDVD;
end;

// -----------------------------------------------------------------------------

function TUWAdobeEncoreDVD.Extension: String;
begin
  Result := '*.txt';
end;

// -----------------------------------------------------------------------------

function TUWAdobeEncoreDVD.IsTimeBased: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWAdobeEncoreDVD.HasStyleSupport: Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------

function TUWAdobeEncoreDVD.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  if ((StringCount(':', Copy(SubtitleFile[Row], 1, 11)) = 3) and
     (StrToIntDef(Copy(SubtitleFile[Row], 10, 2), 0) < 30) and
     (TimeInFormat(Copy(SubtitleFile[Row], 1, 11), ATimeFormat))) and
     ((StringCount(':', Copy(SubtitleFile[Row], 13, 11)) = 3) and
     (StrToIntDef(Copy(SubtitleFile[Row], 22, 2), 0) < 30) and
     (TimeInFormat(Copy(SubtitleFile[Row], 13, 11), ATimeFormat))) and
     (Copy(SubtitleFile[Row], 12, 1) = ' ') and
     (not IsInteger(Copy(SubtitleFile[Row], 25, 2))) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWAdobeEncoreDVD.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean;
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
      if (TimeInFormat(Copy(SubtitleFile[i], 1, 11), ATimeFormat)) and
         (TimeInFormat(Copy(SubtitleFile[i], 13, 11), ATimeFormat)) then
      begin
        InitialTime := HHMMSSFFTimeToMS(Copy(SubtitleFile[i], 1, 11), FPS);
        FinalTime   := HHMMSSFFTimeToMS(Copy(SubtitleFile[i], 13, 11), FPS);
        Text        := Copy(SubtitleFile[i], 25, Length(SubtitleFile[i])-24);

        Inc(i);
        while (i < SubtitleFile.Count) and
          (not TimeInFormat(Copy(SubtitleFile[i], 1, 11), ATimeFormat)) and
          (not TimeInFormat(Copy(SubtitleFile[i], 13, 11), ATimeFormat)) do
        begin
          if Text <> '' then
            Text := Text + sLineBreak + SubtitleFile[i]
          else
            Text := SubtitleFile[i];

          Inc(i);
        end;
        Dec(i);

        if (InitialTime >= 0) and (FinalTime > 0) and not Text.IsEmpty then
          Subtitles.Add(InitialTime, FinalTime, Text, '', NIL, False);
      end;
      Inc(i);
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

// -----------------------------------------------------------------------------

function TUWAdobeEncoreDVD.SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  i: Integer;
begin
  Result  := False;

  for i := FromItem to ToItem do
  begin
    StringList.Add(MSToHHMMSSFFTime(Subtitles.InitialTime[i], FPS) + ' ' +
      MSToHHMMSSFFTime(Subtitles.FinalTime[i], FPS) + ' ' +
      RemoveTSTags(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i])));
  end;

  try
    StringList.SaveToFile(FileName, Encoding);
    Result := True;
  except
  end;
end;

// -----------------------------------------------------------------------------

end.
