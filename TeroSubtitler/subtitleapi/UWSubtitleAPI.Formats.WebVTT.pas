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

unit UWSubtitleAPI.Formats.WebVTT;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSubtitleAPI.Formats;

type

  { TUWWebVTT }

  TUWWebVTT = class(TUWSubtitleCustomFormat)
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

uses UWSubtitleAPI.ExtraInfo, UWSubtitleAPI.Tags, UWSystem.StrUtils,
  UWSystem.SysUtils, UWSubtitleAPI.Utils;

{
This format allows for time notation of hours, minutes, seconds and milliseconds,
respectively use like this:
00:00:00.000 where the 3 zeros at the end are the milliseconds.

Settings have to placed right after the timing, on the same line, separated with one (or more) space or tabulation.

Vertical text
D:vertical (vertical growing left)
D:vertical-lr (vertical growing right)
Line position
A specific position relative to the video frame:
L:[a number]%, where [a number] is a positive integer.
A line number:
L:[a number], where [a number] is a positive or negative integer.
Text position	T:[a number]%, where [a number] is a positive integer.
Text size	S:[a number]%, where [a number] is a positive integer.
Text alignment	A:start or A:middle or A:end

Cue setting example:
WEBVTT FILE

01:23:45.678 --> 01:23:46.789 D:vertical
Hello world!

01:23:48.910 --> 01:23:49.101 S:50%
Hello
world!

Replacements

& has to be replaced with &amp;
< has to be replaced with &lt;
> has to be replaced with &gt;
Voice declaration tags

<v.Name>
Example:
01:23:45.678 --> 01:23:46.789
- <v.John>Hey!</v>
- <v.Jane>Hey!</v>
}

// -----------------------------------------------------------------------------

function TUWWebVTT.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWWebVTT.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfWebVTT;
end;

// -----------------------------------------------------------------------------

function TUWWebVTT.Extension: String;
begin
  Result := '*.vtt;*.webvtt';
end;

// -----------------------------------------------------------------------------

function TUWWebVTT.IsTimeBased: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWWebVTT.HasStyleSupport: Boolean;
begin
  Result := True; // Has tags and coords
end;

// -----------------------------------------------------------------------------

function TUWWebVTT.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
//  if (SubtitleFile[Row] = 'WEBVTT') or
//     ((TimeInFormat(Copy(SubtitleFile[Row], 1, 12), 'hh:mm:ss.zzz')) and
//     (TimeInFormat(Copy(SubtitleFile[Row], 18, 12), 'hh:mm:ss.zzz')) and
//     (Pos(' --> ', SubtitleFile[Row]) > 0)) then
  if (SubtitleFile[0] = 'WEBVTT') then
     Result := True
   else
     Result := False;
end;

// -----------------------------------------------------------------------------

function TUWWebVTT.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean;
var
  i, c, x, v  : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text, s     : String;
  ExtraInfo   : PWebVTT_ExtraInfo;
  ExtraTime   : Integer;
begin
  Result := False;
  try
    ExtraTime := 0;
    with Subtitles.FormatProperties^.WebVTT do
      if UseXTIMESTAMP and (SubtitleFile[1].StartsWith('X-TIMESTAMP-MAP=MPEGTS')) then
      begin
        x := Pos('MPEGTS:', SubtitleFile[1])+7;
        MPEGTS := StrToIntDef(Copy(SubtitleFile[1], x, Pos(',', SubtitleFile[1])-x), 0);
        LOCAL := StringToTime(Copy(SubtitleFile[1], Pos('LOCAL:', SubtitleFile[1])+6, 12));
        ExtraTime := LOCAL;
      end;

    i := 0;
    while i < SubtitleFile.Count do
    begin
      if Pos(' --> ', SubtitleFile[i]) > 0 then
      begin
        s := ReplaceString(SubtitleFile[i], ' ', '');

        InitialTime := StringToTime(Copy(s, 1, 12))  + ExtraTime;
        FinalTime   := StringToTime(Copy(s, 16, 12)) + ExtraTime;

        c := Pos('align', SubtitleFile[i]);
        if (c > 0) then
        begin
          Delete(s, 1, c+2);
          New(ExtraInfo);
          if Copy(s, 1, 4) = 'left' then
          begin
            ExtraInfo^.TextAlign := 'left';
            c := 1;
          end
          else if Copy(s, 1, 6) = 'center' then
          begin
            ExtraInfo^.TextAlign := 'center';
            c := 2;
          end
          else if Copy(s, 1, 5) = 'right' then
          begin
            ExtraInfo^.TextAlign := 'right';
            c := 3;
          end
          else
          begin
            ExtraInfo^.TextAlign := '';
            c := 0;
          end;
        end
        else
        begin
          c := 0;
          ExtraInfo := NIL;
        end;

        v := Pos('line', s);
        if (v > 0) then
        begin
          Delete(s, 1, v+4);
          if s.EndsWith('%') then
            Delete(s, s.Length, 1);

          if not Assigned(ExtraInfo) then
            New(ExtraInfo);

          ExtraInfo^.LinePos := StrToIntDef(s, 0);
          if (ExtraInfo^.LinePos > 0) and (ExtraInfo^.LinePos < 40) then
            v := 2
          else if (ExtraInfo^.LinePos > 40) and (ExtraInfo^.LinePos < 70) then
            v := 1
          else
            v := 0;
        end
        else
          v := 0;

        if (InitialTime >= 0) and (FinalTime > 0) then
        begin
          Inc(i);
          Text := '';
          //while (i < SubtitleFile.Count) and (Pos(' --> ', SubtitleFile[i]) = 0) and (SubtitleFile[i] <> '') do
          while (i < SubtitleFile.Count) and (Pos(' --> ', SubtitleFile[i]) = 0) and
            not IsInteger(SubtitleFile[i]) do
          begin
            if Text <> '' then
              Text := Text + sLineBreak + SubtitleFile[i]
            else
              Text := SubtitleFile[i];

            Inc(i);
          end;
          Dec(i);
          Text := HTMLTagsToTS(HTMLDecode(Text));
          x := Subtitles.Add(InitialTime, FinalTime, Text, '', ExtraInfo, False);
          Subtitles.ItemPointer[x]^.Align  := TSubtitleHAlign(c);
          Subtitles.ItemPointer[x]^.VAlign := TSubtitleVAlign(v);
        end;
      end;
      Inc(i);
    end;
  finally
    if Subtitles.Count > 0 then
    begin
      Subtitles.ExtraInfoType := eiWebVTT;
      Result := True;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function TUWWebVTT.SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  i : Integer;
  Text : String;
  Align : String;
//  ExtraTime : Integer;
begin
  Result  := False;

  StringList.Add('WEBVTT', False);

  //ExtraTime := 0;
  with Subtitles.FormatProperties^.WebVTT do
    if UseXTIMESTAMP then
    begin
      StringList.Add(SysUtils.Format('X-TIMESTAMP-MAP=MPEGTS:%d,LOCAL:%s', [MPEGTS, TimeToString(LOCAL, 'hh:mm:ss.zzz')]), False);
      //ExtraTime := LOCAL;
    end;

  StringList.Add('', False);

  for i := FromItem to ToItem do
  begin
    Align := '';

    // TODO
    if Subtitles.ExtraInfo[i] <> NIL then
      with PWebVTT_ExtraInfo(Subtitles.ExtraInfo[i])^ do
      begin

      end;

    if Subtitles[i].Align <> shaNone then
      case Subtitles[i].Align of
        shaLeft   : Align := ' align:left';
        shaCenter : Align := ' align:center';
        shaRight  : Align := ' align:right';
      end;

    if Subtitles[i].VAlign <> svaBottom then
      case Subtitles[i].VAlign of
        svaBottom : Align := Align + ' line:80%';
        svaCenter : Align := Align + ' line:50%';
        svaTop    : Align := Align + ' line:10%';
      end;

    if Subtitles.FormatProperties^.WebVTT.WriteCueIdentifiers then
      StringList.Add((i+1).ToString, False);

    StringList.Add(TimeToString(Subtitles.InitialTime[i], 'hh:mm:ss.zzz') + ' --> ' + TimeToString(Subtitles.FinalTime[i], 'hh:mm:ss.zzz') + Align, False);
    Text := TSTagsToHTML(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]));
    StringList.Add(Text, False);
    StringList.Add('', False);
  end;

  try
    StringList.SaveToFile(FileName, TEncoding.UTF8); // must be encoded using UTF-8
    Result := True;
  except
  end;
end;

// -----------------------------------------------------------------------------

end.
