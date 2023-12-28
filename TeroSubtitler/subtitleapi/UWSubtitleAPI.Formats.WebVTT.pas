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
  Classes, SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSubtitleAPI.Formats,
  RegExpr, Graphics;

type

  { TUWWebVTT }

  TUWWebVTT = class(TUWSubtitleCustomFormat)
  private
    FColors : TStringList;
    function FromRepl(ARegExpr: TRegExpr): RegExprString;
    function TagsToTS(const Text: String): String;
    function ToRepl(ARegExpr: TRegExpr): RegExprString;
    function TSToTags(const Text: String): String;
  public
    constructor Create;
    destructor Destroy; override;
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

Color declaration tags
<c.lime>Hey!</c>

Voice declaration tags

<v.Name>
Example:
01:23:45.678 --> 01:23:46.789
- <v.John>Hey!</v>
- <v.Jane>Hey!</v>
}

type

  TUWWebVTTColor = record
    Name  : String;
    Color : String;
  end;

const

  TUWWebVTTDefColors : array[0..7] of TUWWebVTTColor =
  (
    (Name: 'white'; Color: 'ffffff'),
    (Name: 'lime'; Color: '00ff00'),
    (Name: 'cyan'; Color: 'ffff00'),
    (Name: 'red'; Color: '0000ff'),
    (Name: 'yellow'; Color: '00ffff'),
    (Name: 'magenta'; Color: 'ff00ff'),
    (Name: 'blue'; Color: 'ff0000'),
    (Name: 'black'; Color: '000000')
  );

// -----------------------------------------------------------------------------

constructor TUWWebVTT.Create;
var
  i: Integer;
begin
  inherited Create;

  FColors := TStringList.Create;
  FColors.Duplicates := dupIgnore;
  for i := 0 to High(TUWWebVTTDefColors) do
    FColors.AddPair(TUWWebVTTDefColors[i].Name, TUWWebVTTDefColors[i].Color);
end;

// -----------------------------------------------------------------------------

destructor TUWWebVTT.Destroy;
begin
  FColors.Free;

  inherited Destroy;
end;

// -----------------------------------------------------------------------------

function TUWWebVTT.FromRepl(ARegExpr: TRegExpr): RegExprString;
var
  i: Integer;
  s: String;
begin
  s := ARegExpr.Match[1];
  i := FColors.IndexOfName(s);

  if i >= 0 then
    s := FColors.ValueFromIndex[i];

  Result := '{\c&' + s + '&}';
end;

// -----------------------------------------------------------------------------

function TUWWebVTT.TagsToTS(const Text: String): String;
begin
  Result := Text;
  if Result.IsEmpty then Exit;

  with TRegExpr.Create('\<c\.(.*?)\>') do
  try
     Result := Replace(Result, @FromRepl);
  finally
    Free;
  end;

  Result := ReplaceString(Result, '</c>', '{\c}');
end;

// -----------------------------------------------------------------------------

function TUWWebVTT.ToRepl(ARegExpr: TRegExpr): RegExprString;
var
  i: Integer;
  s: String;
  b: Boolean;
begin
  b := False;
  s := ARegExpr.Match[1];
  for i := 0 to FColors.Count-1 do
    if s = FColors.ValueFromIndex[i] then
    begin
      s := FColors.Names[i];
      b := True;
      Break;
    end;

  if not b then
    FColors.AddPair(s, s);

  Result := '<c.' + s + '>';
end;

// -----------------------------------------------------------------------------

function TUWWebVTT.TSToTags(const Text: String): String;
var
  i: Integer;
  s: String;
begin
  Result := Text;
  if Result.IsEmpty then Exit;

  with TRegExpr.Create('\{\\c&(.*?)\&}') do
  try
     Result := Replace(Result, @ToRepl);
  finally
    Free;
  end;

  Result := ReplaceString(Result, '{\c}', '</c>');
end;

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

  function rgbToStr(s: String): String;
  var
    sArray: TStringArray;
  begin
    Result := '';
    sArray := s.Split(',');

    if Length(sArray) > 2 then
      Result := IntToHexStr(RGBToColor(sArray[0].ToInteger, sArray[1].ToInteger, sArray[2].ToInteger)).ToLower;

    SetLength(sArray, 0);
  end;

var
  i, c, x, v : Integer;
  InitialTime : Integer;
  FinalTime : Integer;
  Text, s, Voice : String;
  ExtraInfo : PWebVTT_ExtraInfo;
  ExtraTime : Integer;
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
      if SubtitleFile[i].StartsWith('::cue(.') then
      begin
        with TRegExpr.Create('\:\:cue\(([a-zA-Z\._\-\d%#]*)\)\s\{\scolor:rgb\(([\,\d]*)\)\s\}') do // "::cue(.mycolor) { color:rgb(254,219,72) }"
        try
          try
            InputString := SubtitleFile[i];
            if Exec then
              FColors.AddPair(Match[1], rgbToStr(Match[2]));
          except
          end;
        finally
          Free;
        end;
      end
      else if Pos(' --> ', SubtitleFile[i]) > 0 then
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

          // Get speaker/voice
          Voice := '';
          with TRegExpr.Create('\<v\s([\S]*)\>([\s\S]*)\<\/v\>') do // "<v Bob>text</v>"
          try
            try
              InputString := Text;
              if Exec then
              begin
                Voice := Match[1];
                Text := Replace(InputString, '$2', True);
              end;
            except
            end;
          finally
            Free;
          end;

          // Final text
          Text := TagsToTS(HTMLTagsToTS(HTMLDecode(Text)));
          x := Subtitles.Add(InitialTime, FinalTime, Text.Trim, '', ExtraInfo);
          Subtitles.ItemPointer[x]^.Actor := Voice;
          Subtitles.ItemPointer[x]^.Align := TSubtitleHAlign(c);
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
  i, c : Integer;
  Text : String;
  Align : String;
//  ExtraTime : Integer;
  SL : TStringList;
  b : Boolean;
begin
  Result := False;

  StringList.Add('WEBVTT');

  //ExtraTime := 0;
  with Subtitles.FormatProperties^.WebVTT do
    if UseXTIMESTAMP then
    begin
      StringList.Add(SysUtils.Format('X-TIMESTAMP-MAP=MPEGTS:%d,LOCAL:%s', [MPEGTS, TimeToString(LOCAL, 'hh:mm:ss.zzz')]));
      //ExtraTime := LOCAL;
    end;

  StringList.Add('');

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
      StringList.Add((i+1).ToString);

    StringList.Add(TimeToString(Subtitles.InitialTime[i], 'hh:mm:ss.zzz') + ' --> ' + TimeToString(Subtitles.FinalTime[i], 'hh:mm:ss.zzz') + Align);
    Text := TSTagsToHTML(TSToTags(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i])));
    if not Subtitles[i].Actor.IsEmpty then
      Text := '<v ' + Subtitles[i].Actor + '>' + Text + '</v>';

    StringList.Add(Text);
    StringList.Add('');
  end;

  SL := TStringList.Create;
  try
    SL.Duplicates := dupIgnore;
    SL.Clear;

    for i := 0 to FColors.Count-1 do
    begin
      b := False;
      for c := 0 to High(TUWWebVTTDefColors) do
        if FColors.Names[i] = TUWWebVTTDefColors[c].Name then
        begin
          b := True;
          Break;
        end;

      if not b then
        SL.Add(
          SysUtils.Format('::cue(.%s) { color:rgb(%d,%d,%d) }',
            [FColors.Names[i], HexToByte(Copy(FColors.ValueFromIndex[i], 1, 2)), HexToByte(Copy(FColors.ValueFromIndex[i], 3, 2)), HexToByte(Copy(FColors.ValueFromIndex[i], 5, 2))])
        );
    end;

    if SL.Count > 0 then
    begin
      SL.Insert(0, 'STYLE');
      StringList.Insert(2, SL.Text, False);
    end;
  finally
    SL.Free;
  end;

  try
    StringList.SaveToFile(FileName, TEncoding.UTF8); // must be encoded using UTF-8
    Result := True;
  except
  end;
end;

// -----------------------------------------------------------------------------

end.
