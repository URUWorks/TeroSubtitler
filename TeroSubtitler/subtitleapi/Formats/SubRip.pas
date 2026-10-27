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
 *  Copyright (C) 2001-2026 URUWorks, uruworks@gmail.com.
 *}

unit SubRip;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSystem.SysUtils,
  UWSubtitleAPI.Formats;

type

  { TUWSubRip }

  TUWSubRip = class(TUWSubtitleCustomFormat)
  public
    function Name: String; override;
    function Format: TUWSubtitleFormats; override;
    function Extension: String; override;
    function IsTimeBased: Boolean; override;
    function HasStyleSupport: Boolean; override;
    function IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean; override;
    function LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean; override;
    function SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean; override;
  end;

// -----------------------------------------------------------------------------

implementation

uses UWSubtitleAPI.ExtraInfo, UWSubtitleAPI.Tags;

{
First line is the subtitle index, it's just a sequential number.
Second line is the start and stop time, it can optionally include subtitle coordinates in pixels as a bounding box (X1:left X2:right Y1:top Y2:bottom).
The third line is the subtitle text (one or more lines, with optional tags)
Each subtitle is separated by a blank line.

The start and stop time have this format: HH:MM:SS,MIL (hours, minutes, seconds, milliseconds)

The available official tags are:

<b></b> : bold
<i></i> : italic
<u></u> : underline
<font color=”#rrggbb”></font> : text color using 3 color components, red, green and blue.
}

// -----------------------------------------------------------------------------

function TUWSubRip.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWSubRip.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfSubRip;
end;

// -----------------------------------------------------------------------------

function TUWSubRip.Extension: String;
begin
  Result := '*.srt';
end;

// -----------------------------------------------------------------------------

function TUWSubRip.IsTimeBased: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWSubRip.HasStyleSupport: Boolean;
begin
  Result := True; // Has tags and coords
end;

// -----------------------------------------------------------------------------

// Parses "h:mm:ss,zzz" also accepting 1-2 digit hours and "." as decimal
// separator. Returns -1 if the text is not a valid time.
function ParseSRTTime(const AText: String): Integer;
var
  Parts : TStringArray;
  s, ms : String;
  p, h, m, sec, z : Integer;
begin
  Result := -1;
  s := Trim(AText);
  p := Pos(' ', s); // remove coordinates after the final time (X1:...)
  if p > 0 then s := Copy(s, 1, p-1);
  s := StringReplace(s, '.', ',', [rfReplaceAll]);

  Parts := s.Split([':']);
  if Length(Parts) <> 3 then Exit;
  p := Pos(',', Parts[2]);
  if p > 0 then
  begin
    ms := Copy(Parts[2], p+1, 3);
    while Length(ms) < 3 do ms := ms + '0';
    Parts[2] := Copy(Parts[2], 1, p-1);
  end
  else
    ms := '0';

  if not TryStrToInt(Parts[0], h) or not TryStrToInt(Parts[1], m) or
     not TryStrToInt(Parts[2], sec) or not TryStrToInt(ms, z) then Exit;

  Result := ((h * 3600) + (m * 60) + sec) * 1000 + z;
end;

// -----------------------------------------------------------------------------

// Antes se exigÃ­a ' --> ' con espacios: '00:00:01,000-->00:00:03,000' se perdÃ­a
function IsSRTTimingLine(const ALine: String): Boolean;
var
  x: Integer;
begin
  x := Pos('-->', ALine);
  Result := (x > 0) and (ParseSRTTime(Copy(ALine, 1, x-1)) >= 0) and
    (ParseSRTTime(Copy(ALine, x+3, MaxInt)) >= 0);
end;

// -----------------------------------------------------------------------------

function TUWSubRip.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  Result := IsSRTTimingLine(SubtitleFile[Row]);
end;

// -----------------------------------------------------------------------------

function TUWSubRip.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean;
var
  i, x        : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text, s     : String;
  ExtraInfo   : PSubRip_ExtraInfo;
  X1, X2,
  Y1, Y2      : Integer;
  IsTop       : Boolean;
begin
  Result := False;
  try
    i := 0;
    while i < SubtitleFile.Count do
    begin
      if IsSRTTimingLine(SubtitleFile[i]) then
      begin
        // split by "-->" instead of fixed widths, so single digit hours or
        // milliseconds with "." are also read
        s := SubtitleFile[i];
        x := Pos('-->', s);
        InitialTime := ParseSRTTime(Copy(s, 1, x-1));
        FinalTime   := ParseSRTTime(Copy(s, x+3, MaxInt));

        X1 := Pos('X1', SubtitleFile[i]); X2 := Pos('X2', SubtitleFile[i]);
        Y1 := Pos('Y1', SubtitleFile[i]); Y2 := Pos('Y2', SubtitleFile[i]);
        IsTop := False;
        if (X1 > 0) and (X2 > 0) and (Y1 > 0) and (Y2 > 0) then
        begin
          New(ExtraInfo);  // X1:left X2:right Y1:top Y2:bottom
          ExtraInfo^.X1 := StrToIntDef(Copy(SubtitleFile[i], X1+3, ((X2-1)-(X1+3))), 0);
          ExtraInfo^.X2 := StrToIntDef(Copy(SubtitleFile[i], X2+3, ((Y1-1)-(X2+3))), 0);
          ExtraInfo^.Y1 := StrToIntDef(Copy(SubtitleFile[i], Y1+3, ((Y2-1)-(Y1+3))), 0);
          ExtraInfo^.Y2 := StrToIntDef(Copy(SubtitleFile[i], Y2+3, Length(SubtitleFile[i])-(Y2+2)), 0);
        end
        else
        begin
          ExtraInfo := NIL;

          if (X1 > 0) then
          begin
            s := Copy(SubtitleFile[i], X1+3);
            X2 := Pos(' ', s);
            if X2 > 0 then
              s := Copy(s, 1, X2-1);

            if StrToIntDef(s, -1) = 0 then
              IsTop := True;
          end;
        end;

        if (InitialTime >= 0) and (FinalTime > 0) then
        begin
          Inc(i);
          Text := '';

          while (i < SubtitleFile.Count) and
            ( (i = SubtitleFile.Count-1) or
              ((i+1 < SubtitleFile.Count) and not IsSRTTimingLine(SubtitleFile[i+1])) ) do
          begin
            if Text <> '' then
              Text := Text + sLineBreak + SubtitleFile[i]
            else
              Text := SubtitleFile[i];

            Inc(i);
          end;

          // the line just before the next "-->" is the entry number in a
          // normal SRT, but text in an SRT without numbering: keep it
          if (i < SubtitleFile.Count) and (Trim(SubtitleFile[i]) <> '') and
             not IsSRTTimingLine(SubtitleFile[i]) and not IsInteger(Trim(SubtitleFile[i])) then
          begin
            if Text <> '' then
              Text := Text + sLineBreak + SubtitleFile[i]
            else
              Text := SubtitleFile[i];
          end;

          Text := HTMLTagsToTS(Text);
          x := Subtitles.Add(InitialTime, FinalTime, Text, '', ExtraInfo);

          // Antes: TSubtitleVAlign(X1), y si X1 no era 'X1:0' quedaba la
          // posiciÃ³n del texto como valor del enum (fuera de rango)
          if (ExtraInfo = NIL) and IsTop and (x >= 0) then
            Subtitles.ItemPointer[x]^.VAlign := svaTop;
        end
        else if ExtraInfo <> NIL then
          Dispose(ExtraInfo); // invalid times: the entry is not added
      end;
      Inc(i);
    end;
  finally
    if Subtitles.Count > 0 then
    begin
      Subtitles.ExtraInfoType := eiSubRip;
      Result := True;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function TUWSubRip.SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  i     : Integer;
  Count : Integer;
  Text  : String;
  XY    : String;
begin
  Result := False;

  Count := 1;
  for i := FromItem to ToItem do
  begin
    StringList.Add(IntToStr(Count));
    Inc(Count);
    if (Subtitles.ExtraInfoType = eiSubRip) and (Subtitles.ExtraInfo[i] <> NIL) then
      with PSubRip_ExtraInfo(Subtitles.ExtraInfo[i])^ do
      begin
        if (X1 <> 0) or (X2 <> 0) or (Y1 <> 0) or (Y2 <> 0) then
          XY := SysUtils.Format(' X1:%d X2:%d Y1:%d Y2:%d', [X1, X2, Y1, Y2])
        else
          XY := '';
      end
    else
    begin
      if Subtitles[i].VAlign = svaTop then
        XY := ' X1:0'
      else
        XY := '';
    end;

    StringList.Add(TimeToString(Subtitles.InitialTime[i], 'hh:mm:ss,zzz') + ' --> ' + TimeToString(Subtitles.FinalTime[i], 'hh:mm:ss,zzz') + XY);
    Text := TSTagsToHTML(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]));
    StringList.Add(Text);
    StringList.Add('');
  end;

  try
    StringList.SaveToFile(FileName, Encoding);
    Result := True;
  except
  end;
end;

// -----------------------------------------------------------------------------

end.
