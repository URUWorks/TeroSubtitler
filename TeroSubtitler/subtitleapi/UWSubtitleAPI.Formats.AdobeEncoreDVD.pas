{*
 * URUWorks Subtitle API
 *
 * The contents of this file are used with permission, subject to
 * the Mozilla Public License Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/2.0.html
 *
 * Software distributed under the License is distributed on an
 * "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * Copyright (C) 2001-2023 URUWorks, uruworks@gmail.com.
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
    function LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean; override;
    function SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean; override;
  end;

// -----------------------------------------------------------------------------

implementation

uses UWSubtitleAPI.Tags;

const
  ATimeFormat = 'hh:mm:ss:ff';

// -----------------------------------------------------------------------------

function IsValidEncorePrefix(const APrefix: String): Boolean;
var
  P: String;
  Idx: Integer;
begin
  P := Trim(APrefix);

  // Si no hay nada antes es válido (Encore estándar)
  if P = '' then Exit(True);

  // Si contiene el metadato lo quitamos temporalmente
  Idx := Pos('', P);
  if Idx > 0 then
    P := Trim(Copy(P, Idx + 1, MaxInt));

  Result := (P = '') or IsInteger(P);
end;

// -----------------------------------------------------------------------------

function TryParseEncoreLine(const S: String; out NormLine: String): Boolean;
var
  i, j: Integer;
  TC1, TC2: String;
  Prefix: String;
begin
  Result := False;
  if Length(S) < 23 then Exit;

  for i := 1 to Length(S) - 22 do
  begin
    if (S[i+2] = ':') and (S[i+5] = ':') and (S[i+8] in [':', ';']) then
    begin
      TC1 := Copy(S, i, 11);
      if TimeInFormat(TC1, ATimeFormat) then
      begin

        Prefix := Copy(S, 1, i - 1);
        if not IsValidEncorePrefix(Prefix) then
          Continue;

        j := i + 11;
        while (j <= Length(S)) and (S[j] in [' ', #9]) do Inc(j);

        if (Length(S) - j + 1 >= 11) then
        begin
          TC2 := Copy(S, j, 11);
          if TimeInFormat(TC2, ATimeFormat) then
          begin
            NormLine := TC1 + ' ' + TC2;
            j := j + 11;

            while (j <= Length(S)) and (S[j] in [' ', #9]) do Inc(j);

            if j <= Length(S) then
              NormLine := NormLine + ' ' + Copy(S, j, MaxInt)
            else
              NormLine := NormLine + ' ';

            Exit(True);
          end;
        end;
      end;
    end;
  end;
end;

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
var
  NormLine: String;
begin
  if (Row < 0) or (Row >= SubtitleFile.Count) then
    Exit(False);

  if not TryParseEncoreLine(SubtitleFile[Row], NormLine) then
    Exit(False);

  Result := (StrToIntDef(Copy(NormLine, 10, 2), 0) < 30) and
            (StrToIntDef(Copy(NormLine, 22, 2), 0) < 30) and
            (not IsInteger(Copy(NormLine, 25, 2)));
end;

// -----------------------------------------------------------------------------

function TUWAdobeEncoreDVD.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
  Line        : String;
  NormLine    : String;
begin
  Result := False;
  try
    i := 0;
    while i < SubtitleFile.Count do
    begin
      Line := SubtitleFile[i];

      if TryParseEncoreLine(Line, NormLine) then
      begin
        InitialTime := HHMMSSFFTimeToMS(Copy(NormLine, 1, 11), FPS);
        FinalTime   := HHMMSSFFTimeToMS(Copy(NormLine, 13, 11), FPS);
        Text        := Copy(NormLine, 25, MaxInt);

        Inc(i);
        while (i < SubtitleFile.Count) do
        begin
          Line := SubtitleFile[i];

          if TryParseEncoreLine(Line, NormLine) then
            Break;

          if Text <> '' then
            Text := Text + sLineBreak + Line
          else
            Text := Line;

          Inc(i);
        end;
        Dec(i);

        Text := TrimRight(Text);

        if (InitialTime >= 0) and (FinalTime > 0) and not Text.IsEmpty then
          Subtitles.Add(InitialTime, FinalTime, Text, '', NIL);
      end;
      Inc(i);
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

// -----------------------------------------------------------------------------

function TUWAdobeEncoreDVD.SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  i: Integer;
  LineText: String;
begin
  Result  := False;

  StringList.Clear;

  for i := FromItem to ToItem do
  begin
    LineText := RemoveTSTags(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]));

    StringList.Add(MSToHHMMSSFFTime(Subtitles.InitialTime[i], FPS) + ' ' +
                   MSToHHMMSSFFTime(Subtitles.FinalTime[i], FPS) + ' ' +
                   LineText);
  end;

  try
    StringList.SaveToFile(FileName, Encoding);
    Result := True;
  except
  end;
end;

// -----------------------------------------------------------------------------

end.
