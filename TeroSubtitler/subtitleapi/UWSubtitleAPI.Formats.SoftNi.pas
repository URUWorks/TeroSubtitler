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

unit UWSubtitleAPI.Formats.SoftNi;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSubtitleAPI.Formats;

type

  { TUWSoftNi }

  TUWSoftNi = class(TUWSubtitleCustomFormat)
  public
    function Name: String; override;
    function Format: TUWSubtitleFormats; override;
    function Extension: String; override;
    function HasStyleSupport: Boolean; override;
    function IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean; override;
    function LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean; override;
    function SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean; override;
  end;

// -----------------------------------------------------------------------------

implementation

uses UWSubtitleAPI.Tags, UWSystem.SysUtils;

// -----------------------------------------------------------------------------

function TUWSoftNi.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWSoftNi.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfSoftNi;
end;

// -----------------------------------------------------------------------------

function TUWSoftNi.Extension: String;
begin
  Result := '*.sub';
end;

// -----------------------------------------------------------------------------

function TUWSoftNi.HasStyleSupport: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWSoftNi.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  Result := (Pos('\', Trim(SubtitleFile[Row])) = 12) or SubtitleFile[Row].StartsWith('*PART', True);
end;

// -----------------------------------------------------------------------------

function TUWSoftNi.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
  CurrentLine : String;
begin
  Result := False;
  try
    i := 0;
    while i < SubtitleFile.Count do
    begin
      CurrentLine := Trim(SubtitleFile[i]);

      // Ignorar marcas de sección iniciales
      if CurrentLine.StartsWith('*PART', True) then
      begin
        Inc(i);
        if (i < SubtitleFile.Count) and (Pos('\', Trim(SubtitleFile[i])) = 12) then
          Inc(i);

        Continue;
      end;

      // Final del bloque de subtítulos (comienzan los metadatos de SoftNi)
      if CurrentLine.StartsWith('*END*', True) then
        Break;

      // Si no detectamos la barra de tiempo en la columna 12 asumimos que es inicio de texto
      if Pos('\', CurrentLine) <> 12 then
      begin
        Text := '';

        // Acumular texto hasta encontrar el timecode (o el final de los subs)
        while (i < SubtitleFile.Count) do
        begin
          CurrentLine := Trim(SubtitleFile[i]);

          if (Pos('\', CurrentLine) = 12) or CurrentLine.StartsWith('*END*', True) then
            Break;

          if Text <> '' then
            Text := Text + LineEnding + CurrentLine
          else
            Text := CurrentLine;

          Inc(i);
        end;

        if (i < SubtitleFile.Count) and (Pos('\', Trim(SubtitleFile[i])) = 12) then
        begin
          CurrentLine := Trim(SubtitleFile[i]);
          InitialTime := StringToTime(Copy(CurrentLine, 1, 11), False, FPS);
          FinalTime   := StringToTime(Copy(CurrentLine, 13, 11), False, FPS);

          Text := SoftNiTagsToTS(Trim(Text));
          if (InitialTime >= 0) and (FinalTime > 0) and (Text <> '') then
            Subtitles.Add(InitialTime, FinalTime, Text, '', NIL);
        end;
      end;

      Inc(i);
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

// -----------------------------------------------------------------------------

function TUWSoftNi.SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  i    : Integer;
  Text : String;
begin
  Result  := False;
  try
    StringList.Clear;
    StringList.Add('*PART 1*');
    StringList.Add('00:00:00.00\00:00:00.00');

    for i := FromItem to ToItem do
    begin
      Text := TSTagsToSoftNi(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]));
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

    StringList.SaveToFile(FileName, Encoding);
    Result := True;
  except
  end;
end;

// -----------------------------------------------------------------------------

end.
