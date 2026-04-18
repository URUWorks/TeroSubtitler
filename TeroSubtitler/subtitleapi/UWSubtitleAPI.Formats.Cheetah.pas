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

unit UWSubtitleAPI.Formats.Cheetah;

// -----------------------------------------------------------------------------

interface

uses
  Classes, SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSubtitleAPI.Formats;

type

  { TUWCheetah }

  TUWCheetah = class(TUWSubtitleCustomFormat)
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

uses UWSubtitleAPI.Tags, UWSystem.SysUtils, UWSystem.StrUtils;

// -----------------------------------------------------------------------------

function TUWCheetah.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWCheetah.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfCheetah;
end;

// -----------------------------------------------------------------------------

function TUWCheetah.Extension: String;
begin
  Result := '*.asc';
end;

// -----------------------------------------------------------------------------

function TUWCheetah.HasStyleSupport: Boolean;
begin
  Result := True; // soporta cursivas (\AI\)
end;

// -----------------------------------------------------------------------------

function TUWCheetah.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
var
  Line: String;
begin
  Line := LowerCase(Trim(SubtitleFile[Row]));

  if Line.StartsWith('** caption number') then
    Exit(True);

  if Line.StartsWith('*t ') and (Length(Line) >= 14) and
     TimeInFormat(Copy(Line, 4, 11), 'hh:mm:ss:zz') then
    Exit(True);

  Result := False;
end;

// -----------------------------------------------------------------------------

function TUWCheetah.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean;
var
  i: Integer;
  InitialTime, FinalTime: Integer;
  Text, Line, STime: String;
begin
  Result := False;
  InitialTime := -1;
  FinalTime   := -1;
  Text        := '';

  try
    for i := 0 to SubtitleFile.Count - 1 do
    begin
      Line := TrimRight(SubtitleFile[i]);
      if Line = '' then Continue;

      // Detectamos un nuevo bloque
      if Pos('** ', Line) = 1 then
      begin
        if (InitialTime >= 0) and (Text <> '') then
        begin
          if FinalTime = -1 then FinalTime := InitialTime + 3000;
          Subtitles.Add(InitialTime, FinalTime, TrimRight(Text), '');
        end;

        InitialTime := -1;
        FinalTime   := -1;
        Text        := '';
      end
      else if (Pos('*T ', UpperCase(Line)) = 1) or (Pos('*t ', Line) = 1) then
      begin
        STime := Trim(Copy(Line, 4, 11));
        InitialTime := StringToTime(STime);
      end
      else if (Pos('*E ', UpperCase(Line)) = 1) or (Pos('*e ', Line) = 1) then
      begin
        STime := Trim(Copy(Line, 4, 11));
        FinalTime := StringToTime(STime);
      end
      else if Pos('*', Line) = 1 then
      begin
        // Ignoramos etiquetas que no nos sirven ahora (*PopOn, *BottomUp, *Cf16, etc.)
      end
      else
      begin
        Line := ReplaceString(Line, '\E', '', True, True);        // Marcador de fin de texto
        Line := ReplaceString(Line, '\AI\', '{\i1}', True, True); // Cursiva activada
        Line := ReplaceString(Line, '\AN\', '{\i0}', True, True); // Cursiva desactivada

        if Text <> '' then
          Text := Text + LineEnding + Line
        else
          Text := Line;
      end;
    end;

    // último bloque
    if (InitialTime >= 0) and (Text <> '') then
    begin
      if FinalTime = -1 then FinalTime := InitialTime + 3000;
      Subtitles.Add(InitialTime, FinalTime, TrimRight(Text), '');
    end;

    // Si algún bloque carecía de etiqueta *E, recalculamos su tiempo de fin real
    // basándonos en el inicio del siguiente bloque (común en subtitulado Broadcast).
    for i := 0 to Subtitles.Count - 1 do
    begin
      if (Pos('{\i1}', Subtitles.Text[i]) > 0) and (Pos('{\i0}', Subtitles.Text[i]) = 0) then
        Subtitles.Text[i] := Subtitles.Text[i] + '{\i0}';

      // Arreglo de tiempos finales perdidos
      if (i < Subtitles.Count - 1) and (Subtitles.FinalTime[i] = Subtitles.InitialTime[i] + 3000) then
      begin
        // Ajustamos para que desaparezca justo 100ms antes de que empiece el siguiente
        if Subtitles.FinalTime[i] > Subtitles.InitialTime[i+1] then
          Subtitles.FinalTime[i] := Subtitles.InitialTime[i+1] - 100;
      end;
    end;

  finally
    Result := Subtitles.Count > 0;
  end;
end;

// -----------------------------------------------------------------------------

function TUWCheetah.SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  i, SubIndex : Integer;
  Text, CleanText : String;
begin
  Result  := False;

  StringList.Clear;

  StringList.Add('*NonDropFrame');
  StringList.Add('*WIDTH 32');
  StringList.Add('');
  SubIndex := 1;

  for i := FromItem to ToItem do
  begin
    Text := iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]);

    // Reconvertimos las etiquetas internas de cursiva al estándar Cheetah
    if Pos('{\i1}', LowerCase(Text)) > 0 then
    begin
      CleanText := RemoveTSTags(Text);
      CleanText := '\AI\' + CleanText;
    end
    else
      CleanText := RemoveTSTags(Text);

    // Cheetah exige la \E al final del texto físico
    CleanText := CleanText + '\E';

    StringList.Add('** Caption Number '+ IntToStr(SubIndex));
    StringList.Add('*PopOn');
    StringList.Add('*T ' + TimeToString(Subtitles.InitialTime[i], 'hh:mm:ss:zz'));
    StringList.Add('*E ' + TimeToString(Subtitles.FinalTime[i], 'hh:mm:ss:zz'));
    StringList.Add('*BottomUp');
    StringList.Add('*Lf01');
    StringList.Add(CleanText);
    StringList.Add('');

    Inc(SubIndex);
  end;

  try
    StringList.SaveToFile(FileName, Encoding);
    Result := True;
  except
  end;
end;

// -----------------------------------------------------------------------------

end.
