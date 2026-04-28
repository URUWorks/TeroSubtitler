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
 * Copyright (C) 2001-2026 URUWorks, uruworks@gmail.com.
 *}

unit UWSubtitleAPI.Formats.SoftNI;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSubtitleAPI.Formats;

type

  { TUWSoftNi }

  TUWSoftNI = class(TUWSubtitleCustomFormat)
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

function TUWSoftNI.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWSoftNI.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfSoftNI;
end;

// -----------------------------------------------------------------------------

function TUWSoftNI.Extension: String;
begin
  Result := '*.sub';
end;

// -----------------------------------------------------------------------------

function TUWSoftNI.HasStyleSupport: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWSoftNI.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  Result := (Pos('\', Trim(SubtitleFile[Row])) = 12) or SubtitleFile[Row].StartsWith('*PART', True);
end;

// -----------------------------------------------------------------------------

function TUWSoftNI.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
  CurrentLine : String;
  Item        : TUWSubtitleItem;
  LocalFPS    : Double;
  TimingParts : array of String;
begin
  Result := False;
  LocalFPS := FPS;

  // Pre-análisis de *TIMING* para "FPS"
  for i := SubtitleFile.Count - 1 downto 0 do
  begin
    if Trim(SubtitleFile[i]).StartsWith('*TIMING*', True) then
    begin
      if i + 1 < SubtitleFile.Count then
      begin
        TimingParts := Trim(SubtitleFile[i + 1]).Split([' ']);
        if Length(TimingParts) >= 2 then
        begin
          // El segundo valor contiene el framerate (ej. 1 24 0)
          TryStrToFloat(TimingParts[1], LocalFPS);
        end;
      end;
      Break;
    end;
  end;

  try
    Subtitles.FrameRate := LocalFPS;
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

      if CurrentLine.StartsWith('*END*', True) then Break;

      if Pos('\', CurrentLine) <> 12 then
      begin
        Text := '';
        while (i < SubtitleFile.Count) do
        begin
          CurrentLine := Trim(SubtitleFile[i]);
          if (Pos('\', CurrentLine) = 12) or CurrentLine.StartsWith('*END*', True) then Break;

          if Text <> '' then
            Text := Text + LineEnding + CurrentLine
          else
            Text := CurrentLine;
          Inc(i);
        end;

        if (i < SubtitleFile.Count) and (Pos('\', Trim(SubtitleFile[i])) = 12) then
        begin
          CurrentLine := Trim(SubtitleFile[i]);
          // Usamos LocalFPS para calcular el tiempo exacto basado en el archivo
          InitialTime := StringToTime(Copy(CurrentLine, 1, 11), False, LocalFPS);
          FinalTime   := StringToTime(Copy(CurrentLine, 13, 11), False, LocalFPS);

          Text := Trim(Text);

          ClearSubtitleItem(Item);
          Item.InitialTime := InitialTime;
          Item.FinalTime := FinalTime;

          // Detectar posición en pantalla ("Top")
          if Text.StartsWith('}') then
          begin
            Item.VAlign := svaTop;
            // Eliminar "}"
            Text := Trim(Copy(Text, 2, Length(Text)));
          end
          else
            Item.VAlign := svaBottom; // Por defecto

          Item.Text := SoftNITagsToTS(Text);

          if (InitialTime >= 0) and (FinalTime > 0) and (Item.Text <> '') then
            Subtitles.Add(Item, NIL);
        end;
      end;
      Inc(i);
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

// -----------------------------------------------------------------------------

function TUWSoftNI.SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
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
      Text := TSTagsToSoftNI(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]));

      if Subtitles[i].VAlign = svaTop then
        Text := '}' + Text;

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
    StringList.Add(SysUtils.Format('1 %d 1 1 1', [Round(FPS)]));
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
