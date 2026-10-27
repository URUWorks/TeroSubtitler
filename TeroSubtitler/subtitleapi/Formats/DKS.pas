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

unit DKS;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSubtitleAPI.Formats;

type

  { TUWDKS }

  TUWDKS = class(TUWSubtitleCustomFormat)
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

uses UWSubtitleAPI.Tags, UWSystem.StrUtils, UWSystem.SysUtils;

// -----------------------------------------------------------------------------

function TUWDKS.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWDKS.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfDKS;
end;

// -----------------------------------------------------------------------------

function TUWDKS.Extension: String;
begin
  Result := '*.dks';
end;

// -----------------------------------------------------------------------------

function TUWDKS.HasStyleSupport: Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------

function TUWDKS.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  if (Pos('[', SubtitleFile[Row]) = 1) and
     (Pos(']', SubtitleFile[Row]) = 10) and
     (TimeInFormat(Copy(SubtitleFile[Row], 2, 8), 'hh:mm:ss')) and
     (TimeInFormat(Copy(SubtitleFile[Row+1], 2, 8), 'hh:mm:ss')) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWDKS.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
begin
  Result    := False;
  FinalTime := -1;
  try
    for i := 0 to SubtitleFile.Count-1 do
    begin
      If (Pos('[', SubtitleFile[i]) = 1) and
         (Pos(']', SubtitleFile[i]) = 10) and
         (TimeInFormat(Copy(SubtitleFile[i], 2, 8), 'hh:mm:ss')) then
      begin
        InitialTime := StringToTime(Copy(SubtitleFile[i], 2, 8));
        // Antes "if (i <= Count-1)" era SIEMPRE verdadero: en el último cue (o si
        // la línea siguiente no era un marcador de fin) se leía SubtitleFile[i+1]
        // fuera de rango o basura, y FinalTime quedaba heredado del cue anterior.
        // Ahora solo se usa la línea siguiente si realmente es un [hh:mm:ss].
        FinalTime := -1;
        if (i+1 <= SubtitleFile.Count-1) and
           (Pos('[', SubtitleFile[i+1]) = 1) and
           (Pos(']', SubtitleFile[i+1]) = 10) and
           TimeInFormat(Copy(SubtitleFile[i+1], 2, 8), 'hh:mm:ss') then
          FinalTime := StringToTime(Copy(SubtitleFile[i+1], 2, 8));
        if FinalTime <= InitialTime then FinalTime := InitialTime + 2000;

        Text := ReplaceString(Copy(SubtitleFile[i], 11, Length(SubtitleFile[i])), '[br]', LineEnding);

        if (InitialTime >= 0) and (FinalTime > 0) and not Text.IsEmpty then
          Subtitles.Add(InitialTime, FinalTime, Text, '');
      end;
    end;
  finally
    Result := Subtitles.Count > 0;
  end;
end;

// -----------------------------------------------------------------------------

function TUWDKS.SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  i    : Integer;
  Text : String;
  EndT : Integer;
begin
  Result  := False;

  for i := FromItem to ToItem do
  begin
    Text := RemoveTSTags(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]));

    // Antes se modificaba el FinalTime del documento al GUARDAR (sin undo).
    // Ahora un fin local, sin tocar el subtítulo.
    EndT := Subtitles.FinalTime[i];
    if EndT = Subtitles.InitialTime[i] then
      EndT := Subtitles.InitialTime[i] + 1000;

    StringList.Add('[' + TimeToString(Subtitles.InitialTime[i], 'hh:mm:ss') + ']' + ReplaceEnters(Text, LineEnding, '[br]'));
    StringList.Add('[' + TimeToString(EndT, 'hh:mm:ss') + ']');
  end;

  try
    StringList.SaveToFile(FileName, Encoding);
    Result := True;
  except
  end;
end;

// -----------------------------------------------------------------------------

end.
