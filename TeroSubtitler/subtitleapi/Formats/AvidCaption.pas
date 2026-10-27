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

unit AvidCaption;

// -----------------------------------------------------------------------------

interface

uses
  Classes, SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSubtitleAPI.Formats;

type

  { TUWAvidCaption }

  TUWAvidCaption = class(TUWSubtitleCustomFormat)
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

uses UWSubtitleAPI.Tags, UWSystem.StrUtils, UWSystem.SysUtils;

const
  ATimeFormat = 'hh:mm:ss:ff';

// -----------------------------------------------------------------------------

function TUWAvidCaption.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWAvidCaption.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfAvidCaption;
end;

// -----------------------------------------------------------------------------

function TUWAvidCaption.Extension: String;
begin
  Result := '*.txt';
end;

// -----------------------------------------------------------------------------

function TUWAvidCaption.IsTimeBased: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWAvidCaption.HasStyleSupport: Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------

function IsAvidTimingLine(const S: String): Boolean;
var
  L: String;
begin
  L := Trim(S);
  Result := (Length(L) = 23) and (L[12] = ' ') and
            TimeInFormat(Copy(L, 1, 11), ATimeFormat) and
            TimeInFormat(Copy(L, 13, 11), ATimeFormat);
end;

// -----------------------------------------------------------------------------

function TUWAvidCaption.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  Result := Contains('@ This file ', SubtitleFile[Row]) or Contains('<begin subtitles>', SubtitleFile[Row]) or
            IsAvidTimingLine(SubtitleFile[Row]);
end;

// -----------------------------------------------------------------------------

function TUWAvidCaption.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text, L     : String;
begin
  Result := False;
  try
    i := 0;
    while i < SubtitleFile.Count do
    begin
      if IsAvidTimingLine(SubtitleFile[i]) then
      begin
        L           := Trim(SubtitleFile[i]);
        InitialTime := HHMMSSFFTimeToMS(Copy(L, 1, 11), FPS);
        FinalTime   := HHMMSSFFTimeToMS(Copy(L, 13, 11), FPS);
        Text        := '';

        // TUWStringList descarta las líneas vacías, así que el fin del cue es
        // el siguiente tiempo, '<end subtitles>' o el fin del archivo (antes
        // la última línea del archivo nunca se leía, el salto era #13#10 fijo
        // y '<end subtitles>' se pegaba al texto del último cue).
        Inc(i);
        while (i < SubtitleFile.Count) and not IsAvidTimingLine(SubtitleFile[i]) and
              not Contains('<end subtitles>', SubtitleFile[i]) do
        begin
          if (Length(SubtitleFile[i]) > 0) and (SubtitleFile[i][1] <> '@') then
          begin
            if Text <> '' then
              Text := Text + sLineBreak + SubtitleFile[i]
            else
              Text := SubtitleFile[i];
          end;

          Inc(i);
        end;
        Dec(i);

        if (InitialTime >= 0) and (FinalTime > 0) and (Text <> '') then
          Subtitles.Add(InitialTime, FinalTime, Text, '', NIL);
      end;
      Inc(i);
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

// -----------------------------------------------------------------------------

function TUWAvidCaption.SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  i, k: Integer;
  Lines: TStringList;
begin
  Result  := False;

  StringList.Clear;
  StringList.Add('@ This file was written with Tero Subtitler');
  StringList.Add('');
  StringList.Add('<begin subtitles>');

  for i := FromItem to ToItem do
  begin
    StringList.Add(MSToHHMMSSFFMax(Subtitles.InitialTime[i], FPS) + ' ' +
      MSToHHMMSSFFMax(Subtitles.FinalTime[i], FPS));

    // sin líneas vacías internas: una línea en blanco cierra el cue
    Lines := TStringList.Create;
    try
      Lines.Text := RemoveTSTags(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]));
      for k := 0 to Lines.Count - 1 do
        if Trim(Lines[k]) <> '' then StringList.Add(Lines[k]);
    finally
      Lines.Free;
    end;
    StringList.Add('');
  end;

  StringList.Add('<end subtitles>');

  try
    StringList.SaveToFile(FileName, Encoding);
    Result := True;
  except
  end;
end;

// -----------------------------------------------------------------------------

end.
