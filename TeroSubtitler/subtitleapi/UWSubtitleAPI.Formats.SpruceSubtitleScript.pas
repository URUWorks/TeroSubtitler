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

unit UWSubtitleAPI.Formats.SpruceSubtitleScript;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, Classes, UWSubtitleAPI, UWSystem.TimeUtils, UWSubtitleAPI.Formats;

type

  { TUWSpruceSubtitleScript }

  TUWSpruceSubtitleScript = class(TUWSubtitleCustomFormat)
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

function TUWSpruceSubtitleScript.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWSpruceSubtitleScript.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfSpruceSubtitleScript;
end;

// -----------------------------------------------------------------------------

function TUWSpruceSubtitleScript.Extension: String;
begin
  Result := '*.sub;*.sst';
end;

// -----------------------------------------------------------------------------

function TUWSpruceSubtitleScript.HasStyleSupport: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWSpruceSubtitleScript.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
var
  sExt: String;
  LineText: String;
begin
  Result := False;
  sExt := LowerCase(ExtractFileExt(SubtitleFile.FileName));
  
  if (sExt = '.sub') or (sExt = '.sst') then
  begin
    LineText := UpperCase(Trim(SubtitleFile[Row]));
    if LineText.StartsWith('@JUST') or LineText.StartsWith('@STYLE') or LineText.StartsWith('@SHOW') then
      Result := True;
  end;
end;

// -----------------------------------------------------------------------------

function TUWSpruceSubtitleScript.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  TempText    : String;
  CurrentLine : String;
  StyleStr    : String;
  TimeStr     : String;
  pSpace      : Integer;
  IsItalic    : Boolean;
begin
  Result := False;
  try
    TempText := '';
    IsItalic := False;

    for i := 0 to SubtitleFile.Count - 1 do
    begin
      CurrentLine := Trim(SubtitleFile[i]);
      if CurrentLine.IsEmpty then Continue;

      // Justificación (C = Center)
      if CurrentLine.StartsWith('@JUST', True) then Continue;

      // Estilo (1 = cursiva)
      if CurrentLine.StartsWith('@STYLE', True) then
      begin
        StyleStr := Trim(Copy(CurrentLine, 7, Length(CurrentLine)));
        IsItalic := (StyleStr = '1');
        Continue;
      end;

      // @SHOW cierra el bloque y nos da los tiempos
      if CurrentLine.StartsWith('@SHOW', True) then
      begin
        TimeStr := Trim(Copy(CurrentLine, 6, Length(CurrentLine)));
        
        pSpace := Pos(' ', TimeStr);
        if (pSpace > 0) and (TempText <> '') then
        begin
          InitialTime := StringToTime(Trim(Copy(TimeStr, 1, pSpace - 1)), False, FPS);
          FinalTime   := StringToTime(Trim(Copy(TimeStr, pSpace + 1, Length(TimeStr))), False, FPS);

          if (InitialTime >= 0) and (FinalTime > 0) then
          begin
            if IsItalic then
              TempText := '{\i1}' + TempText + '{\i0}';

            Subtitles.Add(InitialTime, FinalTime, TempText, '', NIL);
          end;
        end;

        TempText := '';
        IsItalic := False;
      end
      else
      begin
        if TempText <> '' then
          TempText := TempText + LineEnding + CurrentLine
        else
          TempText := CurrentLine;
      end;
    end;
  finally
    Result := Subtitles.Count > 0;
  end;
end;

// -----------------------------------------------------------------------------

function TUWSpruceSubtitleScript.SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  i : Integer;
  SubtitleText: String;
  IsItalic: Boolean;
begin
  Result  := False;
  StringList.Clear;

  for i := FromItem to ToItem do
  begin
    SubtitleText := iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]);
    
    IsItalic := SubtitleText.Contains('{\i1}');
    
    SubtitleText := ReplaceEnters(SubtitleText, sLineBreak);
    SubtitleText := StringReplace(SubtitleText, '{\i1}', '', [rfReplaceAll, rfIgnoreCase]);
    SubtitleText := StringReplace(SubtitleText, '{\i0}', '', [rfReplaceAll, rfIgnoreCase]);
    SubtitleText := StringReplace(SubtitleText, '{\b1}', '', [rfReplaceAll, rfIgnoreCase]);
    SubtitleText := StringReplace(SubtitleText, '{\b0}', '', [rfReplaceAll, rfIgnoreCase]);

    // Construcción del bloque
    StringList.Add('@JUST C');
    if IsItalic then
      StringList.Add('@STYLE 1')
    else
      StringList.Add('@STYLE 0');
      
    // En este formato el texto va identado
    StringList.Add('                    ' + SubtitleText);
    
    StringList.Add('@SHOW ' + 
                   TimeToString(Subtitles.InitialTime[i], 'hh:mm:ss:ff', FPS) + ' ' +
                   TimeToString(Subtitles.FinalTime[i], 'hh:mm:ss:ff', FPS));
  end;

  try
    StringList.SaveToFile(FileName, Encoding);
    Result := True;
  except
  end;
end;

// -----------------------------------------------------------------------------

end.
