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

unit IAuthor;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, UWSubtitleAPI, UWSystem.StrUtils, UWSystem.SysUtils,
  UWSubtitleAPI.Formats;

type

  { TUWIAuthor }

  TUWIAuthor = class(TUWSubtitleCustomFormat)
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

uses UWSubtitleAPI.Tags;

// -----------------------------------------------------------------------------

function TUWIAuthor.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWIAuthor.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfIAuthorScript;
end;

// -----------------------------------------------------------------------------

function TUWIAuthor.Extension: String;
begin
  Result := '*.txt';
end;

// -----------------------------------------------------------------------------

function TUWIAuthor.HasStyleSupport: Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------

function TUWIAuthor.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  if (Pos('BMPFILE:', SubtitleFile[Row]) = 1) or
     (Pos('TIME:', SubtitleFile[Row]) = 1) or
     (Pos('STARTTIME:', SubtitleFile[Row]) = 1) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWIAuthor.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean;
var
  i, c, u     : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  DecimalSep  : Char;
  SecsStart, SecsEnd: Double;
  EndStr      : String;
  Text        : String;
begin
  Result := False;

  c := 0;
  u := 0;

  with FormatSettings do
  begin
    DecimalSep       := DecimalSeparator;
    DecimalSeparator := '.';
  end;

  try
    for i := SubtitleFile.Count-1 downto 0 do
    begin
      if (Pos('BMPFILE:', SubtitleFile[i]) = 0) and
         ((Pos('TIME:', SubtitleFile[i]) = 1) and ((Pos('DISABLE_OGT', SubtitleFile[i]) = 0))) or
         (Pos('*', SubtitleFile[i]) = 1) then
        SubtitleFile.Delete(i);
    end;

    for i := 0 to SubtitleFile.Count-1 do
    begin
      if (Pos('BMPFILE:', SubtitleFile[i]) = 1) then
      begin
        Text := Trim(Copy(SubtitleFile[i], 9));
        if (i+1 < SubtitleFile.Count) and (Pos('STARTTIME:', SubtitleFile[i+1]) = 1) then
        begin
          // SaveSubtitle escribe el tiempo módulo 256 s, así que al cargar hay
          // que sumar 256*c segundos (c = veces que el tiempo "envolvió").
          // Antes: (1) con '>' dos cues en el MISMO instante contaban como
          // envoltura falsa, (2) el cue que dispara la envoltura quedaba 256 s
          // corto (se calculaba con el c viejo) y (3) el fin se leía con
          // Copy(s, 6, Pos(' ', s)), que con 'TIME: 12.34 DISABLE_OGT' incluía
          // el resto de la línea y StrToFloat lanzaba excepción.
          SecsStart := StrToFloatDef(Trim(Copy(SubtitleFile[i+1], 11, MaxInt)), -1);
          if SecsStart >= 0 then
          begin
            InitialTime := Round((SecsStart + 256.0 * c) * 1000);
            if InitialTime < u then
            begin
              Inc(c);
              InitialTime := Round((SecsStart + 256.0 * c) * 1000);
            end;
            u := InitialTime;

            FinalTime := InitialTime + 2000;
            if (i+2 < SubtitleFile.Count) and (Pos('TIME:', SubtitleFile[i+2]) = 1) then
            begin
              EndStr := Trim(Copy(SubtitleFile[i+2], 6, MaxInt));
              if Pos(' ', EndStr) > 0 then
                EndStr := Copy(EndStr, 1, Pos(' ', EndStr) - 1);
              SecsEnd := StrToFloatDef(EndStr, -1);
              if SecsEnd >= 0 then
              begin
                FinalTime := Round((SecsEnd + 256.0 * c) * 1000);
                // el final puede haber envuelto justo después del inicio
                if FinalTime <= InitialTime then
                  FinalTime := Round((SecsEnd + 256.0 * (c + 1)) * 1000);
              end;
            end;

            if (InitialTime >= 0) and (FinalTime > 0) then
              Subtitles.Add(InitialTime, FinalTime, Text, '');
          end;
        end;
      end;
    end;
  finally
    Result := Subtitles.Count > 0;
    with FormatSettings do DecimalSeparator := DecimalSep;
  end;
end;

// -----------------------------------------------------------------------------

function TUWIAuthor.SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  sTime      : Single;
  i, c       : Integer;
  tmpNum     : String;
  DecimalSep : Char;
  Txt        : String;
begin
  Result  := False;

  with FormatSettings do
  begin
    DecimalSep       := DecimalSeparator;
    DecimalSeparator := '.';
  end;
  c := 1;

  for i := FromItem to ToItem do
  begin
    // Antes: Subtitles.Text[i] := ... — se modificaba el documento ABIERTO al
    // guardar (sin undo) y además se escribía el texto con tags.
    Txt := RemoveTSTags(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]));

    sTime := Subtitles.InitialTime[i] / 1000;
    if sTime > (256*c) Then inc(c);

    sTime  := sTime - (256*(c-1));
    tmpNum := LimitDecimals(sTime, 2);

    if Pos('.',tmpNum) = 0 then
      tmpNum := tmpNum + '.00';
    if Length(Copy(tmpNum,Pos('.',tmpNum)+1,Length(tmpNum))) < 2 then
      tmpNum := tmpNum + '0';

    StringList.Add('BMPFILE: ' + ReplaceEnters(Txt, LineEnding, ' '));
    StringList.Add('');
    StringList.Add('STARTTIME: ' + tmpNum);
    StringList.Add('');
    StringList.Add(SysUtils.Format('TIME: %s SETCOLOR Primary 0, 16, 128, 128',[tmpNum]));
    StringList.Add('');
    StringList.Add(SysUtils.Format('TIME: %s SETCOLOR Primary 1, 234, 128, 128',[tmpNum]));
    StringList.Add('');
    StringList.Add(SysUtils.Format('TIME: %s SETCOLOR Primary 2, 16, 128, 128',[tmpNum]));
    StringList.Add('');
    StringList.Add(SysUtils.Format('TIME: %s SETCOLOR Primary 3, 125, 128, 128',[tmpNum]));
    StringList.Add('');
    StringList.Add(SysUtils.Format('TIME: %s SETCOLOR Highlight 0, 16, 128, 128',[tmpNum]));
    StringList.Add('');
    StringList.Add(SysUtils.Format('TIME: %s SETCOLOR Highlight 1, 209, 146, 17',[tmpNum]));
    StringList.Add('');
    StringList.Add(SysUtils.Format('TIME: %s SETCOLOR Highlight 2, 81, 239, 91',[tmpNum]));
    StringList.Add('');
    StringList.Add(SysUtils.Format('TIME: %s SETCOLOR Highlight 3, 144, 35, 54',[tmpNum]));
    StringList.Add('');
    StringList.Add(SysUtils.Format('TIME: %s region 207, 170 to 432, 190',[tmpNum]));
    StringList.Add('');
    StringList.Add(SysUtils.Format('TIME: %s SETBLEND Primary 0, 15, 15, 15',[tmpNum]));
    StringList.Add('');
    StringList.Add(SysUtils.Format('TIME: %s SETBLEND Hightlight 0, 15, 15, 15',[tmpNum]));
    StringList.Add('');
    StringList.Add(SysUtils.Format('TIME: %s FIELDINDEX 0, 1',[tmpNum]));
    StringList.Add('');
    StringList.Add(SysUtils.Format('TIME: %s ENABLE_OGT',[tmpNum]));
    StringList.Add('');

    sTime := Subtitles.FinalTime[i] / 1000;
    sTime := sTime - (256*(c-1));

    tmpNum := LimitDecimals(sTime, 2);

    if Pos('.',tmpNum) = 0 then
      tmpNum := tmpNum + '.00';
    if Length(Copy(tmpNum,Pos('.',tmpNum)+1,Length(tmpNum))) < 2 then
      tmpNum := tmpNum + '0';

    StringList.Add(SysUtils.Format('TIME: %s DISABLE_OGT',[tmpNum]));
    StringList.Add('');
    StringList.Add('************ Page #' + IntToStr(i+1) + ' Finished ***************');
    StringList.Add('');
  end;

  try
    with FormatSettings do DecimalSeparator := DecimalSep;
    StringList.SaveToFile(FileName, Encoding);
    Result := True;
  except
  end;
end;

// -----------------------------------------------------------------------------

end.
