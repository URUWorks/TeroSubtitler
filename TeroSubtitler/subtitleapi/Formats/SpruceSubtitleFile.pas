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

unit SpruceSubtitleFile;

// -----------------------------------------------------------------------------

interface

uses
  Classes, SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSubtitleAPI.Formats;

type

  { TUWSpruceSubtitleFile }

  TUWSpruceSubtitleFile = class(TUWSubtitleCustomFormat)
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

function TUWSpruceSubtitleFile.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWSpruceSubtitleFile.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfSpruceSubtitleFile;
end;

// -----------------------------------------------------------------------------

function TUWSpruceSubtitleFile.Extension: String;
begin
  Result := '*.stl';
end;

// -----------------------------------------------------------------------------

function TUWSpruceSubtitleFile.HasStyleSupport: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWSpruceSubtitleFile.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
var
  p1, p2: Integer;
  sIni, sFin: String;
  LineText: String;
begin
  Result := False;
  if LowerCase(ExtractFileExt(SubtitleFile.FileName)) <> '.stl' then Exit;

  LineText := Trim(SubtitleFile[Row]);
  if (LineText = '') or LineText.StartsWith('//') or LineText.StartsWith('$') then Exit;
  p1 := Pos(',', LineText);
  if p1 > 0 then
  begin
    p2 := Pos(',', LineText, p1 + 1);
    if p2 > 0 then
    begin
      sIni := Trim(Copy(LineText, 1, p1 - 1));
      sFin := Trim(Copy(LineText, p1 + 1, p2 - p1 - 1));

      if TimeInFormat(sIni, 'hh:mm:ss:ff') and TimeInFormat(sFin, 'hh:mm:ss:ff') then
        Result := True;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function TUWSpruceSubtitleFile.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean;

  function IsFontTagEnabled(const AText: String): Boolean;
  begin
    if AText.ToLower.EndsWith('true') or AText.EndsWith('1') then
      Result := True
    else
      Result := False;
  end;

var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
  Bold,
  Italic,
  UnderLined  : Boolean;
  p1, p2      : Integer;
  sIni, sFin  : String;
  LineText    : String;
  CurrHAlign  : TSubtitleHAlign;
  CurrVAlign  : TSubtitleVAlign;
  PropStr     : String;
  x           : Integer;
begin
  Result := False;
  try
    Bold       := False;
    Italic     := False;
    UnderLined := False;

    CurrHAlign := shaCenter;
    CurrVAlign := svaBottom;

    for i := 0 to SubtitleFile.Count-1 do
    begin
      LineText := Trim(SubtitleFile[i]);
      if LineText.IsEmpty or LineText.StartsWith('//') then Continue; // '//' = comentario

      // Propiedades globales (los comandos no distinguen mayúsculas)
      if LineText.StartsWith('$Bold', True) then
        Bold := IsFontTagEnabled(LineText)
      else if LineText.StartsWith('$Italic', True) then
        Italic := IsFontTagEnabled(LineText)
      else if LineText.StartsWith('$UnderLined', True) then
        UnderLined := IsFontTagEnabled(LineText)
      else if LineText.StartsWith('$HorzAlign', True) then
      begin
        PropStr := Trim(Copy(LineText, Pos('=', LineText) + 1, Length(LineText))).ToLower;
        if PropStr = 'left' then CurrHAlign := shaLeft
        else if PropStr = 'right' then CurrHAlign := shaRight
        else CurrHAlign := shaCenter;
      end
      else if LineText.StartsWith('$VertAlign', True) then
      begin
        PropStr := Trim(Copy(LineText, Pos('=', LineText) + 1, Length(LineText))).ToLower;
        if PropStr = 'top' then CurrVAlign := svaTop
        else if PropStr = 'center' then CurrVAlign := svaCenter
        else CurrVAlign := svaBottom;
      end
      else if LineText.StartsWith('$') then
        Continue // otros comandos ($FontName, $ColorIndex...) no son subtítulos
      else
      begin
        InitialTime := -1;
        FinalTime   := -1;
        Text        := '';

        p1 := Pos(',', LineText);
        if p1 > 0 then
        begin
          p2 := Pos(',', LineText, p1 + 1);
          if p2 > 0 then
          begin
            sIni := Trim(Copy(LineText, 1, p1 - 1));
            sFin := Trim(Copy(LineText, p1 + 1, p2 - p1 - 1));

            if TimeInFormat(sIni, 'hh:mm:ss:ff') and TimeInFormat(sFin, 'hh:mm:ss:ff') then
            begin
              InitialTime := StringToTime(sIni, False, FPS);
              FinalTime   := StringToTime(sFin, False, FPS);
              Text        := Trim(Copy(LineText, p2 + 1, Length(LineText)));
            end;
          end;
        end;

        if (InitialTime >= 0) and (FinalTime > 0) then
        begin
          if Bold       then Text := '{\b1}' + Text + '{\b0}';
          if Italic     then Text := '{\i1}' + Text + '{\i0}';
          if UnderLined then Text := '{\u1}' + Text + '{\u0}';

          Text := STLTagsToTS(ReplaceEnters(Text, '|', sLineBreak));

          x := Subtitles.Add(InitialTime, FinalTime, Text, '', NIL);

          if x >= 0 then
          begin
            Subtitles.ItemPointer[x]^.Align  := CurrHAlign;
            Subtitles.ItemPointer[x]^.VAlign := CurrVAlign;
          end;
        end;
      end;
    end;
  finally
    Result := Subtitles.Count > 0;
  end;
end;

// -----------------------------------------------------------------------------

function TUWSpruceSubtitleFile.SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  i : Integer;
  SubtitleText: String;
  CurrHAlign  : TSubtitleHAlign;
  CurrVAlign  : TSubtitleVAlign;
begin
  Result  := False;

  StringList.Clear;
  StringList.Add('//Font select and font size');
  StringList.Add('$FontName       = Arial');
  StringList.Add('$FontSize       = 30');
  StringList.Add('');
  StringList.Add('//Character attributes (global)');
  StringList.Add('$Bold           = FALSE');
  StringList.Add('$UnderLined     = FALSE');
  StringList.Add('$Italic         = FALSE');
  StringList.Add('');
  StringList.Add('//Position Control');
  StringList.Add('$XOffset        = 0');
  StringList.Add('$YOffset        = 0');
  StringList.Add('');
  StringList.Add('//Contrast Control');
  StringList.Add('$TextContrast           = 15');
  StringList.Add('$Outline1Contrast       = 8');
  StringList.Add('$Outline2Contrast       = 15');
  StringList.Add('$BackgroundContrast     = 0');
  StringList.Add('');
  StringList.Add('//Effects Control');
  StringList.Add('$ForceDisplay   = FALSE');
  StringList.Add('$FadeIn         = 0');
  StringList.Add('$FadeOut        = 0');
  StringList.Add('');
  StringList.Add('//Other Controls');
  StringList.Add('$TapeOffset          = FALSE');
  StringList.Add('//$SetFilePathToken  = <<:>>');
  StringList.Add('');
  StringList.Add('//Colors');
  StringList.Add('$ColorIndex1    = 3');
  StringList.Add('$ColorIndex2    = 1');
  StringList.Add('$ColorIndex3    = 1');
  StringList.Add('$ColorIndex4    = 0');
  StringList.Add('');
  StringList.Add('//Subtitles');

  CurrHAlign := shaCenter;
  CurrVAlign := svaBottom;

  for i := FromItem to ToItem do
  begin
    if Subtitles.Items[i].Align <> CurrHAlign then
    begin
      CurrHAlign := Subtitles.Items[i].Align;
      case CurrHAlign of
        shaLeft  : StringList.Add('$HorzAlign      = Left');
        shaRight : StringList.Add('$HorzAlign      = Right');
        else
          StringList.Add('$HorzAlign      = Center');
      end;
    end;

    if Subtitles.Items[i].VAlign <> CurrVAlign then
    begin
      CurrVAlign := Subtitles.Items[i].VAlign;
      case CurrVAlign of
        svaTop    : StringList.Add('$VertAlign      = Top');
        svaCenter : StringList.Add('$VertAlign      = Center');
        else
          StringList.Add('$VertAlign      = Bottom');
      end;
    end;

    SubtitleText := iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]);
    SubtitleText := ReplaceEnters(SubtitleText);

    // Antes sin el FPS: los cuadros siempre se calculaban a 25 fps
    StringList.Add(TimeToString(Subtitles.InitialTime[i], 'hh:mm:ss:ff', FPS) + ',' +
                   TimeToString(Subtitles.FinalTime[i], 'hh:mm:ss:ff', FPS) + ',' +
                   TSTagsToSTL(SubtitleText));
  end;

  try
    StringList.SaveToFile(FileName, Encoding);
    Result := True;
  except
  end;
end;

// -----------------------------------------------------------------------------

end.
