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
 *  Copyright (C) 2001-2023 URUWorks, uruworks@gmail.com.
 *}

unit UWSubtitleAPI.Formats.SpruceSubtitleFile;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSubtitleAPI.Formats;

type

  { TUWSpruceSubtitleFile }

  TUWSpruceSubtitleFile = class(TUWSubtitleCustomFormat)
  public
    function Name: String; override;
    function Format: TUWSubtitleFormats; override;
    function Extension: String; override;
    function HasStyleSupport: Boolean; override;
    function IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean; override;
    function LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean; override;
    function SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean; override;
  end;

// -----------------------------------------------------------------------------

implementation

uses UWSubtitleAPI.Tags, UWSystem.StrUtils,
  UWSystem.SysUtils;

// -----------------------------------------------------------------------------

function STLTagsToTS(const Text: String): String;

  function ReplaceTag(const sText, Tag: String): String;
  var
    StartTag : Boolean;
    sTag     : String;
  begin
    Result   := sText;
    StartTag := True;
    sTag     := '^' + Tag.ToUpper;
    while Result.Contains(sTag) do
    begin
      if StartTag then
        Result := Result.Replace(sTag, '{\' + Tag + '1}', [rfIgnoreCase])
      else
        Result := Result.Replace(sTag, '{\' + Tag + '0}', [rfIgnoreCase]);

      StartTag := not StartTag;
    end;
  end;

begin
  Result := ReplaceTag(Text, 'b');
  Result := ReplaceTag(Result, 'i');
  Result := ReplaceTag(Result, 'u');
end;

// -----------------------------------------------------------------------------

function TSTagsToSTL(const Text: String): String;
begin
  Result := Text;
  Result := Result.Replace('{\b0}', '^B', [rfReplaceAll]);
  Result := Result.Replace('{\b1}', '^B', [rfReplaceAll]);
  Result := Result.Replace('{\i0}', '^I', [rfReplaceAll]);
  Result := Result.Replace('{\i1}', '^I', [rfReplaceAll]);
  Result := Result.Replace('{\u0}', '^U', [rfReplaceAll]);
  Result := Result.Replace('{\u1}', '^U', [rfReplaceAll]);
  Result := RemoveTSTags(Result);
end;

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
begin
  if (LowerCase(ExtractFileExt(SubtitleFile.FileName)) = '.stl') and
     (((TimeInFormat(Copy(SubtitleFile[Row], 1, 11), 'hh:mm:ss:zz')) and
     (Pos(',', SubtitleFile[Row]) = 13) and
     (TimeInFormat(Copy(SubtitleFile[Row], 15, 11), 'hh:mm:ss:zz'))) or
     ((TimeInFormat(Copy(SubtitleFile[Row], 1, 11), 'hh:mm:ss:zz')) and
     (Pos(',', SubtitleFile[Row]) = 12) and
     (TimeInFormat(Copy(SubtitleFile[Row], 13, 11), 'hh:mm:ss:zz')))) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWSpruceSubtitleFile.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean;

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
begin
  Result := False;
  try
    Bold       := False;
    Italic     := False;
    UnderLined := False;
    for i := 0 to SubtitleFile.Count-1 do
    begin
      if SubtitleFile[i].StartsWith('$Bold') then
        Bold := IsFontTagEnabled(SubtitleFile[i])
      else if SubtitleFile[i].StartsWith('$Italic') then
        Italic := IsFontTagEnabled(SubtitleFile[i])
      else if SubtitleFile[i].StartsWith('$UnderLined') then
        UnderLined := IsFontTagEnabled(SubtitleFile[i]);

      if (TimeInFormat(Copy(SubtitleFile[i], 1, 11), 'hh:mm:ss:zz')) and
         (Pos(',', SubtitleFile[i]) = 13) and
         (TimeInFormat(Copy(SubtitleFile[i], 15, 11), 'hh:mm:ss:zz')) then
      begin
        InitialTime := StringToTime(Copy(SubtitleFile[i], 1, 11));
        FinalTime   := StringToTime(Copy(SubtitleFile[i], 15, 11));
        Text        := Copy(SubtitleFile[i], 29);
      end
      else if (TimeInFormat(Copy(SubtitleFile[i], 1, 11), 'hh:mm:ss:zz')) and
              (Pos(',', SubtitleFile[i]) = 12) and
              (TimeInFormat(Copy(SubtitleFile[i], 13, 11), 'hh:mm:ss:zz')) then
      begin
        InitialTime := StringToTime(Copy(SubtitleFile[i], 1, 11));
        FinalTime   := StringToTime(Copy(SubtitleFile[i], 13, 11));
        Text        := Copy(SubtitleFile[i], 26);
      end
      else
      begin
        InitialTime := -1;
        FinalTime   := -1;
      end;

      if (InitialTime >= 0) and (FinalTime > 0) then
      begin
        if Bold       then Text := '{\b1}' + Text + '{\b0}';
        if Italic     then Text := '{\i1}' + Text + '{\i0}';
        if UnderLined then Text := '{\u1}' + Text + '{\u0}';

        Text := STLTagsToTS(ReplaceEnters(Text, '|', LineEnding));
        Subtitles.Add(InitialTime, FinalTime, Text, '', NIL);
      end;
    end;
  finally
    Result := Subtitles.Count > 0;
  end;
end;

// -----------------------------------------------------------------------------

function TUWSpruceSubtitleFile.SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  i : Integer;
begin
  Result  := False;

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
  StringList.Add('$HorzAlign      = Center');
  StringList.Add('$VertAlign      = Bottom');
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
  StringList.Add('$ColorIndex1    = 0');
  StringList.Add('$ColorIndex2    = 1');
  StringList.Add('$ColorIndex3    = 2');
  StringList.Add('$ColorIndex4    = 3');
  StringList.Add('');
  StringList.Add('//Subtitles');

  for i := FromItem to ToItem do
    StringList.Add(TimeToString(Subtitles.InitialTime[i], 'hh:mm:ss:zz') + ' , ' + TimeToString(Subtitles.FinalTime[i], 'hh:mm:ss:zz') + ' , ' + TSTagsToSTL(ReplaceEnters(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]))));

  try
    StringList.SaveToFile(FileName, Encoding);
    Result := True;
  except
  end;
end;

// -----------------------------------------------------------------------------

end.
