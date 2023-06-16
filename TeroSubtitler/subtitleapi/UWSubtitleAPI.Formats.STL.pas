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

unit UWSubtitleAPI.Formats.STL;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSubtitleAPI.Formats;

type

  { TUWSTL }

  TUWSTL = class(TUWSubtitleCustomFormat)
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

function STLTagsToSW(const Text: String): String;

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

function SWTagsToSTL(const Text: String): String;
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

function TUWSTL.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWSTL.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfSTL;
end;

// -----------------------------------------------------------------------------

function TUWSTL.Extension: String;
begin
  Result := '*.stl';
end;

// -----------------------------------------------------------------------------

function TUWSTL.HasStyleSupport: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWSTL.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
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

function TUWSTL.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
begin
  Result := False;
  try
    for i := 0 to SubtitleFile.Count-1 do
    begin
      if (TimeInFormat(Copy(SubtitleFile[i], 1, 11), 'hh:mm:ss:zz')) and
         (Pos(',', SubtitleFile[i]) = 13) and
         (TimeInFormat(Copy(SubtitleFile[i], 15, 11), 'hh:mm:ss:zz')) then
      begin
        InitialTime := StringToTime(Copy(SubtitleFile[i], 1, 11));
        FinalTime   := StringToTime(Copy(SubtitleFile[i], 15, 11));
        Text        := Copy(SubtitleFile[i], 29, Length(SubtitleFile[i])-28);
      end
      else if (TimeInFormat(Copy(SubtitleFile[i], 1, 11), 'hh:mm:ss:zz')) and
              (Pos(',', SubtitleFile[i]) = 12) and
              (TimeInFormat(Copy(SubtitleFile[i], 13, 11), 'hh:mm:ss:zz')) then
      begin
        InitialTime := StringToTime(Copy(SubtitleFile[i], 1, 11));
        FinalTime   := StringToTime(Copy(SubtitleFile[i], 13, 11));
        Text        := Copy(SubtitleFile[i], 25, Length(SubtitleFile[i])-24);
      end
      else
      begin
        InitialTime := -1;
        FinalTime   := -1;
      end;

      if (InitialTime >= 0) and (FinalTime > 0) then
      begin
        Text := STLTagsToSW(ReplaceEnters(Text, '|', LineEnding));
        Subtitles.Add(InitialTime, FinalTime, Text, '', NIL, False);
      end;
    end;
  finally
    Result := Subtitles.Count > 0;
  end;
end;

// -----------------------------------------------------------------------------

function TUWSTL.SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  i : Integer;
begin
  Result  := False;

  StringList.Add('//Font select and font size');
  StringList.Add('$FontName       = Arial');
  StringList.Add('$FontSize       = 30');
  StringList.Add('', False);
  StringList.Add('//Character attributes (global)');
  StringList.Add('$Bold           = FALSE');
  StringList.Add('$UnderLined     = FALSE');
  StringList.Add('$Italic         = FALSE');
  StringList.Add('', False);
  StringList.Add('//Position Control');
  StringList.Add('$HorzAlign      = Center');
  StringList.Add('$VertAlign      = Bottom');
  StringList.Add('$XOffset        = 0');
  StringList.Add('$YOffset        = 0');
  StringList.Add('', False);
  StringList.Add('//Contrast Control');
  StringList.Add('$TextContrast           = 15');
  StringList.Add('$Outline1Contrast       = 8');
  StringList.Add('$Outline2Contrast       = 15');
  StringList.Add('$BackgroundContrast     = 0');
  StringList.Add('', False);
  StringList.Add('//Effects Control');
  StringList.Add('$ForceDisplay   = FALSE');
  StringList.Add('$FadeIn         = 0');
  StringList.Add('$FadeOut        = 0');
  StringList.Add('', False);
  StringList.Add('//Other Controls');
  StringList.Add('$TapeOffset          = FALSE');
  StringList.Add('//$SetFilePathToken  = <<:>>');
  StringList.Add('', False);
  StringList.Add('//Colors');
  StringList.Add('$ColorIndex1    = 0');
  StringList.Add('$ColorIndex2    = 1');
  StringList.Add('$ColorIndex3    = 2');
  StringList.Add('$ColorIndex4    = 3');
  StringList.Add('', False);
  StringList.Add('//Subtitles');

  for i := FromItem to ToItem do
    StringList.Add(TimeToString(Subtitles.InitialTime[i], 'hh:mm:ss:zz') + ' , ' + TimeToString(Subtitles.FinalTime[i], 'hh:mm:ss:zz') + ' , ' + SWTagsToSTL(ReplaceEnters(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]))));

  try
    StringList.SaveToFile(FileName, Encoding);
    Result := True;
  except
  end;
end;

// -----------------------------------------------------------------------------

end.
