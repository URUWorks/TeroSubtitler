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

unit UWSubtitleAPI.Formats.MacDVDStudioPro;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSystem.StrUtils,
  UWSystem.SysUtils, UWSubtitleAPI.Formats, StrUtils;

type

  { TUWMacDVDStudioPro }

  TUWMacDVDStudioPro = class(TUWSubtitleCustomFormat)
  public
    function Name: String; override;
    function Format: TUWSubtitleFormats; override;
    function Extension: String; override;
    function IsTimeBased: Boolean; override;
    function HasStyleSupport: Boolean; override;
    function IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean; override;
    function LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean; override;
    function SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean; override;
  end;

// -----------------------------------------------------------------------------

implementation

uses UWSubtitleAPI.Tags;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

function StringToHAlign(const AText: String): Integer;
begin
  if AText.ToLower.Contains('left') then
    Result := 1
  else if AText.ToLower.Contains('center') then
    Result := 2
  else if AText.ToLower.Contains('right') then
    Result := 3
  else
    Result := 0;
end;

// -----------------------------------------------------------------------------

function StringToVAlign(const AText: String): Integer;
begin
  if AText.ToLower.Contains('top') then
    Result := 2
  else if AText.ToLower.Contains('center') then
    Result := 1
  else
    Result := 0;
end;

// -----------------------------------------------------------------------------

function HAlignToString(const AValue: Integer): String;
begin
  case AValue of
    1: Result := '$HorzAlign = Left';
    2: Result := '$HorzAlign = Center';
    3: Result := '$HorzAlign = Right';
  else
    Result := '';
  end;
end;

// -----------------------------------------------------------------------------

function VAlignToString(const AValue: Integer): String;
begin
  case AValue of
    0: Result := '$HorzAlign = Bottom';
    1: Result := '$HorzAlign = Center';
    2: Result := '$HorzAlign = Top';
  else
    Result := '';
  end;
end;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

function TUWMacDVDStudioPro.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWMacDVDStudioPro.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfMACDVDStudioPro;
end;

// -----------------------------------------------------------------------------

function TUWMacDVDStudioPro.Extension: String;
begin
  Result := '*.txt';
end;

// -----------------------------------------------------------------------------

function TUWMacDVDStudioPro.IsTimeBased: Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------

function TUWMacDVDStudioPro.HasStyleSupport: Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------

function TUWMacDVDStudioPro.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  //00:00:03:00 00:00:08:00 Text<P>Text
  if (TimeInFormat(Copy(SubtitleFile[Row], 1, 11), 'hh:mm:ss:zz')) and
     (TimeInFormat(Copy(SubtitleFile[Row], 13, 11), 'hh:mm:ss:zz')) and
     (Pos(',', SubtitleFile[Row]) = 12) and
     (StringCount(',', SubtitleFile[Row]) = 2) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWMacDVDStudioPro.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean;
var
  i    : Integer;
  Item : TUWSubtitleItem;
begin
  Result := False;
  try
    ClearSubtitleItem(Item);
    for i := 0 to SubtitleFile.Count-1 do
    begin
      if SubtitleFile[i].StartsWith('$HorzAlign') then
        Item.Align := StringToHAlign(SubtitleFile[i])
      else if SubtitleFile[i].StartsWith('$VertAlign') then
        Item.VAlign := StringToVAlign(SubtitleFile[i]);

      if not TimeInFormat(Copy(SubtitleFile[i], 1, 8), 'hh:mm:ss') then Continue;
      Item.InitialTime := StringToTime(Copy(SubtitleFile[i], 1, 8));
      Item.FinalTime   := StringToTime(Copy(SubtitleFile[i], 13, 8));

      if IsInteger(Copy(SubtitleFile[i], 10, 2)) then
        Item.InitialTime := Item.InitialTime + FramesToTime(StrToInt(Copy(SubtitleFile[i], 10, 2)), FPS);
      if IsInteger(Copy(SubtitleFile[i], 22, 2)) then
        Item.FinalTime := Item.FinalTime + FramesToTime(StrToInt(Copy(SubtitleFile[i], 22, 2)), FPS);

      Item.Text := ReplaceString(Copy(SubtitleFile[i], 25, Length(SubtitleFile[i])), '<P>', LineEnding).Trim;
      Item.Text := ReplaceString(Item.Text, '| ', LineEnding);
      Item.Text := ReplaceString(Item.Text, '|', LineEnding);
      Item.Text := MacDVDTagsToTS(Item.Text);

      if (Item.InitialTime > -1) and (Item.FinalTime > -1) then
      begin
        Subtitles.Add(Item);
        Item.Text := '';
        Item.Translation := ''
      end;
    end;
  finally
    Result := Subtitles.Count > 0;
  end;
end;

// -----------------------------------------------------------------------------

function TUWMacDVDStudioPro.SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  InitialTime : String;
  FinalTime   : String;
  i           : Integer;
  Text        : String;
begin
  Result  := False;

  for i := FromItem to ToItem do
  begin
    //Text := RemoveTSTags(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]));
    Text := TSToMacDVDTags(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]));

    // Time format is hh:mm:ss:ff
    InitialTime := TimeToString(Subtitles[i].InitialTime, 'hh:mm:ss:') +
                   AddChar('0', IntToStr(GetMSecsInFrames(Subtitles[i].InitialTime, FPS)), 2);

    FinalTime := TimeToString(Subtitles[i].FinalTime, 'hh:mm:ss:') +
                 AddChar('0', IntToStr(GetMSecsInFrames(Subtitles[i].FinalTime, FPS)), 2);

    StringList.Add(HAlignToString(Subtitles[i].Align));
    StringList.Add(VAlignToString(Subtitles[i].VAlign));
    StringList.Add(InitialTime + #9 + FinalTime + #9 + ReplaceString(Text, LineEnding, '<P>'));
  end;

  try
    StringList.SaveToFile(FileName, Encoding);
    Result := True;
  except
  end;
end;

// -----------------------------------------------------------------------------

end.
