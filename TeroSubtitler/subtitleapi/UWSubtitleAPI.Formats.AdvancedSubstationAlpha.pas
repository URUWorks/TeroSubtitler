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

unit UWSubtitleAPI.Formats.AdvancedSubstationAlpha;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSubtitleAPI.Formats;

type

  { TUWAdvancedSubstationAlpha }

  TUWAdvancedSubstationAlpha = class(TUWSubtitleCustomFormat)
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

uses strUtils, UWSystem.StrUtils, UWSystem.SysUtils;

const
  SSA_EventFormat = 'Format: Layer, Start, End, Style, Actor, MarginL, MarginR, MarginV, Effect, Text';
  SSA_StyleFormat = 'Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, OutlineColour, BackColour, Bold, Italic, Underline, StrikeOut, ScaleX, ScaleY, Spacing, Angle, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, Encoding';

// -----------------------------------------------------------------------------

function TUWAdvancedSubstationAlpha.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWAdvancedSubstationAlpha.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfAdvancedSubstationAlpha;
end;

// -----------------------------------------------------------------------------

function TUWAdvancedSubstationAlpha.Extension: String;
begin
  Result := '*.ass';
end;

// -----------------------------------------------------------------------------

function TUWAdvancedSubstationAlpha.IsTimeBased: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWAdvancedSubstationAlpha.HasStyleSupport: Boolean;
begin
  Result := True; // Has tags
end;

// -----------------------------------------------------------------------------

function TUWAdvancedSubstationAlpha.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  if (Pos('Dialogue:', SubtitleFile[Row]) > 0) and
     (Pos('Marked=', SubtitleFile[Row]) = 0) and
     (Pos('!effect', SubtitleFile[Row]) = 0) and
     (TimeInFormat(Trim(Copy(SubtitleFile[Row], Pos(',', SubtitleFile[Row]) + 1, PosEx(',', SubtitleFile[Row], Pos(',', SubtitleFile[Row]) + 1) - (Pos(',', SubtitleFile[Row]) + 1))), 'h:mm:ss.zz'))  and
     (TimeInFormat(Trim(Copy(SubtitleFile[Row], PosEx(',', SubtitleFile[Row], Pos(',', SubtitleFile[Row]) + 1) + 1, PosEx(',', SubtitleFile[Row], PosEx(',', SubtitleFile[Row], Pos(',', SubtitleFile[Row]) + 1) + 1) - (PosEx(',', SubtitleFile[Row], Pos(',', SubtitleFile[Row]) + 1) + 1))), 'h:mm:ss.zz')) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWAdvancedSubstationAlpha.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean;
var
  i, a        : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text, Actor : String;
begin
  Result := False;
  try
    i := 0;
    while i < SubtitleFile.Count do
    begin
      if Contains('Dialogue:', SubtitleFile[i]) then
      begin
        a := Pos(',', SubtitleFile[i]);
        InitialTime := StringToTime(Trim(Copy(SubtitleFile[i], a + 1, PosEx(',', SubtitleFile[i], a + 1) - (a + 1))));
        a := PosEx(',', SubtitleFile[i], a + 1);
        FinalTime   := StringToTime(Trim(Copy(SubtitleFile[i], a + 1, PosEx(',', SubtitleFile[i], a + 1) - (a + 1))));

        if (InitialTime >= 0) and (FinalTime > 0) then
        begin
          Text := SubtitleFile[i];
          for a := 1 to 4 do Delete(Text, 1, Pos(',', Text));
          Actor := Copy(Text, 1, Pos(',', Text)-1);
          for a := 1 to 5 do Delete(Text, 1, Pos(',', Text));

          Text := ReplaceString(Trim(Text), '\N', LineEnding);

          a := Subtitles.Add(InitialTime, FinalTime, Text, '', NIL, False);
          Subtitles.ItemPointer[a]^.Actor := Actor;
        end;
      end;
      Inc(i);
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

// -----------------------------------------------------------------------------

function TUWAdvancedSubstationAlpha.SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  i : Integer;
  Text, s : String;
begin
  Result  := False;

  StringList.Add('[Script Info]', False);
  StringList.Add('ScriptType: v4.00+', False);
//  StringList.Add('Collisions: ' + ASSAttributes.Collisions, False);
//  StringList.Add('PlayResX: ' + IntToStr(ASSAttributes.PlayResX), False);
//  StringList.Add('PlayResY: ' + IntToStr(ASSAttributes.PlayResY), False);
//  StringList.Add('Timer: ' + ASSAttributes.Timer, False);
  StringList.Add('', False);
  StringList.Add('[V4+ Styles]', False);
  StringList.Add(SSA_StyleFormat, False);
  StringList.Add('Style: ' + Subtitles.FormatProperties^.SSA.DefaultStyleSettings, False);
  StringList.Add('', False);
  StringList.Add('[Events]', False);
  StringList.Add(SSA_EventFormat, False);

  for i := FromItem to ToItem do
  begin
    Text := iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]);
    Text := ReplaceString(Trim(Text), LineEnding, '\N');

    s := '';
    if Subtitles[i].Align <> 0 then
    begin
      case Subtitles[i].Align of
        1: case Subtitles[i].VAlign of
             1 : s := '{\an4}';
             2 : s := '{\an7}';
           else
             s := '{\an1}';
           end;
        2: case Subtitles[i].VAlign of
             1 : s := '{\an5}';
             2 : s := '{\an8}';
           else
             s := '{\an2}';
           end;
        3: case Subtitles[i].VAlign of
             1 : s := '{\an6}';
             2 : s := '{\an9}';
           else
             s := '{\an3}';
           end;
      end;
    end
    else if Subtitles[i].VAlign <> 0 then
    begin
      case Subtitles[i].VAlign of
        1 : s := '{\an5}';
        2 : s := '{\an8}';
      else
        s := '{\an2}';
      end;
    end;

    if not s.IsEmpty then
      Text := s + Text + '{\an0}';

    StringList.Add('Dialogue: 0,' + TimeToString(Subtitles.InitialTime[i], 'h:mm:ss.zz') + ',' + TimeToString(iff(Subtitles.Tag = 0, Subtitles.FinalTime[i], Subtitles.FinalTime[i] - 1), 'h:mm:ss.zz') + ',Default,' + Subtitles[i].Actor + ',0000,0000,0000,,' + Text, False);
  end;

  try
    StringList.SaveToFile(FileName, Encoding);
    Result := True;
  except
  end;
end;

// -----------------------------------------------------------------------------

end.
