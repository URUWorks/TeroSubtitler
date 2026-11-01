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

unit AdvancedSubstationAlpha;

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
    function LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean; override;
    function SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean; override;
  end;

// -----------------------------------------------------------------------------

implementation

uses strUtils, UWSystem.StrUtils, UWSystem.SysUtils, UWSubtitleAPI.Tags,
  UWSubtitleAPI.ExtraInfo;

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
var
  Line, StartTime, EndTime: String;
  P1, P2, P3: Integer;
begin
  Line := SubtitleFile[Row];

  if (not Contains('Dialogue:', Line)) or
     (Pos('Marked=', Line) > 0) or
     (Pos('!effect', Line) > 0) then
    Exit(False);

  P1 := Pos(',', Line);
  if P1 = 0 then Exit(False);

  P2 := PosEx(',', Line, P1 + 1);
  if P2 = 0 then Exit(False);

  P3 := PosEx(',', Line, P2 + 1);
  if P3 = 0 then Exit(False);

  // Extraemos los tiempos basándonos en la posición de las comas
  StartTime := Trim(Copy(Line, P1 + 1, P2 - P1 - 1));
  EndTime := Trim(Copy(Line, P2 + 1, P3 - P2 - 1));

  // Aceptamos máscaras estándar (1 dígito de hora) y variantes de broadcast (2 dígitos)
  Result := (TimeInFormat(StartTime, 'h:mm:ss.zz') or TimeInFormat(StartTime, 'hh:mm:ss.zz')) and
            (TimeInFormat(EndTime, 'h:mm:ss.zz') or TimeInFormat(EndTime, 'hh:mm:ss.zz'));
end;

// -----------------------------------------------------------------------------

function TUWAdvancedSubstationAlpha.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean;
var
  i, a, e     : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Fields      : array[0..8] of String; // layer, start, end, style, actor, marginl, marginr, marginv, effect
  Line, Text  : String;
  Header,
  Footer      : String;
  InEvents    : Boolean;
  SeenEvents  : Boolean;
  UseExtra    : Boolean;
  ExtraInfo   : PSSA_ExtraInfo;
begin
  Result := False;
  Header := '';
  Footer := '';
  InEvents := False;
  SeenEvents := False;
  // when appending to a subtitle with other extra info, it can't be mixed
  UseExtra := Subtitles.ExtraInfoType in [eiNone, eiSSA];
  try
    i := 0;
    while i < SubtitleFile.Count do
    begin
      Line := SubtitleFile[i];
      // the other sections are kept as they are (styles, script info, fonts...)
      if Line.Trim.StartsWith('[') then
      begin
        InEvents := SameText(Line.Trim, '[Events]');
        if InEvents then SeenEvents := True;
      end;

      if not InEvents then
      begin
        if not SeenEvents then
          Header := Header + Line + LineEnding
        else
          Footer := Footer + Line + LineEnding;
      end
      else if Contains('Dialogue:', Line) then
      begin
        // the first 9 fields, the text is the rest (it can have commas)
        Text := Copy(Line, Pos(':', Line) + 1, Length(Line));
        for a := 0 to High(Fields) do
        begin
          e := Pos(',', Text);
          if e = 0 then
          begin
            Fields[a] := '';
            Continue;
          end;
          Fields[a] := Trim(Copy(Text, 1, e - 1));
          Delete(Text, 1, e);
        end;

        InitialTime := StringToTime(Fields[1]);
        FinalTime   := StringToTime(Fields[2]);

        if (InitialTime >= 0) and (FinalTime > 0) then
        begin
          Text := ReplaceString(Trim(Text), '\N', LineEnding, True, True);

          // layer, margins and effect of the file
          ExtraInfo := NIL;
          if UseExtra then
          begin
            New(ExtraInfo);
            FillChar(ExtraInfo^, SizeOf(TSSA_ExtraInfo), 0);
            with ExtraInfo^ do
            begin
              EventType := etDialogue;
              Marked    := False;
              Style     := Fields[3];
              Name      := Fields[4];
              Layer     := StrToIntDef(Fields[0], 0);
              MarginL   := StrToIntDef(Fields[5], 0);
              MarginR   := StrToIntDef(Fields[6], 0);
              MarginV   := StrToIntDef(Fields[7], 0);
              Effect    := Fields[8];
            end;
          end;

          a := Subtitles.Add(InitialTime, FinalTime, Text, '', ExtraInfo);
          with Subtitles.ItemPointer[a]^ do
          begin
            Actor := Fields[4];
            Style := Fields[3];
          end;
        end;
      end;
      Inc(i);
    end;
  finally
    if Subtitles.Count > 0 then
    begin
      Result := True;
      if UseExtra then
        Subtitles.ExtraInfoType := eiSSA;
      // only with styles, otherwise ours are used as before
      if (Subtitles.ASSHeader = '') and (Pos('styles]', LowerCase(Header)) > 0) then
      begin
        Subtitles.ASSHeader := TrimRight(Header);
        Subtitles.ASSFooter := TrimRight(Footer);
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function TUWAdvancedSubstationAlpha.SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  i : Integer;
  Text, s : String;
  sLayer, sMarginL, sMarginR, sMarginV, sEffect : String;
begin
  Result  := False;

  StringList.Clear;

  // the original sections of the file (script info and styles)
  if Subtitles.ASSHeader <> '' then
  begin
    for s in Subtitles.ASSHeader.Split([LineEnding]) do
    begin
      // the loaded lines have no blank ones, a blank line between sections
      if s.StartsWith('[') and (StringList.Count > 0) then
        StringList.Add('');
      StringList.Add(s);
    end;
  end
  else
  begin
    StringList.Add('[Script Info]');
    StringList.Add('ScriptType: v4.00+');
    // temporary track for mpv (Tag = 1): without PlayRes libass uses 384x288,
    // so the style font size would be huge if mpv doesn't override it. 720 is
    // the same scale mpv uses for sub-font-size.
    if Subtitles.Tag = 1 then
    begin
      StringList.Add('PlayResX: 1280');
      StringList.Add('PlayResY: 720');
    end;

  //  StringList.Add('Collisions: ' + ASSAttributes.Collisions);
  //  StringList.Add('PlayResX: ' + IntToStr(ASSAttributes.PlayResX));
  //  StringList.Add('PlayResY: ' + IntToStr(ASSAttributes.PlayResY));
  //  StringList.Add('Timer: ' + ASSAttributes.Timer);
    StringList.Add('');
    StringList.Add('[V4+ Styles]');
    StringList.Add(SSA_StyleFormat);
    StringList.Add('Style: ' + Subtitles.FormatProperties^.SSA.DefaultStyleSettings);
  end;
  StringList.Add('');
  StringList.Add('[Events]');
  StringList.Add(SSA_EventFormat);

  for i := FromItem to ToItem do
  begin
    Text := iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]);
    Text := ReplaceString(Trim(Text), LineEnding, '\N');
    // temporary track for mpv (Tag = 1): show the text between braces that
    // is not an ASS tag, libass would hide it as a comment
    if Subtitles.Tag = 1 then
      Text := EscapeNonTagBraces(Text);

    s := '';
    if Subtitles[i].Align <> shaNone then
    begin
      case Subtitles[i].Align of
        shaLeft : case Subtitles[i].VAlign of
                    svaCenter : s := '{\an4}';
                    svaTop    : s := '{\an7}';
                  else
                    s := '{\an1}';
           end;
        shaCenter : case Subtitles[i].VAlign of
                      svaCenter : s := '{\an5}';
                      svaTop    : s := '{\an8}';
                    else
                      s := '{\an2}';
                    end;
        shaRight : case Subtitles[i].VAlign of
                     svaCenter : s := '{\an6}';
                     svaTop    : s := '{\an9}';
                   else
                     s := '{\an3}';
                   end;
      end;
    end
    else if Subtitles[i].VAlign <> svaBottom then
    begin
      case Subtitles[i].VAlign of
        svaCenter : s := '{\an5}';
        svaTop    : s := '{\an8}';
      else
        s := '{\an2}';
      end;
    end;

    if not s.IsEmpty then
      Text := s + Text + '{\an0}';

    // layer, margins and effect of the file
    if (Subtitles.ExtraInfoType = eiSSA) and (Subtitles.ExtraInfo[i] <> NIL) then
      with PSSA_ExtraInfo(Subtitles.ExtraInfo[i])^ do
      begin
        sLayer   := IntToStr(Layer);
        sMarginL := SysUtils.Format('%.4d', [MarginL]);
        sMarginR := SysUtils.Format('%.4d', [MarginR]);
        sMarginV := SysUtils.Format('%.4d', [MarginV]);
        sEffect  := Effect;
      end
    else
    begin
      sLayer   := '0';
      sMarginL := '0000';
      sMarginR := '0000';
      sMarginV := '0000';
      sEffect  := '';
    end;

    StringList.Add('Dialogue: ' + sLayer + ',' + TimeToString(iff(Subtitles.Tag = 0, Subtitles.InitialTime[i], Subtitles.InitialTime[i] - 1), 'h:mm:ss.zz') + ',' + TimeToString(iff(Subtitles.Tag = 0, Subtitles.FinalTime[i], Subtitles.FinalTime[i] - 1), 'h:mm:ss.zz') + ',' +
      iff(Subtitles[i].Style <> '', Subtitles[i].Style, 'Default') + ',' + Subtitles[i].Actor + ',' + sMarginL + ',' + sMarginR + ',' + sMarginV + ',' + sEffect + ',' + Text);
  end;

  if Subtitles.ASSFooter <> '' then
  begin
    for s in Subtitles.ASSFooter.Split([LineEnding]) do
    begin
      if s.StartsWith('[') and (StringList[StringList.Count-1] <> '') then
        StringList.Add('');
      StringList.Add(s);
    end;
  end;

  try
    StringList.SaveToFile(FileName, Encoding);
    Result := True;
  except
  end;
end;

// -----------------------------------------------------------------------------

end.
