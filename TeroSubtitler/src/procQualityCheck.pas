{*
 *  URUWorks
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
 *  Copyright (C) 2023 URUWorks, uruworks@gmail.com.
 *}

unit procQualityCheck;

{$mode delphi}
//{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, generics.collections, procConventions;

type

  { TQualityCheck }

  TQualityCheckType    = (qcCPS, qcWPM, qcCPL, qcMaximumLines, qcMinimumDuration, qcMaximumDuration, qcGAP);
  TQualityCheckTypeSet = set of TQualityCheckType;

  TSubtitleCheckItem = record
    Apply       : Boolean;
    Index       : Integer;
    InitialTime,
    FinalTime   : Integer;
    Text,
    Translation : String;
    QC          : TQualityCheckType;
  end;

  TSubtitleCheckItemList = TList<TSubtitleCheckItem>;

function DoQualityCheck(const AProfileName: String; const AProfiles: TProfiles; var AList: TSubtitleCheckItemList; const AFPS: Single): Boolean;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, procSubtitle, UWSystem.StrUtils, UWSystem.TimeUtils;

// -----------------------------------------------------------------------------

function DoQualityCheck(const AProfileName: String; const AProfiles: TProfiles; var AList: TSubtitleCheckItemList; const AFPS: Single): Boolean;

  procedure AddQCItem(const AIndex: Integer; const AQC: TQualityCheckType);
  var
    item: TSubtitleCheckItem;
  begin
    FillByte(item, SizeOf(item), 0);
    item.Index := AIndex;
    item.QC    := AQC;
    AList.Add(item);
  end;

var
  i: Integer;
  Profile: PProfileItem;
begin
  Result := False;
  if (AProfiles = NIL) or (Subtitles.Count = 0) then Exit;

  if not Assigned(AList) then
    AList := TSubtitleCheckItemList.Create;

  AList.Clear;
  Profile := AProfiles.FindProfile(AProfileName);
  if Profile = NIL then
    Profile := @AppOptions.Conventions;

  for i := 0 to Subtitles.Count-1 do
  begin
    if (Profile.MaxCPS > 0) and (Subtitles.TextCPS[i, Profile.CPSLineLenStrategy] > Profile.MaxCPS) then
      AddQCItem(i, qcCPS);

    if (Profile.WPM > 0) and (Subtitles.TextWPM[i] > Profile.WPM) then
      AddQCItem(i, qcWPM);

    if (Profile.CPL > 0) and (GetMaxLinesOf(Subtitles[i].Text) > Profile.CPL) then
      AddQCItem(i, qcCPL);

    if (Profile.MaxLines > 0) and (LineCount(Subtitles[i].Text, sLineBreak) > Profile.MaxLines) then
      AddQCItem(i, qcMaximumLines);

    if (Profile.MinDuration > 0) and (Subtitles.Duration[i] < Profile.MinDuration) then
      AddQCItem(i, qcMinimumDuration)
    else if (Profile.MinDurationPerWord > 0) and (Subtitles[i].Text <> '') and  ((Subtitles.Duration[i] div WordCount(Subtitles[i].Text)) < Profile.MinDurationPerWord) then
      AddQCItem(i, qcMinimumDuration);

    if (Profile.MaxDuration > 0) and (Subtitles.Duration[i] > Profile.MaxDuration) then
      AddQCItem(i, qcMaximumDuration);

    if i <> Subtitles.Count-1 then
    begin
      if Profile.PauseInFrames then
      begin
        if (Subtitles.Pause[i] < FramesToTime(Profile.MinPause, AFPS)) then
          AddQCItem(i, qcGAP);
      end
      else
      begin
        if (Subtitles.Pause[i] < Profile.MinPause) then
          AddQCItem(i, qcGAP);
      end;
    end;
  end;

  Result := True;
end;

// -----------------------------------------------------------------------------

end.

