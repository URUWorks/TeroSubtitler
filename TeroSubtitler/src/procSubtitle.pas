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

unit procSubtitle;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, StdCtrls, SysUtils, Graphics, LazUTF8, procTypes, laz.VirtualTrees,
  UWSubtitleAPI, UWMemo;

procedure CopyToClipboard(const ACut: Boolean = False);
procedure PasteFromClipboard;

function InsertSubtitle(const Index: Integer; const Item: TUWSubtitleItem; const AutoIncrementUndo: Boolean = True; const AUpdate: Boolean = True): Integer; overload;
function InsertSubtitle(const Index: Integer; const InitialTime, FinalTime: Integer; const Text, Translation: String; const AutoIncrementUndo: Boolean = True; const AUpdate: Boolean = True): Integer; overload;
procedure DeleteSubtitle(const Index: Integer; const AUpdate: Boolean = True; const AutoIncrementUndo: Boolean = True);

procedure SubtitleChanged(const AText, ATranslation: Boolean);
procedure SubtitleChangedReset(const ASubtitleMode: TSubtitleMode);
procedure SetSubtitleTimes(const Index: Integer; const AInitialTime, AFinalTime: Integer; const AUpdate: Boolean = True; const AutoIncrementUndo: Boolean = True);
procedure SetSubtitleTime(const Index: Integer; const Time: Integer; const Tag: Integer; const AUpdate: Boolean = True; const AutoIncrementUndo: Boolean = True);
procedure SetSubtitleText(const Index: Integer; const Text: String; const SubtitleMode: TSubtitleMode = smText; const AUpdate: Boolean = True; const AutoIncrementUndo: Boolean = True; const Resent: Boolean = True);
procedure SetSubtitleTexts(const Index: Integer; const Text: String; const Translation: String; const AUpdate: Boolean = True; const AutoIncrementUndo: Boolean = True; const Resent: Boolean = True);
procedure SetSubtitleValues(const Index: Integer; const AInitialTime, AFinalTime: Integer; const Text: String; const AUpdate: Boolean = True; const AutoIncrementUndo: Boolean = True);

function GetCorrectTime(const ATime: Integer; AIsFrames: Boolean): Integer;
function GetTimeStr(const Time: Integer; const Trim: Boolean = False): String;
function GetInitialTimeStr(const Index: Integer; const Trim: Boolean = False): String;
function GetFinalTimeStr(const Index: Integer; const Trim: Boolean = False): String;
function GetDurationTimeStr(const Index: Integer; const Trim: Boolean = False): String;
function GetPauseTimeStr(const Index: Integer; const Trim: Boolean = False): String;
function GetSubtitleText(const Index: Integer; const SubtitleMode: TSubtitleMode = smText): String;

function GetMaxLinesOf(Text: String; const Separator: String = sLineBreak): Integer;
function GetLengthForEachLine(Text: String; const Separator: String = sLineBreak; const LastSeparator: String = sLineBreak): String;
procedure SelectSubtitleAndFocusMemo(const NextSibiling: Boolean; const WaveToo: Boolean = False);
function GetSubtitleIndexAtTime(const MSecs: Cardinal): Integer;
function GetSubtitleTextAtTime(const MSecs: Cardinal): String;

procedure InsertMemoText(const AText: String);

procedure SetTextTag(const Tag: String);
procedure SetTextCustomTag(const Tag: String);
procedure SetTextTagColor(const HexColor: String);

procedure SetAlignTo(const ATag: Integer);
procedure SetVAlignTo(const ATag: Integer);

function SetEndCueOneFrame(const AFinalTime: Integer; const ASubtract: Boolean = False): Integer;

function GetSubtitleMarkedCount: Integer;

// -----------------------------------------------------------------------------

implementation

uses
  formMain, procVST, procUndo, procVST_Loops, procWorkspace, procFixSubtitles,
  UWSystem.Encoding, UWSystem.TimeUtils, UWSystem.StrUtils, procFiles, procMPV,
  UWSubtitleAPI.Tags, UWSubtitleAPI.Formats, procTranscription;

// -----------------------------------------------------------------------------


procedure CopyToClipboard(const ACut: Boolean = False);
var
  Memo: TUWMemo;
begin
  if frmMain.VST.Focused then
    VSTCopySubtitlesToClipboard(frmMain.VST, ACut)
  else
  begin
    Memo := GetMemoFocused;
    if Memo <> NIL then
    begin
      if ACut then
        Memo.CutToClipboard
      else
        Memo.CopyToClipboard;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure PasteFromClipboard;
var
  Memo: TUWMemo;
begin
  if frmMain.VST.Focused then
    VSTPasteSubtitlesFromClipboard(frmMain.VST)
  else
  begin
    Memo := GetMemoFocused;
    if Memo <> NIL then
      Memo.PasteFromClipboard;
  end;
end;

// -----------------------------------------------------------------------------

function InsertSubtitle(const Index: Integer; const Item: TUWSubtitleItem; const AutoIncrementUndo: Boolean = True; const AUpdate: Boolean = True): Integer;
begin
  Result := Index;
  if Subtitles.ValidIndex(Result) then
    Subtitles.Insert(Result, Item, NIL)
  else
    Result := Subtitles.Add(Item, NIL);

  UndoInstance.AddUndo(utInsertLine, Result, Item, AutoIncrementUndo);

  frmMain.VST.RootNodeCount := Subtitles.Count;
  if AUpdate then
  begin
    UpdateValues(True);
    SubtitleChanged(True, True);
  end;
end;

// -----------------------------------------------------------------------------

function InsertSubtitle(const Index: Integer; const InitialTime, FinalTime: Integer; const Text, Translation: String; const AutoIncrementUndo: Boolean = True; const AUpdate: Boolean = True): Integer;
var
  Item: TUWSubtitleItem;
begin
  ClearSubtitleItem(Item);
  Item.InitialTime := InitialTime;
  Item.FinalTime   := FinalTime;
  Item.Text        := Text;
  Item.Translation := Translation;

  Result := InsertSubtitle(Index, Item, AutoIncrementUndo, AUpdate);
end;

// -----------------------------------------------------------------------------

procedure DeleteSubtitle(const Index: Integer; const AUpdate: Boolean = True; const AutoIncrementUndo: Boolean = True);
begin
  if not Subtitles.ValidIndex(Index) then Exit;
  UndoInstance.AddUndo(utDeleteLine, Index, Subtitles[Index], AutoIncrementUndo);

  Subtitles.Delete(Index);
  frmMain.VST.RootNodeCount := Subtitles.Count;
  if AUpdate then
  begin
    UpdateValues(True);
    SubtitleChanged(True, True);
  end;
end;

// -----------------------------------------------------------------------------

procedure SubtitleChanged(const AText, ATranslation: Boolean);
begin
  with SubtitleInfo do
  begin
    if AText        then Text.Changed        := True;
    if ATranslation then Translation.Changed := True;
  end;

  if MPVOptions.SubtitleHandleByMPV then
  begin
    if MPVSaveSubtitleTempTrack then
      MPVReloadSubtitleTempTrack;
  end;
end;

// -----------------------------------------------------------------------------

procedure SubtitleChangedReset(const ASubtitleMode: TSubtitleMode);
begin
  if ASubtitleMode = smText then
    SubtitleInfo.Text.Changed := False
  else
    SubtitleInfo.Translation.Changed := False;
end;

// -----------------------------------------------------------------------------

procedure SetSubtitleTimes(const Index: Integer; const AInitialTime, AFinalTime: Integer; const AUpdate: Boolean = True; const AutoIncrementUndo: Boolean = True);
begin
  UndoInstance.AddUndo(utSubtitleChange, Index, Subtitles[Index], AutoIncrementUndo);

  Subtitles.InitialTime[Index] := AInitialTime;
  Subtitles.FinalTime[Index]   := AFinalTime;

  if AUpdate then
  begin
    SubtitleChanged(True, True);
    if AppOptions.AutoCheckErrors then Subtitles.ItemPointer[Index]^.ErrorType := CheckErrors(Subtitles, Index, AppOptions.CommonErrors - [etOCR], AppOptions.Conventions, [cmTimes]);
    UpdateCPSAndTexts(Index);
    if frmMain.VST.SelectedCount = 1 then frmMain.VST.Invalidate;
  end;
end;

// -----------------------------------------------------------------------------

procedure SetSubtitleTime(const Index: Integer; const Time: Integer; const Tag: Integer; const AUpdate: Boolean = True; const AutoIncrementUndo: Boolean = True);
begin
  UndoInstance.AddUndo(utSubtitleChange, Index, Subtitles[Index], AutoIncrementUndo);

  case Tag of
    TAG_CONTROL_INITIALTIME : Subtitles.InitialTime[Index] := Time;
    TAG_CONTROL_FINALTIME   : begin
                                Subtitles.FinalTime[Index] := Time;
                                if AppOptions.AutoCheckErrors and (Subtitles.ValidIndex(Index+1)) then
                                  CheckErrors(Subtitles, Index+1, AppOptions.CommonErrors - [etOCR], AppOptions.Conventions, [cmTimes]);
                      end;
    TAG_CONTROL_DURATION    : Subtitles.Duration[Index] := Time;
    TAG_CONTROL_PAUSE       : Subtitles.Pause[Index]    := Time;
  end;

  case Tag of
    TAG_CONTROL_INITIALTIME, TAG_CONTROL_FINALTIME:
    begin
      frmMain.tedDuration.SetValueOnly(Subtitles.Duration[Index]);
      frmMain.tedPause.SetValueOnly(Subtitles.Pause[Index]);
    end;
  end;

  if AUpdate then
  begin
    SubtitleChanged(True, True);
    if AppOptions.AutoCheckErrors then Subtitles.ItemPointer[Index]^.ErrorType := CheckErrors(Subtitles, Index, AppOptions.CommonErrors - [etOCR], AppOptions.Conventions, [cmTimes]);
    UpdateCPSAndTexts(Index);
    if frmMain.VST.SelectedCount = 1 then frmMain.VST.Invalidate;
  end;
end;

// -----------------------------------------------------------------------------

procedure SetSubtitleText(const Index: Integer; const Text: String; const SubtitleMode: TSubtitleMode = smText; const AUpdate: Boolean = True; const AutoIncrementUndo: Boolean = True; const Resent: Boolean = True);
begin
  if Resent then
    UndoInstance.AddUndoIfNotResent(utSubtitleChange, Index, Subtitles[Index], AutoIncrementUndo)
  else
    UndoInstance.AddUndo(utSubtitleChange, Index, Subtitles[Index], AutoIncrementUndo);

  if SubtitleMode = smText then
    Subtitles.Text[Index] := Text
  else
    Subtitles.Translation[Index] := Text;

  if AUpdate then
  begin
    SubtitleChanged(SubtitleMode = smText, SubtitleMode = smTranslation);
    if AppOptions.AutoCheckErrors then Subtitles.ItemPointer[Index]^.ErrorType := CheckErrors(Subtitles, Index, AppOptions.CommonErrors - [etOCR], AppOptions.Conventions, [cmTexts]);
    UpdateCPSAndTexts(Index);
    frmMain.VST.Invalidate;
  end;
end;

// -----------------------------------------------------------------------------

procedure SetSubtitleTexts(const Index: Integer; const Text: String; const Translation: String; const AUpdate: Boolean = True; const AutoIncrementUndo: Boolean = True; const Resent: Boolean = True);
begin
  if Resent then
    UndoInstance.AddUndoIfNotResent(utSubtitleChange, Index, Subtitles[Index], AutoIncrementUndo)
  else
    UndoInstance.AddUndo(utSubtitleChange, Index, Subtitles[Index], AutoIncrementUndo);

  Subtitles.Text[Index]        := Text;
  Subtitles.Translation[Index] := Translation;

  if AUpdate then
  begin
    SubtitleChanged(True, True);
    if AppOptions.AutoCheckErrors then Subtitles.ItemPointer[Index]^.ErrorType := CheckErrors(Subtitles, Index, AppOptions.CommonErrors - [etOCR], AppOptions.Conventions, [cmTexts]);
    UpdateCPSAndTexts(Index);
    frmMain.VST.Invalidate;
  end;
end;

// -----------------------------------------------------------------------------

procedure SetSubtitleValues(const Index: Integer; const AInitialTime, AFinalTime: Integer; const Text: String; const AUpdate: Boolean = True; const AutoIncrementUndo: Boolean = True);
begin
  UndoInstance.AddUndo(utSubtitleChange, Index, Subtitles[Index], AutoIncrementUndo);

  Subtitles.InitialTime[Index] := AInitialTime;
  Subtitles.FinalTime[Index]   := AFinalTime;
  Subtitles.Text[Index]        := Text;

  if AUpdate then
  begin
    if AppOptions.AutoCheckErrors then Subtitles.ItemPointer[Index]^.ErrorType := CheckErrors(Subtitles, Index, AppOptions.CommonErrors - [etOCR], AppOptions.Conventions, [cmTexts, cmTimes]);
    UpdateCPSAndTexts(Index);
    frmMain.VST.Invalidate;
  end;
end;

// -----------------------------------------------------------------------------

function GetCorrectTime(const ATime: Integer; AIsFrames: Boolean): Integer;
begin
  if AIsFrames then
  begin
    Result := FramesToTime(ATime, Workspace.FPS.OutputFPS);
  end
  else
    Result := ATime;
end;

// -----------------------------------------------------------------------------

function GetTimeStr(const Time: Integer; const Trim: Boolean = False): String;
begin
  if Workspace.WorkMode = wmTime then
    Result := TimeToString(Time, DefTimeFormat, Workspace.FPS.OutputFPS)
  else
    Result := TimeToString(Time, DefFramesFormat, Workspace.FPS.OutputFPS);

  if Trim then Result := TrimTimeString(Result);
end;

// -----------------------------------------------------------------------------

function GetInitialTimeStr(const Index: Integer; const Trim: Boolean = False): String;
begin
  Result := GetTimeStr(Subtitles[Index].InitialTime, Trim);
end;

// -----------------------------------------------------------------------------

function GetFinalTimeStr(const Index: Integer; const Trim: Boolean = False): String;
begin
  Result := GetTimeStr(Subtitles[Index].FinalTime, Trim);
end;

// -----------------------------------------------------------------------------

function GetDurationTimeStr(const Index: Integer; const Trim: Boolean = False): String;
begin
  Result := GetTimeStr(Subtitles.Duration[Index], Trim);
end;

// -----------------------------------------------------------------------------

function GetPauseTimeStr(const Index: Integer; const Trim: Boolean = False): String;
begin
  Result := GetTimeStr(Subtitles.Pause[Index], Trim);
end;

// -----------------------------------------------------------------------------

function GetSubtitleText(const Index: Integer; const SubtitleMode: TSubtitleMode = smText): String;
begin
  if SubtitleMode = smText then
    Result := Subtitles[Index].Text
  else
    Result := Subtitles[Index].Translation;
end;

// -----------------------------------------------------------------------------

function GetMaxLinesOf(Text: String; const Separator: String = sLineBreak): Integer;
var
  l, PosEnter: Integer;
begin
  Result   := 0;
  Text     := RemoveTSTags(Text);
  PosEnter := Pos(sLineBreak, Text);
  if PosEnter > 0 then
  begin
    while PosEnter > 0 do
    begin
      l := UTF8Length(Copy(Text, 1, PosEnter-1));
      if Result < l then
        Result := l;

      Text     := Copy(Text, PosEnter + 2, UTF8Length(Text));
      PosEnter := Pos(sLineBreak, Text);
    end;
  end
  else
    Result := UTF8Length(Text);
end;

// -----------------------------------------------------------------------------

function GetLengthForEachLine(Text: String; const Separator: String = sLineBreak; const LastSeparator: String = sLineBreak): String;
var
  TotLen   : Integer;
  BreakLen : Integer;
  PosEnter : Integer;
begin
  Result   := '';
  Text     := RemoveTSTags(Text);
  BreakLen := Length(sLineBreak);
  TotLen   := UTF8Length(Text) - (StringCount(sLineBreak, Text) * BreakLen);
  PosEnter := Pos(sLineBreak, Text);
  if PosEnter > 0 then
  begin
    while PosEnter > 0 do
    begin
      Result   := Result + IntToStr(UTF8Length(Copy(Text, 1, PosEnter-1))) + Separator;
      Text     := Copy(Text, PosEnter + BreakLen, UTF8Length(Text));
      PosEnter := Pos(sLineBreak, Text);
    end;
    Result := Result + IntToStr(UTF8Length(Text));
    if LastSeparator <> '' then
      Result := Result + LastSeparator + IntToStr(TotLen);
  end
  else
    Result := IntToStr(UTF8Length(Text));
end;

// -----------------------------------------------------------------------------

procedure SelectSubtitleAndFocusMemo(const NextSibiling: Boolean; const WaveToo: Boolean = False);
begin
  with frmMain do
  begin
    if NextSibiling then
    begin
      if (VSTFocusedNode(VST) < Subtitles.Count) then VSTSelectNode(VST, VSTFocusedNode(VST)+1, True);
    end
    else
    begin
      if (VSTFocusedNode(VST) > 0) then VSTSelectNode(VST, VSTFocusedNode(VST)-1, True);
    end;

    if WaveToo then
      VSTDblClick(NIL)
    else
      FocusMemo;
  end;
end;

// -----------------------------------------------------------------------------

function GetSubtitleIndexAtTime(const MSecs: Cardinal): Integer;
var
  i: Integer;
begin
  Result := -1;

  if Subtitles.Count > 0 then
    for i := 0 to Subtitles.Count-1 do
      with Subtitles[i] do
        if (InitialTime <= MSecs + 1) and (FinalTime > MSecs) then
        begin
          Result := i;
          SubtitleInfo.LastSubtitle.ShowIndex := Result;
          Break;
        end;
end;

// -----------------------------------------------------------------------------

function GetSubtitleTextAtTime(const MSecs: Cardinal): String;
var
  i: Integer;
  s: String;
begin
  Result := '';

  i := GetSubtitleIndexAtTime(MSecs);
  if i >= 0 then
    with Subtitles[i] do
    begin
      Result := Text;
      case MPVOptions.SubtitleToShow of
        TSubtitleMode.smText        : Result := Text;
        TSubtitleMode.smTranslation : Result := Translation;
      end;
      Result := ReplaceString(Result, sLineBreak, #10) + #10 + ' ';
      s := '';
      if Align <> 0 then
      begin
        case Align of
          1: case VAlign of
               1 : s := '{\an4}';
               2 : s := '{\an7}';
             else
               s := '{\an1}';
             end;
          2: case VAlign of
               1 : s := '{\an5}';
               2 : s := '{\an8}';
             else
               s := '{\an2}';
             end;
          3: case VAlign of
               1 : s := '{\an6}';
               2 : s := '{\an9}';
             else
               s := '{\an3}';
             end;
        end;
      end
      else if VAlign <> 0 then
      begin
        case VAlign of
          1 : s := '{\an5}';
          2 : s := '{\an8}';
        else
          s := '{\an2}';
        end;
      end;

      if not s.IsEmpty then
        Result := s + Result + '{\an0}';
    end;
end;

// -----------------------------------------------------------------------------

procedure InsertMemoText(const AText: String);
var
  Memo : TUWMemo;
begin
  if not Assigned(Workspace.Transcription.Memo) then
  begin
    Memo := GetMemoFocused;
    if Memo <> NIL then
      Memo.SelText := AText
    else
      VSTDoLoop(frmMain.VST, @ApplyUnicodeChar);
  end
  else
  begin
    TranscriptionInsertText(AText);
  end;
end;

// -----------------------------------------------------------------------------

procedure SetTag(const Tag, TagFormat: String; const Len: Integer);
var
  Memo : TUWMemo;
  i, l : Integer;
begin
  Memo := GetMemoFocused;
  if Memo = NIL then Exit;

  i := Memo.SelStart;
  l := Memo.SelLength;
  if Memo.SelText <> '' then
    Memo.SelText := Format(TagFormat, [Tag, Memo.SelText, Tag])
  else
    Memo.Text := Format(TagFormat, [Tag, Memo.Text, Tag]);

  Memo.SelStart  := i+Len;
  Memo.SelLength := l;
end;

// -----------------------------------------------------------------------------

procedure SetTextTag(const Tag: String);
begin
  SetTag(Tag, '{\%s1}%s{\%s0}', 5);
end;

// -----------------------------------------------------------------------------

procedure SetTextCustomTag(const Tag: String);
begin
  SetTag(Tag, '%s%s%s', Tag.Length);
end;

// -----------------------------------------------------------------------------

procedure SetTextTagColor(const HexColor: String);
var
  Memo : TUWMemo;
  i    : Integer;
begin
  Memo := GetMemoFocused;
  if Memo = NIL then Exit;

  i := Memo.SelStart;
  if Memo.SelText <> '' then
    Memo.SelText := Format('{\%s&%s&}%s{\%s}', [swt_Color, HexColor, Memo.SelText, swt_Color])
  else
    Memo.Text := Format('{\%s&%s&}%s{\%s}', [swt_Color, HexColor, Memo.Text, swt_Color]);

  Memo.SelStart := i+12;
end;

// -----------------------------------------------------------------------------

procedure SetAlignTo(const ATag: Integer);
begin
  with frmMain do
    if GetMemoFocused = NIL then
      VSTDoLoop(VST, @ApplyAlign)
    else
      Subtitles.ItemPointer[VSTFocusedNode(VST)]^.Align := ATag;
end;

// -----------------------------------------------------------------------------

procedure SetVAlignTo(const ATag: Integer);
begin
  with frmMain do
    if GetMemoFocused = NIL then
      VSTDoLoop(VST, @ApplyVAlign)
    else
      Subtitles.ItemPointer[VSTFocusedNode(VST)]^.VAlign := ATag;
end;

// -----------------------------------------------------------------------------

function SetEndCueOneFrame(const AFinalTime: Integer; const ASubtract: Boolean = False): Integer;
begin
  if not ASubtract then
    Result := AFinalTime + FramesToTime(1, Workspace.FPS.OutputFPS)
  else
    Result := AFinalTime - FramesToTime(1, Workspace.FPS.OutputFPS);
end;

// -----------------------------------------------------------------------------

function GetSubtitleMarkedCount: Integer;
var
  i: Integer;
begin
  Result := 0;

  for i := 0 to Subtitles.Count-1 do
    if Subtitles[i].Marked then
      Inc(Result);
end;

// -----------------------------------------------------------------------------

end.

