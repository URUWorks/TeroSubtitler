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

unit procTranscription;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Controls, SysUtils, Menus, Forms, ATSynEdit, ATSynEdit_Commands;

procedure TranscriptionInitializeControls;
procedure TranscriptionUnInitializeControls;

procedure TranscriptionUndo;
procedure TranscriptionRedo;
procedure TranscriptionCopy;
procedure TranscriptionCut;
procedure TranscriptionPaste;
procedure TranscriptionSelectAll;
procedure TranscriptionSelectInverted;
procedure TranscriptionInsertBelow;
procedure TranscriptionInsertAbove;
procedure TranscriptionDelete;
procedure TranscriptionInsertText(const AText: String);

// -----------------------------------------------------------------------------

implementation

uses
  formMain, procTypes, procColorTheme;

// -----------------------------------------------------------------------------

procedure TranscriptionInitializeControls;
begin
  with frmMain, Workspace.Transcription do
    if not Assigned(Memo) then
    begin
      Memo                        := TATSynEdit.Create(LayoutTranscription);
      Memo.Parent                 := LayoutTranscription;
      Memo.Align                  := alClient;
      Memo.Modified               := False;
      Memo.OptShowMouseSelFrame   := False;
      Memo.OptShowURLsRegex       := RegExprTime;
      Memo.OptUnprintedVisible    := False;
      //Memo.OptGutterVisible       := False;
      //Memo.OptMouse2ClickOpensURL := False;
      //Memo.OptMouseClickOpensURL  := True;
      Memo.OptFoldEnabled         := False;
      Memo.OptFoldCacheEnabled    := False;
      Memo.OptFoldTooltipVisible  := False;
      Memo.OnClickLink            := @MemoClickLink;
      Memo.OnContextPopup         := @MemoContextPopup;
      Memo.OnChange               := @MemoChange;
      // put only basic commands
      with Memo.Keymap do
      begin
        Clear;
        Add(cCommand_KeyLeft,         '', ['Left'], []);
        Add(cCommand_KeyLeft_Sel,     '', ['Shift+Left'], []);
        Add(cCommand_KeyRight,        '', ['Right'], []);
        Add(cCommand_KeyRight_Sel,    '', ['Shift+Right'], []);
        Add(cCommand_KeyUp,           '', ['Up'], []);
        Add(cCommand_KeyUp_Sel,       '', ['Shift+Up'], []);
        Add(cCommand_KeyDown,         '', ['Down'], []);
        Add(cCommand_KeyDown_Sel,     '', ['Shift+Down'], []);
        Add(cCommand_KeyHome,         '', {$ifndef darwin} ['Home'],       {$else} ['Meta+Left'],        {$endif} []);
        Add(cCommand_KeyHome_Sel,     '', {$ifndef darwin} ['Shift+Home'], {$else} ['Meta+Shift+Left'],  {$endif} []);
        Add(cCommand_KeyEnd,          '', {$ifndef darwin} ['End'],        {$else} ['Meta+Right'],       {$endif} []);
        Add(cCommand_KeyEnd_Sel,      '', {$ifndef darwin} ['Shift+End'],  {$else} ['Meta+Shift+Right'], {$endif} []);
        Add(cCommand_KeyPageUp,       '', ['PgUp'], []);
        Add(cCommand_KeyPageUp_Sel,   '', ['Shift+PgUp'], []);
        Add(cCommand_KeyPageDown,     '', ['PgDn'], []);
        Add(cCommand_KeyPageDown_Sel, '', ['Shift+PgDn'], []);
        Add(cCommand_KeyBackspace,    '', ['Bksp'], ['Shift+Bksp']);
        Add(cCommand_KeyDelete,       '', ['Del'], []);
        Add(cCommand_KeyEnter,        '', ['Enter'], ['Shift+Enter']);
      end;
      Memo.DoCommand(cCommand_ToggleFolding, cInvokeInternal);

      ColorThemeInstance.Apply(Memo, cmAuto);
    end;
end;

// -----------------------------------------------------------------------------

procedure TranscriptionUnInitializeControls;
begin
  with Workspace.Transcription do
    if Assigned(Memo) then
      FreeAndNil(Memo);
end;

// -----------------------------------------------------------------------------

procedure TranscriptionDoCommand(const ACommand: Integer; const AUpdate: Boolean = True; const AText: String = '');
begin
  with Workspace.Transcription do
    if Assigned(Memo) then
    begin
      Memo.DoCommand(ACommand, cInvokeInternal, AText);
      if AUpdate then Memo.Update;
    end;
end;

// -----------------------------------------------------------------------------

procedure TranscriptionUndo;
begin
  TranscriptionDoCommand(cCommand_Undo);
end;

// -----------------------------------------------------------------------------

procedure TranscriptionRedo;
begin
  TranscriptionDoCommand(cCommand_Redo);
end;

// -----------------------------------------------------------------------------

procedure TranscriptionCopy;
begin
  TranscriptionDoCommand(cCommand_ClipboardCopy, False);
end;

// -----------------------------------------------------------------------------

procedure TranscriptionCut;
begin
  TranscriptionDoCommand(cCommand_ClipboardCut, False);
end;

// -----------------------------------------------------------------------------

procedure TranscriptionPaste;
begin
  TranscriptionDoCommand(cCommand_ClipboardPaste, False);
end;

// -----------------------------------------------------------------------------

procedure TranscriptionSelectAll;
begin
  TranscriptionDoCommand(cCommand_SelectAll, False);
end;

// -----------------------------------------------------------------------------

procedure TranscriptionSelectInverted;
begin
  TranscriptionDoCommand(cCommand_SelectInverted, False);
end;

// -----------------------------------------------------------------------------

procedure TranscriptionInsertBelow;
begin
  TranscriptionDoCommand(cCommand_TextInsertEmptyBelow, False);
end;

// -----------------------------------------------------------------------------

procedure TranscriptionInsertAbove;
begin
  TranscriptionDoCommand(cCommand_TextInsertEmptyAbove, False);
end;

// -----------------------------------------------------------------------------

procedure TranscriptionDelete;
begin
  TranscriptionDoCommand(cCommand_TextDeleteSelection, False);
end;

// -----------------------------------------------------------------------------

procedure TranscriptionInsertText(const AText: String);
begin
  TranscriptionDoCommand(cCommand_TextInsert, False, AText);
end;

// -----------------------------------------------------------------------------

end.

