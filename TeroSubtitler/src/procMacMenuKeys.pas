{*
 *  URUWorks Tero Subtitler
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
 *  Copyright (C) 2023-2026 URUWorks, uruworks@gmail.com.
 *}

unit procMacMenuKeys;

{$mode objfpc}{$H+}
{$IFDEF DARWIN}
{$modeswitch objectivec1}
{$ENDIF}

interface

uses
  Forms;

function MacMenuPerformKeyEquivalent: Boolean;
procedure MacMenuKeysInstall(const AForm: TCustomForm);

// -----------------------------------------------------------------------------

implementation

// Menu shortcuts through the macOS main menu.
//
// The LCL runs the action shortcuts itself (form OnShortCut, main menu and
// action lists) before Cocoa sees the key, so the top menu never flashes like
// in native apps. Passing the current key event to the main menu first makes
// NSMenu flash the top menu and select the item, which runs the same LCL
// action. If no enabled item has that shortcut, nothing changes.
//
// It's done in an application "key down before" handler, which the LCL calls
// before any shortcut check: from the form OnShortCut it was too late when a
// control has a popup menu with the same shortcuts (the memo, Cmd+C/V/X/A...).

{$IFDEF DARWIN}
uses
  Classes, Controls, LCLType, CocoaAll;

type

  { TMacMenuKeys }

  TMacMenuKeys = class
  public
    Form: TCustomForm;
    procedure KeyDownBefore(Sender: TObject; var Key: Word; Shift: TShiftState);
  end;

var
  FMenuKeys: TMacMenuKeys = NIL;

// -----------------------------------------------------------------------------

function MacMenuPerformKeyEquivalent: Boolean;
var
  Event : NSEvent;
  Menu  : NSMenu;
begin
  Result := False;
  if NSApp = NIL then Exit;

  Event := NSApp.currentEvent;
  if (Event = NIL) or (Event.type_ <> NSKeyDown) then Exit;
  // only keys with a modifier (Cmd, Ctrl, Option): plain keys are typing
  if (Event.modifierFlags and (NSCommandKeyMask or NSControlKeyMask or NSAlternateKeyMask)) = 0 then Exit;

  Menu := NSApp.mainMenu;
  if Menu = NIL then Exit;

  Result := Menu.performKeyEquivalent(Event);
end;

// -----------------------------------------------------------------------------

{ TMacMenuKeys }

// -----------------------------------------------------------------------------

procedure TMacMenuKeys.KeyDownBefore(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_UNKNOWN) or ([ssMeta, ssCtrl, ssAlt] * Shift = []) then Exit;
  // only keys of the main form controls, not of other forms (dialogs)
  if not (Sender is TControl) or (GetParentForm(TControl(Sender)) <> Form) then Exit;

  if MacMenuPerformKeyEquivalent then
    Key := VK_UNKNOWN;
end;

// -----------------------------------------------------------------------------

procedure MacMenuKeysInstall(const AForm: TCustomForm);
begin
  if Assigned(FMenuKeys) or (AForm = NIL) then Exit;

  FMenuKeys := TMacMenuKeys.Create;
  FMenuKeys.Form := AForm;
  Application.AddOnKeyDownBeforeHandler(@FMenuKeys.KeyDownBefore);
end;
{$ELSE}
// -----------------------------------------------------------------------------

function MacMenuPerformKeyEquivalent: Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------

procedure MacMenuKeysInstall(const AForm: TCustomForm);
begin
end;
{$ENDIF}

// -----------------------------------------------------------------------------

end.
