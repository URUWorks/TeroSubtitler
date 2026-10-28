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
//
// While a modal dialog is shown (the native open/save panels too), the LCL
// disables the main menu items but they keep their key equivalents. In a
// native panel the text fields edit through the standard Edit menu actions
// (selectAll:, copy:...), and a disabled Tero item with the same shortcut
// (Select all, Cmd+A) is found first and the key does nothing. So the
// standard edit shortcuts are removed from the main menu items while a modal
// is shown and restored after.

{$IFDEF DARWIN}
uses
  Classes, Controls, LCLType, Menus, CocoaAll;

type

  { TMacMenuKeys }

  TMacMenuKeys = class
  public
    Form: TCustomForm;
    procedure KeyDownBefore(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ModalBegin(Sender: TObject);
    procedure ModalEnd(Sender: TObject);
  end;

  TSavedKeyEquivalent = record
    Item : TMenuItem;
    Key  : NSString;
  end;

var
  FMenuKeys: TMacMenuKeys = NIL;
  FSavedKeys: array of TSavedKeyEquivalent;

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

function IsStandardEditKey(const AItem: NSMenuItem): Boolean;
var
  k: String;
  m: NSUInteger;
begin
  Result := False;
  if (AItem.keyEquivalent = NIL) then Exit;
  k := AItem.keyEquivalent.UTF8String;
  m := AItem.keyEquivalentModifierMask and (NSCommandKeyMask or NSShiftKeyMask or NSControlKeyMask or NSAlternateKeyMask);
  // Cmd+A/C/V/X/Z and Shift+Cmd+Z (redo)
  if m = NSCommandKeyMask then
    Result := (k = 'a') or (k = 'c') or (k = 'v') or (k = 'x') or (k = 'z') or (k = 'Z')
  else if m = (NSCommandKeyMask or NSShiftKeyMask) then
    Result := (k = 'z') or (k = 'Z');
end;

// -----------------------------------------------------------------------------

procedure TMacMenuKeys.ModalBegin(Sender: TObject);

  procedure Scan(const AItem: TMenuItem);
  var
    i: Integer;
    it: NSMenuItem;
  begin
    for i := 0 to AItem.Count-1 do
    begin
      if AItem[i].HandleAllocated then
      begin
        it := NSMenuItem(AItem[i].Handle);
        if (it <> NIL) and IsStandardEditKey(it) then
        begin
          SetLength(FSavedKeys, Length(FSavedKeys)+1);
          FSavedKeys[High(FSavedKeys)].Item := AItem[i];
          FSavedKeys[High(FSavedKeys)].Key  := it.keyEquivalent;
          FSavedKeys[High(FSavedKeys)].Key.retain;
          it.setKeyEquivalent(NSSTR(''));
        end;
      end;
      Scan(AItem[i]);
    end;
  end;

begin
  if (Length(FSavedKeys) > 0) or (Form = NIL) or (Form.Menu = NIL) then Exit;
  Scan(Form.Menu.Items);
end;

// -----------------------------------------------------------------------------

procedure TMacMenuKeys.ModalEnd(Sender: TObject);
var
  i: Integer;
  it: NSMenuItem;
begin
  for i := 0 to High(FSavedKeys) do
  begin
    with FSavedKeys[i] do
    begin
      // only if the shortcut wasn't changed (or removed) while the modal was
      // shown, e.g. in the shortcuts settings
      if (Item.ShortCut <> 0) and Item.HandleAllocated then
      begin
        it := NSMenuItem(Item.Handle);
        if (it <> NIL) and ((it.keyEquivalent = NIL) or (it.keyEquivalent.length = 0)) then
          it.setKeyEquivalent(Key);
      end;
      Key.release;
    end;
  end;
  SetLength(FSavedKeys, 0);
end;

// -----------------------------------------------------------------------------

procedure MacMenuKeysInstall(const AForm: TCustomForm);
begin
  if Assigned(FMenuKeys) or (AForm = NIL) then Exit;

  FMenuKeys := TMacMenuKeys.Create;
  FMenuKeys.Form := AForm;
  Application.AddOnKeyDownBeforeHandler(@FMenuKeys.KeyDownBefore);
  Application.AddOnModalBeginHandler(@FMenuKeys.ModalBegin);
  Application.AddOnModalEndHandler(@FMenuKeys.ModalEnd);
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
