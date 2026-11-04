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

unit procMacSystemMenus;

{$mode objfpc}{$H+}
{$IFDEF DARWIN}
{$modeswitch objectivec1}
{$ENDIF}

interface

uses
  Menus;

procedure MacDisableSystemMenuItems;
procedure MacSystemMenusInstall(const AMenus: array of TMenuItem);

// -----------------------------------------------------------------------------

implementation

// Menu items that macOS adds by itself to the app menus and that Tero doesn't
// use: AutoFill, Writing Tools (Edit menu), Enter Full Screen, Show Tab Bar
// and Show All Tabs (View menu). Start Dictation and Emoji & Symbols are kept,
// they're useful to write the text (see KeepAction).
//
// Dictation, Emoji & Symbols and Full Screen have their own user defaults
// keys, tabs are disabled with NSWindow.allowsAutomaticWindowTabbing (10.12+).
// These must be set before the main menu is created (before
// Application.Initialize).
//
// AutoFill and Writing Tools don't have a key, so all the items of the given
// menus that were not created by the LCL are removed each time those menus
// are opened (the LCL calls the top menu item OnClick from menuNeedsUpdate).

{$IFDEF DARWIN}
uses
  Classes, Forms, CocoaAll;

type

  { NSWindow, 10.12+ (not in the FPC 3.2 headers) }

  NSWindowTeroTabbing = objccategory external (NSWindow)
    class function teroRespondsToSelector(aSelector: SEL): ObjCBOOL; message 'respondsToSelector:';
    class procedure teroSetAllowsAutomaticWindowTabbing(value: ObjCBOOL); message 'setAllowsAutomaticWindowTabbing:';
  end;

  { TMacSystemMenus }

  TMacSystemMenus = class
  public
    Items: array of TMenuItem;
    OldClicks: array of TNotifyEvent;
    procedure MenuClick(Sender: TObject);
    procedure CleanAll(Data: PtrInt);
  end;

var
  FSystemMenus: TMacSystemMenus = NIL;

// -----------------------------------------------------------------------------

procedure MacDisableSystemMenuItems;
var
  d: NSMutableDictionary;
begin
  d := NSMutableDictionary(NSMutableDictionary.dictionary);
  //d.setObject_forKey(NSNumber.numberWithBool(True), NSSTR('NSDisabledDictationMenuItem'));
  //d.setObject_forKey(NSNumber.numberWithBool(True), NSSTR('NSDisabledCharacterPaletteMenuItem'));
  d.setObject_forKey(NSNumber.numberWithBool(False), NSSTR('NSFullScreenMenuItemEverywhere'));
  // AutoFill heuristics in the text fields (macOS 26, also slow typing/scrolling)
  d.setObject_forKey(NSNumber.numberWithBool(False), NSSTR('NSAutoFillHeuristicControllerEnabled'));
  NSUserDefaults.standardUserDefaults.registerDefaults(d);

  // Show Tab Bar/Show All Tabs
  if NSWindow.teroRespondsToSelector(objcselector('setAllowsAutomaticWindowTabbing:')) then
    NSWindow.teroSetAllowsAutomaticWindowTabbing(False);
end;

// -----------------------------------------------------------------------------

function KeepAction(const AItem: NSMenuItem): Boolean;
begin
  // system items we keep: Emoji & Symbols and Start Dictation
  Result := (AItem.action = objcselector('orderFrontCharacterPalette:')) or
    (AItem.action = objcselector('startDictation:'));
end;

// -----------------------------------------------------------------------------

procedure RemoveSystemItems(const AItem: TMenuItem);
var
  m: NSMenu;
  it: NSMenuItem;
  i: Integer;

  function IsOwn(const AMenuItem: NSMenuItem): Boolean;
  var
    j: Integer;
  begin
    Result := False;
    for j := 0 to AItem.Count-1 do
      if AItem[j].HandleAllocated and (NSMenuItem(AItem[j].Handle) = AMenuItem) then
        Exit(True);
  end;

begin
  if (AItem = NIL) or not AItem.HandleAllocated then Exit;
  m := NSMenuItem(AItem.Handle).submenu;
  if m = NIL then Exit;

  // the system items, but the separators and the ones we keep
  for i := m.numberOfItems-1 downto 0 do
  begin
    it := m.itemAtIndex(i);
    if not it.isSeparatorItem and not KeepAction(it) and not IsOwn(it) then
      m.removeItemAtIndex(i);
  end;

  // the system separators left over: at the start, at the end or repeated
  for i := m.numberOfItems-1 downto 0 do
  begin
    it := m.itemAtIndex(i);
    if it.isSeparatorItem and not IsOwn(it) and ((i = 0) or (i = m.numberOfItems-1) or
      m.itemAtIndex(i-1).isSeparatorItem) then
      m.removeItemAtIndex(i);
  end;
end;

// -----------------------------------------------------------------------------

{ TMacSystemMenus }

// -----------------------------------------------------------------------------

procedure TMacSystemMenus.MenuClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to High(Items) do
    if Items[i] = Sender then
    begin
      RemoveSystemItems(Items[i]);
      if Assigned(OldClicks[i]) then OldClicks[i](Sender);
      Break;
    end;
end;

// -----------------------------------------------------------------------------

procedure TMacSystemMenus.CleanAll(Data: PtrInt);
var
  i: Integer;
begin
  for i := 0 to High(Items) do
    RemoveSystemItems(Items[i]);
end;

// -----------------------------------------------------------------------------

procedure MacSystemMenusInstall(const AMenus: array of TMenuItem);
var
  i: Integer;
begin
  if Assigned(FSystemMenus) then Exit;

  FSystemMenus := TMacSystemMenus.Create;
  SetLength(FSystemMenus.Items, Length(AMenus));
  SetLength(FSystemMenus.OldClicks, Length(AMenus));
  for i := 0 to High(AMenus) do
  begin
    FSystemMenus.Items[i] := AMenus[i];
    FSystemMenus.OldClicks[i] := AMenus[i].OnClick;
    AMenus[i].OnClick := @FSystemMenus.MenuClick;
  end;
  // the ones already added at launch
  Application.QueueAsyncCall(@FSystemMenus.CleanAll, 0);
end;

// -----------------------------------------------------------------------------

initialization

// -----------------------------------------------------------------------------

finalization
  FSystemMenus.Free;

{$ELSE}
// -----------------------------------------------------------------------------

procedure MacDisableSystemMenuItems;
begin
end;

// -----------------------------------------------------------------------------

procedure MacSystemMenusInstall(const AMenus: array of TMenuItem);
begin
end;
{$ENDIF}

// -----------------------------------------------------------------------------

end.
