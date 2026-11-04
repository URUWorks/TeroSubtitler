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

unit procMacTextView;

{$mode objfpc}{$H+}
{$IFDEF DARWIN}
{$modeswitch objectivec1}
{$ENDIF}

interface

uses
  Controls;

procedure MacDisableFontPanel(const AMemo: TWinControl);

// -----------------------------------------------------------------------------

implementation

// A memo (NSTextView) that becomes the focus of a window that turns key
// updates the system Font panel with the fonts of the selected text
// (updateFontPanel > fallbackFontInfoForSelectedRange). On some Macs this
// gives an access violation inside AppKit when the Spell check window is
// shown with a word selected. Tero doesn't use the Font panel, so the memo
// is told not to use it and AppKit skips that code.

{$IFDEF DARWIN}
uses
  CocoaAll;

// -----------------------------------------------------------------------------

procedure MacDisableFontPanel(const AMemo: TWinControl);
var
  v: NSView;
begin
  if (AMemo = NIL) then Exit;

  // a TMemo handle is the scroll view, the text view is its document view
  v := NSView(AMemo.Handle);
  if (v <> NIL) and v.isKindOfClass(NSScrollView) then
    v := NSView(NSScrollView(v).documentView);
  if (v = NIL) or not v.isKindOfClass(NSTextView) then Exit;

  NSTextView(v).setUsesFontPanel(False);
end;
{$ELSE}
// -----------------------------------------------------------------------------

procedure MacDisableFontPanel(const AMemo: TWinControl);
begin
end;
{$ENDIF}

// -----------------------------------------------------------------------------

end.
