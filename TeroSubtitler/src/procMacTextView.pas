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

procedure MacDisableFontPanel(const AControl: TWinControl);
procedure MacFontPanelInstall;
procedure MacRedrawTextView(const AControl: TWinControl);

// -----------------------------------------------------------------------------

implementation

// A text view (NSTextView) that is the first responder updates the system
// Font panel with the fonts of the selected text every time the selection
// changes or the window turns key (updateFontPanel >
// fallbackFontInfoForSelectedRange). On some Macs this gives an access
// violation inside AppKit: when the Spell check window is shown with a word
// selected, and in the main memos when the selection is extended (Shift+Down),
// or selected text is cut or deleted (Backspace). On macOS 10.11 the access
// violation even ends in a stack overflow inside the exception handling.
// Tero doesn't use the Font panel, so the text views are told not to use it
// and AppKit skips that code.
//
// Memos are their own text view. Edits and combo boxes (NSTextField) edit
// through the field editor of their window, a text view shared by all of them.
// MacFontPanelInstall does it for every control that gets the focus, in every
// form, so new forms and recreated handles are covered too.

{$IFDEF DARWIN}
uses
  Forms, CocoaAll;

type

  { TMacFontPanel }

  TMacFontPanel = class
  public
    procedure ActiveControlChanged(Sender: TObject; LastControl: TControl);
  end;

var
  FFontPanel: TMacFontPanel = NIL;

// -----------------------------------------------------------------------------

procedure DisableInView(v: NSView);
var
  t: NSText;
begin
  if (v = NIL) then Exit;

  // a TMemo handle is the scroll view, the text view is its document view
  if v.isKindOfClass(NSScrollView) then
    v := NSView(NSScrollView(v).documentView);
  if (v = NIL) then Exit;

  if v.isKindOfClass(NSTextView) then
    NSTextView(v).setUsesFontPanel(False)
  else if v.isKindOfClass(NSTextField) and (v.window <> NIL) then
  begin
    // edits: the field editor of the window (LCL returns its own one)
    t := v.window.fieldEditor_forObject(True, v);
    if (t <> NIL) and t.isKindOfClass(NSTextView) then
      NSTextView(t).setUsesFontPanel(False);
  end;
end;

// -----------------------------------------------------------------------------

procedure MacDisableFontPanel(const AControl: TWinControl);
begin
  if (AControl = NIL) or not AControl.HandleAllocated then Exit;
  DisableInView(NSView(AControl.Handle));
end;

// -----------------------------------------------------------------------------

// The Spell check window changes the text of its memo (setString) and its
// selection several times in a row before AppKit draws (the lines without
// errors are skipped in the same click). Sometimes AppKit then redraws only
// the rect of the new selection and the other lines keep the glyphs of an
// earlier entry (the second line of #285 under the first line of #294) until
// the window loses the focus and the whole view is drawn again. The whole
// text is invalidated here, so the next draw covers every line.
procedure MacRedrawTextView(const AControl: TWinControl);
var
  v  : NSView;
  tv : NSTextView;
  r  : NSRange;
begin
  if (AControl = NIL) or not AControl.HandleAllocated then Exit;
  v := NSView(AControl.Handle);
  if (v = NIL) then Exit;

  if v.isKindOfClass(NSScrollView) then
  begin
    // setNeedsDisplayInRect: setNeedsDisplay is setNeedsDisplay_ in the
    // FPC 3.2 Cocoa headers and setNeedsDisplay in FPC main
    v.setNeedsDisplayInRect(v.bounds);
    NSScrollView(v).contentView.setNeedsDisplayInRect(NSScrollView(v).contentView.bounds);
    v := NSView(NSScrollView(v).documentView);
  end;

  if (v <> NIL) and v.isKindOfClass(NSTextView) then
  begin
    tv := NSTextView(v);
    if (tv.layoutManager <> NIL) and (tv.textStorage <> NIL) then
    begin
      tv.layoutManager.ensureLayoutForTextContainer(tv.textContainer);
      r.location := 0;
      r.length   := tv.textStorage.length;
      tv.layoutManager.invalidateDisplayForCharacterRange(r);
    end;
    tv.setNeedsDisplayInRect(tv.bounds);
  end;
end;

// -----------------------------------------------------------------------------

{ TMacFontPanel }

// -----------------------------------------------------------------------------

procedure TMacFontPanel.ActiveControlChanged(Sender: TObject; LastControl: TControl);
begin
  // setUsesFontPanel only stores a flag, it's cheap to repeat it
  MacDisableFontPanel(Screen.ActiveControl);
end;

// -----------------------------------------------------------------------------

procedure MacFontPanelInstall;
var
  f, c: Integer;
begin
  if Assigned(FFontPanel) then Exit;

  FFontPanel := TMacFontPanel.Create;
  Screen.AddHandlerActiveControlChanged(@FFontPanel.ActiveControlChanged);

  // the controls that already have a handle (and the focus)
  for f := 0 to Screen.CustomFormCount-1 do
    with Screen.CustomForms[f] do
      for c := 0 to ComponentCount-1 do
        if Components[c] is TWinControl then
          MacDisableFontPanel(TWinControl(Components[c]));
end;

{$ELSE}
// -----------------------------------------------------------------------------

procedure MacDisableFontPanel(const AControl: TWinControl);
begin
end;

// -----------------------------------------------------------------------------

procedure MacFontPanelInstall;
begin
end;

// -----------------------------------------------------------------------------

procedure MacRedrawTextView(const AControl: TWinControl);
begin
end;
{$ENDIF}

// -----------------------------------------------------------------------------

{$IFDEF DARWIN}
finalization
  if Assigned(FFontPanel) then
  begin
    if Assigned(Screen) then
      Screen.RemoveHandlerActiveControlChanged(@FFontPanel.ActiveControlChanged);
    FFontPanel.Free;
  end;
{$ENDIF}

end.
