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

unit procMacAdjustSizeFix;

{$mode objfpc}{$H+}
{$IFDEF DARWIN}
{$modeswitch objectivec1}
{$ENDIF}

interface

procedure MacAdjustSizeFixInstall;

// -----------------------------------------------------------------------------

implementation

// Access violation in TASyncLCLControlAdjustSizer.doAdjustSize (LCL Cocoa,
// Lazarus 5) some seconds after closing a form, with "legacy" scroll bars
// (a mouse connected, or System Settings > Show scroll bars: Always).
//
// When the scroll range of a form changes (SetScrollInfo, it happens while
// a form is created/laid out), LCL Cocoa remembers the form in a global
// object and queues an AdjustSize of it (Application.QueueAsyncCall). It
// doesn't keep a reference: if the form is freed before the queue runs (a
// dialog that is closed quickly), the queued call uses the freed form.
// With overlay scroll bars (trackpad) nothing is queued, so it only fails
// on some Macs.
//
// The queued call can't be removed from outside, but the remembered control
// can be changed: TCocoaWSScrollingUtil.asyncAdjustSize replaces it (and
// queues nothing new when a call is already pending). So when a form goes
// away, the main form, still alive, takes its place: the pending call does
// an AdjustSize of the main form, harmless.

{$IFDEF LCLCocoa}
uses
  Classes, Forms, CocoaWSScrolling; // Lazarus 5 (4.x: CocoaWSScrollers, TCocoaWSScrollerUtil)

type
  TMacAdjustSizeFix = class
    procedure RemoveForm(Sender: TObject; AForm: TCustomForm);
  end;

var
  Fix: TMacAdjustSizeFix = NIL;

// -----------------------------------------------------------------------------

procedure TMacAdjustSizeFix.RemoveForm(Sender: TObject; AForm: TCustomForm);
var
  Main: TCustomForm;
begin
  Main := Application.MainForm;
  if (Main = NIL) or (Main = AForm) or (csDestroying in Main.ComponentState) or
     (csDestroying in Application.ComponentState) then Exit;

  try
    TCocoaWSScrollingUtil.asyncAdjustSize(Main);
  except
  end;
end;

// -----------------------------------------------------------------------------

procedure MacAdjustSizeFixInstall;
begin
  if Fix <> NIL then Exit;
  Fix := TMacAdjustSizeFix.Create;
  Screen.AddHandlerRemoveForm(@Fix.RemoveForm);
end;

// -----------------------------------------------------------------------------

finalization
  if Fix <> NIL then
  begin
    if Assigned(Screen) then
      Screen.RemoveHandlerRemoveForm(@Fix.RemoveForm);
    Fix.Free;
  end;

{$ELSE}

procedure MacAdjustSizeFixInstall;
begin
end;

{$ENDIF}

// -----------------------------------------------------------------------------

end.
