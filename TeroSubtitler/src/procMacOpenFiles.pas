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

unit procMacOpenFiles;

{$mode objfpc}{$H+}
{$IFDEF DARWIN}
{$modeswitch objectivec1}
{$ENDIF}

interface

procedure InstallMacOpenFilesFix;

// -----------------------------------------------------------------------------

implementation

// Files dropped on the app icon (Dock/Finder) on macOS < 10.13.
//
// Since Lazarus 4 the Cocoa LCL app delegate only implements
// application:openURLs:, which macOS calls from 10.13. Older systems only
// call application:openFiles:, so without it AppKit falls back to
// NSDocumentController and shows "Tero Subtitler cannot open files in the
// ... format". This adds application:openFiles: to the LCL delegate class
// at runtime, forwarding to its application:openURLs: (which already queues
// the files until the main form exists).

{$IFDEF DARWIN}
uses
  CocoaAll;

const
  NSAppKitVersionNumber10_13 = 1561;

// -----------------------------------------------------------------------------

procedure DelegateOpenFiles(self: Pointer; _cmd: Pointer; sender: NSApplication; filenames: NSArray); cdecl;
var
  urls : NSMutableArray;
  i    : Integer;
  obj  : NSObject;
begin
  obj  := NSObject(self);
  urls := NSMutableArray.alloc.initWithCapacity(filenames.count);
  try
    for i := 0 to filenames.count-1 do
      urls.addObject(NSURL.fileURLWithPath(NSString(filenames.objectAtIndex(i))));

    if obj.respondsToSelector(objcselector('application:openURLs:')) then
      obj.performSelector_withObject_withObject(objcselector('application:openURLs:'), sender, urls);
  finally
    urls.release;
  end;

  sender.replyToOpenOrPrint(NSApplicationDelegateReplySuccess);
end;

// -----------------------------------------------------------------------------

procedure InstallMacOpenFilesFix;
var
  ADelegate: NSObject;
begin
  // 10.13+ calls application:openURLs:, nothing to do
  if NSAppKitVersionNumber >= NSAppKitVersionNumber10_13 then Exit;
  if NSApp = NIL then Exit;

  ADelegate := NSObject(NSApp.delegate);
  if ADelegate = NIL then Exit;
  // an LCL that still implements it (Lazarus < 4) doesn't need the fix
  if ADelegate.respondsToSelector(objcselector('application:openFiles:')) then Exit;
  if not ADelegate.respondsToSelector(objcselector('application:openURLs:')) then Exit;

  class_addMethod(object_getClass(objc.id(ADelegate)), objc.SEL(objcselector('application:openFiles:')),
    objc.IMP(@DelegateOpenFiles), 'v@:@@');
end;
{$ELSE}
procedure InstallMacOpenFilesFix;
begin
end;
{$ENDIF}

// -----------------------------------------------------------------------------

end.
