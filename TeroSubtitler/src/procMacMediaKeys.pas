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

unit procMacMediaKeys;


{$mode objfpc}{$H+}
{$IFDEF DARWIN}
{$modeswitch objectivec1}
{$ENDIF}

interface

procedure MacMediaKeysInitialize;
procedure MacMediaKeysUpdate(const APlaying: Boolean);
procedure MacMediaKeysClear;

// -----------------------------------------------------------------------------

implementation

// Media keys on macOS (F7/F8/F9, headphones, Control Center "Now Playing").
//
// macOS sends the media keys to one app only: the "Now Playing" one, the last
// that started playing and published its info in MPNowPlayingInfoCenter. So
// Tero registers the MPRemoteCommandCenter commands and updates the now
// playing info/state when the video plays or pauses: the keys work without
// focusing Tero, until another app (Music, IINA...) starts playing.
//
// MediaPlayer.framework exists from macOS 10.12.2. It's loaded at runtime and
// every class/method is looked up by name, so nothing is linked and older
// systems simply don't get the feature.

{$IFDEF DARWIN}
uses
  SysUtils, ActnList, CocoaAll, formMain, procMPV;

const
  MPRemoteCommandHandlerStatusSuccess       = 0;
  MPRemoteCommandHandlerStatusCommandFailed = 200;

  MPNowPlayingPlaybackStatePlaying = 1;
  MPNowPlayingPlaybackStatePaused  = 2;
  MPNowPlayingPlaybackStateStopped = 3;

type

  { MediaPlayer methods, sent to objects found by name (nothing is linked) }

  TMPRemoteCommandExt = objccategory external (NSObject)
    // MPRemoteCommandCenter
    function playCommand: NSObject; message 'playCommand';
    function pauseCommand: NSObject; message 'pauseCommand';
    function togglePlayPauseCommand: NSObject; message 'togglePlayPauseCommand';
    function stopCommand: NSObject; message 'stopCommand';
    function nextTrackCommand: NSObject; message 'nextTrackCommand';
    function previousTrackCommand: NSObject; message 'previousTrackCommand';
    function skipForwardCommand: NSObject; message 'skipForwardCommand';
    function skipBackwardCommand: NSObject; message 'skipBackwardCommand';
    function changePlaybackPositionCommand: NSObject; message 'changePlaybackPositionCommand';
    // MPRemoteCommand
    function mpAddTarget_action(target: id; action: SEL): id; message 'addTarget:action:';
    procedure mpRemoveTarget(target: id); message 'removeTarget:';
    procedure mpSetEnabled(AValue: ObjCBOOL); message 'setEnabled:';
    // MPChangePlaybackPositionCommandEvent
    function mpPositionTime: Double; message 'positionTime';
    // MPNowPlayingInfoCenter
    procedure mpSetNowPlayingInfo(AInfo: NSDictionary); message 'setNowPlayingInfo:';
    procedure mpSetPlaybackState(AState: NSUInteger); message 'setPlaybackState:';
  end;

  { TTeroMediaKeysHandler }

  TTeroMediaKeysHandler = objcclass(NSObject)
  public
    function onTogglePlayPause(event: NSObject): NSInteger; message 'onTogglePlayPause:';
    function onPlay(event: NSObject): NSInteger; message 'onPlay:';
    function onPause(event: NSObject): NSInteger; message 'onPause:';
    function onStop(event: NSObject): NSInteger; message 'onStop:';
    function onForward(event: NSObject): NSInteger; message 'onForward:';
    function onRewind(event: NSObject): NSInteger; message 'onRewind:';
    function onPosition(event: NSObject): NSInteger; message 'onPosition:';
  end;

var
  FHandler       : TTeroMediaKeysHandler = NIL;
  FCommandCenter : NSObject = NIL;
  FInfoCenter    : NSObject = NIL;

// -----------------------------------------------------------------------------

function MediaLoaded: Boolean;
begin
  Result := Assigned(frmMain) and frmMain.MPV.IsMediaLoaded;
end;

// -----------------------------------------------------------------------------

function ExecuteAction(const AAction: TObject): NSInteger;
begin
  Result := MPRemoteCommandHandlerStatusCommandFailed;
  if MediaLoaded and (AAction is TAction) and TAction(AAction).Enabled then
  begin
    TAction(AAction).Execute;
    Result := MPRemoteCommandHandlerStatusSuccess;
  end;
end;

// -----------------------------------------------------------------------------

{ TTeroMediaKeysHandler }

// -----------------------------------------------------------------------------

function TTeroMediaKeysHandler.onTogglePlayPause(event: NSObject): NSInteger;
begin
  Result := ExecuteAction(frmMain.actMediaPlay);
end;

// -----------------------------------------------------------------------------

function TTeroMediaKeysHandler.onPlay(event: NSObject): NSInteger;
begin
  if MediaLoaded and frmMain.MPV.IsPlaying then
    Result := MPRemoteCommandHandlerStatusSuccess
  else
    Result := ExecuteAction(frmMain.actMediaPlay);
end;

// -----------------------------------------------------------------------------

function TTeroMediaKeysHandler.onPause(event: NSObject): NSInteger;
begin
  if MediaLoaded and not frmMain.MPV.IsPlaying then
    Result := MPRemoteCommandHandlerStatusSuccess
  else
    Result := ExecuteAction(frmMain.actMediaPlay);
end;

// -----------------------------------------------------------------------------

function TTeroMediaKeysHandler.onStop(event: NSObject): NSInteger;
begin
  Result := ExecuteAction(frmMain.actMediaStop);
end;

// -----------------------------------------------------------------------------

function TTeroMediaKeysHandler.onForward(event: NSObject): NSInteger;
begin
  Result := ExecuteAction(frmMain.actMediaForward);
  MacMediaKeysUpdate(MediaLoaded and frmMain.MPV.IsPlaying);
end;

// -----------------------------------------------------------------------------

function TTeroMediaKeysHandler.onRewind(event: NSObject): NSInteger;
begin
  Result := ExecuteAction(frmMain.actMediaRewind);
  MacMediaKeysUpdate(MediaLoaded and frmMain.MPV.IsPlaying);
end;

// -----------------------------------------------------------------------------

function TTeroMediaKeysHandler.onPosition(event: NSObject): NSInteger;
begin
  Result := MPRemoteCommandHandlerStatusCommandFailed;
  if MediaLoaded and Assigned(event) and event.respondsToSelector(objcselector('positionTime')) then
  begin
    MPVSeekTo(Round(event.mpPositionTime * 1000));
    MacMediaKeysUpdate(frmMain.MPV.IsPlaying);
    Result := MPRemoteCommandHandlerStatusSuccess;
  end;
end;

// -----------------------------------------------------------------------------

function GetSharedObject(const AClassName: String; const ASelector: SEL): NSObject;
var
  cls: Pointer;
begin
  Result := NIL;
  cls := NSClassFromString(NSString.stringWithUTF8String(PChar(AClassName)));
  if cls = NIL then Exit;
  if NSObject(cls).respondsToSelector(ASelector) then
    Result := NSObject(NSObject(cls).performSelector(ASelector));
end;

// -----------------------------------------------------------------------------

procedure AddCommand(const ACommand: NSObject; const AAction: SEL);
begin
  if ACommand = NIL then Exit;
  ACommand.mpAddTarget_action(FHandler, AAction);
  ACommand.mpSetEnabled(True);
end;

// -----------------------------------------------------------------------------

procedure MacMediaKeysInitialize;
var
  Bundle: NSBundle;
begin
  if Assigned(FHandler) then Exit;

  Bundle := NSBundle.bundleWithPath(NSString.stringWithUTF8String('/System/Library/Frameworks/MediaPlayer.framework'));
  if (Bundle = NIL) or not Bundle.load then Exit;

  FCommandCenter := GetSharedObject('MPRemoteCommandCenter', objcselector('sharedCommandCenter'));
  FInfoCenter    := GetSharedObject('MPNowPlayingInfoCenter', objcselector('defaultCenter'));
  if (FCommandCenter = NIL) or (FInfoCenter = NIL) then
  begin
    FCommandCenter := NIL;
    FInfoCenter    := NIL;
    Exit;
  end;

  FHandler := TTeroMediaKeysHandler.alloc.init;

  AddCommand(FCommandCenter.togglePlayPauseCommand, objcselector('onTogglePlayPause:'));
  AddCommand(FCommandCenter.playCommand, objcselector('onPlay:'));
  AddCommand(FCommandCenter.pauseCommand, objcselector('onPause:'));
  AddCommand(FCommandCenter.stopCommand, objcselector('onStop:'));
  // F9/F7 send next/previous track: seek like the forward/rewind actions
  AddCommand(FCommandCenter.nextTrackCommand, objcselector('onForward:'));
  AddCommand(FCommandCenter.previousTrackCommand, objcselector('onRewind:'));
  AddCommand(FCommandCenter.skipForwardCommand, objcselector('onForward:'));
  AddCommand(FCommandCenter.skipBackwardCommand, objcselector('onRewind:'));
  // seek bar of the Control Center "Now Playing" widget
  if FCommandCenter.respondsToSelector(objcselector('changePlaybackPositionCommand')) then
    AddCommand(FCommandCenter.changePlaybackPositionCommand, objcselector('onPosition:'));
end;

// -----------------------------------------------------------------------------

procedure MacMediaKeysUpdate(const APlaying: Boolean);
var
  Info: NSMutableDictionary;
  Title: String;
begin
  if (FInfoCenter = NIL) or not MediaLoaded then Exit;

  Title := MPVMediaName;

  Info := NSMutableDictionary.alloc.init;
  try
    Info.setObject_forKey(NSString.stringWithUTF8String(PChar(Title)), NSString.stringWithUTF8String('title'));
    Info.setObject_forKey(NSNumber.numberWithDouble(frmMain.MPV.GetMediaLenInMs / 1000), NSString.stringWithUTF8String('playbackDuration'));
    Info.setObject_forKey(NSNumber.numberWithDouble(frmMain.MPV.GetMediaPosInMs / 1000), NSString.stringWithUTF8String('MPNowPlayingInfoPropertyElapsedPlaybackTime'));
    if APlaying then
      Info.setObject_forKey(NSNumber.numberWithDouble(1.0), NSString.stringWithUTF8String('MPNowPlayingInfoPropertyPlaybackRate'))
    else
      Info.setObject_forKey(NSNumber.numberWithDouble(0.0), NSString.stringWithUTF8String('MPNowPlayingInfoPropertyPlaybackRate'));

    FInfoCenter.mpSetNowPlayingInfo(Info);
  finally
    Info.release;
  end;

  // macOS needs the state to make Tero the "Now Playing" app
  if FInfoCenter.respondsToSelector(objcselector('setPlaybackState:')) then
  begin
    if APlaying then
      FInfoCenter.mpSetPlaybackState(MPNowPlayingPlaybackStatePlaying)
    else
      FInfoCenter.mpSetPlaybackState(MPNowPlayingPlaybackStatePaused);
  end;
end;

// -----------------------------------------------------------------------------

procedure MacMediaKeysClear;
begin
  if FInfoCenter = NIL then Exit;

  FInfoCenter.mpSetNowPlayingInfo(NIL);
  if FInfoCenter.respondsToSelector(objcselector('setPlaybackState:')) then
    FInfoCenter.mpSetPlaybackState(MPNowPlayingPlaybackStateStopped);
end;

// -----------------------------------------------------------------------------

{$ELSE}
procedure MacMediaKeysInitialize;
begin
end;

// -----------------------------------------------------------------------------

procedure MacMediaKeysUpdate(const APlaying: Boolean);
begin
end;

// -----------------------------------------------------------------------------

procedure MacMediaKeysClear;
begin
end;
{$ENDIF}

// -----------------------------------------------------------------------------

end.
