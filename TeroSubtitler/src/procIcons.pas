{*
 *  URUWorks Tero Subtitler
 *
 *  The contents of this file are used with permission, subject to
 *  the Mozilla Public License Version 2.0 (the "License"); you may
 *  not use this file except in compliance with the License. You may
 *  obtain a copy of the License at
 *  http://www.mozilla.org/MPL/2.0.html
 *
 *  Software distributed under the License is distributed on an
 *  "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 *  implied. See the License for the specific language governing
 *  rights and limitations under the License.
 *
 *  Copyright (C) 2023-2025 URUWorks, uruworks@gmail.com.
 *}

unit procIcons;

// Toolbar icons with an adjustable size (Settings > Appearance).
//
// The image lists of the main form (16x16) are kept as they are for the
// menus, the flat buttons and the list; the toolbars get a copy with the
// size chosen, as a scaled image list: the LCL uses the resolution that
// fits the screen (and the Retina scale on macOS).
//
// The icons are taken from "Icons/svg/<theme>/<name>.svg" or
// "Icons/svg/<name>.svg" (where "currentColor" is the text color of the
// theme) when they exist, so they are sharp at any size; if not, the
// 16x16 icon is scaled.

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, Graphics, ImgList;

const
  // Settings > Appearance > Toolbar icon size, 0 = the size of always
  ToolBarIconSizes: array[0..6] of Integer = (0, 16, 20, 24, 32, 40, 48);

{ the sized copies of ALight/ADark (owned by AOwner), ASize 0 = no copy }
procedure SetToolBarIconSize(const AOwner: TComponent; const ALight, ADark: TImageList; ASize: Integer; const ASVGFolder: String = '');
{ the image list for AToolbar: the sized copy of AImageList (only for the
  toolbars of the owner given above), it also sets the button size }
function ToolBarImages(const AToolbar: TToolBar; const AImageList: TImageList): TImageList;
{ the current size, 0 = the size of always }
function ToolBarIconSize: Integer;
{ index of ToolBarIconSizes for a size, or 0 }
function ToolBarIconSizeIndex(const ASize: Integer): Integer;

// -----------------------------------------------------------------------------

implementation

uses
  Forms, BGRABitmap, BGRABitmapTypes, BGRASVG;

const
  // name of the icon of each index of ImageList_Default/Dark, as the files
  // of the "images/default" and "images/dark" folders
  IconNames: array[0..103] of String = (
    'new', 'open', 'save', 'undo', 'redo', 'cut', 'copy', 'paste',
    'subinsert', 'subinsertbefore', 'subdelete', 'list', 'source',
    'transcription', 'translator', 'videopreview', 'wave', 'modetime',
    'modeframes', 'settings', 'subprev', 'subnext', 'fontbold', 'fontitalic',
    'fontunderline', 'fontstrikeout', 'fontclear', 'textleft', 'textcenter',
    'textright', 'textjustify', 'textbotttom', 'textmiddle', 'texttop',
    'timestart', 'timestop', 'timeduration', 'timepause', 'play', 'pause',
    'stop', 'scrolldown', 'playrate', 'rewind', 'forward', 'stepbackward',
    'stepforward', 'setinittime', 'setfinaltime', 'addsub', 'endsub',
    'playsel', 'playselbefore', 'playselafter', 'playfromsel', 'addsubsel',
    'square-brackets', 'sync1', 'sync2', 'zoom-in', 'zoom-out', 'zoom-sel',
    'volume', 'merge', 'textbreak', 'split', 'encoding', 'format', 'fps',
    'find', 'replace', 'validate', 'tickboxok', 'tickboxx', 'addtime',
    'wavecenter', 'shotchange', 'silent', 'actor', 'color', 'layout',
    'layout1', 'layout2', 'layout3', 'pushfirsttoprev', 'pushlasttonext',
    'pulllastfromprev', 'pulllfirstfromnext', 'wordup', 'worddown',
    'wordtoprev', 'wordtonext', 'dropper', 'mergewithnext',
    'mergewithprevious', 'cleartext', 'delsubsel', 'shotchangeadd',
    'shotchangedelete', 'shotchangenext', 'shotchangeprev',
    'splitatcursor', 'openvideo', 'mute');

  // "currentColor" of the svg files without theme
  SVGTextColor: array[Boolean] of String = ('#333333', '#E6E6E6'); // light, dark

var
  FOwner      : TComponent = NIL;
  FSize       : Integer = 0;
  FSrcLight   : TImageList = NIL;
  FSrcDark    : TImageList = NIL;
  FSizedLight : TImageList = NIL;
  FSizedDark  : TImageList = NIL;

// -----------------------------------------------------------------------------

function ToolBarIconSize: Integer;
begin
  Result := FSize;
end;

// -----------------------------------------------------------------------------

function ToolBarIconSizeIndex(const ASize: Integer): Integer;
var
  i: Integer;
begin
  for i := 0 to High(ToolBarIconSizes) do
    if ToolBarIconSizes[i] = ASize then
      Exit(i);

  Result := 0;
end;

// -----------------------------------------------------------------------------

function LoadSVG(const AFolder, AName: String; const ADark: Boolean): TBGRASVG;
const
  Themes: array[Boolean] of String = ('default', 'dark');
var
  s: String;
  sl: TStringList;
begin
  Result := NIL;
  if (AFolder = '') or (AName = '') then Exit;

  sl := TStringList.Create;
  try
    try
      s := IncludeTrailingPathDelimiter(AFolder) + Themes[ADark] + PathDelim + AName + '.svg';
      if FileExists(s) then
      begin
        sl.LoadFromFile(s);
        Result := TBGRASVG.CreateFromString(sl.Text);
      end
      else
      begin
        s := IncludeTrailingPathDelimiter(AFolder) + AName + '.svg';
        if FileExists(s) then
        begin
          sl.LoadFromFile(s);
          Result := TBGRASVG.CreateFromString(StringReplace(sl.Text,
            'currentColor', SVGTextColor[ADark], [rfReplaceAll, rfIgnoreCase]));
        end;
      end;
    except
      FreeAndNil(Result); // a bad svg file, the png is used
    end;
  finally
    sl.Free;
  end;
end;

// -----------------------------------------------------------------------------

{ the svg drawn at ASize x ASize, as a png (a TCustomBitmap with alpha) }
function SVGToPNG(const ASVG: TBGRASVG; const ASize: Integer): TPortableNetworkGraphic;
var
  bmp: TBGRABitmap;
  ms: TMemoryStream;
begin
  Result := TPortableNetworkGraphic.Create;
  bmp := TBGRABitmap.Create(ASize, ASize, BGRAPixelTransparent);
  ms := TMemoryStream.Create;
  try
    ASVG.StretchDraw(bmp.Canvas2D, 0, 0, ASize, ASize, True);
    bmp.SaveToStreamAsPng(ms);
    ms.Position := 0;
    Result.LoadFromStream(ms);
  finally
    ms.Free;
    bmp.Free;
  end;
end;

// -----------------------------------------------------------------------------

function CreateSizedList(const AOwner: TComponent; const ASource: TImageList;
  const ASize: Integer; const ADark: Boolean; const ASVGFolder: String): TImageList;
var
  Widths : array of Integer;
  Images : array of TCustomBitmap;
  Bmp    : TCustomBitmap;
  SVG    : TBGRASVG;
  i, j   : Integer;

  procedure AddWidth(const AWidth: Integer);
  var
    k, n: Integer;
  begin
    if AWidth < ASize then Exit;
    for k := 0 to High(Widths) do
      if Widths[k] = AWidth then Exit;
    // sorted from the smallest to the biggest, as AddMultipleResolutions wants
    n := Length(Widths);
    SetLength(Widths, n+1);
    k := n;
    while (k > 0) and (Widths[k-1] > AWidth) do
    begin
      Widths[k] := Widths[k-1];
      Dec(k);
    end;
    Widths[k] := AWidth;
  end;

begin
  Result := TImageList.Create(AOwner);
  Result.Scaled := True;
  Result.Width  := ASize;
  Result.Height := ASize;

  // the size at 100%, at the dpi of the screen and the usual scales; the
  // LCL scales one of these for any other
  Widths := NIL;
  AddWidth(ASize);
  AddWidth(Round(ASize * Screen.PixelsPerInch / 96));
  AddWidth(Round(ASize * 1.5));
  AddWidth(ASize * 2);
  {$IFDEF DARWIN}
  AddWidth(Round(ASize * Screen.PixelsPerInch / 96) * 2); // Retina
  {$ENDIF}
  Result.RegisterResolutions(Widths);

  Result.BeginUpdate;
  try
    for i := 0 to ASource.Count-1 do
    begin
      SVG := NIL;
      if i <= High(IconNames) then
        SVG := LoadSVG(ASVGFolder, IconNames[i], ADark);

      if SVG <> NIL then
      begin
        Images := NIL;
        SetLength(Images, Length(Widths));
        try
          for j := 0 to High(Widths) do
            Images[j] := SVGToPNG(SVG, Widths[j]);
          Result.AddMultipleResolutions(Images);
        finally
          for j := 0 to High(Images) do
            Images[j].Free;
          SVG.Free;
        end;
      end
      else
      begin
        // the 16x16 icon, the LCL scales it to each size
        Bmp := TBitmap.Create;
        try
          ASource.GetBitmap(i, Bmp);
          Result.AddMultipleResolutions([Bmp]);
        finally
          Bmp.Free;
        end;
      end;
    end;
  finally
    Result.EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

procedure SetToolBarIconSize(const AOwner: TComponent; const ALight, ADark: TImageList; ASize: Integer; const ASVGFolder: String = '');
var
  OldLight, OldDark: TImageList;
begin
  if ToolBarIconSizeIndex(ASize) = 0 then ASize := 0;

  OldLight := FSizedLight;
  OldDark  := FSizedDark;
  FSizedLight := NIL;
  FSizedDark  := NIL;

  FOwner    := AOwner;
  FSrcLight := ALight;
  FSrcDark  := ADark;
  FSize     := ASize;

  if (ASize > 0) and (AOwner <> NIL) then
  begin
    if ALight <> NIL then
      FSizedLight := CreateSizedList(AOwner, ALight, ASize, False, ASVGFolder);
    if ADark <> NIL then
      FSizedDark := CreateSizedList(AOwner, ADark, ASize, True, ASVGFolder);
  end;

  // the toolbars with the old lists get NIL (free notification) until the
  // theme is applied again, which gives them the new ones
  OldLight.Free;
  OldDark.Free;
end;

// -----------------------------------------------------------------------------

function ToolBarImages(const AToolbar: TToolBar; const AImageList: TImageList): TImageList;
var
  Sized: Boolean;
begin
  Result := AImageList;
  Sized  := False;

  if (AToolbar <> NIL) and (FOwner <> NIL) and (AToolbar.Owner = FOwner) and (FSize > 0) then
  begin
    if (AImageList = FSrcLight) and (FSizedLight <> NIL) then
    begin
      Result := FSizedLight;
      Sized  := True;
    end
    else if (AImageList = FSrcDark) and (FSizedDark <> NIL) then
    begin
      Result := FSizedDark;
      Sized  := True;
    end;
  end;

  // the default button size is for 16x16 icons (23x22), the same margins;
  // the toolbars of the main form use the default size (-1) in the lfm
  if Sized then
    AToolbar.SetButtonSize(AToolbar.Scale96ToFont(FSize + 7), AToolbar.Scale96ToFont(FSize + 6))
  else if (AToolbar <> NIL) and (FOwner <> NIL) and (AToolbar.Owner = FOwner) then
    AToolbar.SetButtonSize(-1, -1);
end;

// -----------------------------------------------------------------------------

end.
