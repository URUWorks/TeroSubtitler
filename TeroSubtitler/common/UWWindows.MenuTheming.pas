{*
 *  URUWorks Windows Menu Theming
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
 *  a modification of the original TWin32MenuStyler.
 *  Copyright (C) Alexey Torgashin, uvviewsoft.com
 *}

unit UWWindows.MenuTheming;

{$IFDEF WINDOWS}
// -----------------------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Windows, SysUtils, Classes, Graphics, Menus, Forms, Types, LCLType, LCLProc,
  Registry, ImgList, CommCtrl;

type

  { TUWCustomMenuTheme }

  TUWCustomMenuTheme = record
    ColorBk                    : TColor;
    ColorBkSelected            : TColor;
    ColorBkSelectedDisabled    : TColor; // used only if <> clNone
    ColorSelBorder             : TColor; // used only if <> clNone
    ColorFont                  : TColor;
    ColorFontSelected          : TColor; // used only if <> clNone
    ColorFontDisabled          : TColor;
    ColorFontShortcut          : TColor;
    CharCheckmark              : WideChar;
    CharRadiomark              : WideChar;
    CharSubmenu                : WideChar;
    FontName                   : String;
    FontSize                   : Integer;
    //indents in percents of average char width
    IndentMinPercents          : Integer; // indent from edges to separator line
    IndentBigPercents          : Integer; // indent from left edge to caption
    IndentIconPercents         : Integer; // indents around the icon
    IndentRightPercents        : Integer; // indent from right edge to end of shortcut text
    IndentSubmenuArrowPercents : Integer; // indent from right edge to submenu '>' char
  end;

  { TUWMenuTheming }

   TUWMenuTheming = class
   private
     procedure HandleMenuDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; AState: TOwnerDrawState);
     procedure HandleMenuMeasureItem(Sender: TObject; ACanvas: TCanvas; var AWidth, AHeight: Integer);
     procedure ApplyBackColor(h: HMENU; AReset: Boolean = False);
     procedure ApplyToMenu(const AMenu: TMenu);
     procedure ApplyToForm(const AForm: TForm);
     procedure ResetMenu(const AMenu: TMenu);
     procedure ResetForm(const AForm: TForm);
     procedure FixMenuBarThemingWhiteLine(const AForm: TForm; const AReset: Boolean = False);
     procedure DoPopupMenuHandler(Sender: TObject);
   public
     AlwaysShowAccelerators: Boolean;
     Theme: TUWCustomMenuTheme;
     constructor Create;
     procedure Apply(const AForm: TForm; const AValue: Boolean); overload;
     procedure Apply(const AForm: TForm; const AValue: Boolean; const ATheme: TUWCustomMenuTheme); overload;
   end;

var
  MenuThemingInstance: TUWMenuTheming = NIL;

// -----------------------------------------------------------------------------

implementation

const
  CSubClassId = 1;

var
  SubClassValid: Boolean = False;

// -----------------------------------------------------------------------------

function NewWndProc(Wnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM; uISubClass: UINT_PTR; dwRefData: DWORD_PTR): LRESULT; stdcall;
const
  OBJID_MENU = LONG($FFFFFFFD);

var
  dc: HDC;
  Brush: TBrush;
  ClientRect, WindowRect: TRect;
  mbi: MENUBARINFO;
begin
  Result := DefSubclassProc(Wnd, uMsg, wParam, lParam);

  case uMsg of
     WM_NCPAINT,
     WM_NCACTIVATE:
     begin
       FillChar(mbi{%H-}, SizeOf(mbi), 0);
       mbi.cbSize := SizeOf(mbi);
       if not GetMenuBarInfo(Wnd, OBJID_MENU, 0, @mbi) then
         Exit;

       dc := GetWindowDC(Wnd);
       try
         GetClientRect(Wnd, ClientRect);
         MapWindowPoints(Wnd, 0, ClientRect, 2);
         GetWindowRect(Wnd, WindowRect);
         OffsetRect(ClientRect, -WindowRect.Left, -WindowRect.Top);
         ClientRect.Bottom := ClientRect.Top;
         Dec(ClientRect.Top);

         Brush := TBrush.Create;
         Brush.Color := MenuThemingInstance.Theme.ColorBk;
         FillRect(dc, ClientRect, Brush.Reference.Handle);
         FreeAndNil(Brush);
       finally
         ReleaseDC(Wnd, dc);
       end;
     end;
  end;
end;

// -----------------------------------------------------------------------------

{ TUWMenuTheming }

// -----------------------------------------------------------------------------

constructor TUWMenuTheming.Create;
begin
  with Theme do
  begin
    ColorBk                    := RGBToColor(40, 40, 40);
    ColorBkSelected            := RGBToColor(83, 83, 83); //RGBToColor(20, 115, 230);
    ColorBkSelectedDisabled    := clNone;
    ColorSelBorder             := clNone;
    ColorFont                  := RGBToColor(240, 240, 240);
    ColorFontSelected          := clNone;
    ColorFontDisabled          := clMedGray;
    ColorFontShortcut          := clNone;
    CharCheckmark              := #$2713;
    CharRadiomark              := #$25CF;
    CharSubmenu                := #$2BC8;
    FontName                   := 'default';
    FontSize                   := 9;
    IndentMinPercents          := 50;
    IndentBigPercents          := 550;
    IndentIconPercents         := 40;
    IndentRightPercents        := 250;
    IndentSubmenuArrowPercents := 150;
  end;

  with TRegistry.Create do
  try
    RootKey := HKEY_CURRENT_USER;
    if OpenKey('Control Panel\Accessibility\Keyboard Preference', False) then
      AlwaysShowAccelerators := ReadString('On')='1';
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWMenuTheming.Apply(const AForm: TForm; const AValue: Boolean);
var
  C: TComponent;
begin
  if AValue then
  begin
    if OnMenuPopupHandler <> @DoPopupMenuHandler then
      OnMenuPopupHandler := @DoPopupMenuHandler;

    ApplyToForm(AForm);

    for C in AForm do
      if (C is TPopupMenu) then
        ApplyToMenu(TMenu(C));
  end
  else
  begin
    OnMenuPopupHandler := NIL;
    ResetForm(AForm);

    for C in AForm do
      if (C is TPopupMenu) then
        ResetMenu(TMenu(C));
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWMenuTheming.Apply(const AForm: TForm; const AValue: Boolean; const ATheme: TUWCustomMenuTheme);
begin
  Theme := ATheme;
  Apply(AForm, AValue);
end;

// -----------------------------------------------------------------------------

procedure TUWMenuTheming.ApplyBackColor(h: HMENU; AReset: Boolean = False);
var
  mi: TMENUINFO;
begin
  FillChar(mi{%H-}, SizeOf(TMENUINFO), 0);
  mi.cbSize := SizeOf(TMENUINFO);
  GetMenuInfo(h, @mi);
  mi.fMask := MIM_BACKGROUND or MIM_APPLYTOSUBMENUS;

  if AReset then
    mi.hbrBack := 0
  else
    mi.hbrBack := CreateSolidBrush(Theme.ColorBk);

  SetMenuInfo(h, @mi);
end;

// -----------------------------------------------------------------------------

procedure TUWMenuTheming.ApplyToMenu(const AMenu: TMenu);
begin
  with AMenu do
  begin
    OwnerDraw     := True;
    OnDrawItem    := @HandleMenuDrawItem;
    OnMeasureItem := @HandleMenuMeasureItem;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWMenuTheming.ApplyToForm(const AForm: TForm);
begin
  if Assigned(AForm.Menu) then
  begin
    ApplyToMenu(AForm.Menu);
    FixMenuBarThemingWhiteLine(AForm);
    ApplyBackColor(GetMenu(AForm.Handle));
    DrawMenuBar(AForm.Handle);
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWMenuTheming.ResetMenu(const AMenu: TMenu);
begin
  with AMenu do
  begin
    OwnerDraw     := False;
    OnDrawItem    := NIL;
    OnMeasureItem := NIL;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWMenuTheming.ResetForm(const AForm: TForm);
begin
  if Assigned(AForm.Menu) then
  begin
    ResetMenu(AForm.Menu);
    FixMenuBarThemingWhiteLine(AForm, True);
    ApplyBackColor(GetMenu(AForm.Handle), True);
    DrawMenuBar(AForm.Handle);
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWMenuTheming.HandleMenuDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; AState: TOwnerDrawState);

const
  cSampleShort = '0';
  cSampleTall  = 'Wj';

var
  mi: TMenuItem;
  Images: TCustomImageList;
  IconW: Integer;
  dx, dxCell, dxMin, dxBig, Y: Integer;
  ExtCell, ExtTall, Ext2: Types.TSize;
  NDrawFlags: UINT;
  bSel, bDisabled, bInBar, bHasSubmenu: Boolean;
  BufW: UnicodeString;
  mark: WideChar;
  R: TRect;
begin
  mi          := Sender as TMenuItem;
  IconW       := 0;
  bSel        := odSelected in AState;
  bDisabled   := odDisabled in AState;
  bInBar      := mi.IsInMenuBar;
  bHasSubmenu := (not bInBar) and (mi.Count>0);

  if bSel and (mi.Caption <> '-') then
  begin
    if bDisabled and (Theme.ColorBkSelectedDisabled <> clNone) then
      ACanvas.Brush.Color := Theme.ColorBkSelectedDisabled
    else
      ACanvas.Brush.Color := Theme.ColorBkSelected;

    if Theme.ColorSelBorder <> clNone then
      ACanvas.Pen.Color := Theme.ColorSelBorder
    else
      ACanvas.Pen.Color := ACanvas.Brush.Color;

    ACanvas.Rectangle(ARect);
  end
  else
  begin
    ACanvas.Brush.Color := Theme.ColorBk;
    ACanvas.FillRect(ARect);
  end;

  with ACanvas.Font do
  begin
    Name  := Theme.FontName;
    Size  := Theme.FontSize;
    Style := [];
  end;

  Windows.GetTextExtentPoint(ACanvas.Handle, PChar(cSampleShort), Length(cSampleShort), ExtCell);
  dxCell := ExtCell.cx;
  dxMin  := dxCell * Theme.IndentMinPercents div 100;
  dxBig  := dxCell * Theme.IndentBigPercents div 100;

  if Assigned(mi.Bitmap) then
    IconW := mi.Bitmap.Width;

  Images := mi.GetParentMenu.Images;
  if Assigned(Images) then
    IconW:= Max(IconW, Images.Width);

  if IconW > 0 then
    dxBig:= Max(dxBig, IconW + dxCell * Theme.IndentIconPercents * 2 div 100);

  if mi.IsLine then
  begin
    ACanvas.Pen.Color := Theme.ColorFontDisabled;
    Y := (ARect.Top+ARect.Bottom) div 2;
    ACanvas.Line(ARect.Left+dxBig, Y, ARect.Right-dxMin, Y);
    Exit;
  end;

  if bDisabled then
    ACanvas.Font.Color := Theme.ColorFontDisabled
  else
  if bSel and (Theme.ColorFontSelected<>clNone) then
    ACanvas.Font.Color := Theme.ColorFontSelected
  else
    ACanvas.Font.Color := Theme.ColorFont;

  Windows.GetTextExtentPoint(ACanvas.Handle, PChar(cSampleTall), Length(cSampleTall), ExtTall);

  if bInBar then
    dx := dxCell
  else
    dx := dxBig;

  Y := (ARect.Top+ARect.Bottom-ExtTall.cy) div 2;

  if AlwaysShowAccelerators then
    NDrawFlags:= 0
  else
  if odNoAccel in AState then
    NDrawFlags := DT_HIDEPREFIX
  else
    NDrawFlags := 0;

  BufW     := UTF8Decode(mi.Caption);
  R.Left   := ARect.Left+dx;
  R.Top    := Y;
  R.Right  := ARect.Right;
  R.Bottom := ARect.Bottom;
  Windows.DrawTextW(ACanvas.Handle, PWideChar(BufW), Length(BufW), R, NDrawFlags);

  if (not bInBar) and Assigned(mi.Bitmap) and (mi.Bitmap.Width > 0) then
  begin
    ACanvas.Draw(ARect.Left + (Theme.IndentIconPercents div 10){(dx-mi.Bitmap.Width) div 2},
                (ARect.Top+ARect.Bottom-mi.Bitmap.Height) div 2,
                 mi.Bitmap);
  end
  else if (not bInBar) and Assigned(Images) and (mi.ImageIndex>=0) then
  begin
    Images.Draw(ACanvas,
                ARect.Left + (dx-Images.Width) div 2,
               (ARect.Top+ARect.Bottom-Images.Height) div 2,
                mi.ImageIndex, not bDisabled);
  end else if mi.Checked then
  begin
    if mi.RadioItem then
      mark := Theme.CharRadiomark
    else
      mark := Theme.CharCheckmark;

    Windows.TextOutW(ACanvas.Handle, ARect.Left + ((dx-dxCell) div 4), Y, @mark, 1);
  end;

  if mi.ShortCut <> 0 then
  begin
    if bDisabled then
      ACanvas.Font.Color:= Theme.ColorFontDisabled
    else
      ACanvas.Font.Color:= Theme.ColorFontShortcut;

    BufW := UTF8Decode(ShortCutToText(mi.Shortcut));
    Windows.GetTextExtentPointW(ACanvas.Handle, PWideChar(BufW), Length(BufW), Ext2);
    Windows.TextOutW(ACanvas.Handle,
      ARect.Right - Ext2.cx - dxCell*Theme.IndentRightPercents div 100,
      Y, PWideChar(BufW), Length(BufW));
  end;

  if bHasSubmenu then
  begin
    if bDisabled then
      ACanvas.Font.Color := Theme.ColorFontDisabled
    else
      ACanvas.Font.Color := Theme.ColorFont;

    Windows.TextOutW(ACanvas.Handle,
      ARect.Right - dxCell*Theme.IndentSubmenuArrowPercents div 100 - 2,
      Y,
      @Theme.CharSubmenu,
      1);

    //block OS drawing of submenu arrow
    Windows.ExcludeClipRect(ACanvas.Handle,
      ARect.Right - dxCell*Theme.IndentRightPercents div 100,
      ARect.Top,
      ARect.Right,
      ARect.Bottom);
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWMenuTheming.HandleMenuMeasureItem(Sender: TObject; ACanvas: TCanvas; var AWidth, AHeight: Integer);
var
  mi: TMenuItem;
  S: String;
  Size: TSize;
begin
  if Theme.FontSize <= 9 then Exit;

  mi := Sender as TMenuItem;
  S  := mi.Caption;

  if S.Equals('-') then Exit;

  with ACanvas.Font do
  begin
    Name  := Theme.FontName;
    Size  := Theme.FontSize;
    Style := [];
  end;

  if not mi.IsInMenuBar then
  begin
    S := '     ' + S;

    if mi.ShortCut <> 0 then
      S += '  ' + ShortCutToText(mi.ShortCut);

    if mi.Count > 0 then;
      S += ' >';
  end;

  Size    := ACanvas.TextExtent(S);
  AWidth  := Max(AWidth, Size.cx);
  AHeight := Max(AHeight, Size.cy);
end;

// -----------------------------------------------------------------------------

procedure TUWMenuTheming.FixMenuBarThemingWhiteLine(const AForm: TForm; const AReset: Boolean = False);
begin
  if not AReset then
    SubClassValid := SetWindowSubclass(AForm.Handle, @NewWndProc, CSubClassId, DWORD_PTR(AForm))
  else if SubClassValid then
  begin
    RemoveWindowSubclass(AForm.Handle, @NewWndProc, CSubClassId);
    SubClassValid := False;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWMenuTheming.DoPopupMenuHandler(Sender: TObject);
begin
  ApplyBackColor(TMenu(Sender).Handle);
end;

// -----------------------------------------------------------------------------

initialization
  MenuThemingInstance := TUWMenuTheming.Create;

// -----------------------------------------------------------------------------

finalization
  FreeAndNil(MenuThemingInstance);
  OnMenuPopupHandler := NIL;

// -----------------------------------------------------------------------------
{$ELSE}
interface
implementation
{$ENDIF}

end.

