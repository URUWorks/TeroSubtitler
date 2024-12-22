{*
 *  URUWorks
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
 *  Copyright (C) 2023 URUWorks, uruworks@gmail.com.
 *}

unit procColorTheme;

{$mode ObjFPC}{$H+}
{$IFDEF DARWIN}
{$modeswitch objectivec1}
{$ENDIF}

interface

uses
  Classes, SysUtils, Graphics, Forms, ComCtrls, Controls, ExtCtrls,
  Menus, Buttons, BGRABitmap, BGRABitmapTypes, UWLayout, UWMemo,
  UWFlatButton, UWSeekBar, laz.VirtualTrees, WAVDisplayer, ATSynEdit, Grids
  {$IFDEF DARWIN}, MacOSAll, CocoaAll, CocoaUtils{$ENDIF}
  {$IFDEF WINDOWS}, Windows, Win32Proc, Registry,
    StdCtrls, UWStatusBar, UWCheckBox, UWRadioButton, UWHotKey, UWTimeEdit,
    UWEditAction, Spin, CheckLst{$ENDIF};

type

  { TColors }

  TColors = record
    Form           : TColor;
    Text           : TColor;
    Highlight      : TColor;
    HighlightText  : TColor;
    BtnHighlight   : TColor;
    BtnChecked     : TColor;
    Window         : TColor;
    SeekBack       : TColor;
    SeekFill       : TColor;
    SeekBtn        : TColor;
    ItemBackground : TColor;
    ItemHeader     : TColor;
    ItemSelHeader  : TColor;
    WaveBackground : TColor;
    Waveform       : TColor;
    WaveGrid       : TColor;
  end;

  TColorMode = (cmAuto, cmLight, cmDark);

  { TColorTheme }

  TColorTheme = class
  private
    LastSystemDarkTheme: Boolean;
    procedure ApplyToControl(const Ctrl: TControl; const AColorMode: TColorMode; const AImageList: TImageList = NIL);
    procedure Apply(const ALayout: TUWLayout; const AColorMode: TColorMode; const AImageList: TImageList = NIL; const ARecursive: Boolean = True); overload;
    procedure Apply(const AMemo: TUWMemo; const AColorMode: TColorMode); overload;
    procedure Apply(const AFlatButton: TUWFlatButton; const AColorMode: TColorMode; const AImageList: TImageList = NIL); overload;
    procedure Apply(const ASeekBar: TUWSeekBar; const AColorMode: TColorMode); overload;
    procedure Apply(const AWave: TUWWaveformDisplayer; const AColorMode: TColorMode); overload;
    procedure Apply(const AMenu: TPopupMenu; const AColorMode: TColorMode; const AImageList: TImageList = NIL); overload;
    procedure Apply(const AToolbar: TToolbar; const AColorMode: TColorMode; const AImageList: TImageList = NIL); overload;
    procedure Apply(const ACoolbar: TCoolbar; const AColorMode: TColorMode; const AImageList: TImageList = NIL); overload;
    {$IFDEF WINDOWS}
    procedure Apply(const AStringGrid: TStringGrid; const AColorMode: TColorMode);
    procedure Apply(const APanel: TPanel; const AColorMode: TColorMode);
    procedure Apply(const AGroupBox: TGroupBox; const AColorMode: TColorMode; const AImageList: TImageList = NIL; const ARecursive: Boolean = True);
    procedure Apply(const AMemo: TMemo; const AColorMode: TColorMode); overload;
    procedure Apply(const ATimeEdit: TUWTimeEdit; const AColorMode: TColorMode); overload;
    procedure Apply(const AEditAction: TUWEditAction; const AColorMode: TColorMode); overload;
    procedure Apply(const ALabel: TLabel; const AColorMode: TColorMode); overload;
    procedure Apply(const ACombo: TComboBox; const AColorMode: TColorMode); overload;
    procedure Apply(const AEdit: TEdit; const AColorMode: TColorMode); overload;
    procedure Apply(const AHotKey: TUWHotKey; const AColorMode: TColorMode); overload;
    procedure Apply(const AListBox: TListBox; const AColorMode: TColorMode); overload;
    procedure Apply(const ASpinEdit: TSpinEdit; const AColorMode: TColorMode); overload;
    procedure Apply(const ACheckBox: TCheckBox; const AColorMode: TColorMode); overload;
    procedure Apply(const ARadioButton: TRadioButton; const AColorMode: TColorMode); overload;
    procedure Apply(const ACheckGroup: TCheckGroup; const AColorMode: TColorMode); overload;
    procedure Apply(const ARadioGroup: TRadioGroup; const AColorMode: TColorMode); overload;
    procedure Apply(const ACheckListBox: TCheckListBox; const AColorMode: TColorMode); overload;
    procedure Apply(const AStatusBar: TUWStatusBar; const AColorMode: TColorMode); overload;
    procedure Apply(const AVST: TLazVirtualStringTree; const AColorMode: TColorMode); overload;
    procedure Apply(const ASplitter: TSplitter; const AColorMode: TColorMode); overload;
    procedure ToolBarDoPaintButton(Sender: TToolButton; State: Integer);
    procedure SplitterDoPaint(Sender: TObject);
    {$ENDIF}
  public
    LightColors : TColors;
    DarkColors  : TColors;
    ColorMode   : TColorMode;
    constructor Create;
    function IsSystemDarkTheme: Boolean;
    function HasSystemColorThemeChanged: Boolean;
    procedure InitializeDefaultColors;
    function Colors: TColors;
    function GetRealColorMode: TColorMode;
    procedure Apply(const AColorMode: TColorMode; const AImageListDark: TImageList = NIL; const AImageListLight: TImageList = NIL; const ARecursive: Boolean = True);
    procedure Apply(const AForm: TForm; const AColorMode: TColorMode; const AImageListDark: TImageList = NIL; const AImageListLight: TImageList = NIL; const ARecursive: Boolean = True); overload;
    procedure Apply(const ASynEdit: TATSynEdit; const AColorMode: TColorMode);
  end;

var
  ColorThemeInstance: TColorTheme;

// -----------------------------------------------------------------------------

implementation

{$IFDEF WINDOWS}
uses
  UWWindows.DarkTheme, UWWindows.MenuTheming;
{$ENDIF}

// -----------------------------------------------------------------------------

{ Helpers }

// -----------------------------------------------------------------------------

{$IFDEF DARWIN}
// True, if this app runs on macOS 10.14 Mojave or newer
function IsMojaveOrNewer: Boolean;
var
  minOsVer: NSOperatingSystemVersion;
begin
  // Setup minimum version (Mojave)
  minOsVer.majorVersion:= 10;
  minOsVer.minorVersion:= 14;
  minOsVer.patchVersion:= 0;

  // Check minimum version
  Result := NSProcessInfo.ProcessInfo.isOperatingSystemAtLeastVersion(minOSVer);
end;

// -----------------------------------------------------------------------------

function IsMacOSDarkTheme: Boolean;
begin
  Result := Pos('dark', LowerCase(CFStringToStr(CFStringRef(NSApp.effectiveAppearance.name)))) > 0;
end;
{$ENDIF}

// -----------------------------------------------------------------------------

{$IFDEF WINDOWS}
// by "jwdietrich" from Lazarus forum
// IsWinDarkTheme: Detects if the Dark Theme (true) has been enabled or not (false)

type
  TWinDarkThemeMode = (dtmLight, dtmDark, dtmUnknown);

function IsWinDarkTheme: TWinDarkThemeMode;
const
  KEYPATH = '\Software\Microsoft\Windows\CurrentVersion\Themes\Personalize';
  KEYNAME = 'AppsUseLightTheme';
begin
  Result := dtmUnknown;
  with TRegistry.Create do
  try
    RootKey := HKEY_CURRENT_USER;
    if OpenKeyReadOnly(KEYPATH) then
      begin
        if ValueExists(KEYNAME) then
        begin
          if ReadBool(KEYNAME) then
            Result := dtmLight
          else
            Result := dtmDark;
        end;
      end;
  finally
    Free;
  end;
end;
{$ENDIF}

// -----------------------------------------------------------------------------

function IsLclDarkTheme: Boolean;
const
  cMax = $A0;
var
  N: TColor;
begin
  N := ColorToRGB(clWindow);
  Result := (Red(N) < cMax) and (Green(N) < cMax) and (Blue(N) < cMax);
end;

// -----------------------------------------------------------------------------

{ TColorTheme }

// -----------------------------------------------------------------------------

constructor TColorTheme.Create;
begin
  LastSystemDarkTheme := IsSystemDarkTheme;
  InitializeDefaultColors;
end;

// -----------------------------------------------------------------------------

function TColorTheme.IsSystemDarkTheme: Boolean;
begin
  {$IFDEF DARWIN_DARK_THEME}
  if IsMojaveOrNewer then
    Exit(IsMacOSDarkTheme);
  {$ENDIF}
  {$IFDEF WINDOWS}
  case IsWinDarkTheme of
    dtmLight : Exit(False);
    dtmDark  : Exit(True);
  end;
  {$ENDIF}
  Result := IsLclDarkTheme;
end;

// -----------------------------------------------------------------------------

function TColorTheme.HasSystemColorThemeChanged: Boolean;
var
  NewState: Boolean;
begin
  NewState := IsSystemDarkTheme;
  if NewState <> LastSystemDarkTheme then
  begin
    Result := True;
    LastSystemDarkTheme:= NewState;
  end
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

procedure TColorTheme.InitializeDefaultColors;
begin
  with LightColors do
  begin
    if not LastSystemDarkTheme then
    begin
      Form           := clForm;
      Text           := clWindowText;
      Highlight      := clHighlight;
      HighlightText  := clHighlightText;
      BtnHighlight   := clBtnFace;
      BtnChecked     := clBtnShadow;
      Window         := clWindow;
      SeekBack       := clGray; //$D3D3D3;
      SeekFill       := clHighlight; //$00A56E3A;
      SeekBtn        := $00A56E3A; //$696969;
      ItemBackground := clMenu;
      ItemHeader     := clInactiveCaption;
      ItemSelHeader  := clActiveCaption;
    end
    else
    begin
      Form           := RGBToColor(240,240,240);
      Text           := RGBToColor(0,0,0);
      Highlight      := RGBToColor(0,120,215);
      HighlightText  := RGBToColor(255,255,255);
      BtnHighlight   := RGBToColor(240,240,240);
      BtnChecked     := RGBToColor(160,160,160);
      Window         := RGBToColor(255,255,255);
      SeekBack       := clGray; //$D3D3D3;
      SeekFill       := RGBToColor(0,120,215);
      SeekBtn        := $00A56E3A; //$696969;
      ItemBackground := RGBToColor(240,240,240);
      ItemHeader     := RGBToColor(191,205,219);
      ItemSelHeader  := RGBToColor(153,180,209);
    end;

    WaveBackground := RGBToColor(10, 10, 10);
    Waveform       := RGBToColor(13, 132, 255); // RGBToColor(66, 255, 66);
    WaveGrid       := RGBToColor(40, 40, 40);
  end;

  with DarkColors do
  begin
    Form           := RGBToColor(50, 50, 50);
    Text           := RGBToColor(230, 230, 230);
    Highlight      := RGBToColor(20, 115, 230);
    HighlightText  := RGBToColor(255, 255, 255);
    BtnHighlight   := RGBToColor(70, 70, 70);
    BtnChecked     := RGBToColor(30, 30, 30);
    Window         := RGBToColor(40, 40, 40);
    SeekBack       := RGBToColor(20, 20, 20);
    SeekFill       := Highlight;
    SeekBtn        := BtnHighlight;

    ItemBackground := RGBToColor(40, 40, 40);
    ItemHeader     := RGBToColor(83, 83, 83);
    ItemSelHeader  := RGBToColor(8, 79, 145);

    WaveBackground := RGBToColor(10, 10, 10);
    Waveform       := RGBToColor(0, 84, 184);
    WaveGrid       := RGBToColor(40, 40, 40);
  end;

  if LastSystemDarkTheme then
    ColorMode := cmDark
  else
    ColorMode := cmLight;
end;

// -----------------------------------------------------------------------------

function TColorTheme.Colors: TColors;
begin
  if GetRealColorMode = cmDark then
    Result := DarkColors
  else
    Result := LightColors;
end;

// -----------------------------------------------------------------------------

function TColorTheme.GetRealColorMode: TColorMode;
begin
  if ColorMode = cmAuto then
  begin
    if LastSystemDarkTheme then
      Result := cmDark
    else
      Result := cmLight;
  end
  else
    Result := ColorMode;
end;

// -----------------------------------------------------------------------------

procedure TColorTheme.Apply(const AColorMode: TColorMode; const AImageListDark: TImageList = NIL; const AImageListLight: TImageList = NIL; const ARecursive: Boolean = True);
var
  i: Integer;
begin
  for i := 0 to Screen.FormCount-1 do
    Apply(Screen.Forms[i], AColorMode, AImageListDark, AImageListLight, ARecursive);
end;

// -----------------------------------------------------------------------------

procedure TColorTheme.ApplyToControl(const Ctrl: TControl; const AColorMode: TColorMode; const AImageList: TImageList = NIL);
begin
  if Ctrl is TUWLayout then
    Apply(TUWLayout(Ctrl), AColorMode, AImageList)
  else if (Ctrl is TUWMemo) then
    Apply(TUWMemo(Ctrl), AColorMode)
  else if Ctrl is TUWFlatButton then
    Apply(TUWFlatButton(Ctrl), AColorMode, AImageList)
  else if Ctrl is TUWSeekBar then
    Apply(TUWSeekBar(Ctrl), AColorMode)
  else if Ctrl is TUWWaveformDisplayer then
    Apply(TUWWaveformDisplayer(Ctrl), AColorMode)
  else if Ctrl is TATSynEdit then
    Apply(TATSynEdit(Ctrl), AColorMode)
  else if Ctrl is TToolbar then
    Apply(TToolbar(Ctrl), AColorMode, AImageList)
  else if Ctrl is TCoolbar then
    Apply(TCoolbar(Ctrl), AColorMode, AImageList)
  {$IFDEF WINDOWS}
  else if (Ctrl is TStringGrid) then
    Apply(TStringGrid(Ctrl), AColorMode)
  else if (Ctrl is TPanel) then
    Apply(TPanel(Ctrl), AColorMode)
  else if (Ctrl is TGroupBox) then
    Apply(TGroupBox(Ctrl), AColorMode)
  else if (Ctrl is TMemo) then
    Apply(TMemo(Ctrl), AColorMode)
  else if Ctrl is TUWTimeEdit then
    Apply(TUWTimeEdit(Ctrl), AColorMode)
  else if Ctrl is TUWEditAction then
    Apply(TUWEditAction(Ctrl), AColorMode)
  else if Ctrl is TLabel then
    Apply(TLabel(Ctrl), AColorMode)
  else if Ctrl is TComboBox then
    Apply(TComboBox(Ctrl), AColorMode)
  else if Ctrl is TEdit then
    Apply(TEdit(Ctrl), AColorMode)
  else if Ctrl is TUWHotKey then
    Apply(TUWHotKey(Ctrl), AColorMode)
  else if Ctrl is TListBox then
    Apply(TListBox(Ctrl), AColorMode)
  else if Ctrl is TSpinEdit then
    Apply(TSpinEdit(Ctrl), AColorMode)
  else if Ctrl is TCheckBox then
    Apply(TCheckBox(Ctrl), AColorMode)
  else if Ctrl is TRadioButton then
    Apply(TRadioButton(Ctrl), AColorMode)
  else if Ctrl is TCheckGroup then
    Apply(TCheckGroup(Ctrl), AColorMode)
  else if Ctrl is TRadioGroup then
    Apply(TRadioGroup(Ctrl), AColorMode)
  else if Ctrl is TCheckListBox then
    Apply(TCheckListBox(Ctrl), AColorMode)
  else if Ctrl is TUWStatusBar then
    Apply(TUWStatusBar(Ctrl), AColorMode)
  else if Ctrl is TLazVirtualStringTree then
    Apply(TLazVirtualStringTree(Ctrl), AColorMode)
  else if Ctrl is TSplitter then
    Apply(TSplitter(Ctrl), AColorMode)
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

procedure TColorTheme.Apply(const AForm: TForm; const AColorMode: TColorMode; const AImageListDark: TImageList = NIL; const AImageListLight: TImageList = NIL; const ARecursive: Boolean = True);
var
  i: Integer;
  AImageList: TImageList;
begin
  case AColorMode of
    cmAuto  : if LastSystemDarkTheme then
                ColorMode := cmDark
              else
                ColorMode := cmLight;
    cmLight : ColorMode := cmLight;
    cmDark  : ColorMode := cmDark;
  end;

  if ColorMode = cmDark then
    AImageList := AImageListDark
  else
    AImageList := AImageListLight;

  AForm.Color      := Colors.Form;
  AForm.Font.Color := Colors.Text;

  if AForm.Menu <> NIL then
    AForm.Menu.Images := AImageList;

  for i := 0 to AForm.ComponentCount-1 do
    if AForm.Components[i] is TPopupMenu then
      Apply(TPopupMenu(AForm.Components[i]), ColorMode, AImageList);

  if ARecursive then
    for i := 0 to AForm.ControlCount-1 do
      ApplyToControl(AForm.Controls[i], ColorMode, AImageList);

  {$IFDEF WINDOWS}
  MenuThemingInstance.Apply(AForm, ColorMode = cmDark);
  with TUWDarkMode.Create(AForm, ColorMode = cmDark) do Free;
  {$ENDIF}

  ColorMode := AColorMode;
end;

// -----------------------------------------------------------------------------

procedure TColorTheme.Apply(const ALayout: TUWLayout; const AColorMode: TColorMode; const AImageList: TImageList = NIL; const ARecursive: Boolean = True);
var
  i: Integer;
begin
  {$IFDEF WINDOWS}
  ALayout.Color      := Colors.Form;
  ALayout.Font.Color := Colors.Text;
  {$ENDIF}

  if ARecursive then
    for i := 0 to ALayout.ControlCount-1 do
      ApplyToControl(ALayout.Controls[i], AColorMode, AImageList);
end;

// -----------------------------------------------------------------------------

{$IFDEF WINDOWS}
procedure TColorTheme.Apply(const AGroupBox: TGroupBox; const AColorMode: TColorMode; const AImageList: TImageList = NIL; const ARecursive: Boolean = True);
var
  i: Integer;
begin
  AGroupBox.Color      := Colors.Form;
  AGroupBox.Font.Color := Colors.Text;

//  if AGroupBox is TUWGroupBox then
//    (AGroupBox as TUWGroupBox).OwnerDraw := (AColorMode <> cmLight);

  if ARecursive then
    for i := 0 to AGroupBox.ControlCount-1 do
      ApplyToControl(AGroupBox.Controls[i], AColorMode, AImageList);
end;

// -----------------------------------------------------------------------------

procedure TColorTheme.Apply(const AMemo: TMemo; const AColorMode: TColorMode);
begin
  AMemo.Color      := Colors.Window;
  AMemo.Font.Color := Colors.Text;

  if AColorMode = cmLight then
    AMemo.BorderStyle := bsSingle
  else
    AMemo.BorderStyle := bsNone;
end;
{$ENDIF}

// -----------------------------------------------------------------------------

procedure TColorTheme.Apply(const AMemo: TUWMemo; const AColorMode: TColorMode);
begin
  {$IFDEF WINDOWS}
  AMemo.Color      := Colors.Window;
  AMemo.Font.Color := Colors.Text;
  {$ENDIF}

  if AColorMode = cmLight then
  begin
    {$IFDEF WINDOWS}
    AMemo.BorderStyle := bsSingle;
    {$ENDIF}
    if AMemo is TUWMemo then
    begin
      TUWMemo(AMemo).CPSBar.BackColor   := Colors.Form;
      TUWMemo(AMemo).CPSBar.BorderColor := clInactiveBorder;
    end;
  end
  else
  begin
    {$IFDEF WINDOWS}
    AMemo.BorderStyle := bsNone;
    {$ENDIF}
    if AMemo is TUWMemo then
    begin
      TUWMemo(AMemo).CPSBar.BackColor   := Colors.Window;
      TUWMemo(AMemo).CPSBar.BorderColor := Colors.Form;
    end;
  end;
end;

// -----------------------------------------------------------------------------

{$IFDEF WINDOWS}
procedure TColorTheme.Apply(const AStringGrid: TStringGrid; const AColorMode: TColorMode);
begin
  AStringGrid.Color      := Colors.Window;
  AStringGrid.Font.Color := Colors.Text;

  if AColorMode = cmLight then
    AStringGrid.BorderStyle := bsSingle
  else
    AStringGrid.BorderStyle := bsNone;
end;

// -----------------------------------------------------------------------------

procedure TColorTheme.Apply(const APanel: TPanel; const AColorMode: TColorMode);
begin
  APanel.Color      := Colors.Window;
  APanel.Font.Color := Colors.Text;

  if AColorMode = cmLight then
    APanel.BevelOuter := bvRaised
  else
    APanel.BevelOuter := bvNone;
end;

// -----------------------------------------------------------------------------

procedure TColorTheme.Apply(const ATimeEdit: TUWTimeEdit; const AColorMode: TColorMode);
begin
  ATimeEdit.Color      := Colors.Window;
  ATimeEdit.Font.Color := Colors.Text;

  if AColorMode = cmLight then
    ATimeEdit.BorderStyle := bsSingle
  else
    ATimeEdit.BorderStyle := bsNone;
end;

// -----------------------------------------------------------------------------

procedure TColorTheme.Apply(const AEditAction: TUWEditAction; const AColorMode: TColorMode); overload;
begin
  AEditAction.Color      := Colors.Window;
  AEditAction.Font.Color := Colors.Text;

  if AColorMode = cmLight then
    AEditAction.BorderStyle := bsSingle
  else
    AEditAction.BorderStyle := bsNone;
end;
{$ENDIF}

// -----------------------------------------------------------------------------

procedure TColorTheme.Apply(const AFlatButton: TUWFlatButton; const AColorMode: TColorMode; const AImageList: TImageList = NIL);
begin
  AFlatButton.Images          := AImageList;
  AFlatButton.Color           := Colors.Form;
  AFlatButton.HightlightColor := Colors.BtnHighlight;
  AFlatButton.BorderColor     := Colors.Window;
  AFlatButton.CheckedColor    := Colors.Window;
  AFlatButton.DrawBuffer;
end;

// -----------------------------------------------------------------------------

procedure TColorTheme.Apply(const ASeekBar: TUWSeekBar; const AColorMode: TColorMode);
begin
  ASeekBar.BackColor    := Colors.SeekBack;
  ASeekBar.FillColor    := Colors.SeekFill;
  ASeekBar.ButtonColor  := Colors.SeekBtn;
  ASeekBar.BtnDownColor := Colors.BtnChecked;
end;

// -----------------------------------------------------------------------------

procedure TColorTheme.Apply(const ACoolbar: TCoolbar; const AColorMode: TColorMode; const AImageList: TImageList = NIL);
var
  i: Integer;
begin
  ACoolbar.Themed := False; //(AColorMode = cmLight);
  ACoolbar.Color  := Colors.Form;

  for i := 0 to ACoolbar.ControlCount-1 do
    ApplyToControl(ACoolbar.Controls[i], AColorMode, AImageList);
end;

// -----------------------------------------------------------------------------

{$IFDEF WINDOWS}
procedure TColorTheme.Apply(const ALabel: TLabel; const AColorMode: TColorMode);
begin
  ALabel.Font.Color := Colors.Text;
end;

// -----------------------------------------------------------------------------

procedure TColorTheme.Apply(const ACombo: TComboBox; const AColorMode: TColorMode);
begin
  ACombo.Color      := Colors.Window;
  ACombo.Font.Color := Colors.Text;

  if AColorMode = cmDark then
    SetWindowRgn(ACombo.Handle, CreateRectRgn(2, 2, ACombo.Width-2, ACombo.Height-2), True)
  else
    SetWindowRgn(ACombo.Handle, CreateRectRgn(0, 0, ACombo.Width, ACombo.Height), True);
end;

// -----------------------------------------------------------------------------

procedure TColorTheme.Apply(const AEdit: TEdit; const AColorMode: TColorMode);
begin
  AEdit.Color      := Colors.Window;
  AEdit.Font.Color := Colors.Text;
end;

// -----------------------------------------------------------------------------

procedure TColorTheme.Apply(const AHotKey: TUWHotKey; const AColorMode: TColorMode);
begin
  AHotKey.Color      := Colors.Window;
  AHotKey.Font.Color := Colors.Text;
end;

// -----------------------------------------------------------------------------

procedure TColorTheme.Apply(const AListBox: TListBox; const AColorMode: TColorMode);
begin
  AListBox.Color      := Colors.Window;
  AListBox.Font.Color := Colors.Text;

{  if AColorMode = cmLight then
    AListBox.BorderStyle := bsSingle
  else
    AListBox.BorderStyle := bsNone;}
end;

// -----------------------------------------------------------------------------

procedure TColorTheme.Apply(const ASpinEdit: TSpinEdit; const AColorMode: TColorMode);
begin
  ASpinEdit.Color      := Colors.Window;
  ASpinEdit.Font.Color := Colors.Text;

  if AColorMode = cmLight then
    ASpinEdit.BorderStyle := bsSingle
  else
    ASpinEdit.BorderStyle := bsNone;
end;

// -----------------------------------------------------------------------------

procedure TColorTheme.Apply(const ACheckBox: TCheckBox; const AColorMode: TColorMode);
begin
  ACheckBox.Color := Colors.Window;
  if ACheckBox is TUWCheckBox then
    with TUWCheckBox(ACheckBox) do
      LabelCheckBox.Font.Color := Colors.Text;
end;

// -----------------------------------------------------------------------------

procedure TColorTheme.Apply(const ARadioButton: TRadioButton; const AColorMode: TColorMode);
begin
  ARadioButton.Color := Colors.Window;
  if ARadioButton is TUWRadioButton then
    with TUWRadioButton(ARadioButton) do
      LabelRadioButton.Font.Color := Colors.Text;
end;

// -----------------------------------------------------------------------------

procedure TColorTheme.Apply(const ACheckGroup: TCheckGroup; const AColorMode: TColorMode);
begin
//  ACheckGroup.Color      := Colors.Window;
  ACheckGroup.Font.Color := Colors.Text;
end;

// -----------------------------------------------------------------------------

procedure TColorTheme.Apply(const ARadioGroup: TRadioGroup; const AColorMode: TColorMode);
begin
//  ARadioGroup.Color      := Colors.Window;
  ARadioGroup.Font.Color := Colors.Text;
end;

// -----------------------------------------------------------------------------

procedure TColorTheme.Apply(const ACheckListBox: TCheckListBox; const AColorMode: TColorMode);
begin
  ACheckListBox.Color      := Colors.Window;
  ACheckListBox.Font.Color := Colors.Text;

  if AColorMode = cmDark then
    ACheckListBox.BorderStyle := bsNone
  else
    ACheckListBox.BorderStyle := bsSingle;
end;

// -----------------------------------------------------------------------------

procedure TColorTheme.Apply(const AStatusBar: TUWStatusBar; const AColorMode: TColorMode);
begin
  if AColorMode = cmDark then
  begin
    AStatusBar.Color          := Colors.Form;
    AStatusBar.Font.Color     := Colors.Text;
    AStatusBar.SeparatorColor := Colors.Window;
    AStatusBar.OwnerDraw      := True;
  end
  else
    AStatusBar.OwnerDraw := False;
end;

// -----------------------------------------------------------------------------

procedure TColorTheme.Apply(const AVST: TLazVirtualStringTree; const AColorMode: TColorMode);
begin
  AVST.Color             := Colors.Window;
  AVST.Font.Color        := Colors.Text;

  if AColorMode = cmDark then
  begin
    AVST.BorderStyle                          := bsNone;
    AVST.Header.Font.Color                    := Colors.HighlightText;
    AVST.Colors.UnfocusedSelectionColor       := Colors.Highlight;
    AVST.Colors.UnfocusedSelectionBorderColor := Colors.Highlight;
  end
  else
  begin
    AVST.BorderStyle                          := bsSingle;
    AVST.Header.Font.Color                    := Colors.Text;
    AVST.Colors.UnfocusedSelectionColor       := Colors.Highlight;
    AVST.Colors.UnfocusedSelectionBorderColor := Colors.Highlight;
  end;
end;

// -----------------------------------------------------------------------------

procedure TColorTheme.Apply(const ASplitter: TSplitter; const AColorMode: TColorMode);
begin
  ASplitter.OnPaint := @SplitterDoPaint;
end;
{$ENDIF}

// -----------------------------------------------------------------------------

procedure TColorTheme.Apply(const AWave: TUWWaveformDisplayer; const AColorMode: TColorMode);
begin
  with AWave.CustomColors do
  begin
    Background := ColorToBGRA(Colors.WaveBackground);
    Waveform   := ColorToBGRA(Colors.Waveform);
    GridLine   := ColorToBGRA(Colors.WaveGrid);
    //Text       := ColorToBGRA(Colors.Text);
  end;
end;

// -----------------------------------------------------------------------------

procedure TColorTheme.Apply(const ASynEdit: TATSynEdit; const AColorMode: TColorMode);
begin
  with ASynEdit.Colors do
  begin
    TextBG        := Colors.Window;
    TextFont      := Colors.Text;
    RulerBG       := Colors.Form;
    GutterBG      := Colors.Form;
    GutterFoldBG  := Colors.Form;
    GutterCaretBG := Colors.Window;
  end;
end;

// -----------------------------------------------------------------------------

procedure TColorTheme.Apply(const AMenu: TPopupMenu; const AColorMode: TColorMode; const AImageList: TImageList = NIL);
begin
  AMenu.Images := AImageList;
end;

// -----------------------------------------------------------------------------

procedure TColorTheme.Apply(const AToolbar: TToolbar; const AColorMode: TColorMode; const AImageList: TImageList = NIL);
var
  i    : Integer;
  Ctrl : TControl;
begin
  AToolbar.Images := AImageList;
  {$IFDEF WINDOWS}
  AToolbar.Color  := Colors.Form;

  if AColorMode = cmDark then
    AToolbar.OnPaintButton := @ToolBarDoPaintButton
  else
    AToolbar.OnPaintButton := NIL;

  for i := 0 to AToolbar.ControlCount-1 do
  begin
    Ctrl := AToolbar.Controls[i];
    if Ctrl is TLabel then
      Apply(TLabel(Ctrl), AColorMode)
    else if Ctrl is TComboBox then
      Apply(TComboBox(Ctrl), AColorMode)
    else if Ctrl is TUWEditAction then
      Apply(TUWEditAction(Ctrl), AColorMode)
    else if Ctrl is TUWSeekBar then
      Apply(TUWSeekBar(Ctrl), AColorMode);
  end;
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

{$IFDEF WINDOWS}
procedure TColorTheme.ToolBarDoPaintButton(Sender: TToolButton; State: Integer);
var
  Bitmap: TBGRABitmap;
begin
  Bitmap := NIL;
  if Sender.Style in [tbsButton, tbsDropDown, tbsButtonDrop, tbsCheck] then
  begin
    if Sender.Enabled then
    begin
      if (State = 3) or Sender.Down then
      begin
        // Down
        Bitmap := TBGRABitmap.Create(Sender.Width, Sender.Height);
        Bitmap.RoundRect(0, 1, Sender.Width, Sender.Height - 1, 2, 2, Colors.Form, Colors.BtnChecked, dmSet);
      end
      else
      begin
        if State = 2 then
        begin
          // Highlighted
          Bitmap := TBGRABitmap.Create(Sender.Width, Sender.Height - 1);
          Bitmap.RoundRect(0, 1, Sender.Width, Sender.Height - 1, 2, 2, Colors.Window, Colors.BtnHighlight, dmSet);
        end
        else
        begin
          // Normal
        end;
      end;
    end
    else
    begin
      // Disabled
    end;

    if Assigned(Bitmap) then
    begin
      Bitmap.Draw(Sender.Canvas, 0, 0, False);
      Bitmap.Free;
    end;

    if (Sender.Parent is TToolBar) then
    begin
      with TToolBar(Sender.Parent) do
        if Assigned(Images) then
        begin
          if Sender.Style = tbsDropDown then
          begin
            Images.Draw(Sender.Canvas, ((Sender.Width - (Images.Width)) div 2)-6, (Sender.Height - (Images.Height)) div 2, Sender.ImageIndex, Sender.Enabled);
            Sender.Canvas.Pen.Color := Colors.Window;
            //{$IFDEF WINDOWS}
            Sender.Canvas.Brush.Style := bsClear;
            Sender.Canvas.Font.Size := 6;
            Sender.Canvas.TextOut(Sender.Width - 10, 5, WideChar(#$25BC)); //Windows.TextOutW(Sender.Canvas.Handle, Sender.Width - 10, 5, #$25BC, 1);
            //{$ELSE}
            //Sender.Canvas.Brush.Color := Colors.Window;
            //Sender.Canvas.RoundRect(Sender.Width - 10, 8, Sender.Width-4, 14, 4, 4);
            //{$ENDIF}
          end
          else
            Images.Draw(Sender.Canvas, (Sender.Width - (Images.Width)) div 2, (Sender.Height - (Images.Height)) div 2, Sender.ImageIndex, Sender.Enabled);
        end;
    end;
  end
  else if Sender.Style in [tbsSeparator, tbsDivider] then
  begin
    Sender.Canvas.Brush.Color := Colors.Window;
    Sender.Canvas.Line(Sender.Width div 2, 1, Sender.Width div 2, Sender.Height-1);
  end;
end;

// -----------------------------------------------------------------------------

procedure TColorTheme.SplitterDoPaint(Sender: TObject);
begin
  // Dummy
end;
{$ENDIF}

// -----------------------------------------------------------------------------

initialization
  ColorThemeInstance := TColorTheme.Create;

// -----------------------------------------------------------------------------

finalization
  ColorThemeInstance.Free;

// -----------------------------------------------------------------------------

end.

