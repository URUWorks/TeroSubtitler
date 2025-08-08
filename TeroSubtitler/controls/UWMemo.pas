{*
 *  URUWorks Memo
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
 *  Copyright (C) 2001-2023 URUWorks, uruworks@gmail.com.
 *}

unit UWMemo;

//------------------------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Controls, StdCtrls, ComCtrls, LCLType, LMessages, SysUtils,
  UWCPSBar, LazarusPackageIntf;

type

  { TUWMemo }

  TUWMemo = class(TCustomMemo)
  private
    FLabel         : TLabel;
    FCPSBar        : TUWCPSBar;
    FLabelSpacing  : Integer;
    FCPSBarSpacing : Integer;

    procedure SetLabelSpacing(const Value: Integer);
    procedure SetCPSBarSpacing(const Value: Integer);
    procedure DoPositionItems;
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure SetName(const Value: TComponentName); override;
    procedure Loaded; override;
    procedure CMBiDiModeChanged(var Msg: TLMessage); message CM_BIDIMODECHANGED;
    procedure CMVisibleChanged(var Msg: TLMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledChanged(var Msg: TLMessage); message CM_ENABLEDCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PasteFromClipboard; override;
  published
    property LabelMemo     : TLabel    read FLabel;
    property CPSBar        : TUWCPSBar read FCPSBar;
    property LabelSpacing  : Integer   read FLabelSpacing  write SetLabelSpacing;
    property CPSBarSpacing : Integer   read FCPSBarSpacing write SetCPSBarSpacing;

    property Align;
    property Alignment;
    property Anchors;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property Lines;
    property MaxLength;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnMouseWheelHorz;
    property OnMouseWheelLeft;
    property OnMouseWheelRight;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentBidiMode;
    property ParentColor;
    property ParentDoubleBuffered;
    property ParentFont;
    property PopupMenu;
    property ParentShowHint;
    property ReadOnly;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property WantReturns;
    property WantTabs;
    property WordWrap;
  end;

procedure Register;

//------------------------------------------------------------------------------

implementation

uses
  Clipbrd, LazUTF8, LCLIntf;

//------------------------------------------------------------------------------

constructor TUWMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width      := 100;
  Height     := 80;
  Font.Bold  := True;
  Alignment  := taCenter;
  ScrollBars := ssAutoVertical;
  Lines.Clear;

  FLabelSpacing  := 3;
  FCPSBarSpacing := 0;

  FLabel := TLabel.Create(Self);
  FLabel.ControlStyle := FLabel.ControlStyle + [csNoDesignSelectable];
  FLabel.FocusControl := Self;
  FLabel.SetSubComponent(True);

  FCPSBar := TUWCPSBar.Create(Self);
  FCPSBar.ControlStyle := FCPSBar.ControlStyle + [csNoDesignSelectable];
  FCPSBar.SetSubComponent(True);
end;

// -----------------------------------------------------------------------------

destructor TUWMemo.Destroy;
begin
  FreeAndNIL(FLabel);
  FreeAndNIL(FCPSBar);

  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TUWMemo.PasteFromClipboard;
  {$ifdef Darwin}
  procedure FixClipboardLineEndings;
  const
    // Unicode separators UTF-8
    LS = #$E2#$80#$A8; // U+2028
    PS = #$E2#$80#$A9; // U+2029
  var
    S, N: String;
  begin
    if not Clipboard.HasFormat(PredefinedClipboardFormat(pcfText)) then
      Exit;

    S := Clipboard.AsText;
    if S = '' then Exit;

    N := S;
    // CRLF -> LF
    N := UTF8StringReplace(N, #13#10, #10, [rfReplaceAll]);
    // CR -> LF
    N := UTF8StringReplace(N, #13,    #10, [rfReplaceAll]);
    // Unicode LS/PS -> LF
    N := UTF8StringReplace(N, LS,     #10, [rfReplaceAll]);
    N := UTF8StringReplace(N, PS,     #10, [rfReplaceAll]);

    if N <> S then
      Clipboard.AsText := N;
  end;
  {$endif}

begin
  {$ifdef Darwin}
  FixClipboardLineEndings;
  {$endif}

  inherited PasteFromClipboard;
end;

// -----------------------------------------------------------------------------

procedure TUWMemo.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  DoPositionItems;
end;

// -----------------------------------------------------------------------------

procedure TUWMemo.SetName(const Value: TComponentName);
begin
  if (csDesigning in ComponentState)
    and ((FLabel.Caption = '') or (AnsiSameText(FLabel.Caption, Name)))
  then
    FLabel.Caption := Value;

  inherited SetName(Value);
end;

// -----------------------------------------------------------------------------

procedure TUWMemo.Loaded;
begin
  inherited Loaded;
  DoPositionItems;
end;

// -----------------------------------------------------------------------------

procedure TUWMemo.CMBiDiModeChanged(var Msg: TLMessage);
begin
  inherited CMBiDiModeChanged(Msg);
  FLabel.BiDiMode := BiDiMode;
  DoPositionItems;
  Invalidate;
end;

// -----------------------------------------------------------------------------

procedure TUWMemo.CMVisibleChanged(var Msg: TLMessage);
begin
  inherited CMVisiblechanged(Msg);

  if FLabel  <> NIL then FLabel.Visible  := Visible;
  if FCPSBar <> NIL then FCPSBar.Visible := Visible;
end;

// -----------------------------------------------------------------------------

procedure TUWMemo.CMEnabledChanged(var Msg: TLMessage);
begin
  inherited CMEnabledChanged(Msg);

  if FLabel  <> NIL then FLabel.Enabled  := Enabled;
  if FCPSBar <> NIL then FCPSBar.Enabled := Enabled;
end;

// -----------------------------------------------------------------------------

procedure TUWMemo.SetLabelSpacing(const Value: Integer);
begin
  if Value = FLabelSpacing then Exit;
  FLabelSpacing := Value;
  DoPositionItems;
end;

// -----------------------------------------------------------------------------

procedure TUWMemo.SetCPSBarSpacing(const Value: Integer);
begin
  if Value = FCPSBarSpacing then Exit;
  FCPSBarSpacing := Value;
  DoPositionItems;
end;

// -----------------------------------------------------------------------------

procedure TUWMemo.DoPositionItems;
begin
  if Parent <> NIL then
    Parent.DisableAlign;

  if FLabel <> NIL then
  begin
    FLabel.Parent  := Parent;
    FLabel.Visible := Visible;
    FLabel.AnchorParallel(akLeft, 0, Self);
    FLabel.AnchorToCompanion(akBottom, FLabelSpacing, Self);
  end;

  if FCPSBar <> NIL then
  begin
    FCPSBar.Parent  := Parent;
    FCPSBar.Visible := Visible;
    FCPSBar.AnchorParallel(akLeft, 0, Self);
    FCPSBar.AnchorToCompanion(akTop, FCPSBarSpacing, Self);
  end;

  if Parent <> NIL then
    Parent.EnableAlign;
end;

// -----------------------------------------------------------------------------

procedure RegisterTeroControlsUnit;
begin
  RegisterComponents('URUWorks Tero', [TUWMemo]);
end;

// -----------------------------------------------------------------------------

procedure Register;
begin
  RegisterUnit('UWMemo', @RegisterTeroControlsUnit);
end;

// -----------------------------------------------------------------------------

end.

