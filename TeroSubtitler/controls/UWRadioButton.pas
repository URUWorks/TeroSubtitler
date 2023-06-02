{*
 *  URUWorks RadioButton
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

unit UWRadioButton;

//------------------------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Controls, SysUtils, Graphics, StdCtrls, LCLType, LMessages, LCLIntf,
  LazarusPackageIntf;

type

  { TUWRadioButton }

  TUWRadioButton = class(TRadioButton)
  private
    {$IFDEF WINDOWS}
    FLabel: TLabel;
    procedure DoPositionItems;
    procedure LabelClick(Sender: TObject);
    {$ENDIF}
  protected
    {$IFDEF WINDOWS}
    procedure SetParent(AParent: TWinControl); override;
    procedure SetName(const Value: TComponentName); override;
    procedure Loaded; override;
    function RealGetText: TCaption; override;
    procedure RealSetText(const Value: TCaption); override;
    procedure CMBiDiModeChanged(var Msg: TLMessage); message CM_BIDIMODECHANGED;
    procedure CMVisibleChanged(var Msg: TLMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledChanged(var Msg: TLMessage); message CM_ENABLEDCHANGED;
    {$ENDIF}
  public
    constructor Create(TheOwner: TComponent); override;
    {$IFDEF WINDOWS}
    destructor Destroy; override;
    function  GetTextLen: Integer; override;
    property LabelRadioButton: TLabel  read FLabel;
    {$ENDIF}
  published
    property Action;
    property Align;
    property Alignment;
    property AllowGrayed;
    property Anchors;
    property AutoSize default True;
    property BidiMode;
    property BorderSpacing;
    property Caption;
    property Checked;
    property Color nodefault;
    property Constraints;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Hint;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyPress;
    property OnKeyDown;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentColor;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property ParentBidiMode;
    property PopupMenu;
    property ShowHint;
    property State;
    property TabOrder;
    property TabStop default True;
    property Visible;
  end;

procedure Register;

// -----------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------

{ UWRadioButton }

//------------------------------------------------------------------------------

constructor TUWRadioButton.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  {$IFDEF WINDOWS}
  Caption             := '';
  AutoSize            := False;
  Width               := 18;
  FLabel              := TLabel.Create(Self);
  FLabel.ControlStyle := FLabel.ControlStyle + [csNoDesignSelectable];
  FLabel.FocusControl := Self;
  FLabel.SetSubComponent(True);
  FLabel.OnClick := @LabelClick;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

{$IFDEF WINDOWS}
destructor TUWRadioButton.Destroy;
begin
  FreeAndNIL(FLabel);

  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TUWRadioButton.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  DoPositionItems;
end;

// -----------------------------------------------------------------------------

procedure TUWRadioButton.SetName(const Value: TComponentName);
begin
  ControlStyle := ControlStyle - [csSetCaption];
  Caption := '';

  if (csDesigning in ComponentState)
    and ((FLabel.Caption = '') or (AnsiSameText(FLabel.Caption, Name)))
  then
    FLabel.Caption := Value;

  inherited SetName(Value);
end;

// -----------------------------------------------------------------------------

procedure TUWRadioButton.Loaded;
begin
  inherited Loaded;
  DoPositionItems;
end;

// -----------------------------------------------------------------------------

function TUWRadioButton.RealGetText: TCaption;
begin
  if FLabel <> NIL then
    Result := FLabel.Caption
  else
    inherited RealGetText;
end;

// -----------------------------------------------------------------------------

procedure TUWRadioButton.RealSetText(const Value: TCaption);
begin
  if RealGetText = Value then Exit;
  if FLabel <> NIL then
    FLabel.Caption := Value
  else
    inherited RealSetText(Value);
end;

// -----------------------------------------------------------------------------

function TUWRadioButton.GetTextLen: Integer;
begin
  if FLabel <> NIL then
    Result := Length(FLabel.Caption)
  else
    inherited GetTextLen;
end;

// -----------------------------------------------------------------------------

procedure TUWRadioButton.CMBiDiModeChanged(var Msg: TLMessage);
begin
  inherited CMBiDiModeChanged(Msg);
  FLabel.BiDiMode := BiDiMode;
  DoPositionItems;
  Invalidate;
end;

// -----------------------------------------------------------------------------

procedure TUWRadioButton.CMVisibleChanged(var Msg: TLMessage);
begin
  inherited CMVisiblechanged(Msg);

  if FLabel  <> NIL then FLabel.Visible  := Visible;
end;

// -----------------------------------------------------------------------------

procedure TUWRadioButton.CMEnabledChanged(var Msg: TLMessage);
begin
  inherited CMEnabledChanged(Msg);

  if FLabel  <> NIL then FLabel.Enabled  := Enabled;
end;

// -----------------------------------------------------------------------------

procedure TUWRadioButton.DoPositionItems;
begin
  if Parent <> NIL then
    Parent.DisableAlign;

  if FLabel <> NIL then
  begin
    FLabel.Parent  := Parent;
    FLabel.Visible := Visible;
    FLabel.AnchorToCompanion(akLeft, 0, Self);
    FLabel.AnchorVerticalCenterTo(Self);
  end;

  if Parent <> NIL then
    Parent.EnableAlign;
end;

// -----------------------------------------------------------------------------

procedure TUWRadioButton.LabelClick(Sender: TObject);
begin
  if not Checked then Checked := True;
end;
{$ENDIF}

// -----------------------------------------------------------------------------

procedure RegisterTeroControlsUnit;
begin
  RegisterComponents('URUWorks Tero', [TUWRadioButton]);
end;

// -----------------------------------------------------------------------------

procedure Register;
begin
  RegisterUnit('UWRadioButton', @RegisterTeroControlsUnit);
end;

// -----------------------------------------------------------------------------

end.

