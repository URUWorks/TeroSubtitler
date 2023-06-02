{*
 *  URUWorks StatusBar
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

unit UWStatusBar;

//------------------------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Controls, SysUtils, Graphics, ComCtrls, LCLType, LMessages, LCLIntf,
  LazarusPackageIntf;

type

  { TUWStatusBar }

  TUWStatusBar = class(TStatusBar)
  {$IFDEF WINDOWS}
  private
    FOwnerDraw: Boolean;
    FDrawSeparator: Boolean;
    FSeparatorColor: TColor;
    procedure SetOwnerDraw(const AValue: Boolean);
    procedure SetDrawSeparator(const AValue: Boolean);
  protected
    procedure WMPaint(var Msg: TLMPaint); message LM_PAINT;
  public
    constructor Create(TheOwner: TComponent); override;
    property OwnerDraw      : Boolean read FOwnerDraw      write SetOwnerDraw;
    property DrawSeparator  : Boolean read FDrawSeparator  write SetDrawSeparator;
    property SeparatorColor : TColor  read FSeparatorColor write FSeparatorColor;
  {$ENDIF}
  published
    property Action;
    property Align;
    property Anchors;
    property AutoHint;
    property AutoSize;
    property BiDiMode;
    property BorderSpacing;
    property BorderWidth;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Panels;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property UseSystemFont;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawPanel;
    property OnEndDock;
    property OnEndDrag;
    property OnHint;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

procedure Register;

// -----------------------------------------------------------------------------

implementation

{$IFDEF WINDOWS}
uses
  Themes;

const
  GRIP_SIZE = 16;

//------------------------------------------------------------------------------

{ TUWStatusBar }

//------------------------------------------------------------------------------

constructor TUWStatusBar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FOwnerDraw      := False;
  FDrawSeparator  := False;
  FSeparatorColor := clLtGray;
end;

//------------------------------------------------------------------------------

procedure TUWStatusBar.WMPaint(var Msg: TLMPaint);
var
  ACanvas: TCanvas;
  APanel: TStatusPanel;
  x, Index: Integer;
  ATextStyle: TTextStyle;
  R: TRect;
  details: TThemedElementDetails;
begin
  if not FOwnerDraw then
    inherited
  else
  begin
    ACanvas := TCanvas.Create;
    try
      ACanvas.Handle := Msg.DC;
      ACanvas.Brush.Color := Color;
      ACanvas.FillRect(ClientRect);
      ACanvas.Brush.Style := bsClear;
      ACanvas.Font := Font;
      FillByte(ATextStyle, SizeOf(TTextStyle), 0);
      ATextStyle.SingleLine  := True;
      ATextStyle.Layout      := tlCenter;
      ATextStyle.RightToLeft := IsRightToLeft;
      x := 0;
      for Index := 0 to Panels.Count-1 do
      begin
        APanel := Panels[Index];
        ATextStyle.Alignment := APanel.Alignment;

        if Index = Panels.Count-1 then
          //ACanvas.TextRect(Rect(x, 0, (x+APanel.Width)-GRIP_SIZE, Height), x+2, 0, APanel.Text, ATextStyle)
          ACanvas.TextRect(Rect(x, 0, (x+APanel.Width)-4, Height), x+2, 0, APanel.Text, ATextStyle)
        else
          ACanvas.TextRect(Rect(x, 0, (x+APanel.Width)-2, Height), x+2, 0, APanel.Text, ATextStyle);

        x += APanel.Width;

        if FDrawSeparator and (Index < Panels.Count-1) then
        begin
          ACanvas.Pen.Color := FSeparatorColor;
          ACanvas.Line(x, 1, x, Height-1);
          x += 2;
        end;
      end;

      R := Rect(Width-GRIP_SIZE, Height-GRIP_SIZE, Width, Height);
      details := ThemeServices.GetElementDetails(tsGripper);
      ThemeServices.DrawElement(ACanvas.Handle, details, R);
    finally
      ReleaseDC(0, ACanvas.Handle);
      ACanvas.Free;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWStatusBar.SetOwnerDraw(const AValue: Boolean);
begin
  if FOwnerDraw <> AValue then
  begin
    FOwnerDraw := AValue;
    Invalidate;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWStatusBar.SetDrawSeparator(const AValue: Boolean);
begin
  if FDrawSeparator <> AValue then
  begin
    FDrawSeparator := AValue;
    if FOwnerDraw then Invalidate;
  end;
end;
{$ENDIF}

// -----------------------------------------------------------------------------

procedure RegisterTeroControlsUnit;
begin
  RegisterComponents('URUWorks Tero', [TUWStatusBar]);
end;

// -----------------------------------------------------------------------------

procedure Register;
begin
  RegisterUnit('UWStatusBar', @RegisterTeroControlsUnit);
end;

// -----------------------------------------------------------------------------

end.

