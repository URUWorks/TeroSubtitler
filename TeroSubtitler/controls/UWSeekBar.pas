{*
 *  URUWorks Layout
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

unit UWSeekBar;

//------------------------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Controls, SysUtils, Graphics, LCLType, LazarusPackageIntf,
  UWSystem.SysUtils;

type

  { TUWSeekBar }

  TUWSeekBarPaintEvent = procedure(Sender: TObject; const ACanvas: TCanvas; const R: TRect) of object;

  TUWSeekBar = class(TGraphicControl)
  private
    FMax                 : Integer;
    FPosition            : Integer;
    FBackRect            : TRect;
    FFillRect            : TRect;
    FButtonRect          : TRect;
    FButtonSize          : TPoint;
    FShowButton          : Boolean;
    FMouseIsDown         : Boolean;
    FBackColor           : TColor;
    FFillColor           : TColor;
    FButtonColor         : TColor;
    FBtnDownColor        : TColor;
    FOnPaintSeekBarEvent : TUWSeekBarPaintEvent;
    FOnChangeEvent       : TNotifyEvent;
    procedure CalculateBackRect;
    procedure CalculateFillAndButtonRect;
    procedure DrawBackground(const ACanvas: TCanvas);
    procedure DrawButton(const ACanvas: TCanvas);
    function CheckXValue(const X: Integer): Integer;
    procedure CheckNewPosition(const X: Integer);
    procedure SetMax(const AMax: Integer);
    procedure SetPosition(const APosition: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property MouseIsDown: Boolean read FMouseIsDown;
  protected
    procedure Paint; override;
    procedure DoOnResize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  published
    property ShowButton   : Boolean              read FShowButton          write FShowButton;
    property Max          : Integer              read FMax                 write SetMax;
    property Position     : Integer              read FPosition            write SetPosition;
    property BackColor    : TColor               read FBackColor           write FBackColor;
    property FillColor    : TColor               read FFillColor           write FFillColor;
    property ButtonColor  : TColor               read FButtonColor         write FButtonColor;
    property BtnDownColor : TColor               read FBtnDownColor        write FBtnDownColor;
    property OnPaintBkg   : TUWSeekBarPaintEvent read FOnPaintSeekBarEvent write FOnPaintSeekBarEvent;
    property OnChange     : TNotifyEvent         read FOnChangeEvent       write FOnChangeEvent;

    property Align;
    property Anchors;
    property BorderSpacing;
    property Cursor;
    property Enabled;
    property Width;
    property Height;
    property PopupMenu;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
  end;

procedure Register;

// -----------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------

{ TUWSeekBar }

// -----------------------------------------------------------------------------

constructor TUWSeekBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width  := 100;
  Height := 18;

  FMax          := 1000;
  FPosition     := 0;
  FButtonSize.X := 12;
  FButtonSize.Y := Height;
  FShowButton   := False;
  FMouseIsDown  := False;

  FBackColor    := $D3D3D3;
  FFillColor    := RGBToColor(0, 84, 184);
  FButtonColor  := $696969;
  FBtnDownColor := $998877;

  Constraints.MinWidth  := 10;
  Constraints.MinHeight := 2;
  Constraints.MaxHeight := 60;
end;

// -----------------------------------------------------------------------------

destructor TUWSeekBar.Destroy;
begin
  inherited;
end;

// -----------------------------------------------------------------------------

procedure TUWSeekBar.Paint;
begin
  Canvas.Lock;
  try
    Canvas.Brush.Color := TCustomControl(Parent).Color;
    Canvas.FillRect(ClientRect);

    DrawBackground(Canvas);
    DrawButton(Canvas);
  finally
    Canvas.Unlock;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWSeekBar.DoOnResize;
begin
  if [csLoading, csDestroying]*ComponentState = [] then
  begin
    CalculateBackRect;
    CalculateFillAndButtonRect;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWSeekBar.CalculateBackRect;
begin
  FBackRect := ClientRect;
end;

// -----------------------------------------------------------------------------

procedure TUWSeekBar.CalculateFillAndButtonRect;
begin
  FFillRect   := FBackRect;
  FButtonRect := FBackRect;

  if FMax <> 0 then
  begin
    if FShowButton then
      FButtonRect.Left := Trunc((FPosition * 100 / FMax) * (Width-FButtonSize.X) / 100)
    else
      FButtonRect.Left := Trunc((FPosition * 100 / FMax) * Width / 100)
  end
  else
    FButtonRect.Left := 0;

  if not FShowButton then
    FFillRect.Right    := FButtonRect.Left
  else
    FFillRect.Right    := FButtonRect.Left + (FButtonSize.X div 2);

  FButtonRect.Right  := FButtonRect.Left + FButtonSize.X;
  FButtonRect.Bottom := FButtonRect.Bottom  + FButtonSize.Y;
end;

// -----------------------------------------------------------------------------

procedure TUWSeekBar.DrawBackground(const ACanvas: TCanvas);
begin
  ACanvas.Brush.Color := FBackColor;
  ACanvas.FillRect(FBackRect);

  if Assigned(FOnPaintSeekBarEvent) then FOnPaintSeekBarEvent(Self, ACanvas, FBackRect);
end;

//------------------------------------------------------------------------------

procedure TUWSeekBar.DrawButton(const ACanvas: TCanvas);
begin
  // Fill
  ACanvas.Brush.Color := FFillColor;
  ACanvas.FillRect(FFillRect);

  if FShowButton then
  begin
    // Border
    //ACanvas.Pen.Color := FBackColor;
    //ACanvas.Rectangle(ClientRect);

    // Button
    if FMouseIsDown then
      ACanvas.Brush.Color := FBtnDownColor
    else
      ACanvas.Brush.Color := FButtonColor;

    ACanvas.Pen.Color := ACanvas.Brush.Color;
    ACanvas.FillRect(FButtonRect);
  end;
end;

//------------------------------------------------------------------------------

function TUWSeekBar.CheckXValue(const X: Integer): Integer;
begin
  if FShowButton then
    Result := ((X - (FButtonSize.X div 2)) * FMax) div (Width-(FButtonSize.X))
  else
    Result := (X * FMax) div Width;

  Constrain(Result, 0, FMax);
end;

//------------------------------------------------------------------------------

procedure TUWSeekBar.CheckNewPosition(const X: Integer);
begin
  FPosition := CheckXValue(X);
  CalculateFillAndButtonRect;
  Repaint;
end;

//------------------------------------------------------------------------------

procedure TUWSeekBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (ssLeft in Shift) then
  begin
    FMouseIsDown := True;
    CheckNewPosition(X);
    inherited;
  end;
end;

//------------------------------------------------------------------------------

procedure TUWSeekBar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FMouseIsDown then
    CheckNewPosition(X);

  inherited;
//  else
//  begin
//    Hint := CheckXValue(X).ToString;
//  end;
end;

//------------------------------------------------------------------------------

procedure TUWSeekBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FMouseIsDown then
  begin
    FMouseIsDown := False;
    CheckNewPosition(X);

    if Assigned(FOnChangeEvent) then FOnChangeEvent(Self);
    inherited;
  end;
end;

//------------------------------------------------------------------------------

procedure TUWSeekBar.SetMax(const AMax: Integer);
begin
  if FMax <> AMax then
  begin
    FMax := AMax;
    CalculateFillAndButtonRect;
    Repaint;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWSeekBar.SetPosition(const APosition: Integer);
begin
  if not FMouseIsDown and (FPosition <> APosition) then
  begin
    FPosition := APosition;
    Constrain(FPosition, 0, FMax);
    CalculateFillAndButtonRect;
    Repaint;

    if Assigned(FOnChangeEvent) then FOnChangeEvent(Self);
  end;
end;

// -----------------------------------------------------------------------------

procedure RegisterTeroControlsUnit;
begin
  RegisterComponents('URUWorks Tero', [TUWSeekBar]);
end;

// -----------------------------------------------------------------------------

procedure Register;
begin
  RegisterUnit('UWSeekBar', @RegisterTeroControlsUnit);
end;

// -----------------------------------------------------------------------------

end.

