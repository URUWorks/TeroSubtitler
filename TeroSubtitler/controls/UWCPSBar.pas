{*
 *  URUWorks CPSBar
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

unit UWCPSBar;

//------------------------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Controls, SysUtils, Graphics, LCLType, LazarusPackageIntf,
  BGRABitmap, BGRABitmapTypes;

type

  { TUWCPSBar }

  TUWCPSBar = class(TGraphicControl)
  private
    FBuffer      : TBGRABitmap;
    FLoaded      : Boolean;
    FValue       : Double;
    FMax         : Integer;
    FBackColor   : TColor;
    FBorderColor : TColor;
    procedure DrawBuffer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetValues(const Min, Max: Integer);
    procedure SetCPS(const CPS: Double);
    procedure SetMax(const AMax: Integer);

    property Buffer: TBGRABitmap read FBuffer;
  protected
    procedure Loaded; override;
    procedure Paint; override;
    procedure Resize; override;
  published
    property CPS         : Double  read FValue       write SetCPS;
    property Max         : Integer read FMax         write SetMax;
    property BackColor   : TColor  read FBackColor   write FBackColor;
    property BorderColor : TColor  read FBorderColor write FBorderColor;

    property Anchors;
    property PopupMenu;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

procedure Register;

// -----------------------------------------------------------------------------

implementation

const
  MaxCPS = 48;

//------------------------------------------------------------------------------

{ TUWCPSBar }

// -----------------------------------------------------------------------------

constructor TUWCPSBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width  := 100;
  Height := 7;

  ControlStyle := ControlStyle + [csOpaque];
  Color        := clDefault;
  Cursor       := crArrow;
  FLoaded      := False;

  FValue := 0;
  FMax   := 27; // Max acceptable value

  FBackColor   := clBtnFace;
  FBorderColor := clInactiveBorder;
  FBuffer := TBGRABitmap.Create(Width, Height, ColorToBGRA(FBackColor));
end;

// -----------------------------------------------------------------------------

destructor TUWCPSBar.Destroy;
begin
  FBuffer.Free;
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TUWCPSBar.Loaded;
begin
  inherited Loaded;
  FLoaded := True;
  DrawBuffer;
end;

// -----------------------------------------------------------------------------

procedure TUWCPSBar.Paint;
begin
  if (csDesigning in ComponentState) then
    Canvas.DrawFocusRect(Rect(0, 0, Width, Height));

  if Visible then
    FBuffer.Draw(Canvas, 0, 0);
end;

// -----------------------------------------------------------------------------

procedure TUWCPSBar.Resize;
begin
  inherited;

  if FLoaded then //and ([csLoading, csDestroying]*ComponentState = []) then
    DrawBuffer;
end;

// -----------------------------------------------------------------------------

procedure TUWCPSBar.SetValues(const Min, Max: Integer);
begin
  FMax := Max;
  DrawBuffer;
  Invalidate;
end;

// -----------------------------------------------------------------------------

procedure TUWCPSBar.SetCPS(const CPS: Double);
begin
  FValue := CPS;
  DrawBuffer;
  Invalidate;
end;

// -----------------------------------------------------------------------------

procedure TUWCPSBar.SetMax(const AMax: Integer);
begin
  if (AMax <> FMax) and (AMax > 0) then
  begin
    FMax := AMax;
    DrawBuffer;
    Invalidate;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWCPSBar.DrawBuffer;
var
  iSize    : Integer;
  maxline,
  pos, clr : Integer;
begin
  FBuffer.SetSize(Width, Height);
  with FBuffer.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := FBackColor;
    FillRect(Rect(0, 0, Width, Height));

    iSize := Width div (MaxCPS-1);

    if FValue < 1 then
      pos := 1
    else if FValue > MaxCPS then
      pos := Width
    else
      pos := Round(iSize * FValue);

    maxline := iSize * FMax;

    if pos <= maxline then
      clr := clGreen
    else
      clr := clRed;

    // line cps
    if pos > 1 then
    begin
      Brush.Color := clr;
      FillRect(Rect(0, 0, pos, Height));
    end;

    // border
{    Brush.Style := bsClear;
    Pen.Style   := psSolid;
    Pen.Color   := FBorderColor;
///    Rectangle(Rect(0, 0, Width, Height));
//    Line(0, 0, 0, Height-1);
//    Line(0, Height-1, Width, Height-1);
//    Line(Width-1, 0, Width-1, Height-1);
}

    // max start line
    Pen.Color := FBackColor; //FBorderColor;
    Line(Rect(maxline, 0, maxline, Height));
  end;
end;

// -----------------------------------------------------------------------------

procedure RegisterTeroControlsUnit;
begin
  RegisterComponents('URUWorks Tero', [TUWCPSBar]);
end;

// -----------------------------------------------------------------------------

procedure Register;
begin
  RegisterUnit('UWCPSBar', @RegisterTeroControlsUnit);
end;

// -----------------------------------------------------------------------------

end.

