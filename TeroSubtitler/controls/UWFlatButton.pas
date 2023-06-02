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

unit UWFlatButton;

//------------------------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Controls, SysUtils, Graphics, LCLType, LazarusPackageIntf,
  BGRABitmap, BGRABitmapTypes, ImgList;

type

  { UWFlatButton }

  TUWFlatButton = class(TGraphicControl)
  private
    FBMP             : TBGRABitmap;
    FLoaded          : Boolean;
    FButtonStyle     : Boolean;
    FHightlightColor : TColor;
    FBorderColor     : TColor;
    FCheckedColor    : TColor;
    FIsOver          : Boolean;
    FMouseIsDown     : Boolean;
    FImageIndex      : TImageIndex;
    FImages          : TCustomImageList;
    procedure SetImageIndex(const AIndex: TImageIndex);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Invalidate; override;
    procedure DrawBuffer;
  protected
    procedure Paint; override;
    procedure Loaded; override;
    procedure MouseLeave; override;
    procedure MouseEnter; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  published
    property ButtonStyle     : Boolean read FButtonStyle     write FButtonStyle;
    property HightlightColor : TColor  read FHightlightColor write FHightlightColor;
    property BorderColor     : TColor  read FBorderColor     write FBorderColor;
    property CheckedColor    : TColor  read FCheckedColor    write FCheckedColor;

    property Images: TCustomImageList read FImages write FImages;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;

    property Action;
    property Align;
    property Anchors;
    property Color;
    property Cursor;
    property Enabled;
    property Width default 23;
    property Height default 23;
    property PopupMenu;
    property OnClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

procedure Register;

// -----------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------

{ UWFlatButton }

// -----------------------------------------------------------------------------

constructor TUWFlatButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  SetBounds(Left, Top, 23, 23);

  FHightlightColor := clBtnHighlight;
  FBorderColor     := clActiveCaption;
  FCheckedColor    := clInactiveBorder;
  FIsOver          := False;
  FMouseIsDown     := False;
  FImageIndex      := -1;
  Color            := clBtnFace;
  FLoaded          := False;
  FButtonStyle     := True;

  FBMP := TBGRABitmap.Create(Width, Height, ColorToBGRA(Color));
end;

// -----------------------------------------------------------------------------

destructor TUWFlatButton.Destroy;
begin
  FBMP.Free;

  inherited;
end;

// -----------------------------------------------------------------------------

procedure TUWFlatButton.Invalidate;
begin
  if FLoaded then
    DrawBuffer;

  inherited;
end;

// -----------------------------------------------------------------------------

procedure TUWFlatButton.DrawBuffer;
begin
  FBMP.SetSize(Width, Height);
  FBMP.Fill(Color);

  if FButtonStyle then
  begin
    if FIsOver and not FMouseIsDown then
      FBMP.RoundRect(0, 0, Width, Height, 8, 8, FBorderColor, FHightlightColor)
    else if FMouseIsDown then
      FBMP.RoundRect(0, 0, Width, Height, 8, 8, FBorderColor, FCheckedColor);
  end;

  if Assigned(FImages) and (FImageIndex >= 0) then
    FImages.Draw(FBMP.Canvas, (Width - (FImages.Width)) div 2, (Height - (FImages.Height)) div 2, FImageIndex, Enabled);
end;

// -----------------------------------------------------------------------------

procedure TUWFlatButton.Paint;
begin
  Canvas.Lock;
  try
    FBMP.Draw(Canvas, 0, 0);
    if csDesigning in ComponentState then Canvas.DrawFocusRect(ClientRect);
  finally
    Canvas.Unlock;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWFlatButton.Loaded;
begin
  inherited Loaded;
  FLoaded := True;
end;

// -----------------------------------------------------------------------------

procedure TUWFlatButton.MouseLeave;
begin
  inherited MouseLeave;

  FIsOver := False;

  if FButtonStyle then Invalidate;
end;

// -----------------------------------------------------------------------------

procedure TUWFlatButton.MouseEnter;
begin
  inherited MouseEnter;

  FIsOver := True;
  if FButtonStyle then Invalidate;
end;

// -----------------------------------------------------------------------------

procedure TUWFlatButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  if (ssLeft in Shift) then
  begin
    FMouseIsDown := True;
    if FButtonStyle then Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TUWFlatButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

  if FMouseIsDown then
  begin
    FMouseIsDown := False;
    if FButtonStyle then Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TUWFlatButton.SetImageIndex(const AIndex: TImageIndex);
begin
  if FImageIndex <> AIndex then
  begin
    FImageIndex := AIndex;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure RegisterTeroControlsUnit;
begin
  RegisterComponents('URUWorks Tero', [TUWFlatButton]);
end;

// -----------------------------------------------------------------------------

procedure Register;
begin
  RegisterUnit('UWFlatButton', @RegisterTeroControlsUnit);
end;

// -----------------------------------------------------------------------------

end.

