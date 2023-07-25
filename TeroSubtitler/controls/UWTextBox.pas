{*
 *  URUWorks TextBox
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

unit UWTextBox;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Controls, SysUtils, Graphics, LCLType, LazarusPackageIntf,
  BGRABitmap, BGRABitmapTypes, BGRATextFX;

type

  { TUWTextBox }

  TUWTextBox = class(TGraphicControl)
  private
    FBitmap      : TBGRABitmap;
    FRenderer    : TBGRATextEffectFontRenderer;
    FLoaded      : Boolean;
    FText        : String;
    FBackColor   : TColor;
    FSpacing     : Byte;
    FTransparent : Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DrawBuffer(const AWidth: Integer = -1; const AHeight: Integer = -1);
    procedure ReDraw;
    procedure SaveImageToFile(const AFileName: String; const AText: String = ''; const AWidth: Integer = -1; const AHeight: Integer = -1; const ATransparent: Boolean = False);
    property Bitmap: TBGRABitmap read FBitmap;
    property Renderer: TBGRATextEffectFontRenderer read FRenderer;
  protected
    procedure Loaded; override;
    procedure Paint; override;
    procedure Resize; override;
  published
    property Text        : String  read FText        write FText;
    property BackColor   : TColor  read FBackColor   write FBackColor;
    property Spacing     : Byte    read FSpacing     write FSpacing;
    property Transparent : Boolean read FTransparent write FTransparent;

    property Anchors;
    property Font;
    property PopupMenu;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

procedure Register;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

{ TUWTextBox }

// -----------------------------------------------------------------------------

constructor TUWTextBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width  := 200;
  Height := 100;

  ControlStyle := ControlStyle + [csOpaque];
  Color        := clDefault;
  Cursor       := crArrow;
  FLoaded      := False;

  if Parent <> NIL then
    FBackColor := Parent.Color
  else
    FBackColor := clBtnFace;

  FText        := 'URUWorks Tero Subtitler';
  FSpacing     := 0;
  FTransparent := False;

  with Font do
  begin
    Color := clWhite;
    Size  := 24;
  end;

  FBitmap   := TBGRABitmap.Create(Width, Height, ColorToBGRA(FBackColor));
  FRenderer := TBGRATextEffectFontRenderer.Create;

  with FRenderer do
  begin
    FontName         := Font.Name;

    OutlineColor     := ColorToBGRA(clBlack);
    OutlineWidth     := 4;
    OutlineVisible   := True;
    OuterOutlineOnly := True;

    ShadowColor      := ColorToBGRA(clBlack);
    ShadowVisible    := True;
  end;

  with FBitmap do
  begin
    FontRenderer := FRenderer;
    FontQuality  := fqFineAntialiasing;
  end;
end;

// -----------------------------------------------------------------------------

destructor TUWTextBox.Destroy;
begin
  FBitmap.Free;
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TUWTextBox.Loaded;
begin
  inherited Loaded;
  FLoaded := True;
  DrawBuffer;
end;

// -----------------------------------------------------------------------------

procedure TUWTextBox.Paint;
begin
  if (csDesigning in ComponentState) then
    Canvas.DrawFocusRect(Rect(0, 0, Width, Height));

  if Visible then
    FBitmap.Draw(Canvas, 0, 0);
end;

// -----------------------------------------------------------------------------

procedure TUWTextBox.Resize;
begin
  inherited;

  if FLoaded then
    DrawBuffer;
end;

// -----------------------------------------------------------------------------

procedure TUWTextBox.DrawBuffer(const AWidth: Integer = -1; const AHeight: Integer = -1);
begin
  with FBitmap do
  begin
    if (AWidth > 0) and (AHeight > 0) then
      SetSize(AWidth, AHeight)
    else
      SetSize(Self.Width, Self.Height);

    if FTransparent then
      FillTransparent
    else
      Fill(ColorToBGRA(FBackColor));

    FontName := Font.Name;
    FontFullHeight := Font.Size;
    TextOut(Width div 2, Height div 3, FText, ColorToBGRA(Font.Color), taCenter, FSpacing);
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWTextBox.ReDraw;
begin
  DrawBuffer;
  Invalidate;
end;

// -----------------------------------------------------------------------------

procedure TUWTextBox.SaveImageToFile(const AFileName: String; const AText: String = ''; const AWidth: Integer = -1; const AHeight: Integer = -1; const ATransparent: Boolean = False);
begin
  if not AText.IsEmpty then
    FText := AText;

  FTransparent := ATransparent;

  DrawBuffer(AWidth, AHeight);
  FBitmap.SaveToFile(AFileName);
end;

// -----------------------------------------------------------------------------

procedure RegisterTeroControlsUnit;
begin
  RegisterComponents('URUWorks Tero', [TUWTextBox]);
end;

// -----------------------------------------------------------------------------

procedure Register;
begin
  RegisterUnit('UWTextBox', @RegisterTeroControlsUnit);
end;

// -----------------------------------------------------------------------------

end.

