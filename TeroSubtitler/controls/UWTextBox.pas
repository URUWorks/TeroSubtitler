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
    FBuffer    : TBGRABitmap;
    FRenderer  : TBGRATextEffectFontRenderer;
    FLoaded    : Boolean;
    FText      : String;
    FBackColor : TColor;
    FSpacing   : Byte;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DrawBuffer(const ABuffer: TBGRABitmap = NIL; const AWidth: Integer = -1; const AHeight: Integer = -1; const ATransparent: Boolean = False);
    procedure ReDraw;
    procedure SaveToFile(const AFileName: String; const AWidth: Integer = -1; const AHeight: Integer = -1; const ATransparent: Boolean = False);
    property Buffer: TBGRABitmap read FBuffer;
    property Renderer: TBGRATextEffectFontRenderer read FRenderer;
  protected
    procedure Loaded; override;
    procedure Paint; override;
    procedure Resize; override;
  published
    property Text      : String read FText      write FText;
    property BackColor : TColor read FBackColor write FBackColor;
    property Spacing   : Byte   read FSpacing   write FSpacing;

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

  Width  := 100;
  Height := 7;

  ControlStyle := ControlStyle + [csOpaque];
  Color        := clDefault;
  Cursor       := crArrow;
  FLoaded      := False;

  if Parent <> NIL then
    FBackColor := Parent.Color
  else
    FBackColor := clBtnFace;

  FText    := 'URUWorks Tero Subtitler';
  FSpacing := 0;
  with Font do
  begin
    Color := clWhite;
    Size  := 24;
  end;

  FBuffer   := TBGRABitmap.Create(Width, Height, ColorToBGRA(FBackColor));
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
end;

// -----------------------------------------------------------------------------

destructor TUWTextBox.Destroy;
begin
  FBuffer.Free;
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
    FBuffer.Draw(Canvas, 0, 0);
end;

// -----------------------------------------------------------------------------

procedure TUWTextBox.Resize;
begin
  inherited;

  if FLoaded then
    DrawBuffer;
end;

// -----------------------------------------------------------------------------

procedure TUWTextBox.DrawBuffer(const ABuffer: TBGRABitmap = NIL; const AWidth: Integer = -1; const AHeight: Integer = -1; const ATransparent: Boolean = False);
var
  bgra: TBGRABitmap;
begin
  if ABuffer <> NIL then
    bgra := ABuffer
  else
    bgra := FBuffer;

  bgra.FontRenderer := FRenderer;
  bgra.FontQuality  := fqFineAntialiasing;

  if (AWidth > 0) and (AHeight > 0) then
    bgra.SetSize(AWidth, AHeight)
  else
    bgra.SetSize(Width, Height);

  with bgra do
  begin
    if ATransparent then
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

procedure TUWTextBox.SaveToFile(const AFileName: String; const AWidth: Integer = -1; const AHeight: Integer = -1; const ATransparent: Boolean = False);
var
  bgra: TBGRABitmap;
begin
  bgra := TBGRABitmap.Create(Width, Height, ColorToBGRA(FBackColor));
  try
    DrawBuffer(bgra, AWidth, AHeight, ATransparent);
    bgra.SaveToFile(AFileName);
  finally
    bgra.Free;
  end;
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

