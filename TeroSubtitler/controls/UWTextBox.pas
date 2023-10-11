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

  { TUWBGRAText }

  TUWBGRAText = class
  private
    FBitmap      : TBGRABitmap;
    FRenderer    : TBGRATextEffectFontRenderer;
    FText        : String;
    FTextColor   : TColor;
    FBackColor   : TColor;
    FSpacing     : Byte;
    FTransparent : Boolean;
    FFont        : TFont;
  public
    constructor Create(const AText: String; const AWidth: Integer; const AHeight: Integer; const AFont: TFont; const ATransparent: Boolean = False);
    destructor Destroy; override;
    procedure DrawBuffer(const AText: String; const AWidth: Integer = -1; const AHeight: Integer = -1; const AAlign: TAlignment = taCenter; const VAlign: TTextLayout = tlCenter; const ALeft: Integer = 0; const ATop: Integer = 0; const ARight: Integer = 0; const ABottom: Integer = 0);
    property Bitmap: TBGRABitmap read FBitmap;
    property Renderer: TBGRATextEffectFontRenderer read FRenderer;
    property TextColor: TColor read FTextColor write FTextColor;
    property BackColor: TColor read FBackColor write FBackColor;
    property Font: TFont read FFont write FFont;
  end;

  { TUWTextBox }

  TUWTextBox = class(TGraphicControl)
  private
    FBuffer: TUWBGRAText;
    FLoaded: Boolean;
    procedure SetText(const AText: String);
    function GetText: String;
    procedure SetBackColor(const AColor: TColor);
    function GetBackColor: TColor;
    procedure SetSpacing(const ASpacing: Byte);
    function GetSpacing: Byte;
    procedure SetTransparent(const ATransparent: Boolean);
    function GetTransparent: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ReDraw;
    property Buffer: TUWBGRAText read FBuffer;
  protected
    procedure Loaded; override;
    procedure Paint; override;
    procedure Resize; override;
  published
    property Text        : String read GetText write SetText;
    property BackColor   : TColor  read GetBackColor   write SetBackColor;
    property Spacing     : Byte    read GetSpacing     write SetSpacing;
    property Transparent : Boolean read GetTransparent write SetTransparent;

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

uses
  UWSubtitleAPI.Tags, BGRADefaultBitmap;

// -----------------------------------------------------------------------------

{ TUWBGRAText }

// -----------------------------------------------------------------------------

constructor TUWBGRAText.Create(const AText: String; const AWidth: Integer; const AHeight: Integer; const AFont: TFont; const ATransparent: Boolean = False);
begin
  FBitmap   := TBGRABitmap.Create(AWidth, AHeight, ColorToBGRA(FBackColor));
  FRenderer := TBGRATextEffectFontRenderer.Create;
  FFont     := TFont.Create;
  FFont.Assign(AFont);

  with FRenderer do
  begin
    FontName := FFont.Name;

    OutlineColor     := ColorToBGRA(clBlack);
    OutlineWidth     := 4;
    OutlineVisible   := True;
    OuterOutlineOnly := True;

    ShadowColor   := ColorToBGRA(clBlack);
    ShadowVisible := True;
  end;

  with FBitmap do
  begin
    FontRenderer   := FRenderer;
    FontQuality    := fqFineAntialiasing;
    FontFullHeight := FFont.Size;
  end;

  FText := AText;
  FSpacing := 0;
  FTransparent := ATransparent;

  FTextColor := clWhite;
  FBackColor := clBtnFace;
end;

// -----------------------------------------------------------------------------

destructor TUWBGRAText.Destroy;
begin
  FFont.Free;
  FBitmap.Free;
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TUWBGRAText.DrawBuffer(const AText: String; const AWidth: Integer = -1; const AHeight: Integer = -1; const AAlign: TAlignment = taCenter; const VAlign: TTextLayout = tlCenter; const ALeft: Integer = 0; const ATop: Integer = 0; const ARight: Integer = 0; const ABottom: Integer = 0);
var
  ts : TTextStyle;
  r  : TRect;
  s  : TSize;
begin
  with FBitmap do
  begin
    FText := AText;

    FontName := FFont.Name;
    FontFullHeight := FFont.Size;

    ts := BGRADefaultBitmap.DefaultTextStyle;
    ts.Alignment := AAlign;
    ts.Layout    := VAlign;

    r := Rect(ClipRect.Left + ALeft, ClipRect.Top + ATop, ClipRect.Right - ARight, ClipRect.Bottom - ABottom);

    if (AWidth > 0) and (AHeight > 0) then // Full Size
      SetSize(AWidth, AHeight)
    else
    begin // Auto Size
      s := TextSizeMultiline(FText);
      SetSize(s.Width + 15, s.Height + 15);
      ts.Alignment := taCenter;
      ts.Layout    := tlCenter;
      r := Rect(ClipRect.Left, ClipRect.Top, ClipRect.Right, ClipRect.Bottom);
    end;

    if FTransparent then
      FillTransparent
    else
      Fill(ColorToBGRA(FBackColor));

    TextRect(r, 0, 0, RemoveTSTags(FText), ts, ColorToBGRA(Font.Color));
  end;
end;

// -----------------------------------------------------------------------------

{ TUWTextBox }

// -----------------------------------------------------------------------------

constructor TUWTextBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width  := 320;
  Height := 240;

  ControlStyle := ControlStyle + [csOpaque];
  Color        := clDefault;
  Cursor       := crArrow;
  FLoaded      := False;

  FBuffer := TUWBGRAText.Create('URUWorks Tero Subtitler', Width, Height, Font, True);

  if Parent <> NIL then
    FBuffer.BackColor := Parent.Color
  else
    FBuffer.BackColor := clBtnFace;
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
  FBuffer.DrawBuffer(FBuffer.FText, Width, Height);
end;

// -----------------------------------------------------------------------------

procedure TUWTextBox.Paint;
begin
  if (csDesigning in ComponentState) then
    Canvas.DrawFocusRect(Rect(0, 0, Width, Height));

  if Visible then
    FBuffer.FBitmap.Draw(Canvas, 0, 0);
end;

// -----------------------------------------------------------------------------

procedure TUWTextBox.Resize;
begin
  inherited;

  if FLoaded then
    FBuffer.DrawBuffer(FBuffer.FText, Width, Height);
end;

// -----------------------------------------------------------------------------

procedure TUWTextBox.ReDraw;
begin
  FBuffer.Font.Assign(Font);
  FBuffer.DrawBuffer(FBuffer.FText, Width, Height);
  Invalidate;
end;

// -----------------------------------------------------------------------------

procedure TUWTextBox.SetText(const AText: String);
begin
  FBuffer.FText := AText;
end;

// -----------------------------------------------------------------------------

function TUWTextBox.GetText: String;
begin
  Result := FBuffer.FText;
end;

// -----------------------------------------------------------------------------

procedure TUWTextBox.SetBackColor(const AColor: TColor);
begin
  FBuffer.FBackColor := AColor;
end;

// -----------------------------------------------------------------------------

function TUWTextBox.GetBackColor: TColor;
begin
  Result := FBuffer.FBackColor;
end;

// -----------------------------------------------------------------------------

procedure TUWTextBox.SetSpacing(const ASpacing: Byte);
begin
  FBuffer.FSpacing := ASpacing;
end;

// -----------------------------------------------------------------------------

function TUWTextBox.GetSpacing: Byte;
begin
  Result := FBuffer.FSpacing;
end;

// -----------------------------------------------------------------------------

procedure TUWTextBox.SetTransparent(const ATransparent: Boolean);
begin
  FBuffer.FTransparent := ATransparent;
end;

// -----------------------------------------------------------------------------

function TUWTextBox.GetTransparent: Boolean;
begin
  Result := FBuffer.FTransparent;
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

