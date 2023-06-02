{*
 *  URUWorks GroupBox
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

unit UWGroupBox;

//------------------------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Controls, SysUtils, Graphics, StdCtrls, LCLType, LCLIntf,
  LazarusPackageIntf, Types;

type

  { TUWGroupBox }

  TUWGroupBox = class(TGroupBox)
  {$IFDEF WINDOWS}
  private
    FOwnerDraw: Boolean;
    procedure SetOwnerDraw(const AValue: Boolean);
  public
    constructor Create(TheOwner: TComponent); override;
  {$ENDIF}
  published
    {$IFDEF WINDOWS}
    property OwnerDraw: Boolean read FOwnerDraw write SetOwnerDraw;
    {$ENDIF}
    property Align;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Caption;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBackground;
    property ParentBidiMode;
    property ParentColor;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDockDrop;
    property OnDockOver;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
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
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property OnUTF8KeyPress;
  end;

procedure Register;

// -----------------------------------------------------------------------------

implementation

{$IFDEF WINDOWS}
uses
  Windows, CommCtrl, Themes;

const
  ID_UWGROUPBOX = 1;
  TEXT_MARGIN   = 8;

//------------------------------------------------------------------------------

{ UWGroupBox }

//------------------------------------------------------------------------------

function NewWndProc(Wnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM; uISubClass: UINT_PTR; dwRefData: DWORD_PTR): LRESULT; stdcall;
var
  AGroupBox: TUWGroupBox absolute dwRefData;
  ACanvas: TCanvas;
  DC: HDC;
  PS: PAINTSTRUCT;
  R: TRect;
  CaptionRect,
  OuterRect: TRect;
  Box: TThemedButton;
  Details: TThemedElementDetails;
  ATextStyle: TTextStyle;
  fw, fh: Integer;
begin
  case uMsg of
    //WM_NCPAINT,
    WM_PAINT:
    begin
      DC := BeginPaint(Wnd, @PS);
      try
        ACanvas := TCanvas.Create;
        try
          with ACanvas do
          begin
            Handle      := DC;
            Brush.Style := bsClear;
            Font        := AGroupBox.Font;
            Brush.Color := AGroupBox.Color;
            FillRect(PS.rcPaint);
            Font.GetTextSize(AGroupBox.Caption, fw, fh);

            if AGroupBox.Caption <> '' then
            begin
              CaptionRect := Types.Rect(0, 0, fw+1, fh);
              if not AGroupBox.UseRightToLeftAlignment then
                OffsetRect(CaptionRect, TEXT_MARGIN, 0)
              else
                OffsetRect(CaptionRect, Width-TEXT_MARGIN-CaptionRect.Right, 0);
            end
            else
              CaptionRect := Types.Rect(0, 0, 0, 0);

            OuterRect     := PS.rcPaint;
            OuterRect.Top := (CaptionRect.Bottom-CaptionRect.Top) div 2;

            with CaptionRect do
              ExcludeClipRect(Handle, Left, Top, Right, Bottom);

            if AGroupBox.Enabled then
              Box := tbGroupBoxNormal
            else
              Box := tbGroupBoxDisabled;

            // Draw themed frame
            Details := ThemeServices.GetElementDetails(Box);
            ThemeServices.DrawElement(Handle, Details, OuterRect);
            SelectClipRgn(Handle, 0);
            // Draw caption
            if AGroupBox.Caption <> '' then
            begin
              if not AGroupBox.UseRightToLeftAlignment then
                R := Types.Rect(TEXT_MARGIN, 0, fw+TEXT_MARGIN, fh)
              else
                R := Types.Rect(R.Right-fw-TEXT_MARGIN, 0, Width-TEXT_MARGIN, fh);

              FillByte(ATextStyle, SizeOf(TTextStyle), 0);
              with ATextStyle do
              begin
                SingleLine  := True;
                Clipping    := True;
                RightToLeft := AGroupBox.UseRightToLeftAlignment;
              end;
              ACanvas.TextRect(R, R.Left, R.Top, AGroupBox.Caption, ATextStyle);
            end;
          end
        finally
          ReleaseDC(Wnd, ACanvas.Handle);
          ACanvas.Free;
        end;
      finally
        EndPaint(Wnd, @PS);
      end;
      Result:= 0;
    end;
  else
    Result := DefSubclassProc(Wnd, uMsg, wParam, lParam);
  end
end;

//------------------------------------------------------------------------------

constructor TUWGroupBox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FOwnerDraw := False;
end;

//------------------------------------------------------------------------------

procedure TUWGroupBox.SetOwnerDraw(const AValue: Boolean);
begin
  if FOwnerDraw <> AValue then
  begin
    FOwnerDraw := AValue;

    if FOwnerDraw then
      SetWindowSubclass(Handle, @NewWndProc, ID_UWGROUPBOX, DWORD_PTR(Self))
    else
      RemoveWindowSubclass(Handle, @NewWndProc, ID_UWGROUPBOX);

    Invalidate;
  end;
end;
{$ENDIF}

// -----------------------------------------------------------------------------

procedure RegisterTeroControlsUnit;
begin
  RegisterComponents('URUWorks Tero', [TUWGroupBox]);
end;

// -----------------------------------------------------------------------------

procedure Register;
begin
  RegisterUnit('UWGroupBox', @RegisterTeroControlsUnit);
end;

// -----------------------------------------------------------------------------

end.

