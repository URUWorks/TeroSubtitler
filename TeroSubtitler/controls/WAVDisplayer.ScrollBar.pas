{*
 *  URUWorks Waveform Displayer Control
 *
 *  Copyright (C) 2021-2023 URUWorks, uruworks@gmail.com.
 *
 *  Based on the great work of:
 * -----------------------------------------------------------------------------
 *  VisualSubSync
 * -----------------------------------------------------------------------------
 *  Copyright (C) 2003 Christophe Paris
 * -----------------------------------------------------------------------------
 *  This Program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This Program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with GNU Make; see the file COPYING.  If not, write to
 *  the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 *  http://www.gnu.org/copyleft/gpl.html
 * -----------------------------------------------------------------------------
 *}

unit WAVDisplayer.ScrollBar;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Controls, Graphics, ExtCtrls, LMessages, Math,
  UWSystem.SysUtils, LazarusPackageIntf;

type

  { TUWScrollBar }

  TUWScrollBarThumb = class(TCustomPanel);

  TUWScrollBar = class(TCustomPanel)
  private
    FThumb            : TUWScrollBarThumb;
    FTracking         : Boolean;
    FTrackStartPointX : Integer;
    FMax, FMin        : Integer;
    FPageSize         : Integer;
    FPosition         : Integer;
    FThumbMinSize     : Integer;
    FOnChange         : TNotifyEvent;
    FColorNormal      : TColor;
    FColorDown        : TColor;
    FScrollWithTimer  : Boolean;
    FRepeatTimer      : TTimer;
    FLastXOnMouse     : Integer;
    FOnBeginScrolling : TNotifyEvent;
    FOnEndScrolling   : TNotifyEvent;
    procedure OnThumbMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnThumbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure OnThumbMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnRepeatTimer(Sender: TObject);
    procedure UpdateThumb;
    procedure SetMax(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure SetPageSize(Value: Integer);
    procedure SetPosition(Value: Integer);
  protected
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetPositionAndPageSize(NewPosition, NewPageSize: Integer);
  published
    property Max              : Integer      read FMax             write SetMax;
    property Min              : Integer      read FMin             write SetMin;
    property ScrollWithTimer  : Boolean      read FScrollWithTimer write FScrollWithTimer;
    property PageSize         : Integer      read FPageSize        write SetPageSize;
    property Position         : Integer      read FPosition        write SetPosition;
    property OnChange         : TNotifyEvent read FOnChange        write FOnChange;
    property OnBeginScrolling : TNotifyEvent read FOnChange        write FOnChange;
    property OnEndScrolling   : TNotifyEvent read FOnChange        write FOnChange;
  end;

//------------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------

{ TUWScrollBar }

// -----------------------------------------------------------------------------

constructor TUWScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FMax                  := 100;
  FMin                  := 0;
  FPosition             := 0;
  FPageSize             := 10;
  FThumbMinSize         := 5;
  FColorNormal          := $00A56E3A;
  FColorDown            := $00D5965E;
  Self.Color            := clGray;
  Self.BevelOuter       := bvNone;
  Self.Caption          := '';

  FThumb                := TUWScrollBarThumb.Create(Self);
  FThumb.Parent         := Self;
  FThumb.Color          := FColorNormal;
  FThumb.BevelOuter     := bvNone;
  FThumb.OnMouseDown    := @OnThumbMouseDown;
  FThumb.OnMouseMove    := @OnThumbMouseMove;
  FThumb.OnMouseUp      := @OnThumbMouseUp;
  FThumb.Caption        := '';
  FThumb.Left           := 0;
  UpdateThumb;

  FScrollWithTimer      := True;
  FRepeatTimer          := TTimer.Create(Self);
  FRepeatTimer.Enabled  := False;
  FRepeatTimer.Interval := 150;
  FRepeatTimer.OnTimer  := @OnRepeatTimer;
end;

//------------------------------------------------------------------------------

procedure TUWScrollBar.WMSize(var Message: TLMSize);
begin
  inherited;
  FThumb.Height := Height;
  UpdateThumb;
end;

//------------------------------------------------------------------------------

procedure TUWScrollBar.OnRepeatTimer(Sender: TObject);
begin
  // Are we on the thumb ?
  if (FLastXOnMouse >= FThumb.Left) and
     (FLastXOnMouse <= FThumb.Left+FThumb.Width) then
  begin
    FRepeatTimer.Enabled := False;
    Exit;
  end;

  if FLastXOnMouse > FThumb.Left then
    Position := Position + PageSize
  else if FLastXOnMouse < FThumb.Left then
    Position := Position - PageSize;

  if Assigned(FOnChange) then FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TUWScrollBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  NewX: Integer;
begin
  inherited;
  if (FMax = FMin) and (FMax = 0) then  Exit;

  if Assigned(FOnBeginScrolling) then FOnBeginScrolling(Self);

  if FScrollWithTimer then
  begin
    if X > FThumb.Left then
      Position := Position + PageSize
    else if X < FThumb.Left then
      Position := Position - PageSize;

    FRepeatTimer.Enabled := True;
  end
  else
  begin
    NewX := X - FThumb.Width div 2;
    Constrain(NewX, 0, Width - FThumb.Width);
    FThumb.Left := NewX;
    FPosition := MulDiv(NewX, (FMax - FMin + 1)-Math.Max(1, FPageSize), Width - FThumb.Width)+FMin;
  end;

  if Assigned(FOnChange) then FOnChange(Self);
  FLastXOnMouse := X;
end;

//------------------------------------------------------------------------------

procedure TUWScrollBar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FLastXOnMouse := X;
end;

//------------------------------------------------------------------------------

procedure TUWScrollBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FRepeatTimer.Enabled and Assigned(FOnEndScrolling) then FOnEndScrolling(Self);
  FRepeatTimer.Enabled := False;
end;

//------------------------------------------------------------------------------

procedure TUWScrollBar.OnThumbMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (FMax = FMin) and (FMax = 0) then  Exit;

  if ssLeft in Shift then
  begin
    FTracking           := True;
    FTrackStartPointX   := X;
    FThumb.Color        := FColorDown;
    FThumb.MouseCapture := True;
    if Assigned(FOnBeginScrolling) then FOnBeginScrolling(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TUWScrollBar.OnThumbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  NewX: Integer;
begin
  if (FMax = FMin) and (FMax = 0) then Exit;

  if FTracking then
  begin
    NewX := X - FTrackStartPointX + FThumb.Left;
    Constrain(NewX, 0, Width - FThumb.Width);

    if FThumb.Left <> NewX then
    begin
      FThumb.Left  := NewX;
      FThumb.Color := FColorDown;
      // Update FPosition
      FPosition := MulDiv(NewX, (FMax - FMin + 1)-Math.Max(1,FPageSize),
        Width - FThumb.Width)+FMin;

      //Assert(FPosition >= FMin);
      //Assert(FPosition <= FMax);

      Constrain(FPosition, FMin, FMax - Math.Max(FPageSize-1, 0));
      if Assigned(FOnChange) then FOnChange(Self);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TUWScrollBar.OnThumbMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (FMax = FMin) and (FMax = 0) then Exit;

  FThumb.MouseCapture := False;
  FTracking           := False;
  FThumb.Color        := clBlue;
  FThumb.Color        := FColorNormal;

  if Assigned(FOnEndScrolling) then FOnEndScrolling(Self);
end;

//------------------------------------------------------------------------------

procedure TUWScrollBar.SetMax(Value: Integer);
begin
  if Value <> FMax then
  begin
    FMax           := Value;
    FThumb.Visible := (FMax <> FMin);
    UpdateThumb;
  end;
end;

//------------------------------------------------------------------------------

procedure TUWScrollBar.SetMin(Value: Integer);
begin
  if Value <> FMin then
  begin
    FMin           := Value;
    FThumb.Visible := (FMax <> FMin);
    UpdateThumb;
  end;
end;

//------------------------------------------------------------------------------

procedure TUWScrollBar.SetPageSize(Value: Integer);
begin
  Constrain(Value, 0, FMax - FMin + 1);
  if Value <> FPageSize then
  begin
    FPageSize := Value;
    UpdateThumb;
  end;
end;

//------------------------------------------------------------------------------

procedure TUWScrollBar.SetPosition(Value: Integer);
begin
  Constrain(Value, FMin, FMax - Math.Max(FPageSize-1, 0));
  if Value <> FPosition then
  begin
    FPosition := Value;
    UpdateThumb;
  end;
end;

//------------------------------------------------------------------------------

procedure TUWScrollBar.SetPositionAndPageSize(NewPosition, NewPageSize: Integer);
begin
  Constrain(NewPageSize, 0, FMax - FMin + 1);
  Constrain(NewPosition, FMin, FMax - Math.Max(NewPageSize-1, 0));
  if (NewPageSize <> FPageSize) or (NewPosition <> FPosition) then
  begin
    FPageSize := NewPageSize;
    FPosition := NewPosition;
    UpdateThumb;
  end;
end;

//------------------------------------------------------------------------------

procedure TUWScrollBar.UpdateThumb;
var
  NewThumbWidth, NewPosition, MaxMin : Integer;
begin
  MaxMin := (FMax - FMin) + 1;
  if FPageSize = 0 then
    NewThumbWidth := FThumbMinSize
  else
  begin
    NewThumbWidth := MulDiv(FPageSize, Width, MaxMin);
    if NewThumbWidth < FThumbMinSize then NewThumbWidth := FThumbMinSize;
  end;

  NewPosition := MulDiv(FPosition - FMin, Width - NewThumbWidth, MaxMin - Math.Max(1, FPageSize));
  Constrain(NewPosition, 0, Width - NewThumbWidth);

  FThumb.SetBounds(NewPosition, FThumb.Top, NewThumbWidth, FThumb.Height);
end;

//------------------------------------------------------------------------------

end.
