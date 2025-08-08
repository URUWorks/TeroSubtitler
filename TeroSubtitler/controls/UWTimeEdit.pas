{*
 *  URUWorks TimeEdit
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

unit UWTimeEdit;

//------------------------------------------------------------------------------

{$mode ObjFPC}{$H+}
{.$DEFINE INSIDECONTROL} // TUpDown inside or next to

interface

uses
  Classes, Controls, StdCtrls, ComCtrls, LCLType, LMessages, SysUtils,
  Graphics, LazarusPackageIntf;

type

  { TUWTimeEdit }

  TUWTimeEditMode        = (temTime, temFrames);
  TUWTimeEditChangeEvent = procedure(Sender: TObject; const NewTime: Cardinal) of object;

  TUWTimeEdit = class(TCustomEdit)
  private
    FUpDown      : TUpDown;
    {$IFNDEF INSIDECONTROL}
    FSpinSpacing : Integer;
    {$ENDIF}
    FTimeMode    : TUWTimeEditMode;
    FValue       : Integer;
    FFPS         : Single;
    FTimeStep    : Word;
    FFrameStep   : Word;
    FChangeEvent : TUWTimeEditChangeEvent;
    procedure SetFPS(const AFPS: Single);
    procedure SetTimeMode(const ATimeMode: TUWTimeEditMode);
    procedure SetValueFromString(const S: String);
    procedure SetValue(const NewValue: Integer);
    procedure UpdateValue(const FireChangeEvent: Boolean = True);
    procedure SetSel(const Start, Len: Integer);
    procedure IncFrame(const Increment: Boolean);
    procedure DoTimeDown;
    procedure DoTimeUp;
    procedure UpDownClick(Sender: TObject; Button: TUDBtnType);
    {$IFDEF INSIDECONTROL}
    procedure UpDownMouseEnter(Sender: TObject);
    procedure UpDownMouseLeave(Sender: TObject);
    {$ELSE}
    procedure SetSpinSpacing(const Value: Integer);
    procedure DoPositionSpin;
    {$ENDIF}
  protected
    {$IFDEF INSIDECONTROL}
    procedure CreateParams(var Params: TCreateParams); override;
    function ChildClassAllowed(ChildClass: TClass): Boolean; override;
    procedure AdjustClientRect(var ARect: TRect); override;
    {$ENDIF}
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure CMVisibleChanged(var Msg: TLMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledChanged(var Msg: TLMessage); message CM_ENABLEDCHANGED;
    {$IFNDEF INSIDECONTROL}
    procedure SetParent(AParent: TWinControl); override;
    procedure Loaded; override;
    {$ELSE}
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetValueOnly(const NewValue: Integer);
    procedure SetFPSValueOnly(const AFPS: Single);
    procedure ReAdjustWidth;
  published
    property Value        : Integer                read FValue       write SetValue;
    property FPS          : Single                 read FFPS         write SetFPS;
    property FrameStep    : Word                   read FFrameStep   write FFrameStep;
    property TimeStep     : Word                   read FTimeStep    write FTimeStep;
    property TimeMode     : TUWTimeEditMode        read FTimeMode    write SetTimeMode;
    property OnTimeChange : TUWTimeEditChangeEvent read FChangeEvent write FChangeEvent;

    property Align;
    property Alignment;
    property Anchors;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EchoMode;
    property Enabled;
    property Font;
    property HideSelection;
    property Hint;
    property ParentBidiMode;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
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
    property OnStartDrag;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property TextHint;
    property Visible;
  end;

procedure Register;

//------------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------

uses
  UWSystem.SysUtils, UWSystem.TimeUtils, Math;

const
  OneSecond = 1000;
  OneMinute = OneSecond * 60;
  OneHour   = OneMinute * 60;
  MaxTime   = 86399999; // 23:59:59.999

// -----------------------------------------------------------------------------

constructor TUWTimeEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Constraints.MinWidth  := 90;
  Constraints.MinHeight := 21;
  Width      := 90;
  Height     := 23;
  FFPS       := 25;
  FValue     := 0;
  FTimeStep  := 1;
  FFrameStep := 1;
  FTimeMode  := TUWTimeEditMode.temTime;
  Text       := '00:00:00.000';

  AutoSelect  := False;
  AutoSize    := False;
  Alignment   := taCenter;
  Font.Style  := [fsBold];

  {$IFNDEF INSIDECONTROL}
  FSpinSpacing := 0;
  {$ELSE}
  ChildSizing.SetGridSpacing(0);
  {$ENDIF}

  FUpDown := TUpDown.Create(Self);
  with FUpDown do
  begin
    {$IFNDEF INSIDECONTROL}
    ControlStyle := ControlStyle + [csNoDesignSelectable];
    {$ENDIF}
    SetSubComponent(True);
    Width     := 14;
    Height    := Self.Height;
    Increment := 1;
    Wrap      := True;
    Flat      := True;
    Visible   := False;
    OnClick   := @UpDownClick;
    {$IFDEF INSIDECONTROL}
    OnMouseEnter := @UpDownMouseEnter;
    OnMouseLeave := @UpDownMouseLeave;
    {$ENDIF}
  end;

  ReAdjustWidth;
end;

// -----------------------------------------------------------------------------

destructor TUWTimeEdit.Destroy;
begin
  FreeAndNIL(FUpDown);

  inherited Destroy;
end;

// -----------------------------------------------------------------------------

{$IFDEF INSIDECONTROL}
procedure TUWTimeEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or WS_CLIPCHILDREN;
end;

// -----------------------------------------------------------------------------

function TUWTimeEdit.ChildClassAllowed(ChildClass: TClass): Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

procedure TUWTimeEdit.AdjustClientRect(var ARect: TRect);
begin
  inherited;
  with FUpDown do
  begin
    Align   := alNone;
    Anchors := [akLeft, akTop];
    Parent  := Self.Parent;
    Left    := Self.Left + Self.Width - Width;
    Top     := Self.Top;
    Height  := Self.Height;
  end;
end;
{$ENDIF}

// -----------------------------------------------------------------------------

procedure TUWTimeEdit.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
begin
  Handled := True; // Turn off default right mouse click popup
end;

// -----------------------------------------------------------------------------

procedure TUWTimeEdit.SetSel(const Start, Len: Integer);
begin
  SelStart  := Start;
  SelLength := Len;
end;

// -----------------------------------------------------------------------------

procedure TUWTimeEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // 01 34 67 901
  //   2  5  8
  // 00:00:00,000 times
  // 00:00:00:00  frames
  // if clicked on colon or comma, then fix cursor position
  case SelStart of
     0..1  : SetSel(0, 2);
     3..4  : SetSel(3, 2);
     6..7  : SetSel(6, 2);
     9..12 : SetSel(9, 3);
     2, 5  : SetSel(SelStart + 1, 2);
  else
    if FTimeMode = TUWTimeEditMode.temTime then
      SetSel(SelStart + 1, 3)
    else
      SetSel(SelStart + 1, 2);
  end;

  Abort;
end;

// -----------------------------------------------------------------------------

procedure TUWTimeEdit.KeyDown(var Key: Word; Shift: TShiftState);
const
  MINMASK = '00:00:00.000';
  MAXMASK = '23:59:59.999';

var
  s        : String;
  SelSt    : Integer;
  KeyChar  : Char;

  FMINMASK : String;
  FMAXMASK : String;
begin
  if Key = VK_ESCAPE then Exit;

  try
    if Key = VK_TAB then
    begin
      PerformTab(True);
      Exit;
    end;

    case Key of
      VK_LEFT  : if (SelStart > 0) then
                 begin
                   if SelStart in [3, 6, 9] then
                      SelStart := SelStart - 3
                   else if SelStart in [1, 4, 7, 10] then
                     SelStart := SelStart - 1
                   else if (FTimeMode = TUWTimeEditMode.temTime) and (SelStart in [11]) then
                     SelStart := SelStart - 2
                   else
                     SelStart := 0;

                   if (FTimeMode = TUWTimeEditMode.temTime) and (SelStart = 9) then
                     SelLength := 3
                   else
                     SelLength := 2;
                 end;

      VK_RIGHT : if (SelStart < 11) then
                 begin
                   if SelStart in [0, 3, 6] then
                     SelStart := SelStart + 3
                   else if SelStart in [1, 4, 7] then
                     SelStart := SelStart + 2
                   else if SelStart in [10] then
                     SelStart := 9
                   else if (FTimeMode = TUWTimeEditMode.temTime) and (SelStart in [11]) then
                     SelStart := 9;

                   if (FTimeMode = TUWTimeEditMode.temTime) and (SelStart = 9) then
                     SelLength := 3
                   else
                     SelLength := 2;
                 end;

       VK_UP   : DoTimeUp;
       VK_DOWN : DoTimeDown;
    else
      KeyChar := Chr(Key);
      SelSt   := SelStart;
      s       := Text;

      if CharInSet(KeyChar, [#96..#105]) then // numpad 0-9
        KeyChar := Chr(Key - 48)
      else if not CharInSet(KeyChar, [#48..#57]) then // 0-9
        Exit;

      if (FTimeMode = TUWTimeEditMode.temTime) then
      begin
        if KeyChar > MAXMASK[SelSt+1] then
          s[SelSt+1] := MAXMASK[SelSt+1]
        else if KeyChar < MINMASK[SelSt+1] then
          s[SelSt+1] := MINMASK[SelSt+1]
        else
          s[SelSt+1] := KeyChar;

        SetValueFromString(s);
      end
      else
      begin
        FMINMASK := '00:00:00:00';
        FMAXMASK := MSToHHMMSSFFMax(MaxTime, FFPS);

        if ((SelSt+1)=11) and (Text[10] < FMAXMASK[10]) then
          s[SelSt+1] := KeyChar
        else if KeyChar > FMAXMASK[SelSt+1] then
          s[SelSt+1] := FMAXMASK[SelSt+1]
        else if KeyChar < FMINMASK[SelSt+1] then
          s[SelSt+1] := FMINMASK[SelSt+1]
        else
          s[SelSt+1] := KeyChar;

        SetValueFromString(s);
      end;

      // put cursor to next position
      // 01 34 67 901
      //   2  5  8
      // 00:00:00,000 times
      // 00:00:00:00  frames
      case SelSt of
        1, 4, 7  : SelStart := SelSt + 2;
        0,2,3,5,
        6,8,9    : SelStart := SelSt + 1;
        10       : if (FTimeMode = TUWTimeEditMode.temTime) then SelStart := SelSt + 1;
      else
        SelStart := 0;
      end;

      SelLength := 1;
    end;

    if Assigned(FChangeEvent) then FChangeEvent(Self, FValue);
  finally
    Key := VK_UNKNOWN;
    inherited KeyDown(Key, Shift);
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWTimeEdit.IncFrame(const Increment: Boolean);
var
  Wpart : Integer = 0; //WholePart
  Fpart : Integer = 0; //Fraction part
  Frames : Integer = 0;
begin
  Wpart := Trunc(FValue/1000)*1000;
  Fpart := FValue - Wpart;
  Frames := Trunc(Fpart*FFPS/1000);

  if not Increment then
    Value := Wpart + Ceil((Frames-1)*1000/FFPS) + Ceil(100/FFPS) //Add 10 % of the frame duration, to make sure that the control will stay within the frame.
  else
    Value := Wpart + Ceil((Frames+1.1)*1000/FFPS);
end;

// -----------------------------------------------------------------------------

procedure TUWTimeEdit.DoTimeDown;

  procedure DecHour;
  begin
    if FValue >= OneHour then
    begin
      Value := FValue - OneHour;
      SetSel(0, 2);
    end
    else
      SetSel(3, 2);
  end;

  procedure DecMin;
  begin
    if FValue >= OneMinute then
    begin
      Value := FValue - OneMinute;
      SetSel(3, 2);
    end
    else
      SetSel(6, 2);
  end;

  procedure DecSec;
  begin
    if FValue >= OneSecond then
    begin
      Value := FValue - OneSecond;
      SetSel(6, 2);
    end
    else
      SetSel(9, 3);
  end;

  procedure DecMSec;
  begin
    if FValue > 10 then
      Value := FValue - FTimeStep
    else
      Value := 0;

    SetSel(9, 3);
  end;

  procedure DecFrame;
  begin
    {if FValue > FramesToTime(FFrameStep, FFPS) then
      Value := FValue - FramesToTime(FFrameStep, FFPS)
    else
      Value := 0;}

    IncFrame(False);
    SetSel(9, 2);
  end;

begin
  if FValue > 0 then
  begin
    if SelLength <= 1 then
    begin
      if (FTimeMode = TUWTimeEditMode.temTime) then
        SetSel(9, 3)
      else
        SetSel(9, 2);
    end;

    case SelStart of
      0..1  : DecHour;
      3..4  : DecMin;
      6..7  : DecSec;
      9..11 : if FTimeMode = TUWTimeEditMode.temTime then
                DecMSec
              else
                DecFrame;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWTimeEdit.DoTimeUp;
begin
  if SelLength <= 1 then
  begin
    if FTimeMode = TUWTimeEditMode.temTime then
      SetSel(9, 3)
    else
      SetSel(9, 2);
  end;

  case SelStart of
     0..1  : begin
               Value := FValue + OneHour;
               SetSel(0, 2);
             end;
     3..4  : begin
               Value := FValue + OneMinute;
               SetSel(3, 2);
             end;
     6..7  : begin
               Value := FValue + OneSecond;
               SetSel(6, 2);
             end;
     9..11 : if FTimeMode = TUWTimeEditMode.temTime then
             begin
               Value := FValue + FTimeStep;
               SetSel(9, 3);
             end
             else
             begin
               //Value := FValue + FramesToTime(FFrameStep, FFPS);
               IncFrame(True);
               SetSel(9, 2);
             end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWTimeEdit.SetValue(const NewValue: Integer);
begin
  if FValue <> NewValue then
  begin
    FValue := NewValue;
    Constrain(FValue, 0, MaxTime);
    UpdateValue;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWTimeEdit.SetValueOnly(const NewValue: Integer);
begin
  //if FValue <> NewValue then
  //begin
    FValue := NewValue;
    Constrain(FValue, 0, MaxTime);
    UpdateValue(False);
  //end;
end;

// -----------------------------------------------------------------------------

procedure TUWTimeEdit.UpdateValue(const FireChangeEvent: Boolean = True);
begin
  if FTimeMode = TUWTimeEditMode.temFrames then
    Text := TimeToString(FValue, 'hh:mm:ss:ff', FFPS)
  else
    Text := TimeToString(FValue, Format('hh:mm:ss%szzz', [DefaultFormatSettings.DecimalSeparator])); //Text := TimeToString(FValue, 'hh:mm:ss.zzz');

  if FireChangeEvent and Assigned(FChangeEvent) then FChangeEvent(Self, FValue);
end;

// -----------------------------------------------------------------------------

procedure TUWTimeEdit.SetFPS(const AFPS: Single);
begin
  FFPS := AFPS;
  UpdateValue;
end;

// -----------------------------------------------------------------------------

procedure TUWTimeEdit.SetFPSValueOnly(const AFPS: Single);
begin
  FFPS := AFPS;
end;

// -----------------------------------------------------------------------------

procedure TUWTimeEdit.SetTimeMode(const ATimeMode: TUWTimeEditMode);
begin
  FTimeMode := ATimeMode;
  UpdateValue;
end;

// -----------------------------------------------------------------------------

procedure TUWTimeEdit.SetValueFromString(const S: String);
begin
  if FTimeMode = TUWTimeEditMode.temFrames then
    FValue := StringToTime(S, False, FFPS)
  else
    FValue := StringToTime(S);

  Text := S;
end;

// -----------------------------------------------------------------------------

procedure TUWTimeEdit.UpDownClick(Sender: TObject; Button: TUDBtnType);
begin
  if not Focused then
  begin
    SetFocus;
    SelLength := 0;
  end;

  if Button = btNext then
    DoTimeUp
  else
    DoTimeDown;
end;

// -----------------------------------------------------------------------------

function TUWTimeEdit.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  if WheelDelta > 0 then
    DoTimeUp
  else
    DoTimeDown;

  Result := True;
end;

// -----------------------------------------------------------------------------

procedure TUWTimeEdit.CMVisibleChanged(var Msg: TLMessage);
begin
  inherited CMVisiblechanged(Msg);

  if FUpDown <> NIL then FUpDown.Visible := Visible;
end;

// -----------------------------------------------------------------------------

procedure TUWTimeEdit.CMEnabledChanged(var Msg: TLMessage);
begin
  inherited CMEnabledChanged(Msg);

  if FUpDown <> NIL then FUpDown.Enabled := Enabled;
end;

// -----------------------------------------------------------------------------

{$IFNDEF INSIDECONTROL}
procedure TUWTimeEdit.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  DoPositionSpin;
end;

// -----------------------------------------------------------------------------

procedure TUWTimeEdit.Loaded;
begin
  inherited Loaded;
  DoPositionSpin;
end;

// -----------------------------------------------------------------------------

procedure TUWTimeEdit.SetSpinSpacing(const Value: Integer);
begin
  if Value = FSpinSpacing then Exit;
  FSpinSpacing := Value;
  DoPositionSpin;
end;

// -----------------------------------------------------------------------------

procedure TUWTimeEdit.DoPositionSpin;
begin
  if Parent <> NIL then
    Parent.DisableAlign;

  if FUpDown <> NIL then
  begin
    FUpDown.Parent  := Parent;
    FUpDown.Visible := Visible;
    FUpDown.AnchorToCompanion(akLeft, FSpinSpacing, Self);
    FUpDown.AnchorVerticalCenterTo(Self);
  end;

  if Parent <> NIL then
    Parent.EnableAlign;
end;
{$ELSE}
// -----------------------------------------------------------------------------

procedure TUWTimeEdit.MouseEnter;
begin
  inherited MouseEnter;
  FUpDown.Visible := True;
end;

procedure TUWTimeEdit.MouseLeave;
begin
  inherited MouseLeave;
  FUpDown.Visible := False;
end;

// ------------------------------------------------------------------------------

procedure TUWTimeEdit.UpDownMouseEnter(Sender: TObject);
begin
  if not FUpDown.Visible then
    FUpDown.Visible := True;
end;

// ------------------------------------------------------------------------------

procedure TUWTimeEdit.UpDownMouseLeave(Sender: TObject);
begin
  if FUpDown.Visible then
    FUpDown.Visible := False;
end;
{$ENDIF}

// ------------------------------------------------------------------------------

procedure TUWTimeEdit.ReAdjustWidth;
var
  lbl: TLabel;
begin
  if Text <> '' then
  begin
    lbl := TLabel.Create(NIL);
    try
      lbl.Font     := Font;
      lbl.AutoSize := True;
      lbl.Caption  := Text;
      {$IFDEF INSIDECONTROL}
      if Alignment = taCenter then
        Width := lbl.Width + (FUpDown.Width*4)
      else
        Width := lbl.Width + (FUpDown.Width*2);
      {$ELSE}
      Width := lbl.Width;
      {$ENDIF}
     finally
       lbl.Free;
     end;
  end;
end;

// -----------------------------------------------------------------------------

procedure RegisterTeroControlsUnit;
begin
  RegisterComponents('URUWorks Tero', [TUWTimeEdit]);
end;

// -----------------------------------------------------------------------------

procedure Register;
begin
  RegisterUnit('UWTimeEdit', @RegisterTeroControlsUnit);
end;

// -----------------------------------------------------------------------------

end.

