{*
 *  URUWorks HotKey
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

unit UWHotKey;

//------------------------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Controls, StdCtrls, ComCtrls, LCLType, SysUtils,
  Graphics, LazarusPackageIntf, LCLProc;

type

  { TUWHotKey }

  TUWHotKey = class(TCustomEdit)
  private
    FHotKey    : TShortCut;
    FEmptyText : String;
    procedure SetHotKey(const AHotKey: TShortCut);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property EmptyText : String    read FEmptyText write FEmptyText;
    property HotKey    : TShortcut read FHotKey    write SetHotKey;

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
    property ParentBidiMode;
    property OnChange;
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

{ Helpers }

function ShortCutToTextEx(const AShortCut: TShortCut): String;
procedure Register;

//------------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

constructor TUWHotKey.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  AutoSelect := False;
  AutoSize   := False;
  FEmptyText := 'None';
  HotKey     := 0;
end;

// -----------------------------------------------------------------------------

procedure TUWHotKey.KeyDown(var Key: Word; Shift: TShiftState);
var
  newShift, newShortCut: TShortCut;
  s: String;
begin
  try
    if (Key = 8) and (Shift = []) and (FHotKey = Key) then
    begin
      FHotKey := 0;
      Text    := FEmptyText;
      Exit;
    end;

    newShift := 0;

    if ssShift in Shift then
      newShift := newShift or scShift;

    if ssCtrl in Shift then
      newShift := newShift or scCtrl;

    if ssAlt in Shift then
      newShift := newShift or scAlt;

    if ssMeta in Shift then
      newShift := newShift or scMeta;

    newShortCut := Key or newShift;
    s := ShortCutToTextEx(newShortCut);
    if (s <> '') then
    begin
      FHotKey := newShortCut;
      Text    := s;
    end
    else
    begin
      FHotKey := 0;
      Text    := FEmptyText;
    end;
  finally
    Key := 0;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWHotKey.SetHotKey(const AHotKey: TShortCut);
var
  s: String;
begin
  FHotKey := AHotKey;
  if FHotKey = 0 then
    s := FEmptyText
  else
    s := ShortCutToTextEx(FHotKey);

  Text := s;
end;

// -----------------------------------------------------------------------------

{ Helpers }

// -----------------------------------------------------------------------------

function ShortCutToTextEx(const AShortCut: TShortCut): String;
begin
  Result := ShortCutToText(AShortCut){$IFDEF DARWIN}.Replace('Meta', #$2318){$ENDIF};
  if Result = 'Unknown' then Result := '';
end;

// -----------------------------------------------------------------------------

procedure RegisterTeroControlsUnit;
begin
  RegisterComponents('URUWorks Tero', [TUWHotKey]);
end;

// -----------------------------------------------------------------------------

procedure Register;
begin
  RegisterUnit('UWHotKey', @RegisterTeroControlsUnit);
end;

// -----------------------------------------------------------------------------

end.

