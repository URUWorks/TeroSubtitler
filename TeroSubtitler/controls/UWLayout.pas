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

unit UWLayout;

//------------------------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Controls, SysUtils, LCLType, LazarusPackageIntf;

type

  { TUWLayout }

  TUWLayout = class(TCustomControl)
  public
    constructor Create(TheOwner: TComponent); override;
  protected
    procedure Paint; override;
  published
    property Align default alNone;
    property Anchors;
    property AutoSize;
    property BidiMode;
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
    property ParentBidiMode;
    property ParentColor;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default False;
    property UseDockManager default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

procedure Register;

// -----------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------

{ TUWLayout }

//------------------------------------------------------------------------------

constructor TUWLayout.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FCompStyle   := csPanel;
  ControlStyle := ControlStyle + [csAcceptsControls, csCaptureMouse,
    csClickEvents, csDoubleClicks, csReplicatable,
    csNoFocus, csAutoSize0x0, csParentBackground]
    - [csOpaque];

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);

  UseDockManager := True;
  // Accessibility
  AccessibleRole := larGroup;
  AccessibleDescription := 'Layout';
end;

//------------------------------------------------------------------------------

procedure TUWLayout.Paint;
begin
  inherited Paint;

  if (csDesigning in ComponentState) then
    Canvas.DrawFocusRect(Rect(0, 0, Width, Height));
end;

// -----------------------------------------------------------------------------

procedure RegisterTeroControlsUnit;
begin
  RegisterComponents('URUWorks Tero', [TUWLayout]);
end;

// -----------------------------------------------------------------------------

procedure Register;
begin
  RegisterUnit('UWLayout', @RegisterTeroControlsUnit);
end;

// -----------------------------------------------------------------------------

end.

