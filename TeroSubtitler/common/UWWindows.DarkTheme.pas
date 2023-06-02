{*
 *  URUWorks Windows Dark Theme
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
 *  Copyright (C) 2021-2023 URUWorks, uruworks@gmail.com.
 *}

unit UWWindows.DarkTheme;

// -----------------------------------------------------------------------------

{$IFDEF WINDOWS}
{$mode ObjFPC}{$H+}

interface

uses
  Windows, Forms, Classes, SysUtils, Controls, StdCtrls;

const
  uxtheme_lib = 'uxtheme.dll';
  dwmapi_lib  = 'dwmapi.dll';
  DWMWA_USE_IMMERSIVE_DARK_MODE_BEFORE_20H1 = 19;
  DWMWA_USE_IMMERSIVE_DARK_MODE = 20;

type

  TDwmSetWindowAttribute = function(hwnd: HWND; dwAttribute: DWORD; pvAttribute: Pointer; cbAttribute: DWORD): HRESULT; stdcall;
  TSetWindowTheme = function(hwnd: HWND; pszSubAppName: LPCWSTR; pszSubIdList: LPCWSTR): HRESULT; stdcall;

  { TUWDarkMode }

  TUWDarkMode = class
  private
    hLibUX: HMODULE;
    hLibDWM: TLibHandle;
    DwmSetWindowAttribute: TDwmSetWindowAttribute;
    SetWindowTheme: TSetWindowTheme;
  public
    constructor Create(const AForm: TForm; const ADarkMode: Boolean);
    destructor Destroy; override;
    function Loaded: Boolean;
    function Apply(const AForm: TForm; const AValue: Bool = True): Boolean;
  end;

function IsWindows10OrGreater(const ABuild: Integer = 0): Boolean;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

{ TUWDarkMode }

// -----------------------------------------------------------------------------

constructor TUWDarkMode.Create(const AForm: TForm; const ADarkMode: Boolean);
begin
  DwmSetWindowAttribute := NIL;
  SetWindowTheme  := NIL;

  hLibDWM := LoadLibraryExW(dwmapi_lib, 0, LOAD_LIBRARY_SEARCH_SYSTEM32);
  if hLibDWM <> 0 then Pointer(DwmSetWindowAttribute) := GetProcAddress(hLibDWM, 'DwmSetWindowAttribute');

  hLibUX := LoadLibraryExW(uxtheme_lib, 0, LOAD_LIBRARY_SEARCH_SYSTEM32);
  if hLibUX <> 0 then
    Pointer(SetWindowTheme) := GetProcAddress(hLibUX, 'SetWindowTheme');

  Apply(AForm, ADarkMode);
end;

// -----------------------------------------------------------------------------

destructor TUWDarkMode.Destroy;
begin
  if Pointer(DwmSetWindowAttribute) <> NIL then DwmSetWindowAttribute := NIL;
  if hLibDWM <> 0 then FreeLibrary(hLibDWM);

  if Pointer(SetWindowTheme) <> NIL then SetWindowTheme := NIL;

  if hLibUX <> 0 then FreeLibrary(hLibUX);

  inherited Destroy;
end;

// -----------------------------------------------------------------------------

function TUWDarkMode.Loaded: Boolean;
begin
  Result := (hLibDWM <> 0) and (hLibUX <> 0)
    and Assigned(DwmSetWindowAttribute) and Assigned(SetWindowTheme);
end;

// -----------------------------------------------------------------------------

function TUWDarkMode.Apply(const AForm: TForm; const AValue: Bool = True): Boolean;

  procedure SetWindowTheme_(const AHandle: THandle);
  begin
    if AValue then
      SetWindowTheme(AHandle, 'DarkMode_Explorer', NIL)
    else
      SetWindowTheme(AHandle, 'LightMode_Explorer', NIL);
  end;

var
  attr: DWord;
  C: TComponent;
begin
  Result := False;
  if (AForm = NIL) or not Loaded then Exit;

  if IsWindows10OrGreater(17763) then
  begin
    attr := DWMWA_USE_IMMERSIVE_DARK_MODE_BEFORE_20H1;
    if IsWindows10OrGreater(18985) then
      attr := DWMWA_USE_IMMERSIVE_DARK_MODE;

    DwmSetWindowAttribute(AForm.Handle, attr, @AValue, SizeOf(AValue));
  end;

  SetWindowTheme_(AForm.Handle);

  for C in AForm do
    if (C is TWinControl) then
    begin
      if not AValue then
        SetWindowTheme_(TWinControl(C).Handle)
      else
      begin
        if (C is TComboBox) or (C is TEdit) then
          SetWindowTheme(TWinControl(C).Handle, 'DarkMode_CFD', NIL)
        else
          SetWindowTheme_(TWinControl(C).Handle);
      end;
    end;

  Result := True;
end;

// -----------------------------------------------------------------------------

function IsWindows10OrGreater(const ABuild: Integer = 0): Boolean;
begin
  Result := (Win32MajorVersion >= 10) and (Win32BuildNumber >= ABuild);
end;

// -----------------------------------------------------------------------------
{$ELSE}
interface
implementation
{$ENDIF}

end.

