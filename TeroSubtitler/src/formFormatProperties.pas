{*
 *  URUWorks
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

unit formFormatProperties;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, UWLayout,
  UWCheckBox;

type

  { TfrmFormatProperties }

  TfrmFormatProperties = class(TForm)
    btnApply: TButton;
    btnClose: TButton;
    cboFormats: TComboBox;
    lyoWebVTT: TUWLayout;
    chkVTTWriteCueIdentifiers: TUWCheckBox;
    procedure btnApplyClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure FillComboWithFormatProperties;
  public

  end;

var
  frmFormatProperties: TfrmFormatProperties;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, LazUTF8, procWorkspace, procCommon, UWSubtitleAPI.Formats, XMLConf;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmFormatProperties }

// -----------------------------------------------------------------------------

procedure TfrmFormatProperties.FormCreate(Sender: TObject);
begin
  LoadLanguage(Self);
  FillComboWithFormatProperties;

  with TXMLConfig.Create(NIL) do
  try
    FileName := SettingsFileName;
    OpenKey('FormatProperties');
    //
    OpenKey(FormatToName(sfWebVTT));
    chkVTTWriteCueIdentifiers.Checked := GetValue('WriteCueIdentifiers', False);
    CloseKey;
    //
    CloseKey;
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmFormatProperties.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  frmFormatProperties := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmFormatProperties.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmFormatProperties.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmFormatProperties.btnApplyClick(Sender: TObject);
begin
  with TXMLConfig.Create(NIL) do
  try
    FileName := SettingsFileName;
    RootName := RootCfg;
    OpenKey('FormatProperties');
    //
    OpenKey(FormatToName(sfWebVTT));
    SetValue('WriteCueIdentifiers', chkVTTWriteCueIdentifiers.Checked);
    CloseKey;
    //
    CloseKey;
  finally
    Free;
  end;
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmFormatProperties.FillComboWithFormatProperties;
begin
  with cboFormats.Items do
  begin
    BeginUpdate;
    Clear;
    Add(FormatToName(sfWebVTT));
    EndUpdate;
  end;
  cboFormats.ItemIndex := 0;
end;

// -----------------------------------------------------------------------------

end.

