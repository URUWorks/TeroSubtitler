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
 *  Copyright (C) 2023-2024 URUWorks, uruworks@gmail.com.
 *}

unit formCustomSelectDlgWithPreview;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, LCLType, LCLIntf, LCLTranslator;

type

  { TfrmCustomSelectDlgWithPreview }

  TfrmCustomSelectDlgWithPreview = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    lblMessage: TLabel;
    lstItems: TListBox;
    mmoPreview: TMemo;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lstItemsSelectionChange(Sender: TObject; User: boolean);
  private
    PrevBuffer: TBytes;
    procedure DoPreview(AEncoding: TEncoding);
  public
    procedure ReadFileBuffer(const AFileName: String);
  end;

function ExecuteDialog(const ACaption, APrompt: String; const AItems: TStrings; const AFileName: String = ''; const ADefault: Integer = 0; const AOnlyOkButton: Boolean = False): Integer;

var
  frmCustomSelectDlgWithPreview: TfrmCustomSelectDlgWithPreview;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, procWorkspace, UWSystem.Encoding;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmCustomSelectDlgWithPreview }

// -----------------------------------------------------------------------------

procedure TfrmCustomSelectDlgWithPreview.FormCreate(Sender: TObject);
begin
  lblMessage.Caption := '';
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomSelectDlgWithPreview.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if Length(PrevBuffer) > 0 then
    SetLength(PrevBuffer, 0);
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomSelectDlgWithPreview.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomSelectDlgWithPreview.lstItemsSelectionChange(
  Sender: TObject; User: boolean);
begin
  if (lstItems.Count > 0) and (lstItems.ItemIndex >= 0) then
    DoPreview(TEncoding.GetEncoding(Encodings[lstItems.ItemIndex].CPID));
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomSelectDlgWithPreview.ReadFileBuffer(const AFileName: String);
var
  Stream : TStream;
  Size   : Integer;
begin
  if AFileName.IsEmpty or not FileExists(AFileName) then Exit;

  Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    Size := Stream.Size - Stream.Position;
    SetLength(PrevBuffer, Size);
    Stream.Read(PrevBuffer[0], Size);
  finally
    Stream.Free;
  end;

  lstItemsSelectionChange(NIL, False);
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomSelectDlgWithPreview.DoPreview(AEncoding: TEncoding);
var
  Size : Integer;
begin
  if Length(PrevBuffer) > 0 then
  begin
    Size := TEncoding.GetBufferEncoding(PrevBuffer, AEncoding, TEncoding.GetEncoding(1252));
    mmoPreview.Text := AEncoding.GetString(PrevBuffer, Size, Length(PrevBuffer) - Size);
  end;
end;

// -----------------------------------------------------------------------------

function ExecuteDialog(const ACaption, APrompt: String; const AItems: TStrings; const AFileName: String = ''; const ADefault: Integer = 0; const AOnlyOkButton: Boolean = False): Integer;
begin
  with TfrmCustomSelectDlgWithPreview.Create(NIL) do
  try
    Caption := ACaption;
    lblMessage.Caption := APrompt;
    lstItems.Items.Assign(AItems);

    if lstItems.Items.Count > 0 then
    begin
      if ADefault >= 0 then
        lstItems.ItemIndex := ADefault
      else
        lstItems.ItemIndex := 0;
    end;

    if AOnlyOkButton then
    begin
      btnCancel.Hide;
      btnOk.Left := btnCancel.Left
    end;

    ReadFileBuffer(AFileName);

    if ShowModal = mrOK then
      Result := lstItems.ItemIndex
    else
      Result := ADefault;
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

end.

