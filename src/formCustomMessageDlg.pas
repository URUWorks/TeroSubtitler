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

unit formCustomMessageDlg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  LCLTranslator;

type

  { TfrmCustomMessageDlg }

  TIconMode = (imQuestion, imInformation);

  TfrmCustomMessageDlg = class(TForm)
    btnOk: TButton;
    ImageDlg: TImage;
    ImageList: TImageList;
    lblCustomAction: TLabel;
    lblMessage: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    function Execute(const AMessage: String; const ACaption: String = ''; const AIcon: TIconMode = imQuestion; const ABold: Boolean = True; const ACenter: Boolean = True; const ACustomAction: String = ''; const ACustomActionClick: TNotifyEvent = NIL): Integer;
  end;

var
  frmCustomMessageDlg: TfrmCustomMessageDlg;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, procWorkspace, procColorTheme, UWSystem.StrUtils;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmCustomMessageDlg }

// -----------------------------------------------------------------------------

procedure TfrmCustomMessageDlg.FormCreate(Sender: TObject);
begin
  lblMessage.Caption := '';
  Constraints.MinHeight := Height;
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomMessageDlg.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

function TfrmCustomMessageDlg.Execute(const AMessage: String; const ACaption: String = ''; const AIcon: TIconMode = imQuestion; const ABold: Boolean = True; const ACenter: Boolean = True; const ACustomAction: String = ''; const ACustomActionClick: TNotifyEvent = NIL): Integer;
var
  c, h: Integer;
begin
  if ColorThemeInstance.GetRealColorMode = cmDark then
  begin
    if AIcon = imQuestion then
      ImageList.GetBitmap(1, ImageDlg.Picture.Bitmap)
    else
      ImageList.GetBitmap(3, ImageDlg.Picture.Bitmap);
  end
  else
  begin
    if AIcon = imQuestion then
      ImageList.GetBitmap(0, ImageDlg.Picture.Bitmap)
    else
      ImageList.GetBitmap(2, ImageDlg.Picture.Bitmap);
  end;

  if ACaption = '' then
    Caption := ProgramName
  else
    Caption := ACaption;

  if not ABold then
    lblMessage.Font.Style := [];

  if not ACenter then
    lblMessage.Alignment := taLeftJustify;

  lblMessage.Caption := AMessage;
  lblCustomAction.Caption := ACustomAction;
  lblCustomAction.OnClick := ACustomActionClick;
  lblCustomAction.Visible := not ACustomAction.IsEmpty and (ACustomActionClick <> NIL);

  c := LineCount(AMessage, LineEnding);
  h := lblCustomAction.Canvas.TextHeight('Aj');
  Height := (c*h) + ((Height - lblMessage.Height) + {$IFDEF WINDOWS}h{$ELSE}(h*2){$ENDIF});

  Result := ShowModal;
end;

// -----------------------------------------------------------------------------

end.

