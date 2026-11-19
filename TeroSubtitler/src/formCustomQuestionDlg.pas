{*
 *  URUWorks Tero Subtitler
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
 *  Copyright (C) 2023-2026 URUWorks, uruworks@gmail.com.
 *}

unit formCustomQuestionDlg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TfrmCustomQuestionDlg }

  TCustomDlgBtn     = (dbYes, dbNo, dbCancel);
  TCustomDlgButtons = set of TCustomDlgBtn;

  TfrmCustomQuestionDlg = class(TForm)
    btnCancel: TButton;
    btnYes: TButton;
    btnNo: TButton;
    ImageDlg: TImage;
    ImageList: TImageList;
    lblTitle: TLabel;
    lblQuestion: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    function Execute(const ATitle, AQuestion: String; const ACaption: String = ''; AButtons: TCustomDlgButtons = []): Integer;
  end;

function ShowQuestionDialog(const ATitle, AQuestion: String; const ACaption: String = ''; AButtons: TCustomDlgButtons = []): Integer;

var
  frmCustomQuestionDlg: TfrmCustomQuestionDlg;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, procWorkspace, procColorTheme, LCLTranslator, Math,
  formCustomMessageDlg;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmCustomQuestionDlg }

// -----------------------------------------------------------------------------

procedure TfrmCustomQuestionDlg.FormCreate(Sender: TObject);
begin
  lblTitle.Caption    := '';
  lblQuestion.Caption := '';
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomQuestionDlg.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
  if ColorThemeInstance.GetRealColorMode = cmDark then
    ImageList.GetBitmap(1, ImageDlg.Picture.Bitmap)
  else
    ImageList.GetBitmap(0, ImageDlg.Picture.Bitmap);
end;

// -----------------------------------------------------------------------------

function TfrmCustomQuestionDlg.Execute(const ATitle, AQuestion: String; const ACaption: String = ''; AButtons: TCustomDlgButtons = []): Integer;
var
  ALeft, h, d: Integer;

  procedure SetButton(AButton: TButton; const AID: TCustomDlgBtn);
  begin
    if (AID in AButtons) then
    begin
      AButton.Left    := ALeft;
      AButton.Visible := True;
      ALeft := ALeft - AButton.Width - 5;
    end
    else
      btnCancel.Hide;
  end;

begin
  Caption             := ACaption;
  lblTitle.Caption    := ATitle;
  lblQuestion.Caption := AQuestion;

  if AButtons = [] then
    AButtons := [dbYes, dbNo, dbCancel];

  ALeft := 331;
  SetButton(btnCancel, dbCancel);
  SetButton(btnNo, dbNo);
  SetButton(btnYes, dbYes);

  btnCancel.Cancel := btnCancel.Visible;
  btnNo.Cancel := not btnCancel.Visible;

  // taller if the wrapped texts don't fit (they were cut at the bottom)
  h := GetWrappedTextHeight(lblTitle.Font, ATitle, lblTitle.Width);
  if h > lblTitle.Height then
  begin
    d := h - lblTitle.Height;
    lblTitle.Height := h;
    lblQuestion.Top := lblQuestion.Top + d;
    btnYes.Top      := btnYes.Top + d;
    btnNo.Top       := btnNo.Top + d;
    btnCancel.Top   := btnCancel.Top + d;
    ClientHeight    := ClientHeight + d;
  end;
  h := GetWrappedTextHeight(lblQuestion.Font, AQuestion, lblQuestion.Width);
  if h > lblQuestion.Height then
  begin
    d := Min(h - lblQuestion.Height, Max(0, Screen.WorkAreaHeight - 100 - Height));
    lblQuestion.Height := lblQuestion.Height + d;
    btnYes.Top      := btnYes.Top + d;
    btnNo.Top       := btnNo.Top + d;
    btnCancel.Top   := btnCancel.Top + d;
    ClientHeight    := ClientHeight + d;
  end;

  Result := ShowModal;
end;

// -----------------------------------------------------------------------------

function ShowQuestionDialog(const ATitle, AQuestion: String; const ACaption: String = ''; AButtons: TCustomDlgButtons = []): Integer;
begin
  with TfrmCustomQuestionDlg.Create(NIL) do
  try
    Result := Execute(ATitle, AQuestion, ACaption, AButtons);
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

end.

