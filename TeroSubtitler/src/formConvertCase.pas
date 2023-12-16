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

unit formConvertCase;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  UWRadioButton, UWLayout, LCLTranslator;

type

  { TfrmConvertCase }

  TfrmConvertCase = class(TForm)
    btnApply: TButton;
    btnClose: TButton;
    lblScope: TLabel;
    rbnAllTheSubtitles: TUWRadioButton;
    rbnFromTheSelectedSubtitle: TUWRadioButton;
    rbnOnlySelectedSubtitles: TUWRadioButton;
    rdoInverseType: TUWRadioButton;
    rdoLowercase: TUWRadioButton;
    rdoSentenceType: TUWRadioButton;
    rdoTitleType: TUWRadioButton;
    rdoUppercase: TUWRadioButton;
    lyoScope: TUWLayout;
    procedure btnApplyClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  frmConvertCase: TfrmConvertCase;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, procVST, procVST_Loops, UWSystem.StrUtils,
  UWSubtitleAPI, formMain, LazUTF8, procWorkspace;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmConvertCase }

// -----------------------------------------------------------------------------

procedure TfrmConvertCase.FormCreate(Sender: TObject);
begin
  if (frmMain.VST.SelectedCount = 1) then
    rbnFromTheSelectedSubtitle.Checked := True
  else if (frmMain.VST.SelectedCount > 1) then
    rbnOnlySelectedSubtitles.Checked := True
  else
    rbnAllTheSubtitles.Checked := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmConvertCase.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  frmConvertCase := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmConvertCase.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmConvertCase.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure ApplyConvertCase(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with frmConvertCase, Item^ do
  begin
    if rdoSentenceType.Checked then
      ApplyText(Item, Index, SentenceCase(Text), SentenceCase(Translation), False)
    else if rdoLowercase.Checked then
      ApplyText(Item, Index, UTF8LowerCase(Text), UTF8LowerCase(Translation), False)
    else if rdoUppercase.Checked then
      ApplyText(Item, Index, UTF8UpperCase(Text), UTF8UpperCase(Translation), False)
    else if rdoTitleType.Checked then
      ApplyText(Item, Index, TitleCase(Text), TitleCase(Translation), False)
    else
      ApplyText(Item, Index, InvertCase(Text), InvertCase(Translation), False);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmConvertCase.btnApplyClick(Sender: TObject);
var
  SelLoop: TVSTDoLoopSelection;
begin
  if rbnAllTheSubtitles.Checked then
    SelLoop := dlAll
  else if rbnFromTheSelectedSubtitle.Checked then
    SelLoop := dlCurrentToLast
  else
    SelLoop := dlSelected;

  VSTDoLoop(frmMain.VST, @ApplyConvertCase, SelLoop, True, True);
  Close;
end;

// -----------------------------------------------------------------------------

end.

