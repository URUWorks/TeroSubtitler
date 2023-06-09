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

unit formTranslate;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, UWRadioButton;

type

  { TfrmTranslate }

  TfrmTranslate = class(TForm)
    btnTranslate: TButton;
    btnClose: TButton;
    cboInput: TComboBox;
    cboOutput: TComboBox;
    cboSourceLanguage: TComboBox;
    cboTranslationLanguage: TComboBox;
    lblInput: TLabel;
    lblOutput: TLabel;
    lblScope: TLabel;
    lblSourceLanguage: TLabel;
    lblTranslationLanguage: TLabel;
    prbTranslate: TProgressBar;
    rbnAllTheSubtitles: TUWRadioButton;
    rbnFromTheSelectedSubtitle: TUWRadioButton;
    rbnOnlySelectedSubtitles: TUWRadioButton;
    procedure btnTranslateClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure EnableControls(const AValue: Boolean);
  public

  end;

var
  frmTranslate: TfrmTranslate;
  CancelTranslation: Boolean;

// -----------------------------------------------------------------------------

implementation

uses
  procWorkspace, procTypes, procVST, procCommon, UWSystem.InetUtils,
  UWTranslateAPI.Google, UWSubtitleAPI, formMain;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmTranslate }

// -----------------------------------------------------------------------------

procedure TfrmTranslate.FormCreate(Sender: TObject);
begin
  LoadLanguage(Self);

  FillComboWithGoogleLanguages(cboSourceLanguage);
  FillComboWithGoogleLanguages(cboTranslationLanguage, 44);
  cboTranslationLanguage.Items.Delete(0);

  cboSourceLanguage.Items[0] := GetCommonString('Detect');
  cboInput.Items.Add(GetLangString('Text'));
  cboInput.Items.Add(GetLangString('Translation'));

  cboOutput.Items.Assign(cboInput.Items);
  cboInput.ItemIndex  := 0;
  cboOutput.ItemIndex := 1;

  CancelTranslation := False;

  if (frmMain.VST.SelectedCount = 1) then
    rbnFromTheSelectedSubtitle.Checked := True
  else if (frmMain.VST.SelectedCount > 1) then
    rbnOnlySelectedSubtitles.Checked := True
  else
    rbnAllTheSubtitles.Checked := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmTranslate.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  CloseAction := caFree;
  frmTranslate := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmTranslate.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmTranslate.btnCloseClick(Sender: TObject);
begin
  if btnTranslate.Enabled then
    Close
  else
    CancelTranslation := True;
end;

// -----------------------------------------------------------------------------

procedure ApplyGoogleTranslate(const Item: PUWSubtitleItem; const Index: Integer);
var
  s1, s2: String;
begin
  with frmTranslate, Item^ do
  begin
    case cboInput.ItemIndex of
      0: s1 := Text;
      1: s1 := Translation;
    end;

    s2 := GoogleTranslateText(s1, GoogleTranslateLocale[cboSourceLanguage.ItemIndex],
      GoogleTranslateLocale[cboTranslationLanguage.ItemIndex+1]);

    case cboOutput.ItemIndex of
      0: Text        := s2;
      1: Translation := s2;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure UpdateTranslateState(const CurrentItem, TotalCount: Integer; var Cancel: Boolean);
begin
  with frmTranslate.prbTranslate do
  begin
    Max      := TotalCount;
    Position := CurrentItem;
  end;
  Cancel := CancelTranslation;
end;

// -----------------------------------------------------------------------------

procedure TfrmTranslate.btnTranslateClick(Sender: TObject);
var
  SelLoop: TVSTDoLoopSelection;
begin
  if rbnAllTheSubtitles.Checked then
    SelLoop := dlAll
  else if rbnFromTheSelectedSubtitle.Checked then
    SelLoop := dlCurrentToLast
  else
    SelLoop := dlSelected;

  if IsInternetAlive then
  begin
    CancelTranslation := False;
    EnableControls(False);
    VSTDoLoop(frmMain.VST, @ApplyGoogleTranslate, SelLoop, True, True, @UpdateTranslateState);
    EnableControls(True);
  end
  else
    ShowErrorMessageDialog(GetCommonString('NoInternetConnection'));

  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmTranslate.EnableControls(const AValue: Boolean);
begin
  if not AValue then
    btnClose.Caption := GetCommonString('btnCancel', 'CommonControls')
  else
    btnClose.Caption := GetCommonString('btnClose', 'CommonControls');

  cboSourceLanguage.Enabled          := AValue;
  cboTranslationLanguage.Enabled     := AValue;
  cboInput.Enabled                   := AValue;
  cboOutput.Enabled                  := AValue;
  rbnAllTheSubtitles.Enabled         := AValue;
  rbnFromTheSelectedSubtitle.Enabled := AValue;
  rbnOnlySelectedSubtitles.Enabled   := AValue;
  btnTranslate.Enabled               := AValue;
end;

// -----------------------------------------------------------------------------

end.

