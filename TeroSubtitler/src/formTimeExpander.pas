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

unit formTimeExpander;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  UWCheckBox, UWRadioButton;

type

  { TfrmTimeExpander }

  TfrmTimeExpander = class(TForm)
    btnApply: TButton;
    btnClose: TButton;
    cboMode: TComboBox;
    chkDuration: TUWCheckBox;
    chkSubtitle: TUWCheckBox;
    edtDurationValue: TEdit;
    edtSubtitleValue: TEdit;
    edtTime: TEdit;
    lblMs1: TLabel;
    lblChars: TLabel;
    lblMs: TLabel;
    lblScope: TLabel;
    rbnAllTheSubtitles: TUWRadioButton;
    rbnFromTheSelectedSubtitle: TUWRadioButton;
    rbnOnlySelectedSubtitles: TUWRadioButton;
    procedure btnApplyClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure edtTimeChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  frmTimeExpander: TfrmTimeExpander;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, procVST, procSubtitle, UWSubtitleAPI, UWSubtitles.Utils,
  formMain, UWSystem.SysUtils, procWorkspace, procCommon, UWSystem.XMLLang;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmTimeExpander }

// -----------------------------------------------------------------------------

procedure TfrmTimeExpander.FormCreate(Sender: TObject);
var
  FAppStringList: TAppStringList = NIL;
begin
  LoadLanguage(Self);
  lblMs1.Caption := lblMs.Caption;

  LanguageManager.GetAppStringList('TimeExpanderStrings', FAppStringList);
  with cboMode.Items do
  begin
    Add(GetString(FAppStringList, 'Expand'));
    Add(GetString(FAppStringList, 'Reduce'));
  end;
  FAppStringList.Free;
  cboMode.ItemIndex := 0;

  edtTime.Text          := AppOptions.ExpandMs.ToString;
  edtSubtitleValue.Text := AppOptions.ExpandChar.ToString;
  edtDurationValue.Text := AppOptions.ExpandLen.ToString;

  if (frmMain.VST.SelectedCount = 1) then
    rbnFromTheSelectedSubtitle.Checked := True
  else if (frmMain.VST.SelectedCount > 1) then
    rbnOnlySelectedSubtitles.Checked := True
  else
    rbnAllTheSubtitles.Checked := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmTimeExpander.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  AppOptions.ExpandMs := StrToIntDef(edtTime.Text, 0);
  AppOptions.ExpandChar := StrToIntDef(edtSubtitleValue.Text, 0);
  AppOptions.ExpandLen := StrToIntDef(edtDurationValue.Text, 0);

  CloseAction := caFree;
  frmTimeExpander := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmTimeExpander.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmTimeExpander.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmTimeExpander.edtTimeChange(Sender: TObject);
begin
  with Sender as TEdit do
    btnApply.Enabled := Text <> '';
end;

// -----------------------------------------------------------------------------

procedure ApplyTimeExpander(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with Item^, frmTimeExpander do
    SetSubtitleTime(Index, InitialTime + TimeExpander(Text, Subtitles.Duration[Index], StrToInt(edtTime.Text), iff(chkSubtitle.Checked, StrToInt(edtSubtitleValue.Text), 0), iff(chkDuration.Checked, StrToInt(edtDurationValue.Text), 0), cboMode.ItemIndex = 0), 1, False, False);
end;

// -----------------------------------------------------------------------------

procedure TfrmTimeExpander.btnApplyClick(Sender: TObject);
var
  SelLoop: TVSTDoLoopSelection;
begin
  if (edtSubtitleValue.Text <> '') and (edtDurationValue.Text <> '') then
  begin
    if rbnAllTheSubtitles.Checked then
      SelLoop := dlAll
    else if rbnFromTheSelectedSubtitle.Checked then
      SelLoop := dlCurrentToLast
    else
      SelLoop := dlSelected;

    VSTDoLoop(frmMain.VST, @ApplyTimeExpander, SelLoop, True, True);
    Close;
  end
  else
  begin
    if (edtSubtitleValue.Text = '') then
      edtSubtitleValue.SetFocus
    else if (edtDurationValue.Text = '') then
      edtDurationValue.SetFocus;
  end;
end;

// -----------------------------------------------------------------------------

end.

