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

unit formAutomaticDurations;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  UWRadioButton, LCLTranslator, procLocalize;

type

  { TfrmAutomaticDurations }

  TfrmAutomaticDurations = class(TForm)
    btnApply: TButton;
    btnClose: TButton;
    cboCondition: TComboBox;
    edtChars: TEdit;
    edtLine: TEdit;
    edtWord: TEdit;
    lblChars: TLabel;
    lblCondition: TLabel;
    lblLine: TLabel;
    lblScope: TLabel;
    lblWord: TLabel;
    rbnAllTheSubtitles: TUWRadioButton;
    rbnFromTheSelectedSubtitle: TUWRadioButton;
    rbnOnlySelectedSubtitles: TUWRadioButton;
    procedure btnApplyClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  frmAutomaticDurations: TfrmAutomaticDurations;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, procVST, procSubtitle, UWSubtitleAPI, UWSubtitles.Utils,
  formMain, procWorkspace;

// -----------------------------------------------------------------------------

{$R *.lfm}

{ TfrmAutomaticDurations }

// -----------------------------------------------------------------------------

procedure TfrmAutomaticDurations.FormCreate(Sender: TObject);
begin
  with cboCondition.Items do
  begin
    Add(lngNewDurationAll);
    Add(lngNewDurationGreater);
    Add(lngNewDurationSmaller);
  end;
  cboCondition.ItemIndex := 0;

  edtChars.Text := AppOptions.AutoLengthChar.ToString;
  edtWord.Text := AppOptions.AutoLengthWord.ToString;
  edtLine.Text := AppOptions.AutoLengthLine.ToString;

  if (frmMain.VST.SelectedCount = 1) then
    rbnFromTheSelectedSubtitle.Checked := True
  else if (frmMain.VST.SelectedCount > 1) then
    rbnOnlySelectedSubtitles.Checked := True
  else
    rbnAllTheSubtitles.Checked := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmAutomaticDurations.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  AppOptions.AutoLengthChar := StrToIntDef(edtChars.Text, 1);
  AppOptions.AutoLengthWord := StrToIntDef(edtWord.Text, 1);
  AppOptions.AutoLengthLine := StrToIntDef(edtLine.Text, 1);

  CloseAction := caFree;
  frmAutomaticDurations := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmAutomaticDurations.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmAutomaticDurations.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure ApplyAutomaticDurations(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with Item^, frmAutomaticDurations do
    SetSubtitleTime(Index, InitialTime + AutomaticDurations(Text, Subtitles.Duration[Index], StrToInt(edtChars.Text), StrToInt(edtWord.Text), StrToInt(edtLine.Text), TAutomaticDurationMode(cboCondition.ItemIndex)), TAG_CONTROL_FINALTIME, False, False);
end;

// -----------------------------------------------------------------------------

procedure TfrmAutomaticDurations.btnApplyClick(Sender: TObject);
var
  SelLoop: TVSTDoLoopSelection;
begin
  if (edtChars.Text <> '') and (edtWord.Text <> '') and (edtLine.Text <> '') then
  begin
     if rbnAllTheSubtitles.Checked then
       SelLoop := dlAll
     else if rbnFromTheSelectedSubtitle.Checked then
       SelLoop := dlCurrentToLast
     else
       SelLoop := dlSelected;

    VSTDoLoop(frmMain.VST, @ApplyAutomaticDurations, SelLoop, True, True);
    Close;
  end
  else
  begin
    if (edtChars.Text = '') then
      edtChars.SetFocus
    else if (edtWord.Text = '') then
      edtWord.SetFocus
    else if (edtLine.Text = '') then
      edtLine.SetFocus;
  end;
end;

// -----------------------------------------------------------------------------

end.

