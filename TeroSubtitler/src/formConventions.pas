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

unit formConventions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  UWLayout, UWCheckBox, procConventions;

type

  { TfrmConventions }

  TfrmConventions = class(TForm)
    btnRemove: TButton;
    btnClose: TButton;
    btnAdd: TButton;
    btnUpdate: TButton;
    cboPauseMode: TComboBox;
    chkDotsOnSplit: TUWCheckBox;
    edtName: TEdit;
    edtProhibitedChars: TEdit;
    edtCPSStrategy: TEdit;
    edtRepeatableChars: TEdit;
    edtRepeatableChars1: TEdit;
    lblCPL: TLabel;
    lblSCSnapArea: TLabel;
    lblSCSnapInCues: TLabel;
    lblSCSnapOutCues: TLabel;
    lblCPS: TLabel;
    lblMaxDurationMs: TLabel;
    lblMaxLineCount: TLabel;
    lblMinDurationMs: TLabel;
    lblName: TLabel;
    lblNewSubtitleMs: TLabel;
    lblProhibitedChars: TLabel;
    lblCPSStrategy: TLabel;
    lblRepeatableChars: TLabel;
    lblChaining: TLabel;
    lblSubtitlePauseMs: TLabel;
    lblWPM: TLabel;
    lstTree: TListBox;
    lyoConvention: TUWLayout;
    spnCPL: TSpinEdit;
    spnSCSnapArea: TSpinEdit;
    spnChaining: TSpinEdit;
    spnSCSnapThreshold: TSpinEdit;
    spnSCSnapInCues: TSpinEdit;
    spnSCSnapOutCues: TSpinEdit;
    spnCPS: TSpinEdit;
    spnMaxDurationMs: TSpinEdit;
    spnMaxLineCount: TSpinEdit;
    spnMinDurationMs: TSpinEdit;
    spnMinDurationPerWord: TSpinEdit;
    spnNewSubtitleMs: TSpinEdit;
    spnSubtitlePauseMs: TSpinEdit;
    spnWPM: TSpinEdit;
    procedure btnAddClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lstTreeSelectionChange(Sender: TObject; User: boolean);
  private

  public
    FList: TProfiles;
  end;

var
  frmConventions: TfrmConventions;

// -----------------------------------------------------------------------------

implementation


uses
  procTypes, RegExpr, procWorkspace, procColorTheme, procCommon,
  UWSystem.XMLLang;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmConventions }

// -----------------------------------------------------------------------------

procedure TfrmConventions.FormCreate(Sender: TObject);
begin
  LoadLanguage(Self);
  FList := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmConventions.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  FList := NIL;
  CloseAction := caFree;
  frmConventions := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmConventions.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmConventions.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmConventions.lstTreeSelectionChange(Sender: TObject;
  User: boolean);
begin
  if FList <> NIL then
    if (FList.Items.Count > 0) and (lstTree.ItemIndex >= 0) then
      with FList.Items[lstTree.ItemIndex]^ do
      begin
        if PauseInFrames then
          cboPauseMode.ItemIndex := 1
        else
          cboPauseMode.ItemIndex := 0;

        edtName.Text                := Name;
        edtRepeatableChars.Text     := RepeatableChars;
        edtProhibitedChars.Text     := ProhibitedChars;
        edtCPSStrategy.Text         := CPSLineLenStrategy;
        spnNewSubtitleMs.Value      := NewSubtitleMs;
        spnMinDurationMs.Value      := MinDuration;
        spnMinDurationPerWord.Value := MinDurationPerWord;
        spnMaxDurationMs.Value      := MaxDuration;
        spnMaxLineCount.Value       := MaxLines;
        spnSubtitlePauseMs.Value    := MinPause;
        spnCPS.Value                := MaxCPS;
        spnWPM.Value                := WPM;
        spnCPL.Value                := CPL;
        spnSCSnapArea.Value         := ShotcutSnapArea;
        spnSCSnapThreshold.Value    := ShotcutThreshold;
        spnSCSnapInCues.Value       := ShotcutInCues;
        spnSCSnapOutCues.Value      := ShotcutOutCues;
        spnChaining.Value           := Chaining;
        chkDotsOnSplit.Checked      := DotsOnSplit;
      end;
end;

// -----------------------------------------------------------------------------

procedure TfrmConventions.btnAddClick(Sender: TObject);
var
  i: Integer;
begin
  if FList <> NIL then
  begin
    if edtName.Text <> '' then
    begin
      i := FList.AddItem(edtName.Text, spnNewSubtitleMs.Value, spnMinDurationMs.Value,
        spnMinDurationPerWord.Value, spnMaxDurationMs.Value,
        spnMaxLineCount.Value, spnSubtitlePauseMs.Value,
        spnCPS.Value, spnWPM.Value, spnCPL.Value,
        spnSCSnapArea.Value, spnSCSnapThreshold.Value, spnSCSnapInCues.Value, spnSCSnapOutCues.Value,
        spnChaining.Value,
        cboPauseMode.ItemIndex = 1,
        edtRepeatableChars.Text, edtProhibitedChars.Text,
        chkDotsOnSplit.Checked, edtCPSStrategy.Text);

      if i >= 0 then
      begin
        lstTree.Items.Insert(i, edtName.Text);
        lstTree.ItemIndex := i;
      end;
    end
    else
      edtName.SetFocus;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmConventions.btnRemoveClick(Sender: TObject);
begin
  if FList <> NIL then
    if (lstTree.Count > 0) and (lstTree.ItemIndex >= 0) then
    begin
      FList.Items.Delete(lstTree.ItemIndex);
      lstTree.DeleteSelected;

      if lstTree.Count > 0 then
        lstTree.ItemIndex := 0;
    end;
end;

// -----------------------------------------------------------------------------

procedure TfrmConventions.btnUpdateClick(Sender: TObject);
begin
  if (edtName.Text <> '') and (lstTree.ItemIndex >= 0) then

  if FList <> NIL then
    FList.UpdateItem(lstTree.ItemIndex, edtName.Text, spnNewSubtitleMs.Value,
      spnMinDurationMs.Value, spnMinDurationPerWord.Value, spnMaxDurationMs.Value,
      spnMaxLineCount.Value, spnSubtitlePauseMs.Value,
      spnCPS.Value, spnWPM.Value, spnCPL.Value,
      spnSCSnapArea.Value, spnSCSnapThreshold.Value, spnSCSnapInCues.Value, spnSCSnapOutCues.Value,
      spnChaining.Value,
      cboPauseMode.ItemIndex = 1, edtRepeatableChars.Text,
      edtProhibitedChars.Text, chkDotsOnSplit.Checked, edtCPSStrategy.Text);
end;

// -----------------------------------------------------------------------------

end.

