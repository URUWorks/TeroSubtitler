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

unit formFindAndReplace;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  LCLType, UWCheckBox, UWRadioButton, LCLTranslator, procTypes;

type

  { TfrmFindAndReplace }

  TfrmFindAndReplace = class(TForm)
    btnClose: TButton;
    btnFind: TButton;
    btnReplace: TButton;
    btnFindNext: TButton;
    btnReplaceAll: TButton;
    btnFindPrev: TButton;
    chkCaseSensitive: TUWCheckBox;
    chkPreserveCaseOnReplace: TUWCheckBox;
    chkRegularExpression: TUWCheckBox;
    chkWholeWord: TUWCheckBox;
    chkReplaceWith: TUWCheckBox;
    lblOptions: TLabel;
    lblScope: TLabel;
    lblSource: TLabel;
    lblTextToFind: TLabel;
    mmoTextToFind: TMemo;
    mmoReplaceWith: TMemo;
    pnlLocation: TPanel;
    pnlScope: TPanel;
    rbnText: TRadioButton;
    rbnAllTheSubtitles: TUWRadioButton;
    rbnFromTheSelectedSubtitle: TUWRadioButton;
    rbnTranslation: TRadioButton;
    rbnSearchBoth: TRadioButton;
    procedure btnCloseClick(Sender: TObject);
    procedure btnFindClick(Sender: TObject);
    procedure btnFindNextClick(Sender: TObject);
    procedure btnFindPrevClick(Sender: TObject);
    procedure btnReplaceAllClick(Sender: TObject);
    procedure btnReplaceClick(Sender: TObject);
    procedure chkReplaceWithChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mmoReplaceWithKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure mmoTextToFindKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private

  public
    function FindSource: TFindSource;
  end;

var
  frmFindAndReplace: TfrmFindAndReplace;

// -----------------------------------------------------------------------------

implementation

uses procVST, procWorkspace, procConfig, formMain;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ Helpers }

// -----------------------------------------------------------------------------

function FindModeFromBoolean(const AFromBegining: Boolean): TFindMode;
begin
  if AFromBegining then
    Result := fmBegin
  else
    Result := fmCurrent;
end;

// -----------------------------------------------------------------------------

{ TfrmFindAndReplace }

// -----------------------------------------------------------------------------

procedure TfrmFindAndReplace.FormCreate(Sender: TObject);
begin
  chkReplaceWithChange(Sender);

  if (frmMain.VST.SelectedCount = 1) then
    rbnFromTheSelectedSubtitle.Checked := True
  else
    rbnAllTheSubtitles.Checked := True;

  rbnSearchBoth.Checked := True;

  {$IFNDEF WINDOWS}
  PrepareCustomControls(Self);
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

procedure TfrmFindAndReplace.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  SaveFormSettings(Self, Format('%d,%d,%d,%d', [chkCaseSensitive.Checked.ToInteger, chkPreserveCaseOnReplace.Checked.ToInteger, chkWholeWord.Checked.ToInteger, chkRegularExpression.Checked.ToInteger]));

  CloseAction := caFree;
  frmFindAndReplace := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmFindAndReplace.FormShow(Sender: TObject);
var
  s: String;
  AParamArray: TStringArray;
begin
  CheckColorTheme(Self);
  Height := 440;

  //I did not understand why these do not get their values from the IDE
  pnlLocation.Color := Color;
  pnlScope.Color := Color;
  pnlLocation.BevelColor := Color;
  pnlScope.BevelColor := Color;

  s := LoadFormSettings(Self);
  if not s.IsEmpty then
  begin
    AParamArray := s.Split(',');
    if Length(AParamArray) = 4 then
    begin
      chkCaseSensitive.Checked := AParamArray[0].ToBoolean;
      chkPreserveCaseOnReplace.Checked := AParamArray[1].ToBoolean;
      chkWholeWord.Checked := AParamArray[2].ToBoolean;
      chkRegularExpression.Checked := AParamArray[3].ToBoolean;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmFindAndReplace.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmFindAndReplace.mmoTextToFindKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (Shift = []) then
  begin
    Key := 0;
    btnFind.Click;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmFindAndReplace.mmoReplaceWithKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (Shift = []) then
  begin
    Key := 0;
    btnReplace.Click;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmFindAndReplace.chkReplaceWithChange(Sender: TObject);
begin
  with chkReplaceWith do
  begin
    mmoReplaceWith.Enabled := Checked;

    btnReplaceAll.Top      := btnClose.Top;  //NOTE: btnReplaceAll.Top := does NOT work properly in GTK if set any other place
    btnReplace.Top         := btnClose.Top;
    btnReplaceAll.Visible  := Checked;
    btnReplace.Visible     := Checked;
    btnFindNext.Visible    := not Checked;
    btnFind.Visible        := not Checked;
    btnFindPrev.Visible    := not Checked;
  end;
end;

// -----------------------------------------------------------------------------

function TfrmFindAndReplace.FindSource: TFindSource;
begin
  Result := fsTextAndTranslation;
  if frmFindAndReplace.rbnText.Checked then exit (fsText);
  if frmFindAndReplace.rbnTranslation.Checked then exit (fsTranslation);
end;

procedure TfrmFindAndReplace.btnFindClick(Sender: TObject);
begin
  VSTFind(mmoTextToFind.Text, chkCaseSensitive.Checked, fmBegin, FindSource, False, '',
    False, False, chkWholeWord.Checked, chkRegularExpression.Checked);
  //Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmFindAndReplace.btnFindNextClick(Sender: TObject);
begin
  VSTFindNext(chkCaseSensitive.Checked, chkWholeWord.Checked, chkRegularExpression.Checked);
  //Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmFindAndReplace.btnFindPrevClick(Sender: TObject);
begin
  VSTFindPrevious(chkCaseSensitive.Checked, chkWholeWord.Checked, chkRegularExpression.Checked);
  //Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmFindAndReplace.btnReplaceAllClick(Sender: TObject);
begin
  VSTFind(mmoTextToFind.Text, chkCaseSensitive.Checked, FindModeFromBoolean(rbnAllTheSubtitles.Checked), FindSource,
    True, mmoReplaceWith.Text, True, chkPreserveCaseOnReplace.Checked, chkWholeWord.Checked, chkRegularExpression.Checked);

  //Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmFindAndReplace.btnReplaceClick(Sender: TObject);
begin
  VSTFind(mmoTextToFind.Text, chkCaseSensitive.Checked, FindModeFromBoolean(rbnAllTheSubtitles.Checked), FindSource,
    True, mmoReplaceWith.Text, False, chkPreserveCaseOnReplace.Checked, chkRegularExpression.Checked);

  //Close;
end;

// -----------------------------------------------------------------------------

end.

