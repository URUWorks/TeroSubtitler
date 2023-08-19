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
  LCLType, UWCheckBox, UWRadioButton;

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
    lblTextToFind: TLabel;
    mmoTextToFind: TMemo;
    mmoReplaceWith: TMemo;
    rbnAllTheSubtitles: TUWRadioButton;
    rbnFromTheSelectedSubtitle: TUWRadioButton;
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
    procedure mmoReplaceWithKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure mmoTextToFindKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private

  public

  end;

var
  frmFindAndReplace: TfrmFindAndReplace;

// -----------------------------------------------------------------------------

implementation

uses procVST, procWorkspace, procConfig, formMain;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmFindAndReplace }

// -----------------------------------------------------------------------------

procedure TfrmFindAndReplace.FormCreate(Sender: TObject);
begin
  LoadLanguage(Self);

  btnReplaceAll.Top := btnFindNext.Top;
  btnReplace.Top    := btnReplaceAll.Top;
  chkReplaceWithChange(Sender);

  if (frmMain.VST.SelectedCount = 1) then
    rbnFromTheSelectedSubtitle.Checked := True
  else
    rbnAllTheSubtitles.Checked := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmFindAndReplace.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  frmFindAndReplace := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmFindAndReplace.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
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

    btnReplaceAll.Visible  := Checked;
    btnReplace.Visible     := Checked;
    btnFindNext.Visible    := not Checked;
    btnFind.Visible        := not Checked;
    btnFindPrev.Visible    := not Checked;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmFindAndReplace.btnFindClick(Sender: TObject);
begin
  VSTFind(mmoTextToFind.Text, chkCaseSensitive.Checked, True, False,
    False, '', False, False, chkWholeWord.Checked, chkRegularExpression.Checked);

  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmFindAndReplace.btnFindNextClick(Sender: TObject);
begin
  VSTFindNext;
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmFindAndReplace.btnFindPrevClick(Sender: TObject);
begin
  VSTFindPrevious;
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmFindAndReplace.btnReplaceAllClick(Sender: TObject);
begin
  VSTFind(mmoTextToFind.Text, chkCaseSensitive.Checked, rbnAllTheSubtitles.Checked, False,
    True, mmoReplaceWith.Text, True, chkPreserveCaseOnReplace.Checked, chkWholeWord.Checked, chkRegularExpression.Checked);

  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmFindAndReplace.btnReplaceClick(Sender: TObject);
begin
  VSTFind(mmoTextToFind.Text, chkCaseSensitive.Checked, rbnAllTheSubtitles.Checked, False,
    True, mmoReplaceWith.Text, False, chkPreserveCaseOnReplace.Checked, chkRegularExpression.Checked);

  Close;
end;

// -----------------------------------------------------------------------------

end.

