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

unit formTTS;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, UWRadioButton, UWSystem.TextToSpeech, LCLTranslator, procLocalize;

type

  { TfrmTTS }

  TfrmTTS = class(TForm)
    btnFolder: TButton;
    btnGenerate: TButton;
    btnClose: TButton;
    cboVoice: TComboBox;
    edtFolder: TEdit;
    lblFolder: TLabel;
    lblScope: TLabel;
    lblVoice: TLabel;
    prbTranslate: TProgressBar;
    rbnAllTheSubtitles: TUWRadioButton;
    rbnFromTheSelectedSubtitle: TUWRadioButton;
    rbnOnlySelectedSubtitles: TUWRadioButton;
    procedure btnFolderClick(Sender: TObject);
    procedure btnGenerateClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    TTS: TTextToSpeech;
    procedure EnableControls(const AValue: Boolean);
    procedure DoError(Sender: TObject; const Error, CurrentJob: Integer);
    procedure DoProgress(Sender: TObject; const Index, Total: Integer);
    procedure DoTerminate(Sender: TObject);
    procedure FillComboWithVoices;
  public

  end;

var
  frmTTS: TfrmTTS;
  CancelJobs: Boolean;

// -----------------------------------------------------------------------------

implementation

uses
  procWorkspace, procTypes, procVST, procConfig, procDialogs,
  UWSystem.InetUtils, UWTranslateAPI.Google, UWSubtitleAPI, formMain;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmTTS }

// -----------------------------------------------------------------------------

procedure TfrmTTS.FormCreate(Sender: TObject);
begin
  CancelJobs := False;

  if (frmMain.VST.SelectedCount = 1) then
    rbnFromTheSelectedSubtitle.Checked := True
  else if (frmMain.VST.SelectedCount > 1) then
    rbnOnlySelectedSubtitles.Checked := True
  else
    rbnAllTheSubtitles.Checked := True;

  TTS := TTextToSpeech.Create(Tools.API_KEY_TTS);
  TTS.OnJobsError := @DoError;
  TTS.OnJobsProgress := @DoProgress;
  TTS.OnTerminate := @DoTerminate;

  btnGenerate.Enabled := TTS.api_key <> '';
  FillComboWithVoices;

  {$IFNDEF WINDOWS}
  PrepareCustomControls(Self);
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

procedure TfrmTTS.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  TTS.Free;
  CloseAction := caFree;
  frmTTS := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmTTS.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmTTS.btnCloseClick(Sender: TObject);
begin
  if cboVoice.Enabled then
    Close
  else
    CancelJobs := True;
end;

// -----------------------------------------------------------------------------

procedure ApplyJobTTS(const Item: PUWSubtitleItem; const Index: Integer);
var
  job: PJobInfo;
begin
  New(job);
  job^.Text := Item^.Text;
  job^.FileName := ConcatPaths([frmTTS.edtFolder.Text, 'Entry' + IntToStr(Index+1) + '.mp3']);
  job^.VoiceID := frmTTS.TTS.Voices[frmTTS.cboVoice.ItemIndex]^.ID;
  frmTTS.TTS.Jobs.Add(job);
end;

// -----------------------------------------------------------------------------

procedure TfrmTTS.btnGenerateClick(Sender: TObject);
var
  SelLoop: TVSTDoLoopSelection;
begin
  if edtFolder.Text = '' then
  begin
    btnFolder.SetFocus;
    Exit;
  end;

  if rbnAllTheSubtitles.Checked then
    SelLoop := dlAll
  else if rbnFromTheSelectedSubtitle.Checked then
    SelLoop := dlCurrentToLast
  else
    SelLoop := dlSelected;

  if IsInternetAlive then
  begin
    CancelJobs := False;
    prbTranslate.Position := 0;
    EnableControls(False);
    VSTDoLoop(frmMain.VST, @ApplyJobTTS, SelLoop, False);
    TTS.DoJobs;
  end
  else
    ShowErrorMessageDialog(lngNoInternetConnection);
end;

// -----------------------------------------------------------------------------

procedure TfrmTTS.btnFolderClick(Sender: TObject);
begin
  with TSelectDirectoryDialog.Create(Self) do
  try
    Options := Options + [ofOldStyleDialog, ofCreatePrompt];

    if Execute then
      edtFolder.Text := FileName;
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmTTS.EnableControls(const AValue: Boolean);
begin
  if not AValue then
    btnClose.Caption := lngbtnCancel
  else
    btnClose.Caption := lngbtnClose;

  cboVoice.Enabled                   := AValue;
  edtFolder.Enabled                  := AValue;
  btnFolder.Enabled                  := AValue;
  rbnAllTheSubtitles.Enabled         := AValue;
  rbnFromTheSelectedSubtitle.Enabled := AValue;
  rbnOnlySelectedSubtitles.Enabled   := AValue;
  btnGenerate.Enabled                := AValue;
  prbTranslate.Visible               := not AValue;
end;

// -----------------------------------------------------------------------------

procedure TfrmTTS.DoError(Sender: TObject; const Error, CurrentJob: Integer);
begin
end;

// -----------------------------------------------------------------------------

procedure TfrmTTS.DoProgress(Sender: TObject; const Index, Total: Integer);
begin
  prbTranslate.Max := Total;
  prbTranslate.Position := Index;
end;

// -----------------------------------------------------------------------------

procedure TfrmTTS.DoTerminate(Sender: TObject);
begin
  EnableControls(True);
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmTTS.FillComboWithVoices;
var
  i: Integer;
begin
  with cboVoice do
  begin
    Items.BeginUpdate;
    Clear;

    if TTS.GetVoices then
      for i := 0 to TTS.Voices.Count-1 do
        Items.Add(TTS.Voices[i]^.Name);

    if Items.Count > 0 then
      ItemIndex := 0;

    Items.EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

end.

