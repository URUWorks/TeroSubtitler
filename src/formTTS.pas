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
  ComCtrls, UWRadioButton, UWCheckBox, UWSystem.TextToSpeech, LCLTranslator,
  Spin, LCLIntf, procLocalize;

type

  { TfrmTTS }

  TfrmTTS = class(TForm)
    btnPreview: TButton;
    btnFolder: TButton;
    btnGenerate: TButton;
    btnClose: TButton;
    cboVoice: TComboBox;
    edtFolder: TEdit;
    lblStability: TLabel;
    lblSimilarityBoost: TLabel;
    lblFolder: TLabel;
    lblScope: TLabel;
    lblStyle: TLabel;
    lblVoice: TLabel;
    prbTranslate: TProgressBar;
    rbnAllTheSubtitles: TUWRadioButton;
    rbnFromTheSelectedSubtitle: TUWRadioButton;
    rbnOnlySelectedSubtitles: TUWRadioButton;
    spnSimilarityBoost: TSpinEdit;
    spnStability: TSpinEdit;
    spnStyle: TSpinEdit;
    chkBoost: TUWCheckBox;
    procedure btnFolderClick(Sender: TObject);
    procedure btnGenerateClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnPreviewClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    TTS: TTextToSpeech;
    //MPV: TMPVPlayer;
    procedure EnableControls(const AValue: Boolean);
    procedure DoError(Sender: TObject; const ErrorDesc: String; const ErrorID, CurrentJob: Integer; var Cancel: Boolean);
    procedure DoProgress(Sender: TObject; const Index, Total: Integer);
    procedure DoTerminate(Sender: TObject);
    procedure FillComboWithVoices;
    procedure OpenFolderClick(Sender: TObject);
  public

  end;

var
  frmTTS: TfrmTTS;
  CancelJobs: Boolean;

// -----------------------------------------------------------------------------

implementation

uses
  procWorkspace, procTypes, procVST, procDialogs, UWSystem.InetUtils,
  UWTranslateAPI.Google, UWSubtitleAPI, UWSubtitleAPI.Tags, formMain,
  procConfig, Math;

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
  btnPreview.Enabled := btnGenerate.Enabled;
  FillComboWithVoices;

  {MPV := TMPVPlayer.Create(Self);
  MPV.Parent := Self;
  MPV.Top := Height + 50;
  MPV.LogLevel := llNo;
  MPV.AutoStartPlayback := True;}

  {$IFNDEF WINDOWS}
  PrepareCustomControls(Self);
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

procedure TfrmTTS.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  SaveFormSettings(Self, Format('%d,%d,%d,%d,%d', [cboVoice.ItemIndex, spnSimilarityBoost.Value, spnStability.Value, spnStyle.Value, Integer(chkBoost.Checked)]));

  //MPV.Free;
  TTS.Free;
  CloseAction := caFree;
  frmTTS := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmTTS.FormShow(Sender: TObject);
var
  s: String;
  AParamArray: TStringArray;
begin
  CheckColorTheme(Self);
  s := LoadFormSettings(Self);
  if not s.IsEmpty then
  begin
    AParamArray := s.Split(',');
    if Length(AParamArray) = 5 then
    begin
      if InRange(AParamArray[0].ToInteger, 0, cboVoice.Items.Count-1) then
        cboVoice.ItemIndex := AParamArray[0].ToInteger;

      spnSimilarityBoost.Value := AParamArray[1].ToInteger;
      spnStability.Value := AParamArray[2].ToInteger;
      spnStyle.Value := AParamArray[3].ToInteger;
      chkBoost.Checked := Boolean(AParamArray[4].ToInteger);
    end;
  end;
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

procedure TfrmTTS.btnPreviewClick(Sender: TObject);
var
  s: String;
begin
  if IsInternetAlive then
  begin
    s := ChangeFileExt(GetTempFileName, '.mp3');
{    if TTS.Make('You are using Tero Subtitler!', s, TTS.Voices[cboVoice.ItemIndex]^.ID, spnSimilarityBoost.Value, spnStability.Value) then
    begin
      if MPV.IsMediaLoaded then MPV.Close;
      MPV.Play(s);
    end;}
  end
  else
    ShowErrorMessageDialog(lngNoInternetConnection);
end;

// -----------------------------------------------------------------------------

procedure ApplyJobTTS(const Item: PUWSubtitleItem; const Index: Integer);
var
  job: PJobInfo;
begin
  New(job);
  job^.Text := RemoveTSTags(Item^.Text);
  job^.FileName := ConcatPaths([frmTTS.edtFolder.Text, 'Entry' + IntToStr(Index+1) + '.mp3']);
  job^.VoiceID := frmTTS.TTS.Voices[frmTTS.cboVoice.ItemIndex]^.ID;
  job^.VoiceIndex := 0;
  job^.Similarity := frmTTS.spnSimilarityBoost.Value / 100;
  job^.Stability := frmTTS.spnStability.Value / 100;
  job^.Style := frmTTS.spnStyle.Value / 100;
  job^.Boost := frmTTS.chkBoost.Checked;
  frmTTS.TTS.Jobs.Add(job);
end;

// -----------------------------------------------------------------------------

procedure TfrmTTS.btnGenerateClick(Sender: TObject);
var
  SelLoop: TVSTDoLoopSelection;
begin
  CancelJobs := False;

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
    prbTranslate.Position := 0;
    EnableControls(False);

    if TTS.Finished then
    begin
      Application.ProcessMessages;
      TTS.Free;
      TTS := TTextToSpeech.Create(Tools.API_KEY_TTS);
      TTS.OnJobsError := @DoError;
      TTS.OnJobsProgress := @DoProgress;
      TTS.OnTerminate := @DoTerminate;
      TTS.GetVoices;
    end;

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
  spnSimilarityBoost.Enabled         := AValue;
  spnStability.Enabled               := AValue;
  spnStyle.Enabled                   := AValue;
  chkBoost.Enabled                   := AValue;
  btnPreview.Enabled                 := AValue;
  prbTranslate.Visible               := not AValue;
end;

// -----------------------------------------------------------------------------

procedure TfrmTTS.DoError(Sender: TObject; const ErrorDesc: String; const ErrorID, CurrentJob: Integer; var Cancel: Boolean);
begin
  Cancel := (ErrorID = JOB_ERROR_QuotaExceeded);
  if Cancel then
  begin
    ShowErrorMessageDialog(lngvdQuotaExceeded);
    CancelJobs := True;
  end
  else
    ShowErrorMessageDialog(Format(lngOperationFailed, [CurrentJob, TTS.Jobs.Count, ErrorID]));
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
  if CancelJobs then
    ShowErrorMessageDialog(lngOperationCancelled)
  else if TTS.Jobs.Count > 0 then
    ShowMessageDialog(lngOperationCompleted, '', lngOpenContainingFolder, @OpenFolderClick);

  EnableControls(True);
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

procedure TfrmTTS.OpenFolderClick(Sender: TObject);
begin
  OpenDocument(edtFolder.Text);
end;

// -----------------------------------------------------------------------------

end.

