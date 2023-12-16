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

unit formWaveExtractor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, StdCtrls, ComCtrls, LCLTranslator, procLocalize;

type

  { TfrmWaveExtractor }

  TfrmWaveExtractor = class(TForm)
    btnExtract: TButton;
    btnClose: TButton;
    cboTrack: TComboBox;
    lblStatus: TLabel;
    lblTimeElapsed: TLabel;
    lblTrack: TLabel;
    lblWait: TLabel;
    prbProgress: TProgressBar;
    procedure btnCloseClick(Sender: TObject);
    procedure btnExtractClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  frmWaveExtractor: TfrmWaveExtractor;
  CancelExtraction: Boolean;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, procWorkspace, procConfig, procDialogs, UWSystem.Process,
  UWSystem.TimeUtils, StrUtils, formMain, MPVPlayer;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmWaveExtractor }

// -----------------------------------------------------------------------------

procedure TfrmWaveExtractor.FormCreate(Sender: TObject);
begin
  FillComboWithAudioStreams(cboTrack);
  btnExtract.Enabled := (cboTrack.Items.Count > 0);
  CancelExtraction := False;
end;

// -----------------------------------------------------------------------------

procedure TfrmWaveExtractor.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  frmWaveExtractor := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmWaveExtractor.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmWaveExtractor.btnCloseClick(Sender: TObject);
begin
  if btnExtract.Enabled then
    Close
  else
    CancelExtraction := True;
end;

// -----------------------------------------------------------------------------

procedure ProcessCB(const TimeElapsed: Double; var Cancel: Boolean);
begin
  frmWaveExtractor.lblTimeElapsed.Caption := TimeToString(Trunc(TimeElapsed)*1000, 'mm:ss');
  Cancel := CancelExtraction;
end;

// -----------------------------------------------------------------------------

procedure TfrmWaveExtractor.btnExtractClick(Sender: TObject);
var
  s: String;
  p: Boolean;
  i: Integer;
  AParamArray: TStringArray;
begin
  if frmMain.MPV.FileName.StartsWith('http') then
    ShowErrorMessageDialog(lngFeatureNotAvailableFromURL)
  else if not FileExists(GetExtractAppFile) then
    ShowErrorMessageDialog(Format(lngExtractAppError, [ExtractFileName(GetExtractAppFile)]))
  else if (Tools.FFmpeg_ParamsForAudioExtract <> '') then
  begin
    CancelExtraction       := False;
    btnExtract.Enabled     := False;
    //btnClose.Enabled       := False;
    btnClose.Caption       := lngbtnCancel;
    lblWait.Visible        := True;
    lblStatus.Visible      := True;
    lblTimeElapsed.Visible := True;
    lblStatus.Caption      := lngExtracting;

    Application.ProcessMessages;
    try
      if not DirectoryExists(WaveformsFolder) then
      begin
        if not ForceDirectories(WaveformsFolder) then
        begin
          ShowErrorMessageDialog(lngWriteDenieded);
          Exit;
        end;
      end;

      s := WaveformsFolder + ChangeFileExt(ExtractFileName(frmMain.MPV.FileName), '.wav');
      p := frmMain.MPV.IsPlaying;
      if p then frmMain.MPV.Pause;

      AParamArray := Tools.FFmpeg_ParamsForAudioExtract.Split(' ');
      for i := 0 to High(AParamArray) do
        AParamArray[i] := StringsReplace(AParamArray[i], ['%input', '%output', '%trackid'], [frmMain.MPV.FileName, s, cboTrack.ItemIndex.ToString], []);

      ExecuteApp(GetExtractAppFile, AParamArray, True, True, @ProcessCB);

      if FileExists(s) and frmMain.WAVE.LoadWaveFromFile(s) then
      begin
        if not frmMain.actWaveformPreview.Checked then frmMain.actWaveformPreview.Execute;
        EnableActionsByTag([TAG_ACTION_AUDIO], True);

        DeleteFile(s);
      end;

      if p then frmMain.MPV.Resume;
    finally
      lblWait.Visible        := False;
      lblStatus.Visible      := False;
      lblTimeElapsed.Visible := False;
      btnExtract.Enabled     := True;
      //btnClose.Enabled       := True;
      btnClose.Caption       := lngbtnClose;
      Close;
    end;
  end
end;

// -----------------------------------------------------------------------------

end.

