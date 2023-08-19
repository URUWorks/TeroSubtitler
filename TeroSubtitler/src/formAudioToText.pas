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

unit formAudioToText;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  UWRadioButton, UWCheckBox;

type

  { TfrmAudioToText }

  TfrmAudioToText = class(TForm)
    btnGenerate: TButton;
    btnClose: TButton;
    btnModel: TButton;
    cboLanguage: TComboBox;
    cboTrack: TComboBox;
    cboModel: TComboBox;
    cboModel1: TComboBox;
    lblLanguage: TLabel;
    lblStatus: TLabel;
    lblTimeElapsed: TLabel;
    lblTrack: TLabel;
    lblModel: TLabel;
    lblModel1: TLabel;
    lblWait: TLabel;
    prbProgress: TProgressBar;
    rbnAddSubtitlesWhileTranscribing: TUWRadioButton;
    rbnLoadSubtitlesAfterTranscript: TUWRadioButton;
    chkTranslate: TUWCheckBox;
    procedure btnCloseClick(Sender: TObject);
    procedure btnGenerateClick(Sender: TObject);
    procedure btnModelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure SetControlsEnabled(const AValue: Boolean);
  public

  end;

var
  frmAudioToText: TfrmAudioToText;
  CancelProcess: Boolean;

// -----------------------------------------------------------------------------

implementation

uses
  procWorkspace, procConfig, procDialogs, procTypes, UWSystem.TimeUtils,
  formMain, UWSystem.Process, procFiles, procForms, StrUtils,
  UWTranslateAPI.Google, procSubtitle, procVST, UWSubtitleAPI.Formats,
  UWSystem.SysUtils;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmAudioToText }

// -----------------------------------------------------------------------------

procedure TfrmAudioToText.FormCreate(Sender: TObject);
begin
  LoadLanguage(Self);
  FillComboWithGoogleLanguages(cboLanguage, 0);
  FillComboWithModels(cboModel);
  FillComboWithAudioStreams(cboTrack);
  cboLanguage.Items[0] := GetCommonString('Detect');
  btnGenerate.Enabled := (cboTrack.Items.Count > 0);
  CancelProcess := False;
end;

// -----------------------------------------------------------------------------

procedure TfrmAudioToText.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  frmAudioToText := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmAudioToText.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmAudioToText.btnCloseClick(Sender: TObject);
begin
  if (cboTrack.Items.Count = 0) or btnGenerate.Enabled then
    Close
  else
    CancelProcess := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmAudioToText.btnModelClick(Sender: TObject);
begin
  // model download
  ShowAudioToTextModels;
  FillComboWithModels(cboModel);
end;

// -----------------------------------------------------------------------------

procedure ProcessCB(const TimeElapsed: Double; var Cancel: Boolean);
begin
  frmAudioToText.lblTimeElapsed.Caption := TimeToString(Trunc(TimeElapsed)*1000, 'mm:ss');
  Cancel := CancelProcess;
end;

// -----------------------------------------------------------------------------

procedure ProcessCBEx(Output: String; var Cancel: Boolean);
var
  sl : TStringList;
  i, it, ft : Integer;
  s : String;
begin
  Cancel := CancelProcess;
  Application.ProcessMessages;
  // process output received
  sl := TStringList.Create;
  try
    sl.Text := Output;
    for i := 0 to sl.Count-1 do
    begin
      // Subtitle info
      if Pos('-->', sl[i]) = 15 then
      begin
        it     := StringToTime(Copy(sl[i], 2, 12));
        ft     := StringToTime(Copy(sl[i], 19, 12));
        Output := Copy(sl[i], 35, sl[i].Length-34);
        VSTSelectNode(frmMain.VST, InsertSubtitle(frmMain.VST.RootNodeCount+1, it, ft, Output, '', False, True), True);
      end;
      // Progress
      if Pos('progress =', sl[i]) > 0 then
      begin
        it := sl[i].LastIndexOf(' ');
        ft := sl[i].Length-it;
        s  := Copy(sl[i], it+1, ft-1).Trim;
        frmAudioToText.prbProgress.Position   := s.ToInteger;
        frmAudioToText.lblTimeElapsed.Caption := s + '%';
      end;
    end;
  finally
    sl.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmAudioToText.btnGenerateClick(Sender: TObject);
var
  s, ss, cn: String;
  i: Integer;
  AParamArray: TStringArray;
  isPlaying: Boolean;
  TimerSub: Boolean;
begin
  if frmMain.MPV.FileName.StartsWith('http') then
    ShowErrorMessageDialog(GetCommonString('FeatureNotAvailableFromURL'))
  else if not FileExists(GetExtractAppFile) then
    ShowErrorMessageDialog(Format(GetCommonString('ExtractAppError'), [ExtractFileName(Tools.FFmpeg)]))
  else if not FileExists(GetAudioToTextAppFile) then
    ShowErrorMessageDialog(Format(GetCommonString('ExtractAppError'), [ExtractFileName(Tools.WhisperCPP)]))
  else if (Tools.FFmpeg_ParamsForAudioExtract <> '') and (Tools.WhisperCPP_Params <> '') and (cboModel.ItemIndex >= 0) then
  begin
    CancelProcess     := False;
    lblStatus.Caption := GetCommonString('Extracting');
    SetControlsEnabled(False);

    TimerSub := frmMain.TimerSubtitle.Enabled;
    frmMain.TimerSubtitle.Enabled := False;

    isPlaying := frmMain.MPV.IsPlaying;
    if isPlaying then
      frmMain.MPV.Pause;

    Application.ProcessMessages;
    try
      // extract wave file, ffmpeg -i input.mp3 -ar 16000 -ac 1 -c:a pcm_s16le output.wav
      //s := ChangeFileExt(GetTempFileName(WhisperTranscriptionsFolder, 'temp'), '.wav');
      s := ChangeFileExt(GetTempFileName, '.wav');
      AParamArray := WHISPER_ffParams.Split(' ');
      for i := 0 to High(AParamArray) do
        AParamArray[i] := StringsReplace(AParamArray[i], ['%input', '%output', '%trackid'], [frmMain.MPV.FileName, s, cboTrack.ItemIndex.ToString], []);

      if ExecuteApp(GetExtractAppFile, AParamArray, True, True, @ProcessCB) then
      begin
        if FileExists(s) then // wave extracted
        begin
          // do transcribe, main -m models/ggml-base.en.bin -l en -osrt -of outputfile.srt -f samples/jfk.wav
          ss := WHISPER_Params;
          if rbnAddSubtitlesWhileTranscribing.Checked then
            ss.Insert(ss.IndexOf(' -osrt'), ' -pp');

          if chkTranslate.Checked then
            ss.Insert(ss.IndexOf(' -osrt'), ' -tr');

          AParamArray := ss.Split(' ');

          cn := GoogleTranslateLocale[cboLanguage.ItemIndex];
          ss := ConcatPaths([WhisperTranscriptionsFolder, ChangeFileExt(ExtractFileName(frmMain.MPV.FileName), '_'+cn)]);
          lblStatus.Caption := GetCommonString('Transcribing');
          Application.ProcessMessages;

          for i := 0 to High(AParamArray) do
            AParamArray[i] := StringsReplace(AParamArray[i], ['%input', '%output', '%model', '%lang'], [s, ss, ConcatPaths([WhisperModelsFolder, cboModel.Text+'.bin']), cn], []);

          if rbnAddSubtitlesWhileTranscribing.Checked then
          begin
            if ExecuteAppEx(GetAudioToTextAppFile, AParamArray, @ProcessCBEx) then
            begin
              // delete wave file
              DeleteFile(s);

              with frmMain, AppOptions do
              begin
                i := cboInputFPS.Items.IndexOf( SingleToStr( MPV.GetVideoFPS, FormatSettings) );
                if i >= 0 then
                begin
                  cboFPS.ItemIndex      := i;
                  cboInputFPS.ItemIndex := i;
                end;

                DoAutoCheckErrors;
                if (VST.RootNodeCount > 0) and not VST.Enabled then
                begin
                  EnableWorkArea;
                  EnableActionsByTag([TAG_ACTION_VIDEO], True);
                  if WAVE.IsTimeLineEnabled then
                    EnableActionsByTag([TAG_ACTION_AUDIO], True);
                end;
                TimerSubtitle.Enabled := TimerSub;
                if isPlaying then MPV.Resume;
              end;
            end
            else
              ShowErrorMessageDialog(Format(GetCommonString('ErrorExecuting'), [ExtractFileName(Tools.FFmpeg)]));
          end
          else
          begin
            if ExecuteApp(GetAudioToTextAppFile, AParamArray, True, True, @ProcessCB) then
            begin
              // delete wave file
              DeleteFile(s);

              //s := ChangeFileExt(ss, '.srt'); // doesnt work :S
              s := ss; // transcribed srt file
              if not s.EndsWith('.srt') then
                s := s + '.srt';

              if FileExists(s) then
              begin
                LoadSubtitle(s, sfInvalid, NIL, -1, False);
              end;
            end
            else
              ShowErrorMessageDialog(Format(GetCommonString('ErrorExecuting'), [ExtractFileName(Tools.WhisperCPP)]));
          end;
        end;
      end
      else
        ShowErrorMessageDialog(Format(GetCommonString('ErrorExecuting'), [ExtractFileName(Tools.FFmpeg)]));
    finally
      SetControlsEnabled(True);
      Close;
    end;
  end
end;

// -----------------------------------------------------------------------------

procedure TfrmAudioToText.SetControlsEnabled(const AValue: Boolean);
begin
  prbProgress.Position   := 0;
  lblTimeElapsed.Caption := '';

  btnGenerate.Enabled    := AValue;
  lblWait.Visible        := not AValue;
  lblStatus.Visible      := not AValue;
  lblTimeElapsed.Visible := not AValue;

  btnGenerate.Enabled := AValue;
  btnModel.Enabled    := AValue;
  cboTrack.Enabled    := AValue;
  cboLanguage.Enabled := AValue;
  cboModel.Enabled    := AValue;

  rbnAddSubtitlesWhileTranscribing.Enabled := AValue;
  rbnLoadSubtitlesAfterTranscript.Enabled  := AValue;

  if rbnAddSubtitlesWhileTranscribing.Checked then
    prbProgress.Visible := not AValue;

  if AValue then
  begin
    btnClose.Caption := GetCommonString('btnClose', 'CommonControls');
    btnClose.Tag := 0;
  end
  else
  begin
    btnClose.Caption := GetCommonString('btnCancel', 'CommonControls');
    btnClose.Tag := 1;
  end;
end;

// -----------------------------------------------------------------------------

end.

