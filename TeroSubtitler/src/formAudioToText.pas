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
  UWRadioButton, UWCheckBox, LCLIntf, LCLTranslator, procLocalize;

type

  { TfrmAudioToText }

  TfrmAudioToText = class(TForm)
    btnGenerate: TButton;
    btnClose: TButton;
    btnModel: TButton;
    btnEngine: TButton;
    cboLanguage: TComboBox;
    cboEngine: TComboBox;
    cboTrack: TComboBox;
    cboModel: TComboBox;
    cboModel1: TComboBox;
    lblLanguage: TLabel;
    lblEngine: TLabel;
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
    procedure btnEngineClick(Sender: TObject);
    procedure btnGenerateClick(Sender: TObject);
    procedure btnModelClick(Sender: TObject);
    procedure cboEngineSelect(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure SetControlsEnabled(const AValue: Boolean);
    procedure OpenFolderClick(Sender: TObject);
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
  UWSystem.SysUtils, UWSystem.ThreadProcess;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmAudioToText }

// -----------------------------------------------------------------------------

procedure TfrmAudioToText.FormCreate(Sender: TObject);
begin
  FillComboWithWhisperEngines(cboEngine, Integer(Tools.WhisperEngine));
  FillComboWithGoogleLanguages(cboLanguage, 0);
  FillComboWithAudioStreams(cboTrack);
  cboLanguage.Items[0] := lngDetect;
  btnGenerate.Enabled := (cboTrack.Items.Count > 0);
  CancelProcess := False;
  cboEngineSelect(NIL);
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

procedure TfrmAudioToText.btnEngineClick(Sender: TObject);
begin
  // Additional engine params
  with Tools do
    if WhisperEngine = TWhisperEngine.WhisperCPP then
      WhisperCPP_Additional := InputDialog('Whisper.CPP', lngAdditionalParams, WhisperCPP_Additional)
    else
      FasterWhisper_Additional := InputDialog('Faster-Whisper', lngAdditionalParams, FasterWhisper_Additional);
end;

// -----------------------------------------------------------------------------

procedure TfrmAudioToText.btnModelClick(Sender: TObject);
begin
  // model download
  ShowAudioToTextModels;
  FillComboWithModels(cboModel);
end;

// -----------------------------------------------------------------------------

procedure TfrmAudioToText.cboEngineSelect(Sender: TObject);
begin
  with Tools do
  begin
    WhisperEngine := TWhisperEngine(cboEngine.ItemIndex);

    {rbnAddSubtitlesWhileTranscribing.Enabled := WhisperEngine = TWhisperEngine.WhisperCPP;
    if not rbnAddSubtitlesWhileTranscribing.Enabled then
      rbnLoadSubtitlesAfterTranscript.Checked := True;}

    if WhisperEngine = TWhisperEngine.WhisperCPP then
      FillComboWithModels(cboModel)
    else
      FillComboWithFasterModels(cboModel);
  end;
end;

// -----------------------------------------------------------------------------

procedure ProcessCB(const TimeElapsed: Double; var Cancel: Boolean);
begin
  frmAudioToText.lblTimeElapsed.Caption := TimeToString(Trunc(TimeElapsed)*1000, 'mm:ss');
  Cancel := CancelProcess;
end;

// -----------------------------------------------------------------------------

procedure ThreadProcessCB(Output: String; var ATerminate: Boolean);
var
  sl : TStringList;
  i, it, ft : Integer;
  s : String;
begin
  ATerminate := CancelProcess;
  //writeln(output);
  Application.ProcessMessages;
  // process output received
  sl := TStringList.Create;
  try
    sl.Text := Output;

    for i := 0 to sl.Count-1 do
    begin
      if Tools.WhisperEngine = WhisperCPP then
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
      end
      else
      begin
        // Subtitle info
        if Pos('-->', sl[i]) = 12 then
        begin
          it     := StringToTime(Copy(sl[i], 2, 9), True);
          ft     := StringToTime(Copy(sl[i], 16, 9), True);
          Output := Copy(sl[i], 28);
          VSTSelectNode(frmMain.VST, InsertSubtitle(frmMain.VST.RootNodeCount+1, it, ft, Output, '', False, True), True);
        end;
      end;
    end;
  finally
    sl.Free;
  end;
end;

procedure ThreadProcessCBTime(const ATime: Double);
begin
  if Tools.WhisperEngine <> WhisperCPP then
    frmAudioToText.lblTimeElapsed.Caption := TimeToString(Trunc(ATime)*1000, 'mm:ss');
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
      if Tools.WhisperEngine = WhisperCPP then
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
      end
      else
      begin
        // Subtitle info
        if Pos('-->', sl[i]) = 12 then
        begin
          it     := StringToTime(Copy(sl[i], 2, 9), True);
          ft     := StringToTime(Copy(sl[i], 16, 9), True);
          Output := Copy(sl[i], 28);
          VSTSelectNode(frmMain.VST, InsertSubtitle(frmMain.VST.RootNodeCount+1, it, ft, Output, '', False, True), True);
        end;
      end;
    end;
  finally
    sl.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmAudioToText.btnGenerateClick(Sender: TObject);
const
  pyEnvironment = 'PYTHONUTF8=1;PYTHONIOENCODING=utf-8;PYTHONLEGACYWINDOWSSTDIO=utf-8';

var
  s, ss, cn, model, modelpath, srtfile: String;
  i: Integer;
  AParamArray: TStringArray;
  AEnvironment: TStringArray;
  isPlaying: Boolean;
  TimerSub: Boolean;
begin
  if frmMain.MPV.FileName.StartsWith('http') then
    ShowErrorMessageDialog(lngFeatureNotAvailableFromURL)
  else if not FileExists(GetExtractAppFile) then
    ShowErrorMessageDialog(Format(lngExtractAppError, [ExtractFileName(Tools.FFmpeg)]))
  else if not FileExists(GetAudioToTextAppFile) then
    ShowErrorMessageDialog(Format(lngExtractAppError, [ExtractFileName(GetAudioToTextAppFile)]))
  else if (Tools.FFmpeg_ParamsForAudioExtract <> '') and ((Tools.WhisperCPP_Params <> '') or (Tools.FasterWhisper_Params <> '')) and (cboModel.ItemIndex >= 0) then
  begin
    CancelProcess     := False;
    lblStatus.Caption := lngExtracting;
    SetControlsEnabled(False);

    TimerSub := frmMain.TimerSubtitle.Enabled;
    frmMain.TimerSubtitle.Enabled := False;

    isPlaying := frmMain.MPV.IsPlaying;
    if isPlaying then
      frmMain.MPV.Pause;

    Application.ProcessMessages;
    try
      SetLength(AEnvironment, 0);

      // extract wave file, ffmpeg -i input.mp3 -ar 16000 -ac 1 -c:a pcm_s16le output.wav
      s := ChangeFileExt(GetTempFileName, '.wav');
      AParamArray := WHISPER_ffParams.Split(' ');
      for i := 0 to High(AParamArray) do
        AParamArray[i] := StringsReplace(AParamArray[i], ['%input', '%output', '%trackid'], [frmMain.MPV.FileName, s, cboTrack.ItemIndex.ToString], []);

      if ExecuteApp(GetExtractAppFile, AParamArray, True, True, @ProcessCB) then
      begin
        SetLength(AParamArray, 0);

        if FileExists(s) then // wave extracted
        begin
          // do transcribe, main -m models/ggml-base.en.bin -l en -osrt -of outputfile.srt -f samples/jfk.wav
          ss := GetAudioToTextParams;
          cn := GoogleTranslateLocale[cboLanguage.ItemIndex];

          if Tools.WhisperEngine = TWhisperEngine.WhisperCPP then
          begin
            if rbnAddSubtitlesWhileTranscribing.Checked then
              ss.Insert(ss.IndexOf(' -osrt'), ' -pp');

            if chkTranslate.Checked then
              ss.Insert(ss.IndexOf(' -osrt'), ' -tr');

            if not Tools.WhisperCPP_Additional.IsEmpty then
              ss := ss + ' ' + Tools.WhisperCPP_Additional;

            model := ConcatPaths([WhisperModelsFolder, cboModel.Text+'.bin']);
            modelpath := '';
          end
          else
          begin
            if cboLanguage.ItemIndex <> 0 then
              ss := ss + ' --language %lang';

            if chkTranslate.Checked then
              ss := ss + ' --task translate';

            if not Tools.FasterWhisper_Additional.IsEmpty then
              ss := ss + ' ' + Tools.FasterWhisper_Additional;

            model := cboModel.Text;
            modelpath := ExcludeTrailingPathDelimiter(WhisperModelsFolder);

            AEnvironment := pyEnvironment.Split(';');
          end;

          AParamArray := ss.Split(' ');

          if Tools.WhisperEngine = TWhisperEngine.WhisperCPP then
          begin
            ss := ConcatPaths([WhisperTranscriptionsFolder, ChangeFileExt(ExtractFileName(frmMain.MPV.FileName), '_' + cn)]);
            srtfile := ss + '.srt';
          end
          else
          begin
            ss := ExcludeTrailingPathDelimiter(WhisperTranscriptionsFolder);
            srtfile := ConcatPaths([WhisperTranscriptionsFolder, ChangeFileExt(ExtractFileName(frmMain.MPV.FileName), '_' + cn)]) + '.srt';
          end;

          lblStatus.Caption := lngTranscribing;
          Application.ProcessMessages;

          for i := 0 to High(AParamArray) do
            AParamArray[i] := StringsReplace(AParamArray[i], ['%input', '%output', '%model', '%binpath', '%lang'], [s, ss, model, modelpath, cn], []);

          if rbnAddSubtitlesWhileTranscribing.Checked then //and rbnAddSubtitlesWhileTranscribing.Enabled then
          begin
            //if ExecuteAppEx(GetAudioToTextAppFile, AParamArray, @ProcessCBEx) then
            if ExecuteThreadProcess(GetAudioToTextAppFile, AParamArray, AEnvironment, @ThreadProcessCB, @ThreadProcessCBTime) then
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
              ShowErrorMessageDialog(Format(lngErrorExecuting, [ExtractFileName(Tools.FFmpeg)]));
          end
          else
          begin
            if ExecuteApp(GetAudioToTextAppFile, AParamArray, True, True, @ProcessCB) then
            begin
              // delete wave file
              DeleteFile(s);

              if Tools.WhisperEngine <> TWhisperEngine.WhisperCPP then
              begin
                s := ConcatPaths([WhisperTranscriptionsFolder, ChangeFileExt(ExtractFileName(s), '.srt')]);
                if FileExists(srtfile) then DeleteFile(srtfile);
                RenameFile(s, srtfile);
              end;

              if FileExists(srtfile) then
              begin
                ShowMessageDialog(lngFileSavedSuccessfully, '', lngOpenContainingFolder, @OpenFolderClick);
                LoadSubtitle(srtfile, sfSubRip, NIL, -1, False);
              end;
            end
            else
              ShowErrorMessageDialog(Format(lngErrorExecuting, [ExtractFileName(Tools.WhisperCPP)]));
          end;
        end;
      end
      else
        ShowErrorMessageDialog(Format(lngErrorExecuting, [ExtractFileName(Tools.FFmpeg)]));
    finally
      SetLength(AParamArray, 0);
      SetLength(AEnvironment, 0);
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

  btnEngine.Enabled   := AValue;
  btnModel.Enabled    := AValue;
  cboTrack.Enabled    := AValue;
  cboEngine.Enabled   := AValue;
  cboLanguage.Enabled := AValue;
  cboModel.Enabled    := AValue;

  rbnAddSubtitlesWhileTranscribing.Enabled := AValue;
  rbnLoadSubtitlesAfterTranscript.Enabled  := AValue;

  if rbnAddSubtitlesWhileTranscribing.Checked and (cboEngine.ItemIndex = 0) then
    prbProgress.Visible := not AValue;

  if AValue then
  begin
    btnClose.Caption := lngbtnClose;
    btnClose.Tag := 0;
  end
  else
  begin
    btnClose.Caption := lngbtnCancel;
    btnClose.Tag := 1;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmAudioToText.OpenFolderClick(Sender: TObject);
begin
  OpenDocument(WhisperTranscriptionsFolder);
end;

// -----------------------------------------------------------------------------

end.

