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

unit procForms;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Forms;

{ Show }
procedure ShowAbout;
procedure ShowWelcome;
procedure ShowWizard;
procedure ShowFindAndReplace(const ReplaceMode: Boolean = False);
procedure ShowGoTo;
procedure ShowDurationLimits;
procedure ShowAutomaticDurations;
procedure ShowTimeExpander;
procedure ShowSetDelay;
procedure ShowShiftTimes;
procedure ShowConvertCase;
procedure ShowAdjustSubtitle;
procedure ShowMultipleReplace;
procedure ShowModifySelection;
procedure ShowTranslate;
procedure ShowWaveExtractor;
procedure ShowSettings;
procedure ShowFormatProperties;
procedure ShowRestoreBackup;
procedure ShowFixSubtitles;
procedure ShowTranslationMemory;
procedure ShowTranslationMemorySettings;
procedure ShowTranslationMemoryList;
procedure ShowSpellCheck;
procedure ShowGoToTime;
procedure ShowActors;
procedure ShowShotChanges;
procedure ShowAudioToText;
procedure ShowAudioToTextModels;
procedure ShowBatchConvert;
procedure ShowQualityCheck;
procedure ShowCompare;
procedure ShowCustomTextFormat;
procedure ShowTBXSettings;
procedure ShowTBX;
procedure ShowTBXList;
procedure ShowTBXEdit;
{ Close }
procedure CloseTranslationMemory;
procedure CloseTBX;

// -----------------------------------------------------------------------------

implementation

uses
  formAbout, formWelcome, formFindAndReplace, formGoTo, formDurationLimits,
  formAutomaticDurations, formTimeExpander, formDelay, formConvertCase,
  formMain, formAdjustSubtitle, formMultipleReplace, formModifySelection,
  formTranslate, formWaveExtractor, formSettings, formRestoreBackup,
  formFixSubtitles, formTranslationMemory, formTranslationMemorySettings,
  formSpellCheck, formGoToTime, formShotChanges, formBatchConvert,
  formQualityCheck, formCompare, formTranslationMemoryList, formTBXList,
  formTBX, formTBXSettings, formTBXEdit, formWizard, formShiftTimes,
  formAudioToText, formAudioToTextModel, procTypes, procCommon, procWorkspace,
  formFormatProperties, formCustomTextFormat, formActors, procMPV;

// -----------------------------------------------------------------------------

{ Show }

// -----------------------------------------------------------------------------

procedure ShowForm(const AForm: TForm; const AModal: Boolean = True);
begin
  if AForm <> NIL then
  begin
    if AModal then
      AForm.ShowModal
    else
      AForm.Show;
  end;
end;

// -----------------------------------------------------------------------------

procedure ShowAbout;
begin
  if frmAbout = NIL then
  begin
    frmAbout := TfrmAbout.Create(Application);
    ShowForm(frmAbout);
  end
  else
    frmAbout.BringToFront;
end;

// -----------------------------------------------------------------------------

procedure ShowWelcome;
begin
  if frmWelcome = NIL then
  begin
    frmWelcome := TfrmWelcome.Create(Application);
    ShowForm(frmWelcome{$IFDEF DARWIN}, False{$ENDIF});
  end;
end;

// -----------------------------------------------------------------------------

procedure ShowWizard;
begin
  if frmWizard = NIL then
  begin
    frmWizard := TfrmWizard.Create(Application);
    ShowForm(frmWizard);
  end;
end;

// -----------------------------------------------------------------------------

procedure ShowFindAndReplace(const ReplaceMode: Boolean = False);
begin
  if frmFindAndReplace = NIL then
  begin
    frmFindAndReplace := TfrmFindAndReplace.Create(Application);
    frmFindAndReplace.chkReplaceWith.Checked := ReplaceMode;
    ShowForm(frmFindAndReplace);
  end
  else
  begin
    frmFindAndReplace.chkReplaceWith.Checked := ReplaceMode;
    frmFindAndReplace.BringToFront;
  end;
end;

// -----------------------------------------------------------------------------

procedure ShowGoTo;
begin
  if frmGoTo = NIL then
  begin
    frmGoTo := TfrmGoTo.Create(Application);
    ShowForm(frmGoTo);
  end
  else
    frmGoTo.BringToFront;
end;

// -----------------------------------------------------------------------------

procedure ShowDurationLimits;
begin
  if frmDurationLimits = NIL then
  begin
    frmDurationLimits := TfrmDurationLimits.Create(Application);
    ShowForm(frmDurationLimits);
  end
  else
    frmDurationLimits.BringToFront;
end;

// -----------------------------------------------------------------------------

procedure ShowAutomaticDurations;
begin
  if frmAutomaticDurations = NIL then
  begin
    frmAutomaticDurations := TfrmAutomaticDurations.Create(Application);
    ShowForm(frmAutomaticDurations);
  end
  else
    frmAutomaticDurations.BringToFront;
end;

// -----------------------------------------------------------------------------

procedure ShowTimeExpander;
begin
  if frmTimeExpander = NIL then
  begin
    frmTimeExpander := TfrmTimeExpander.Create(Application);
    ShowForm(frmTimeExpander);
  end
  else
    frmTimeExpander.BringToFront;
end;

// -----------------------------------------------------------------------------

procedure ShowSetDelay;
begin
  if frmDelay = NIL then
  begin
    frmDelay := TfrmDelay.Create(Application);
    ShowForm(frmDelay);
  end
  else
    frmDelay.BringToFront;
end;

// -----------------------------------------------------------------------------

procedure ShowShiftTimes;
begin
  if frmShiftTimes = NIL then
  begin
    frmShiftTimes := TfrmShiftTimes.Create(Application);
    ShowForm(frmShiftTimes);
  end
  else
    frmShiftTimes.BringToFront;
end;

// -----------------------------------------------------------------------------

procedure ShowConvertCase;
begin
  if frmConvertCase = NIL then
  begin
    frmConvertCase := TfrmConvertCase.Create(Application);
    ShowForm(frmConvertCase);
  end
  else
    frmConvertCase.BringToFront;
end;

// -----------------------------------------------------------------------------

procedure ShowAdjustSubtitle;
begin
  if frmAdjustSubtitle = NIL then
  begin
    frmAdjustSubtitle := TfrmAdjustSubtitle.Create(Application);
    ShowForm(frmAdjustSubtitle);
  end
  else
    frmAdjustSubtitle.BringToFront;
end;

// -----------------------------------------------------------------------------

procedure ShowMultipleReplace;
begin
  if frmMultipleReplace = NIL then
  begin
    frmMultipleReplace := TfrmMultipleReplace.Create(Application);
    ShowForm(frmMultipleReplace);
  end
  else
    frmMultipleReplace.BringToFront;
end;

// -----------------------------------------------------------------------------

procedure ShowModifySelection;
begin
  if frmModifySelection = NIL then
  begin
    frmModifySelection := TfrmModifySelection.Create(Application);
    ShowForm(frmModifySelection);
  end
  else
    frmModifySelection.BringToFront;
end;

// -----------------------------------------------------------------------------

procedure ShowTranslate;
begin
  if frmTranslate = NIL then
  begin
    frmTranslate := TfrmTranslate.Create(Application);
    ShowForm(frmTranslate);
  end
  else
    frmTranslate.BringToFront;
end;

// -----------------------------------------------------------------------------

procedure ShowWaveExtractor;
begin
  if frmWaveExtractor = NIL then
  begin
    frmWaveExtractor := TfrmWaveExtractor.Create(Application);
    ShowForm(frmWaveExtractor);
  end
  else
    frmWaveExtractor.BringToFront;
end;

// -----------------------------------------------------------------------------

procedure ShowSettings;
var
  OldLang: String;
  OldShiftMs: Integer;
  OldHandleByMPV: Boolean;
begin
  if frmSettings = NIL then
  begin
    OldLang        := AppOptions.Language;
    OldShiftMs     := AppOptions.ShiftTimeMS;
    OldHandleByMPV := MPVOptions.SubtitleHandleByMPV;

    frmSettings := TfrmSettings.Create(Application);
    frmSettings.ShowModal;
    if OldLang <> AppOptions.Language then
    begin
      LoadLanguage(frmMain);
      UpdateCommonActionString;
      UpdateVideoLengthString;
      RefreshAppTitle;

      with frmMain do
      begin
        VSTDrawInitialize(VSTOptions.DrawMode);
        VSTResize(NIL);
      end;
    end
    else if OldShiftMs <> AppOptions.ShiftTimeMS then
      UpdateCommonActionString;

    if frmMain.MPV.IsMediaLoaded then
    begin
      if (OldHandleByMPV <> MPVOptions.SubtitleHandleByMPV) then
      begin
        if MPVOptions.SubtitleHandleByMPV then
        begin
          frmMain.MPV.ShowText('', '');
          if MPVSaveSubtitleTempTrack then
            MPVLoadSubtitleTempTrack;
        end
        else
        begin
          MPVRemoveSubtitleTempTrack;
          MPVDeleteSubtitleTempTrack;
        end;
      end
      else if MPVOptions.SubtitleHandleByMPV then
        MPVReloadSubtitleTempTrack(True);
    end;

    DoAutoCheckErrors;
  end
  else
    frmSettings.BringToFront;
end;

// -----------------------------------------------------------------------------

procedure ShowFormatProperties;
begin
  if frmFormatProperties = NIL then
  begin
    frmFormatProperties := TfrmFormatProperties.Create(Application);
    ShowForm(frmFormatProperties);
  end
  else
    frmFormatProperties.BringToFront;
end;

// -----------------------------------------------------------------------------

procedure ShowRestoreBackup;
begin
  if frmRestoreBackup = NIL then
  begin
    frmRestoreBackup := TfrmRestoreBackup.Create(Application);
    ShowForm(frmRestoreBackup);
  end
  else
    frmRestoreBackup.BringToFront;
end;

// -----------------------------------------------------------------------------

procedure ShowFixSubtitles;
begin
  if frmFixSubtitles = NIL then
  begin
    frmFixSubtitles := TfrmFixSubtitles.Create(Application);
    ShowForm(frmFixSubtitles);
  end
  else
    frmFixSubtitles.BringToFront;
end;

// -----------------------------------------------------------------------------

procedure ShowTranslationMemory;
begin
  if frmTranslationMemory = NIL then
  begin
    frmTranslationMemory := TfrmTranslationMemory.Create(Application);
    ShowForm(frmTranslationMemory, False);
  end
  else
    frmTranslationMemory.BringToFront;

  frmMain.tlbValidate.Visible := TMX.Ready;
end;

// -----------------------------------------------------------------------------

procedure ShowTranslationMemorySettings;
begin
  if frmTranslationMemorySettings = NIL then
  begin
    frmTranslationMemorySettings := TfrmTranslationMemorySettings.Create(Application);
    ShowForm(frmTranslationMemorySettings);
    if TMX.Ready then
      with frmMain do
      begin
        actTranslationMemoryClose.Enabled := True;
        actTranslationMemory.Enabled      := True;
        actTranslationMemoryList.Enabled  := True;
      end;
  end
  else
    frmTranslationMemorySettings.BringToFront;
end;

// -----------------------------------------------------------------------------

procedure ShowTranslationMemoryList;
begin
  if frmTranslationMemoryList = NIL then
  begin
    frmTranslationMemoryList := TfrmTranslationMemoryList.Create(Application);
    ShowForm(frmTranslationMemoryList);
  end
  else
    frmTranslationMemoryList.BringToFront;
end;

// -----------------------------------------------------------------------------

procedure ShowSpellCheck;
begin
  if frmSpellCheck = NIL then
  begin
    frmSpellCheck := TfrmSpellCheck.Create(Application);
    ShowForm(frmSpellCheck);
  end
  else
    frmSpellCheck.BringToFront;
end;

// -----------------------------------------------------------------------------

procedure ShowGoToTime;
begin
  if frmGoToTime = NIL then
  begin
    frmGoToTime := TfrmGoToTime.Create(Application);
    ShowForm(frmGoToTime);
  end
  else
    frmGoToTime.BringToFront;
end;

// -----------------------------------------------------------------------------

procedure ShowActors;
begin
  if frmActors = NIL then
  begin
    frmActors := TfrmActors.Create(Application);
    ShowForm(frmActors);
  end
  else
    frmActors.BringToFront;
end;

// -----------------------------------------------------------------------------

procedure ShowShotChanges;
begin
  if frmShotChanges = NIL then
  begin
    frmShotChanges := TfrmShotChanges.Create(Application);
    ShowForm(frmShotChanges);
  end
  else
    frmShotChanges.BringToFront;
end;

// -----------------------------------------------------------------------------

procedure ShowAudioToText;
begin
  if frmAudioToText = NIL then
  begin
    frmAudioToText := TfrmAudioToText.Create(Application);
    ShowForm(frmAudioToText);
  end
  else
    frmAudioToText.BringToFront;
end;

// -----------------------------------------------------------------------------

procedure ShowAudioToTextModels;
begin
  if frmAudioToTextModel = NIL then
  begin
    frmAudioToTextModel := TfrmAudioToTextModel.Create(Application);
    ShowForm(frmAudioToTextModel);
  end
  else
    frmAudioToTextModel.BringToFront;
end;

// -----------------------------------------------------------------------------

procedure ShowBatchConvert;
begin
  if frmBatchConvert = NIL then
  begin
    frmBatchConvert := TfrmBatchConvert.Create(Application);
    ShowForm(frmBatchConvert);
  end
  else
    frmBatchConvert.BringToFront;
end;

// -----------------------------------------------------------------------------

procedure ShowQualityCheck;
begin
  if frmQualityCheck = NIL then
  begin
    frmQualityCheck := TfrmQualityCheck.Create(Application);
    ShowForm(frmQualityCheck);
  end
  else
    frmQualityCheck.BringToFront;
end;

// -----------------------------------------------------------------------------

procedure ShowCompare;
begin
  if frmCompare = NIL then
  begin
    frmCompare := TfrmCompare.Create(Application);
    ShowForm(frmCompare);
  end
  else
    frmCompare.BringToFront;
end;

// -----------------------------------------------------------------------------

procedure ShowCustomTextFormat;
begin
  if frmCustomTextFormat = NIL then
  begin
    frmCustomTextFormat := TfrmCustomTextFormat.Create(Application);
    ShowForm(frmCustomTextFormat);
  end
  else
    frmCustomTextFormat.BringToFront;
end;

// -----------------------------------------------------------------------------

procedure ShowTBXSettings;
begin
  if frmTBXSettings = NIL then
  begin
    frmTBXSettings := TfrmTBXSettings.Create(Application);
    ShowForm(frmTBXSettings);
    if TBX.Ready then
      with frmMain do
      begin
        actTBXClose.Enabled := True;
        actTBXEdit.Enabled  := True;
        actTBXList.Enabled  := True;
        actTBX.Enabled      := True;
      end;
  end
  else
    frmTBXSettings.BringToFront;
end;

// -----------------------------------------------------------------------------

procedure ShowTBX;
begin
  if frmTBX = NIL then
  begin
    frmTBX := TfrmTBX.Create(Application);
    ShowForm(frmTBX, False);
  end
  else
    frmTBX.BringToFront;
end;

// -----------------------------------------------------------------------------

procedure ShowTBXList;
begin
  if frmTBXList = NIL then
  begin
    frmTBXList := TfrmTBXList.Create(Application);
    ShowForm(frmTBXList);
  end
  else
    frmTBXList.BringToFront;
end;

// -----------------------------------------------------------------------------

procedure ShowTBXEdit;
begin
  if frmTBXEdit = NIL then
  begin
    frmTBXEdit := TfrmTBXEdit.Create(Application);
    ShowForm(frmTBXEdit);
  end
  else
    frmTBXEdit.BringToFront;
end;

// -----------------------------------------------------------------------------

{ Close }

// -----------------------------------------------------------------------------

procedure CloseTranslationMemory;
begin
  TMX.Close;
  if frmTranslationMemory <> NIL then
    frmTranslationMemory.Close;

  if frmTranslationMemoryList <> NIL then
    frmTranslationMemoryList.Close;

  with frmMain do
  begin
    tlbValidate.Hide;
    actTranslationMemoryClose.Enabled := False;
    actTranslationMemory.Enabled      := False;
    actTranslationMemoryList.Enabled  := False;
  end;
end;

// -----------------------------------------------------------------------------

procedure CloseTBX;
begin
  TBX.Close;
  if frmTBX <> NIL then
    frmTBX.Close;

  if frmTBXList <> NIL then
    frmTBXList.Close;

  with frmMain do
  begin
    actTBXClose.Enabled := False;
    actTBXEdit.Enabled  := False;
    actTBXList.Enabled  := False;
    actTBX.Enabled      := False;
  end;
end;

// -----------------------------------------------------------------------------

end.
