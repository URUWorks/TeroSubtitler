program tero;

{$mode objfpc}{$H+}

{$IFOPT D+}
  {$IFDEF WINDOWS}
    {$APPTYPE CONSOLE}
  {$ENDIF}
{$ENDIF}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, formMain, formWaveform, formVideo, procUndo,
  formFindAndReplace, formGoTo, formDurationLimits, formAutomaticDurations,
  formTimeExpander, formDelay, formConvertCase, formAdjustSubtitle,
  formMultipleReplace, formModifySelection, formCustomFileDlg, formAbout,
  formTranslate, formWaveExtractor, formSettings, formRestoreBackup,
  formFixSubtitles, formWelcome, formTranslationMemory,
  formTranslationMemorySettings, formSpellCheck, formCustomInputDlg,
  formGoToTime, formCustomQuestionDlg, formShotChanges, formBatchConvert,
  formQualityCheck, procQualityCheck, formCompare, procFixSubtitles, procFiles,
  procForms, procMPV, procSubtitle, procVST_Loops, formConventions,
  formTranslationMemoryList, formWizard, formDownload, procMRU, formShiftTimes,
  formAudioToText, formAudioToTextModel, formTBX;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='Tero Subtitler';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

