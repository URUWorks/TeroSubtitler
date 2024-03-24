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
  {$IF DEFINED(LCLGTK2) or DEFINED(LCLGTK3)}
  procXLib,
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
  formAudioToText, formAudioToTextModel, formTBX, formFormatProperties,
  procFileTypes, formCustomTextFormat, formActors, formCustomSelectDlg,
  procProjectFile, formCustomImageFormat, procCustomFormat, procDialogs,
  formExportSUP, procSUP, formGenerateVideo, procGenerateVideo, procLocalize,
  formCharacterMap, formRoundTime, formStatistics, formGenerateBlankVideo,
  procThumbnails, formStreamExtractor, formPauses,
  formCustomSelectDlgWithPreview, formTTS, formVideoDubbing;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='Tero Subtitler';
  Application.Scaled:=True;
  Application.Initialize;
  Application.ShowMainForm := False;
  Application.CreateForm(TfrmMain, frmMain);
  InitializeApp;
  Application.Run;
end.

