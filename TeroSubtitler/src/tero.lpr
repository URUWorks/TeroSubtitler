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
  formExportSUP, formGenerateVideo, procGenerateVideo, procLocalize,
  formCharacterMap, formRoundTime, formStatistics, formGenerateBlankVideo,
  procThumbnails, formStreamExtractor, formPauses,
  formCustomSelectDlgWithPreview, formTTS, formVideoDubbing, formImportSUP,
  formImportSUPLanguages, procMacOpenFiles, procMacMediaKeys, procMacMenuKeys;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='Tero Subtitler';
  Application.Scaled:=True;
  Application.Initialize;
  {$IFDEF WINDOWS}
  // the taskbar button belongs to the main form instead of the hidden
  // application window, so Windows shows it on the taskbar of the monitor
  // where the window is (Show taskbar buttons on: taskbar where window is open)
  Application.MainFormOnTaskBar := True;
  {$ENDIF}
  {$IFDEF DARWIN}
  // Let macOS use the bundle icon (tero.icns). Otherwise the LCL picks the
  // 48x48 image from MAINICON (closest to SM_CXICON = 128) for the Dock.
  Application.Icon.Clear;
  // files dropped on the app icon on macOS < 10.13 (Lazarus 4+)
  InstallMacOpenFilesFix;
  {$ENDIF}
  Application.ShowMainForm := False;
  Application.CreateForm(TfrmMain, frmMain);
  InitializeApp;
  {$IFDEF DARWIN}
  // media keys (Control Center) on macOS 10.12.2+
  MacMediaKeysInitialize;
  // main menu shortcuts through NSMenu, so the top menu flashes
  MacMenuKeysInstall(frmMain);
  {$ENDIF}
  Application.Run;
end.

