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

unit formMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  StdCtrls, ExtCtrls, Types, UWLayout, WAVDisplayer, UWTimeEdit, UWMemo,
  UWFlatButton, UWSeekBar, UWStatusBar, UWSubtitleAPI, BGRABitmap,
  BGRABitmapTypes, UWSystem.TimeUtils, LCLIntf, LCLType, ActnList, Buttons,
  laz.VirtualTrees, MPVPlayer, UWSubtitleAPI.Formats, procTypes, procUndo;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actFind: TAction;
    actFindNext: TAction;
    actFindPrevious: TAction;
    actAbout: TAction;
    actInsertSubtitleBefore: TAction;
    actInsertSubtitle: TAction;
    actDeleteSubtitle: TAction;
    actFontBold: TAction;
    actFontItalic: TAction;
    actFontUnderline: TAction;
    actFontStrikeout: TAction;
    actFontClear: TAction;
    actAlignToLeft: TAction;
    actAlignToCenter: TAction;
    actAlignToRight: TAction;
    actAlignToNone: TAction;
    actCombineSubtitles: TAction;
    actCloseVideo: TAction;
    actFixSubtitles: TAction;
    actFontColorDlg: TAction;
    actFontColor: TAction;
    actInsertCurrentTime: TAction;
    actGoToTime: TAction;
    actBatchConvert: TAction;
    actCompare: TAction;
    actInsertShotChange: TAction;
    actDeleteShotChange: TAction;
    actCenterWaveform: TAction;
    actDetectSilentZones: TAction;
    actExportMarkedSubtitles: TAction;
    actImportSubtitles: TAction;
    actExportTextOnly: TAction;
    actAddQuotationMarks: TAction;
    actAudioToText: TAction;
    actCheckForUpdates: TAction;
    actFormatProperties: TAction;
    actExportCustomTextFormat: TAction;
    actActor: TAction;
    actExportCustomImageFormat: TAction;
    actDetectDialogSegments: TAction;
    actWebPreview: TAction;
    actShowActorOnPreview: TAction;
    actRoundTimes: TAction;
    actJumpToNextShotChange: TAction;
    actJumpToPrevShotChange: TAction;
    actSaveProject: TAction;
    actLoadProject: TAction;
    actMediaMoveSubtitle: TAction;
    actLoadTranslation: TAction;
    actSort: TAction;
    actTBX: TAction;
    actMediaBack1Sec: TAction;
    actMediaForward1Sec: TAction;
    actMediaBack1Min: TAction;
    actMediaForward1Min: TAction;
    actSelectCurrentToBeginning: TAction;
    actSelectCurrentToEnd: TAction;
    actShiftTimes: TAction;
    actShowToolbarMain: TAction;
    actShowToolbarEncoding: TAction;
    actShowToolbarFormat: TAction;
    actShowToolbarFPS: TAction;
    actMediaVolumeMute: TAction;
    actMediaVolumeDown: TAction;
    actMediaVolumeUp: TAction;
    actMediaSubSizeInc: TAction;
    actMediaSubSizeDec: TAction;
    actTBXEdit: TAction;
    actTBXClose: TAction;
    actTBXList: TAction;
    actTBXSettings: TAction;
    actSetAsDefaultEncoding: TAction;
    actSetAsDefaultFormat: TAction;
    actTranslationMemoryList: TAction;
    actViewShotChange: TAction;
    actSwapWorkspace: TAction;
    actMediaDeleteSubtitle: TAction;
    actQualityCheck: TAction;
    actShotChanges: TAction;
    actLoadTranscription: TAction;
    actSaveTranscription: TAction;
    actTranslationMemoryClose: TAction;
    actTranslationMemory: TAction;
    actSpellCheck: TAction;
    actValidateTM: TAction;
    actTM1: TAction;
    actTM2: TAction;
    actTM3: TAction;
    actTranslationMemorySettings: TAction;
    actRestoreBackup: TAction;
    actSettings: TAction;
    actWaveExtract: TAction;
    actInvertSelection: TAction;
    actDurationLimits: TAction;
    actAutomaticDuration: TAction;
    actConvertCase: TAction;
    actFixRTLPunctuation: TAction;
    actDivideSubtitle: TAction;
    actAdjustSubtitle: TAction;
    actEndCueAddOneFrame: TAction;
    actEndCueSubtractOneFrame: TAction;
    actExtendLengthToPrevious: TAction;
    actExtendLengthToNext: TAction;
    actAddNote: TAction;
    actHelp: TAction;
    actTranslate: TAction;
    actLoadVideoFromURL: TAction;
    actSplitSubtitleAtPosition: TAction;
    actWebReference: TAction;
    actShowColumnCPL: TAction;
    actShowColumnWPM: TAction;
    actShowColumnCPS: TAction;
    actShowColumnDuration: TAction;
    actShowColumnTimes: TAction;
    actShowColumnNumber: TAction;
    actShowColumnStyleAndActor: TAction;
    actSwapTexts: TAction;
    actSetAutomaticDuration: TAction;
    actSetDefaultGap: TAction;
    actReadTextsFromFile: TAction;
    actReadTimingsFromFile: TAction;
    actModifySelection: TAction;
    actMultipleReplace: TAction;
    actJumpToPreviousMarked: TAction;
    actJumpToNextMarked: TAction;
    actMarkSubtitle: TAction;
    actUnMarkSubtitle: TAction;
    actReverseText: TAction;
    actSetMaximumLineLength: TAction;
    actUnbreakSubtitle: TAction;
    actShiftTimeLess: TAction;
    actShiftTimeMore: TAction;
    actSetDelay: TAction;
    actTimeExpander: TAction;
    actSaveTranslation: TAction;
    actSaveSubtitle: TAction;
    actLoadVideo: TAction;
    actSelectAll: TAction;
    actAutoBreakSubtitle: TAction;
    actMediaZoomSelection: TAction;
    actMediaZoomOut: TAction;
    actMediaZoomIn: TAction;
    actMediaAddSubtitle: TAction;
    actMediaPlayAfterSelection: TAction;
    actMediaPlayBeforeSelection: TAction;
    actMediaPlayFromSelection: TAction;
    actMediaPlaySelection: TAction;
    actMediaEndSubtitle: TAction;
    actMediaStartSubtitle: TAction;
    actMediaSetFinalTime: TAction;
    actMediaSetInitialTime: TAction;
    actMediaNextFrame: TAction;
    actMediaPreviousFrame: TAction;
    actMediaForward: TAction;
    actMediaRewind: TAction;
    actMediaChangePlayRate: TAction;
    actMediaAutoScroll: TAction;
    actMediaStop: TAction;
    actMediaPlay: TAction;
    actVAlignToTop: TAction;
    actVAlignToMiddle: TAction;
    actVAlignToBottom: TAction;
    actNextSubtitle: TAction;
    actPreviousSubtitle: TAction;
    actListMode: TAction;
    actTranscriptionMode: TAction;
    actTimeMode: TAction;
    actFramesMode: TAction;
    actSourceMode: TAction;
    actUnDockWaveform: TAction;
    actVideoPreview: TAction;
    actWaveformPreview: TAction;
    actTranslatorMode: TAction;
    actUnDockVideo: TAction;
    actReplace: TAction;
    actGoTo: TAction;
    actUndo: TAction;
    actRedo: TAction;
    actCut: TAction;
    actCopy: TAction;
    actPaste: TAction;
    actLoadSubtitle: TAction;
    actSaveSubtitleAs: TAction;
    actSaveTranslationAs: TAction;
    actCloseSubtitle: TAction;
    actCloseApp: TAction;
    actNewSubtitle: TAction;
    ActionList: TActionList;
    btnActor: TUWFlatButton;
    btnFinalTime: TUWFlatButton;
    btnDuration: TUWFlatButton;
    btnPause: TUWFlatButton;
    btnInitialTime: TUWFlatButton;
    cboActor: TComboBox;
    cboFPS: TComboBox;
    cboFormat: TComboBox;
    cboEncoding: TComboBox;
    cboInputFPS: TComboBox;
    CoolBarMain: TCoolBar;
    ImageList_ET_Default: TImageList;
    ImageList_ET_Dark: TImageList;
    ImageList_Default: TImageList;
    ImageList_Dark: TImageList;
    lblSeparatorFPS: TLabel;
    LayoutEditor: TUWLayout;
    LayoutEditorTimes: TUWLayout;
    LayoutSource: TUWLayout;
    LayoutSubtitles: TUWLayout;
    LayoutTranscription: TUWLayout;
    LayoutVST: TUWLayout;
    lblMediaLength: TLabel;
    lblMediaTime: TLabel;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem100: TMenuItem;
    MenuItem101: TMenuItem;
    MenuItem102: TMenuItem;
    MenuItem103: TMenuItem;
    MenuItem104: TMenuItem;
    MenuItem105: TMenuItem;
    MenuItem106: TMenuItem;
    MenuItem107: TMenuItem;
    MenuItem108: TMenuItem;
    MenuItem109: TMenuItem;
    MenuItem112: TMenuItem;
    MenuItem134: TMenuItem;
    MenuItem135: TMenuItem;
    MenuItem136: TMenuItem;
    MenuItem137: TMenuItem;
    MenuItem138: TMenuItem;
    MenuItem139: TMenuItem;
    MenuItem140: TMenuItem;
    MenuItem141: TMenuItem;
    MenuItem142: TMenuItem;
    MenuItem143: TMenuItem;
    MenuItem144: TMenuItem;
    MenuItem145: TMenuItem;
    MenuItem146: TMenuItem;
    MenuItem147: TMenuItem;
    MenuItem148: TMenuItem;
    MenuItem149: TMenuItem;
    MenuItem150: TMenuItem;
    MenuItem151: TMenuItem;
    MenuItem152: TMenuItem;
    MenuItem153: TMenuItem;
    MenuItem154: TMenuItem;
    MenuItem155: TMenuItem;
    MenuItem156: TMenuItem;
    MenuItem157: TMenuItem;
    MenuItem158: TMenuItem;
    MenuItem159: TMenuItem;
    MenuItem160: TMenuItem;
    MenuItem161: TMenuItem;
    MenuItem162: TMenuItem;
    MenuItem163: TMenuItem;
    MenuItem164: TMenuItem;
    MenuItem165: TMenuItem;
    MenuItem166: TMenuItem;
    MenuItem167: TMenuItem;
    MenuItem168: TMenuItem;
    MenuItem169: TMenuItem;
    MenuItem170: TMenuItem;
    MenuItem171: TMenuItem;
    MenuItem172: TMenuItem;
    MenuItem173: TMenuItem;
    MenuItem174: TMenuItem;
    MenuItem175: TMenuItem;
    MenuItem176: TMenuItem;
    MenuItem177: TMenuItem;
    mnuVSTFormat: TMenuItem;
    mnuMemoFormat: TMenuItem;
    mnuFormatProperties: TMenuItem;
    mnuCheckForUpdates: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem52: TMenuItem;
    MenuItem69: TMenuItem;
    MenuItem70: TMenuItem;
    mnuSelect: TMenuItem;
    mnuViewToolbars: TMenuItem;
    mnuVideoVolume: TMenuItem;
    mnuToolsTerminology: TMenuItem;
    mnuMemoInsertSymbol: TMenuItem;
    mnuVSTInsertSymbol: TMenuItem;
    mnuEditInsertSymbol: TMenuItem;
    MenuItem8: TMenuItem;
    mnuAbout: TMenuItem;
    MenuItem110: TMenuItem;
    MenuItem111: TMenuItem;
    mnuSettings: TMenuItem;
    MenuItem113: TMenuItem;
    MenuItem114: TMenuItem;
    MenuItem115: TMenuItem;
    MenuItem124: TMenuItem;
    MenuItem125: TMenuItem;
    mnuTranscriptionInsertTime: TMenuItem;
    MenuItem126: TMenuItem;
    MenuItem127: TMenuItem;
    MenuItem128: TMenuItem;
    MenuItem129: TMenuItem;
    MenuItem130: TMenuItem;
    MenuItem131: TMenuItem;
    MenuItem132: TMenuItem;
    MenuItem133: TMenuItem;
    mnuFileImport: TMenuItem;
    mnuFileExport: TMenuItem;
    mnuViewWindows: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    mnuToolsTranslationMemory: TMenuItem;
    MenuItem116: TMenuItem;
    MenuItem117: TMenuItem;
    MenuItem118: TMenuItem;
    MenuItem119: TMenuItem;
    MenuItem120: TMenuItem;
    MenuItem121: TMenuItem;
    MenuItem122: TMenuItem;
    MenuItem123: TMenuItem;
    mnuEditFormat: TMenuItem;
    mnuAddNote: TMenuItem;
    mnuFileMRU: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
    MenuItem43: TMenuItem;
    MenuItem44: TMenuItem;
    MenuItem45: TMenuItem;
    MenuItem46: TMenuItem;
    MenuItem47: TMenuItem;
    MenuItem48: TMenuItem;
    MenuItem49: TMenuItem;
    MenuItem50: TMenuItem;
    MenuItem51: TMenuItem;
    MenuItem53: TMenuItem;
    MenuItem54: TMenuItem;
    MenuItem55: TMenuItem;
    MenuItem56: TMenuItem;
    MenuItem57: TMenuItem;
    MenuItem58: TMenuItem;
    MenuItem59: TMenuItem;
    MenuItem60: TMenuItem;
    MenuItem61: TMenuItem;
    MenuItem62: TMenuItem;
    MenuItem63: TMenuItem;
    MenuItem64: TMenuItem;
    MenuItem65: TMenuItem;
    MenuItem66: TMenuItem;
    MenuItem67: TMenuItem;
    MenuItem68: TMenuItem;
    MenuItem71: TMenuItem;
    MenuItem72: TMenuItem;
    MenuItem73: TMenuItem;
    MenuItem74: TMenuItem;
    MenuItem75: TMenuItem;
    MenuItem76: TMenuItem;
    MenuItem77: TMenuItem;
    MenuItem78: TMenuItem;
    MenuItem79: TMenuItem;
    MenuItem80: TMenuItem;
    MenuItem81: TMenuItem;
    MenuItem82: TMenuItem;
    MenuItem83: TMenuItem;
    MenuItem84: TMenuItem;
    MenuItem85: TMenuItem;
    MenuItem86: TMenuItem;
    MenuItem87: TMenuItem;
    MenuItem88: TMenuItem;
    MenuItem89: TMenuItem;
    MenuItem90: TMenuItem;
    MenuItem91: TMenuItem;
    MenuItem92: TMenuItem;
    MenuItem93: TMenuItem;
    MenuItem94: TMenuItem;
    MenuItem95: TMenuItem;
    MenuItem96: TMenuItem;
    MenuItem97: TMenuItem;
    MenuItem98: TMenuItem;
    MenuItem99: TMenuItem;
    mnuDictionary: TMenuItem;
    mnuHunspellSeparator: TMenuItem;
    mnuViewColumns: TMenuItem;
    mnuEditTimingsExtend: TMenuItem;
    mnuEditTimingsEndCue: TMenuItem;
    mnuEditSubtitles: TMenuItem;
    mnuEditRTL: TMenuItem;
    mnuEditTexts: TMenuItem;
    mnuEditTimings: TMenuItem;
    mnuVideoPlayback: TMenuItem;
    mnuVideoAudio: TMenuItem;
    mnuVideoSubtitles: TMenuItem;
    MenuItem32: TMenuItem;
    popLoopCount: TPopupMenu;
    popMemo: TPopupMenu;
    popTranscription: TPopupMenu;
    popSilentZones: TPopupMenu;
    PopupMenu1: TPopupMenu;
    popWAVE: TPopupMenu;
    popVideo: TPopupMenu;
    popVST: TPopupMenu;
    popVSTHeader: TPopupMenu;
    Separator10: TMenuItem;
    Separator11: TMenuItem;
    Separator12: TMenuItem;
    Separator13: TMenuItem;
    Separator14: TMenuItem;
    Separator15: TMenuItem;
    Separator16: TMenuItem;
    Separator17: TMenuItem;
    Separator18: TMenuItem;
    Separator19: TMenuItem;
    Separator20: TMenuItem;
    Separator21: TMenuItem;
    Separator22: TMenuItem;
    mnuNoteSeparator: TMenuItem;
    Separator23: TMenuItem;
    Separator24: TMenuItem;
    Separator25: TMenuItem;
    Separator26: TMenuItem;
    Separator27: TMenuItem;
    Separator28: TMenuItem;
    Separator29: TMenuItem;
    Separator30: TMenuItem;
    Separator31: TMenuItem;
    Separator32: TMenuItem;
    Separator33: TMenuItem;
    Separator34: TMenuItem;
    Separator35: TMenuItem;
    Separator36: TMenuItem;
    Separator37: TMenuItem;
    Separator38: TMenuItem;
    Separator39: TMenuItem;
    Separator40: TMenuItem;
    Separator41: TMenuItem;
    Separator42: TMenuItem;
    Separator43: TMenuItem;
    Separator44: TMenuItem;
    Separator45: TMenuItem;
    Separator46: TMenuItem;
    mnuHelpSeparator: TMenuItem;
    Separator47: TMenuItem;
    Separator48: TMenuItem;
    Separator49: TMenuItem;
    Separator50: TMenuItem;
    Separator8: TMenuItem;
    Separator9: TMenuItem;
    Separator7: TMenuItem;
    Separator6: TMenuItem;
    Separator5: TMenuItem;
    Separator4: TMenuItem;
    Separator3: TMenuItem;
    Separator2: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    mnuExit: TMenuItem;
    MenuItem9: TMenuItem;
    Separator1: TMenuItem;
    mnuFile: TMenuItem;
    mnuEdit: TMenuItem;
    mnuFind: TMenuItem;
    mnuTools: TMenuItem;
    mnuView: TMenuItem;
    mnuVideo: TMenuItem;
    mnuHelp: TMenuItem;
    mmoSourceView: TMemo;
    mmoText: TUWMemo;
    mmoTranslation: TUWMemo;
    MPV: TMPVPlayer;
    popDropInsert: TPopupMenu;
    popMRU: TPopupMenu;
    popPlayRate: TPopupMenu;
    SplitterVideo: TSplitter;
    SplitterWaveform: TSplitter;
    StatusBar: TUWStatusBar;
    tedFinal: TUWTimeEdit;
    tedDuration: TUWTimeEdit;
    tedPause: TUWTimeEdit;
    tedInitial: TUWTimeEdit;
    TimerStatus: TTimer;
    TimerAutoBackup: TTimer;
    TimerSubtitle: TTimer;
    TimerWaveform: TTimer;
    ToolBarEncoding: TToolBar;
    ToolBarFPS: TToolBar;
    ToolBarFormat: TToolBar;
    ToolBarEditor: TToolBar;
    ToolBarVideo: TToolBar;
    ToolBarWaveform: TToolBar;
    ToolBarMain: TToolBar;
    tlb0: TToolButton;
    LayoutWaveform: TUWLayout;
    LayoutVideo: TUWLayout;
    LayoutVideoControls: TUWLayout;
    tlb7: TToolButton;
    tlbdiv2: TToolButton;
    tlb8: TToolButton;
    tlb10: TToolButton;
    tlb11: TToolButton;
    tlb12: TToolButton;
    tlb13: TToolButton;
    tlbdiv4: TToolButton;
    tlb14: TToolButton;
    tlb1: TToolButton;
    tlb15: TToolButton;
    tlb16: TToolButton;
    tlbdiv5: TToolButton;
    tlb17: TToolButton;
    tlb18: TToolButton;
    etlb0: TToolButton;
    etlb1: TToolButton;
    etlbdiv0: TToolButton;
    etlb2: TToolButton;
    etlb3: TToolButton;
    tlb2: TToolButton;
    etlb4: TToolButton;
    etlb7: TToolButton;
    etlbdiv1: TToolButton;
    etlb8: TToolButton;
    etlb9: TToolButton;
    etlb10: TToolButton;
    etlb11: TToolButton;
    etlbdiv2: TToolButton;
    etlb12: TToolButton;
    etlb13: TToolButton;
    tlbdiv0: TToolButton;
    etlb14: TToolButton;
    etlb6: TToolButton;
    wtlb0: TToolButton;
    wtlb1: TToolButton;
    wtlb2: TToolButton;
    wtlb3: TToolButton;
    wtlbdiv0: TToolButton;
    wtlb4: TToolButton;
    wtlbdiv1: TToolButton;
    wtlb5: TToolButton;
    wtlb6: TToolButton;
    tlb3: TToolButton;
    wtlb7: TToolButton;
    vtlb0: TToolButton;
    vtlb1: TToolButton;
    vtlbdiv0: TToolButton;
    vtlb2: TToolButton;
    vtlb3: TToolButton;
    tlbValidate: TToolButton;
    wtlb8: TToolButton;
    vtlbdiv1: TToolButton;
    vtlb4: TToolButton;
    vtlb5: TToolButton;
    vtlb6: TToolButton;
    tlb4: TToolButton;
    vtlb7: TToolButton;
    vtlbdiv2: TToolButton;
    vtlb8: TToolButton;
    vtlb9: TToolButton;
    vtlb10: TToolButton;
    vtlb11: TToolButton;
    tlb9: TToolButton;
    etlbdiv3: TToolButton;
    etlb15: TToolButton;
    tlbdiv3: TToolButton;
    tlbdiv1: TToolButton;
    etlb5: TToolButton;
    tlb19: TToolButton;
    tlb20: TToolButton;
    wtlbdiv2: TToolButton;
    wtlb9: TToolButton;
    wtlb10: TToolButton;
    tlb5: TToolButton;
    tlb6: TToolButton;
    LayoutEditorClient: TUWLayout;
    sbrSeek: TUWSeekBar;
    VST: TLazVirtualStringTree;
    WAVE: TUWWaveformDisplayer;
    // formMain
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LayoutVSTResize(Sender: TObject);
    procedure tedTimeChange(Sender: TObject; const NewTime: Cardinal);
    procedure UndoChanged(const ChangeType: TUndoChangeType);
    procedure mmoTextChange(Sender: TObject);
    procedure mmoSourceViewChange(Sender: TObject);
    procedure cboInputFPSSelect(Sender: TObject);
    procedure cboFPSKeyPress(Sender: TObject; var Key: char);
    procedure cboFPSSelect(Sender: TObject);
    procedure cboFormatSelect(Sender: TObject);
    procedure cboActorChange(Sender: TObject);
    procedure cboActorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MRUItemClick(Sender: TObject);
    procedure MRUOnChange(Sender: TObject);
    procedure PlayRateClick(Sender: TObject);
    procedure LoopCountClick(Sender: TObject);
    procedure DoAudioTrackSet(Sender: TObject);
    procedure DoHunspellItemClick(Sender: TObject);
    procedure DoDictionaryItemClick(Sender: TObject);
    procedure DoUnicodeSymbolClick(Sender: TObject);
    procedure DoSilentZoneClick(Sender: TObject);
    procedure DoMemoPopup(Sender: TObject);
    procedure DoVSTPopup(Sender: TObject);
    procedure DoTranscriptionPopup(Sender: TObject);
    procedure DoWAVEPopup(Sender: TObject);
    procedure DoAutoBackupTimer(Sender: TObject);
    procedure DoStatusTimer(Sender: TObject);
    // Memo (ATSynEdit)
    procedure MemoClickLink(Sender: TObject; const ALink: String);
    procedure MemoContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure MemoChange(Sender: TObject);
    // formMain_VST
    procedure VSTDrawInitialize(const ADrawMode: TVSTDrawMode);
    procedure VSTDrawErrors(TargetCanvas: TCanvas; CellRect: TRect; AIndex: Integer; AX: Integer = 4; AW: Integer = 18; ALeftMargin: Integer = 4; ABottomMargin: Integer = 18);
    procedure VSTDrawValidateIcon(TargetCanvas: TCanvas; CellRect: TRect; AX, AY, AIndex: Integer);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure VSTBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure VSTAfterItemPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect);
    procedure VSTDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const CellText: String;
      const CellRect: TRect; var DefaultDraw: Boolean);
    procedure VSTBeforeCellPaint_Block(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure VSTResize(Sender: TObject);
    procedure VSTHeaderDrawQueryElements(Sender: TVTHeader;
      var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
    procedure VSTAdvancedHeaderDraw(Sender: TVTHeader;
      var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: String);
    procedure VSTDblClick(Sender: TObject);
    procedure VSTContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure WAVEContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    // formMain_WAVE
    procedure WAVESelectedSubtitleItem(Sender: TObject; const Index: Integer;
      const SubtitleItem: TUWSubtitleItem; const IsDynamic: Boolean);
    procedure WAVESelectedSubtitleItemChange(Sender: TObject);
    procedure WAVESelectedSubtitleItemChanged(Sender: TObject;
      const Index: Integer; const OldInitialTime, OldFinalTime: Integer;
      const NeedSort: Boolean);
    procedure WAVESelectionChange(Sender: TObject);
    procedure DoWaveformTimer(Sender: TObject);
    procedure WAVEClick(Sender: TObject);
    procedure WAVETimeLineClick(Sender: TObject; const Time: Integer);
    procedure GoToNextShotChange(const APrevious: Boolean = False);
    procedure DetectDialogSegments;
    // formMain_MPV
    procedure MPVClick(Sender: TObject);
    procedure MPVStartFile(Sender: TObject);
    procedure MPVFileLoaded(Sender: TObject);
    procedure MPVEndFile(ASender: TObject; AParam: Integer);
    procedure MPVPause(Sender: TObject);
    procedure MPVPlay(Sender: TObject);
    procedure MPVStop(Sender: TObject);
    procedure MPVAudioReconfig(Sender: TObject);
    procedure MPVTimeChanged(ASender: TObject; AParam: Integer);
    procedure MPVDoTimeChanged(const Time: Integer);
    procedure MPVMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure MPVMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure DoSubtitleTimer(Sender: TObject);
    procedure sbrSeekMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sbrSeekMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sbrSeekMouseLeave(Sender: TObject);
    procedure sbrSeekMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    // formMain_Actions
    procedure actAboutExecute(Sender: TObject);
    procedure actHelpExecute(Sender: TObject);
    procedure actCheckForUpdatesExecute(Sender: TObject);
    procedure actListModeExecute(Sender: TObject);
    procedure actSourceModeExecute(Sender: TObject);
    procedure actTranscriptionModeExecute(Sender: TObject);
    procedure actTranslatorModeExecute(Sender: TObject);
    procedure actVideoPreviewExecute(Sender: TObject);
    procedure actWaveformPreviewExecute(Sender: TObject);
    procedure actNewSubtitleExecute(Sender: TObject);
    procedure actLoadSubtitleExecute(Sender: TObject);
    procedure actLoadTranslationExecute(Sender: TObject);
    procedure actSaveSubtitleExecute(Sender: TObject);
    procedure actSaveSubtitleAsExecute(Sender: TObject);
    procedure actSaveTranslationExecute(Sender: TObject);
    procedure actSaveTranslationAsExecute(Sender: TObject);
    procedure actCloseAppExecute(Sender: TObject);
    procedure actCloseSubtitleExecute(Sender: TObject);
    procedure actUndoExecute(Sender: TObject);
    procedure actRedoExecute(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure actInsertSubtitleBeforeExecute(Sender: TObject);
    procedure actInsertSubtitleExecute(Sender: TObject);
    procedure actDeleteSubtitleExecute(Sender: TObject);
    procedure actFramesModeExecute(Sender: TObject);
    procedure actTimeModeExecute(Sender: TObject);
    procedure actUnDockVideoExecute(Sender: TObject);
    procedure actUnDockWaveformExecute(Sender: TObject);
    procedure actShowActorOnPreviewExecute(Sender: TObject);
    procedure actPreviousSubtitleExecute(Sender: TObject);
    procedure actNextSubtitleExecute(Sender: TObject);
    procedure actFontBoldExecute(Sender: TObject);
    procedure actFontItalicExecute(Sender: TObject);
    procedure actFontUnderlineExecute(Sender: TObject);
    procedure actFontStrikeoutExecute(Sender: TObject);
    procedure actFontClearExecute(Sender: TObject);
    procedure actFontColorDlgExecute(Sender: TObject);
    procedure actFontColorExecute(Sender: TObject);
    procedure actAddQuotationMarksExecute(Sender: TObject);
    procedure actAlignToLeftExecute(Sender: TObject);
    procedure actAlignToCenterExecute(Sender: TObject);
    procedure actAlignToRightExecute(Sender: TObject);
    procedure actAlignToNoneExecute(Sender: TObject);
    procedure actVAlignToBottomExecute(Sender: TObject);
    procedure actVAlignToMiddleExecute(Sender: TObject);
    procedure actVAlignToTopExecute(Sender: TObject);
    procedure actMediaPlayExecute(Sender: TObject);
    procedure actMediaStopExecute(Sender: TObject);
    procedure actMediaAutoScrollExecute(Sender: TObject);
    procedure actMediaChangePlayRateExecute(Sender: TObject);
    procedure actMediaRewindExecute(Sender: TObject);
    procedure actMediaForwardExecute(Sender: TObject);
    procedure actMediaPreviousFrameExecute(Sender: TObject);
    procedure actMediaNextFrameExecute(Sender: TObject);
    procedure actMediaBack1SecExecute(Sender: TObject);
    procedure actMediaForward1SecExecute(Sender: TObject);
    procedure actMediaBack1MinExecute(Sender: TObject);
    procedure actMediaForward1MinExecute(Sender: TObject);
    procedure actMediaMoveSubtitleExecute(Sender: TObject);
    procedure actMediaSetInitialTimeExecute(Sender: TObject);
    procedure actMediaSetFinalTimeExecute(Sender: TObject);
    procedure actMediaStartSubtitleExecute(Sender: TObject);
    procedure actMediaEndSubtitleExecute(Sender: TObject);
    procedure actMediaPlaySelectionExecute(Sender: TObject);
    procedure actMediaPlayFromSelectionExecute(Sender: TObject);
    procedure actMediaPlayBeforeSelectionExecute(Sender: TObject);
    procedure actMediaPlayAfterSelectionExecute(Sender: TObject);
    procedure actMediaAddSubtitleExecute(Sender: TObject);
    procedure actMediaDeleteSubtitleExecute(Sender: TObject);
    procedure actMediaZoomInExecute(Sender: TObject);
    procedure actMediaZoomOutExecute(Sender: TObject);
    procedure actMediaZoomSelectionExecute(Sender: TObject);
    procedure actCombineSubtitlesExecute(Sender: TObject);
    procedure actAutoBreakSubtitleExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure actInvertSelectionExecute(Sender: TObject);
    procedure actSelectCurrentToBeginningExecute(Sender: TObject);
    procedure actSelectCurrentToEndExecute(Sender: TObject);
    procedure actModifySelectionExecute(Sender: TObject);
    procedure actLoadVideoExecute(Sender: TObject);
    procedure actCloseVideoExecute(Sender: TObject);
    procedure actFindExecute(Sender: TObject);
    procedure actFindNextExecute(Sender: TObject);
    procedure actFindPreviousExecute(Sender: TObject);
    procedure actReplaceExecute(Sender: TObject);
    procedure actMultipleReplaceExecute(Sender: TObject);
    procedure actGoToExecute(Sender: TObject);
    procedure actDurationLimitsExecute(Sender: TObject);
    procedure actAutomaticDurationExecute(Sender: TObject);
    procedure actTimeExpanderExecute(Sender: TObject);
    procedure actSetDelayExecute(Sender: TObject);
    procedure actShiftTimesExecute(Sender: TObject);
    procedure actShiftTimeMoreExecute(Sender: TObject);
    procedure actShiftTimeLessExecute(Sender: TObject);
    procedure actConvertCaseExecute(Sender: TObject);
    procedure actUnbreakSubtitleExecute(Sender: TObject);
    procedure actSetMaximumLineLengthExecute(Sender: TObject);
    procedure actReverseTextExecute(Sender: TObject);
    procedure actFixRTLPunctuationExecute(Sender: TObject);
    procedure actDivideSubtitleExecute(Sender: TObject);
    procedure actMarkSubtitleExecute(Sender: TObject);
    procedure actUnMarkSubtitleExecute(Sender: TObject);
    procedure actAdjustSubtitleExecute(Sender: TObject);
    procedure actJumpToNextMarkedExecute(Sender: TObject);
    procedure actJumpToPreviousMarkedExecute(Sender: TObject);
    procedure actEndCueAddOneFrameExecute(Sender: TObject);
    procedure actEndCueSubtractOneFrameExecute(Sender: TObject);
    procedure actRoundTimesExecute(Sender: TObject);
    procedure actReadTimingsFromFileExecute(Sender: TObject);
    procedure actReadTextsFromFileExecute(Sender: TObject);
    procedure actExtendLengthToNextExecute(Sender: TObject);
    procedure actExtendLengthToPreviousExecute(Sender: TObject);
    procedure actSetAutomaticDurationExecute(Sender: TObject);
    procedure actSetDefaultGapExecute(Sender: TObject);
    procedure actSwapTextsExecute(Sender: TObject);
    procedure actShowColumnNumberExecute(Sender: TObject);
    procedure actShowColumnTimesExecute(Sender: TObject);
    procedure actShowColumnDurationExecute(Sender: TObject);
    procedure actShowColumnCPSExecute(Sender: TObject);
    procedure actShowColumnWPMExecute(Sender: TObject);
    procedure actShowColumnCPLExecute(Sender: TObject);
    procedure actShowColumnStyleAndActorExecute(Sender: TObject);
    procedure actWebReferenceExecute(Sender: TObject);
    procedure actSplitSubtitleAtPositionExecute(Sender: TObject);
    procedure actAddNoteExecute(Sender: TObject);
    procedure actLoadVideoFromURLExecute(Sender: TObject);
    procedure actTranslateExecute(Sender: TObject);
    procedure actWaveExtractExecute(Sender: TObject);
    procedure actSettingsExecute(Sender: TObject);
    procedure actFormatPropertiesExecute(Sender: TObject);
    procedure actRestoreBackupExecute(Sender: TObject);
    procedure actFixSubtitlesExecute(Sender: TObject);
    procedure actTranslationMemorySettingsExecute(Sender: TObject);
    procedure actTranslationMemoryListExecute(Sender: TObject);
    procedure actTranslationMemoryExecute(Sender: TObject);
    procedure actTranslationMemoryCloseExecute(Sender: TObject);
    procedure actTM1Execute(Sender: TObject);
    procedure actTM2Execute(Sender: TObject);
    procedure actTM3Execute(Sender: TObject);
    procedure actValidateTMExecute(Sender: TObject);
    procedure actSpellCheckExecute(Sender: TObject);
    procedure actInsertCurrentTimeExecute(Sender: TObject);
    procedure actLoadTranscriptionExecute(Sender: TObject);
    procedure actSaveTranscriptionExecute(Sender: TObject);
    procedure actGoToTimeExecute(Sender: TObject);
    procedure actShotChangesExecute(Sender: TObject);
    procedure actAudioToTextExecute(Sender: TObject);
    procedure actBatchConvertExecute(Sender: TObject);
    procedure actQualityCheckExecute(Sender: TObject);
    procedure actCompareExecute(Sender: TObject);
    procedure actSwapWorkspaceExecute(Sender: TObject);
    procedure actInsertShotChangeExecute(Sender: TObject);
    procedure actDeleteShotChangeExecute(Sender: TObject);
    procedure actJumpToNextShotChangeExecute(Sender: TObject);
    procedure actJumpToPrevShotChangeExecute(Sender: TObject);
    procedure actViewShotChangeExecute(Sender: TObject);
    procedure actCenterWaveformExecute(Sender: TObject);
    procedure actSetAsDefaultEncodingExecute(Sender: TObject);
    procedure actSetAsDefaultFormatExecute(Sender: TObject);
    procedure actDetectSilentZonesExecute(Sender: TObject);
    procedure actDetectDialogSegmentsExecute(Sender: TObject);
    procedure actTBXExecute(Sender: TObject);
    procedure actTBXListExecute(Sender: TObject);
    procedure actTBXSettingsExecute(Sender: TObject);
    procedure actTBXCloseExecute(Sender: TObject);
    procedure actTBXEditExecute(Sender: TObject);
    procedure actExportMarkedSubtitlesExecute(Sender: TObject);
    procedure actExportTextOnlyExecute(Sender: TObject);
    procedure actImportSubtitlesExecute(Sender: TObject);
    procedure actMediaSubSizeDecExecute(Sender: TObject);
    procedure actMediaSubSizeIncExecute(Sender: TObject);
    procedure actMediaVolumeDownExecute(Sender: TObject);
    procedure actMediaVolumeMuteExecute(Sender: TObject);
    procedure actMediaVolumeUpExecute(Sender: TObject);
    procedure actShowToolbarMainExecute(Sender: TObject);
    procedure actShowToolbarFPSExecute(Sender: TObject);
    procedure actShowToolbarFormatExecute(Sender: TObject);
    procedure actShowToolbarEncodingExecute(Sender: TObject);
    procedure actSortExecute(Sender: TObject);
    procedure actExportCustomTextFormatExecute(Sender: TObject);
    procedure actExportCustomImageFormatExecute(Sender: TObject);
    procedure actActorExecute(Sender: TObject);
    procedure actLoadProjectExecute(Sender: TObject);
    procedure actSaveProjectExecute(Sender: TObject);
    procedure actWebPreviewExecute(Sender: TObject);
  private

  public

  end;

  { SubtileAPI OnLoadDataFunc }

  function SAPILoadDataFunc(const AData: Pointer; const ADataClass: ShortString): Integer;

var
  frmMain: TfrmMain;

// -----------------------------------------------------------------------------

implementation

uses
  FormVideo, UWSpellcheck.Hunspell, RegExpr, procShortCut, procTranscription,
  procConfig, procDialogs, procWorkspace, procVST, procVST_Loops,
  procUnDockVideoControls, procColorTheme, procFiles, procMPV, procSubtitle,
  procForms, UWSubtitleAPI.Tags, UWSubtitles.Utils, procMRU, UWSystem.XMLLang,
  UWSystem.SysUtils, UWSystem.StrUtils, UWSubtitleAPI.TMX, UWSubtitleAPI.TBX,
  formCustomQuestionDlg, formCustomSelectDlg;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmMain }

// -----------------------------------------------------------------------------

procedure TfrmMain.FormCreate(Sender: TObject);
{$IFDEF DARWIN}
var
  AppMenu, AboutMenu, SettingsMenu, //FormatPropertiesMenu,
  AppUpdate, SepMenu: TMenuItem;
{$ENDIF}
begin
  Randomize; // used for tips
  DoubleBuffered := True;
  // Initialize SubtitleAPI
  Subtitles := TUWSubtitles.Create;
  Subtitles.OnLoadDataFunc := @SAPILoadDataFunc;
  // Link subtitles to Waveform
  WAVE.EmptyText := '';
  WAVE.Subtitles := Subtitles;
  // Link undo event
  UndoInstance.OnChange := @UndoChanged;
  // Set default values
  DefaultValues;
  LoadSettings;
  LoadShortCutsFromFile(ShortCutFileName, ActionList);
  // Init and load language
  LanguageManager.SetLanguageFolder(LanguageFolder);
  LoadLanguage(Self, False);
  UpdateCommonActionString;
  RefreshAppTitle;
  // VST
  VSTDrawInitialize(VSTOptions.DrawMode);
  // Prepare combos and menus
  FillComboWithFPS(cboInputFPS, Workspace.FPS.InputFPS);
  FillComboWithFPS(cboFPS, Workspace.FPS.InputFPS);
  FillComboWithEncodings(cboEncoding);
  FillComboWithFormats(cboFormat);
  FillMenuWithPlayRate(popPlayRate);
  FillMenuWithLoopCount(popLoopCount);
  FillWithDictionaries(mnuDictionary, NIL);
  FillMenuWithUnicodeSymbols(mnuEditInsertSymbol);
  mnuVSTFormat.Assign(mnuEditFormat);
  mnuMemoFormat.Assign(mnuEditFormat);
  mnuVSTInsertSymbol.Assign(mnuEditInsertSymbol);
  mnuMemoInsertSymbol.Assign(mnuEditInsertSymbol);
  // Tag controls
  tedInitial.Tag     := TAG_CONTROL_INITIALTIME;
  tedFinal.Tag       := TAG_CONTROL_FINALTIME;
  tedDuration.Tag    := TAG_CONTROL_DURATION;
  tedPause.Tag       := TAG_CONTROL_PAUSE;
  mmoText.Tag        := TAG_CONTROL_TEXT;
  mmoTranslation.Tag := TAG_CONTROL_TRANSLATION;
  // MPV
  PrepareMPV;
  // MRU
  MRU := TMRU.Create(popMRU);
  MRU.OnMRUItemClick := @MRUItemClick;
  MRU.OnChange       := @MRUOnChange;
  MRU.LoadFromXML(MRUFileName);
  // Actors
  LoadActors;
  // TMX
  TMX := TUWTMX.Create('');
  // TBX
  TBX := TUWTBX.Create('');
  // Hunspell
  if not HunspellInstance.Ready then
    HunspellInstance.LoadHunspell(HunspellFileName);
  HunspellInstance.LoadDictionary(DictionariesFolder+AppOptions.HunspellLanguage+'.aff', DictionariesFolder+AppOptions.HunspellLanguage+'.dic');
  // prepare special folders
  ForceDirectories(WaveformsFolder);
  ForceDirectories(ShotChangesFolder);
  ForceDirectories(BackupFolder);
  ForceDirectories(ProjectsFolder);
  ForceDirectories(TranslationMemoryFolder);
  ForceDirectories(TerminologyFolder);
  ForceDirectories(WhisperModelsFolder);
  ForceDirectories(WhisperTranscriptionsFolder);
  {$IFDEF DARWIN}
  // rsLine not working on macOS?
  SplitterVideo.ResizeStyle    := rsUpdate;
  SplitterWaveform.ResizeStyle := SplitterVideo.ResizeStyle;

  // prepare macOS menu
  AppMenu := TMenuItem.Create(Self); {Application menu}
  AppMenu.Caption := #$EF#$A3#$BF;   {Unicode Apple logo char}
  MainMenu.Items.Insert(0, AppMenu);

  AboutMenu := TMenuItem.Create(Self);
  AboutMenu.Action := actAbout;
  AppMenu.Add(AboutMenu); {Add About as item in application menu}

  SepMenu := TMenuItem.Create(Self);
  SepMenu.Caption := '-';
  AppMenu.Add(SepMenu); {Add - as item in application menu}

  AppUpdate := TMenuItem.Create(Self);
  AppUpdate.Action := actCheckForUpdates;
  AppMenu.Add(AppUpdate); {Add Check for updates as item in application menu}

  SepMenu := TMenuItem.Create(Self);
  SepMenu.Caption := '-';
  AppMenu.Add(SepMenu); {Add - as item in application menu}

  //FormatPropertiesMenu := TMenuItem.Create(Self);
  //FormatPropertiesMenu.Action := actFormatProperties;
  //AppMenu.Add(FormatPropertiesMenu); {Add format properties as item in application menu}

  SettingsMenu := TMenuItem.Create(Self);
  SettingsMenu.Action := actSettings;
  AppMenu.Add(SettingsMenu); {Add Settings as item in application menu}

  SepMenu := TMenuItem.Create(Self);
  SepMenu.Caption := '-';
  AppMenu.Add(SepMenu); {Add - as item in application menu}

  mnuAbout.Visible            := False;
  mnuCheckForUpdates.Visible  := False;
  mnuHelpSeparator.Visible    := False;
  //mnuFormatProperties.Visible := False;
  mnuSettings.Visible         := False;
  mnuExit.Visible             := False;
  {$ENDIF}

  // update workspace
  SetWorkMode(Workspace.WorkMode);
  EnableWorkArea(False);
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if (Workspace.ViewMode = vmTranscription) then
  begin
    with Workspace.Transcription do
      if Assigned(Memo) and (Memo.Modified) then
        CanClose := (MsgExitTranscriptionMode = mrYes)
      else
        CanClose := True;
  end
  else
    CanClose := CloseSubtitle(True);

  if CanClose then
  begin
    // Settings
    SaveSettings;
    // Transcription
    TranscriptionUnInitializeControls;
    // TMX
    TMX.Free;
    // TBX
    TBX.Free;
    // MRU
    MRU.SaveToXML(MRUFileName);
    MRU.Free;
    // Actors
    SaveActors;
    // MPV
    MPV.Close;
    // VST
    VST.RootNodeCount := 0;
    if Assigned(VSTOptions.BackgroundBlock) then VSTOptions.BackgroundBlock.Free;
    // Unlink Subtitles
    WAVE.Subtitles := NIL;
    // Free SubtitleAPI
    Subtitles.Free;
    // Temp subtitle
    MPVDeleteSubtitleTempTrack;
    // Temp Web preview file
    if FileExists(WebPreviewTempFileName) then DeleteFile(WebPreviewTempFileName);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.FormResize(Sender: TObject);
begin
  StatusBar.Panels[0].Width := ClientWidth - StatusBar.Panels[1].Width;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.FormShow(Sender: TObject);
begin
  LoadFormSettings(Self);
  // Show random tip
  SetStatusBarText(GetRandomTipString, 0, False);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.FormDropFiles(Sender: TObject;
  const FileNames: array of string);
begin
  DropFilesProcess(FileNames);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.LayoutVSTResize(Sender: TObject);
var
  Margin,
  NewWidth: Integer;
begin
  // update our memo bounds
  Margin   := 6;
  NewWidth := LayoutEditorClient.Width;

  if Workspace.TranslatorMode then
  begin
    NewWidth := (NewWidth - Margin) div 2;
    mmoText.Width        := NewWidth;
    mmoTranslation.Width := NewWidth;
    mmoTranslation.Left  := NewWidth + Margin;
  end
  else
    mmoText.Width := NewWidth;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.tedTimeChange(Sender: TObject; const NewTime: Cardinal);
begin
  if not VSTUpdating(VST) and (VSTFocusedNode(VST) > -1) then
  begin
    with (Sender as TUWTimeEdit) do
    begin
      if VST.SelectedCount = 1 then
        SetSubtitleTime(VSTFocusedNode(VST), NewTime, Tag)
      else
        case Tag of
          TAG_CONTROL_INITIALTIME : VSTDoLoop(VST, @ApplySetTimeInitialFromSpin);
          TAG_CONTROL_FINALTIME   : VSTDoLoop(VST, @ApplySetTimeFinalFromSpin);
          TAG_CONTROL_DURATION    : VSTDoLoop(VST, @ApplySetTimeDurationFromSpin);
          TAG_CONTROL_PAUSE       : VSTDoLoop(VST, @ApplySetTimePauseFromSpin);
        end;
    end;
    WAVE.DoUpdate;
    DoAutoCheckErrors(False);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.UndoChanged(const ChangeType: TUndoChangeType);
begin
  actUndo.Enabled := UndoInstance.CanUndo;
  actRedo.Enabled := UndoInstance.CanRedo;

  if ChangeType = uctReIndex then
  begin
    VST.RootNodeCount := Subtitles.Count;
    UpdateValues(True);
  end
  else if ChangeType = uctItems then
    UpdateValues(True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mmoTextChange(Sender: TObject);
begin
  if not VSTUpdating(VST) and (VSTFocusedNode(VST) > -1) then
    with (Sender as TUWMemo) do
      if VST.SelectedCount = 1 then
      begin
        SetSubtitleText(VSTFocusedNode(VST), Text, TSubtitleMode(TAG_CONTROL_TEXT - Tag));
        if (GetTickCount - LastTickCount) > 700 then // only if > 700ms
        begin
          CheckForTerminology(VSTFocusedNode(VST));
          CheckForTranslationMemory(VSTFocusedNode(VST));
          LastTickCount := GetTickCount;
        end;
      end
      else
        case Tag of
          TAG_CONTROL_TEXT        : VSTDoLoop(VST, @ApplySetTextFromMemo);
          TAG_CONTROL_TRANSLATION : VSTDoLoop(VST, @ApplySetTranslationFromMemo);
        end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mmoSourceViewChange(Sender: TObject);
begin
  if (mmoSourceView.Tag = TAG_CONTROL_NORMAL) and ((GetTickCount - LastTickCount) > 700) then // only if > 700ms
  begin
    Subtitles.LoadFromString(mmoSourceView.Text, NIL, 0, TUWSubtitleFormats(cboFormat.ItemIndex+1));
    VST.RootNodeCount := Subtitles.Count;
    WAVE.DoUpdate;
    LastTickCount := GetTickCount;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.cboFPSKeyPress(Sender: TObject; var Key: char);
begin
  if Key = ',' then
    Key := '.';

  if (Key = Chr(VK_RETURN)) and IsFloat(cboFPS.Text, AppOptions.FormatSettings) then
  begin
    AddFPSToCombo(SysUtils.StrToFloat(cboFPS.Text, AppOptions.FormatSettings), cboFPS);
    cboFPSSelect(Sender);
  end
  else if not CharInSet(Key, ['0'..'9', '.', Chr(VK_BACK)]) or
    (Key = '.') and (StringCount('.', cboFPS.Text) = 1) then
    Key := #0;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.cboInputFPSSelect(Sender: TObject);
begin
  if (VST.RootNodeCount > 0) and (Sender <> NIL) then
    VSTDoLoop(VST, @ApplyChangeInputFPS, dlAll, False, True);

  Workspace.FPS.InputFPS := GetInputFPS;

  cboFPS.ItemIndex := cboInputFPS.ItemIndex;
  Workspace.FPS.OutputFPS := Workspace.FPS.InputFPS;
  SetTimeFPStoTimeEditCtrls;

  DoAutoCheckErrors;

  if (Sender <> NIL) then
    UpdateValues(True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.cboFPSSelect(Sender: TObject);
begin
  if (VST.RootNodeCount > 0) and (Sender <> NIL) then
    VSTDoLoop(VST, @ApplyChangeFPS, dlAll, False, True);

  Workspace.FPS.OutputFPS := GetFPS;
  SetTimeFPStoTimeEditCtrls;

  if (Workspace.WorkMode = wmFrames) then
    SetSMPTEMode(not IsInteger(GetFPS));

  DoAutoCheckErrors;

  if (Sender <> NIL) then
    UpdateValues(True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.cboFormatSelect(Sender: TObject);
begin
  Subtitles.Format := TUWSubtitleFormats(cboFormat.ItemIndex+1);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.MRUItemClick(Sender: TObject);
var
  s : String;
begin
  s := (Sender as TMenuItem).Caption;
  if LowerCase(ExtractFileExt(s)) = TProjectExt then
    LoadProject(s)
  else
    LoadSubtitle(s);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.MRUOnChange(Sender: TObject);
begin
  MRU.UpdateMenu(mnuFileMRU);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.PlayRateClick(Sender: TObject);
begin
  with Sender as TMenuItem do
  begin
    AppOptions.DefChangePlayRate := StrToInt(Copy(Caption, 1, Length(Caption)-1));
    actMediaChangePlayRate.Checked := True;
    vtlb3.Down := actMediaChangePlayRate.Checked;
    actMediaChangePlayRateExecute(NIL);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.LoopCountClick(Sender: TObject);
begin
  with Sender as TMenuItem do
  begin
    WAVEOptions.LoopCount := StrToInt(Caption);
    MPVPlaySelectionOnly;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.DoAudioTrackSet(Sender: TObject);
begin
  with Sender as TMenuItem do
    MPV.SetTrack(ttAudio, (Sender as TMenuItem).Tag);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.DoHunspellItemClick(Sender: TObject);
var
  Memo : TUWMemo;
begin
  // word suggest clicked
  Memo := GetMemoFocused;
  if Memo = NIL then Exit;

  if Memo.SelText = '' then GetMemoWordUnderCaret(Memo, True);

  Memo.SelText   := (Sender as TMenuItem).Caption;
  Memo.SelLength := Length(Memo.SelText);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.DoDictionaryItemClick(Sender: TObject);
var
  i: Integer;
begin
  // select dictionary
  if AppOptions.HunspellLanguage <> (Sender as TMenuItem).Caption then
  begin
    for i := 0 to mnuDictionary.Count-1 do mnuDictionary.Items[i].Checked := False;

    (Sender as TMenuItem).Checked := True;
    AppOptions.HunspellLanguage := GetDictionaryNameFromCaption((Sender as TMenuItem).Caption);

    HunspellInstance.LoadDictionary(DictionariesFolder+AppOptions.HunspellLanguage+'.aff', DictionariesFolder+AppOptions.HunspellLanguage+'.dic');
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.DoUnicodeSymbolClick(Sender: TObject);
begin
  with (Sender as TMenuItem), AppOptions do
  begin
    LastUnicodeChar := Caption;
    InsertMemoText(LastUnicodeChar);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.DoSilentZoneClick(Sender: TObject);
begin
  with WAVE do
    if Length(SilentZones) > 0 then
    begin
      with SilentZones[(Sender as TMenuItem).Tag]^ do
        SetSelectionAndPos(Start, Stop);

      DoUpdate;
    end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.DoMemoPopup(Sender: TObject);
var
  Memo     : TUWMemo;
  Suggests : TStrings;
  i        : Integer;
  mnu      : TMenuItem;
  s        : String;
begin
  // check word under caret
  Memo     := GetMemoFocused;
  Suggests := NIL;

  if Memo = NIL then Exit;
  mnuHunspellSeparator.Visible := False;

  for i := popMemo.Items.Count-1 downto 0 do
    if Copy(popMemo.Items[i].Name, 1, 3) = 'hs_' then
      popMemo.Items[i].Free;

  s := GetMemoWordUnderCaret(Memo);

  if HunspellInstance.Suggest(s, Suggests) then
  begin
    for i := 0 to Suggests.Count-1 do
    begin
      mnu         := TMenuItem.Create(popMemo);
      mnu.Name    := 'hs_' + IntToStr(i);
      mnu.Caption := Suggests[i];
      mnu.OnClick := @DoHunspellItemClick;
      popMemo.Items.Insert(i, mnu);
    end;
    mnuHunspellSeparator.Visible := True;
  end;

  if Suggests <> NIL then Suggests.Free;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.DoVSTPopup(Sender: TObject);
begin
  mnuNoteSeparator.Visible := (VST.SelectedCount = 1);
  mnuAddNote.Visible       := mnuNoteSeparator.Visible;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.DoTranscriptionPopup(Sender: TObject);
begin
  mnuTranscriptionInsertTime.Enabled := (MPV.GetMediaLenInMs > 0);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.DoWAVEPopup(Sender: TObject);
begin
  with WAVE do
    if IsTimeLineEnabled then
    begin
      actMediaDeleteSubtitle.Enabled := SelectedItem <> NIL;

      if SelectionIsEmpty then
        actDeleteShotChange.Enabled := ContainsSceneChange(CursorPosMS, CursorPosMS)
      else
        actDeleteShotChange.Enabled := ContainsSceneChange(Selection.InitialTime, Selection.FinalTime);
    end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.DoAutoBackupTimer(Sender: TObject);
begin
  if (VST.RootNodeCount > 0) then
  begin
    SaveSubtitleAutoBackup;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.DoStatusTimer(Sender: TObject);
begin
  TimerStatus.Enabled := False;
  StatusBar.Panels[0].Text := '';
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.MemoClickLink(Sender: TObject; const ALink: String);
begin
  if Workspace.WorkMode = wmFrames then
    MPV.SeekInMs(StringToTime(ALink, False, Workspace.FPS.OutputFPS))
  else
    MPV.SeekInMs(StringToTime(ALink));
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.MemoContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := True;
  with Workspace.Transcription.Memo, ClientToScreen(MousePos) do
    popTranscription.PopUp(X, Y);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.MemoChange(Sender: TObject);
begin
  with Workspace.Transcription do
    if Assigned(Memo) then
    begin
      actUndo.Enabled := not Memo.Strings.UndoEmpty;
      actRedo.Enabled := not Memo.Strings.RedoEmpty;
    end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.cboActorChange(Sender: TObject);
begin
  if cboActor.Tag = 0 then SetActor;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.cboActorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (cboActor.Text <> '') and (cboActor.Items.IndexOf(cboActor.Text) < 0) then
  begin
    cboActor.Items.Add(cboActor.Text);
    Key := 0;
  end;
end;

// -----------------------------------------------------------------------------

{ SubtileAPI OnLoadDataFunc }

// -----------------------------------------------------------------------------

function SAPILoadDataFunc(const AData: Pointer; const ADataClass: ShortString): Integer;
begin
  Result := 0;
  if ADataClass = 'TStringList' then
    Result := formCustomSelectDlg.ExecuteDialog('', GetCommonString('SelectSheetToUse'), TStrings(AData), 0);
end;

// -----------------------------------------------------------------------------

{$I formMain_VST.inc}
{$I formMain_WAVE.inc}
{$I formMain_MPV.inc}
{$I formMain_Actions.inc}

// -----------------------------------------------------------------------------

end.

