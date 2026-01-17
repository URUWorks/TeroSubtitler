{*
 *  URUWorks Waveform Displayer Control
 *
 *  Copyright (C) 2021-2023 URUWorks, uruworks@gmail.com.
 *
 *  Based on the great work of:
 * -----------------------------------------------------------------------------
 *  VisualSubSync
 * -----------------------------------------------------------------------------
 *  Copyright (C) 2003 Christophe Paris
 * -----------------------------------------------------------------------------
 *  This Program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This Program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with GNU Make; see the file COPYING.  If not, write to
 *  the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 *  http://www.gnu.org/copyleft/gpl.html
 * -----------------------------------------------------------------------------
 *}

unit WAVDisplayer;

{$mode ObjFPC}{$H+}
{$IFNDEF WINDOWS}
  {$DEFINE USEBGRABITMAP}
{$ENDIF}

interface

uses
  Classes, Types, SysUtils, Controls, Graphics, Math, LazarusPackageIntf,
  LResources,
  WAVDisplayer.WaveFile, WAVDisplayer.ScrollBar,
  UWSubtitleAPI, UWSubtitleAPI.Tags
  {$IFDEF USEBGRABITMAP}
  , BGRABitmap, BGRABitmapTypes
  {$ELSE}
  , laz.VTGraphics
  {$ENDIF};

type

  TThumbnails = array of TBitmap;

  PPeak = ^TPeak;
  TPeak = record
    Max : SmallInt;
    Min : Smallint;
  end;

  TPeakCreationEventType = (pcetStart, pcetProgress, pcetStop);
  TPeakCreationEvent     = procedure (Sender: TObject; const EventType: TPeakCreationEventType; const Param: Integer) of object;

  TMinBlankInfoPart = (mbipStart, mbipStop, mbipInvalid);
  TMinBlankInfo = class(TObject)
  private
    Range : PUWSubtitleItem;
    Part  : TMinBlankInfoPart;
  public
    constructor Create;
    function Exists : Boolean;
    function SetInfo(NewRange : PUWSubtitleItem; NewPart : TMinBlankInfoPart) : Boolean;
    function GetStart(MinBlankTime : Integer) : Integer;
    function GetStop(MinBlankTime : Integer) : Integer;
    function GetSnappingPoint(MinBlankTime : Integer) : Integer;
  end;

  PZoneRange = ^TZoneRange;
  TZoneRange = record
    Start,
    Stop     : Integer;
    RmsSum   : Double;
    RmsCount : Integer;
  end;
  TZonesList = array of PZoneRange;

  TCustomColors = record
    Waveform,
    Background,
    GridLine,
    Text,
    Cursor,
    PlayCursor,
    Item,
    ItemSelected,
    SceneChange: {$IFDEF USEBGRABITMAP}TBGRAPixel{$ELSE}TColor{$ENDIF};
  end;

  TCustomDrawSubtitleEvent   = procedure(Sender: TObject; ACanvas: TCanvas; const Index: Integer; const SubtitleItem: TUWSubtitleItem; const Rect: TRect) of object;
  TSubtitleItemChangedEvent  = procedure(Sender: TObject; const Index: Integer; const OldInitialTime, OldFinalTime: Integer; const NeedSort: Boolean) of object;
  TSelectedSubtitleItemEvent = procedure(Sender: TObject; const Index: Integer; const SubtitleItem: TUWSubtitleItem; const IsDynamic: Boolean) of object;
  TTimeLineClickEvent        = procedure(Sender: TObject; const Time: Integer) of object;
  //TSubtitleItemDblClickEvent = procedure(Sender: TObject; const SubtitleItem: TUWSubtitleItem) of object;

  TUpdateViewFlag  = (uvfCursor, uvfSelection, uvfSubtitle, uvfPosition, uvfPageSize, uvfPlayCursor);
  TUpdateViewFlags = set of TUpdateViewFlag;

  TDynamicEditMode = (demNone, demInitial, demFinal, demWhole);

  TUWWaveformDisplayer = class(TCustomControl)
  private
    FPeakTab         : array of TPeak;
    FPeakTabSize     : Cardinal;
    FSamplesPerPeak  : Cardinal;
    FPeakDataLoaded  : Boolean;
    FSavePeakToFile  : Boolean;
    FWaveFormat      : TWaveFormatEx;

    FSilentZones     : TZonesList;

    FSubtitles       : TUWSubtitles;

    FBackBuffer,
    FBackBufferWAVE,
    FBackBufferItems : {$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF};

    FFileName        : String;
    FFPS             : Double;
    FFPSTimeMode     : Boolean;
    FSMPTE           : Boolean;

    FLengthMS      : Integer;
    FPositionMS    : Integer;
    FPageSizeMS    : Integer;
    FOldPositionMs : Integer; // Used to optimize drawing
    FOldPageSizeMs : Integer;

    FStepMs  : Integer;
    FStepLog : Integer;

    FVerticalScaling : Integer; // 1..800%
    FTimeLineHeight  : Byte;

    FDynamicEditMode   : TDynamicEditMode;
    FDynamicSelSub     : PUWSubtitleItem;
    FDynamicSelOldSub  : PUWSubtitleItem;
    FDynamicEditTimeMs : Integer;

    FSelectedSubtitle  : PUWSubtitleItem;
    FDynamicSelection  : TUWSubtitleItem;
    FDynamicSelValueMs : Integer;
    FNeedToSortList    : Boolean;

    FDrawThumbnails    : Boolean;
    FThumbnails        : TThumbnails;
    FDefaultThumbnail  : TBitmap;
    FStretchThumbnails : Boolean;

    FGAP               : Integer; // Minimum blank between subtitle in ms
    FDrawGAP           : Boolean;
    FSnappingEnabled   : Boolean;

    FMinBlankInfo1,
    FMinBlankInfo2 : TMinBlankInfo;

    FMinSelTime  : Integer;
    FMaxSelTime  : Integer;

    FPrevSubIdx : Integer;
    FNextSubIdx : Integer;

    FSceneChangeList    : TIntegerDynArray;
    FSceneChangeEnabled : Boolean;

    FMouseIsDown                 : Boolean;
    FEnableMouseAntiOverlapping  : Boolean;

    FCursorMS        : Integer;
    FPlayCursorMS    : Integer;
    FCenterPlay      : Boolean;

    FOldInitialTime : Integer;
    FOldFinalTime   : Integer;
    FOldMouseDownX  : Integer;

    FScrollBar : TUWScrollBar;

    FTS : TTextStyle;
    FEmptyText : String;

    FOnPeakCreation               : TPeakCreationEvent;
    FOnCustomDrawSubtitleItem     : TCustomDrawSubtitleEvent;
    FOnItemChangedEvent           : TSubtitleItemChangedEvent;
    FOnCursorChange               : TNotifyEvent;
    FOnPlayCursorChange           : TNotifyEvent;
    FOnSelectionChange            : TNotifyEvent;
    FOnViewChange                 : TNotifyEvent;
    FOnSelectedSubtitleItem       : TSelectedSubtitleItemEvent;
    FOnSelectedSubtitleItemChange : TNotifyEvent;
    FOnTimeLineClickEvent         : TTimeLineClickEvent;
    //FOnSubtitleItemStartDblClick  : TSubtitleItemDblClickEvent;
    //FOnSubtitleItemStopDblClick   : TSubtitleItemDblClickEvent;

    function PixelToTime(const Pixel: Integer): Integer;
    function TimeToPixel(const Time: Integer): Integer;
    function GetCorrectTimePos(const PosMs: Integer): Integer;
    function GetCanvasHeight: Integer;
    function GetSubCanvasHeight: Integer;
    function GetSubCanvasY: Integer;
    function GetWAVECanvasY: Integer;

    function IsTimeLineClicked(const XY: Integer): Boolean;

    function FindSnappingPoint(const PosMs: Integer): Integer;
    function FindCorrectedSnappingPoint(const PosMs: Integer): Integer;
    function SetMinBlankOnIdx(const Idx: Integer): Boolean;
    function SetMinBlankAt(const TimeMs: Integer): Boolean;

    procedure SetSelectedSubtitleItem(const Value: PUWSubtitleItem; const UpdateDisplay: Boolean; const UpdatePosition: Boolean);
    function CheckSubtitleItemForDynamicSelection(const Subtitle: TUWSubtitleItem; const CursorPosMS, SubtitleSelWindow, X, Y: Integer): Boolean;
    procedure SetLengthMS(const LenghtMS: Integer);
    procedure SetPositionMS(NewPosition: Integer);
    procedure OnScrollBarChange(Sender: TObject);
    function GetPositionMS: Integer;
    procedure UpdateView(const UpdateViewFlags: TUpdateViewFlags; const DoRepaint: Boolean = True);
    procedure ZoomSubtitle(const Subtitle: TUWSubtitleItem); overload;
    procedure ZoomSubtitle(const Start, Stop: Integer); overload;
    procedure SetEmptyText(const AText: String);

    // peak
    procedure ClearPeakData;
    procedure CreatePeakTab(WAVFile: TUWWAVEFile);
    function NormalizePeakTab(NormFactor: Double): Boolean;
    // draw
    procedure DrawAlphaRect(const ABitmap:{$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF}; const x1, x2 : Integer; const y1, y2 : Integer; const AColor: {$IFDEF USEBGRABITMAP}TBGRAPixel{$ELSE}TColor{$ENDIF});
    procedure DrawAlphaRectByTimes(const ABitmap:{$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF}; const t1, t2 : Integer; const y1, y2 : Integer; const AColor: {$IFDEF USEBGRABITMAP}TBGRAPixel{$ELSE}TColor{$ENDIF});
    procedure DrawWave(const ABitmap: {$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF}; const TryOptimize: Boolean = False);
    procedure DrawTimeLine(const ABitmap: {$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF});
    procedure DrawItemsCanvas(const ACompleteDraw: Boolean = False);
    procedure DrawSubtitleItem(const ABitmap: {$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF}; const ATop, ABottom: Integer);
    procedure DrawThumbnails(const ABitmap: {$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF}; const ATop, ABottom: Integer);
    procedure DrawSelection(const ABitmap: {$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF});
    procedure DrawCursor(const ABitmap: {$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF});
    procedure DrawPlayCursor(const ABitmap: {$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF});
    procedure DrawGridLines(const ABitmap: {$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF});
    procedure DrawSceneChange(const ABitmap: {$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF}; const ATop, ABottom: Integer);
    procedure DrawMinimumBlank(const ABitmap: {$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF}; const ATop, ABottom: Integer);
    procedure CalculateTimeDivisionStepMs;
  protected
    procedure Paint; override;
    procedure DoOnResize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Click; override;
    procedure DblClick; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
  public
    CustomColors: TCustomColors;
    //
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsPeakDataLoaded: Boolean;
    function LoadWaveFromFile(const FileName: String): Boolean;
    procedure GenerateDummyPeakTab(const LengthMS: Cardinal);
    procedure Close;
    function IsOnlySelection: Boolean;
    function SelectionIsEmpty: Boolean;
    procedure ClearSelection;
    function IsTimeLineEnabled: Boolean;
    function GetPlayCursorMS: Integer;
    procedure SetPlayCursorMS(NewPosMS: Integer; const ABothCursor: Boolean = False);
    procedure SetCursorMS(NewPosMS: Integer; const AUpdate: Boolean = False);
    procedure ZoomAll;
    procedure ZoomIn;
    procedure ZoomOut;
    procedure ZoomSelection;
    procedure SetSelectionAndPos(const Start, Stop: Integer);
    procedure VZoomRestore(const AInvalidate: Boolean = False);
    procedure VZoomMore(const AInvalidate: Boolean = False);
    procedure VZoomLess(const AInvalidate: Boolean = False);
    procedure SelectSubtitle(const Index: Integer; const UpdateDisplay: Boolean; const UpdatePosition: Boolean);
    function GetSubtitleIdxAtCursorPos: Integer;
    procedure DoUpdate(const Complete: Boolean = True);
    // scenechange
    procedure SetSceneChangeEnabled(const AValue: Boolean);
    procedure LoadSceneChangeFile(const AFileName: String; const ASMPTE: Boolean = False);
    procedure SaveSceneChangeFile(const AFileName: String);
    procedure SetSceneChangeList(const SceneChangeList: TIntegerDynArray);
    procedure ClearSceneChange;
    function GetSceneChangeCount: Integer;
    function GetSceneChangeList: TIntegerDynArray;
    function GetSceneChangeAt(const Index: Integer): Integer;
    function GetNextSceneChangeIndex(const ATimeMS: Integer; const ABackward: Boolean = False): Integer;
    function GetNextSceneChange(const ATimeMS: Integer): Integer;
    function GetPreviousSceneChange(const ATimeMS: Integer): Integer;
    function ContainsSceneChange(const AStart, AStop: Integer): Boolean;
    function DeleteSceneChange(const AInitialTimeMS, AFinalTimeMS: Integer): TIntegerDynArray;
    procedure InsertSceneChange(const Src: TIntegerDynArray); overload;
    procedure InsertSceneChange(const ATimeInMS: Integer); overload;
    procedure SetSceneChangeTimeMode(const ASMPTE: Boolean);
    // silent/zonelist
    procedure ClearZoneList(var AZoneList: TZonesList);
    function DetectZoneList(var AZoneList: TZonesList; const AFindSilent: Boolean = True; const Threshold: Integer = 100; const WinSizeMS: Integer = 100): Boolean;
    function DetectSilentZone(const Threshold: Integer = 100; const WinSizeMS: Integer = 100): Boolean;
    // properties
    property Selection: TUWSubtitleItem read FDynamicSelection;
    property SelectedItem: PUWSubtitleItem read FSelectedSubtitle;
    property IsMouseDown: Boolean read FMouseIsDown;
    property SubTextStyle: TTextStyle read FTS write FTS;
    property Thumbnails: TThumbnails read FThumbnails write FThumbnails;
    property DefaultThumbnail: TBitmap read FDefaultThumbnail;
    property SMPTE: Boolean read FSMPTE write FSMPTE;
  published
    property Subtitles                     : TUWSubtitles                 read FSubtitles                    write FSubtitles;
    property MinimumBlank                  : Integer                      read FGAP                          write FGAP;
    property EmptyText                     : String                       read FEmptyText                    write SetEmptyText;
    property FileName                      : String                       read FFileName;
    property FPS                           : Double                       read FFPS                          write FFPS;
    property FPSTimeMode                   : Boolean                      read FFPSTimeMode                  write FFPSTimeMode;
    property CursorPosMS                   : Integer                      read FCursorMS;
    property CenterPlayCursor              : Boolean                      read FCenterPlay                   write FCenterPlay;
    property SceneChangeEnabled            : Boolean                      read FSceneChangeEnabled           write SetSceneChangeEnabled;
    property DrawGAP                       : Boolean                      read FDrawGAP                      write FDrawGAP;
    property DrawThumbnail                 : Boolean                      read FDrawThumbnails               write FDrawThumbnails;
    property SilentZones                   : TZonesList                   read FSilentZones;
    property SavePeakToFile                : Boolean                      read FSavePeakToFile               write FSavePeakToFile;
    property StretchThumbnails             : Boolean                      read FStretchThumbnails            write FStretchThumbnails;
    property OnCustomDrawSubtitleItem      : TCustomDrawSubtitleEvent     read FOnCustomDrawSubtitleItem     write FOnCustomDrawSubtitleItem;
    property OnCursorChange                : TNotifyEvent                 read FOnCursorChange               write FOnCursorChange;
    property OnPlayCursorChange            : TNotifyEvent                 read FOnPlayCursorChange           write FOnPlayCursorChange;
    property OnSelectionChange             : TNotifyEvent                 read FOnSelectionChange            write FOnSelectionChange;
    property OnViewChange                  : TNotifyEvent                 read FOnViewChange                 write FOnViewChange;
    property OnSelectedSubtitleItem        : TSelectedSubtitleItemEvent   read FOnSelectedSubtitleItem       write FOnSelectedSubtitleItem;
    property OnSelectedSubtitleItemChange  : TNotifyEvent                 read FOnSelectedSubtitleItemChange write FOnSelectedSubtitleItemChange;
    property OnSelectedSubtitleItemChanged : TSubtitleItemChangedEvent    read FOnItemChangedEvent           write FOnItemChangedEvent;
    property OnTimeLineClick               : TTimeLineClickEvent          read FOnTimeLineClickEvent         write FOnTimeLineClickEvent;
    property OnPeakCreation                : TPeakCreationEvent           read FOnPeakCreation               write FOnPeakCreation;
    //property OnSubtitleItemStartDblClick   : TSubtitleItemDblClickEvent   read FOnSubtitleItemStartDblClick  write FOnSubtitleItemStartDblClick;
    //property OnSubtitleItemStopDblClick    : TSubtitleItemDblClickEvent   read FOnSubtitleItemStopDblClick   write FOnSubtitleItemStopDblClick;

    property Align;
    property Anchors;
    property BorderSpacing;
    property Constraints;
    property Enabled;
    property PopupMenu;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property Visible;
    property OnClick; // only if not wav is loaded
    property OnContextPopup;
  end;

procedure Register;

const
  DefaultThumbnailsCount = 100;

// -----------------------------------------------------------------------------

implementation

uses
  UWSystem.TimeUtils, UWSystem.SysUtils;

const
  DefaultPageSizeMS = 10000; // 5000

// -----------------------------------------------------------------------------

{ Helpers }

// -----------------------------------------------------------------------------

constructor TMinBlankInfo.Create;
begin
  inherited;
  Range := NIL;
end;

// -----------------------------------------------------------------------------

function TMinBlankInfo.Exists : Boolean;
begin
  Result := Assigned(Range);
end;

// -----------------------------------------------------------------------------

function TMinBlankInfo.SetInfo(NewRange : PUWSubtitleItem; NewPart : TMinBlankInfoPart) : Boolean;
begin
  Result := False;
  if (Range <> NewRange) then
  begin
    Range := NewRange;
    Result := True; // Changed
  end;
  if (Part <> NewPart) then
  begin
    Part := NewPart;
    Result := True; // Changed
  end;
end;

// -----------------------------------------------------------------------------

function TMinBlankInfo.GetStart(MinBlankTime : Integer): Integer;
begin
  if (Part = mbipStart) then
    Result := Range^.InitialTime - MinBlankTime
  else
    Result := Range^.FinalTime;
end;

// -----------------------------------------------------------------------------

function TMinBlankInfo.GetStop(MinBlankTime : Integer) : Integer;
begin
  if (Part = mbipStart) then
    Result := Range^.InitialTime
  else
    Result := Range^.FinalTime + MinBlankTime;
end;

// -----------------------------------------------------------------------------

function TMinBlankInfo.GetSnappingPoint(MinBlankTime : Integer) : Integer;
begin
  if (Part = mbipStart) then
    Result := GetStart(MinBlankTime)
  else
    Result := GetStop(MinBlankTime);
end;

// -----------------------------------------------------------------------------

{ TUWWaveformDisplayer }

// -----------------------------------------------------------------------------

constructor TUWWaveformDisplayer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  AutoSize := False;
  TabStop  := True;

  FLengthMS        := 0;
  FPositionMS      := 0;
  FPageSizeMS      := DefaultPageSizeMS;
  FPeakDataLoaded  := False;
  FSavePeakToFile  := True;
  FVerticalScaling := 85;
  FTimeLineHeight  := 16;

  FDynamicEditMode   := demNone;
  FDynamicSelSub     := NIL;
  FDynamicSelOldSub  := NIL;
  FDynamicEditTimeMs := 0;

  FSceneChangeEnabled := False;

  FSnappingEnabled := True;
  FDrawGAP         := False;
  FGAP             := 30;
  FMinBlankInfo1   := TMinBlankInfo.Create;
  FMinBlankInfo2   := TMinBlankInfo.Create;

  FDrawThumbnails   := False;
  FThumbnails       := NIL;
  FDefaultThumbnail := TBitmap.Create;
  StretchThumbnails := False;

  FDynamicSelection.InitialTime := 0;
  FDynamicSelection.FinalTime   := 0;

  FMouseIsDown                := False;
  FEnableMouseAntiOverlapping := True;

  FNeedToSortList    := False;
  FDynamicSelValueMs := -1;
  FCursorMS          := 0;
  FPlayCursorMS      := 0;
  FCenterPlay        := True;

  FOldInitialTime    := -1;
  FOldFinalTime      := -1;

  FMinSelTime  := -1;
  FMaxSelTime  := -1;

  FPrevSubIdx := -1;
  FNextSubIdx := -1;

  FOldPositionMs := -1;
  FOldPageSizeMs := -1;

  FScrollBar          := TUWScrollBar.Create(Self);
  FScrollBar.Parent   := Self;
  FScrollBar.Height   := 7;
  FScrollBar.Align    := alBottom;
  FScrollBar.Min      := 0;
  FScrollBar.Max      := FLengthMs;
  FScrollBar.Position := 0;
  FScrollBar.PageSize := FLengthMs;
  FScrollBar.OnChange := @OnScrollBarChange;

  FillByte(FTS, SizeOf(FTS), 0);
  FTS.Clipping    := True;
  FTS.SingleLine  := False;
  //FTS.EndEllipsis := True;

  FFPS         := 25;
  FFPSTimeMode := False;
  FSMPTE       := False;
  FEmptyText   := '';

  with CustomColors do
  begin
    Waveform     := {$IFDEF USEBGRABITMAP}BGRA{$ELSE}RGBToColor{$ENDIF}(0, 84, 184);
    Background   := {$IFDEF USEBGRABITMAP}BGRA{$ELSE}RGBToColor{$ENDIF}(10, 10, 10);
    GridLine     := {$IFDEF USEBGRABITMAP}BGRA{$ELSE}RGBToColor{$ENDIF}(45, 45, 45);
    Text         := {$IFDEF USEBGRABITMAP}BGRA{$ELSE}RGBToColor{$ENDIF}(224, 224, 224);
    PlayCursor   := {$IFDEF USEBGRABITMAP}BGRA{$ELSE}RGBToColor{$ENDIF}(255, 255, 255);
    Cursor       := {$IFDEF USEBGRABITMAP}BGRA{$ELSE}RGBToColor{$ENDIF}(230, 230, 0);
    Item         := {$IFDEF USEBGRABITMAP}BGRA{$ELSE}RGBToColor{$ENDIF}(0, 35, 65);
    ItemSelected := {$IFDEF USEBGRABITMAP}BGRA{$ELSE}RGBToColor{$ENDIF}(255, 255, 255);
    SceneChange  := {$IFDEF USEBGRABITMAP}BGRA{$ELSE}RGBToColor{$ENDIF}(200, 100, 40);
  end;

  {$IFDEF USEBGRABITMAP}
  FBackBufferWAVE  := TBGRABitmap.Create(Width, Height, CustomColors.Background);
  FBackBufferWAVE.AntialiasingDrawMode := dmSet;
  FBackBufferWAVE.CanvasBGRA.AntialiasingMode := amOff;
  FBackBuffer:= TBGRABitmap.Create(Width, Height, CustomColors.Background);
  FBackBuffer.AntialiasingDrawMode := dmSet;
  FBackBuffer.CanvasBGRA.AntialiasingMode := amOff;
  FBackBufferItems := TBGRABitmap.Create(Width, Height, CustomColors.Background);
  FBackBufferItems.AntialiasingDrawMode := dmSetExceptTransparent;
  FBackBufferItems.CanvasBGRA.AntialiasingMode := amOff;

  FBackBufferWAVE.CanvasBGRA.Font.Name := 'Tahoma';
  {$ELSE}
  FBackBufferWAVE := TBitmap.Create;
  FBackBufferWAVE.PixelFormat := pf32bit;
  FBackBufferWAVE.SetSize(Width, Height);
  FBackBufferWAVE.Canvas.Brush.Color := CustomColors.Background;
  FBackBufferWAVE.Canvas.FillRect(FBackBufferWAVE.Canvas.ClipRect);

  FBackBuffer := TBitmap.Create;
  FBackBuffer.PixelFormat := pf32bit;
  FBackBuffer.SetSize(Width, Height);
  FBackBuffer.Canvas.Brush.Color := CustomColors.Background;
  FBackBuffer.Canvas.FillRect(FBackBuffer.Canvas.ClipRect);

  FBackBufferItems := TBitmap.Create;
  FBackBufferItems.PixelFormat := pf32bit;
  FBackBufferItems.SetSize(Width, Height);
  FBackBufferItems.Canvas.Brush.Color := CustomColors.Background;
  FBackBufferItems.Canvas.FillRect(FBackBufferItems.Canvas.ClipRect);
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

destructor TUWWaveformDisplayer.Destroy;
begin
  FPeakTab          := NIL;
  FDynamicSelSub    := NIL;
  FDynamicSelOldSub := NIL;
  FSubtitles        := NIL;

  ClearZoneList(FSilentZones);

  FDefaultThumbnail.Free;
  FBackBufferItems.Free;
  FBackBufferWAVE.Free;
  FBackBuffer.Free;
  FScrollBar.Free;
  FMinBlankInfo1.Free;
  FMinBlankInfo2.Free;

  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TUWWaveformDisplayer.Paint;
begin
  if (FBackBuffer.Width > 0) and (FBackBuffer.Height > 0) then
    {$IFDEF USEBGRABITMAP}
    FBackBuffer.Draw(Canvas, 0, 0)
    {$ELSE}
    Canvas.Draw(0, 0, FBackBuffer)
    {$ENDIF}
  else
  begin
    Canvas.Brush.Color := CustomColors.Background;
    Canvas.FillRect(ClientRect);
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWWaveformDisplayer.DoOnResize;
begin
  UpdateView([uvfPageSize], False);
end;

// -----------------------------------------------------------------------------

function TUWWaveformDisplayer.PixelToTime(const Pixel: Integer): Integer;
begin
  if not IsTimeLineEnabled then
    Result := 0
  else
    Result := Round(Pixel * (FPageSizeMS / Width));
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplayer.TimeToPixel(const Time: Integer): Integer;
begin
  if not IsTimeLineEnabled then
    Result := 0
  else
  begin
    if FFPSTimeMode then
      Result := Round(GetCorrectTimePos(Time) / (FPageSizeMs / Width))
    else
      Result := Round(Time / (FPageSizeMs / Width));
  end;
end;

// -----------------------------------------------------------------------------

function TUWWaveformDisplayer.GetCorrectTimePos(const PosMs: Integer): Integer;
begin
  if FFPSTimeMode then
  begin
    if FSMPTE then
      Result := IncDecFrame(PosMs, FFPS, 0, FSMPTE) //RoundTimeWithFrames(PosMs, NormalizeFPS(FFPS))
    else
      Result := RoundTimeWithFrames(PosMs, FFPS);
  end
  else
    Result := PosMs;
end;

// -----------------------------------------------------------------------------

function TUWWaveformDisplayer.GetCanvasHeight: Integer;
begin
  Result := Height - FScrollBar.Height;
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplayer.GetSubCanvasHeight: Integer;
begin
  Result := GetCanvasHeight div iff(FDrawThumbnails, 3, 2);
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplayer.GetSubCanvasY: Integer;
begin
  Result := GetSubCanvasHeight * iff(FDrawThumbnails, 1, 0);
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplayer.GetWAVECanvasY: Integer;
begin
  Result := GetSubCanvasHeight * iff(FDrawThumbnails, 2, 1);
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplayer.IsTimeLineClicked(const XY: Integer): Boolean;
begin
  Result := InRange(XY, GetCanvasHeight - FTimeLineHeight,  GetCanvasHeight);
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplayer.FindSnappingPoint(const PosMs: Integer): Integer;
const
  SNAPPING_DISTANCE_PIXEL: Integer = 8;

var
  Candidate,
  SnappingDistanceTime,
  Idx, IdxForward, IdxBackward,
  DistForward, DistBackward: Integer;
begin
  Result    := -1;
  Candidate := -1;

  SnappingDistanceTime := PixelToTime(SNAPPING_DISTANCE_PIXEL);

  if (FGAP > 0) then
  begin
    if (FMinBlankInfo1.Exists) then
    begin
      Candidate := FMinBlankInfo1.GetSnappingPoint(FGAP);
      if Abs(Candidate - PosMs) <= SnappingDistanceTime then
      begin
        Result := Candidate;
        Exit;
      end;
    end;

    if (FMinBlankInfo2.Exists) then
    begin
      Candidate := FMinBlankInfo2.GetSnappingPoint(FGAP);
      if Abs(Candidate - PosMs) <= SnappingDistanceTime then
      begin
        Result := Candidate;
        Exit;
      end;
    end;
  end;

  if FSceneChangeEnabled and (System.Length(FSceneChangeList) > 0) then
  begin
    Idx         := BinarySearch_IntArray(FSceneChangeList, PosMs);
    IdxForward  := Idx;
    IdxBackward := Idx - 1;

    if (FDynamicSelValueMs = -1) then
    begin
      // Choose the closest one
      if (IdxForward < System.Length(FSceneChangeList)) and (IdxBackward >= 0) then
      begin
        DistForward := FSceneChangeList[IdxForward] - PosMs;
        DistBackward := PosMs - FSceneChangeList[IdxBackward];

        if (DistForward < DistBackward) then
          Candidate := FSceneChangeList[IdxForward]
        else
          Candidate := FSceneChangeList[IdxBackward];
      end
      else if (IdxBackward >= 0) then
      begin
        Candidate := FSceneChangeList[IdxBackward];
      end
      else if (IdxForward < System.Length(FSceneChangeList)) then
      begin
        Candidate := FSceneChangeList[IdxForward];
      end;

      if (Candidate <> -1) and (Abs(Candidate - PosMs) <= SnappingDistanceTime) then
        Result := Candidate;
    end
    else
    begin
      // Check forward
      if (IdxForward < System.Length(FSceneChangeList)) and (FDynamicSelValueMs < FSceneChangeList[IdxForward]) then
      begin
        Candidate := FSceneChangeList[IdxForward];
        if Abs(Candidate - PosMs) <= SnappingDistanceTime then
          Exit(Candidate);
      end;

      // Check backward
      if (IdxBackward >= 0) and (FDynamicSelValueMs > FSceneChangeList[IdxBackward]) then
      begin
        Candidate := FSceneChangeList[IdxBackward];
        if Abs(Candidate - PosMs) <= SnappingDistanceTime then
          Exit(Candidate);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplayer.FindCorrectedSnappingPoint(const PosMs: Integer): Integer;
begin
  Result := FindSnappingPoint(PosMs);
  if (Result = -1) or (not FEnableMouseAntiOverlapping) then
    Exit;

  // Do a correction here
  if (FMinSelTime <> -1) and (Result < FMinSelTime) then
    Result := FMinSelTime;

  if (FMaxSelTime <> -1) and (Result > FMaxSelTime) then
    Result := FMaxSelTime;
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplayer.SetMinBlankOnIdx(const Idx: Integer): Boolean;
var
  ChangedInStep : Boolean;
begin
  Result := False;

  if (Idx > 0) then
    ChangedInStep := FMinBlankInfo1.SetInfo(FSubtitles.ItemPointer[Idx - 1], mbipStop)
  else
    ChangedInStep := FMinBlankInfo1.SetInfo(nil, mbipInvalid);

  Result := Result or ChangedInStep;

  if (Idx < FSubtitles.Count-1) then
    ChangedInStep := FMinBlankInfo2.SetInfo(FSubtitles.ItemPointer[Idx + 1], mbipStart)
  else
    ChangedInStep := FMinBlankInfo2.SetInfo(NIL, mbipInvalid);

  Result := Result or ChangedInStep;
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplayer.SetMinBlankAt(const TimeMs: Integer): Boolean;
var
  idx : Integer;
  ChangedInStep : Boolean;
begin
  Result := False;

  idx := FSubtitles.IndexOf(TimeMs);
  if (idx <> -1) then
  begin
    ChangedInStep := SetMinBlankOnIdx(idx);
    Result := Result or ChangedInStep;
  end
  else
  begin
    idx := FSubtitles.FindInsertPos(TimeMs, -1);
    if (idx > 0) then
      ChangedInStep := FMinBlankInfo1.SetInfo(FSubtitles.ItemPointer[Idx - 1], mbipStop)
    else
      ChangedInStep := FMinBlankInfo1.SetInfo(nil, mbipInvalid);

    Result := Result or ChangedInStep;

    if (idx < FSubtitles.Count) then
      ChangedInStep := FMinBlankInfo2.SetInfo(FSubtitles.ItemPointer[Idx], mbipStart)
    else
      ChangedInStep := FMinBlankInfo2.SetInfo(NIL, mbipInvalid);

    Result := Result or ChangedInStep;
  end;
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.CalculateTimeDivisionStepMs;
var
  MaxPosStep : Integer;
begin
  // Do some little calculation to try to show "round" time
  if FFPSTimeMode then
  begin
    MaxPosStep := Round(Width / (Canvas.TextWidth('0:00:00:00') * 2));
    FStepMs    := Round(FPageSizeMS / MaxPosStep);
    if FStepMs = 0 then FStepMs := 1;

    FStepLog := Trunc(Power(10, Trunc(Log10(FStepMs))));
    FStepMs  := GetCorrectTimePos(FStepMs div FStepLog * FStepLog);
    if FStepMs = 0 then FStepMs := 1;
  end
  else
  begin
    MaxPosStep := Round(Width / (Canvas.TextWidth('0:00:00.000') * 2));
    FStepMs    := Round(FPageSizeMS / MaxPosStep);
    if FStepMs = 0 then FStepMs := 1;

    FStepLog := Trunc(Power(10, Trunc(Log10(FStepMs))));
    FStepMs  := FStepMs div FStepLog * FStepLog;
  end;
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplayer.GetPlayCursorMS: Integer;
begin
  Result := FPlayCursorMS;
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.SetPlayCursorMS(NewPosMS: Integer; const ABothCursor: Boolean = False);
var
  AMiddle: Integer;
begin
  Constrain(NewPosMS, 0, FLengthMS);
  if FPlayCursorMS <> NewPosMS then
  begin
    FPlayCursorMS := NewPosMS;
    if ABothCursor then FCursorMS := FPlayCursorMS;
    if Assigned(FOnPlayCursorChange) then FOnPlayCursorChange(Self);

    if not FCenterPlay then
    begin
      if not FMouseIsDown and ((NewPosMS < FPositionMS) or (NewPosMS > (FPositionMS + FPageSizeMS))) then
        SetPositionMS(NewPosMS - (FPageSizeMS div 8)) // Keep 1/8 of the previous display
      else
        UpdateView([uvfPlayCursor]);
    end
    else
    begin
      AMiddle := FPageSizeMS div 2;
      if not FMouseIsDown and
        ((NewPosMS >= (FPositionMS + AMiddle)) and ((NewPosMS - AMiddle) <= (FLengthMS - FPageSizeMS))) or
        ((NewPosMS < FPositionMS) or (NewPosMS > (FPositionMS + FPageSizeMS))) then
        SetPositionMS(NewPosMS - AMiddle)
      else
        UpdateView([uvfPlayCursor]);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.SetCursorMS(NewPosMS: Integer; const AUpdate: Boolean = False);
begin
  Constrain(NewPosMS, 0, FLengthMS);
  if FCursorMS <> NewPosMS then
  begin
    FCursorMS := NewPosMS;
    if AUpdate then UpdateView([uvfCursor]);
  end;
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplayer.IsOnlySelection: Boolean;
begin
  Result := (not SelectionIsEmpty) and (not Assigned(FSelectedSubtitle));
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplayer.SelectionIsEmpty: Boolean;
begin
  Result := (FDynamicSelection.FinalTime = 0);
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.ClearSelection;
begin
  SetSelectedSubtitleItem(NIL, False, False);
  if Assigned(FOnSelectionChange) then FOnSelectionChange(Self);
  UpdateView([uvfSelection, uvfSubtitle]);
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.SetSelectedSubtitleItem(const Value: PUWSubtitleItem; const UpdateDisplay: Boolean; const UpdatePosition: Boolean);
begin
  // Make sure to reset selection if Value is nil
  if (Value = NIL) then
  begin
    FDynamicSelection.InitialTime := 0;
    FDynamicSelection.FinalTime   := 0;
  end;

  if (FSelectedSubtitle <> Value) then
  begin
    FSelectedSubtitle := Value;
    if (FSelectedSubtitle <> NIL) then
    begin
      FDynamicSelection.InitialTime := FSelectedSubtitle^.InitialTime;
      FDynamicSelection.FinalTime   := FSelectedSubtitle^.FinalTime;

      SetMinBlankAt(FSelectedSubtitle^.InitialTime);
    end;

    if UpdatePosition and (Value <> NIL) then
      SetPositionMS(Value^.InitialTime);

    if UpdateDisplay and not UpdatePosition then
      UpdateView([uvfSelection, uvfSubtitle]);

    if Assigned(FOnSelectionChange) then FOnSelectionChange(Self);
  end;
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplayer.CheckSubtitleItemForDynamicSelection(const Subtitle: TUWSubtitleItem;
  const CursorPosMS, SubtitleSelWindow, X, Y: Integer): Boolean;
var
  NewDynamicEditMode : TDynamicEditMode;
  VisualStartX, VisualEndX, Sensitivity: Integer;
begin
  Result             := False;
  NewDynamicEditMode := demNone;
  Sensitivity        := 6; // Sensibilidad visual fija (6px)

  // 1. Obtener coordenadas visuales EXACTAS (incluye el ajuste de frames si está activo)
  VisualStartX := TimeToPixel(Subtitle.InitialTime - FPositionMS);
  VisualEndX   := TimeToPixel(Subtitle.FinalTime - FPositionMS);

  if (Y > GetSubCanvasY) and (Y < GetSubCanvasY+GetSubCanvasHeight) then
  begin
    // 2. Detección basada puramente en PÍXELES
    // Borde Inicial
    if (Abs(X - VisualStartX) <= Sensitivity) then
    begin
      NewDynamicEditMode := demInitial;
      FDynamicEditTimeMs := Subtitle.InitialTime;
    end
    // Borde Final
    else if (Abs(X - VisualEndX) <= Sensitivity) then
    begin
      NewDynamicEditMode := demFinal;
      FDynamicEditTimeMs := Subtitle.FinalTime;
    end
    // Cuerpo del subtítulo (Click central)
    else if (X > VisualStartX + Sensitivity) and (X < VisualEndX - Sensitivity) then
    begin
      NewDynamicEditMode := demWhole;
      FDynamicEditTimeMs := Subtitle.InitialTime;
    end
    // Caso especial: Subtítulos muy pequeños donde no caben los bordes
    else if ((VisualEndX - VisualStartX) <= (Sensitivity * 2)) and
            (X >= VisualStartX) and (X <= VisualEndX) then
    begin
      NewDynamicEditMode := demWhole;
      FDynamicEditTimeMs := Subtitle.InitialTime;
    end;

    // Asignar cursor
    if (NewDynamicEditMode = demWhole) then
      Cursor := crHandPoint
    else if (NewDynamicEditMode = demInitial) or (NewDynamicEditMode = demFinal) then
      Cursor := crHSplit;
  end;

  if (NewDynamicEditMode <> demNone) then
  begin
    Result := True;
    FDynamicEditMode := NewDynamicEditMode;
    FDynamicSelOldSub := FDynamicSelSub;

    if not FSubtitles.IsEqualItemTimes(Subtitle, FDynamicSelection) then
      FDynamicSelSub := @Subtitle
    else if Assigned(FSelectedSubtitle) then
      FDynamicSelSub := FSelectedSubtitle
    else
      FDynamicSelSub := NIL;
  end;

  if (FDynamicSelSub <> FDynamicSelOldSub) then
  begin
    if Assigned(FDynamicSelSub) and (NewDynamicEditMode <> demNone) then
      SetMinBlankAt(FDynamicSelSub^.InitialTime);
    if FDrawGAP then
      UpdateView([uvfSubtitle]);
  end;
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  UpdateFlags  : TUpdateViewFlags;
  ACursorPosMs : Integer;
  x1, x2, i,
  Dist         : Integer;
begin
  inherited;

  FPrevSubIdx := -1;
  FNextSubIdx := -1;
  UpdateFlags  := [];

  if (ssDouble in Shift) or (not IsTimeLineEnabled) or (not InRange(X, 0, Width)) then Exit;

  if IsTimeLineClicked(Y) then
  begin
    if Assigned(FOnTimeLineClickEvent) then
      FOnTimeLineClickEvent(Self, PixelToTime(X) + FPositionMS);

    Exit;
  end;

  if (ssLeft in Shift) and not (ssCtrl in Shift) then
  begin
    FMouseIsDown   := True;
    FOldMouseDownX := PixelToTime(X) + FPositionMS;

    // Re-adjust mouse cursor position
    if (FDynamicEditMode <> demNone) then
      X := TimeToPixel(FDynamicEditTimeMs - FPositionMS);

    ACursorPosMs := PixelToTime(X) + FPositionMS;

    // Snapping
    if (not (ssCtrl in Shift)) and (FSnappingEnabled) then
    begin
      Dist := FindCorrectedSnappingPoint(ACursorPosMs);
      if (Dist <> -1) then
      begin
        ACursorPosMs := Dist;
      end
    end;

    if (ssShift in Shift) or (FDynamicEditMode <> demNone) then
    begin
      if Assigned(FDynamicSelSub) and (FDynamicSelSub <> @FSelectedSubtitle) then
      begin
        SetSelectedSubtitleItem(FDynamicSelSub, False, False);
        Include(UpdateFlags, uvfSelection);
        if Assigned(FOnSelectedSubtitleItem) then FOnSelectedSubtitleItem(Self, FSubtitles.IndexOf(FSelectedSubtitle), FSelectedSubtitle^, True);
      end;

      // Selection modification using shift key
      if (ACursorPosMs > FDynamicSelection.InitialTime + ((FDynamicSelection.FinalTime - FDynamicSelection.InitialTime) div 2)) then
      begin
        // We are close to the end of the selection
        if SelectionIsEmpty then
        begin
          if (ACursorPosMs > FCursorMs) then
          begin
            FDynamicSelection.FinalTime   := ACursorPosMs;
            FDynamicSelection.InitialTime := FCursorMs;
          end
          else
          begin
            FDynamicSelection.FinalTime   := FCursorMs;
            FDynamicSelection.InitialTime := ACursorPosMs;
          end;
        end
        else
          FDynamicSelection.FinalTime := ACursorPosMs;

        FDynamicSelValueMs := FDynamicSelection.InitialTime;
      end
      else
      begin
        // We are close to the start of the selection
        FDynamicSelection.InitialTime := ACursorPosMs;
        FDynamicSelValueMs            := FDynamicSelection.FinalTime;
      end;

      if Assigned(FSelectedSubtitle) then
      begin
        //FNeedToSortList := True;
        FOldInitialTime := FSelectedSubtitle^.InitialTime;
        FOldFinalTime   := FSelectedSubtitle^.FinalTime;

        if (FDynamicEditMode <> demWhole) then
        begin
          FSelectedSubtitle^.InitialTime := FDynamicSelection.InitialTime;
          FSelectedSubtitle^.FinalTime   := FDynamicSelection.FinalTime;
        end;
        Include(UpdateFlags, uvfSubtitle);
        if Assigned(FOnSelectedSubtitleItem) and (FDynamicEditMode = demNone) then FOnSelectedSubtitleItemChange(Self);
      end;
      if Assigned(FOnSelectionChange) then FOnSelectionChange(Self);
      Include(UpdateFlags, uvfSelection);
    end
    else
    begin
      if (FDynamicSelection.InitialTime <> FDynamicSelection.FinalTime) then Include(UpdateFlags, uvfSelection); // clear selection
      FDynamicSelValueMs := ACursorPosMs;
      SetSelectedSubtitleItem(NIL, False, False);
    end;

    if FEnableMouseAntiOverlapping then
    begin
      // Clip mouse left/right position to avoid overlapp on previous/next subtitle
      x1 := 0;
      x2 := Width;
      FMinSelTime := -1;
      FMaxSelTime := -1;

      i := FSubtitles.FindInsertPos(ACursorPosMs, -1);
      if (i >= 0) then
      begin
        if Assigned(FSelectedSubtitle) then
        begin
          if (FDynamicEditMode = demWhole) and not (ssAlt in Shift) then
          begin
            i := FSubtitles.IndexOf(FSelectedSubtitle);
            if (i > 0) then
              FMinSelTime := FSubtitles[i-1].FinalTime + FGAP
            else
              FMinSelTime := 0;

            if (i < FSubtitles.Count-1) then
              FMaxSelTime := FSubtitles[i+1].InitialTime - FGAP
            else
              FMaxSelTime := FLengthMS;
          end
          else
          begin
            if (ACursorPosMs = FSelectedSubtitle^.InitialTime) then
            begin
              if (i > 0) then
              begin
                if not (ssAlt in Shift) then
                  FMinSelTime := FSubtitles[i-1].FinalTime + FGAP
                else
                begin
                  FPrevSubIdx := i-1;
                  FMinSelTime := FSubtitles[FPrevSubIdx].InitialTime + FGAP
                end;
                x1 := TimeToPixel(FMinSelTime - FPositionMs);
              end;
              FMaxSelTime := FSelectedSubtitle^.FinalTime - FGAP;
              x2 := TimeToPixel(FMaxSelTime - FPositionMs);
            end
            else
            begin
              // TODO : better change stop when ovelapping on next sub ???
              FMinSelTime := FSelectedSubtitle^.InitialTime + FGAP;
              x1 := TimeToPixel(FMinSelTime - FPositionMs);
              if(i < FSubtitles.Count) then
              begin
                if not (ssAlt in Shift) then
                  FMaxSelTime := FSubtitles[i].InitialTime - FGAP
                else
                begin
                  FNextSubIdx := i;
                  FMaxSelTime := FSubtitles[FNextSubIdx].FinalTime - FGAP
                end;
                x2 := TimeToPixel(FMaxSelTime - FPositionMs);
              end;
            end;
          end;
        end
        else
        begin
          if (i > 0) and (ACursorPosMs >= FSubtitles[i-1].InitialTime) and
            (ACursorPosMs <= FSubtitles[i-1].FinalTime) then
          begin
            // Selection only INSIDE subtitle range
          end
          else
          begin
            // Selection only OUTSIDE subtitle range
            if (i > 0) then
            begin
              FMinSelTime := FSubtitles[i-1].FinalTime  + FGAP;
              x1 := TimeToPixel(FMinSelTime - FPositionMs);
            end;

            if(i < FSubtitles.Count) then
            begin
              FMaxSelTime := FSubtitles[i].InitialTime - FGAP;
              x2 := TimeToPixel(FMaxSelTime - FPositionMs);
            end;
          end;
        end;
      end;
    end;
    Constrain(x1, 0, Width);
    Constrain(x2, 0, Width);
    if (FCursorMS <> ACursorPosMs) and (FDynamicEditMode = demNone) then
    begin
      FCursorMS := ACursorPosMs;
      if Assigned(FOnCursorChange) then FOnCursorChange(Self);
    end;

    UpdateView(UpdateFlags);
  end
  else if (ssLeft in Shift) and (ssCtrl in Shift) then
  begin
    FDynamicSelection.InitialTime := 0;
    FDynamicSelection.FinalTime   := 0;
    FCursorMs := PixelToTime(X) + FPositionMS;
    UpdateView([uvfCursor]);
  end;
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ACursorPosMs      : Integer;
  SubtitleUnder     : PUWSubtitleItem;
  SubtitleSelWindow : Integer;
  UpdateFlags       : TUpdateViewFlags;
  SubLen,
  NewIT, NewFT      : Integer;
  Dist              : Integer;
begin
  inherited;

  if (ssDouble in Shift) or (not IsTimeLineEnabled) then Exit;

  UpdateFlags := [];

  if (FMouseIsDown) then
  begin
    if (ssLeft in Shift) then
    begin
      Constrain(X, 0, Width);
      ACursorPosMS := PixelToTime(X) + FPositionMS;

      // Make sure to clip selection
      if(FMinSelTime <> -1) then
        Constrain(ACursorPosMS, FMinSelTime, MaxInt);

      if(FMaxSelTime <> -1) then
        Constrain(ACursorPosMS, 0, FMaxSelTime);

      // Snapping
      if (not (ssCtrl in Shift)) and (FSnappingEnabled) then
      begin
        Dist := FindCorrectedSnappingPoint(ACursorPosMS);
        if (Dist <> -1) then
          ACursorPosMS := Dist;
      end;

      if (FDynamicSelValueMs <> -1) and (FDynamicSelValueMs <> ACursorPosMS) then
      begin
        // Update selection
        if FDynamicEditMode <> demWhole then
        begin  //Move cursors
          ACursorPosMS := GetCorrectTimePos(ACursorPosMS);
          if (ACursorPosMS > FDynamicSelValueMs) then
          begin
            FDynamicSelection.InitialTime := FDynamicSelValueMs;
            FDynamicSelection.FinalTime   := ACursorPosMS;

            if (FNextSubIdx <> -1) then
            begin
              if ACursorPosMS >= FSubtitles[FNextSubIdx].InitialTime - Dist then
                FSubtitles.ItemPointer[FNextSubIdx]^.InitialTime := ACursorPosMS + FGAP;
            end;
          end
          else
          begin
            FDynamicSelection.InitialTime := ACursorPosMS;
            FDynamicSelection.FinalTime   := FDynamicSelValueMs;

            if (FPrevSubIdx <> -1) then
            begin
              if ACursorPosMS <= FSubtitles[FPrevSubIdx].FinalTime + Dist then
                FSubtitles.ItemPointer[FPrevSubIdx]^.FinalTime := ACursorPosMS - FGAP;
            end;
          end;
        end //Move cursors
        else if (FDynamicEditMode = demWhole) and Assigned(FSelectedSubtitle) then
        begin //Drag selection
          SubLen := Range(FOldFinalTime - FOldInitialTime, 0, FOldFinalTime);
          NewIT  := FOldInitialTime + ((PixelToTime(X) + FPositionMS) - FOldMouseDownX);
          NewIT := GetCorrectTimePos(NewIT);
          NewFT  := NewIT + SubLen;

          if FEnableMouseAntiOverlapping and not (ssAlt in Shift) then
          begin
            if NewFT >= FMaxSelTime then
            begin
              NewFT := FMaxSelTime;
              NewIT := NewFT-SubLen;
            end
            else if NewIT <= FMinSelTime then
            begin
              NewIT := FMinSelTime;
              NewFT := NewIT+SubLen;
            end;
          end;

          FDynamicSelection.InitialTime := Range(NewIT, 0, FLengthMS-SubLen);
          FDynamicSelection.FinalTime   := Range(NewFT, SubLen, FLengthMS);
        end; //Drag selection

        if Assigned(FSelectedSubtitle) then
        begin
          FNeedToSortList := True;

          if (FSelectedSubtitle^.InitialTime <> FDynamicSelection.InitialTime) or
             (FSelectedSubtitle^.FinalTime <> FDynamicSelection.FinalTime) then
          begin
            FSelectedSubtitle^.InitialTime := FDynamicSelection.InitialTime;
            FSelectedSubtitle^.FinalTime   := FDynamicSelection.FinalTime;

            Include(UpdateFlags, uvfSubtitle);
            if Assigned(FOnSelectedSubtitleItemChange) then FOnSelectedSubtitleItemChange(Self);
          end;
        end;
        if Assigned(FOnSelectionChange) then FOnSelectionChange(Self);
        Include(UpdateFlags, uvfSelection);
      end;

      if (FCursorMS <> ACursorPosMS) and (FDynamicEditMode = demNone) then
      begin
        FCursorMS := ACursorPosMS;
        if Assigned(FOnCursorChange) then FOnCursorChange(Self);
        Include(UpdateFlags, uvfCursor);
      end;
    end;
  end
  else
  begin
    Constrain(X, 0, Width);
    ACursorPosMS := PixelToTime(X) + GetCorrectTimePos(FPositionMS);

    // "Dynamic selection"
    if (Shift = []) or (ssAlt in Shift) then
    begin
      // Find a subtitle under the mouse
      SubtitleSelWindow := PixelToTime(6);
      if (SubtitleSelWindow < 1) then SubtitleSelWindow := 1;

      if FFPSTimeMode and (FFPS > 0) then
        SubtitleSelWindow := Max(SubtitleSelWindow, Round(1000 / FFPS) + 2);

      // First pass : check only inside sub
      SubtitleUnder := FSubtitles.FindFirstPointer(ACursorPosMS, 0);
      while Assigned(SubtitleUnder) do
      begin
        if CheckSubtitleItemForDynamicSelection(SubtitleUnder^, ACursorPosMS, SubtitleSelWindow, X, Y) then Exit;
        SubtitleUnder := FSubtitles.FindNextPointer;
      end;

      // 2nd pass : Wider search
      SubtitleSelWindow := PixelToTime(4);
      if (SubtitleSelWindow < 1) then SubtitleSelWindow := 1;

      if FFPSTimeMode and (FFPS > 0) then
        SubtitleSelWindow := Max(SubtitleSelWindow, Round(1000 / FFPS) + 2);

      SubtitleUnder := FSubtitles.FindFirstPointer(ACursorPosMS, SubtitleSelWindow);
      while Assigned(SubtitleUnder) do
      begin
        if CheckSubtitleItemForDynamicSelection(SubtitleUnder^, ACursorPosMS, SubtitleSelWindow, X, Y) then Exit;
        SubtitleUnder := FSubtitles.FindNextPointer;
      end;

      // Check selection
     {if not SelectionIsEmpty then
      begin
        SubtitleSelWindow := PixelToTime(4);
        if (SubtitleSelWindow < 1) then SubtitleSelWindow := 1;
        if CheckSubtitleItemForDynamicSelection(FDynamicSelection, ACursorPosMS, SubtitleSelWindow, X, Y) then Exit;
      end;}
    end;

    if SetMinBlankAt(ACursorPosMs) then
      Include(UpdateFlags, uvfSubtitle);

    Cursor            := crDefault;
    FDynamicEditMode  := demNone;
    FDynamicSelOldSub := FDynamicSelSub;
    FDynamicSelSub    := NIL;

    if (FDynamicSelSub <> FDynamicSelOldSub) then
      Include(UpdateFlags, uvfSubtitle);
  end;

  UpdateView(UpdateFlags);
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if (FMouseIsDown) then
  begin
    if ((FDynamicSelection.FinalTime - FDynamicSelection.InitialTime) < 40) and
      (not (Assigned(FSelectedSubtitle) or Assigned(FDynamicSelSub))) then
      ClearSelection;

    // The selected sub has changed, we need to keep Subtitle list sorted
    if FNeedToSortList then
    begin
      if Assigned(FOnItemChangedEvent) and Assigned(FSelectedSubtitle) then
        FOnItemChangedEvent(Self, FSubtitles.IndexOf(FSelectedSubtitle), FOldInitialTime, FOldFinalTime, FNeedToSortList);

      // Make sure we keep the list sorted internally
      FSubtitles.Sort;
      FNeedToSortList := False;
    end;

    if FDynamicEditMode = demNone then
      Cursor := crDefault;

    FDynamicSelValueMs := -1;
    FDynamicEditMode   := demNone;
    FMouseIsDown       := False;
    FDynamicSelSub     := NIL;
    FMinSelTime        := -1;
    FMaxSelTime        := -1;
    FPrevSubIdx        := -1;
    FNextSubIdx        := -1;
    FCursorMS          :=  GetCorrectTimePos(PixelToTime(X) + FPositionMS);

    UpdateView([uvfSelection, uvfSubtitle]);
  end;
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.Click;
begin
  if not IsTimeLineEnabled then inherited;
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.DblClick;
{var
  idx : Integer;
  Sub : PUWSubtitleItem;}
begin
  if not IsTimeLineEnabled then Exit;

  if Assigned(FOnTimeLineClickEvent) then
    FOnTimeLineClickEvent(Self, FOldMouseDownX);

{  // Detect double click on start or stop timestamp
  inherited;

  if Assigned(FDynamicSelSub) then
  begin
    case FDynamicEditMode of
      demInitial : if Assigned(FOnSubtitleItemStartDblClick) then
                   begin
                     FOnSubtitleItemStartDblClick(Self, FDynamicSelSub^);
                     Exit;
                   end;
      demFinal   : if Assigned(FOnSubtitleItemStopDblClick) then
                   begin
                     FOnSubtitleItemStopDblClick(Self, FDynamicSelSub^);
                     Exit;
                   end;
    end;
  end
  else
  begin
    idx := FSubtitles.FindFirst(FCursorMS);
    if idx = -1 then
      Sub := NIL
    else
      Sub := FSubtitles.ItemPointer[idx];

    // Full subtitle selection
    SetSelectedSubtitleItem(Sub, False, False);
    UpdateView([uvfSelection, uvfSubtitle]);
    if Assigned(FOnSelectedSubtitleItem) then FOnSelectedSubtitleItem(Self, FSubtitles.IndexOf(FSelectedSubtitle), FSelectedSubtitle^, False);
  end;}
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplayer.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
var
  ScrollAmount : Integer;
begin
  Result := True;

  ScrollAmount := Round(FPageSizeMS / 4); // scroll amount = 1/4 of visible interval
  if (ScrollAmount = 0) then ScrollAmount := 1;

  if WheelDelta > 0 then
  begin
    if ssShift in Shift then
      ZoomIn
    else if ssCtrl in Shift then
      VZoomLess
    else
      SetPositionMS(FPositionMs + ScrollAmount);
  end
  else
  begin
    if ssShift in Shift then
      ZoomOut
    else if ssCtrl in Shift then
      VZoomMore
    else
      SetPositionMS(FPositionMs - ScrollAmount);
  end;

  UpdateView([uvfPageSize]);
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.Close;
begin
  FPeakDataLoaded        := False;
  FPeakTab               := NIL;
  FPeakTabSize           := 0;
  FSamplesPerPeak        := 0;

  FFileName              := '';
  FPageSizeMS            := 0;
  FPositionMs            := 0;
  FDynamicSelection.InitialTime := 0;
  FDynamicSelection.FinalTime   := 0;
  FDynamicEditMode       := demNone;
  FDynamicSelSub         := NIL;
  FDynamicEditTimeMs     := 0;
  FCursorMS              := 0;
  FPlayCursorMS          := 0;
  FLengthMS              := 0;
  FDynamicSelValueMs     := -1;
  FOldInitialTime        := -1;
  FOldFinalTime          := -1;
  FMinSelTime            := -1;
  FMaxSelTime            := -1;
  FPrevSubIdx            := -1;
  FNextSubIdx            := -1;

  FMouseIsDown           := False;
  FNeedToSortList        := False;

  FillByte(FWaveFormat, SizeOf(FWaveFormat), 0);

  FScrollBar.PageSize := 0;
  FScrollBar.Position := 0;
  FScrollBar.Min      := 0;
  FScrollBar.Max      := 0;

  ClearSceneChange;
  UpdateView([uvfPageSize]);

  Cursor := crDefault;
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplayer.LoadWaveFromFile(const FileName: String): Boolean;
var
  PeakFileName    : String;
  PeakFS          : TFileStream;
  PeakFileIDRead  : String;
  PeakFileVerRead : Cardinal;
  WAVFile         : TUWWAVEFile;
  HDRSize         : Integer;
  CreatePeakFile  : Boolean;
  Normalized      : Boolean;
  LengthMS        : Integer;

const
  PeakFileID  : AnsiString = 'PeakFile';
  PeakFileVer : Cardinal   = $0100;

  procedure SavePeakFile;
  begin
    PeakFS := TFileStream.Create(PeakFileName, fmCreate);
    try
      with PeakFS do
      begin
        WriteBuffer(PeakFileID[1], Length(PeakFileID));
        WriteBuffer(PeakFileVer, SizeOf(PeakFileVer));
        WriteBuffer(LengthMS, SizeOf(FLengthMS));
        WriteBuffer(FWaveFormat.nSamplesPerSec, SizeOf(FWaveFormat.nSamplesPerSec));
        WriteBuffer(FWaveFormat.nChannels, SizeOf(FWaveFormat.nChannels));
        WriteBuffer(FWaveFormat.wBitsPerSample, SizeOf(FWaveFormat.wBitsPerSample));
        WriteBuffer(FSamplesPerPeak, SizeOf(FSamplesPerPeak));
        WriteBuffer(FPeakTabSize, SizeOf(FPeakTabSize));
        WriteBuffer(FPeakTab[0], FPeakTabSize*SizeOf(TPeak));
      end;
    finally
      PeakFS.Free;
    end;
  end;

begin
  Result := False;
  ClearPeakData;
  FPeakDataLoaded := False;
  CreatePeakFile  := True;

  // Search for a "peak" file with the same name
  PeakFileName := ChangeFileExt(FileName, '.peak');
  if FileExists(PeakFileName) then
  begin
    // Load peak file
    PeakFS := TFileStream.Create(PeakFileName, fmOpenRead or fmShareDenyWrite);
    try
      // Check filesize, we need at least
      HDRSize := Length(PeakFileID) + SizeOf(PeakFileVerRead) + SizeOf(FLengthMs) +
        SizeOf(FWaveFormat.nSamplesPerSec) + SizeOf(FWaveFormat.nChannels) +
        SizeOf(FWaveFormat.wBitsPerSample) + SizeOf(FSamplesPerPeak) +
        SizeOf(FPeakTabSize);

      if (PeakFS.Size > HDRSize) then
      begin
        SetLength(PeakFileIDRead, Length(PeakFileID));
        PeakFS.ReadBuffer(PeakFileIDRead[1], Length(PeakFileID));
        PeakFS.ReadBuffer(PeakFileVerRead, SizeOf(PeakFileVerRead));
        PeakFS.ReadBuffer(LengthMS, SizeOf(LengthMS));
        PeakFS.ReadBuffer(FWaveFormat.nSamplesPerSec, SizeOf(FWaveFormat.nSamplesPerSec));
        PeakFS.ReadBuffer(FWaveFormat.nChannels, SizeOf(FWaveFormat.nChannels));
        PeakFS.ReadBuffer(FWaveFormat.wBitsPerSample, SizeOf(FWaveFormat.wBitsPerSample));
        PeakFS.ReadBuffer(FSamplesPerPeak, SizeOf(FSamplesPerPeak));
        PeakFS.ReadBuffer(FPeakTabSize, SizeOf(FPeakTabSize));
        FPeakTab := NIL;
        SetLength(FPeakTab, FPeakTabSize);
        PeakFS.Read(FPeakTab[0], FPeakTabSize * SizeOf(TPeak));

        Normalized := NormalizePeakTab(-1);
        if Normalized then
        begin
          // rewrite normalized data
          SavePeakFile;
        end;

        CreatePeakFile := False;
      end;
    finally
      PeakFS.Free;
    end;
  end;

  if CreatePeakFile then
  begin
    // No wave file
    if not FileExists(FileName) then Exit;

    WAVFile := TUWWAVEFile.Create;
    if not WAVFile.Open(FileName) then
    begin
      WAVFile.Free;
      Exit;
    end;

    LengthMS    := WAVFile.Duration;
    FWaveFormat := WAVFile.GetWaveFormatEx^;
    // Create the "peak" file
    CreatePeakTab(WAVFile);
    // Save it
    if FSavePeakToFile then SavePeakFile;

    WAVFile.Close;
    WAVFile.Free;
  end;

  FPageSizeMS := DefaultPageSizeMS;
  SetLengthMS(LengthMS);
  FPeakDataLoaded := True;
  FFileName := PeakFileName;
  Result := True;
  UpdateView([uvfPageSize]);
end;

// -----------------------------------------------------------------------------

procedure TUWWaveformDisplayer.SetLengthMS(const LenghtMS: Integer);
begin
  if FPeakDataLoaded then Exit;

  if (FLengthMS <> LenghtMS) then
  begin
    FLengthMS := LenghtMS;
    if (FPositionMS + FPageSizeMS > LenghtMS) then
    begin
      if (FPageSizeMS < LenghtMS) then
        FPositionMS := LenghtMS - FPageSizeMS
      else
      begin
        FPositionMS := 0;
        FPageSizeMS := LenghtMS;
      end;
    end;

    FScrollBar.Max      := LenghtMs;
    FScrollBar.PageSize := FPageSizeMs;
    FScrollBar.Position := FPositionMs;
  end;
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.SetPositionMS(NewPosition: Integer);
begin
  Constrain(NewPosition, 0, FLengthMS - FPageSizeMS);
  if NewPosition <> FPositionMS then
  begin
    FPositionMS         := NewPosition;
    FScrollBar.Position := FPositionMS;
    UpdateView([uvfPageSize, uvfPosition]);
    if Assigned(FOnViewChange) then FOnViewChange(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.OnScrollBarChange(Sender: TObject);
begin
  FPositionMS := GetPositionMS;
  UpdateView([uvfPageSize]);
  if Assigned(FOnViewChange) then FOnViewChange(Self);
end;

//------------------------------------------------------------------------------

// Called only internally when the scroll bar change
function TUWWaveformDisplayer.GetPositionMS: Integer;
begin
  if (FScrollBar.Position + FPageSizeMS - 1) > FLengthMS then
    Result := (FLengthMS - FPageSizeMS)
  else
    Result := FScrollBar.Position;
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.UpdateView(const UpdateViewFlags: TUpdateViewFlags; const DoRepaint: Boolean = True);
begin
  if (UpdateViewFlags = []) then Exit;

  if (uvfPageSize in UpdateViewFlags) then
  begin
    FBackBufferWAVE.SetSize(Width, Height);
    FBackBuffer.SetSize(FBackBufferWAVE.Width, FBackBufferWAVE.Height);
    FBackBufferItems.SetSize(FBackBufferWAVE.Width, FBackBufferWAVE.Height);
  end;

  if not IsTimeLineEnabled then
  begin
    FBackBufferWAVE.Canvas.Brush.Color := CustomColors.Background;
    FBackBufferWAVE.Canvas.FillRect(FBackBufferWAVE.Canvas.ClipRect);
    FBackBuffer.Canvas.Brush.Color := CustomColors.Background;
    FBackBuffer.Canvas.FillRect(FBackBuffer.Canvas.ClipRect);
    DrawGridLines(FBackBuffer);

    {$IFDEF USEBGRABITMAP}
    FBackBuffer.FontName   := 'Tahoma';
    FBackBuffer.FontHeight := 11;
    {$ELSE}
    FBackBuffer.Canvas.Brush.Style := bsClear;
    FBackBuffer.Canvas.Font.Name   := 'Tahoma';
    FBackBuffer.Canvas.Font.Size   := 8;
    FBackBuffer.Canvas.Font.Color  := CustomColors.Text;
    {$ENDIF}

    if not FEmptyText.IsEmpty then
    begin
      {$IFDEF USEBGRABITMAP}
      FBackBuffer.TextRect(
        Rect(FBackBuffer.ClipRect.Left, FBackBuffer.ClipRect.Top, FBackBuffer.ClipRect.Right, FBackBuffer.ClipRect.Bottom),
        FEmptyText, taCenter, tlCenter, CustomColors.Text);
      {$ELSE}
      FTS.Alignment := taCenter;
      FTS.Layout    := tlCenter;
      with FBackBuffer.Canvas do
        TextRect(ClipRect, ClipRect.Left, ClipRect.Top, FEmptyText, FTS);
      {$ENDIF}
    end;

    {$IFDEF USEBGRABITMAP}
    FBackBuffer.Rectangle(FBackBuffer.Canvas.ClipRect, BGRA(40, 40, 40)); // Some border
    {$ELSE}
    FBackBuffer.Canvas.Pen.Color := RGBToColor(40, 40, 40);
    FBackBuffer.Canvas.Rectangle(FBackBuffer.Canvas.ClipRect); // Some border
    {$ENDIF}

    Invalidate;
    Exit;
  end;

  if (uvfPageSize in UpdateViewFlags) then
  begin
    CalculateTimeDivisionStepMs;
    DrawWave(FBackBufferWAVE);

    FOldPositionMs := FPositionMs;
    FOldPageSizeMs := FPageSizeMs;

    DrawTimeLine(FBackBufferWAVE);

    if FDrawThumbnails then
      DrawThumbnails(FBackBufferWAVE, 0, GetSubCanvasHeight);
  end
  else if (uvfPosition in UpdateViewFlags) then
  begin
    // Maybe we can draw only a part of the WAV
    DrawWave(FBackBufferWAVE, True);

    FOldPositionMs := FPositionMs;
    FOldPageSizeMs := FPageSizeMs;

    DrawTimeLine(FBackBufferWAVE);

    if FDrawThumbnails then
      DrawThumbnails(FBackBufferWAVE, 0, GetSubCanvasHeight);
  end;

  if (uvfPageSize in UpdateViewFlags) or (uvfSubtitle in UpdateViewFlags) then
    DrawItemsCanvas(True)
  else
    DrawItemsCanvas(False);

  //if (uvfCursor in UpdateViewFlags) then
    DrawCursor(FBackBuffer);

  //if (uvfSelection in UpdateViewFlags) then
    DrawSelection(FBackBuffer);

  //if (uvfPlayCursor in UpdateViewFlags) then
    DrawPlayCursor(FBackBuffer);

  if DoRepaint then Invalidate;
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.ZoomSubtitle(const Subtitle: TUWSubtitleItem);
const
  MaxZoom = 200;

var
  NewPosition, NewPageSize : Integer;
  UpdateFlags : TUpdateViewFlags;
begin
  if (Subtitle.InitialTime >= Subtitle.FinalTime) then Exit;

  NewPageSize := Subtitle.FinalTime - Subtitle.InitialTime;
  if (NewPageSize < MaxZoom) then
  begin
    NewPageSize := MaxZoom; // Zoom to max ms
    NewPosition := Subtitle.InitialTime + ((Subtitle.FinalTime - Subtitle.InitialTime - NewPageSize) div 2);
  end
  else
  begin
    Constrain(NewPageSize, 0, FLengthMs);
    NewPosition := Subtitle.InitialTime;
  end;

  Constrain(NewPosition, 0, FLengthMs - NewPageSize);
  FScrollBar.SetPositionAndPageSize(NewPosition, NewPageSize);

  UpdateFlags := [];
  if (NewPosition <> FPositionMs) or (NewPageSize <> FPageSizeMs) then
  begin
    if(NewPageSize <> FPageSizeMs) then Include(UpdateFlags, uvfPageSize);
    FPageSizeMs := NewPageSize;
    if(NewPosition <> FPositionMs) then Include(UpdateFlags, uvfPosition);
    FPositionMs := NewPosition;
    UpdateView(UpdateFlags);
    if Assigned(FOnViewChange) then FOnViewChange(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.ZoomSubtitle(const Start, Stop: Integer);
var
  Subtitle: TUWSubtitleItem;
begin
  Subtitle.InitialTime := Start;
  Subtitle.FinalTime   := Stop;
  ZoomSubtitle(Subtitle);
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.ZoomAll;
begin
  ZoomSubtitle(0, FLengthMs);
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.ZoomIn;
begin
  ZoomSubtitle(FPositionMS + Round(FPageSizeMS / 3), FPositionMS + Round((FPageSizeMS / 3) * 2));
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.ZoomOut;
begin
  ZoomSubtitle(FPositionMS - FPageSizeMS, FPositionMS + (FPageSizeMS * 2));
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.ZoomSelection;
begin
  ZoomSubtitle(FDynamicSelection.InitialTime, FDynamicSelection.FinalTime);
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.VZoomRestore(const AInvalidate: Boolean = False);
begin
  FVerticalScaling := 100;
  if AInvalidate then UpdateView([uvfPageSize]);
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.VZoomMore(const AInvalidate: Boolean = False);
begin
  if InRange(FVerticalScaling, 5, 400) then Dec(FVerticalScaling, 5);
  if AInvalidate then UpdateView([uvfPageSize]);
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.VZoomLess(const AInvalidate: Boolean = False);
begin
  if InRange(FVerticalScaling, 0, 395) then Inc(FVerticalScaling, 5);
  if AInvalidate then UpdateView([uvfPageSize]);
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.SetSelectionAndPos(const Start, Stop: Integer);
begin
  FDynamicSelection.InitialTime := Start;
  FDynamicSelection.FinalTime   := Stop;
  SetPositionMS(Start);
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.SelectSubtitle(const Index: Integer; const UpdateDisplay: Boolean; const UpdatePosition: Boolean);
var
  ASubtitle: PUWSubtitleItem;
begin
  if FSubtitles.ValidIndex(Index) then
    ASubtitle := FSubtitles.ItemPointer[Index]
  else
    Exit;

  // Full subtitle selection
  SetSelectedSubtitleItem(ASubtitle, UpdateDisplay, UpdatePosition);
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplayer.GetSubtitleIdxAtCursorPos: Integer;
begin
  Result := FSubtitles.FindFirst(FCursorMS);
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.DoUpdate(const Complete: Boolean = True);
begin
  if Complete then
    UpdateView([uvfPageSize])
  else
    UpdateView([uvfSelection]);

  SetMinBlankAt(-1);
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.SetEmptyText(const AText: String);
begin
  FEmptyText := AText;
  if not IsTimeLineEnabled then
    UpdateView([uvfPosition]);
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplayer.IsTimeLineEnabled: Boolean;
begin
  Result := FLengthMS > 0;
end;

//------------------------------------------------------------------------------

{$I WAVDisplayer_PeakFile.inc}
{$I WAVDisplayer_Draw.inc}
{$I WAVDisplayer_SceneChange.inc}
{$I WAVDisplayer_SilentZone.inc}

//------------------------------------------------------------------------------

procedure RegisterWaveformDisplayer;
begin
  RegisterComponents('URUWorks Tero', [TUWWaveformDisplayer]);
end;

procedure Register;
begin
  RegisterUnit('WAVDisplayer', @RegisterWaveformDisplayer);
end;

// -----------------------------------------------------------------------------

initialization
  {$I TeroControls.lrs}

// -----------------------------------------------------------------------------

end.

