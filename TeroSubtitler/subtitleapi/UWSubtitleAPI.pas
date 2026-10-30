{*
 *  URUWorks Subtitle API
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
 *  Copyright (C) 2001-2026 URUWorks, uruworks@gmail.com.
 *}

unit UWSubtitleAPI;

// -----------------------------------------------------------------------------

interface

uses
  Classes, SysUtils, FGL, UWSubtitleAPI.TimeCode, UWSubtitleAPI.Formats,
  UWSubtitleAPI.ExtraInfo, UWSystem.TimeUtils, Math, LazUTF8;

type

  { TUWStringList }

  PStringItemList = ^TStringItemList;
  TStringItemList = array[0..MaxListSize] of String;

  TUWStringList = class
  private
    FFileName  : String;
    FList      : PStringItemList;
    FCount     : Integer;
    FCapacity  : Integer;
    FLineBreak : String;
    FCodePage  : Cardinal;
    FWriteBOM  : Boolean;
    FBlankBefore : array of Boolean; // por linea: la precedia una linea en blanco?
    FBlankValid  : Boolean;          // solo es fiable justo despues de SetTextStr
    FInSetText   : Boolean;
    function Get(Index: Integer): String;
    procedure Put(Index: Integer; const S: String);
    procedure SetCapacity(const NewCapacity: Integer);
    procedure Grow;
    function GetTextStr: String;
    procedure SetTextStr(const Value: String);
  public
    constructor Create(const FileName: String = ''; Encoding: TEncoding = NIL; const NameIsFile: Boolean = True);
    destructor Destroy; override;
    procedure LoadFromString(const S: String);
    procedure LoadFromFile(const FileName: String; Encoding: TEncoding);
    procedure SaveToFile(const FileName: String; Encoding: TEncoding);
    procedure SaveToStream(Stream: TStream; Encoding: TEncoding);
    function ValidIndex(const Index: Integer): Boolean;
    function Add(const S: String; const SkipEmptyLine: Boolean = False): Integer;
    procedure Insert(const Index: Integer; const S: String; const SkipEmptyLine: Boolean = False);
    procedure Move(const CurIndex, NewIndex: Integer);
    procedure Delete(const Index: Integer);
    procedure Clear;
    // Las lineas vacias se descartan al cargar; BlockStart(i) dice si la linea
    // i abria un bloque (la precedia una linea vacia o es la primera)
    function BlockStart(const Index: Integer): Boolean;
    property HasBlockInfo: Boolean read FBlankValid;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount;
    property LineBreak: String read FLineBreak write FLineBreak;
    property Strings[Index: Integer]: String read Get write Put; default;
    property Text: String read GetTextStr write SetTextStr;
    property FileName: String read FFileName;
    property CodePage: Cardinal read FCodePage;
    property WriteBOM: Boolean read FWriteBOM write FWriteBOM;
  end;

  { TUWSubtitles }

  TSubtitleErrorType =
  (
    etBadValues,
    etTimeTooLong,
    etTimeTooShort,
    etPauseTooShort,
    etMaxCPS,
    etOverlapping,
    etFixTags,
    etEmpty,
    etUnbreak,
    etUnnecessarySpaces,
    etUnnecessaryDots,
    etRepeatedChars,
    etProhibitedChars,
    etHearingImpaired,
    etBreakLongLines,
    etRepeatedSubtitle,
    etEllipsesSingleSmartCharacter,
    etOCR,
    etMaxLines,
    etIncompleteHyphenText,
    etSpaceOfOpeningHyphen,
    etRemoveSpacesWithinBrackets,
    etFixInterrobang,
    etOverlappingWithPrev,
    etOverlappingWithNext,
    etSnapToShotChanges,
    etSnapToShotChangesInCue,
    etSnapToShotChangesOutCue,
    etSnapToShotChangesInCueAway,
    etChaining,
    etCleanupTags,
    etStraightToCurlyQuotes,
    etCurlyToStraightQuotes,
    etFixBrackets,
    etNone
  );

  TSubtitleErrorTypeSet = set of TSubtitleErrorType;
  TSubtitleMode = (smText, smTranslation);
  TSubtitleTimeBase = (stbMedia, stbSMPTE);
  TSubtitleHAlign = (shaNone, shaLeft, shaCenter, shaRight);
  TSubtitleVAlign = (svaBottom, svaCenter, svaTop);
  TSubtitleLoadDataFunc = function(const AData: Pointer; const ADataClass: ShortString): Integer;

  PUWSubtitleItem = ^TUWSubtitleItem;
  TUWSubtitleItem = record
    Text,
    Translation : String;
    InitialTime,
    FinalTime   : TUWTimeCode;
    Marked      : Boolean;
    ErrorType   : TSubtitleErrorTypeSet;
    Align       : TSubtitleHAlign;
    VAlign      : TSubtitleVAlign;
    R           : TRect;
    Style       : String;
    Actor       : String;
    Notes       : String;
    Data        : Integer;
  end;

  TUWSubtitleItemList = specialize TFPGList<PUWSubtitleItem>;

  TUWSubtitles = class
  private
    FFormat           : TUWSubtitleFormats;
    FFPS              : Double;
    FTimecodeMode     : TTimecodeMode;
    FEIType           : TUWSubtitleExtraInfoType;
    FList             : TUWSubtitleItemList;
    FExtraInfo        : TList;
    FHeader           : Pointer;
    FStyles           : TList;
    FCodePage         : Integer;
    FSearchStartAt    : Integer;
    FSearchIdx        : Integer;
    FSearchSkip       : Integer;
    FWriteBOM         : Boolean;
    FAutoSort         : Boolean;
    FReplaceEntity    : Boolean;
    FIsRTL            : Boolean;
    FTag              : Byte;
    FTimeBase         : TSubtitleTimeBase;
    FFormatProperties : PFormatProperties;
    FLoadDataFunc     : TSubtitleLoadDataFunc;
    procedure CheckIsRightToLeft;
    function GetCount: Integer;
    procedure SetFormat(const Format: TUWSubtitleFormats);
    function GetItem(Index: Integer): TUWSubtitleItem;
    function GetItemPointer(Index: Integer): PUWSubtitleItem;
    procedure PutItem(Index: Integer; const Item: TUWSubtitleItem);
    function GetText(Index: Integer): String;
    procedure PutText(Index: Integer; const S: String);
    function GetTranslation(Index: Integer): String;
    procedure PutTranslation(Index: Integer; const S: String);
    function GetInitialTime(Index: Integer): TUWTimeCode;
    procedure PutInitialTime(Index: Integer; const Time: TUWTimeCode);
    function GetInitialFrames(Index: Integer; FPS: Double): Integer;
    function GetFinalTime(Index: Integer): TUWTimeCode;
    procedure PutFinalTime(Index: Integer; const Time: TUWTimeCode);
    function GetFinalFrames(Index: Integer; FPS: Double): Integer;
    function GetDuration(Index: Integer): TUWTimeCode;
    procedure PutDuration(Index: Integer; const Time: TUWTimeCode);
    function GetDurationFrames(Index: Integer; FPS: Double): Integer;
    function GetPause(Index: Integer): TUWTimeCode;
    procedure PutPause(Index: Integer; const Time: TUWTimeCode);
    function GetPauseFrames(Index: Integer; FPS: Double): Integer;
    function GetExtraInfo(Index: Integer): Pointer;
    procedure SetExtraInfo(Index: Integer; const P: Pointer);
    function GetStringCPS(Index: Integer; const IsOriginal: Boolean; const SkipChars: String = ''): Double;
    function GetTextCPS(Index: Integer; const SkipChars: String = ''): Double;
    function GetTranslationCPS(Index: Integer; const SkipChars: String = ''): Double;
    function GetStringWPM(Index: Integer; const IsOriginal: Boolean): Double;
    function GetTextWPM(Index: Integer): Double;
    function GetTranslationWPM(Index: Integer): Double;
    function FixExtension(const AFileName, AExtension: String): String;
  public
    constructor Create;
    destructor Destroy; override;
    function ValidIndex(const Index: Integer): Boolean;
    procedure Assign(Source: TUWSubtitles);
    function Add(const AInitialTime, AFinalTime: TUWTimeCode; const AText, ATranslation: String; const AExtraInfo: Pointer = NIL; const SkipInvalidValues: Boolean = False): Integer; overload;
    function Add(const Item: TUWSubtitleItem; const ExtraInfo: Pointer = NIL; const SkipInvalidValues: Boolean = False): Integer; overload;
    procedure Insert(const Index: Integer; const AInitialTime, AFinalTime: TUWTimeCode; const AText, ATranslation: String; const ExtraInfo: Pointer = NIL; const SkipInvalidValues: Boolean = False); overload;
    procedure Insert(const Index: Integer; const Item: TUWSubtitleItem; const ExtraInfo: Pointer = NIL); overload;
    procedure Move(const CurIndex, NewIndex: Integer);
    procedure Delete(const Index: Integer);
    procedure Clear;
    function IndexOf(Item: PUWSubtitleItem): Integer; overload;
    function IndexOf(const PosMS: Integer): Integer; overload;
    function FindInsertPos(const Item: TUWSubtitleItem): Integer; overload;
    function FindInsertPos(const InitialTime, FinalTime: TUWTimeCode): Integer; overload;
    function FindFirst(const MSecs: TUWTimeCode; const Skip: Integer = 0): Integer;
    function FindFirstPointer(const MSecs: TUWTimeCode; const Skip: Integer = 0): PUWSubtitleItem;
    function FindNext: Integer;
    function FindNextPointer: PUWSubtitleItem;
    procedure Sort;
    function IsEqualItemTimes(const I1, I2: TUWSubtitleItem): Boolean;
    function IsEqualItem(const I1, I2: TUWSubtitleItem): Boolean;
    function GetFormatInfoFromFile(const AFileName: String; out AFormat: TUWSubtitleFormats; out ATimeBase: TSubtitleTimeBase): Boolean;
    function GetFormatFromFile(const AFileName: String): TUWSubtitleFormats;
    function GetTimeBaseFromFile(const AFileName: String): TSubtitleTimeBase;
    function LoadFromFile(const FileName: String; Encoding: TEncoding; const FPS: TFPSInfo; const Format: TUWSubtitleFormats = sfInvalid; const ClearAll: Boolean = True; const NameIsFile: Boolean = True): Boolean;
    function LoadFromString(const S: String; Encoding: TEncoding; const FPS: TFPSInfo; const Format: TUWSubtitleFormats = sfInvalid; const ClearAll: Boolean = True): Boolean;
    function SaveToFile(const FileName: String; const FPS: TFPSInfo; const Encoding: TEncoding; const Format: TUWSubtitleFormats; SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1; const FixFileName: Boolean = False): Boolean;
    function SaveToString(const FPS: Double; const Encoding: TEncoding; const Format: TUWSubtitleFormats; SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): String;
    function FillDialogFilter(AllSupportedText: String = 'All supported files'): String;
    function FixFileNameExtension(const AFileName: String; const AFormat: TUWSubtitleFormats): String;
    function FormatExtensions(const AFormat: TUWSubtitleFormats): String;
    procedure DoFPSConversion(const AOldFPS, ANewFPS: Double);
    procedure ConvertTimesToSMPTE(const AValue: Boolean; const AFromIndex: Integer = 0);
    function IsSMPTESupported(const AFormat: TUWSubtitleFormats = sfInvalid): Boolean;
    property Count: Integer read GetCount;
    property Format: TUWSubtitleFormats read FFormat write SetFormat;
    property CodePage: Integer read FCodePage;
    property FrameRate: Double read FFPS write FFPS;
    property TimecodeMode: TTimecodeMode read FTimecodeMode write FTimecodeMode;
    property WriteBOM: Boolean read FWriteBOM write FWriteBOM;
    property AutoSort: Boolean read FAutoSort write FAutoSort; // Auto sort subtitles on load
    property Items[Index: Integer]: TUWSubtitleItem read GetItem write PutItem; default;
    property ItemPointer[Index: Integer]: PUWSubtitleItem read GetItemPointer;
    property Text[Index: Integer]: String read GetText write PutText;
    property Translation[Index: Integer]: String read GetTranslation write PutTranslation;
    property InitialTime[Index: Integer]: TUWTimeCode read GetInitialTime write PutInitialTime;
    property InitialFrames[Index: Integer; FPS: Double]: Integer read GetInitialFrames;
    property FinalTime[Index: Integer]: TUWTimeCode read GetFinalTime write PutFinalTime;
    property FinalFrames[Index: Integer; FPS: Double]: Integer read GetFinalFrames;
    property Duration[Index: Integer]: TUWTimeCode read GetDuration write PutDuration;
    property DurationFrames[Index: Integer; FPS: Double]: Integer read GetDurationFrames;
    property Pause[Index: Integer]: TUWTimeCode read GetPause write PutPause;
    property PauseFrames[Index: Integer; FPS: Double]: Integer read GetPauseFrames;
    property TextCPS[Index: Integer; SkipChars: String]: Double read GetTextCPS;
    property TranslationCPS[Index: Integer; const SkipChars: String]: Double read GetTranslationCPS;
    property TextWPM[Index: Integer]: Double read GetTextWPM;
    property TranslationWPM[Index: Integer]: Double read GetTranslationWPM;
    property ExtraInfoType: TUWSubtitleExtraInfoType read FEIType write FEIType;
    property ExtraInfo[Index: Integer]: Pointer read GetExtraInfo write SetExtraInfo;
    property Header: Pointer read FHeader write FHeader;
    property IsRightToLeft: Boolean read FIsRTL;
    property Tag: Byte read FTag write FTag;
    property ReplaceEntities: Boolean read FReplaceEntity write FReplaceEntity;
    property TimeBase: TSubtitleTimeBase read FTimeBase write FTimeBase;
    property FormatProperties: PFormatProperties read FFormatProperties write FFormatProperties;
    property OnLoadDataFunc: TSubtitleLoadDataFunc read FLoadDataFunc write FLoadDataFunc;
  end;

  { TUWSubtitleCustomFormat }

  TUWSubtitleCustomFormat = class
  private
    FStringList: TUWStringList;
  public
    constructor Create(const AShouldWriteBOM: Boolean = False);
    destructor Destroy; override;
    function Name: String; virtual;
    function Format: TUWSubtitleFormats; virtual;
    function Extension: String; virtual;
    function IsTimeBased: Boolean; virtual;
    function IsFrameBased: Boolean; virtual;
    function IsTextBased: Boolean; virtual;
    function HasStyleSupport: Boolean; virtual;
    function IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean; virtual;
    function LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean; virtual;
    function SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean; virtual;
    property StringList: TUWStringList read FStringList write FStringList;
  end;

  { TUWSubtitleCustomFormatList }

  TUWSubtitleCustomFormatList = specialize TFPGList<TUWSubtitleCustomFormat>;

  { Helpers }

  procedure ClearSubtitleItem(var Item: TUWSubtitleItem);
  function GetColorCPS(const CPS: Double; const Default: Cardinal): Cardinal;
  function GetSubtitleFileFormat(AFileName: String): TUWSubtitleFormats;
  function HAlignToAlignment(const HAlign: TSubtitleHAlign): TAlignment;
  function VAlignToAlignment(const VAlign: TSubtitleVAlign): TVerticalAlignment;

// -----------------------------------------------------------------------------

implementation

uses UWSystem.StrUtils, UWSystem.SysUtils, UWSystem.Encoding, UWSystem.FileUtils,
  UWSubtitleAPI.Tags,
  ABCiView,
  AdobeEncoreDVD,
  AdvancedSubstationAlpha,
  AdvancedSubtitles,
  AQTitle,
  AvidCaption,
  Captions32,
  CaptionsInc,
  Cavena890,
  Cheetah,
  CheetahCaption,
  CPC600,
  CSV,
  DKS,
  DRTIC,
  DVDJunior,
  DVDSubtitle,
  DVDSubtitleSystem,
  EBU,
  FABSubtitler,
  GPACTTXT,
  IAuthor,
  InscriberCG,
  ITunesTimedText,
  JACOSub,
  KaraokeLyricsLRC,
  KaraokeLyricsVKT,
  MacDVDStudioPro,
  MacSUB,
  MicroDVD,
  MPlayer,
  MPlayer2,
  NetflixTimedText,
  SoftNI,
  Spreadsheet,
  SpruceSubtitleFile,
  SpruceSubtitleScript,
  SubRip,
  SubViewer,
  TeroSubtitler,
  TimedText,
  TSV,
  WebVTT;

// -----------------------------------------------------------------------------

{ TUWStringList }

// -----------------------------------------------------------------------------

constructor TUWStringList.Create(const FileName: String = ''; Encoding: TEncoding = NIL; const NameIsFile: Boolean = True);
begin
  FCount     := 0;
  FCapacity  := 0;
  FLineBreak := sLineBreak;
  FWriteBOM  := True;

  if not FileName.IsEmpty then
  begin
    if NameIsFile then
      LoadFromFile(FileName, Encoding)
    else
      LoadFromString(FileName);
  end;
end;

// -----------------------------------------------------------------------------

destructor TUWStringList.Destroy;
begin
  if FCount <> 0 then
    Finalize(FList^[0], FCount);

  FCount := 0;
  SetCapacity(0);

  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TUWStringList.LoadFromString(const S: String);
begin
  Clear;
  SetTextStr(S);
end;

// -----------------------------------------------------------------------------

procedure TUWStringList.LoadFromFile(const FileName: String; Encoding: TEncoding);
var
  Stream : TStream;
  Size   : Integer;
  Buffer : TBytes;
begin
  Clear;
  FFileName := '';
  if not FileExists(FileName) then Exit;
  FFileName := FileName;

  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Size := Stream.Size - Stream.Position;
    SetLength(Buffer, Size);
    Stream.Read(Buffer[0], Size);
    if Encoding = NIL then Encoding := GetEncodingFromFile(FileName);
    Size := TEncoding.GetBufferEncoding(Buffer, Encoding, GetCachedEncoding(1252));
    SetTextStr(Encoding.GetString(Buffer, Size, Length(Buffer) - Size));
    FCodePage := Encoding.CodePage;
  finally
    Stream.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWStringList.SaveToFile(const FileName: String; Encoding: TEncoding);
var
  Stream   : TFileStream;
  Buffer,
  Preamble : TBytes;
  TmpName,
  BakName  : String;
begin
  if (FCount = 0) or (FileName.IsEmpty) then Exit;

  if Encoding = NIL then Encoding := TEncoding.Default;

  Buffer   := Encoding.GetBytes(GetTextStr);
  Preamble := Encoding.GetPreamble;

  // write to a temporary file first and replace the original only when
  // everything was written, so an interrupted save can't truncate it
  TmpName := FileName + '.tmp';
  Stream := TFileStream.Create(TmpName, fmCreate);
  try
    if FWriteBOM and (Length(Preamble) > 0) then
      Stream.WriteBuffer(Preamble[0], Length(Preamble));

    if Length(Buffer) > 0 then
      Stream.WriteBuffer(Buffer[0], Length(Buffer));
  finally
    Stream.Free;
  end;

  if FileExists(FileName) then
  begin
    BakName := FileName + '.bak~';
    if FileExists(BakName) then DeleteFile(BakName);
    if not RenameFile(FileName, BakName) then
    begin
      DeleteFile(TmpName);
      raise EFCreateError.CreateFmt('Unable to replace file "%s"', [FileName]);
    end;
    if RenameFile(TmpName, FileName) then
      DeleteFile(BakName)
    else
    begin
      RenameFile(BakName, FileName);
      DeleteFile(TmpName);
      raise EFCreateError.CreateFmt('Unable to replace file "%s"', [FileName]);
    end;
  end
  else if not RenameFile(TmpName, FileName) then
  begin
    DeleteFile(TmpName);
    raise EFCreateError.CreateFmt('Unable to create file "%s"', [FileName]);
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWStringList.SaveToStream(Stream: TStream; Encoding: TEncoding);
var
  Buffer,
  Preamble : TBytes;
begin
  if (FCount = 0) then Exit;

  if Encoding = NIL then Encoding := TEncoding.Default;

  Buffer   := Encoding.GetBytes(GetTextStr);
  Preamble := Encoding.GetPreamble;

  if FWriteBOM and (Length(Preamble) > 0) then
    Stream.WriteBuffer(Preamble[0], Length(Preamble));

  if Length(Buffer) > 0 then
    Stream.WriteBuffer(Buffer[0], Length(Buffer));
end;

// -----------------------------------------------------------------------------

function TUWStringList.ValidIndex(const Index: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index < FCount);
end;

// -----------------------------------------------------------------------------

function TUWStringList.Get(Index: Integer): String;
begin
  if ValidIndex(Index) then
    Result := FList^[Index]
  else
    Result := '';
end;

// -----------------------------------------------------------------------------

procedure TUWStringList.Put(Index: Integer; const S: String);
begin
  if ValidIndex(Index) then
    FList^[Index] := S;
end;

// -----------------------------------------------------------------------------

function TUWStringList.Add(const S: String; const SkipEmptyLine: Boolean = False): Integer;
begin
  Result := FCount;
  Insert(Result, S, SkipEmptyLine);
end;

// -----------------------------------------------------------------------------

procedure TUWStringList.Insert(const Index: Integer; const S: String; const SkipEmptyLine: Boolean = False);
var
  FLine: String;
  Idx: Integer;
begin
  if not FInSetText then FBlankValid := False;
  if SkipEmptyLine then
  begin
    FLine := SysUtils.Trim(S);
    if FLine = '' then Exit;
  end
  else
    FLine := S;

  // an out of range index would corrupt memory, clamp it to [0..Count]
  Idx := Index;
  if Idx < 0 then Idx := 0;
  if Idx > FCount then Idx := FCount;

  if FCount = FCapacity then
    Grow;

  if Idx < FCount then
    System.Move(FList^[Idx], FList^[Idx + 1], (FCount - Idx) * SizeOf(String));

  Pointer(FList^[Idx]) := NIL;
  FList^[Idx]          := FLine;

  Inc(FCount);
end;

// -----------------------------------------------------------------------------

procedure TUWStringList.Move(const CurIndex, NewIndex: Integer);
var
  TempString: String;
begin
  if (CurIndex >= 0) and (CurIndex < FCount) and (CurIndex <> NewIndex) then
  begin
    TempString := Get(CurIndex);
    Delete(CurIndex);
    Insert(NewIndex, TempString, False);
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWStringList.Delete(const Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then Exit;
  if not FInSetText then FBlankValid := False;

  Finalize(FList^[Index]);
  Dec(FCount);

  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index], (FCount - Index) * SizeOf(String));
end;

// -----------------------------------------------------------------------------

procedure TUWStringList.Clear;
begin
  FBlankValid := False;
  if FCount <> 0 then
  begin
    Finalize(FList^[0], FCount);
    FCount := 0;
    SetCapacity(0);
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWStringList.SetCapacity(const NewCapacity: Integer);
begin
  ReallocMem(FList, NewCapacity * SizeOf(String));
  FCapacity := NewCapacity;
end;

// -----------------------------------------------------------------------------

procedure TUWStringList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else if FCapacity > 8 then
    Delta := 16
  else
    Delta := 4;

  SetCapacity(FCapacity + Delta);
end;

// -----------------------------------------------------------------------------

function TUWStringList.GetTextStr: String;
var
  I, L, Size : Integer;
  P          : PChar;
  S, LB      : String;
begin
  Size := 0;
  LB   := FLineBreak;

  for I := 0 to FCount - 1 do
    Inc(Size, Length(Get(I)) + Length(LB));

  SetString(Result, NIL, Size);
  P := Pointer(Result);

  for I := 0 to FCount - 1 do
  begin
    S := Get(I);
    L := Length(S);

    if L <> 0 then
    begin
      System.Move(Pointer(S)^, P^, L * SizeOf(Char));
      Inc(P, L);
    end;

    L := Length(LB);

    if L <> 0 then
    begin
      System.Move(Pointer(LB)^, P^, L * SizeOf(Char));
      Inc(P, L);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWStringList.SetTextStr(const Value: String);
var
  P, Start, LB : PChar;
  S            : String;
  LineBreakLen : Integer;
  Pending      : Boolean;

  procedure AddLine(const L: String);
  begin
    if Trim(L) = '' then
    begin
      Pending := True;
      Exit;
    end;
    Add(L, True);
    if FCount > Length(FBlankBefore) then
      SetLength(FBlankBefore, FCount * 2 + 64);
    FBlankBefore[FCount - 1] := Pending;
    Pending := False;
  end;

begin
  Clear;
  FInSetText := True;
  SetLength(FBlankBefore, 0);
  Pending := True;
  P := Pointer(Value);

  if P <> NIL then
    if CompareStr(FLineBreak, sLineBreak) = 0 then
    begin
      while P^ <> #0 do
      begin
        Start := P;

        while not CharInSet(P^, [#0, #10, #13]) do Inc(P);

        SetString(S, Start, P - Start);
        AddLine(S);

        if P^ = #13 then Inc(P);
        if P^ = #10 then Inc(P);
      end;
    end
    else
    begin
      LineBreakLen := Length(FLineBreak);
      while P^ <> #0 do
      begin
        Start := P;
        LB := AnsiStrPos(P, PChar(FLineBreak));

        while (P^ <> #0) and (P <> LB) do Inc(P);

        SetString(S, Start, P - Start);
        AddLine(S);
        if P = LB then Inc(P, LineBreakLen);
      end;
    end;

  FInSetText := False;
  FBlankValid := True;
end;

// -----------------------------------------------------------------------------

function TUWStringList.BlockStart(const Index: Integer): Boolean;
begin
  if FBlankValid and (Index >= 0) and (Index < FCount) then
    Result := FBlankBefore[Index]
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

{ Helpers }

// -----------------------------------------------------------------------------

function CompareItems(const R1, R2: TUWSubtitleItem): Integer;
begin
  if R1.InitialTime < R2.InitialTime then
    Result := 1
  else if R1.InitialTime > R2.InitialTime then
    Result := -1
  else
  begin
    if R1.FinalTime < R2.FinalTime then
      Result := 1
    else if R1.FinalTime > R2.FinalTime then
      Result := -1
    else
      Result := 0
  end;
end;

//------------------------------------------------------------------------------

function CompareItemsToSort(const R1, R2: PUWSubtitleItem): Integer;
begin
  Result := -CompareItems(R1^, R2^);
end;

// -----------------------------------------------------------------------------

procedure ClearExtraInfoList(var List: TList; const EIType: TUWSubtitleExtraInfoType);
var
  i: Integer;
begin
  if not Assigned(List) then Exit;

  if List.Count > 0 then
  begin
    for i := List.Count-1 downto 0 do
    begin
      if List.Items[i] <> NIL then
      begin
        case EIType of
          eiCavena890 : Dispose(PCavena890_Info(List.Items[i]));
          eiEBU       : Dispose(PEBU_ExtraInfo(List.Items[i]));
          eiMicroDVD  : Dispose(PMicroDVD_ExtraInfo(List.Items[i]));
          eiSSA       : Dispose(PSSA_ExtraInfo(List.Items[i]));
          eiSubRip    : Dispose(PSubRip_ExtraInfo(List.Items[i]));
          eiWebVTT    : Dispose(PWebVTT_ExtraInfo(List.Items[i]));
        end;
        List.Items[i] := NIL;
      end;
      List.Delete(i);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure ClearList(var List: TUWSubtitleItemList);
var
  i: Integer;
begin
  if not Assigned(List) then Exit;

  if List.Count > 0 then
  begin
    for i := List.Count-1 downto 0 do
    begin
      Dispose(PUWSubtitleItem(List.Items[i]));
      List.Items[i] := NIL;
      List.Delete(i);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure InitializeSubtitleFormats(var AList: TUWSubtitleCustomFormatList; const AShouldWriteBOM: Boolean);
begin
  if AList = NIL then AList := TUWSubtitleCustomFormatList.Create;

  AList.Add( TUWSubtitleCustomFormat(TUWABCiView.Create(AShouldWriteBOM)) );
  AList.Add( TUWSubtitleCustomFormat(TUWAdobeEncoreDVD.Create(AShouldWriteBOM)) );
  AList.Add( TUWSubtitleCustomFormat(TUWAdvancedSubstationAlpha.Create(AShouldWriteBOM)) );
  AList.Add( TUWSubtitleCustomFormat(TUWAdvancedSubtitles.Create(AShouldWriteBOM)) );
  AList.Add( TUWSubtitleCustomFormat(TUWAQTitle.Create(AShouldWriteBOM)) );
  AList.Add( TUWSubtitleCustomFormat(TUWAvidCaption.Create(AShouldWriteBOM)) );
  AList.Add( TUWSubtitleCustomFormat(TUWCaptions32.Create(AShouldWriteBOM)) );
  AList.Add( TUWSubtitleCustomFormat(TUWCaptionsInc.Create(AShouldWriteBOM)) );
  AList.Add( TUWSubtitleCustomFormat(TUWCavena890.Create(AShouldWriteBOM)) );
  AList.Add( TUWSubtitleCustomFormat(TUWCheetah.Create(AShouldWriteBOM)) );
  AList.Add( TUWSubtitleCustomFormat(TUWCheetahCaption.Create(AShouldWriteBOM)) ); // binary
  AList.Add( TUWSubtitleCustomFormat(TUWCPC600.Create(AShouldWriteBOM)) );
  AList.Add( TUWSubtitleCustomFormat(TUWCSV.Create(AShouldWriteBOM)) );
  AList.Add( TUWSubtitleCustomFormat(TUWDKS.Create(AShouldWriteBOM)) );
  AList.Add( TUWSubtitleCustomFormat(TUWDRTIC.Create(AShouldWriteBOM)) );
  AList.Add( TUWSubtitleCustomFormat(TUWDVDJunior.Create(AShouldWriteBOM)) );
  AList.Add( TUWSubtitleCustomFormat(TUWDVDSubtitleSystem.Create(AShouldWriteBOM)) );
  AList.Add( TUWSubtitleCustomFormat(TUWDVDSubtitle.Create(AShouldWriteBOM)) );
  AList.Add( TUWSubtitleCustomFormat(TUWEBU.Create(AShouldWriteBOM)) ); // binary
  AList.Add( TUWSubtitleCustomFormat(TUWFABSubtitler.Create(AShouldWriteBOM)) );
  AList.Add( TUWSubtitleCustomFormat(TUWGPACTTXT.Create(AShouldWriteBOM)) );
  AList.Add( TUWSubtitleCustomFormat(TUWIAuthor.Create(AShouldWriteBOM)) );
  AList.Add( TUWSubtitleCustomFormat(TUWInscriberCG.Create(AShouldWriteBOM)) );
  AList.Add( TUWSubtitleCustomFormat(TUWITunesTimedText.Create(False)) ); //"Important: The iTT file must not contain a byte-order mark (BOM)."
  AList.Add( TUWSubtitleCustomFormat(TUWJACOSub.Create(AShouldWriteBOM)) );
  AList.Add( TUWSubtitleCustomFormat(TUWKaraokeLyricsLRC.Create(AShouldWriteBOM)) );
  AList.Add( TUWSubtitleCustomFormat(TUWKaraokeLyricsVKT.Create(AShouldWriteBOM)) );
  AList.Add( TUWSubtitleCustomFormat(TUWMacDVDStudioPro.Create(AShouldWriteBOM)) );
  AList.Add( TUWSubtitleCustomFormat(TUWMacSUB.Create(AShouldWriteBOM)) );
  AList.Add( TUWSubtitleCustomFormat(TUWMicroDVD.Create(AShouldWriteBOM)) );
  AList.Add( TUWSubtitleCustomFormat(TUWMPlayer.Create(AShouldWriteBOM)) );
  AList.Add( TUWSubtitleCustomFormat(TUWMPlayer2.Create(AShouldWriteBOM)) );
  AList.Add( TUWSubtitleCustomFormat(TUWNetflixTimedText.Create(AShouldWriteBOM)) );
  AList.Add( TUWSubtitleCustomFormat(TUWSoftNI.Create(AShouldWriteBOM)) );
  AList.Add( TUWSubtitleCustomFormat(TUWSpreadsheet.Create(AShouldWriteBOM)) ); // binary
  AList.Add( TUWSubtitleCustomFormat(TUWSpruceSubtitleFile.Create(True)) );  // must contain BOM?
  AList.Add( TUWSubtitleCustomFormat(TUWSpruceSubtitleScript.Create(AShouldWriteBOM)) );
  AList.Add( TUWSubtitleCustomFormat(TUWSubRip.Create(AShouldWriteBOM)) );
  AList.Add( TUWSubtitleCustomFormat(TUWSubViewer.Create(AShouldWriteBOM)) );
  AList.Add( TUWSubtitleCustomFormat(TUWTeroSubtitler.Create(AShouldWriteBOM)) );
  AList.Add( TUWSubtitleCustomFormat(TUWTimedText.Create(AShouldWriteBOM)) );
  AList.Add( TUWSubtitleCustomFormat(TUWTSV.Create(AShouldWriteBOM)) );
  AList.Add( TUWSubtitleCustomFormat(TUWWebVTT.Create(AShouldWriteBOM)) );
end;

// -----------------------------------------------------------------------------

procedure FinalizeSubtitleFormats(var AList: TUWSubtitleCustomFormatList);
var
  i: Integer;
begin
  for i := AList.Count-1 downto 0 do AList[i].Free;
  AList.Free;
  AList := NIL;
end;

// -----------------------------------------------------------------------------

{ TUWSubtitles }

// -----------------------------------------------------------------------------

constructor TUWSubtitles.Create;
begin
  FCodePage      := -1;
  FFormat        := sfInvalid;
  FFPS           := -1;
  FTimecodeMode  := tcNDF;
  FList          := TUWSubtitleItemList.Create;
  FExtraInfo     := TList.Create;
  FStyles        := TList.Create;
  FSearchStartAt := 0;
  FSearchIdx     := 0;
  FSearchSkip    := 0;
  FHeader        := NIL;
  FWriteBOM      := True;
  FAutoSort      := True;
  FReplaceEntity := False;
  FIsRTL         := False;
  FTag           := 0;
  FTimeBase      := stbMedia;
  FLoadDataFunc  := NIL;
  New(FFormatProperties);
  DefaultFormatPropertiesSettings(FFormatProperties);
end;

// -----------------------------------------------------------------------------

destructor TUWSubtitles.Destroy;
begin
  FHeader := NIL;
  FLoadDataFunc := NIL;
  Dispose(FFormatProperties);
  ClearExtraInfoList(FExtraInfo, FEIType);
  FExtraInfo.Free;
  FStyles.Free;
  ClearList(FList);
  FList.Free;

  inherited Destroy;
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.GetCount: Integer;
begin
  Result := FList.Count;
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.ValidIndex(const Index: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index < FList.Count);
end;

// -----------------------------------------------------------------------------

procedure TUWSubtitles.Assign(Source: TUWSubtitles);
var
  i: Integer;
begin
  Clear;
  for i := 0 to Source.Count-1 do
  begin
    Add(Source[i]);
  end;
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.GetItem(Index: Integer): TUWSubtitleItem;
begin
  ClearSubtitleItem(Result);
  if ValidIndex(Index) then
    with FList[Index]^ do
    begin
      Result.Text        := Text;
      Result.Translation := Translation;
      Result.InitialTime := InitialTime;
      Result.FinalTime   := FinalTime;
      Result.Marked      := Marked;
      Result.ErrorType   := ErrorType;
      Result.Align       := Align;
      Result.VAlign      := VAlign;
      Result.R           := R;
      Result.Style       := Style;
      Result.Actor       := Actor;
      Result.Notes       := Notes;
      Result.Data        := Data;
    end;
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.GetItemPointer(Index: Integer): PUWSubtitleItem;
begin
  Result := NIL;
  if ValidIndex(Index) then
    Result := FList[Index];
end;

// -----------------------------------------------------------------------------

procedure TUWSubtitles.PutItem(Index: Integer; const Item: TUWSubtitleItem);
begin
  if ValidIndex(Index) then
  begin
    with FList[Index]^ do
    begin
      Text        := Item.Text;
      Translation := Item.Translation;
      InitialTime := Item.InitialTime;
      FinalTime   := Item.FinalTime;
      Marked      := Item.Marked;
      ErrorType   := Item.ErrorType;
      Align       := Item.Align;
      VAlign      := Item.VAlign;
      R           := Item.R;
      Style       := Item.Style;
      Actor       := Item.Actor;
      Notes       := Item.Notes;
      Data        := Item.Data;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWSubtitles.SetFormat(const Format: TUWSubtitleFormats);
begin
  FFormat := Format;
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.GetText(Index: Integer): String;
begin
  Result := '';
  if ValidIndex(Index) then
    Result := FList[Index]^.Text;
end;

// -----------------------------------------------------------------------------

procedure TUWSubtitles.PutText(Index: Integer; const S: String);
begin
  // assign the field directly instead of copying the whole record twice
  if ValidIndex(Index) then
    FList[Index]^.Text := S;
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.GetTranslation(Index: Integer): String;
begin
  Result := '';
  if ValidIndex(Index) then
    Result := FList[Index]^.Translation;
end;

// -----------------------------------------------------------------------------

procedure TUWSubtitles.PutTranslation(Index: Integer; const S: String);
begin
  // assign the field directly instead of copying the whole record twice
  if ValidIndex(Index) then
    FList[Index]^.Translation := S;
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.GetInitialTime(Index: Integer): TUWTimeCode;
begin
  Result := 0;

  if ValidIndex(Index) then
  begin
    Result := FList[Index]^.InitialTime;
    if FTimeBase = stbSMPTE then
      Result := SMPTEConversion(Result);
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWSubtitles.PutInitialTime(Index: Integer; const Time: TUWTimeCode);
begin
  // assign the field directly instead of copying the whole record twice
  if ValidIndex(Index) then
    FList[Index]^.InitialTime := Time;
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.GetInitialFrames(Index: Integer; FPS: Double): Integer;
begin
  Result := 0;

  if ValidIndex(Index) then
    Result := TimeToFrames(FList[Index]^.InitialTime, FPS);
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.GetFinalTime(Index: Integer): TUWTimeCode;
begin
  Result := 0;

  if ValidIndex(Index) then
  begin
    Result := FList[Index]^.FinalTime;
    if FTimeBase = stbSMPTE then
      Result := SMPTEConversion(Result);
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWSubtitles.PutFinalTime(Index: Integer; const Time: TUWTimeCode);
begin
  // assign the field directly instead of copying the whole record twice
  if ValidIndex(Index) then
    FList[Index]^.FinalTime := Time;
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.GetFinalFrames(Index: Integer; FPS: Double): Integer;
begin
  Result := 0;

  if ValidIndex(Index) then
    Result := TimeToFrames(FList[Index]^.FinalTime, FPS);
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.GetDuration(Index: Integer): TUWTimeCode;
begin
  Result := 0;

  if ValidIndex(Index) then
    Result := Range(FList[Index]^.FinalTime - FList[Index]^.InitialTime, 0, FList[Index]^.FinalTime);
end;

// -----------------------------------------------------------------------------

procedure TUWSubtitles.PutDuration(Index: Integer; const Time: TUWTimeCode);
begin
  // assign the field directly instead of copying the whole record twice
  if ValidIndex(Index) then
    FList[Index]^.FinalTime := Time + FList[Index]^.InitialTime;
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.GetDurationFrames(Index: Integer; FPS: Double): Integer;
begin
  Result := 0;

  if ValidIndex(Index) then
    Result := TimeToFrames(FList[Index]^.FinalTime - FList[Index]^.InitialTime, FPS);
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.GetPause(Index: Integer): TUWTimeCode;
begin
  Result := 0;

  if (Index >= 0) and ((Index+1) < Count) then
    Result := Max(0, FList[Index+1]^.InitialTime - FList[Index]^.FinalTime);
end;

// -----------------------------------------------------------------------------

procedure TUWSubtitles.PutPause(Index: Integer; const Time: TUWTimeCode);
begin
  if (Index >= 0) and ((Index+1) < Count) then
    FList[Index+1]^.InitialTime := Max(0, FList[Index]^.FinalTime + Time);
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.GetPauseFrames(Index: Integer; FPS: Double): Integer;
begin
  Result := 0;

  if (Index >= 0) and ((Index+1) < Count) then
    Result := TimeToFrames(FList[Index+1]^.InitialTime - FList[Index]^.FinalTime, FPS);
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.GetExtraInfo(Index: Integer): Pointer;
begin
  Result := NIL;
  if (Index >= 0) and (Index < FExtraInfo.Count) then
    Result := FExtraInfo[Index];
end;

// -----------------------------------------------------------------------------

procedure TUWSubtitles.SetExtraInfo(Index: Integer; const P: Pointer);
begin
  if (Index >= 0) and (Index < FExtraInfo.Count) then
    FExtraInfo[Index] := P;
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.GetStringCPS(Index: Integer; const IsOriginal: Boolean; const SkipChars: String = ''): Double;
var
  s: String;
  i: Double;
  x: Integer;
begin
  Result := 0;

  if ValidIndex(Index) and (FList[Index]^.InitialTime >= 0) and (FList[Index]^.FinalTime > 0) then
  begin
    if IsOriginal then
      s := FList[Index]^.Text
    else
      s := FList[Index]^.Translation;

    i := (FList[Index]^.FinalTime-FList[Index]^.InitialTime) / 1000;
    if i > 0 then
    begin
      if SkipChars <> '' then
      begin
        for x := 1 to SkipChars.Length do
          s := ReplaceString(s, SkipChars[x], '');
      end;

      Result := (UTF8Length(ReplaceString(RemoveTSTags(s), sLineBreak, ' '))) / i;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.GetTextCPS(Index: Integer; const SkipChars: String = ''): Double;
begin
  Result := GetStringCPS(Index, True, SkipChars);
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.GetTranslationCPS(Index: Integer; const SkipChars: String = ''): Double;
begin
  Result := GetStringCPS(Index, False, SkipChars);
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.GetStringWPM(Index: Integer; const IsOriginal: Boolean): Double;
var
  d: TUWTimeCode;
  s: String;
begin
  d := GetDuration(Index);

  if (d > 0) then
  begin
    if IsOriginal then
      s := FList[Index]^.Text
    else
      s := FList[Index]^.Translation;

    Result := (60.0 / (d / 1000)) * WordCount(ReplaceString(RemoveTSTags(s), sLineBreak, ' '));
  end
  else
    Result := 0;
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.GetTextWPM(Index: Integer): Double;
begin
  Result := GetStringWPM(Index, True);
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.GetTranslationWPM(Index: Integer): Double;
begin
  Result := GetStringWPM(Index, False);
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.Add(const AInitialTime, AFinalTime: TUWTimeCode; const AText, ATranslation: String; const AExtraInfo: Pointer = NIL; const SkipInvalidValues: Boolean = False): Integer;
begin
  if SkipInvalidValues and (Trim(AText) = '') then
  begin
    Result := -1;
    Exit;
  end;

  Result := FList.Count;
  Insert(Result, AInitialTime, AFinalTime, AText, ATranslation, AExtraInfo, SkipInvalidValues);
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.Add(const Item: TUWSubtitleItem; const ExtraInfo: Pointer = NIL; const SkipInvalidValues: Boolean = False): Integer;
begin
  if SkipInvalidValues and ((Trim(Item.Text) = '') or (Item.InitialTime > Item.FinalTime)) then
  begin
    Result := -1;
    Exit;
  end;

  Result := FList.Count;
  Insert(Result, Item, ExtraInfo);
end;

// -----------------------------------------------------------------------------

procedure TUWSubtitles.Insert(const Index: Integer; const AInitialTime, AFinalTime: TUWTimeCode; const AText, ATranslation: String; const ExtraInfo: Pointer = NIL; const SkipInvalidValues: Boolean = False);
var
  I: PUWSubtitleItem;
begin
//  if (SkipInvalidValues and ((Trim(AText) = '')) or (AInitialTime > AFinalTime) or (AInitialTime = -1)) then Exit;
  if SkipInvalidValues and (Trim(AText) = '') and ((AInitialTime > AFinalTime) or (AInitialTime = -1)) then Exit;

  New(I);
  try
    FillChar(I^, SizeOf(TUWSubtitleItem), 0);
    with I^ do
    begin
      Text         := AText;
      Translation  := ATranslation;
      InitialTime  := AInitialTime;
      FinalTime    := AFinalTime;
      Marked       := False;
      ErrorType    := [];
      Align        := shaNone;
      VAlign       := svaBottom;
      R            := Rect(0, 0, 0, 0);
      Style        := '';
      Actor        := '';
      Notes        := '';
      Data         := 0;
    end;

    FList.Insert(Index, I);
    try
      FExtraInfo.Insert(Index, ExtraInfo);
    except
    end;
  except
    Dispose(I);
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWSubtitles.Insert(const Index: Integer; const Item: TUWSubtitleItem; const ExtraInfo: Pointer = NIL);
var
  I: PUWSubtitleItem;
begin
  New(I);
  try
    I^ := Item;
    FList.Insert(Index, I);
    try
      FExtraInfo.Insert(Index, ExtraInfo);
    except
    end;
  except
    Dispose(I);
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWSubtitles.Move(const CurIndex, NewIndex: Integer);
var
  TempExtra : Pointer;
begin
  if (CurIndex >= 0) and (CurIndex < FList.Count) and (CurIndex <> NewIndex) then
  begin
    FList.Move(CurIndex, NewIndex);
    TempExtra := FExtraInfo.Items[CurIndex];
    FExtraInfo.Delete(CurIndex);
    FExtraInfo.Insert(NewIndex, TempExtra);
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWSubtitles.Delete(const Index: Integer);
begin
  if not ValidIndex(Index) then Exit;

  Dispose(PUWSubtitleItem(FList[Index]));
  FList.Delete(Index);
  if Index < FExtraInfo.Count then
  begin
    if FExtraInfo[Index] <> NIL then
      case FEIType of
        eiCavena890 : Dispose(PCavena890_Info(FExtraInfo[Index]));
        eiEBU       : Dispose(PEBU_ExtraInfo(FExtraInfo[Index]));
        eiMicroDVD  : Dispose(PMicroDVD_ExtraInfo(FExtraInfo[Index]));
        eiSSA       : Dispose(PSSA_ExtraInfo(FExtraInfo[Index]));
        eiSubRip    : Dispose(PSubRip_ExtraInfo(FExtraInfo[Index]));
        eiWebVTT    : Dispose(PWebVTT_ExtraInfo(FExtraInfo[Index]));
      end;
    FExtraInfo.Delete(Index);
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWSubtitles.Clear;
begin
  FIsRTL := False;
  // free the items and their extra info, not only the pointers
  ClearExtraInfoList(FExtraInfo, FEIType);
  ClearList(FList);
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.IndexOf(Item: PUWSubtitleItem): Integer;
var
  C: Integer;
begin
  Result := 0;
  C := FList.Count;
  while (Result < C) and (FList[Result] <> Item) do Inc(Result);
  if Result >= C then Result := -1;
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.IndexOf(const PosMS: Integer): Integer;
var
  C, Lim: Integer;
begin
  Result := -1;
  if FList.Count = 0 then Exit;

  C := FindInsertPos(PosMS, -1);
  Constrain(C, 0, FList.Count-1);
  // the list is sorted by initial time, so only a few previous entries can
  // still cover PosMS; bound the backward scan (called on every mouse move)
  Lim := Max(0, C - 64);

  while (C >= Lim) do
  begin
    if (FList[C]^.InitialTime <= PosMS) and (FList[C]^.FinalTime >= PosMS) then
      Exit(C);

    Dec(C);
  end;
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.FindInsertPos(const Item: TUWSubtitleItem): Integer;
var
  Min, Mid, Max : Integer;
  ItemCursor    : PUWSubtitleItem;
  CompareResult : Integer;
begin
  Min := 0;
  Max := FList.Count-1;
  Mid := (Max+Min) div 2;

  while (Min <= Max) do
  begin
    ItemCursor    := FList[Mid];
    CompareResult := CompareItems(ItemCursor^, Item);
    if CompareResult = 1 then // ItemCursor < Item
      Min := Mid+1
    else if CompareResult = -1 then // ItemCursor > Item
      Max := Mid-1
    else // r = Item
    begin
      Result := Mid;
      Exit;
    end;
    Mid := (Max+Min) div 2;
  end;

  Result := Min;
end;

//------------------------------------------------------------------------------

function TUWSubtitles.FindInsertPos(const InitialTime, FinalTime: TUWTimeCode): Integer;
var
  Item: TUWSubtitleItem;
begin
  Item.InitialTime := InitialTime;

  if FinalTime = -1 then
    Item.FinalTime := InitialTime + 1
  else
    Item.FinalTime := FinalTime;

  Result := FindInsertPos(Item);
end;

//------------------------------------------------------------------------------

function TUWSubtitles.FindFirst(const MSecs: TUWTimeCode; const Skip: Integer = 0): Integer;
begin
  if FList.Count = 0 then
  begin
    Result := -1;
    Exit;
  end;

  FSearchStartAt  := MSecs;
  FSearchSkip     := Skip;
  FSearchIdx      := FindInsertPos(FSearchStartAt - FSearchSkip, -1);
  Constrain(FSearchIdx, 0, FList.Count-1);
  while (FSearchIdx > 0) and
    (FList[FSearchIdx]^.FinalTime > (FSearchStartAt + FSearchSkip)) do
  begin
    Dec(FSearchIdx);
  end;
  Result := FindNext;
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.FindFirstPointer(const MSecs: TUWTimeCode; const Skip: Integer = 0): PUWSubtitleItem;
var
  idx: Integer;
begin
  idx := FindFirst(MSecs, Skip);
  if idx > -1 then
    Result := FList[idx]
  else
    Result := NIL;
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.FindNext: Integer;
var
  Item: TUWSubtitleItem;
begin
  Result := -1;
  while (FSearchIdx >= 0) and (FSearchIdx < FList.Count) do
  begin
    Item := FList[FSearchIdx]^;
    Inc(FSearchIdx);
    if((Item.InitialTime - FSearchSkip) > FSearchStartAt) then
      Exit
    else if ((Item.InitialTime - FSearchSkip) <= FSearchStartAt) and
            ((Item.FinalTime + FSearchSkip) >= FSearchStartAt) then
    begin
      Result := (FSearchIdx-1);
      Exit;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.FindNextPointer: PUWSubtitleItem;
var
  idx: Integer;
begin
  idx := FindNext;
  if idx > -1 then
    Result := FList[idx]
  else
    Result := NIL;
end;

// -----------------------------------------------------------------------------

procedure TUWSubtitles.Sort;
var
  Idx, Tmp: array of Integer;
  OldItems: array of PUWSubtitleItem;
  OldExtra: array of Pointer;
  i, c, w, lo, mid, hi, a, b, k: Integer;
begin
  // stable merge sort over an index array, so FExtraInfo can follow FList
  c := FList.Count;
  if c < 2 then Exit;

  SetLength(Idx, c);
  SetLength(Tmp, c);
  for i := 0 to c-1 do Idx[i] := i;

  w := 1;
  while w < c do
  begin
    lo := 0;
    while lo < c do
    begin
      mid := Min(lo + w, c);
      hi  := Min(lo + 2*w, c);
      a := lo; b := mid; k := lo;
      while (a < mid) and (b < hi) do
      begin
        if CompareItemsToSort(FList[Idx[b]], FList[Idx[a]]) < 0 then
        begin
          Tmp[k] := Idx[b]; Inc(b);
        end
        else
        begin
          Tmp[k] := Idx[a]; Inc(a);
        end;
        Inc(k);
      end;
      while a < mid do begin Tmp[k] := Idx[a]; Inc(a); Inc(k); end;
      while b < hi do begin Tmp[k] := Idx[b]; Inc(b); Inc(k); end;
      lo := hi;
    end;
    for i := 0 to c-1 do Idx[i] := Tmp[i];
    w := w * 2;
  end;

  SetLength(OldItems, c);
  SetLength(OldExtra, c);
  for i := 0 to c-1 do
  begin
    OldItems[i] := FList[i];
    if i < FExtraInfo.Count then
      OldExtra[i] := FExtraInfo[i]
    else
      OldExtra[i] := NIL;
  end;

  for i := 0 to c-1 do
  begin
    FList[i] := OldItems[Idx[i]];
    if i < FExtraInfo.Count then
      FExtraInfo[i] := OldExtra[Idx[i]];
  end;
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.IsEqualItemTimes(const I1, I2: TUWSubtitleItem): Boolean;
begin
  Result := (I1.InitialTime = I2.InitialTime) and (I1.FinalTime = I2.FinalTime);
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.IsEqualItem(const I1, I2: TUWSubtitleItem): Boolean;
begin
  if (I1.InitialTime = I2.InitialTime) and (I1.FinalTime = I2.FinalTime) and (I1.Text = I2.Text) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.GetFormatInfoFromFile(const AFileName: String; out AFormat: TUWSubtitleFormats; out ATimeBase: TSubtitleTimeBase): Boolean;
var
  Sub : TUWSubtitles;
  FPSInfo : TFPSInfo;
begin
  Result := False;
  AFormat := sfInvalid;
  ATimeBase := stbMedia;

  Sub := TUWSubtitles.Create;
  try
    FPSInfo.FPS := 0;
    FPSInfo.Mode := tcNDF;
    if Sub.LoadFromFile(AFileName, NIL, FPSInfo) then
    begin
      Result := True;
      AFormat := Sub.Format;
      if ((AFormat = sfTimedText) or (AFormat = sfNetflixTimedText) or (AFormat = sfITunesTimedText)) and (Sub.FFPS > 0) then
        ATimeBase := stbSMPTE
      else
        ATimeBase := Sub.TimeBase;
    end;
  finally
    Sub.Free;
  end;
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.GetFormatFromFile(const AFileName: String): TUWSubtitleFormats;
var
  AFormat: TUWSubtitleFormats;
  ATimeBase: TSubtitleTimeBase;
begin
  GetFormatInfoFromFile(AFileName, AFormat, ATimeBase);
  Result := AFormat;
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.GetTimeBaseFromFile(const AFileName: String): TSubtitleTimeBase;
var
  AFormat: TUWSubtitleFormats;
  ATimeBase: TSubtitleTimeBase;
begin
  GetFormatInfoFromFile(AFileName, AFormat, ATimeBase);
  Result := ATimeBase;
end;

// -----------------------------------------------------------------------------

procedure TUWSubtitles.CheckIsRightToLeft;
const
  MaxLineCount = 20;

var
  i, c: Integer;
begin
  FIsRTL := False;

  if Count = 0 then
    Exit
  else
    c := Min(MaxLineCount, Count);

  for i := 0 to c-1 do
    if IsRightToLeftText(Text[i]) then
    begin
      FIsRTL := True;
      Break;
    end;
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.LoadFromFile(const FileName: String; Encoding: TEncoding; const FPS: TFPSInfo; const Format: TUWSubtitleFormats = sfInvalid; const ClearAll: Boolean = True; const NameIsFile: Boolean = True): Boolean;
var
  AList   : TUWSubtitleCustomFormatList;
  SubFile : TUWStringList;
  i, f    : Integer;
  AFPS    : Double;
  StartCount : Integer;
begin
  Result := False;
  AList  := NIL;
  InitializeSubtitleFormats(AList, WriteBOM);
  try
    SubFile := TUWStringList.Create(FileName, Encoding, NameIsFile);
    try
      if ClearAll then
      begin
        ClearExtraInfoList(FExtraInfo, FEIType);
        FEIType := eiNone;
        //FStyles.Clear;
        ClearList(FList);
      end;
      // when appending, only the newly loaded items must be converted to SMPTE
      StartCount := FList.Count;

      AFPS := FPS.FPS;
      if AFPS <= 0 then AFPS := 25;
      FFPS := AFPS; //-1;
      FTimeCodeMode := FPS.Mode;
      FTimeBase := stbMedia;

      // First try Text formats
      if SubFile.Count > 0 then
        for i := 0 to SubFile.Count-1 do
          try
            for f := 0 to AList.Count-1 do
              if AList[f].IsTextBased and (((Format = sfInvalid) and AList[f].IsMine(SubFile, i)) or (AList[f].Format = Format)) then
              begin
                Result := AList[f].LoadSubtitle(SubFile, AFPS, Self);
                Self.FCodePage := SubFile.CodePage;
                Self.FFormat := AList[f].Format;
                Exit;
              end;
          except
          end;

      // If not, try binary formats
      if NameIsFile then
        try
          for f := 0 to AList.Count-1 do
            if not AList[f].IsTextBased and (((Format = sfInvalid) and AList[f].IsMine(SubFile, 0)) or (AList[f].Format = Format)) then
            begin
              Result := AList[f].LoadSubtitle(SubFile, AFPS, Self);
              Self.FCodePage := 65001; //SubFile.CodePage;
              Self.FFormat := AList[f].Format;
              Exit;
            end;
        except
        end;
    finally
      if (Self.FFormat <> sfITunesTimedText) and (TimecodeMode = tcDF) then ConvertTimesToSMPTE(True, StartCount);
      if FAutoSort then Sort;
      CheckIsRightToLeft; //
      SubFile.Free;
    end;
  finally
    FinalizeSubtitleFormats(AList);
  end;
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.LoadFromString(const S: String; Encoding: TEncoding; const FPS: TFPSInfo; const Format: TUWSubtitleFormats = sfInvalid; const ClearAll: Boolean = True): Boolean;
begin
  Result := LoadFromFile(S, Encoding, FPS, Format, ClearAll, False);
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.SaveToFile(const FileName: String; const FPS: TFPSInfo; const Encoding: TEncoding; const Format: TUWSubtitleFormats; SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1; const FixFileName: Boolean = False): Boolean;
var
  AList      : TUWSubtitleCustomFormatList;
  f          : Integer;
  iFrom, iTo : Integer;
begin
  Result := False;
  AList  := NIL;
  InitializeSubtitleFormats(AList, WriteBOM);
  try
    for f := 0 to AList.Count-1 do
      if AList[f].Format = Format then
      begin
        if FromItem = -1 then iFrom := 0 else iFrom := FromItem;
        if ToItem   = -1 then iTo := Count-1 else iTo := ToItem;
        if not FixFileName then
          Result := AList[f].SaveSubtitle(FileName, FPS.FPS, Encoding, Self, SubtitleMode, iFrom, iTo)
        else
          Result := AList[f].SaveSubtitle(FixExtension(FileName, AList[f].Extension), FPS.FPS, Encoding, Self, SubtitleMode, iFrom, iTo);

        Exit;
      end;
  finally
    FinalizeSubtitleFormats(AList);
  end;
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.SaveToString(const FPS: Double; const Encoding: TEncoding; const Format: TUWSubtitleFormats; SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): String;
var
  AList      : TUWSubtitleCustomFormatList;
  f          : Integer;
  iFrom, iTo : Integer;
begin
  Result := '';
  AList  := NIL;
  InitializeSubtitleFormats(AList, WriteBOM);
  try
    for f := 0 to AList.Count-1 do
      if AList[f].Format = Format then
      begin
        if FromItem = -1 then iFrom := 0 else iFrom := FromItem;
        if ToItem   = -1 then iTo := Count-1 else iTo := ToItem;
        AList[f].SaveSubtitle('', FPS, Encoding, Self, SubtitleMode, iFrom, iTo);
        Result := AList[f].StringList.Text;
        Exit;
      end;
  finally
    FinalizeSubtitleFormats(AList);
  end;
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.FillDialogFilter(AllSupportedText: String = 'All supported files'): String;
var
  AList : TUWSubtitleCustomFormatList;
  Exts  : String;
  f     : Integer;
begin
  Result := '';
  Exts   := '';
  AList  := NIL;
  InitializeSubtitleFormats(AList, WriteBOM);
  try
    for f := 0 to AList.Count-1 do
    begin
      Result  := Result + AList[f].Name + ' (' + AList[f].Extension + ')|' + AList[f].Extension + '|';
      if Pos(AList[f].Extension, Exts) = 0 then Exts := Exts + AList[f].Extension + ';';
    end;

    if AllSupportedText <> '' then
    begin
      System.Delete(Exts, Length(Exts), 1);
      Result := AllSupportedText + ' (' + Exts + ')|' + Exts + '|' + Result;
    end;
  finally
    FinalizeSubtitleFormats(AList);
  end;
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.FixExtension(const AFileName, AExtension: String): String;
var
  s: String;
  x: Integer;
begin
  x := Pos(';', AExtension);
  if x > 0 then
    s := Copy(AExtension, 1, x-1)
  else
    s := AExtension;

  Result := ChangeFileExt(AFileName, s.Replace('*', ''));
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.FormatExtensions(const AFormat: TUWSubtitleFormats): String;
var
  AList : TUWSubtitleCustomFormatList;
  f     : Integer;
begin
  // e.g. '*.vtt;*.webvtt'
  Result := '';
  AList  := NIL;
  InitializeSubtitleFormats(AList, WriteBOM);
  try
    for f := 0 to AList.Count-1 do
      if (AList[f].Format = AFormat) then
        Exit(AList[f].Extension);
  finally
    FinalizeSubtitleFormats(AList);
  end;
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.FixFileNameExtension(const AFileName: String; const AFormat: TUWSubtitleFormats): String;
var
  AList : TUWSubtitleCustomFormatList;
  f     : Integer;
begin
  Result := '';
  AList  := NIL;
  InitializeSubtitleFormats(AList, WriteBOM);
  try
    for f := 0 to AList.Count-1 do
      if (AList[f].Format = AFormat) then
        Result := FixExtension(AFileName, AList[f].Extension);
  finally
    FinalizeSubtitleFormats(AList);
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWSubtitles.DoFPSConversion(const AOldFPS, ANewFPS: Double);
var
  i, it, ft : Integer;
begin
  for i := 0 to Count-1 do
    with FList[i]^ do
    begin
      it          := TimeToFrames(InitialTime, AOldFPS);
      ft          := TimeToFrames(FinalTime, AOldFPS);
      InitialTime := FramesToTime(it, ANewFPS);
      FinalTime   := FramesToTime(ft, ANewFPS);
    end;
end;

// -----------------------------------------------------------------------------

procedure TUWSubtitles.ConvertTimesToSMPTE(const AValue: Boolean; const AFromIndex: Integer = 0);
var
  i: Integer;
begin
  for i := Max(0, AFromIndex) to Count - 1 do
    with FList[i]^ do
    begin
      InitialTime := SMPTEConversion(InitialTime, not AValue);
      FinalTime   := SMPTEConversion(FinalTime, not AValue);
    end;

  if not AValue then
    FTimeBase := stbMedia
  else
    FTimeBase := stbSMPTE;
end;

// -----------------------------------------------------------------------------

function TUWSubtitles.IsSMPTESupported(const AFormat: TUWSubtitleFormats = sfInvalid): Boolean;
var
  f: TUWSubtitleFormats;
begin
  if AFormat = sfInvalid then
    f := FFormat
  else
    f := AFormat;

  case f of
    sfITunesTimedText: Result := True;
    else
      Result := False;
  end;
end;

// -----------------------------------------------------------------------------

{ TUWSubtitleCustomFormat }

// -----------------------------------------------------------------------------

constructor TUWSubtitleCustomFormat.Create(const AShouldWriteBOM: Boolean = False);
begin
  FStringList := TUWStringList.Create;
  FStringList.WriteBOM := AShouldWriteBOM;
end;

// -----------------------------------------------------------------------------

destructor TUWSubtitleCustomFormat.Destroy;
begin
  FStringList.Free;
  FStringList := NIL;

  inherited Destroy;
end;

// -----------------------------------------------------------------------------

function TUWSubtitleCustomFormat.Name: String;
begin
  Result := '';
end;

// -----------------------------------------------------------------------------

function TUWSubtitleCustomFormat.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfInvalid;
end;

// -----------------------------------------------------------------------------

function TUWSubtitleCustomFormat.Extension: String;
begin
  Result := '';
end;

// -----------------------------------------------------------------------------

function TUWSubtitleCustomFormat.IsTimeBased: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWSubtitleCustomFormat.IsFrameBased: Boolean;
begin
  Result := not IsTimeBased;
end;

// -----------------------------------------------------------------------------

function TUWSubtitleCustomFormat.IsTextBased: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWSubtitleCustomFormat.HasStyleSupport: Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------

function TUWSubtitleCustomFormat.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------

function TUWSubtitleCustomFormat.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------

function TUWSubtitleCustomFormat.SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------

{ Helpers}

// -----------------------------------------------------------------------------

procedure ClearSubtitleItem(var Item: TUWSubtitleItem);
begin
  with Item do
  begin
    Text        := '';
    Translation := '';
    InitialTime := 0;
    FinalTime   := 0;
    Marked      := False;
    ErrorType   := [];
    Align       := shaNone;
    VAlign      := svaBottom;
    R           := Rect(0, 0, 0, 0);
    Style       := '';
    Actor       := '';
    Notes       := '';
    Data        := 0;
  end;
end;

// -----------------------------------------------------------------------------

function GetColorCPS(const CPS: Double; const Default: Cardinal): Cardinal;
begin
  if      (CPS < 5)  then Result := $0000FF // 240 "TOO SLOW!"
  else if (CPS < 10) then Result := $0099FF // 210 "Slow, acceptable.";
  else if (CPS < 13) then Result := $00CCFF // 180 "A bit slow.";
  //else if (CPS < 15) then Result := Default    // 150 "Good."
  //else if (CPS < 23) then Result := Default    // 120 "Perfect.";
  else if (CPS < 27) then Result := Default //  90 "Good.";
  else if (CPS < 31) then Result := $FFCC00 //  60 "A bit fast.";
  else if (CPS < 35) then Result := $FF9900 //  30 "Fast, acceptable.";
  else                    Result := $FF0000 //   0 "TOO FAST!";
end;

// -----------------------------------------------------------------------------

function GetSubtitleFileFormat(AFileName: String): TUWSubtitleFormats;
var
  Sub: TUWSubtitles;
  FPSInfo: TFPSInfo;
begin
  Result := sfInvalid;
  if GetFileSize(AFileName) > 2000000 then Exit;

  Sub := TUWSubtitles.Create;
  try
    FPSInfo.FPS := 1;
    FPSInfo.Mode := tcNDF;
    if Sub.LoadFromFile(AFileName, NIL, FPSInfo) then
      Result := Sub.Format;
  finally
    Sub.Free;
  end;
end;

// -----------------------------------------------------------------------------

function HAlignToAlignment(const HAlign: TSubtitleHAlign): TAlignment;
begin
  case HAlign of
    shaRight  : Result := taRightJustify;
    shaCenter : Result := taCenter;
  else
    Result := taLeftJustify;
  end;
end;

// -----------------------------------------------------------------------------

function VAlignToAlignment(const VAlign: TSubtitleVAlign): TVerticalAlignment;
begin
  case VAlign of
    svaTop    : Result := taAlignTop;
    svaCenter : Result := taVerticalCenter;
  else
    Result := taAlignBottom;
  end;
end;

// -----------------------------------------------------------------------------

end.
