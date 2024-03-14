{*
 *  URUWorks Translation Memory Exchange (TMX)
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
 *  Copyright (C) 2001-2023 URUWorks, uruworks@gmail.com.
 *}

unit UWSubtitleAPI.TMX;

// -----------------------------------------------------------------------------

interface

uses Classes, SysUtils, FGL, laz2_XMLRead, laz2_DOM, laz2_XMLWrite, Math,
  UWSystem.StrUtils;

type

  { TUWTMXHeader }

  PUWTMXHeader = ^TUWTMXHeader;
  TUWTMXHeader = record
    creationtool,        // "tero subtitler"
    creationtoolversion, // "1.0"
    datatype,            // "tbx"
    segtype,             // "block", "sentence"
    adminlang,           // "en-US"
    srclang,             // "en-US"
    otmf: String;        // ""
  end;

  { TUWTMXItem }

  PUWTMXItem = ^TUWTMXItem;
  TUWTMXItem = record
    Original,
    Translated,
    Notes: String;
  end;

  { TUWTMXItemInfo }

  PUWTMXItemInfo = ^TUWTMXItemInfo;
  TUWTMXItemInfo = record
    SrcLang,         // "en-US"
    DstLang: String; // "es-UY"
  end;

  { TUWTMXList }

  TUWTMXList = specialize TFPGList<PUWTMXItem>;

  { TUWTMXMap }

  TUWTMXMap = specialize TFPGMap<Double, Integer>; // Ratio, Index

  { TUWTMXThread }

  TUWTMXOnFindFinished = procedure(Sender: TObject; const ACount: Integer) of Object;

  TUWTMX = class;

  TUWTMXThread = class(TThread)
  private
    FTMX: TUWTMX;
    procedure DoOnFindFinished;
    procedure DoOnTerminated(Sender: TObject);
  protected
    procedure Execute; override;
  public
    constructor Create(const ATMX: TUWTMX);
    destructor Destroy; override;
  end;

  { TUWTMX }

  TUWTMX = class
  private
    FFileName : String;
    FFind     : String;
    FChanged  : Boolean;
    FHeader   : TUWTMXHeader;
    FLangs    : TUWTMXItemInfo;
    FList     : TUWTMXList;
    FMap      : TUWTMXMap;
    FPercent  : Single;
    FThread   : TUWTMXThread;
    FOnFindFinished : TUWTMXOnFindFinished;
    function GetLangPointer: PUWTMXItemInfo;
  public
    constructor Create(const AFileName: String; ASrcLang: String = ''; ADstLang: String = ''; AOnFindFinished: TUWTMXOnFindFinished = NIL);
    destructor Destroy; override;
    procedure Clear;
    procedure Close;
    procedure Save;
    procedure LoadFromFile(const AFileName: String);
    function SaveToFile(const AFileName: String): Boolean;
    function AddItem(const Original, Translated, Notes: String; const AllowDuplicate: Boolean = False): Integer;
    procedure FindSimilary(const AText: String); // fill Map with results
    function ItemFromMap(const AIndex: Integer): TUWTMXItem;
    function IndexFromMap(const AIndex: Integer): Integer;
    function Ready: Boolean;
    function FillMapWithIndex(const AIndex: Integer): Integer;
    function Find(AText: String): Integer;
    procedure Cancel;
    property FileName     : String         read FFileName write FFileName;
    property Items        : TUWTMXList     read FList;
    property Map          : TUWTMXMap      read FMap;
    property Langs        : PUWTMXItemInfo read GetLangPointer;
    property Header       : TUWTMXHeader   read FHeader write FHeader;
    property SimilPercent : Single         read FPercent;
  public
    property OnFindFinished : TUWTMXOnFindFinished read FOnFindFinished write FOnFindFinished;
  end;

// -----------------------------------------------------------------------------

implementation

uses UWSubtitleAPI.Utils;

// -----------------------------------------------------------------------------

{ lgStrHelpers }

// -----------------------------------------------------------------------------

{$PUSH}{$WARN 5057 OFF}{$WARN 5036 OFF}{$Q-}{$R-}
const
  NULL_INDEX: SizeInt = SizeInt(-1);

function LevDistMyersD(pL, pR: PByte; aLenL, aLenR: SizeInt): SizeInt;
var
  Pm: array[Byte] of DWord;
  PmI, Hp, Hn, Vp, Vn, D0: DWord;
  I: SizeInt;
begin
  System.FillChar(Pm, SizeOf(Pm), 0);
  for I := 0 to Pred(aLenL) do
    Pm[pL[I]] := Pm[pL[I]] or QWord(1) shl I;

  Result := aLenL;
  Vn := 0;
  Vp := High(DWord);

  for I := 0 to Pred(aLenR) do
    begin
      PmI := Pm[pR[I]];
      D0 := (((PmI and Vp) + Vp) xor Vp) or PmI or Vn;
      Hp := Vn or not(D0 or Vp);
      Hn := D0 and Vp;
      Vp := Hn shl 1 or not(D0 or Hp shl 1 or 1);
      Vn := D0 and (Hp shl 1 or 1);
      if Hn and (DWord(1) shl Pred(aLenL)) <> 0 then
        Dec(Result)
      else
        if Hp and (DWord(1) shl Pred(aLenL)) <> 0 then
          Inc(Result);
    end;
end;

// -----------------------------------------------------------------------------

function LevDistMyersD(pL, pR: PByte; aLenL, aLenR, aLimit: SizeInt): SizeInt;
var
  Pm: array[Byte] of DWord;
  PmI, Hp, Hn, Vp, Vn, D0: DWord;
  I: SizeInt;
begin
  System.FillChar(Pm, SizeOf(Pm), 0);
  for I := 0 to Pred(aLenL) do
    Pm[pL[I]] := Pm[pL[I]] or QWord(1) shl I;

  Result := aLenL;
  aLimit += aLenR - aLenL;
  Vn := 0;
  Vp := High(DWord);

  for I := 0 to Pred(aLenR) do
    begin
      PmI := Pm[pR[I]];
      D0 := (((PmI and Vp) + Vp) xor Vp) or PmI or Vn;
      Hp := Vn or not(D0 or Vp);
      Hn := D0 and Vp;
      Vp := Hn shl 1 or not(D0 or Hp shl 1 or 1);
      Vn := D0 and (Hp shl 1 or 1);
      if Hn and (DWord(1) shl Pred(aLenL)) <> 0 then
        Dec(Result)
      else
        begin
          if Hp and (DWord(1) shl Pred(aLenL)) <> 0 then
            begin
              Inc(Result);
              aLimit -= 2;
            end
          else
            Dec(aLimit);
          if aLimit < 0 then
            exit(NULL_INDEX);
        end;
    end;
end;

// -----------------------------------------------------------------------------

function LevDistMyersQ(pL, pR: PByte; aLenL, aLenR: SizeInt): SizeInt;
var
  Pm: array[Byte] of QWord;
  PmI, Hp, Hv, Vp, Vn, D0: QWord;
  I: SizeInt;
begin
  System.FillChar(Pm, SizeOf(Pm), 0);
  for I := 0 to Pred(aLenL) do
    Pm[pL[I]] := Pm[pL[I]] or QWord(1) shl I;

  Result := aLenL;
  Vn := 0;
  Vp := High(QWord);

  for I := 0 to Pred(aLenR) do
    begin
      PmI := Pm[pR[I]];
      D0 := (((PmI and Vp) + Vp) xor Vp) or PmI or Vn;
      Hp := Vn or not(D0 or Vp);
      Hv := D0 and Vp;
      Vp := Hv shl 1 or not(D0 or Hp shl 1 or 1);
      Vn := D0 and (Hp shl 1 or 1);
      if Hv and (QWord(1) shl Pred(aLenL)) <> 0 then
        Dec(Result)
      else
        if Hp and (QWord(1) shl Pred(aLenL)) <> 0 then
          Inc(Result);
    end;
end;

// -----------------------------------------------------------------------------

function LevDistMyersQ(pL, pR: PByte; aLenL, aLenR, aLimit: SizeInt): SizeInt;
var
  Pm: array[Byte] of QWord;
  PmI, Hp, Hn, Vp, Vn, D0: QWord;
  I: SizeInt;
begin
  System.FillChar(Pm, SizeOf(Pm), 0);
  for I := 0 to Pred(aLenL) do
    Pm[pL[I]] := Pm[pL[I]] or QWord(1) shl I;

  Result := aLenL;
  aLimit += aLenR - aLenL;
  Vn := 0;
  Vp := High(QWord);

  for I := 0 to Pred(aLenR) do
    begin
      PmI := Pm[pR[I]];
      D0 := (((PmI and Vp) + Vp) xor Vp) or PmI or Vn;
      Hp := Vn or not(D0 or Vp);
      Hn := D0 and Vp;
      Vp := Hn shl 1 or not(D0 or Hp shl 1 or 1);
      Vn := D0 and (Hp shl 1 or 1);
      if Hn and (QWord(1) shl Pred(aLenL)) <> 0 then
        Dec(Result)
      else
        begin
          if Hp and (QWord(1) shl Pred(aLenL)) <> 0 then
            begin
              Inc(Result);
              aLimit -= 2;
            end
          else
            Dec(aLimit);
          if aLimit < 0 then
            exit(NULL_INDEX);
        end;
    end;
end;

// -----------------------------------------------------------------------------

const
  BLOCK_SIZE = BitSizeOf(QWord);
  BSIZE_MASK = Pred(BLOCK_SIZE);
  BSIZE_LOG  = 6;

// -----------------------------------------------------------------------------

function LevDistMyersDQ(pL, pR: PByte; aLenL, aLenR: SizeInt): SizeInt;
var
  Peq: array[Byte] of array[0..1] of QWord;
  Eq0, Eq1, Ph, Mh, Pv0, Mv0, Pv1, Mv1, Xv, Xh, Hin: QWord;
  I: SizeInt;
begin
  System.FillChar(Peq, SizeOf(Peq), 0);
  for I := 0 to Pred(BLOCK_SIZE) do
    Peq[pL[I]][0] := Peq[pL[I]][0] or QWord(1) shl I;
  for I := BLOCK_SIZE to Pred(aLenL) do
    Peq[pL[I]][1] := Peq[pL[I]][1] or QWord(1) shl (I - BLOCK_SIZE);

  Result := aLenL;
  Pv0 := High(QWord);
  Pv1 := High(QWord);
  Mv0 := 0;
  Mv1 := 0;

  for I := 0 to Pred(aLenR) do
    begin
      Eq0 := Peq[pR[I]][0];
      Eq1 := Peq[pR[I]][1];
      ///////////////////////
      Xv := Mv0 or Eq0;
      Xh := ((Pv0 and Eq0 + Pv0) xor Pv0) or Eq0;
      Ph := Mv0 or not(Xh or Pv0);
      Mh := Pv0 and Xh;
      Hin := Ph shr BSIZE_MASK - Mh shr BSIZE_MASK;
      Ph := Ph shl 1 or 1;
      Pv0 := Mh shl 1 or not(Xv or Ph);
      Mv0 := Xv and Ph;
      ///////////////////////
      Xv := Mv1 or Eq1;
      Eq1 := Eq1 or Hin shr BSIZE_MASK;
      Xh := ((Pv1 and Eq1 + Pv1) xor Pv1) or Eq1;
      Ph := Mv1 or not(Xh or Pv1);
      Mh := Pv1 and Xh;
      ///////////////////////
      if Mh and (QWord(1) shl Pred(aLenL - BLOCK_SIZE)) <> 0 then
        Dec(Result)
      else
        if Ph and (QWord(1) shl Pred(aLenL - BLOCK_SIZE)) <> 0 then
          Inc(Result);
      ///////////////////////
      Ph := Ph shl 1 or (Hin + 1) shr 1;
      Pv1 := (Mh shl 1 or Hin shr BSIZE_MASK) or not(Xv or Ph);
      Mv1 := Xv and Ph;
    end;
end;

// -----------------------------------------------------------------------------

function LevDistMyersDQ(pL, pR: PByte; aLenL, aLenR, aLimit: SizeInt): SizeInt;
var
  Peq: array[Byte] of array[0..1] of QWord;
  Eq0, Eq1, Ph, Mh, Pv0, Mv0, Pv1, Mv1, Xv, Xh, Hin: QWord;
  I: SizeInt;
begin
  System.FillChar(Peq, SizeOf(Peq), 0);
  for I := 0 to Pred(BLOCK_SIZE) do
    Peq[pL[I]][0] := Peq[pL[I]][0] or QWord(1) shl I;
  for I := BLOCK_SIZE to Pred(aLenL) do
    Peq[pL[I]][1] := Peq[pL[I]][1] or QWord(1) shl (I - BLOCK_SIZE);

  Result := aLenL;
  aLimit += aLenR - aLenL;
  Pv0 := High(QWord);
  Pv1 := High(QWord);
  Mv0 := 0;
  Mv1 := 0;

  for I := 0 to Pred(aLenR) do
    begin
      Eq0 := Peq[pR[I]][0];
      Eq1 := Peq[pR[I]][1];
      ///////////////////////
      Xv := Mv0 or Eq0;
      Xh := ((Pv0 and Eq0 + Pv0) xor Pv0) or Eq0;
      Ph := Mv0 or not(Xh or Pv0);
      Mh := Pv0 and Xh;
      Hin := Ph shr BSIZE_MASK - Mh shr BSIZE_MASK;
      Ph := Ph shl 1 or 1;
      Pv0 := Mh shl 1 or not(Xv or Ph);
      Mv0 := Xv and Ph;
      ///////////////////////
      Xv := Mv1 or Eq1;
      Eq1 := Eq1 or Hin shr BSIZE_MASK;
      Xh := ((Pv1 and Eq1 + Pv1) xor Pv1) or Eq1;
      Ph := Mv1 or not(Xh or Pv1);
      Mh := Pv1 and Xh;
      ///////////////////////
      if Mh and (QWord(1) shl Pred(aLenL - BLOCK_SIZE)) <> 0 then
        Dec(Result)
      else
        begin
          if Ph and (QWord(1) shl Pred(aLenL - BLOCK_SIZE)) <> 0 then
            begin
              Inc(Result);
              aLimit -= 2;
            end
          else
            Dec(aLimit);
          if aLimit < 0 then
            exit(NULL_INDEX);
        end;
      ///////////////////////
      Ph := Ph shl 1 or (Hin + 1) shr 1;
      Pv1 := (Mh shl 1 or Hin shr BSIZE_MASK) or not(Xv or Ph);
      Mv1 := Xv and Ph;
    end;
end;

{ recodes sequences to determine alphabet size and minimize memory usage;
  returns the size of the new alphabet and recoded sequences in aBuffer }

function RecodeSeq(pL, pR: PByte; aLenL, aLenR: SizeInt; out aBuffer: TBytes): SizeInt;
var
  InTable: array[Byte] of Boolean;
  CodeTable: array[Byte] of Byte;
  I: SizeInt;
  b: Byte;
begin
  System.FillChar(InTable, SizeOf(InTable), 0);
  System.SetLength(aBuffer, aLenL + aLenR);
  Result := 0;
  for I := 0 to Pred(aLenL) do
    begin
      b := pL[I];
      if not InTable[b] then
        begin
          CodeTable[b] := Result;
          Inc(Result);
          InTable[b] := True;
        end;
      aBuffer[I] := CodeTable[b];
    end;
  for I := aLenL to Pred(aLenL + aLenR) do
    begin
      b := pR[I-aLenL];
      if not InTable[b] then
        begin
          CodeTable[b] := Result;
          Inc(Result);
          InTable[b] := True;
        end;
      aBuffer[I] := CodeTable[b];
    end;
end;

// -----------------------------------------------------------------------------

type
  TPeq = record
    Peq: array of PQWord;
    Buffer: array of QWord;
    BlockCount: SizeInt;
  end;

procedure CreatePeq(aSeq: PByte; aSeqLen, AlphabetSize: SizeInt; out aPeq: TPeq);
var
  I, J, BCount, LastRow: SizeInt;
  Pad: QWord;
begin
  LastRow := aSeqLen and BSIZE_MASK;
  BCount := aSeqLen shr BSIZE_LOG + Ord(LastRow <> 0);
  aPeq.BlockCount := BCount;
  System.SetLength(aPeq.Peq, AlphabetSize);

  System.SetLength(aPeq.Buffer, BCount * AlphabetSize);
  if LastRow <> 0 then
    Pad := System.High(QWord) shl LastRow
  else
    Pad := 0;
  J := 0;
  with aPeq do
    for I := 0 to Pred(AlphabetSize) do
      begin
        Peq[I] := @Buffer[J];
        Peq[I][Pred(BCount)] := Pad; ////////////???
        J += BCount;
      end;
  with aPeq do
    for I := 0 to Pred(aSeqLen) do
      Peq[aSeq[I]][I shr BSIZE_LOG] := Peq[aSeq[I]][I shr BSIZE_LOG] or QWord(1) shl (I and BSIZE_MASK);
end;

type
  TBlock = record
    P,
    M: QWord;
    Score: SizeInt;
  end;

{
  with some imrovements from Martin Šošić, Mile Šikić:
    "Edlib: a C/C 11 library for fast, exact sequence alignment using edit distance"
}

function LevDistMyersCutoff(const aPeq: TPeq; pR: PByte; aLenL, aLenR, K: SizeInt): SizeInt;
  function ReadBlockCell(const aBlock: TBlock; aIndex: SizeInt): SizeInt;
  var
    I: SizeInt;
  begin
    Result := aBlock.Score;
    for I := BSIZE_MASK downto Succ(aIndex) do
      if aBlock.P and (QWord(1) shl I) <> 0 then
        Dec(Result)
      else
        if aBlock.M and (QWord(1) shl I) <> 0 then
          Inc(Result);
  end;
var
  Blocks: array of TBlock;
  Eq, Xv, Xh, Pv, Mv, Ph, Mh, HIn, HOut: QWord;
  I, J, First, Last: SizeInt;
begin
  //here aLenL <= aLenR and K >= aLenR - aLenL
  K := Math.Min(k, aLenR);
  First := 0;
  I := Succ(Math.Min(K, (K - aLenR + aLenL) div 2));
  Last := Pred(Math.Min(aPeq.BlockCount, I shr BSIZE_LOG + Ord(I and BSIZE_MASK <> 0)));
  System.SetLength(Blocks, aPeq.BlockCount);
  Result := NULL_INDEX;

  for I := First to Last do
    with Blocks[I] do
      begin
        P := System.High(QWord);
        Score := BLOCK_SIZE * Succ(I);
      end;

  for I := 0 to Pred(aLenR) do
    begin
      HOut := 1;
      for J := First to Last do
        begin
          HIn := HOut;
          Eq := aPeq.Peq[pR[I]][J];
          Pv := Blocks[J].P;
          Mv := Blocks[J].M;
          Xv := Mv or Eq;
          Eq := Eq or HIn shr BSIZE_MASK;
          Xh := ((Pv and Eq + Pv) xor Pv) or Eq;
          Ph := Mv or not(Xh or Pv);
          Mh := Pv and Xh;

          HOut := Ph shr BSIZE_MASK - Mh shr BSIZE_MASK;

          Ph := Ph shl 1 or (HIn + 1) shr 1;

          Blocks[J].P := (Mh shl 1 or HIn shr BSIZE_MASK) or not(Xv or Ph);
          Blocks[J].M := Xv and Ph;
          Blocks[J].Score += SizeInt(HOut);
        end;
      // adjust last block
      if (Last < Pred(aPeq.BlockCount)) and
         (K-Blocks[Last].Score+BSIZE_MASK-aLenR+aLenL+I >= Last*BLOCK_SIZE) then
        begin
          Inc(Last);
          HIn := HOut;
          Eq := aPeq.Peq[pR[I]][Last];
          Pv := System.High(QWord);
          Mv := 0;
          Xv := Mv or Eq;
          Eq := Eq or HIn shr BSIZE_MASK;
          Xh := ((Pv and Eq + Pv) xor Pv) or Eq;
          Ph := Mv or not(Xh or Pv);
          Mh := Pv and Xh;

          HOut := Ph shr BSIZE_MASK - Mh shr BSIZE_MASK;

          Ph := Ph shl 1 or (HIn + 1) shr 1;

          Blocks[Last].P := (Mh shl 1 or HIn shr BSIZE_MASK) or not(Xv or Ph);
          Blocks[Last].M := Xv and Ph;
          Blocks[Last].Score := Blocks[Last-1].Score - SizeInt(HIn) + BLOCK_SIZE + SizeInt(HOut);
        end
      else
        while (Last >= First) and ((Blocks[Last].Score >= K + BLOCK_SIZE) or
              (K-Blocks[Last].Score+BSIZE_MASK-aLenR+aLenL+I+1 < Last*BLOCK_SIZE)) do
          Dec(Last);
      // adjust first block
      while (First <= Last) and ((Blocks[First].Score >= K + BLOCK_SIZE) or
            (Blocks[First].Score-K-aLenR+aLenL+I > (First+1)*BLOCK_SIZE-1)) do
        Inc(First);

      if Last < First then exit;
    end;

  if Last = Pred(aPeq.BlockCount) then
    begin
      I := Pred(aLenL and BSIZE_MASK);
      if I < 0 then I += BLOCK_SIZE;
      J := ReadBlockCell(Blocks[Last], I);
      if J <= K then
        Result := J;
    end;
end;
{$POP}

// -----------------------------------------------------------------------------

function LevDistMyersDyn(pL, pR: PByte; aLenL, aLenR: SizeInt): SizeInt;
var
  Peq: TPeq;
  Buffer: TBytes;
  AlphabetSize, Limit: SizeInt;
begin
  //here aLenL <= aLenR
  AlphabetSize := RecodeSeq(pL, pR, aLenL, aLenR, Buffer);
  CreatePeq(Pointer(Buffer), aLenL, AlphabetSize, Peq);
  Limit := Math.Max(BLOCK_SIZE, aLenR - aLenL);
  repeat
    Result := LevDistMyersCutoff(Peq, @Buffer[aLenL], aLenL, aLenR, Limit);
    Limit += Limit;
  until Result <> NULL_INDEX;
end;

// -----------------------------------------------------------------------------

function SkipSuffix(pL, pR: PByte; var aLenL, aLenR: SizeInt): SizeInt; inline;
begin
  //implied aLenL <= aLenR
  Result := 0;
  while (aLenL > 0) and (pL[Pred(aLenL)] = pR[Pred(aLenR)]) do
    begin
      Dec(aLenL);
      Dec(aLenR);
      Inc(Result);
    end;
end;

// -----------------------------------------------------------------------------

function SkipPrefix(var pL, pR: PByte; var aLenL, aLenR: SizeInt): SizeInt; inline;
begin
  //implied aLenL <= aLenR
  Result := 0;

  while (Result < aLenL) and (pL[Result] = pR[Result]) do
    Inc(Result);

  pL += Result;
  pR += Result;
  aLenL -= Result;
  aLenR -= Result;
end;

// -----------------------------------------------------------------------------

function GetLevDistMyers(pL, pR: PByte; aLenL, aLenR: SizeInt): SizeInt;
begin
  //here aLenL <= aLenR
  if pL = pR then
    exit(aLenR - aLenL);

  SkipSuffix(pL, pR, aLenL, aLenR);
  SkipPrefix(pL, pR, aLenL, aLenR);

  if aLenL = 0 then
    exit(aLenR);

  case aLenL of
    1..BitSizeOf(DWord):
      Result := LevDistMyersD(pL, pR, aLenL, aLenR);
    BitSizeOf(DWord)+1..BitSizeOf(QWord):
      Result := LevDistMyersQ(pL, pR, aLenL, aLenR);
    BitSizeOf(QWord)+1..BitSizeOf(QWord)*2:
      Result := LevDistMyersDQ(pL, pR, aLenL, aLenR);
  else
    Result := LevDistMyersDyn(pL, pR, aLenL, aLenR);
  end;
end;

// -----------------------------------------------------------------------------

function LevDistanceMyers(const L, R: rawbytestring): SizeInt;
begin
  if L = '' then
    exit(System.Length(R))
  else
    if R = '' then
      exit(System.Length(L));
  if System.Length(L) <= System.Length(R) then
    Result := GetLevDistMyers(Pointer(L), Pointer(R), System.Length(L), System.Length(R))
  else
    Result := GetLevDistMyers(Pointer(R), Pointer(L), System.Length(R), System.Length(L));
end;

// -----------------------------------------------------------------------------

{ Helpers}

// -----------------------------------------------------------------------------

function ListCompare(const Item1, Item2: PUWTMXItem): Integer;
begin
  if Item1^.Original < Item2^.Original then
    Result := -1
  else if Item1^.Original > Item2^.Original then
    Result := 1
  else
    Result := 0;
end;

// -----------------------------------------------------------------------------

function KeyMapCompare(const Key1, Key2: Double): Integer;
begin
  if Key1 < Key2 then
    Result := 1
  else if Key1 > Key2 then
    Result := -1
  else
    Result := 0;
end;

// -----------------------------------------------------------------------------

function StrSimilarityRatio(const Str1, Str2: String): Double;
var
  MaxLen: Integer;
begin
  Result := 1.0;
  MaxLen := Math.Max(Length(Str1), Length(Str2));

  if MaxLen <> 0 then
    Result := Result - (LevDistanceMyers(Str1, Str2) / MaxLen);
end;

// -----------------------------------------------------------------------------

{ TUWTMXThread }

// -----------------------------------------------------------------------------

constructor TUWTMXThread.Create(const ATMX: TUWTMX);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  OnTerminate := @DoOnTerminated;
  FTMX := ATMX;
  Start;
end;

// -----------------------------------------------------------------------------

destructor TUWTMXThread.Destroy;
begin
  FTMX := NIL;
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TUWTMXThread.DoOnFindFinished;
begin
  if Assigned(FTMX) then
    with FTMX do
      if Assigned(FOnFindFinished) then
        FOnFindFinished(FTMX, FMap.Count);
end;

// -----------------------------------------------------------------------------

procedure TUWTMXThread.DoOnTerminated(Sender: TObject);
begin
  if Assigned(FTMX) then
    FTMX.FThread := NIL;
end;

// -----------------------------------------------------------------------------

procedure TUWTMXThread.Execute;
var
  i: Integer;
  Percent: Double;
begin
  if Assigned(FTMX) then
    with FTMX do
    begin
      FMap.Clear;

      if (FList.Count > 0) and (FFind <> '') then
      begin
        for i := 0 to FList.Count-1 do
          if not Terminated then
          begin
            Percent := StrSimilarityRatio(FFind, FList.Items[i]^.Original);
            if Percent > FPercent then
            begin
              FMap.Add(Percent, i);
              if FMap.Count >= 5 then
                Break;
            end;
          end;
      end;
      Synchronize(@DoOnFindFinished);
    end;
end;

// -----------------------------------------------------------------------------

{ TUWTMX }

// -----------------------------------------------------------------------------

constructor TUWTMX.Create(const AFileName: String; ASrcLang: String = ''; ADstLang: String = ''; AOnFindFinished: TUWTMXOnFindFinished = NIL);
begin
  FillByte(FHeader, SizeOf(TUWTMXHeader), 0);
  if ASrcLang.IsEmpty then ASrcLang := 'en-US';
  if ADstLang.IsEmpty then ADstLang := 'es-ES';
  FLangs.SrcLang := ASrcLang;
  FLangs.DstLang := ADstLang;

  FList := TUWTMXList.Create;

  FMap := TUWTMXMap.Create;
  FMap.OnKeyCompare := @KeyMapCompare;
  FMap.Sorted := True;

  FFileName := '';
  FFind     := '';
  FChanged  := False;
  FPercent  := 0.65;

  FThread         := NIL;
  FOnFindFinished := AOnFindFinished;

  if AFileName <> '' then LoadFromFile(AFileName);
end;

// -----------------------------------------------------------------------------

destructor TUWTMX.Destroy;
begin
  Close;
  FList.Free;
  FMap.Clear;
  FMap.Free;

  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TUWTMX.Clear;
var
  i: Integer;
begin
  if FList.Count > 0 then
  begin
    for i := FList.Count-1 downto 0 do
    begin
      Dispose(PUWTMXItem(FList.Items[i]));
      FList.Items[i] := NIL;
      FList.Delete(i);
    end;
  end;

  FLangs.SrcLang := '';
  FLangs.DstLang := '';
end;

// -----------------------------------------------------------------------------

procedure TUWTMX.Close;
begin
  Save;
  Clear;
end;

// -----------------------------------------------------------------------------

procedure TUWTMX.Save;
begin
  if FChanged then
    SaveToFile(FFileName);
end;

// -----------------------------------------------------------------------------

procedure TUWTMX.LoadFromFile(const AFileName: String);
var
  XmlDoc: TXMLDocument;
  NodeTU, NodeTUV,
  NodeID, NodeText: TDOMNode;
  Item: PUWTMXItem;
begin
  FFileName := AFileName;
  if not FileExists(AFileName) then Exit;

  XmlDoc := NIL;
  ReadXMLFile(XmlDoc, AFileName);
  if Assigned(XmlDoc) then
  try
    // Header
    NodeID := XMLFindNodeByName(XmlDoc, 'header');
    if NodeID <> NIL then
      with FHeader, NodeID.Attributes do
      begin
        creationtool        := GetNamedItem('creationtool').NodeValue;
        creationtoolversion := GetNamedItem('creationtoolversion').NodeValue;
        datatype            := GetNamedItem('datatype').NodeValue;
        segtype             := GetNamedItem('segtype').NodeValue;
        adminlang           := GetNamedItem('adminlang').NodeValue;
        srclang             := GetNamedItem('srclang').NodeValue;
        otmf                := GetNamedItem('o-tmf').NodeValue;
      end;

    if FHeader.srclang <> '' then FLangs.SrcLang := FHeader.srclang;

    // TMs
    Item := NIL;
    NodeTU := XMLFindNodeByName(XmlDoc, 'tu');
    while NodeTU <> NIL do
    begin
      New(Item);
      FillByte(Item[0], SizeOf(PUWTMXItem), 0);

      NodeTUV := NodeTU.FirstChild;
      while NodeTUV <> NIL do
      begin
        NodeID := NodeTUV.Attributes.GetNamedItem('xml:lang');
        if NodeID <> NIL then
        begin
          if (NodeID.NodeValue = FLangs.SrcLang) then
          begin // src
            NodeText := NodeTUV.FirstChild;
            while NodeText <> NIL do
            begin
              if NodeText.NodeName = 'seg' then
                Item^.Original := ReplaceEnters(NodeText.TextContent, '|', sLineBreak);

              NodeText := NodeText.NextSibling;
            end;
          end
          else if (NodeID.NodeValue = FLangs.DstLang) then
          begin // dst
            NodeText := NodeTUV.FirstChild;
            while NodeText <> NIL do
            begin
              if NodeText.NodeName = 'seg' then
                Item^.Translated := ReplaceEnters(NodeText.TextContent, '|', sLineBreak)
              else if NodeText.NodeName = 'note' then
                Item^.Notes := NodeText.TextContent;

              NodeText := NodeText.NextSibling;
            end;
          end;
        end;
        NodeTUV := NodeTUV.NextSibling;
      end;
      NodeTU := NodeTU.NextSibling;

      if (Item^.Original <> '') and (Item^.Translated <> '') then
        FList.Add(Item)
      else
        Dispose(Item);
    end;
  finally
    XmlDoc.Free;
  end;
  // Sort for fast searching
  if FList.Count > 0 then FList.Sort(@ListCompare);
end;

// -----------------------------------------------------------------------------

function TUWTMX.SaveToFile(const AFileName: String): Boolean;
var
  XmlDoc : TXMLDocument;
  Root, Element, Node, SubNode : TDOMNode;
  i : Integer;
begin
  Result := False;
  if FList.Count = 0 then
    Exit
  else if not AFileName.IsEmpty then
    FFileName := AFileName;

  XmlDoc := TXMLDocument.Create;
  try
    Root := XmlDoc.CreateElement('tmx');
      TDOMElement(Root).SetAttribute('version', '1.4');
      XmlDoc.Appendchild(Root);
    Root := XmlDoc.DocumentElement;

    Element := XmlDoc.CreateElement('header');
      with TDOMElement(Element), FHeader do
      begin
        SetAttribute('creationtool', creationtool);
        SetAttribute('creationtoolversion', creationtoolversion);
        SetAttribute('datatype', datatype);
        SetAttribute('segtype', segtype);
        SetAttribute('adminlang', adminlang);
        SetAttribute('srclang', srclang);
        SetAttribute('o-tmf', otmf);

      end;
    Root.AppendChild(Element);

    Element := XmlDoc.CreateElement('body');
    Root.AppendChild(Element);

    Root := Element; // body

    for i := 0 to FList.Count-1 do
    begin
      Node := XmlDoc.CreateElement('tu');
      Root.AppendChild(Node);

      // original
      Element := XmlDoc.CreateElement('tuv');
      TDOMElement(Element).SetAttribute('xml:lang', FLangs.SrcLang);
        SubNode := XmlDoc.CreateElement('seg');
        SubNode.TextContent := ReplaceEnters(FList[i]^.Original);
      Element.AppendChild(SubNode); // tuv
      Node.AppendChild(Element); // tu

      // translated
      Element := XmlDoc.CreateElement('tuv');
      TDOMElement(Element).SetAttribute('xml:lang', FLangs.DstLang);
        SubNode := XmlDoc.CreateElement('seg');
        SubNode.TextContent := ReplaceEnters(FList[i]^.Translated);
        Element.AppendChild(SubNode); // tuv
        if FList[i]^.Notes <> '' then
        begin
          SubNode := XmlDoc.CreateElement('note');
          SubNode.TextContent := FList[i]^.Notes;
          Element.AppendChild(SubNode); // tuv
        end;
      Node.AppendChild(Element); // tu
    end;

    try
      WriteXMLFile(XmlDoc, FFileName);
      Result := True;
    except
    end;
  finally
    XmlDoc.Free;
  end;
end;

// -----------------------------------------------------------------------------

function TUWTMX.AddItem(const Original, Translated, Notes: String; const AllowDuplicate: Boolean = False): Integer;
var
  Item: PUWTMXItem;
  i: Integer;
begin
  Result := 0;
  if (Original = '') or (Translated = '') then Exit;

  if not AllowDuplicate and (FList.Count > 0) then
    for i := 0 to FList.Count-1 do
      if FList.Items[i]^.Original = Original then Exit;

  New(Item);
  Item^.Original   := Original;
  Item^.Translated := Translated;
  Item^.Notes      := Notes;
  Result   := FList.Add(Item)+1;
  FChanged := True;
end;

// -----------------------------------------------------------------------------

procedure TUWTMX.FindSimilary(const AText: String);
begin
  Cancel;
  FFind   := AText;
  FThread := TUWTMXThread.Create(Self);
end;

// -----------------------------------------------------------------------------

function TUWTMX.ItemFromMap(const AIndex: Integer): TUWTMXItem;
begin
  FillByte(Result, SizeOf(TUWTMXItem), 0);
  if (FMap.Count > 0) and InRange(AIndex, 0, FMap.Count-1) then
    Result := FList[ FMap.Data[AIndex] ]^;
end;

// -----------------------------------------------------------------------------

function TUWTMX.IndexFromMap(const AIndex: Integer): Integer;
begin
  if (FMap.Count > 0) and InRange(AIndex, 0, FMap.Count-1) then
    Result := FMap.Data[AIndex];
end;

// -----------------------------------------------------------------------------

function TUWTMX.GetLangPointer: PUWTMXItemInfo;
begin
  Result := PUWTMXItemInfo(@FLangs);
end;

// -----------------------------------------------------------------------------

function TUWTMX.Ready: Boolean;
begin
  Result := (FFileName <> '') and (FLangs.SrcLang <> '') and (FLangs.DstLang <> '');
end;

// -----------------------------------------------------------------------------

function TUWTMX.FillMapWithIndex(const AIndex: Integer): Integer;
begin
  Result := 0;
  FMap.Clear;

  if (FList.Count > 0) and InRange(AIndex, 0, FList.Count-1) then
  begin
    FMap.Add(1.0, AIndex);
    Result := FMap.Count;
  end;
end;

// -----------------------------------------------------------------------------

function TUWTMX.Find(AText: String): Integer;
var
  i: Integer;
begin
  Result := 0;

  if (FList.Count > 0) and (AText <> '') then
  begin
    AText := AText.ToLower;
    for i := 0 to FList.Count-1 do
    begin
      Result := Pos(AText, FList.Items[i]^.Original.ToLower);
      if Result > 0 then
      begin
        Result := i;
        Break;
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWTMX.Cancel;
begin
  if Assigned(FThread) then
    with FThread do
    begin
      Terminate;
      WaitFor;
    end;
end;

// -----------------------------------------------------------------------------

end.
