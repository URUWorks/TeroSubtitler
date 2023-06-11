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
 *  Copyright (C) 2001-2023 URUWorks, uruworks@gmail.com.
 *}

unit UWSubtitleAPI.Formats.EBU.Types;

//------------------------------------------------------------------------------

interface

uses
  StrUtils;

type

  { TGSIBlock }

  TGSIBlock = record
    CodePageNumber            : array[0..2] of AnsiChar;   // CPN (i.e. 437)
    DiskFormatCode            : array[0..7] of AnsiChar;   // DFC
    DisplayStandardCode       : Byte;                      // DSC
    CharCodeTableNumber       : array[0..1] of AnsiChar;   // CCT
    LanguageCode              : array[0..1] of AnsiChar;   // LC
    OriginalProgrammeTitle    : array[0..31] of AnsiChar;  // OPT
    OriginalEpisodeTitle      : array[0..31] of AnsiChar;  // OET
    TranslatedProgrammeTitle  : array[0..31] of AnsiChar;  // TPT
    TranslatedEpisodeTitle    : array[0..31] of AnsiChar;  // TET
    TranslatorName            : array[0..31] of AnsiChar;  // TN
    TranslatorContactDetails  : array[0..31] of AnsiChar;  // TCD
    SubtitleListRefCode       : array[0..15] of AnsiChar;  // SLR
    CreationDate              : array[0..5] of AnsiChar;   // CD (i.e. YYMMDD)
    RevisionDate              : array[0..5] of AnsiChar;   // RD (i.e. YYMMDD)
    RevisionNumber            : array[0..1] of AnsiChar;   // RN
    TotalNumberTTIBlocks      : array[0..4] of AnsiChar;   // TNB (0-99 999 decimal)
    TotalNumberSubtitles      : array[0..4] of AnsiChar;   // TNS (0-99 999 decimal)
    TotalNumberSubtitleGroups : array[0..2] of AnsiChar;   // TNG (0-255 decimal)
    MaxNumberDisplayableChars : array[0..1] of AnsiChar;   // MNC (0-99 decimal)
    MaxNumberDisplayableRows  : array[0..1] of AnsiChar;   // MNR (0-99 decimal)
    TimeCodeStatus            : Byte;                      // TCS
    TimeCodeStartProgramme    : array[0..7] of AnsiChar;   // TCP (i.e. HHMMSSFF)
    TimeCodeFirstCue          : array[0..7] of AnsiChar;   // TCF (i.e. HHMMSSFF)
    TotalNumberDisks          : Byte;                      // TND (max 9)
    DiskSequenceNumber        : Byte;                      // DSN (i.e. 1-TND)
    CountryOrigin             : array[0..2] of AnsiChar;   // CO
    Publisher                 : array[0..31] of AnsiChar;  // PUB
    EditorName                : array[0..31] of AnsiChar;  // EN
    EditorContact             : array[0..31] of AnsiChar;  // ECD
    SpareBytes                : array[0..74] of Byte;
    UserDefinedArea           : array[0..575] of AnsiChar; // UDA
  end;

  { TTTIBlock }

  TTTIBlock = record
    SubtitleGroupNumber : Byte;                  // SGN (00h-FFh)
    SubtitleNumber      : array[0..1] of Byte;   // SN  (0000h-FFFFh)
    ExtBlockNumber      : Byte;                  // EBN (max 256)
    CumulativeStatus    : Byte;                  // CS  (00h-03h)
    TimeCodeIn          : array[0..3] of Byte;   // TCI (00..23/00..59/00..59/00.24-29)
    TimeCodeOut         : array[0..3] of Byte;   // TCO (00..23/00..59/00..59/00.24-29)
    VerticalPosition    : Byte;                  // VP
    JustificationCode   : Byte;                  // JC
    CommentFlag         : Byte;                  // CF
    TextField           : array[0..111] of Byte; // TF (CR/LF=8Ah,
                                                 // last TTI block of a subtitle must always terminate with code 8Fh,
                                                 // unused space will be set to 8Fh)
  end;

const

  // GSI and TTI block size
  GSIBlockSize         = 1024;
  TTIBlockSize         = 128;

  // Disk Format Code
  DFC_25               = 'STL25.01';
  DFC_30               = 'STL30.01';

  // Display Standard Code
  DSC_Undefined        = $20;
  DSC_OpenSubtitling   = $30;
  DSC_Level1Teletext   = $31;
  DSC_Level2Teletext   = $32;

  // TimeCode Status
  TCS_NotUse           = $30;
  TCS_Use              = $31;

  // Cummulative Status Code
  CS_Not               = $00;
  CS_First             = $01;
  CS_Intermediate      = $02;
  CS_Last              = $03;

  // Justification Code
  JC_Unchanged         = $00;
  JC_Left              = $01;
  JC_Center            = $02;
  JC_Right             = $03;

  // Comment Flag
  CF_TFHaveData        = $00;
  CF_TFHaveComments    = $01;

  // Control Codes
  TF_ItalicsOn         = $80;
  TF_ItalicsOff        = $81;
  TF_UnderlineOn       = $82;
  TF_UnderlineOff      = $83;
  TF_BoxingOn          = $84;
  TF_BoxingOff         = $85;
  TF_CRLF              = $8A;
  TF_UnusedSpace       = $8F;

  // Teletext Control Codes (00h-1Fh)
  TTC_AlphaBlack       = $00;
  TTC_AlphaRed         = $01;
  TTC_AlphaGreen       = $02;
  TTC_AlphaYellow      = $03;
  TTC_AlphaBlue        = $04;
  TTC_AlphaMagenta     = $05;
  TTC_AlphaCyn         = $06;
  TTC_AlphaWhite       = $07;
  TTC_Flash            = $08;
  TTC_Steady           = $09;
  TTC_EndBox           = $0A;
  TTC_StartBox         = $0B;
  TTC_NormalHeight     = $0C;
  TTC_DoubleHeight     = $0D;
  TTC_DoubleWidth      = $0E;
  TTC_DoubleSize       = $0F;
  TTC_MosaicBlack      = $10;
  TTC_MosaicRed        = $11;
  TTC_MosaicGreen      = $12;
  TTC_MosaicYellow     = $13;
  TTC_MosaicBlue       = $14;
  TTC_MosaicMagenta    = $15;
  TTC_MosaicCyn        = $16;
  TTC_MosaicWhite      = $17;
  TTC_Conceal          = $18;
  TTC_ContiguousMosaic = $19;
  TTC_SeparatedMosaic  = $1A;
  TTC_BlackBackground  = $1C;
  TTC_NewBackground    = $1D;
  TTC_HoldMosaic       = $1E;
  TTC_ReleaseMosaic    = $1F;

function CharCodeTableToEncoding(const CCT: AnsiString): Integer;
function ReplaceTagsEBU2TS(const Text: String): String;
function ReplaceTagsTS2EBU(const Text: String): String;

//------------------------------------------------------------------------------

implementation

uses SysUtils, UWSystem.Encoding;

//------------------------------------------------------------------------------

function CharCodeTableToEncoding(const CCT: AnsiString): Integer;
begin
  if CCT = '00' then
    Result := GetEncodingInteger('x-cp20261')
  else if CCT = '01' then // Latin/Cyrillic 8859/5-1988
    Result := GetEncodingInteger('iso-8859-5')
  else if CCT = '02' then // Latin/Arabic 8859/6-1987
    Result := GetEncodingInteger('iso-8859-6')
  else if CCT = '03' then // Latin/Greek 8859/7-1987
    Result := GetEncodingInteger('iso-8859-7')
  else if CCT = '04' then // Latin/Hebrew 8859/8-1988
    Result := GetEncodingInteger('iso-8859-8')
  else // Latin 6937/2-1983/Add.1:1989
    Result := GetEncodingInteger('x-cp20269'); // x-cp20261
end;

//------------------------------------------------------------------------------

function ReplaceTagsEBU2TS(const Text: String): String;
var
  i: Integer;
begin
  // Unused space
  Result := ReplaceStr(Text, Chr(TF_UnusedSpace), '');
  // Codes reserved for future use
  for i := $86 to $89 do Result := ReplaceStr(Result, Chr(i), '');
  for i := $8B to $8E do Result := ReplaceStr(Result, Chr(i), '');
  for i := $90 to $9F do Result := ReplaceStr(Result, Chr(i), '');
  // Teletext Control Codes, TO DO!
  for i := 0 to 31 do Result := ReplaceStr(Result, Chr(i), '');

  Result := ReplaceStr(Result, '|', LineEnding);
  Result := ReplaceStr(Result, Chr(TF_CRLF), LineEnding);
  Result := ReplaceStr(Result, Chr(TF_ItalicsOn), '{\i1}');
  Result := ReplaceStr(Result, Chr(TF_ItalicsOff), '{\i0}');
  Result := ReplaceStr(Result, Chr(TF_UnderlineOn), '{\u1}');
  Result := ReplaceStr(Result, Chr(TF_UnderlineOff), '{\u0}');
  Result := ReplaceStr(Result, Chr(TF_BoxingOn), '{\b1}');
  Result := ReplaceStr(Result, Chr(TF_BoxingOff), '{\b0}');

  Result := Result.Trim;
end;

//------------------------------------------------------------------------------

function ReplaceTagsTS2EBU(const Text: String): String;
begin
  Result := ReplaceStr(Text, LineEnding, '|');
  //Result := ReplaceStr(Text, LineEnding, Chr(TF_CRLF));
  Result := ReplaceStr(Result, '{\i1}', Chr(TF_ItalicsOn));
  Result := ReplaceStr(Result, '{\i0}', Chr(TF_ItalicsOff));
  Result := ReplaceStr(Result, '{\u1}', Chr(TF_UnderlineOn));
  Result := ReplaceStr(Result, '{\u0}', Chr(TF_UnderlineOff));
  Result := ReplaceStr(Result, '{\b1}', Chr(TF_BoxingOn));
  Result := ReplaceStr(Result, '{\b1}', Chr(TF_BoxingOff));
end;

//------------------------------------------------------------------------------

end.
