{*
 * URUWorks Subtitle API
 *
 * The contents of this file are used with permission, subject to
 * the Mozilla Public License Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/2.0.html
 *
 * Software distributed under the License is distributed on an
 * "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * Copyright (C) 2001-2023 URUWorks, uruworks@gmail.com.
 *}

unit UWSubtitleAPI.Formats.Cavena890.Types;

//------------------------------------------------------------------------------

interface

type

  { THeaderBlock }

  THeaderBlock = record
    Reserved1         : array[0..1] of Byte;      // $00$00
    TapeNumber        : array[0..37] of AnsiChar; // end with $00$00
    TranslatedTitle   : array[0..27] of AnsiChar;
    Translator        : array[0..36] of AnsiChar;
    TranslatedEpisode : array[0..32] of AnsiChar;
    Reserved2         : array[0..9] of Byte;
    Comments          : array[0..38] of AnsiChar;
    PrimaryFont       : array[0..6] of AnsiChar;
    PrimaryLanguage   : Byte;                     // $09 English
    Reserved3         : array[0..22] of Byte;
    OriginalTitle     : array[0..27] of AnsiChar;
    SecondaryFont     : array[0..6] of AnsiChar;
    SecondaryLanguage : Byte;
    Reserved4         : array[0..1] of Byte;
    StartTime         : array[0..10] of AnsiChar; // 00:00:00:00
    Reserved5         : array[0..25] of Byte;
    Producer          : array[0..37] of AnsiChar;
    EpisodeTitle      : array[0..56] of AnsiChar;
  end;

  TLanguageId = record
    ShortName : AnsiString;
    Hex       : Byte;
  end;

const

  LangIdDanish             = $07;
  LangIdEnglish            = $09;
  LangIdSpanish            = $20;
  LangIdFrench             = $25;
  LangIdSwedish            = $28;
  LangIdItalian            = $2B;
  LangIdPortuguese         = $37;
  LangIdHebrew             = $8F;
  LangIdArabic             = $80;
  LangIdRussian            = $56;
  LangIdChineseTraditional = $90;
  LangIdChineseSimplified  = $91;
  LangIdBulgarian          = $77;

  LanguageId: array[0..12] of TLanguageId = (
    (ShortName: 'da';    Hex: LangIdDanish),
    (ShortName: 'en';    Hex: LangIdEnglish),
    (ShortName: 'es';    Hex: LangIdSpanish),
    (ShortName: 'fr';    Hex: LangIdFrench),
    (ShortName: 'sv';    Hex: LangIdSwedish),
    (ShortName: 'it';    Hex: LangIdItalian),
    (ShortName: 'pt-BR'; Hex: LangIdPortuguese),
    (ShortName: 'he';    Hex: LangIdHebrew),
    (ShortName: 'ar';    Hex: LangIdArabic),
    (ShortName: 'ru';    Hex: LangIdRussian),
    (ShortName: 'zh-TW'; Hex: LangIdChineseTraditional),
    (ShortName: 'zh-CN'; Hex: LangIdChineseSimplified),
    (ShortName: 'bg';    Hex: LangIdBulgarian)
  );

  TReservedBlock : array[0..6] of Byte = ($00, $00, $00, $00, $00, $00, $00);

  THebrewCodes : array[0..26] of Byte =
  (
    $40, $41, $42, $43, $44, $45, $46, $47, $49, $4c, $4d, $4e, $4f, $50,
    $51, $52, $54, $56, $57, $58, $59, $5A, $4b, $4a, $48, $53, $55
  );

  THebrewLetters : array[0..26] of String =
  (
    'א', 'ב', 'ג', 'ד', 'ה', 'ו', 'ז', 'ח', 'י', 'ל', 'ם', 'מ', 'ן', 'נ',
    'ס', 'ע', 'פ', 'צ', 'ק', 'ר', 'ש', 'ת', 'כ', 'ך', 'ט', 'ף', 'ץ'
  );

  TRussianCodes : array[0..43] of Byte =
  (
    $42, $45, $5A, $56, $49, $4E, $58, $51, $56, $53, $72, $69, $71, $6E,
    $74, $5C, $77, $46, $5E, $44, $62, $73, $75, $64, $60, $6A, $6C, $47,
    $78, $7A, $7E, $6D, $67, $79, $70, $76, $55, $7D, $66, $7C, $7B, $50,
    $52, $68
  );

  TRussianLetters : array[0..43] of String =
  (
    'Б', 'Е', 'З', 'В', 'И', 'Н', 'Ы', 'Я', 'V', 'С', 'р', 'и', 'я', 'н',
    'т', 'Э', 'ю', 'Ф', 'Ч', 'Д', 'б', 'с', 'у', 'д', 'ж', 'й', 'л', 'Г',
    'ы', 'з', 'ч', 'м', 'г', 'ь', 'п', 'в', 'У', 'щ', 'ф', 'э', 'ш', 'П',
    'Р', 'х'
  );

function GetCavena890LangIndex(const ALang: AnsiString): Integer; overload;
function GetCavena890LangIndex(const ALangId: Byte): Integer; overload;

//------------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------

function GetCavena890LangIndex(const ALang: AnsiString): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Length(LanguageId)-1 do
    if ALang = LanguageId[I].ShortName then
      Exit(I);
end;

// -----------------------------------------------------------------------------

function GetCavena890LangIndex(const ALangId: Byte): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Length(LanguageId)-1 do
    if ALangId = LanguageId[I].Hex then
      Exit(I);
end;

// -----------------------------------------------------------------------------

end.
