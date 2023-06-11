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

unit UWSubtitleAPI.Formats.Cavena890.Types;

//------------------------------------------------------------------------------

interface

type

  { THeaderBlock }

  THeaderBlock = record
    UnknowCodes1      : array[0..1] of Byte;      // $00$00
    TapeNumber        : array[0..37] of AnsiChar; // end with $00$00
    TranslatedTitle   : array[0..27] of AnsiChar;
    Translator        : array[0..36] of AnsiChar;
    TranslatedEpisode : array[0..32] of AnsiChar;
    UnknowCodes2      : array[0..9] of Byte;
    Comments          : array[0..38] of AnsiChar;
    PrimaryFont       : array[0..6] of AnsiChar;
    PrimaryLanguage   : Byte;                     // $09 English
    UnknowCodes3      : array[0..22] of Byte;
    OriginalTitle     : array[0..27] of AnsiChar;
    SecondaryFont     : array[0..6] of AnsiChar;
    SecondaryLanguage : Byte;
    UnknowCodes4      : array[0..1] of Byte;
    StartTime         : array[0..10] of AnsiChar; // 00:00:00:00
    UnknowCodes5      : array[0..25] of Byte;
    Producer          : array[0..37] of AnsiChar;
    EpisodeTitle      : array[0..56] of AnsiChar;
//    UnknowCodes6      : array[0..14] of Byte;
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

  LanguageId: array[0..11] of TLanguageId = (
    (ShortName: 'da';    Hex: LangIdDanish),
    (ShortName: 'en';    Hex: LangIdEnglish),
    (ShortName: 'es';    Hex: LangIdSpanish),
    (ShortName: 'fr';    Hex: LangIdFrench),
    (ShortName: 'sv';    Hex: LangIdSwedish),
    (ShortName: 'it';    Hex: LangIdItalian),
    (ShortName: 'pr';    Hex: LangIdPortuguese),
    (ShortName: 'el';    Hex: LangIdHebrew),
    (ShortName: 'ar';    Hex: LangIdArabic),
    (ShortName: 'ru';    Hex: LangIdRussian),
    (ShortName: 'zh-CN'; Hex: LangIdChineseTraditional),
    (ShortName: 'zh-TW'; Hex: LangIdChineseSimplified)
  );

  TUnkownCodes : array[0..6] of Byte = ($00, $00, $00, $00, $00, $00, $00);

  THebrewCodes : array[0..26] of Byte =
  (
    $40, // א
    $41, // ב
    $42, // ג
    $43, // ד
    $44, // ה
    $45, // ו
    $46, // ז
    $47, // ח
    $49, // י
    $4c, // ל
    $4d, // ם
    $4e, // מ
    $4f, // ן
    $50, // נ
    $51, // ס
    $52, // ע
    $54, // פ
    $56, // צ
    $57, // ק
    $58, // ר
    $59, // ש
    $5A, // ת
    $4b, // כ
    $4a, // ך
    $48, // ט
    $53, // ף
    $55  // ץ
  );

  THebrewLetters : array[0..26] of String =
  (
    'א',
    'ב',
    'ג',
    'ד',
    'ה',
    'ו',
    'ז',
    'ח',
    'י',
    'ל',
    'ם',
    'מ',
    'ן',
    'נ',
    'ס',
    'ע',
    'פ',
    'צ',
    'ק',
    'ר',
    'ש',
    'ת',
    'כ',
    'ך',
    'ט',
    'ף',
    'ץ'
  );

  TRussianCodes : array[0..43] of Byte =
  (
    $42, // Б
    $45, // Е
    $5A, // З
    $56, // В
    $49, // И
    $4E, // Н
    $58, // Ы
    $51, // Я
    $56, // V
    $53, // С
    $72, // р
    $69, // и
    $71, // я
    $6E, // н
    $74, // т
    $5C, // Э
    $77, // ю
    $46, // Ф
    $5E, // Ч
    $44, // Д
    $62, // б
    $73, // с
    $75, // у
    $64, // д
    $60, // ж
    $6A, // й
    $6C, // л
    $47, // Г
    $78, // ы
    $7A, // з
    $7E, // ч
    $6D, // м
    $67, // г
    $79, // ь
    $70, // п
    $76, // в
    $55, // У
    $7D, // щ
    $66, // ф
    $7C, // э
    $7B, // ш
    $50, // П
    $52, // П
    $68  // П
  );

  TRussianLetters : array[0..43] of String =
  (
    'Б',
    'Е',
    'З',
    'В',
    'И',
    'Н',
    'Ы',
    'Я',
    'V',
    'С',
    'р',
    'и',
    'я',
    'н',
    'т',
    'Э',
    'ю',
    'Ф',
    'Ч',
    'Д',
    'б',
    'с',
    'у',
    'д',
    'ж',
    'й',
    'л',
    'Г',
    'ы',
    'з',
    'ч',
    'м',
    'г',
    'ь',
    'п',
    'в',
    'У',
    'щ',
    'ф',
    'э',
    'ш',
    'П',
    'Р',
    'х'
  );

function GetCavena890LangIndex(const ALang: AnsiString): Integer; overload;
function GetCavena890LangIndex(const ALangId: Byte): Integer; overload;

//------------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------

function GetCavena890LangIndex(const ALang: AnsiString): Integer;
var
  i: Integer;
begin
  Result := 0;

  for i := 0 to Length(LanguageId)-1 do
    if ALang = LanguageId[i].ShortName then
      Exit(i);
end;

// -----------------------------------------------------------------------------

function GetCavena890LangIndex(const ALangId: Byte): Integer;
var
  i: Integer;
begin
  Result := 0;

  for i := 0 to Length(LanguageId)-1 do
    if ALangId = LanguageId[i].Hex then
      Exit(i);
end;

// -----------------------------------------------------------------------------

end.
