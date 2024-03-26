{*
 *  URUWorks Google Translate API
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

unit UWTranslateAPI.Google;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, Classes, fpjson, UWSystem.InetUtils, HTTPDefs;


function GoogleTranslateText(const AText, SourceLng, DestLng: String): String;
function GoogleGetLanguageIndex(const ACulture: String): Integer;

type
  TGoogleCultureInfo = record
    DisplayName : String;
    CultureName : AnsiString;
  end;

const
  MaxGoogleCultureCount = 135;

  GoogleCultureInfo: array[0..MaxGoogleCultureCount-1] of TGoogleCultureInfo =
  (
    (DisplayName: 'Detect'; CultureName: 'auto'),
    (DisplayName: 'Afrikaans'; CultureName: 'af'),
    (DisplayName: 'Albanian'; CultureName: 'sq'),
    (DisplayName: 'Amharic'; CultureName: 'am'),
    (DisplayName: 'Arabic'; CultureName: 'ar'),
    (DisplayName: 'Armenian'; CultureName: 'hy'),
    (DisplayName: 'Assamese'; CultureName: 'as'),
    (DisplayName: 'Aymara'; CultureName: 'ay'),
    (DisplayName: 'Azerbaijani'; CultureName: 'az'),
    (DisplayName: 'Bambara'; CultureName: 'bm'),
    (DisplayName: 'Basque'; CultureName: 'eu'),
    (DisplayName: 'Belarusian'; CultureName: 'be'),
    (DisplayName: 'Bengali'; CultureName: 'bn'),
    (DisplayName: 'Bhojpuri'; CultureName: 'bho'),
    (DisplayName: 'Bosnian'; CultureName: 'bs'),
    (DisplayName: 'Bulgarian'; CultureName: 'bg'),
    (DisplayName: 'Catalan'; CultureName: 'ca'),
    (DisplayName: 'Cebuano'; CultureName: 'ceb'),
    (DisplayName: 'Chinese (Simplified)'; CultureName: 'zh-CN'),
    (DisplayName: 'Chinese (Traditional)'; CultureName: 'zh-TW'),
    (DisplayName: 'Corsican'; CultureName: 'co'),
    (DisplayName: 'Croatian'; CultureName: 'hr'),
    (DisplayName: 'Czech'; CultureName: 'cs'),
    (DisplayName: 'Danish'; CultureName: 'da'),
    (DisplayName: 'Dhivehi'; CultureName: 'dv'),
    (DisplayName: 'Dogri'; CultureName: 'doi'),
    (DisplayName: 'Dutch'; CultureName: 'nl'),
    (DisplayName: 'English'; CultureName: 'en'),
    (DisplayName: 'Esperanto'; CultureName: 'eo'),
    (DisplayName: 'Estonian'; CultureName: 'et'),
    (DisplayName: 'Ewe'; CultureName: 'ee'),
    (DisplayName: 'Filipino (Tagalog)'; CultureName: 'fil'),
    (DisplayName: 'Finnish'; CultureName: 'fi'),
    (DisplayName: 'French'; CultureName: 'fr'),
    (DisplayName: 'Frisian'; CultureName: 'fy'),
    (DisplayName: 'Galician'; CultureName: 'gl'),
    (DisplayName: 'Georgian'; CultureName: 'ka'),
    (DisplayName: 'German'; CultureName: 'de'),
    (DisplayName: 'Greek'; CultureName: 'el'),
    (DisplayName: 'Guarani'; CultureName: 'gn'),
    (DisplayName: 'Gujarati'; CultureName: 'gu'),
    (DisplayName: 'Haitian Creole'; CultureName: 'ht'),
    (DisplayName: 'Hausa'; CultureName: 'ha'),
    (DisplayName: 'Hawaiian'; CultureName: 'haw'),
    (DisplayName: 'Hebrew'; CultureName: 'he'),
    (DisplayName: 'Hindi'; CultureName: 'hi'),
    (DisplayName: 'Hmong'; CultureName: 'hmn'),
    (DisplayName: 'Hungarian'; CultureName: 'hu'),
    (DisplayName: 'Icelandic'; CultureName: 'is'),
    (DisplayName: 'Igbo'; CultureName: 'ig'),
    (DisplayName: 'Ilocano'; CultureName: 'ilo'),
    (DisplayName: 'Indonesian'; CultureName: 'id'),
    (DisplayName: 'Irish'; CultureName: 'ga'),
    (DisplayName: 'Italian'; CultureName: 'it'),
    (DisplayName: 'Japanese'; CultureName: 'ja'),
    (DisplayName: 'Javanese'; CultureName: 'jv'),
    (DisplayName: 'Kannada'; CultureName: 'kn'),
    (DisplayName: 'Kazakh'; CultureName: 'kk'),
    (DisplayName: 'Khmer'; CultureName: 'km'),
    (DisplayName: 'Kinyarwanda'; CultureName: 'rw'),
    (DisplayName: 'Konkani'; CultureName: 'gom'),
    (DisplayName: 'Korean'; CultureName: 'ko'),
    (DisplayName: 'Krio'; CultureName: 'kri'),
    (DisplayName: 'Kurdish'; CultureName: 'ku'),
    (DisplayName: 'Kurdish (Sorani)'; CultureName: 'ckb'),
    (DisplayName: 'Kyrgyz'; CultureName: 'ky'),
    (DisplayName: 'Lao'; CultureName: 'lo'),
    (DisplayName: 'Latin'; CultureName: 'la'),
    (DisplayName: 'Latvian'; CultureName: 'lv'),
    (DisplayName: 'Lingala'; CultureName: 'ln'),
    (DisplayName: 'Lithuanian'; CultureName: 'lt'),
    (DisplayName: 'Luganda'; CultureName: 'lg'),
    (DisplayName: 'Luxembourgish'; CultureName: 'lb'),
    (DisplayName: 'Macedonian'; CultureName: 'mk'),
    (DisplayName: 'Maithili'; CultureName: 'mai'),
    (DisplayName: 'Malagasy'; CultureName: 'mg'),
    (DisplayName: 'Malay'; CultureName: 'ms'),
    (DisplayName: 'Malayalam'; CultureName: 'ml'),
    (DisplayName: 'Maltese'; CultureName: 'mt'),
    (DisplayName: 'Maori'; CultureName: 'mi'),
    (DisplayName: 'Marathi'; CultureName: 'mr'),
    (DisplayName: 'Meiteilon (Manipuri)'; CultureName: 'mni-Mtei'),
    (DisplayName: 'Mizo'; CultureName: 'lus'),
    (DisplayName: 'Mongolian'; CultureName: 'mn'),
    (DisplayName: 'Myanmar (Burmese)'; CultureName: 'my'),
    (DisplayName: 'Nepali'; CultureName: 'ne'),
    (DisplayName: 'Norwegian'; CultureName: 'no'),
    (DisplayName: 'Nyanja (Chichewa)'; CultureName: 'ny'),
    (DisplayName: 'Odia (Oriya)'; CultureName: 'or'),
    (DisplayName: 'Oromo'; CultureName: 'om'),
    (DisplayName: 'Pashto'; CultureName: 'ps'),
    (DisplayName: 'Persian'; CultureName: 'fa'),
    (DisplayName: 'Polish'; CultureName: 'pl'),
    (DisplayName: 'Portuguese (Portugal, Brazil)'; CultureName: 'pt'),
    (DisplayName: 'Punjabi'; CultureName: 'pa'),
    (DisplayName: 'Quechua'; CultureName: 'qu'),
    (DisplayName: 'Romanian'; CultureName: 'ro'),
    (DisplayName: 'Russian'; CultureName: 'ru'),
    (DisplayName: 'Samoan'; CultureName: 'sm'),
    (DisplayName: 'Sanskrit'; CultureName: 'sa'),
    (DisplayName: 'Scots Gaelic'; CultureName: 'gd'),
    (DisplayName: 'Sepedi'; CultureName: 'nso'),
    (DisplayName: 'Serbian'; CultureName: 'sr'),
    (DisplayName: 'Sesotho'; CultureName: 'st'),
    (DisplayName: 'Shona'; CultureName: 'sn'),
    (DisplayName: 'Sindhi'; CultureName: 'sd'),
    (DisplayName: 'Sinhala (Sinhalese)'; CultureName: 'si'),
    (DisplayName: 'Slovak'; CultureName: 'sk'),
    (DisplayName: 'Slovenian'; CultureName: 'sl'),
    (DisplayName: 'Somali'; CultureName: 'so'),
    (DisplayName: 'Spanish'; CultureName: 'es'),
    (DisplayName: 'Sundanese'; CultureName: 'su'),
    (DisplayName: 'Swahili'; CultureName: 'sw'),
    (DisplayName: 'Swedish'; CultureName: 'sv'),
    (DisplayName: 'Tagalog (Filipino)'; CultureName: 'tl'),
    (DisplayName: 'Tajik'; CultureName: 'tg'),
    (DisplayName: 'Tamil'; CultureName: 'ta'),
    (DisplayName: 'Tatar'; CultureName: 'tt'),
    (DisplayName: 'Telugu'; CultureName: 'te'),
    (DisplayName: 'Thai'; CultureName: 'th'),
    (DisplayName: 'Tigrinya'; CultureName: 'ti'),
    (DisplayName: 'Tsonga'; CultureName: 'ts'),
    (DisplayName: 'Turkish'; CultureName: 'tr'),
    (DisplayName: 'Turkmen'; CultureName: 'tk'),
    (DisplayName: 'Twi (Akan)'; CultureName: 'ak'),
    (DisplayName: 'Ukrainian'; CultureName: 'uk'),
    (DisplayName: 'Urdu'; CultureName: 'ur'),
    (DisplayName: 'Uyghur'; CultureName: 'ug'),
    (DisplayName: 'Uzbek'; CultureName: 'uz'),
    (DisplayName: 'Vietnamese'; CultureName: 'vi'),
    (DisplayName: 'Welsh'; CultureName: 'cy'),
    (DisplayName: 'Xhosa'; CultureName: 'xh'),
    (DisplayName: 'Yiddish'; CultureName: 'yi'),
    (DisplayName: 'Yoruba'; CultureName: 'yo'),
    (DisplayName: 'Zulu'; CultureName: 'zu')
  );

// -----------------------------------------------------------------------------

implementation

const
  GoogleTranslateURL = 'https://translate.googleapis.com/translate_a/single?client=gtx&ie=UTF-8&oe=UTF-8&dt=bd&dt=ex&dt=ld&dt=md&dt=rw&dt=rm&dt=ss&dt=t&dt=at&dt=qc&q=%s&sl=%s&tl=%s';


// -----------------------------------------------------------------------------

function CallGoogleTranslate(const AURL: String): TJSONStringType;
var
  AString : String;
begin
  Result := '';
  if DownloadToString(AURL, AString) then
    Result := AString;
end;

// -----------------------------------------------------------------------------

function GoogleTranslateText(const AText, SourceLng, DestLng: String): String;
var
  Index: Integer;
  strResponse: TJSONStringType;
  jdResponse, jdTranslation, jdTranslationArray: TJSONData;
  jaTranslation, jaTranslationArray: TJSONArray;
begin
  Result := '';
  if (AText = '') or (SourceLng = '') or (DestLng = '') then Exit;

  strResponse := CallGoogleTranslate(Format(GoogleTranslateURL,[HTTPEncode(AText), SourceLng, DestLng]));
  if strResponse <> '' then
    try
      jdResponse    := GetJSON(strResponse);
      jdTranslation := jdResponse.FindPath('[0]');
      if (jdTranslation <> NIL) and (jdTranslation.JSONType = jtArray) then
      begin
        jaTranslation := TJSONArray(jdTranslation);
        for Index := 0 to Pred(jaTranslation.Count) do
        begin
          jdTranslationArray := jaTranslation[Index];
          if (jdTranslationArray <> NIL) and (jdTranslationArray.JSONType = jtArray) then
          begin
            jaTranslationArray := TJSONArray(jdTranslationArray);
            if jaTranslationArray[0].AsString <> '' then
              Result := Result + jaTranslationArray[0].AsString
            else
              Result := Result + LineEnding;
          end;
        end;
      end;
    finally
      jdResponse.Free;
    end;
end;

// -----------------------------------------------------------------------------

function GoogleGetLanguageIndex(const ACulture: String): Integer;
var
  i: Integer;
begin
  Result := 0;

  for i := 1 to MaxGoogleCultureCount-1 do
    if ACulture = GoogleCultureInfo[i].CultureName then
      Result := i;
end;

// -----------------------------------------------------------------------------

end.
