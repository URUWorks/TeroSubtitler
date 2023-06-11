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
function GoogleGetLanguageIndex(ALang: String): Integer;

const
  GoogleTranslateLocale: array [0..56] of String = (
    'auto',
    'am',
    'ar',
    'eu',
    'bn',
    'en-GB',
    'pt-BR',
    'bg',
    'ca',
    'chr',
    'hr',
    'cs',
    'da',
    'nl',
    'en',
    'et',
    'fil',
    'fi',
    'fr',
    'de',
    'el',
    'gu',
    'iw',
    'hi',
    'hu',
    'is',
    'id',
    'it',
    'ja',
    'kn',
    'ko',
    'lv',
    'lt',
    'ms',
    'ml',
    'mr',
    'no',
    'pl',
    'pt-PT',
    'ro',
    'ru',
    'sr',
    'zh-CN',
    'sk',
    'sl',
    'es',
    'sw',
    'sv',
    'ta',
    'te',
    'th',
    'zh-TW',
    'tr',
    'ur',
    'uk',
    'vi',
    'cy'
  );

  GoogleTranslateName: array [0..56] of String = (
    'Detect',
    'Amharic',
    'Arabic',
    'Basque',
    'Bengali',
    'English (UK)',
    'Portuguese (Brazil)',
    'Bulgarian',
    'Catalan',
    'Cherokee',
    'Croatian',
    'Czech',
    'Danish',
    'Dutch',
    'English (US)',
    'Estonian',
    'Filipino',
    'Finnish',
    'French',
    'German',
    'Greek',
    'Gujarati',
    'Hebrew',
    'Hindi',
    'Hungarian',
    'Icelandic',
    'Indonesian',
    'Italian',
    'Japanese',
    'Kannada',
    'Korean',
    'Latvian',
    'Lithuanian',
    'Malay',
    'Malayalam',
    'Marathi',
    'Norwegian',
    'Polish',
    'Portuguese (Portugal)',
    'Romanian',
    'Russian',
    'Serbian',
    'Chinese (PRC)',
    'Slovak',
    'Slovenian',
    'Spanish',
    'Swahili',
    'Swedish',
    'Tamil',
    'Telugu',
    'Thai',
    'Chinese (Taiwan)',
    'Turkish',
    'Urdu',
    'Ukrainian',
    'Vietnamese',
    'Welsh'
  );

// -----------------------------------------------------------------------------

implementation

const
  GoogleTranslateURL = 'https://translate.googleapis.com/translate_a/single?client=gtx&q=%s&sl=%s&tl=%s&dt=t&ie=UTF-8&oe=UTF-8';

// -----------------------------------------------------------------------------

function CallGoogleTranslate(const AURL: String): TJSONStringType;
var
  Client  : TDownloader;
  AString : String;
begin
  Result := '';
  Client := TDownloader.Create;
  try
    if Client.DownloadToString(AURL, AString) then
      Result := AString;
  finally
    Client.Free;
  end;
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
            if Result <> '' then
              Result := Result + LineEnding + Trim(jaTranslationArray[0].AsString)
            else
              Result := Trim(jaTranslationArray[0].AsString);
          end;
        end;
      end;
    finally
      jdResponse.Free;
    end;
end;

// -----------------------------------------------------------------------------

function GoogleGetLanguageIndex(ALang: String): Integer;
var
  i: Integer;
begin
  Result := 0;

  //ALang := ALang.ToLower;
  for i := 1 to Length(GoogleTranslateLocale)-1 do
    if ALang = GoogleTranslateLocale[i] then
      Result := i;
end;

// -----------------------------------------------------------------------------

end.
