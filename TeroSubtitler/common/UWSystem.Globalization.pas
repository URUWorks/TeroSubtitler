{*
 *  URUWorks Globalization
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

unit UWSystem.Globalization;

// -----------------------------------------------------------------------------

{$C-}

interface

uses
  SysUtils, Classes;

function GetCultureDisplayName(const CultureName: AnsiString): String;
function GetCultureName(const Index: Integer): String; overload;
function GetCultureName(const ADisplayName: String): String; overload;
function GetCultureShortName(const Index: Integer): String;
procedure FillCultureTStrings(const Items: TStrings);

function ISO_639_2CodeToName(const Code: AnsiString): AnsiString;
function ISO_639_2NameToCode(const Name: String): AnsiString;
function ISO_639_2CodeFromIndex(const idx: Integer): AnsiString;
procedure ISO_639_2FillTStrings(const Items: TStrings);

function GetOSLanguage: String;

// -----------------------------------------------------------------------------

implementation

uses
  Math, GetText;

type
  TCultureInfo = record
    CultureName : AnsiString;
    DisplayName : String;
  end;

const
  MaxCultureItems = 137;

  CultureInfo: array[0..MaxCultureItems-1] of TCultureInfo =
  (
    (CultureName: 'af-ZA'; DisplayName: 'Afrikaans - South Africa'),
    (CultureName: 'sq-AL'; DisplayName: 'Albanian - Albania'),
    (CultureName: 'ar-DZ'; DisplayName: 'Arabic - Algeria'),
    (CultureName: 'ar-BH'; DisplayName: 'Arabic - Bahrain'),
    (CultureName: 'ar-EG'; DisplayName: 'Arabic - Egypt'),
    (CultureName: 'ar-IQ'; DisplayName: 'Arabic - Iraq'),
    (CultureName: 'ar-JO'; DisplayName: 'Arabic - Jordan'),
    (CultureName: 'ar-KW'; DisplayName: 'Arabic - Kuwait'),
    (CultureName: 'ar-LB'; DisplayName: 'Arabic - Lebanon'),
    (CultureName: 'ar-LY'; DisplayName: 'Arabic - Libya'),
    (CultureName: 'ar-MA'; DisplayName: 'Arabic - Morocco'),
    (CultureName: 'ar-OM'; DisplayName: 'Arabic - Oman'),
    (CultureName: 'ar-QA'; DisplayName: 'Arabic - Qatar'),
    (CultureName: 'ar-SA'; DisplayName: 'Arabic - Saudi Arabia'),
    (CultureName: 'ar-SY'; DisplayName: 'Arabic - Syria'),
    (CultureName: 'ar-TN'; DisplayName: 'Arabic - Tunisia'),
    (CultureName: 'ar-AE'; DisplayName: 'Arabic - United Arab Emirates'),
    (CultureName: 'ar-YE'; DisplayName: 'Arabic - Yemen'),
    (CultureName: 'hy-AM'; DisplayName: 'Armenian - Armenia'),
    (CultureName: 'Cy-az-AZ'; DisplayName: 'Azeri (Cyrillic) - Azerbaijan'),
    (CultureName: 'Lt-az-AZ'; DisplayName: 'Azeri (Latin) - Azerbaijan'),
    (CultureName: 'eu-ES'; DisplayName: 'Basque - Basque'),
    (CultureName: 'be-BY'; DisplayName: 'Belarusian - Belarus'),
    (CultureName: 'bg-BG'; DisplayName: 'Български - БАН'),
    (CultureName: 'ca-ES'; DisplayName: 'Catalan - Catalan'),
    (CultureName: 'zh-CN'; DisplayName: 'Chinese - China'),
    (CultureName: 'zh-HK'; DisplayName: 'Chinese - Hong Kong SAR'),
    (CultureName: 'zh-MO'; DisplayName: 'Chinese - Macau SAR'),
    (CultureName: 'zh-SG'; DisplayName: 'Chinese - Singapore'),
    (CultureName: 'zh-TW'; DisplayName: 'Chinese - Taiwan'),
    (CultureName: 'zh-CHS'; DisplayName: 'Chinese (Simplified)'),
    (CultureName: 'zh-CHT'; DisplayName: 'Chinese (Traditional)'),
    (CultureName: 'hr-HR'; DisplayName: 'Croatian - Croatia'),
    (CultureName: 'cs-CZ'; DisplayName: 'Czech - Czech Republic'),
    (CultureName: 'da-DK'; DisplayName: 'Danish - Denmark'),
    (CultureName: 'div-MV'; DisplayName: 'Dhivehi - Maldives'),
    (CultureName: 'nl-BE'; DisplayName: 'Dutch - Belgium'),
    (CultureName: 'nl-NL'; DisplayName: 'Dutch - The Netherlands'),
    (CultureName: 'en-AU'; DisplayName: 'English - Australia'),
    (CultureName: 'en-BZ'; DisplayName: 'English - Belize'),
    (CultureName: 'en-CA'; DisplayName: 'English - Canada'),
    (CultureName: 'en-CB'; DisplayName: 'English - Caribbean'),
    (CultureName: 'en-IE'; DisplayName: 'English - Ireland'),
    (CultureName: 'en-JM'; DisplayName: 'English - Jamaica'),
    (CultureName: 'en-NZ'; DisplayName: 'English - New Zealand'),
    (CultureName: 'en-PH'; DisplayName: 'English - Philippines'),
    (CultureName: 'en-ZA'; DisplayName: 'English - South Africa'),
    (CultureName: 'en-TT'; DisplayName: 'English - Trinidad and Tobago'),
    (CultureName: 'en-GB'; DisplayName: 'English - United Kingdom'),
    (CultureName: 'en-US'; DisplayName: 'English - United States'),
    (CultureName: 'en-ZW'; DisplayName: 'English - Zimbabwe'),
    (CultureName: 'et-EE'; DisplayName: 'Estonian - Estonia'),
    (CultureName: 'fo-FO'; DisplayName: 'Faroese - Faroe Islands'),
    (CultureName: 'fa-IR'; DisplayName: 'Farsi - Iran'),
    (CultureName: 'fi-FI'; DisplayName: 'Finnish - Finland'),
    (CultureName: 'fr-BE'; DisplayName: 'French - Belgium'),
    (CultureName: 'fr-CA'; DisplayName: 'French - Canada'),
    (CultureName: 'fr-FR'; DisplayName: 'French - France'),
    (CultureName: 'fr-LU'; DisplayName: 'French - Luxembourg'),
    (CultureName: 'fr-MC'; DisplayName: 'French - Monaco'),
    (CultureName: 'fr-CH'; DisplayName: 'French - Switzerland'),
    (CultureName: 'gl-ES'; DisplayName: 'Galician - Galician'),
    (CultureName: 'ka-GE'; DisplayName: 'Georgian - Georgia'),
    (CultureName: 'de-AT'; DisplayName: 'German - Austria'),
    (CultureName: 'de-DE'; DisplayName: 'German - Germany'),
    (CultureName: 'de-LI'; DisplayName: 'German - Liechtenstein'),
    (CultureName: 'de-LU'; DisplayName: 'German - Luxembourg'),
    (CultureName: 'de-CH'; DisplayName: 'German - Switzerland'),
    (CultureName: 'el-GR'; DisplayName: 'Greek - Greece'),
    (CultureName: 'gu-IN'; DisplayName: 'Gujarati - India'),
    (CultureName: 'he-IL'; DisplayName: 'Hebrew - Israel'),
    (CultureName: 'hi-IN'; DisplayName: 'Hindi - India'),
    (CultureName: 'hu-HU'; DisplayName: 'Hungarian - Hungary'),
    (CultureName: 'is-IS'; DisplayName: 'Icelandic - Iceland'),
    (CultureName: 'id-ID'; DisplayName: 'Indonesian - Indonesia'),
    (CultureName: 'it-IT'; DisplayName: 'Italian - Italy'),
    (CultureName: 'it-CH'; DisplayName: 'Italian - Switzerland'),
    (CultureName: 'ja-JP'; DisplayName: 'Japanese - Japan'),
    (CultureName: 'kn-IN'; DisplayName: 'Kannada - India'),
    (CultureName: 'kk-KZ'; DisplayName: 'Kazakh - Kazakhstan'),
    (CultureName: 'kok-IN'; DisplayName: 'Konkani - India'),
    (CultureName: 'ko-KR'; DisplayName: 'Korean - Korea'),
    (CultureName: 'ky-KZ'; DisplayName: 'Kyrgyz - Kazakhstan'),
    (CultureName: 'lv-LV'; DisplayName: 'Latvian - Latvia'),
    (CultureName: 'lt-LT'; DisplayName: 'Lithuanian - Lithuania'),
    (CultureName: 'mk-MK'; DisplayName: 'Macedonian (FYROM)'),
    (CultureName: 'ms-BN'; DisplayName: 'Malay - Brunei'),
    (CultureName: 'ms-MY'; DisplayName: 'Malay - Malaysia'),
    (CultureName: 'mr-IN'; DisplayName: 'Marathi - India'),
    (CultureName: 'mn-MN'; DisplayName: 'Mongolian - Mongolia'),
    (CultureName: 'nb-NO'; DisplayName: 'Norwegian (Bokmål) - Norway'),
    (CultureName: 'nn-NO'; DisplayName: 'Norwegian (Nynorsk) - Norway'),
    (CultureName: 'pl-PL'; DisplayName: 'Polish - Poland'),
    (CultureName: 'pt-BR'; DisplayName: 'Portuguese - Brazil'),
    (CultureName: 'pt-PT'; DisplayName: 'Portuguese - Portugal'),
    (CultureName: 'pa-IN'; DisplayName: 'Punjabi - India'),
    (CultureName: 'ro-RO'; DisplayName: 'Romanian - Romania'),
    (CultureName: 'ru-RU'; DisplayName: 'Russian - Russia'),
    (CultureName: 'sa-IN'; DisplayName: 'Sanskrit - India'),
    (CultureName: 'sr-Cyrl-BA'; DisplayName: 'Serbian (Cyrillic, Bosnia and Herzegovina)'),
    (CultureName: 'sr-Cyrl-RS'; DisplayName: 'Serbian (Cyrillic) - Serbia'),
    (CultureName: 'sr-Latn-RS'; DisplayName: 'Serbian (Latin) - Serbia'),
    (CultureName: 'sk-SK'; DisplayName: 'Slovak - Slovakia'),
    (CultureName: 'sl-SI'; DisplayName: 'Slovenian - Slovenia'),
    (CultureName: 'es-AR'; DisplayName: 'Español - Argentina'),
    (CultureName: 'es-BO'; DisplayName: 'Español - Bolivia'),
    (CultureName: 'es-CL'; DisplayName: 'Español - Chile'),
    (CultureName: 'es-CO'; DisplayName: 'Español - Colombia'),
    (CultureName: 'es-CR'; DisplayName: 'Español - Costa Rica'),
    (CultureName: 'es-DO'; DisplayName: 'Español - Dominican Republic'),
    (CultureName: 'es-EC'; DisplayName: 'Español - Ecuador'),
    (CultureName: 'es-SV'; DisplayName: 'Español - El Salvador'),
    (CultureName: 'es-GT'; DisplayName: 'Español - Guatemala'),
    (CultureName: 'es-HN'; DisplayName: 'Español - Honduras'),
    (CultureName: 'es-MX'; DisplayName: 'Español - Mexico'),
    (CultureName: 'es-NI'; DisplayName: 'Español - Nicaragua'),
    (CultureName: 'es-PA'; DisplayName: 'Español - Panama'),
    (CultureName: 'es-PY'; DisplayName: 'Español - Paraguay'),
    (CultureName: 'es-PE'; DisplayName: 'Español - Peru'),
    (CultureName: 'es-PR'; DisplayName: 'Español - Puerto Rico'),
    (CultureName: 'es-ES'; DisplayName: 'Español - Spain'),
    (CultureName: 'es-UY'; DisplayName: 'Español - Uruguay'),
    (CultureName: 'es-VE'; DisplayName: 'Español - Venezuela'),
    (CultureName: 'sw-KE'; DisplayName: 'Swahili - Kenya'),
    (CultureName: 'sv-FI'; DisplayName: 'Swedish - Finland'),
    (CultureName: 'sv-SE'; DisplayName: 'Swedish - Sweden'),
    (CultureName: 'syr-SY'; DisplayName: 'Syriac - Syria'),
    (CultureName: 'ta-IN'; DisplayName: 'Tamil - India'),
    (CultureName: 'tt-RU'; DisplayName: 'Tatar - Russia'),
    (CultureName: 'te-IN'; DisplayName: 'Telugu - India'),
    (CultureName: 'th-TH'; DisplayName: 'Thai - Thailand'),
    (CultureName: 'tr-TR'; DisplayName: 'Turkish - Turkey'),
    (CultureName: 'uk-UA'; DisplayName: 'Ukrainian - Ukraine'),
    (CultureName: 'ur-PK'; DisplayName: 'Urdu - Pakistan'),
    (CultureName: 'Cy-uz-UZ'; DisplayName: 'Uzbek (Cyrillic) - Uzbekistan'),
    (CultureName: 'Lt-uz-UZ'; DisplayName: 'Uzbek (Latin) - Uzbekistan'),
    (CultureName: 'vi-VN'; DisplayName: 'Vietnamese - Vietnam')
  );


  { http://www.loc.gov/standards/iso639-2/englangn.html }

  CISO_639_2Info: array [0..475] of TCultureInfo =
  (
   {0}(CultureName: 'aar'; DisplayName: 'Afar'),
    (CultureName: 'abk'; DisplayName: 'Abkhazian'),
    (CultureName: 'ace'; DisplayName: 'Achinese'),
    (CultureName: 'ach'; DisplayName: 'Acoli'),
    (CultureName: 'ada'; DisplayName: 'Adangme'),
    (CultureName: 'afa'; DisplayName: 'Afro-Asiatic (Other)'),
    (CultureName: 'afh'; DisplayName: 'Afrihili'),
    (CultureName: 'afr'; DisplayName: 'Afrikaans'),
    (CultureName: 'aka'; DisplayName: 'Akan'),
    (CultureName: 'akk'; DisplayName: 'Akkadian'),
    {10}(CultureName: 'alb'; DisplayName: 'Albanian'), // Also 'sqi'
    (CultureName: 'ale'; DisplayName: 'Aleut'),
    (CultureName: 'alg'; DisplayName: 'Algonquian languages'),
    (CultureName: 'amh'; DisplayName: 'Amharic'),
    (CultureName: 'ang'; DisplayName: 'English, Old (ca.450-1100)'),
    (CultureName: 'apa'; DisplayName: 'Apache languages'),
    (CultureName: 'ara'; DisplayName: 'Arabic'),
    (CultureName: 'arc'; DisplayName: 'Aramaic'),
    (CultureName: 'arg'; DisplayName: 'Aragonese'),
    (CultureName: 'arm'; DisplayName: 'Armenian'), // Also 'hye'
    {20}(CultureName: 'arn'; DisplayName: 'Araucanian'),
    (CultureName: 'arp'; DisplayName: 'Arapaho'),
    (CultureName: 'art'; DisplayName: 'Artificial (Other)'),
    (CultureName: 'arw'; DisplayName: 'Arawak'),
    (CultureName: 'asm'; DisplayName: 'Assamese'),
    (CultureName: 'ast'; DisplayName: 'Asturian; Bable'),
    (CultureName: 'ath'; DisplayName: 'Athapascan languages'),
    (CultureName: 'aus'; DisplayName: 'Australian languages'),
    (CultureName: 'ava'; DisplayName: 'Avaric'),
    (CultureName: 'ave'; DisplayName: 'Avestan'),
    {30}(CultureName: 'awa'; DisplayName: 'Awadhi'),
    (CultureName: 'aym'; DisplayName: 'Aymara'),
    (CultureName: 'aze'; DisplayName: 'Azerbaijani'),
    (CultureName: 'bad'; DisplayName: 'Banda'),
    (CultureName: 'bai'; DisplayName: 'Bamileke languages'),
    (CultureName: 'bak'; DisplayName: 'Bashkir'),
    (CultureName: 'bal'; DisplayName: 'Baluchi'),
    (CultureName: 'bam'; DisplayName: 'Bambara'),
    (CultureName: 'ban'; DisplayName: 'Balinese'),
    (CultureName: 'baq'; DisplayName: 'Basque'), // Also 'eus'
    {40}(CultureName: 'bas'; DisplayName: 'Basa'),
    (CultureName: 'bat'; DisplayName: 'Baltic (Other)'),
    (CultureName: 'bej'; DisplayName: 'Beja'),
    (CultureName: 'bel'; DisplayName: 'Belarusian'),
    (CultureName: 'bem'; DisplayName: 'Bemba'),
    (CultureName: 'ben'; DisplayName: 'Bengali'),
    (CultureName: 'ber'; DisplayName: 'Berber (Other)'),
    (CultureName: 'bho'; DisplayName: 'Bhojpuri'),
    (CultureName: 'bih'; DisplayName: 'Bihari'),
    (CultureName: 'bik'; DisplayName: 'Bikol'),
    {50}(CultureName: 'bin'; DisplayName: 'Bini'),
    (CultureName: 'bis'; DisplayName: 'Bislama'),
    (CultureName: 'bla'; DisplayName: 'Siksika'),
    (CultureName: 'bnt'; DisplayName: 'Bantu (Other)'),
    (CultureName: 'bod'; DisplayName: 'Tibetan'), // Also 'tib'
    (CultureName: 'bos'; DisplayName: 'Bosnian'),
    (CultureName: 'bra'; DisplayName: 'Braj'),
    (CultureName: 'bre'; DisplayName: 'Breton'),
    (CultureName: 'btk'; DisplayName: 'Batak (Indonesia)'),
    (CultureName: 'bua'; DisplayName: 'Buriat'),
    {60}(CultureName: 'bug'; DisplayName: 'Buginese'),
    (CultureName: 'bul'; DisplayName: 'Bulgarian'),
    (CultureName: 'bur'; DisplayName: 'Burmese'), // Also 'mya'
    (CultureName: 'cad'; DisplayName: 'Caddo'),
    (CultureName: 'cai'; DisplayName: 'Central American Indian (Other)'),
    (CultureName: 'car'; DisplayName: 'Carib'),
    (CultureName: 'cat'; DisplayName: 'Catalan'),
    (CultureName: 'cau'; DisplayName: 'Caucasian (Other)'),
    (CultureName: 'ceb'; DisplayName: 'Cebuano'),
    (CultureName: 'cel'; DisplayName: 'Celtic (Other)'),
    {70}(CultureName: 'ces'; DisplayName: 'Czech'), // Also 'cze'
    (CultureName: 'cha'; DisplayName: 'Chamorro'),
    (CultureName: 'chb'; DisplayName: 'Chibcha'),
    (CultureName: 'che'; DisplayName: 'Chechen'),
    (CultureName: 'chg'; DisplayName: 'Chagatai'),
    (CultureName: 'chi'; DisplayName: 'Chinese'), // Also 'zho'
    (CultureName: 'chk'; DisplayName: 'Chuukese'),
    (CultureName: 'chm'; DisplayName: 'Mari'),
    (CultureName: 'chn'; DisplayName: 'Chinook jargon'),
    (CultureName: 'cho'; DisplayName: 'Choctaw'),
    {80}(CultureName: 'chp'; DisplayName: 'Chipewyan'),
    (CultureName: 'chr'; DisplayName: 'Cherokee'),
    (CultureName: 'chu'; DisplayName: 'Old Bulgarian'),
    (CultureName: 'chv'; DisplayName: 'Chuvash'),
    (CultureName: 'chy'; DisplayName: 'Cheyenne'),
    (CultureName: 'cmc'; DisplayName: 'Chamic languages'),
    (CultureName: 'cop'; DisplayName: 'Coptic'),
    (CultureName: 'cor'; DisplayName: 'Cornish'),
    (CultureName: 'cos'; DisplayName: 'Corsican'),
    (CultureName: 'cpe'; DisplayName: 'Creoles and pidgins, English-based (Other)'),
    {90}(CultureName: 'cpf'; DisplayName: 'Creoles and pidgins, French-based (Other)'),
    (CultureName: 'cpp'; DisplayName: 'Creoles and pidgins, Portuguese-based (Other)'),
    (CultureName: 'cre'; DisplayName: 'Cree'),
    (CultureName: 'crp'; DisplayName: 'Creoles and pidgins(Other)'),
    (CultureName: 'cus'; DisplayName: 'Cushitic (Other)'),
    (CultureName: 'cym'; DisplayName: 'Welsh'), // Also 'wel'
    (CultureName: 'cze'; DisplayName: 'Czech'), // Also 'ces'
    (CultureName: 'dak'; DisplayName: 'Dakota'),
    (CultureName: 'dan'; DisplayName: 'Danish'),
    (CultureName: 'dar'; DisplayName: 'Dargwa'),
    {100}(CultureName: 'day'; DisplayName: 'Dayak'),
    (CultureName: 'del'; DisplayName: 'Delaware'),
    (CultureName: 'den'; DisplayName: 'Slave (Athapascan)'),
    (CultureName: 'deu'; DisplayName: 'German'), // Also 'ger'
    (CultureName: 'dgr'; DisplayName: 'Dogrib'),
    (CultureName: 'din'; DisplayName: 'Dinka'),
    (CultureName: 'div'; DisplayName: 'Divehi'),
    (CultureName: 'doi'; DisplayName: 'Dogri'),
    (CultureName: 'dra'; DisplayName: 'Dravidian (Other)'),
    (CultureName: 'dua'; DisplayName: 'Duala'),
    {110}(CultureName: 'dum'; DisplayName: 'Dutch, Middle (ca. 1050-1350)'),
    (CultureName: 'dut'; DisplayName: 'Dutch'), // Also 'nld'
    (CultureName: 'dyu'; DisplayName: 'Dyula'),
    (CultureName: 'dzo'; DisplayName: 'Dzongkha'),
    (CultureName: 'efi'; DisplayName: 'Efik'),
    (CultureName: 'egy'; DisplayName: 'Egyptian (Ancient)'),
    (CultureName: 'eka'; DisplayName: 'Ekajuk'),
    (CultureName: 'ell'; DisplayName: 'Greek, Modern (1453-)'), // Also 'gre'
    (CultureName: 'elx'; DisplayName: 'Elamite'),
    (CultureName: 'eng'; DisplayName: 'English'),
    {120}(CultureName: 'enm'; DisplayName: 'English, Middle (1100-1500)'),
    (CultureName: 'epo'; DisplayName: 'Esperanto'),
    (CultureName: 'est'; DisplayName: 'Estonian'),
    (CultureName: 'eus'; DisplayName: 'Basque'), // Also 'baq'
    (CultureName: 'ewe'; DisplayName: 'Ewe'),
    (CultureName: 'ewo'; DisplayName: 'Ewondo'),
    (CultureName: 'fan'; DisplayName: 'Fang'),
    (CultureName: 'fao'; DisplayName: 'Faroese'),
    (CultureName: 'fas'; DisplayName: 'Persian'), // Also 'per'
    (CultureName: 'fat'; DisplayName: 'Fanti'),
    {130}(CultureName: 'fij'; DisplayName: 'Fijian'),
    (CultureName: 'fin'; DisplayName: 'Finnish'),
    (CultureName: 'fiu'; DisplayName: 'Finno-Ugrian (Other)'),
    (CultureName: 'fon'; DisplayName: 'Fon'),
    (CultureName: 'fra'; DisplayName: 'French'), // Also 'fre'
    (CultureName: 'fre'; DisplayName: 'French'), // Also 'fra'
    (CultureName: 'frm'; DisplayName: 'French, Middle (ca.1400-1600)'),
    (CultureName: 'fro'; DisplayName: 'French, Old (842-ca.1400)'),
    (CultureName: 'fry'; DisplayName: 'Frisian'),
    (CultureName: 'ful'; DisplayName: 'Fulah'),
    {140}(CultureName: 'fur'; DisplayName: 'Friulian'),
    (CultureName: 'gaa'; DisplayName: 'Ga'),
    (CultureName: 'gay'; DisplayName: 'Gayo'),
    (CultureName: 'gba'; DisplayName: 'Gbaya'),
    (CultureName: 'gem'; DisplayName: 'Germanic (Other)'),
    (CultureName: 'geo'; DisplayName: 'Georgian'), // Also 'kat'
    (CultureName: 'ger'; DisplayName: 'German'), // Also 'deu'
    (CultureName: 'gez'; DisplayName: 'Geez'),
    (CultureName: 'gil'; DisplayName: 'Gilbertese'),
    (CultureName: 'gla'; DisplayName: 'Gaelic; Scottish Gaelic'),
    {150}(CultureName: 'gle'; DisplayName: 'Irish'),
    (CultureName: 'glg'; DisplayName: 'Gallegan'),
    (CultureName: 'glv'; DisplayName: 'Manx'),
    (CultureName: 'gmh'; DisplayName: 'German, Middle High (ca.1050-1500)'),
    (CultureName: 'goh'; DisplayName: 'German, Old High (ca.750-1050)'),
    (CultureName: 'gon'; DisplayName: 'Gondi'),
    (CultureName: 'gor'; DisplayName: 'Gorontalo'),
    (CultureName: 'got'; DisplayName: 'Gothic'),
    (CultureName: 'grb'; DisplayName: 'Grebo'),
    (CultureName: 'grc'; DisplayName: 'Greek, Ancient (to 1453)'),
    {160}(CultureName: 'gre'; DisplayName: 'Greek, Modern (1453-)'), // Also 'ell'
    (CultureName: 'grn'; DisplayName: 'Guarani'),
    (CultureName: 'guj'; DisplayName: 'Gujarati'),
    (CultureName: 'gwi'; DisplayName: 'Gwich´in'),
    (CultureName: 'hai'; DisplayName: 'Haida'),
    (CultureName: 'hau'; DisplayName: 'Hausa'),
    (CultureName: 'haw'; DisplayName: 'Hawaiian'),
    (CultureName: 'heb'; DisplayName: 'Hebrew'),
    (CultureName: 'her'; DisplayName: 'Herero'),
    (CultureName: 'hil'; DisplayName: 'Hiligaynon'),
    {170}(CultureName: 'him'; DisplayName: 'Himachali'),
    (CultureName: 'hin'; DisplayName: 'Hindi'),
    (CultureName: 'hit'; DisplayName: 'Hittite'),
    (CultureName: 'hmn'; DisplayName: 'Hmong'),
    (CultureName: 'hmo'; DisplayName: 'Hiri Motu'),
    (CultureName: 'hrv'; DisplayName: 'Croatian'), // Also 'scr'
    (CultureName: 'hun'; DisplayName: 'Hungarian'),
    (CultureName: 'hup'; DisplayName: 'Hupa'),
    (CultureName: 'hye'; DisplayName: 'Armenian'), // Also 'arm'
    (CultureName: 'iba'; DisplayName: 'Iban'),
    {180}(CultureName: 'ibo'; DisplayName: 'Igbo'),
    (CultureName: 'ice'; DisplayName: 'Icelandic'), // Also 'isl'
    (CultureName: 'ido'; DisplayName: 'Ido'),
    (CultureName: 'iii'; DisplayName: 'Sichuan Yi'),
    (CultureName: 'ijo'; DisplayName: 'Ijo'),
    (CultureName: 'iku'; DisplayName: 'Inuktitut'),
    (CultureName: 'ile'; DisplayName: 'Interlingue'),
    (CultureName: 'ilo'; DisplayName: 'Iloko'),
    (CultureName: 'ina'; DisplayName: 'Interlingua (International Auxiliary Language Association)'),
    (CultureName: 'inc'; DisplayName: 'Indic (Other)'),
    {190}(CultureName: 'ind'; DisplayName: 'Indonesian'),
    (CultureName: 'ine'; DisplayName: 'Indo-European (Other)'),
    (CultureName: 'inh'; DisplayName: 'Ingush'),
    (CultureName: 'ipk'; DisplayName: 'Inupiaq'),
    (CultureName: 'ira'; DisplayName: 'Iranian (Other)'),
    (CultureName: 'iro'; DisplayName: 'Iroquoian languages'),
    (CultureName: 'isl'; DisplayName: 'Icelandic'), // Also 'ice'
    (CultureName: 'ita'; DisplayName: 'Italian'),
    (CultureName: 'jav'; DisplayName: 'Javanese'),
    (CultureName: 'jpn'; DisplayName: 'Japanese'),
    {200}(CultureName: 'jpr'; DisplayName: 'Judeo-Persian'),
    (CultureName: 'jrb'; DisplayName: 'Judeo-Arabic'),
    (CultureName: 'kaa'; DisplayName: 'Kara-Kalpak'),
    (CultureName: 'kab'; DisplayName: 'Kabyle'),
    (CultureName: 'kac'; DisplayName: 'Kachin'),
    (CultureName: 'kal'; DisplayName: 'Kalaallisut'),
    (CultureName: 'kam'; DisplayName: 'Kamba'),
    (CultureName: 'kan'; DisplayName: 'Kannada'),
    (CultureName: 'kar'; DisplayName: 'Karen'),
    (CultureName: 'kas'; DisplayName: 'Kashmiri'),
    {210}(CultureName: 'kat'; DisplayName: 'Georgian'), // Also 'geo'
    (CultureName: 'kau'; DisplayName: 'Kanuri'),
    (CultureName: 'kaw'; DisplayName: 'Kawi'),
    (CultureName: 'kaz'; DisplayName: 'Kazakh'),
    (CultureName: 'kbd'; DisplayName: 'Kabardian'),
    (CultureName: 'kha'; DisplayName: 'Khasi'),
    (CultureName: 'khi'; DisplayName: 'Khoisan (Other)'),
    (CultureName: 'khm'; DisplayName: 'Khmer'),
    (CultureName: 'kho'; DisplayName: 'Khotanese'),
    (CultureName: 'kik'; DisplayName: 'Kikuyu; Gikuyu'),
    {220}(CultureName: 'kin'; DisplayName: 'Kinyarwanda'),
    (CultureName: 'kir'; DisplayName: 'Kirghiz'),
    (CultureName: 'kmb'; DisplayName: 'Kimbundu'),
    (CultureName: 'kok'; DisplayName: 'Konkani'),
    (CultureName: 'kom'; DisplayName: 'Komi'),
    (CultureName: 'kon'; DisplayName: 'Kongo'),
    (CultureName: 'kor'; DisplayName: 'Korean'),
    (CultureName: 'kos'; DisplayName: 'Kosraean'),
    (CultureName: 'kpe'; DisplayName: 'Kpelle'),
    (CultureName: 'kro'; DisplayName: 'Kru'),
    {230}(CultureName: 'kru'; DisplayName: 'Kurukh'),
    (CultureName: 'kua'; DisplayName: 'Kuanyama; Kwanyama'),
    (CultureName: 'kum'; DisplayName: 'Kumyk'),
    (CultureName: 'kur'; DisplayName: 'Kurdish'),
    (CultureName: 'kut'; DisplayName: 'Kutenai'),
    (CultureName: 'lad'; DisplayName: 'Ladino'),
    (CultureName: 'lah'; DisplayName: 'Lahnda'),
    (CultureName: 'lam'; DisplayName: 'Lamba'),
    (CultureName: 'lao'; DisplayName: 'Lao'),
    (CultureName: 'lat'; DisplayName: 'Latin'),
    {240}(CultureName: 'lav'; DisplayName: 'Latvian'),
    (CultureName: 'lez'; DisplayName: 'Lezghian'),
    (CultureName: 'lim'; DisplayName: 'Limburgan'),
    (CultureName: 'lin'; DisplayName: 'Lingala'),
    (CultureName: 'lit'; DisplayName: 'Lithuanian'),
    (CultureName: 'lol'; DisplayName: 'Mongo'),
    (CultureName: 'loz'; DisplayName: 'Lozi'),
    (CultureName: 'ltz'; DisplayName: 'Luxembourgish'),
    (CultureName: 'lua'; DisplayName: 'Luba-Lulua'),
    (CultureName: 'lub'; DisplayName: 'Luba-Katanga'),
    {250}(CultureName: 'lug'; DisplayName: 'Ganda'),
    (CultureName: 'lui'; DisplayName: 'Luiseno'),
    (CultureName: 'lun'; DisplayName: 'Lunda'),
    (CultureName: 'luo'; DisplayName: 'Luo (Kenya and Tanzania)'),
    (CultureName: 'lus'; DisplayName: 'Lushai'),
    (CultureName: 'mac'; DisplayName: 'Macedonian'), // Also 'mkd'
    (CultureName: 'mad'; DisplayName: 'Madurese'),
    (CultureName: 'mag'; DisplayName: 'Magahi'),
    (CultureName: 'mah'; DisplayName: 'Marshallese'),
    (CultureName: 'mai'; DisplayName: 'Maithili'),
    {260}(CultureName: 'mak'; DisplayName: 'Makasar'),
    (CultureName: 'mal'; DisplayName: 'Malayalam'),
    (CultureName: 'man'; DisplayName: 'Mandingo'),
    (CultureName: 'mao'; DisplayName: 'Maori'), // Also 'mri'
    (CultureName: 'map'; DisplayName: 'Austronesian (Other)'),
    (CultureName: 'mar'; DisplayName: 'Marathi'),
    (CultureName: 'mas'; DisplayName: 'Masai'),
    (CultureName: 'may'; DisplayName: 'Malay'), // Also 'msa'
    (CultureName: 'mdr'; DisplayName: 'Mandar'),
    (CultureName: 'men'; DisplayName: 'Mende'),
    {270}(CultureName: 'mga'; DisplayName: 'Irish, Middle (900-1200)'),
    (CultureName: 'mic'; DisplayName: 'Micmac'),
    (CultureName: 'min'; DisplayName: 'Minangkabau'),
    (CultureName: 'mis'; DisplayName: 'Miscellaneous languages'),
    (CultureName: 'mkd'; DisplayName: 'Macedonian'), // Also 'mac'
    (CultureName: 'mkh'; DisplayName: 'Mon-Khmer (Other)'),
    (CultureName: 'mlg'; DisplayName: 'Malagasy'),
    (CultureName: 'mlt'; DisplayName: 'Maltese'),
    (CultureName: 'mnc'; DisplayName: 'Manchu'),
    (CultureName: 'mni'; DisplayName: 'Manipuri'),
    {280}(CultureName: 'mno'; DisplayName: 'Manobo languages'),
    (CultureName: 'moh'; DisplayName: 'Mohawk'),
    (CultureName: 'mol'; DisplayName: 'Moldavian'),
    (CultureName: 'mon'; DisplayName: 'Mongolian'),
    (CultureName: 'mos'; DisplayName: 'Mossi'),
    (CultureName: 'mri'; DisplayName: 'Maori'), // Also 'mao'
    (CultureName: 'msa'; DisplayName: 'Malay'), // Also 'may'
    (CultureName: 'mul'; DisplayName: 'Multiple languages'),
    (CultureName: 'mun'; DisplayName: 'Munda languages'),
    (CultureName: 'mus'; DisplayName: 'Creek'),
    {290}(CultureName: 'mwr'; DisplayName: 'Marwari'),
    (CultureName: 'mya'; DisplayName: 'Burmese'), // Also 'bur'
    (CultureName: 'myn'; DisplayName: 'Mayan languages'),
    (CultureName: 'nah'; DisplayName: 'Nahuatl'),
    (CultureName: 'nai'; DisplayName: 'North American Indian (Other)'),
    (CultureName: 'nap'; DisplayName: 'Neapolitan'),
    (CultureName: 'nau'; DisplayName: 'Nauru'),
    (CultureName: 'nav'; DisplayName: 'Navajo; Navaho'),
    (CultureName: 'nbl'; DisplayName: 'Ndebele, South'),
    (CultureName: 'nde'; DisplayName: 'Ndebele, North'),
    {300}(CultureName: 'ndo'; DisplayName: 'Ndonga'),
    (CultureName: 'nds'; DisplayName: 'German, Low'),
    (CultureName: 'nep'; DisplayName: 'Nepali'),
    (CultureName: 'new'; DisplayName: 'Newari'),
    (CultureName: 'nia'; DisplayName: 'Nias'),
    (CultureName: 'nic'; DisplayName: 'Niger-Kordofanian (Other)'),
    (CultureName: 'niu'; DisplayName: 'Niuean'),
    (CultureName: 'nld'; DisplayName: 'Dutch'), // Also 'dut'
    (CultureName: 'nno'; DisplayName: 'Nynorsk, Norwegian'),
    (CultureName: 'nob'; DisplayName: 'Bokmål, Norwegian'),
    {310}(CultureName: 'non'; DisplayName: 'Norse, Old'),
    (CultureName: 'nor'; DisplayName: 'Norwegian'),
    (CultureName: 'nso'; DisplayName: 'Sotho, Northern'),
    (CultureName: 'nub'; DisplayName: 'Nubian languages'),
    (CultureName: 'nya'; DisplayName: 'Chichewa'),
    (CultureName: 'nym'; DisplayName: 'Nyamwezi'),
    (CultureName: 'nyn'; DisplayName: 'Nyankole'),
    (CultureName: 'nyo'; DisplayName: 'Nyoro'),
    (CultureName: 'nzi'; DisplayName: 'Nzima'),
    (CultureName: 'oci'; DisplayName: 'Occitan (post 1500); Provençal'),
    {320}(CultureName: 'oji'; DisplayName: 'Ojibwa'),
    (CultureName: 'ori'; DisplayName: 'Oriya'),
    (CultureName: 'orm'; DisplayName: 'Oromo'),
    (CultureName: 'osa'; DisplayName: 'Osage'),
    (CultureName: 'oss'; DisplayName: 'Ossetian'),
    (CultureName: 'ota'; DisplayName: 'Turkish, Ottoman (1500-1928)'),
    (CultureName: 'oto'; DisplayName: 'Otomian languages'),
    (CultureName: 'paa'; DisplayName: 'Papuan (Other)'),
    (CultureName: 'pag'; DisplayName: 'Pangasinan'),
    (CultureName: 'pal'; DisplayName: 'Pahlavi'),
    {330}(CultureName: 'pam'; DisplayName: 'Pampanga'),
    (CultureName: 'pan'; DisplayName: 'Panjabi'),
    (CultureName: 'pap'; DisplayName: 'Papiamento'),
    (CultureName: 'pau'; DisplayName: 'Palauan'),
    (CultureName: 'peo'; DisplayName: 'Persian, Old (ca.600-400)'),
    (CultureName: 'per'; DisplayName: 'Persian'), // Also 'fas'
    (CultureName: 'phi'; DisplayName: 'Philippine (Other)'),
    (CultureName: 'phn'; DisplayName: 'Phoenician'),
    (CultureName: 'pli'; DisplayName: 'Pali'),
    (CultureName: 'pol'; DisplayName: 'Polish'),
    {340}(CultureName: 'pon'; DisplayName: 'Pohnpeian'),
    (CultureName: 'por'; DisplayName: 'Portuguese'),
    (CultureName: 'pra'; DisplayName: 'Prakrit languages'),
    (CultureName: 'pro'; DisplayName: 'Provençal, Old (to 1500)'),
    (CultureName: 'pus'; DisplayName: 'Pushto'),
    (CultureName: 'qtz'; DisplayName: 'Reserved for local user; qaa'),
    (CultureName: 'que'; DisplayName: 'Quechua'),
    (CultureName: 'raj'; DisplayName: 'Rajasthani'),
    (CultureName: 'rap'; DisplayName: 'Rapanui'),
    (CultureName: 'rar'; DisplayName: 'Rarotongan'),
    {350}(CultureName: 'roa'; DisplayName: 'Romance (Other)'),
    (CultureName: 'roh'; DisplayName: 'Raeto-Romance'),
    (CultureName: 'rom'; DisplayName: 'Romany'),
    (CultureName: 'ron'; DisplayName: 'Romanian'), // Also 'rum'
    (CultureName: 'rum'; DisplayName: 'Romanian'), // Also 'ron'
    (CultureName: 'run'; DisplayName: 'Rundi'),
    (CultureName: 'rus'; DisplayName: 'Russian'),
    (CultureName: 'sad'; DisplayName: 'Sandawe'),
    (CultureName: 'sag'; DisplayName: 'Sango'),
    (CultureName: 'sah'; DisplayName: 'Yakut'),
    {360}(CultureName: 'sai'; DisplayName: 'South American Indian (Other)'),
    (CultureName: 'sal'; DisplayName: 'Salishan languages'),
    (CultureName: 'sam'; DisplayName: 'Samaritan Aramaic'),
    (CultureName: 'san'; DisplayName: 'Sanskrit'),
    (CultureName: 'sas'; DisplayName: 'Sasak'),
    (CultureName: 'sat'; DisplayName: 'Santali'),
    (CultureName: 'scc'; DisplayName: 'Serbian'), // Also 'srp'
    (CultureName: 'sco'; DisplayName: 'Scots'),
    (CultureName: 'scr'; DisplayName: 'Croatian'), // Also 'hrv'
    (CultureName: 'sel'; DisplayName: 'Selkup'),
    {370}(CultureName: 'sem'; DisplayName: 'Semitic (Other)'),
    (CultureName: 'sga'; DisplayName: 'Irish, Old (to 900)'),
    (CultureName: 'sgn'; DisplayName: 'Sign languages'),
    (CultureName: 'shn'; DisplayName: 'Shan'),
    (CultureName: 'sid'; DisplayName: 'Sidamo'),
    (CultureName: 'sin'; DisplayName: 'Sinhalese'),
    (CultureName: 'sio'; DisplayName: 'Siouan languages'),
    (CultureName: 'sit'; DisplayName: 'Sino-Tibetan (Other)'),
    (CultureName: 'sla'; DisplayName: 'Slavic (Other)'),
    (CultureName: 'slk'; DisplayName: 'Slovak'), // Also 'slo'
    {380}(CultureName: 'slo'; DisplayName: 'Slovak'), // Also 'slk'
    (CultureName: 'slv'; DisplayName: 'Slovenian'),
    (CultureName: 'sma'; DisplayName: 'Southern Sami'),
    (CultureName: 'sme'; DisplayName: 'Northern Sami'),
    (CultureName: 'smi'; DisplayName: 'Sami languages (Other)'),
    (CultureName: 'smj'; DisplayName: 'Lule Sami'),
    (CultureName: 'smn'; DisplayName: 'Inari Sami'),
    (CultureName: 'smo'; DisplayName: 'Samoan'),
    (CultureName: 'sms'; DisplayName: 'Skolt Sami'),
    (CultureName: 'sna'; DisplayName: 'Shona'),
    {390}(CultureName: 'snd'; DisplayName: 'Sindhi'),
    (CultureName: 'snk'; DisplayName: 'Soninke'),
    (CultureName: 'sog'; DisplayName: 'Sogdian'),
    (CultureName: 'som'; DisplayName: 'Somali'),
    (CultureName: 'son'; DisplayName: 'Songhai'),
    (CultureName: 'sot'; DisplayName: 'Sotho, Southern'),
    (CultureName: 'spa'; DisplayName: 'Spanish'),
    (CultureName: 'sqi'; DisplayName: 'Albanian'), // Also 'alb'
    (CultureName: 'srd'; DisplayName: 'Sardinian'),
    (CultureName: 'srp'; DisplayName: 'Serbian'), // Also 'scc'
    {400}(CultureName: 'srr'; DisplayName: 'Serer'),
    (CultureName: 'ssa'; DisplayName: 'Nilo-Saharan (Other)'),
    (CultureName: 'ssw'; DisplayName: 'Swati'),
    (CultureName: 'suk'; DisplayName: 'Sukuma'),
    (CultureName: 'sun'; DisplayName: 'Sundanese'),
    (CultureName: 'sus'; DisplayName: 'Susu'),
    (CultureName: 'sux'; DisplayName: 'Sumerian'),
    (CultureName: 'swa'; DisplayName: 'Swahili'),
    (CultureName: 'swe'; DisplayName: 'Swedish'),
    (CultureName: 'syr'; DisplayName: 'Syriac'),
    {410}(CultureName: 'tah'; DisplayName: 'Tahitian'),
    (CultureName: 'tai'; DisplayName: 'Tai (Other)'),
    (CultureName: 'tam'; DisplayName: 'Tamil'),
    (CultureName: 'tat'; DisplayName: 'Tatar'),
    (CultureName: 'tel'; DisplayName: 'Telugu'),
    (CultureName: 'tem'; DisplayName: 'Timne'),
    (CultureName: 'ter'; DisplayName: 'Tereno'),
    (CultureName: 'tet'; DisplayName: 'Tetum'),
    (CultureName: 'tgk'; DisplayName: 'Tajik'),
    (CultureName: 'tgl'; DisplayName: 'Tagalog'),
    {420}(CultureName: 'tha'; DisplayName: 'Thai'),
    (CultureName: 'tib'; DisplayName: 'Tibetan'), // Also 'bod'
    (CultureName: 'tig'; DisplayName: 'Tigre'),
    (CultureName: 'tir'; DisplayName: 'Tigrinya'),
    (CultureName: 'tiv'; DisplayName: 'Tiv'),
    (CultureName: 'tkl'; DisplayName: 'Tokelau'),
    (CultureName: 'tli'; DisplayName: 'Tlingit'),
    (CultureName: 'tmh'; DisplayName: 'Tamashek'),
    (CultureName: 'tog'; DisplayName: 'Tonga (Nyasa)'),
    (CultureName: 'ton'; DisplayName: 'Tonga (Tonga Islands)'),
    {430}(CultureName: 'tpi'; DisplayName: 'Tok Pisin'),
    (CultureName: 'tsi'; DisplayName: 'Tsimshian'),
    (CultureName: 'tsn'; DisplayName: 'Tswana'),
    (CultureName: 'tso'; DisplayName: 'Tsonga'),
    (CultureName: 'tuk'; DisplayName: 'Turkmen'),
    (CultureName: 'tum'; DisplayName: 'Tumbuka'),
    (CultureName: 'tup'; DisplayName: 'Tupi languages'),
    (CultureName: 'tur'; DisplayName: 'Turkish'),
    (CultureName: 'tut'; DisplayName: 'Altaic (Other)'),
    (CultureName: 'tvl'; DisplayName: 'Tuvalu'),
    {440}(CultureName: 'twi'; DisplayName: 'Twi'),
    (CultureName: 'tyv'; DisplayName: 'Tuvinian'),
    (CultureName: 'uga'; DisplayName: 'Ugaritic'),
    (CultureName: 'uig'; DisplayName: 'Uighur'),
    (CultureName: 'ukr'; DisplayName: 'Ukrainian'),
    (CultureName: 'umb'; DisplayName: 'Umbundu'),
    (CultureName: 'und'; DisplayName: 'Undetermined'),
    (CultureName: 'urd'; DisplayName: 'Urdu'),
    (CultureName: 'uzb'; DisplayName: 'Uzbek'),
    (CultureName: 'vai'; DisplayName: 'Vai'),
    {450}(CultureName: 'ven'; DisplayName: 'Venda'),
    (CultureName: 'vie'; DisplayName: 'Vietnamese'),
    (CultureName: 'vol'; DisplayName: 'Volapük'),
    (CultureName: 'vot'; DisplayName: 'Votic'),
    (CultureName: 'wak'; DisplayName: 'Wakashan languages'),
    (CultureName: 'wal'; DisplayName: 'Walamo'),
    (CultureName: 'war'; DisplayName: 'Waray'),
    (CultureName: 'was'; DisplayName: 'Washo'),
    (CultureName: 'wel'; DisplayName: 'Welsh'), // Also 'cym'
    (CultureName: 'wen'; DisplayName: 'Sorbian languages'),
    {460}(CultureName: 'wln'; DisplayName: 'Walloon'),
    (CultureName: 'wol'; DisplayName: 'Wolof'),
    (CultureName: 'xho'; DisplayName: 'Xhosa'),
    (CultureName: 'yao'; DisplayName: 'Yao'),
    (CultureName: 'yap'; DisplayName: 'Yapese'),
    (CultureName: 'yid'; DisplayName: 'Yiddish'),
    (CultureName: 'yor'; DisplayName: 'Yoruba'),
    (CultureName: 'ypk'; DisplayName: 'Yupik languages'),
    (CultureName: 'zap'; DisplayName: 'Zapotec'),
    (CultureName: 'zen'; DisplayName: 'Zenaga'),
    {470}(CultureName: 'zha'; DisplayName: 'Zhuang; Chuang'),
    (CultureName: 'zho'; DisplayName: 'Chinese'), // Also 'chi'
    (CultureName: 'znd'; DisplayName: 'Zande'),
    (CultureName: 'zul'; DisplayName: 'Zulu'),
    (CultureName: 'zun'; DisplayName: 'Zuni'),
    (CultureName: 'zza'; DisplayName: 'Zaza')
    );

// -----------------------------------------------------------------------------

function GetCultureDisplayName(const CultureName: AnsiString): String;
var
  i: Integer;
begin
  Result := '';

  for i := 0 to MaxCultureItems-1 do
    if AnsiLowerCase(CultureInfo[i].CultureName) = AnsiLowerCase(CultureName) then
    begin
      Result := CultureInfo[i].DisplayName;
      Break;
    end;
end;

// -----------------------------------------------------------------------------

function GetCultureName(const Index: Integer): String;
begin
  if InRange(Index, 0, MaxCultureItems-1) then
    Result := CultureInfo[Index].CultureName
  else
    Result := '';
end;

// -----------------------------------------------------------------------------

function GetCultureName(const ADisplayName: String): String;
var
  i: Integer;
begin
  Result := '';

  for i := 0 to MaxCultureItems-1 do
    if AnsiLowerCase(CultureInfo[i].DisplayName) = AnsiLowerCase(ADisplayName) then
    begin
      Result := CultureInfo[i].CultureName;
      Break;
    end;
end;

// -----------------------------------------------------------------------------

function GetCultureShortName(const Index: Integer): String;
begin
  Result := GetCultureName(Index);
  if not Result.IsEmpty then
    Result := Copy(Result, 1, Pos('-', Result)-1);
end;

// -----------------------------------------------------------------------------

procedure FillCultureTStrings(const Items: TStrings);
var
  i: Integer;
begin
  Items.BeginUpdate;
  try
    for i := 0 to High(CultureInfo)-1 do
      Items.Add(CultureInfo[i].DisplayName);
  finally
    Items.EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

function ISO_639_2CodeToName(const Code: AnsiString): AnsiString;
var
  i: Integer;
begin
  Result := '';

  if Length(Code) <> 3 then Exit;

  for i := 0 to High(CISO_639_2Info)-1 do
    if AnsiLowerCase(CISO_639_2Info[i].CultureName) = AnsiLowerCase(Code) then
    begin
      Result := CISO_639_2Info[i].DisplayName;
      Break;
    end;
end;

// -----------------------------------------------------------------------------

function ISO_639_2NameToCode(const Name: String): AnsiString;
var
  i: Integer;
begin
  Result := '';

  if Name = '' then Exit;

  for i := 0 to High(CISO_639_2Info)-1 do
    if AnsiLowerCase(CISO_639_2Info[i].DisplayName) = AnsiLowerCase(Name) then
    begin
      Result := CISO_639_2Info[i].CultureName;
      Break;
    end;
end;

// -----------------------------------------------------------------------------

function ISO_639_2CodeFromIndex(const idx: Integer): AnsiString;
begin
  Result := '';

  if (idx < Low(CISO_639_2Info)) or (idx > High(CISO_639_2Info)) then Exit;

  Result := CISO_639_2Info[idx].CultureName;
end;

// -----------------------------------------------------------------------------

procedure ISO_639_2FillTStrings(const Items: TStrings);
var
  i: Integer;
begin
  Items.BeginUpdate;
  try
    for i := 0 to High(CISO_639_2Info)-1 do
      Items.Add(CISO_639_2Info[i].DisplayName);
  finally
    Items.EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

function GetOSLanguage: String;
var
  Lang,            // es_ES
  SecLang: String; // es
begin
  GetLanguageIDs(Lang, SecLang);
  Result := Lang.Replace('_', '-'); // make compatible with our CultureInfo
end;

// -----------------------------------------------------------------------------

end.
