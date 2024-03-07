{*
 *  URUWorks Lazarus Encoding
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

unit UWSystem.Encoding;

// -----------------------------------------------------------------------------

{$C-}

interface

uses
  SysUtils, Classes, UWSystem.StrUtils;

type

  TCPData = record
    CPID   : Integer;
    CPName : String;
  end;

const

  MaxEncodings = 140;

  Encodings: array[0..MaxEncodings-1] of TCPData =
  (
    (CPID: 37; CPName: 'IBM037'),
    (CPID: 437; CPName: 'IBM437'),
    (CPID: 500; CPName: 'IBM500'),
    (CPID: 708; CPName: 'ASMO-708'),
    (CPID: 720; CPName: 'DOS-720'),
    (CPID: 737; CPName: 'ibm737'),
    (CPID: 775; CPName: 'ibm775'),
    (CPID: 850; CPName: 'ibm850'),
    (CPID: 852; CPName: 'ibm852'),
    (CPID: 855; CPName: 'IBM855'),
    (CPID: 857; CPName: 'ibm857'),
    (CPID: 858; CPName: 'IBM00858'),
    (CPID: 860; CPName: 'IBM860'),
    (CPID: 861; CPName: 'ibm861'),
    (CPID: 862; CPName: 'DOS-862'),
    (CPID: 863; CPName: 'IBM863'),
    (CPID: 864; CPName: 'IBM864'),
    (CPID: 865; CPName: 'IBM865'),
    (CPID: 866; CPName: 'cp866'),
    (CPID: 869; CPName: 'ibm869'),
    (CPID: 870; CPName: 'IBM870'),
    (CPID: 874; CPName: 'windows-874'),
    (CPID: 875; CPName: 'cp875'),
    (CPID: 932; CPName: 'shift_jis'),
    (CPID: 936; CPName: 'gb2312'),
    (CPID: 949; CPName: 'ks_c_5601-1987'),
    (CPID: 950; CPName: 'big5'),
    (CPID: 1026; CPName: 'IBM1026'),
    (CPID: 1047; CPName: 'IBM01047'),
    (CPID: 1140; CPName: 'IBM01140'),
    (CPID: 1141; CPName: 'IBM01141'),
    (CPID: 1142; CPName: 'IBM01142'),
    (CPID: 1143; CPName: 'IBM01143'),
    (CPID: 1144; CPName: 'IBM01144'),
    (CPID: 1145; CPName: 'IBM01145'),
    (CPID: 1146; CPName: 'IBM01146'),
    (CPID: 1147; CPName: 'IBM01147'),
    (CPID: 1148; CPName: 'IBM01148'),
    (CPID: 1149; CPName: 'IBM01149'),
    (CPID: 1200; CPName: 'utf-16'),
    (CPID: 1201; CPName: 'unicode'),
    (CPID: 1250; CPName: 'windows-1250'),
    (CPID: 1251; CPName: 'windows-1251'),
    (CPID: 1252; CPName: 'Windows-1252'),
    (CPID: 1253; CPName: 'windows-1253'),
    (CPID: 1254; CPName: 'windows-1254'),
    (CPID: 1255; CPName: 'windows-1255'),
    (CPID: 1256; CPName: 'windows-1256'),
    (CPID: 1257; CPName: 'windows-1257'),
    (CPID: 1258; CPName: 'windows-1258'),
    (CPID: 1361; CPName: 'Johab'),
    (CPID: 10000; CPName: 'macintosh'),
    (CPID: 10001; CPName: 'x-mac-japanese'),
    (CPID: 10002; CPName: 'x-mac-chinesetrad'),
    (CPID: 10003; CPName: 'x-mac-korean'),
    (CPID: 10004; CPName: 'x-mac-arabic'),
    (CPID: 10005; CPName: 'x-mac-hebrew'),
    (CPID: 10006; CPName: 'x-mac-greek'),
    (CPID: 10007; CPName: 'x-mac-cyrillic'),
    (CPID: 10008; CPName: 'x-mac-chinesesimp'),
    (CPID: 10010; CPName: 'x-mac-romanian'),
    (CPID: 10017; CPName: 'x-mac-ukrainian'),
    (CPID: 10021; CPName: 'x-mac-thai'),
    (CPID: 10029; CPName: 'x-mac-ce'),
    (CPID: 10079; CPName: 'x-mac-icelandic'),
    (CPID: 10081; CPName: 'x-mac-turkish'),
    (CPID: 10082; CPName: 'x-mac-croatian'),
    (CPID: 12000; CPName: 'utf-32'),
    (CPID: 12001; CPName: 'utf-32BE'),
    (CPID: 20000; CPName: 'x-Chinese-CNS'),
    (CPID: 20001; CPName: 'x-cp20001'),
    (CPID: 20002; CPName: 'x-Chinese-Eten'),
    (CPID: 20003; CPName: 'x-cp20003'),
    (CPID: 20004; CPName: 'x-cp20004'),
    (CPID: 20005; CPName: 'x-cp20005'),
    (CPID: 20105; CPName: 'x-IA5'),
    (CPID: 20106; CPName: 'x-IA5-German'),
    (CPID: 20107; CPName: 'x-IA5-Swedish'),
    (CPID: 20108; CPName: 'x-IA5-Norwegian'),
    (CPID: 20127; CPName: 'us-ascii'),
    (CPID: 20261; CPName: 'x-cp20261'),
    (CPID: 20269; CPName: 'x-cp20269'),
    (CPID: 20273; CPName: 'IBM273'),
    (CPID: 20277; CPName: 'IBM277'),
    (CPID: 20278; CPName: 'IBM278'),
    (CPID: 20280; CPName: 'IBM280'),
    (CPID: 20284; CPName: 'IBM284'),
    (CPID: 20285; CPName: 'IBM285'),
    (CPID: 20290; CPName: 'IBM290'),
    (CPID: 20297; CPName: 'IBM297'),
    (CPID: 20420; CPName: 'IBM420'),
    (CPID: 20423; CPName: 'IBM423'),
    (CPID: 20424; CPName: 'IBM424'),
    (CPID: 20833; CPName: 'x-EBCDIC-KoreanExtended'),
    (CPID: 20838; CPName: 'IBM-Thai'),
    (CPID: 20866; CPName: 'koi8-r'),
    (CPID: 20871; CPName: 'IBM871'),
    (CPID: 20880; CPName: 'IBM880'),
    (CPID: 20905; CPName: 'IBM905'),
    (CPID: 20924; CPName: 'IBM00924'),
    (CPID: 20932; CPName: 'EUC-JP'),
    (CPID: 20936; CPName: 'x-cp20936'),
    (CPID: 20949; CPName: 'x-cp20949'),
    (CPID: 21025; CPName: 'cp1025'),
    (CPID: 21866; CPName: 'koi8-u'),
    (CPID: 28591; CPName: 'iso-8859-1'),
    (CPID: 28592; CPName: 'iso-8859-2'),
    (CPID: 28593; CPName: 'iso-8859-3'),
    (CPID: 28594; CPName: 'iso-8859-4'),
    (CPID: 28595; CPName: 'iso-8859-5'),
    (CPID: 28596; CPName: 'iso-8859-6'),
    (CPID: 28597; CPName: 'iso-8859-7'),
    (CPID: 28598; CPName: 'iso-8859-8'),
    (CPID: 28599; CPName: 'iso-8859-9'),
    (CPID: 28603; CPName: 'iso-8859-13'),
    (CPID: 28605; CPName: 'iso-8859-15'),
    (CPID: 29001; CPName: 'x-Europa'),
    (CPID: 38598; CPName: 'iso-8859-8-i'),
    (CPID: 50220; CPName: 'iso-2022-jp'),
    (CPID: 50221; CPName: 'csISO2022JP'),
    (CPID: 50222; CPName: 'iso-2022-jp'),
    (CPID: 50225; CPName: 'iso-2022-kr'),
    (CPID: 50227; CPName: 'x-cp50227'),
    (CPID: 51932; CPName: 'euc-jp'),
    (CPID: 51936; CPName: 'EUC-CN'),
    (CPID: 51949; CPName: 'euc-kr'),
    (CPID: 52936; CPName: 'hz-gb-2312'),
    (CPID: 54936; CPName: 'GB18030'),
    (CPID: 57002; CPName: 'x-iscii-de'),
    (CPID: 57003; CPName: 'x-iscii-be'),
    (CPID: 57004; CPName: 'x-iscii-ta'),
    (CPID: 57005; CPName: 'x-iscii-te'),
    (CPID: 57006; CPName: 'x-iscii-as'),
    (CPID: 57007; CPName: 'x-iscii-or'),
    (CPID: 57008; CPName: 'x-iscii-ka'),
    (CPID: 57009; CPName: 'x-iscii-ma'),
    (CPID: 57010; CPName: 'x-iscii-gu'),
    (CPID: 57011; CPName: 'x-iscii-pa'),
    (CPID: 65000; CPName: 'utf-7'),
    (CPID: 65001; CPName: 'utf-8')
  );

function GetEncodingString(const CPID: Integer): String;
function GetEncodingInteger(const CPName: String): Integer;
function GetEncodingIndex(const CPID: Integer): Integer;

function GetEncodingFromFile(const FileName: String; const TryAnsiDetect: Boolean = True): TEncoding;

function IsRightToLeftText(const AText: String): Boolean;

// -----------------------------------------------------------------------------

implementation

uses RegExpr;

const
  _Russian: array of String = ('что', 'быть', 'весь', 'этот', 'один', 'такой', '[Ээч]?то', '[Нн]е', '[ТтМмбв]ы', 'Да', '[Нн]ет', 'Он', 'его', 'тебя', 'как', 'меня', 'Но', 'всё', 'мне', 'вас', 'знаю', 'ещё', 'за', 'нас', 'чтобы', 'был', 'Я', '[Bb]тор.*', 'Держ.*');
  _Bulgarian: array of String = ('Какво', 'тук', 'може', 'Как', 'Ваше', 'какво');
  _SerbianCyrillic: array of String = ('сам', 'али', 'није', 'само', 'ово', 'како', 'добро', 'све', 'тако', 'ће', 'могу', 'ћу', 'зашто', 'нешто', 'за', 'шта', 'овде', 'бити', 'чини', 'учениче', 'побегне', 'остати', 'Један', 'Назад', 'Молим', 'ће', 'ћемо', 'Хоћу');
  _Greek: array of String = ('μου', '[Εε]ίναι', 'αυτό', 'Τόμπυ', 'καλά', 'Ενταξει', 'πρεπει', 'Λοιπον', 'τιποτα', 'ξερεις');
  _CroatianAndSerbian: array of String = ('sam', 'ali', 'nije', 'Nije', 'samo', 'ovo', 'kako', 'dobro', 'Dobro', 'sve', 'tako', 'će', 'mogu', 'ću', 'zašto', 'nešto', 'za', 'misliš', 'možeš', 'možemo', 'ništa', 'znaš', 'ćemo', 'znam');
  _CzechAndSlovak: array of String = ('[Oo]n[ao]?', '[Jj]?si', '[Aa]le', '[Tt]en(to)?', '[Rr]ok', '[Tt]ak', '[Aa]by', '[Tt]am', '[Jj]ed(en|na|no)', '[Nn]ež', '[Aa]ni', '[Bb]ez', '[Dd]obr[ýáé]', '[Vv]šak', '[Cc]el[ýáé]', '[Nn]ov[ýáé]', '[Dd]ruh[ýáé]', 'jsem', 'poøádku', 'Pojïme', 'háje', 'není', 'Jdeme', 'všecko', 'jsme', 'Prosím', 'Vezmi', 'když', 'Takže', 'Dìkuji', 'prechádzku', 'všetko', 'Poïme', 'potom', 'Takže', 'Neviem', 'budúcnosti', 'trochu');
  _Czech: array of String = ('.*[Řř].*', '.*[ůě].*', '[Bb]ýt', '[Jj]sem', '[Jj]si', '[Jj]á', '[Mm]ít', '[Aa]no', '[Nn]e',  '[Nn]ic', '[Dd]en', '[Jj]en', '[Cc]o', '[Jj]ak[o]?', '[Nn]ebo',  '[Pp]ři', '[Pp]ro', '[Pp]řed.*', '[Jj](ít|du|de|deme|dou)', '[Mm]ezi',  '[Jj]eště', '[Čč]lověk', '[Pp]odle', '[Dd]alší');
  _Polish: array of String = ('Czy', 'ale', 'ty', 'siê', 'się', 'jest', 'mnie', 'Proszę', 'życie', 'statku', 'życia', 'Czyli', 'Wszystko', 'Wiem', 'Przepraszam', 'dobrze', 'chciałam', 'Dziękuję', 'Żołnierzyk', 'Łowca', 'został', 'stało', 'dolarów', 'wiadomości', 'Dobrze', 'będzie', 'Dzień', 'przyszłość', 'Uratowałaś', 'Cześć', 'Trzeba', 'zginąć', 'walczyć', 'ludzkość', 'maszyny', 'Jeszcze', 'okrążenie', 'wyścigu', 'porządku', 'detektywie', 'przebieralni', 'który');
  _Hungarian: array of String = ('hogy', 'lesz', 'tudom', 'vagy', 'mondtam', 'még', 'vagyok', 'csak', 'Hát', 'felesége', 'Csak', 'utána', 'jött', 'Miért', 'Akkor', 'magát', 'holnap', 'Tudja', 'Köszönöm', 'élet', 'Örvendek', 'vissza', 'hogy', 'tudom', 'Rendben', 'Istenem', 'Gyerünk', 'értem', 'vagyok', 'hiszem', 'történt', 'rendben', 'olyan', 'őket', 'vannak', 'mindig', 'Kérlek', 'Gyere', 'kicsim', 'vagyunk');
  _Latvian: array of String = ('labrīt', 'labdien', 'sveiki', 'labvakar!', 'atā', 'redzēšanos', 'nē', 'jā', 'varbūt', 'labi', 'paldies', 'lūdzu', 'atvainojiet', 'žēl', 'kā', 'jums', 'klājas', 'dzīvo', 'kopā', 'runājat');
  _Arabic: array of String = ('من', 'هل', 'لا', 'في', 'لقد', 'ما', 'ماذا', 'يا', 'هذا', 'إلى', 'على', 'أنا', 'أنت', 'حسناً', 'أيها', 'كان', 'كيف', 'يكون', 'هذه', 'هذان', 'الذي', 'التي', 'الذين', 'هناك', 'هنالك');
  _Hebrew: array of String = ('אתה', 'אולי', 'הוא', 'בסדר', 'יודע', 'טוב');
  {_Hebrew: array of String = ('אתה', 'אולי', 'הוא', 'בסדר', 'יודע', 'טוב', 'אֶת', 'שֶׁל', 'עַל', 'הוּא',
  'אֲשֶׁר', 'הִיא', 'הַמְלֶט', 'נָסִיךְ', 'הֲלֹא', 'עוֹד', 'אֵין', 'אֲנִי', 'זֹאת', 'וְלֹא',
  'כָּךְ', 'הִנֵּה', 'מְאֹד', 'אֲדוֹנִי');}
  _Farsi: array of String = ('این', 'برای', 'اون', 'سیب', 'کال', 'رو', 'خيلي', 'آره', 'بود', 'اون', 'نيست', 'اينجا', 'باشه', 'سلام', 'ميکني', 'داري', 'چيزي', 'چرا', 'خوبه');

// -----------------------------------------------------------------------------

function GetEncodingString(const CPID: Integer): String;
var
  I: Integer;
begin
  Result := 'iso-8859-1'; //default encoding

  for I := 0 to MaxEncodings - 1 do
    if Encodings[I].CPID = CPID then
    begin
      Result := Encodings[I].CPName;
      Break;
    end;
end;

// -----------------------------------------------------------------------------

function GetEncodingInteger(const CPName: String): Integer;
var
  I: Integer;
begin
  Result := 28591; //default encoding

  for I := 0 to MaxEncodings - 1 do
    if LowerCase(Encodings[I].CPName) = LowerCase(CPName) then
    begin
      Result := Encodings[I].CPID;
      Break;
    end;
end;

// -----------------------------------------------------------------------------

function GetEncodingIndex(const CPID: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;

  for I := 0 to MaxEncodings - 1 do
    if Encodings[I].CPID = CPID then
    begin
      Result := I;
      Break;
    end;
end;

// -----------------------------------------------------------------------------

function IsUTF8(const Buffer: TBytes; out CouldBeUTF8: Boolean): Boolean;
var
  utf8count, i: Integer;
  b: Byte;
begin
  Result := False;
  CouldBeUTF8 := False;
  utf8count := 0;
  i := 0;
  while (i < Length(Buffer) - 3) do
  begin
    b := Buffer[i];
    if b > 127 then
    begin
      if (b >= 194) and (b <= 223) and (Buffer[i+1] >= 128) and (Buffer[i+1] <= 191) then
      begin
        // 2-byte sequence
        Inc(utf8count);
        Inc(i);
      end
      else if (b >= 224) and (b <= 239) and (Buffer[i+1] >= 128) and (Buffer[i+1] <= 191) and (Buffer[i+2] >= 128) and (Buffer[i+2] <= 191) then
      begin
        // 3-byte sequence
        Inc(utf8count);
        Inc(i, 2);
      end
      else if (b >= 240) and (b <= 244) and (Buffer[i+1] >= 128) and (Buffer[i+1] <= 191) and (Buffer[i+2] >= 128) and (Buffer[i+2] <= 191) and (Buffer[i+3] >= 128) and (Buffer[i+3] <= 191) then
      begin
        // 4-byte sequence
        Inc(utf8count);
        Inc(i, 3);
      end
      else
      begin
        Exit;
      end;
    end;
    Inc(i);
  end;
  CouldBeUTF8 := True;
  Result := (utf8count > 0);
end;

// -----------------------------------------------------------------------------

function IsUTF16_NewLineChars(const Buffer: TBytes; out ShouldBeUTF16: Boolean): Boolean;
var
  le_control_chars, be_control_chars, size, i : Integer;
  ch1, ch2 : Byte;
begin
  Result := False;
  ShouldBeUTF16 := False;

  le_control_chars := 0;
  be_control_chars := 0;
  size := Length(Buffer)-1;
  if size < 2 then Exit;

  i := 0;
  while (i < size) do
  begin
    ch1 := Buffer[i];
    Inc(i);
    ch2 := Buffer[i];
    Inc(i);

    if ch1 = 0 then
    begin
      if (ch2 = $0A) or (ch2 = $0D) then
        Inc(be_control_chars);
    end
    else if ch2 = 0 then
    begin
      if (ch1 = $0A) or (ch1 = $0D) then
        Inc(le_control_chars);
    end;

    // If we are getting both LE and BE control chars then this file is not utf16
    if (le_control_chars > 0) and (be_control_chars > 0) then
      Exit;
  end;

  if (le_control_chars > 0) then
  begin
    ShouldBeUTF16 := True;
    Exit(True); //UTF16 LE
  end
  else if (be_control_chars > 0) then
    ShouldBeUTF16 := True; //UTF16 BE
end;

// -----------------------------------------------------------------------------

function IsUTF16_ASCII(const Buffer: TBytes; out ShouldBeUTF16: Boolean): Boolean;
const
  utf16_expected_null_percent_   = 70;
  utf16_unexpected_null_percent_ = 10;

var
  num_odd_nulls, num_even_nulls, size, i : Integer;
  even_null_threshold, odd_null_threshold,
  expected_null_threshold, unexpected_null_threshold : Double;
begin
  Result := False;
  ShouldBeUTF16 := False;

  num_odd_nulls := 0;
  num_even_nulls := 0;
  size := Length(Buffer);

  // Get even nulls
  i := 0;
  while (i < size) do
  begin
    if Buffer[i] = 0 then
      Inc(num_even_nulls);

    Inc(i, 2);
  end;

  // Get odd nulls
  i := 1;
  while (i < size) do
  begin
    if Buffer[i] = 0 then
      Inc(num_odd_nulls);

    Inc(i, 2);
  end;

  even_null_threshold       := (num_even_nulls * 2.0) / size;
  odd_null_threshold        := (num_odd_nulls * 2.0) / size;
  expected_null_threshold   := utf16_expected_null_percent_ / 100.0;
  unexpected_null_threshold := utf16_unexpected_null_percent_ / 100.0;

  // Lots of odd nulls, low number of even nulls
  if (even_null_threshold < unexpected_null_threshold) and (odd_null_threshold > expected_null_threshold) then
  begin
    ShouldBeUTF16 := True;
    Exit(True); //UTF16 LE;
  end;

  // Lots of even nulls, low number of odd nulls
  if (odd_null_threshold < unexpected_null_threshold) and (even_null_threshold > expected_null_threshold) then
    ShouldBeUTF16 := True; //UTF16 BE;
end;

// -----------------------------------------------------------------------------

function ContainNulls(const Buffer: TBytes): Boolean; // True = Binary (assume nulls can't appear in ANSI/ASCII/UTF8 text files)
var
  size, i : Integer;
begin
  Result := False;
  size := Length(Buffer);

  i := 0;
  while (i < size) do
  begin
    if Buffer[i] = 0 then
      Exit(True);

    Inc(i);
  end;
end;

// -----------------------------------------------------------------------------

function GetStringCount(const ASourceString: String; const AWords: array of String): Integer;
begin
  Result := RE_StringCount('\b(' + String.Join('|', AWords) + ')\b', ASourceString);
  if Result = 0 then // I don't know why regexpr sometimes gives me 0 when it shouldnt
    Result := StringsCount(ASourceString, AWords);
end;

// -----------------------------------------------------------------------------

function DetectAnsiEncoding(const Buffer: TBytes): TEncoding;
var
  Encoding, Enc : TEncoding;
  Text : String;
  MinCount : Integer;
begin
  Result := NIL;
  MinCount := Length(Buffer) div 300;

  // Cyrillic
  Encoding := TEncoding.GetEncoding(1251);
  Text := Encoding.GetString(Buffer);
  if (GetStringCount(Text, _Russian) > 5) or
     (GetStringCount(Text, _Bulgarian) > 5) or
     (GetStringCount(Text, _SerbianCyrillic) > MinCount) then
  begin
    Exit(Encoding);
  end;

  // Greek
  Encoding := TEncoding.GetEncoding(1253);
  Text := Encoding.GetString(Buffer);
  if (GetStringCount(Text, _Greek) > 5) then
  begin
    Exit(Encoding);
  end;

  // Latvian
  Encoding := TEncoding.GetEncoding(1257);
  Text := Encoding.GetString(Buffer);
  if (GetStringCount(Text, _Latvian) > 5) then
  begin
    Exit(Encoding);
  end;

  // Central European/Eastern European
  Encoding := TEncoding.GetEncoding(1250);
  Text := Encoding.GetString(Buffer);
  if (GetStringCount(Text, _CroatianAndSerbian) > MinCount) or
     (GetStringCount(Text, _CzechAndSlovak) > MinCount) or
     //(GetStringCount(Text, _Czech) > MinCount) or
     (GetStringCount(Text, _Polish) > MinCount) or
     (GetStringCount(Text, _Hungarian) > MinCount) or
     (GetStringCount(Text, ['să', 'şi', 'văzut', 'regulă', 'găsit', 'viaţă']) > 99) then
  begin
    Exit(Encoding);
  end;

  // Arabic
  Encoding := TEncoding.GetEncoding(1256);
  // Hebrew
  Enc := TEncoding.GetEncoding(1255);
  Text := Encoding.GetString(Buffer);
  if (GetStringCount(Text, _Arabic) > 5) then
  begin
    // Hebrew
    if (GetStringCount(Enc.GetString(Buffer), _Hebrew) > 10) then
    begin
      Exit(Enc);
    end;
    Exit(Encoding);
  end;
  // Hebrew
  if (GetStringCount(Enc.GetString(Buffer), _Hebrew) > 5) then
  begin
    Exit(Enc);
  end;
end;

// -----------------------------------------------------------------------------

function GetEncodingFromFile(const FileName: String; const TryAnsiDetect: Boolean = True): TEncoding;
var
  Stream     : TStream;
  Size       : Integer;
  BOM        : TBytes;
  CouldBeUTF : Boolean;
begin
  Result := NIL;
  if not FileExists(FileName) then Exit;

  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(BOM, 12);
    Size := Stream.Size - Stream.Position;
    Stream.Position := 0;
    Stream.Read(BOM[0], Length(BOM));

    if (BOM[0] = $EF) and (BOM[1] = $BB) and (BOM[2] = $BF) then // UTF-8
       Result := TEncoding.UTF8
    else if (BOM[0] = $FF) and (BOM[1] = $FE) and (BOM[2] = 0) and (BOM[3] = 0) then // UTF-32 (LE)
       Result := TEncoding.GetEncoding(12000)
    else if (BOM[0] = $FF) and (BOM[1] = $FE) then // UTF-16 (LE)
       Result := TEncoding.Unicode
    else if (BOM[0] = $FE) and (BOM[1] = $FF) then // UTF-16 (BE)
       Result := TEncoding.BigEndianUnicode
    else if (BOM[0] = 0) and (BOM[1] = 0) and (BOM[2] = $FE) and (BOM[3] = $FF) then // UTF-32 (BE)
       Result := TEncoding.GetEncoding(12001)
    else if (BOM[0] = $2B) and (BOM[1] = $2F) and (BOM[2] = $76) and ((BOM[3] = $38) or (BOM[3] = $39) or (BOM[3] = $2B) or (BOM[3] = $2F)) then // UTF-7
       Result := TEncoding.UTF7
    else if Size > Length(BOM) then
    begin
      if Size > 500000 then Size := 500000;
      Stream.Position := 0;
      SetLength(BOM, Size);
      Stream.Read(BOM[0], Size);

      if IsUTF8(BOM, CouldBeUTF) then
        Result := TEncoding.UTF8
      else if CouldBeUTF then
        Result := TEncoding.UTF8
      else if IsUTF16_NewLineChars(BOM, CouldBeUTF) then
        Result := TEncoding.Unicode
      else if CouldBeUTF then
        Result := TEncoding.BigEndianUnicode
      else if IsUTF16_ASCII(BOM, CouldBeUTF) then
        Result := TEncoding.Unicode
      else if CouldBeUTF then
        Result := TEncoding.BigEndianUnicode
      else if TryAnsiDetect then
        Result := DetectAnsiEncoding(BOM);
    end;
  finally
    Stream.Free;
    SetLength(BOM, 0);
  end;
end;

// -----------------------------------------------------------------------------

function IsRightToLeftText(const AText: String): Boolean;
begin
  Result := (GetStringCount(AText, _Arabic) > 0) or
    (GetStringCount(AText, _Hebrew) > 0) or
    (GetStringCount(AText, _Farsi) > 0);
end;

// -----------------------------------------------------------------------------

end.
