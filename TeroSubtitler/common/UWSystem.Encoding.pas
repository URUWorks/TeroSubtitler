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
 *  Copyright (C) 2001-2026 URUWorks, uruworks@gmail.com.
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

  TAnsiProfileData = record
    CP   : Integer;
    Data : String; // letter + weight (frequency * 100), UTF-8
  end;

  TAnsiProfile = record
    CP      : Integer;
    Letters : UnicodeString;
    Probs   : array of Double;
  end;

  TAnsiTable = array[$80..$FF] of UnicodeChar;

  TAnsiTableCache = record
    CP    : Integer;
    Table : TAnsiTable;
  end;

const
  AnsiCandidates: array[0..7] of Integer = (1252, 1250, 1251, 1253, 1254, 1255, 1256, 1257);

  AnsiProfiles: array[0..29] of TAnsiProfileData = (
    (CP: 1252; Data: 'á50é43í73ó83ú17ñ31ü2'), // es
    (CP: 1252; Data: 'é190è27ê22à49â5ç9ù6û6î5ô2ë1ï1œ2'), // fr
    (CP: 1252; Data: 'à60è100é5ì3ò10ù17'), // it
    (CP: 1252; Data: 'á12à7â15ã73ç53é34ê45í13ó30ô10õ4ú21'), // pt
    (CP: 1252; Data: 'ä58ö44ü99ß31'), // de
    (CP: 1252; Data: 'å134ä180ö131é2'), // sv
    (CP: 1252; Data: 'æ87ø94å119é2'), // da
    (CP: 1252; Data: 'ä360ö44'), // fi
    (CP: 1252; Data: 'à50è40é40í30ò30ó30ú10ç20ï10'), // ca
    (CP: 1252; Data: 'á180ð440é60í150ó100ú60ý90þ150æ90ö80'), // is
    (CP: 1252; Data: 'ä100õ100ö20ü90'), // et
    (CP: 1250; Data: 'á87č46ď2é63ě122í164ň1ó2ř38š69ť1ú5ů20ý99ž72'), // cs
    (CP: 1250; Data: 'á200ä10č90ď10é70í140ĺ1ľ30ň10ó20ô20š70ť30ú70ý140ž80'), // sk
    (CP: 1250; Data: 'ą99ć40ę111ł182ń20ó85ś68ź6ż83'), // pl
    (CP: 1250; Data: 'á340é390í50ó100ö100ő90ú30ü50ű10'), // hu
    (CP: 1250; Data: 'č100ć100đ20š100ž70'), // hr
    (CP: 1250; Data: 'č150š100ž70'), // sl
    (CP: 1250; Data: 'ă400â100î100ş140ţ100'), // ro
    (CP: 1254; Data: 'ç146ğ113ı511ö78ş178ü185â5'), // tr
    (CP: 1257; Data: 'ā400č50ē200ģ10ī170ķ20ļ40ņ50š100ū50ž50'), // lv
    (CP: 1257; Data: 'ą60č40ę20ė170į100š100ų70ū50ž70'), // lt
    (CP: 1257; Data: 'ä100õ100ö20ü90'), // et
    (CP: 1251; Data: 'о1100е850а800и740н670т630с550р470в450л440к350м320д300п280у260я200ы190ь170г170з160б160ч140й120х97ж94ш73ю64ц48щ36э32ф26ъ4ё4'), // ru
    (CP: 1251; Data: 'а1000о900е800и800н700т700р500с450в450л330к350д300п300м260з160у100ъ250я200г150б130ч130ж70ц60ш50щ40х50ф20ю20й70ь10'), // bg
    (CP: 1251; Data: 'о940а800н650і550и610в540т530е490р460с430к360л360у340д320м310п280я240з220ь180г160б160ч130х120ж90й120ц70ш80ю80є80ї80щ60ф20ґ2'), // uk
    (CP: 1251; Data: 'а1150и980о920е850н500т480р470с490ј500у400в360д340к360л300м340п290з170г160б140ш100ч100ц80ћ100х30ж60њ70љ40ђ30ф10џ10'), // sr
    (CP: 1253; Data: 'α1050ε700ι680ο790τ800ν670σ460ς250η340κ400ρ420π400μ340λ270υ360ω130γ180δ170θ120χ100φ80β70ξ50ζ40ψ10ά180έ130ή90ί160ό150ύ70ώ50ϊ5'), // el
    (CP: 1255; Data: 'י1100ו1000ה900ל700א600ר550ת500ב450מ500ש400נ400ע300ד250כ250ק200ח200פ150ם250ן150צ100ג100ס100ז70ט80ך50ף30ץ10'), // he
    (CP: 1256; Data: 'ا1200ل1100ي650م600و550ن550ه400ر450ت400ب350ع300ك250ف250د300ق200س200ة200أ200ح200ج100إ80ذ80ى100خ90ش100ص80ط70ض40ز50ث50غ30ظ20ئ30ؤ10آ10ء20'), // ar
    (CP: 1256; Data: 'ا1100ي900ر600د550ن550م500و500ه500ت400ب400س300ش200ک300ل250ز150گ100خ100ف100ق100ج80پ80چ60ع70ح40آ100ژ5ط20ص30غ20')  // fa
  );

  AnsiProfileEps = 0.002;
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
// Returns a shared TEncoding instance for ACodePage. TEncoding.GetEncoding
// creates a new object on each call, so use this one instead and never free
// the result: the cached instances are released at finalization.
function GetCachedEncoding(const ACodePage: Integer): TEncoding;

function IsRightToLeftText(const AText: String): Boolean;
function IsRightToLeftByUCC(const AText: String): Boolean;

// -----------------------------------------------------------------------------

implementation

uses RegExpr, LazUTF8, Character;

var
  FEncodingCache: TList = NIL;
  FAnsiProfiles: array of TAnsiProfile;
  FAnsiTables: array of TAnsiTableCache;
  AnsiSingleLetters: UnicodeString; // latin letters that can be a word
  AnsiNoStartLetters: UnicodeString; // ...or only a word, never its start

// -----------------------------------------------------------------------------

function GetCachedEncoding(const ACodePage: Integer): TEncoding;
var
  i: Integer;
begin
  if FEncodingCache = NIL then
    FEncodingCache := TList.Create;

  for i := 0 to FEncodingCache.Count-1 do
    if TEncoding(FEncodingCache[i]).CodePage = ACodePage then
      Exit(TEncoding(FEncodingCache[i]));

  Result := TEncoding.GetEncoding(ACodePage);
  FEncodingCache.Add(Result);
end;

// -----------------------------------------------------------------------------

procedure FreeEncodingCache;
var
  i: Integer;
begin
  if FEncodingCache = NIL then Exit;
  for i := 0 to FEncodingCache.Count-1 do
    TEncoding(FEncodingCache[i]).Free;
  FreeAndNil(FEncodingCache);
end;

const
  _Arabic: array of String = ('من', 'هل', 'لا', 'في', 'لقد', 'ما', 'ماذا', 'يا', 'هذا', 'إلى', 'على', 'أنا', 'أنت', 'حسناً', 'أيها', 'كان', 'كيف', 'يكون', 'هذه', 'هذان', 'الذي', 'التي', 'الذين', 'هناك', 'هنالك');
  _Hebrew: array of String = ('אתה', 'אולי', 'הוא', 'בסדר', 'יודע', 'טוב', 'אֶת', 'שֶׁל', 'עַל', 'הוּא', 'אֲשֶׁר', 'הִיא', 'הַמְלֶט', 'נָסִיךְ', 'הֲלֹא', 'עוֹד', 'אֵין', 'אֲנִי', 'זֹאת', 'וְלֹא', 'כָּךְ', 'הִנֵּה', 'מְאֹד', 'אֲדוֹנִי');
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

{ ANSI code page detection

  Each candidate code page decodes the text, and every word with non ASCII
  characters is checked for plausibility: letters of a single script, sane
  casing, no symbols between letters... A wrong code page usually produces
  words like "żDónde", "mьssen" or "×òî".
  The non ASCII letters are then compared with the letter frequencies of the
  languages that use that code page, so code pages that decode to plausible
  words (1250 vs 1252, 1251 vs 1253...) can be told apart. }


// -----------------------------------------------------------------------------

procedure LoadAnsiProfiles;
var
  i, j, w, sum : Integer;
  s : UnicodeString;
  Weights : array of Integer;
begin
  if Length(FAnsiProfiles) > 0 then Exit;
  AnsiSingleLetters := UTF8Decode('àèéìòùóáőöå');
  AnsiNoStartLetters := UTF8Decode('ÀÈÌÒÙ');
  SetLength(FAnsiProfiles, Length(AnsiProfiles));
  for i := 0 to High(AnsiProfiles) do
  begin
    FAnsiProfiles[i].CP := AnsiProfiles[i].CP;
    FAnsiProfiles[i].Letters := '';
    SetLength(Weights, 0);
    s := UTF8Decode(AnsiProfiles[i].Data);
    j := 1;
    while j <= Length(s) do
    begin
      FAnsiProfiles[i].Letters := FAnsiProfiles[i].Letters + s[j];
      Inc(j);
      w := 0;
      while (j <= Length(s)) and (s[j] >= '0') and (s[j] <= '9') do
      begin
        w := w * 10 + Ord(s[j]) - Ord('0');
        Inc(j);
      end;
      SetLength(Weights, Length(Weights)+1);
      Weights[High(Weights)] := w;
    end;
    sum := 0;
    for j := 0 to High(Weights) do sum := sum + Weights[j];
    if sum = 0 then sum := 1;
    SetLength(FAnsiProfiles[i].Probs, Length(Weights));
    for j := 0 to High(Weights) do
      FAnsiProfiles[i].Probs[j] := Weights[j] / sum;
  end;
end;

// -----------------------------------------------------------------------------

function GetAnsiTable(const ACodePage: Integer): TAnsiTable;
var
  i, b : Integer;
  Enc  : TEncoding;
  B1   : TBytes;
  u    : UnicodeString;
  c    : UnicodeChar;
begin
  for i := 0 to High(FAnsiTables) do
    if FAnsiTables[i].CP = ACodePage then
      Exit(FAnsiTables[i].Table);

  Enc := GetCachedEncoding(ACodePage);
  // followed by a space, some converters (iconv 1255/1258) wait for combining marks
  SetLength(B1, 2);
  B1[1] := $20;
  for b := $80 to $FF do
  begin
    c := #0;
    B1[0] := b;
    try
      u := Enc.GetString(B1);
      if (Length(u) = 2) and (u[2] = ' ') then c := u[1];
    except
      c := #0;
    end;
    // undefined bytes (replacement chars, C1 controls, private use area...)
    if (c < #$A0) or (c = '?') or (c = #$FFFD) or ((c >= #$E000) and (c <= #$F8FF)) then
      c := #0;
    Result[b] := c;
  end;

  SetLength(FAnsiTables, Length(FAnsiTables)+1);
  FAnsiTables[High(FAnsiTables)].CP := ACodePage;
  FAnsiTables[High(FAnsiTables)].Table := Result;
end;

// -----------------------------------------------------------------------------

function IsAnsiSeparator(const C: UnicodeChar): Boolean;
begin
  case Ord(C) of
    $A0, $A1, $BF, $AB, $BB, $B7, $B4, $B0, $A8, $B8,
    $2018, $2019, $201A, $201C, $201D, $201E, $2026, $2013, $2014, $2022,
    $060C, $061B, $061F: Result := True;
  else
    Result := TCharacter.IsWhiteSpace(C);
  end;
end;

// -----------------------------------------------------------------------------

// 1 = Latin, 2 = Greek, 3 = Cyrillic, 4 = Hebrew, 5 = Arabic, 0 = other
function CharScript(const C: UnicodeChar): Integer;
begin
  case Ord(C) of
    $41..$5A, $61..$7A, $C0..$24F : Result := 1;
    $370..$3FF : Result := 2;
    $400..$4FF : Result := 3;
    $590..$5FF : Result := 4;
    $600..$6FF : Result := 5;
  else
    Result := 0;
  end;
end;

// -----------------------------------------------------------------------------

function ScoreAnsiCodePage(const Buffer: TBytes; const ACodePage: Integer; out AScore, ARatio: Double): Boolean;
var
  Table : TAnsiTable;
  Freq  : array[$80..$FF] of Integer;
  i, b, p, Good, Bad, Cased, Caps, AsciiWords : Integer;
  // current word
  wLen, wLetters, wNonAscii, wUpper, wScripts, wScript : Integer;
  wInvalid, wFirstUpper, wFirstCased : Boolean;
  wFirst : UnicodeChar;
  // letter frequencies
  Chars  : UnicodeString;
  Counts : array of Integer;
  c : UnicodeChar;
  n : Integer;
  LL, Best, r : Double;

  procedure EndWord;
  var
    ok : Boolean;
  begin
    if wLen = 0 then Exit;
    if wFirstCased then
    begin
      Inc(Cased);
      if wFirstUpper then Inc(Caps);
    end;

    if wNonAscii = 0 then
    begin
      if (wLetters >= 2) and not wInvalid then Inc(AsciiWords);
    end
    else
    begin
      ok := not wInvalid and (wScript > 0) and (wScripts = 1 shl wScript);
      // casing: lower, UPPER or Capitalized
      if ok and (wScript <= 3) and (wUpper > 0) and (wUpper < wLetters) then
        ok := (wUpper = 1) and TCharacter.IsUpper(wFirst);
      // latin: a word can't be made of accented letters only
      if ok and (wScript = 1) then
      begin
        if (wLetters = 1) then
          ok := Pos(TCharacter.ToLower(wFirst), AnsiSingleLetters) > 0
        else if Pos(wFirst, AnsiNoStartLetters) > 0 then
          ok := False
        else if (wLetters >= 3) and (wNonAscii = wLetters) then
          ok := False
        else if (wLetters >= 4) and (wNonAscii*3 > wLetters*2) then
          ok := False;
      end;

      if ok then Inc(Good) else Inc(Bad);
    end;

    wLen := 0; wLetters := 0; wNonAscii := 0; wUpper := 0; wScripts := 0; wScript := 0;
    wInvalid := False; wFirstUpper := False; wFirstCased := False; wFirst := #0;
  end;

  procedure AddLetter(const AChar: UnicodeChar; const ANonAscii: Boolean);
  var
    s : Integer;
  begin
    s := CharScript(AChar);
    if wLetters = 0 then wFirst := AChar;
    if wLen = 0 then
    begin
      wFirstCased := (s >= 1) and (s <= 3);
      wFirstUpper := TCharacter.IsUpper(AChar);
    end;
    if s = 0 then
      wInvalid := True
    else
    begin
      if wScript = 0 then wScript := s;
      wScripts := wScripts or (1 shl s);
    end;
    if TCharacter.IsUpper(AChar) then Inc(wUpper);
    Inc(wLetters);
    if ANonAscii then Inc(wNonAscii);
    Inc(wLen);
  end;

begin
  Result := False;
  AScore := 0;
  ARatio := 0;
  LoadAnsiProfiles;
  Table := GetAnsiTable(ACodePage);
  FillChar(Freq, SizeOf(Freq), 0);
  Good := 0; Bad := 0; Cased := 0; Caps := 0; AsciiWords := 0;
  wLen := 0; wLetters := 0; wNonAscii := 0; wUpper := 0; wScripts := 0; wScript := 0;
  wInvalid := False; wFirstUpper := False; wFirstCased := False; wFirst := #0;

  for i := 0 to High(Buffer) do
  begin
    b := Buffer[i];
    if b < $80 then
    begin
      if Char(b) in ['A'..'Z', 'a'..'z'] then
        AddLetter(UnicodeChar(b), False)
      else
        EndWord;
    end
    else
    begin
      c := Table[b];
      if c = #0 then
      begin
        // undefined in this code page
        wInvalid := True;
        Inc(wNonAscii);
        Inc(wLen);
      end
      else if IsAnsiSeparator(c) then
        EndWord
      else if TCharacter.IsLetter(c) then
      begin
        AddLetter(c, True);
        Inc(Freq[b]);
      end
      else if (TCharacter.GetUnicodeCategory(c) in [TUnicodeCategory.ucNonSpacingMark, TUnicodeCategory.ucCombiningMark, TUnicodeCategory.ucEnclosingMark]) or (c = #$0640) then
      begin
        // marks (niqqud, harakat) and tatweel
        Inc(wNonAscii);
        Inc(wLen);
      end
      else
      begin
        // symbols can't be inside a word
        wInvalid := True;
        Inc(wNonAscii);
        Inc(wLen);
      end;
    end;
  end;
  EndWord;

  if Good + Bad = 0 then Exit;
  ARatio := Good / (Good + Bad);

  // frequencies of the (lowercase) non ASCII letters
  Chars := '';
  SetLength(Counts, 0);
  n := 0;
  for b := $80 to $FF do
    if Freq[b] > 0 then
    begin
      c := TCharacter.ToLower(Table[b]);
      p := Pos(c, Chars);
      if p = 0 then
      begin
        Chars := Chars + c;
        SetLength(Counts, Length(Chars));
        Counts[High(Counts)] := 0;
        p := Length(Chars);
      end;
      Inc(Counts[p-1], Freq[b]);
      Inc(n, Freq[b]);
    end;

  // best matching language of this code page
  Best := -1000;
  for i := 0 to High(FAnsiProfiles) do
    if FAnsiProfiles[i].CP = ACodePage then
    begin
      LL := 0;
      for b := 1 to Length(Chars) do
      begin
        p := Pos(Chars[b], FAnsiProfiles[i].Letters);
        if p > 0 then
          r := FAnsiProfiles[i].Probs[p-1]
        else
          r := 0;
        LL := LL + Counts[b-1] * Ln(r + AnsiProfileEps);
      end;
      if n > 0 then LL := LL / n;
      if LL > Best then Best := LL;
    end;

  // words of cased scripts must start with uppercase sometimes
  if (Cased >= 50) and (Caps * 50 < Cased) then
    Best := Best - 1;
  // a non latin text has (almost) no ascii words
  if ((ACodePage = 1251) or (ACodePage = 1253) or (ACodePage = 1255) or (ACodePage = 1256)) and (AsciiWords > Good + Bad) then
    Best := Best - 3;

  if ARatio > 0.01 then
    AScore := Best + 3 * Ln(ARatio)
  else
    AScore := Best + 3 * Ln(0.01);
  // 1252 is the most common one
  if ACodePage = 1252 then
    AScore := AScore + 0.2;

  Result := True;
end;

// -----------------------------------------------------------------------------

function DetectAnsiEncoding(const Buffer: TBytes): TEncoding;
var
  i, BestCP : Integer;
  Score, Ratio, BestScore, BestRatio : Double;
begin
  Result := NIL;
  BestCP := 0;
  BestScore := 0;
  BestRatio := 0;

  for i := 0 to High(AnsiCandidates) do
    if ScoreAnsiCodePage(Buffer, AnsiCandidates[i], Score, Ratio) then
      if (BestCP = 0) or (Score > BestScore) then
      begin
        BestCP := AnsiCandidates[i];
        BestScore := Score;
        BestRatio := Ratio;
      end;

  if (BestCP > 0) and (BestRatio >= 0.5) then
    Result := GetCachedEncoding(BestCP);
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
       Result := GetCachedEncoding(12000)
    else if (BOM[0] = $FF) and (BOM[1] = $FE) then // UTF-16 (LE)
       Result := TEncoding.Unicode
    else if (BOM[0] = $FE) and (BOM[1] = $FF) then // UTF-16 (BE)
       Result := TEncoding.BigEndianUnicode
    else if (BOM[0] = 0) and (BOM[1] = 0) and (BOM[2] = $FE) and (BOM[3] = $FF) then // UTF-32 (BE)
       Result := GetCachedEncoding(12001)
    else if (BOM[0] = $2B) and (BOM[1] = $2F) and (BOM[2] = $76) and ((BOM[3] = $38) or (BOM[3] = $39) or (BOM[3] = $2B) or (BOM[3] = $2F)) then // UTF-7
       Result := TEncoding.UTF7
    else if Size > 0 then
    begin
      if Size > 500000 then Size := 500000;
      Stream.Position := 0;
      SetLength(BOM, Size);
      Stream.Read(BOM[0], Size);

      // UTF-16 without BOM: ASCII text would also pass as UTF-8,
      // so check it first when there are nulls (never in UTF-8/ANSI text)
      if ContainNulls(BOM) then
      begin
        if IsUTF16_NewLineChars(BOM, CouldBeUTF) then
          Result := TEncoding.Unicode
        else if CouldBeUTF then
          Result := TEncoding.BigEndianUnicode
        else if IsUTF16_ASCII(BOM, CouldBeUTF) then
          Result := TEncoding.Unicode
        else if CouldBeUTF then
          Result := TEncoding.BigEndianUnicode;
        if Result <> NIL then Exit;
      end;

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

function IsRightToLeftByUCC(const AText: String): Boolean;
var
  CurP: PChar;
  CodePoint: Cardinal;
  CharLen: Integer;
begin
  Result := False;
  if AText.IsEmpty then Exit;
  CurP := PChar(AText);
  while CurP^ <> #0 do
  begin
    CodePoint := UTF8CodepointToUnicode(CurP, CharLen);
    {if ((CodePoint >= $0590) and (CodePoint <= $05FF)) or    // Hebrew
       ((CodePoint >= $0600) and (CodePoint <= $06FF)) or    // Arabic
       ((CodePoint >= $0700) and (CodePoint <= $074F)) or    // Syriac
       ((CodePoint >= $0780) and (CodePoint <= $07BF)) or    // Thaana
       ((CodePoint >= $07C0) and (CodePoint <= $07FF)) then  // N'Ko}
    if (CodePoint >= $0590) and (CodePoint <= $08FF) then
      Exit(True);

    Inc(CurP, CharLen);
  end;
end;

// -----------------------------------------------------------------------------

finalization
  FreeEncodingCache;

end.
