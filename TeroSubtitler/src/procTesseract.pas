{*
 *  URUWorks
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
 *  Copyright (C) 2023-2024 URUWorks, uruworks@gmail.com.
 *}

unit procTesseract;

// -----------------------------------------------------------------------------

{$mode objfpc}{$H+}

interface

uses
  Classes, StdCtrls;

type

  TCultureInfo = packed record
    CultureName : AnsiString;
    DisplayName : String;
  end;

const

  MaxCultureItems = 127;

  TTesseractLanguages : array [0..MaxCultureItems-1] of TCultureInfo =
  (
    (CultureName: 'afr'; DisplayName: 'Afrikaans'),
    (CultureName: 'amh'; DisplayName: 'Amharic'),
    (CultureName: 'ara'; DisplayName: 'Arabic'),
    (CultureName: 'asm'; DisplayName: 'Assamese'),
    (CultureName: 'aze'; DisplayName: 'Azerbaijani'),
    (CultureName: 'aze_cyrl'; DisplayName: 'Azerbaijani - Cyrilic'),
    (CultureName: 'bel'; DisplayName: 'Belarusian'),
    (CultureName: 'ben'; DisplayName: 'Bengali'),
    (CultureName: 'bod'; DisplayName: 'Tibetan'),
    (CultureName: 'bos'; DisplayName: 'Bosnian'),
    (CultureName: 'bre'; DisplayName: 'Breton'),
    (CultureName: 'bul'; DisplayName: 'Bulgarian'),
    (CultureName: 'cat'; DisplayName: 'Catalan; Valencian'),
    (CultureName: 'ceb'; DisplayName: 'Cebuano'),
    (CultureName: 'ces'; DisplayName: 'Czech'),
    (CultureName: 'chi_sim'; DisplayName: 'Chinese - Simplified'),
    (CultureName: 'chi_tra'; DisplayName: 'Chinese - Traditional'),
    (CultureName: 'chr'; DisplayName: 'Cherokee'),
    (CultureName: 'cos'; DisplayName: 'Corsican'),
    (CultureName: 'cym'; DisplayName: 'Welsh'),
    (CultureName: 'dan'; DisplayName: 'Danish'),
    (CultureName: 'dan_frak'; DisplayName: 'Danish - Fraktur (contrib)'),
    (CultureName: 'deu'; DisplayName: 'German'),
    (CultureName: 'deu_frak'; DisplayName: 'German - Fraktur (contrib)'),
    (CultureName: 'deu_latf'; DisplayName: 'German (Fraktur Latin)'),
    (CultureName: 'dzo'; DisplayName: 'Dzongkha'),
    (CultureName: 'ell'; DisplayName: 'Greek, Modern (1453-)'),
    (CultureName: 'eng'; DisplayName: 'English'),
    (CultureName: 'enm'; DisplayName: 'English, Middle (1100-1500)'),
    (CultureName: 'epo'; DisplayName: 'Esperanto'),
    (CultureName: 'equ'; DisplayName: 'Math / equation detection module'),
    (CultureName: 'est'; DisplayName: 'Estonian'),
    (CultureName: 'eus'; DisplayName: 'Basque'),
    (CultureName: 'fao'; DisplayName: 'Faroese'),
    (CultureName: 'fas'; DisplayName: 'Persian'),
    (CultureName: 'fil'; DisplayName: 'Filipino (old - Tagalog)'),
    (CultureName: 'fin'; DisplayName: 'Finnish'),
    (CultureName: 'fra'; DisplayName: 'French'),
    (CultureName: 'frk'; DisplayName: 'German - Fraktur (now deu_latf)'),
    (CultureName: 'frm'; DisplayName: 'French, Middle (ca.1400-1600)'),
    (CultureName: 'fry'; DisplayName: 'Western Frisian'),
    (CultureName: 'gla'; DisplayName: 'Scottish Gaelic'),
    (CultureName: 'gle'; DisplayName: 'Irish'),
    (CultureName: 'glg'; DisplayName: 'Galician'),
    (CultureName: 'grc'; DisplayName: 'Greek, Ancient (to 1453) (contrib)'),
    (CultureName: 'guj'; DisplayName: 'Gujarati'),
    (CultureName: 'hat'; DisplayName: 'Haitian; Haitian Creole'),
    (CultureName: 'heb'; DisplayName: 'Hebrew'),
    (CultureName: 'hin'; DisplayName: 'Hindi'),
    (CultureName: 'hrv'; DisplayName: 'Croatian'),
    (CultureName: 'hun'; DisplayName: 'Hungarian'),
    (CultureName: 'hye'; DisplayName: 'Armenian'),
    (CultureName: 'iku'; DisplayName: 'Inuktitut'),
    (CultureName: 'ind'; DisplayName: 'Indonesian'),
    (CultureName: 'isl'; DisplayName: 'Icelandic'),
    (CultureName: 'ita'; DisplayName: 'Italian'),
    (CultureName: 'ita_old'; DisplayName: 'Italian - Old'),
    (CultureName: 'jav'; DisplayName: 'Javanese'),
    (CultureName: 'jpn'; DisplayName: 'Japanese'),
    (CultureName: 'kan'; DisplayName: 'Kannada'),
    (CultureName: 'kat'; DisplayName: 'Georgian'),
    (CultureName: 'kat_old'; DisplayName: 'Georgian - Old'),
    (CultureName: 'kaz'; DisplayName: 'Kazakh'),
    (CultureName: 'khm'; DisplayName: 'Central Khmer'),
    (CultureName: 'kir'; DisplayName: 'Kirghiz; Kyrgyz'),
    (CultureName: 'kmr'; DisplayName: 'Kurmanji (Kurdish - Latin Script)'),
    (CultureName: 'kor'; DisplayName: 'Korean'),
    (CultureName: 'kor_vert'; DisplayName: 'Korean (vertical)'),
    (CultureName: 'kur'; DisplayName: 'Kurdish (Arabic Script)'),
    (CultureName: 'lao'; DisplayName: 'Lao'),
    (CultureName: 'lat'; DisplayName: 'Latin'),
    (CultureName: 'lav'; DisplayName: 'Latvian'),
    (CultureName: 'lit'; DisplayName: 'Lithuanian'),
    (CultureName: 'ltz'; DisplayName: 'Luxembourgish'),
    (CultureName: 'mal'; DisplayName: 'Malayalam'),
    (CultureName: 'mar'; DisplayName: 'Marathi'),
    (CultureName: 'mkd'; DisplayName: 'Macedonian'),
    (CultureName: 'mlt'; DisplayName: 'Maltese'),
    (CultureName: 'mon'; DisplayName: 'Mongolian'),
    (CultureName: 'mri'; DisplayName: 'Maori'),
    (CultureName: 'msa'; DisplayName: 'Malay'),
    (CultureName: 'mya'; DisplayName: 'Burmese'),
    (CultureName: 'nep'; DisplayName: 'Nepali'),
    (CultureName: 'nld'; DisplayName: 'Dutch; Flemish'),
    (CultureName: 'nor'; DisplayName: 'Norwegian'),
    (CultureName: 'oci'; DisplayName: 'Occitan (post 1500)'),
    (CultureName: 'ori'; DisplayName: 'Oriya'),
    (CultureName: 'osd'; DisplayName: 'Orientation and script detection module'),
    (CultureName: 'pan'; DisplayName: 'Panjabi; Punjabi'),
    (CultureName: 'pol'; DisplayName: 'Polish'),
    (CultureName: 'por'; DisplayName: 'Portuguese'),
    (CultureName: 'pus'; DisplayName: 'Pushto; Pashto'),
    (CultureName: 'que'; DisplayName: 'Quechua'),
    (CultureName: 'ron'; DisplayName: 'Romanian; Moldavian; Moldovan'),
    (CultureName: 'rus'; DisplayName: 'Russian'),
    (CultureName: 'san'; DisplayName: 'Sanskrit'),
    (CultureName: 'sin'; DisplayName: 'Sinhala; Sinhalese'),
    (CultureName: 'slk'; DisplayName: 'Slovak'),
    (CultureName: 'slk_frak'; DisplayName: 'Slovak - Fraktur (contrib)'),
    (CultureName: 'slv'; DisplayName: 'Slovenian'),
    (CultureName: 'snd'; DisplayName: 'Sindhi'),
    (CultureName: 'spa'; DisplayName: 'Spanish; Castilian'),
    (CultureName: 'spa_old'; DisplayName: 'Spanish; Castilian - Old'),
    (CultureName: 'sqi'; DisplayName: 'Albanian'),
    (CultureName: 'srp'; DisplayName: 'Serbian'),
    (CultureName: 'srp_latn'; DisplayName: 'Serbian - Latin'),
    (CultureName: 'sun'; DisplayName: 'Sundanese'),
    (CultureName: 'swa'; DisplayName: 'Swahili'),
    (CultureName: 'swe'; DisplayName: 'Swedish'),
    (CultureName: 'syr'; DisplayName: 'Syriac'),
    (CultureName: 'tam'; DisplayName: 'Tamil'),
    (CultureName: 'tat'; DisplayName: 'Tatar'),
    (CultureName: 'tel'; DisplayName: 'Telugu'),
    (CultureName: 'tgk'; DisplayName: 'Tajik'),
    (CultureName: 'tgl'; DisplayName: 'Tagalog (new - Filipino)'),
    (CultureName: 'tha'; DisplayName: 'Thai'),
    (CultureName: 'tir'; DisplayName: 'Tigrinya'),
    (CultureName: 'ton'; DisplayName: 'Tonga'),
    (CultureName: 'tur'; DisplayName: 'Turkish'),
    (CultureName: 'uig'; DisplayName: 'Uighur; Uyghur'),
    (CultureName: 'ukr'; DisplayName: 'Ukrainian'),
    (CultureName: 'urd'; DisplayName: 'Urdu'),
    (CultureName: 'uzb'; DisplayName: 'Uzbek'),
    (CultureName: 'uzb_cyrl'; DisplayName: 'Uzbek - Cyrilic'),
    (CultureName: 'vie'; DisplayName: 'Vietnamese'),
    (CultureName: 'yid'; DisplayName: 'Yiddish'),
    (CultureName: 'yor'; DisplayName: 'Yoruba')
  );

function CultureNameToDisplayName(const CultureName: AnsiString): AnsiString;
function DisplayNameToCultureName(const DisplayName: String): AnsiString;
function CultureNameFromIndex(const Index: Integer): AnsiString;
procedure FillTStringsWithLanguages(const Items: TStrings);
procedure FillComboWithAvailableLanguages(DataFolder: String; const Combo: TComboBox);

// -----------------------------------------------------------------------------

implementation

uses
  SysUtils, FileUtil;

// -----------------------------------------------------------------------------

function CultureNameToDisplayName(const CultureName: AnsiString): AnsiString;
var
  i: Integer;
begin
  Result := '';

  for i := 0 to High(TTesseractLanguages)-1 do
    if AnsiLowerCase(TTesseractLanguages[i].CultureName) = AnsiLowerCase(CultureName) then
    begin
      Result := TTesseractLanguages[i].DisplayName;
      Break;
    end;
end;

// -----------------------------------------------------------------------------

function DisplayNameToCultureName(const DisplayName: String): AnsiString;
var
  i: Integer;
begin
  Result := '';
  if DisplayName = '' then Exit;

  for i := 0 to High(TTesseractLanguages)-1 do
    if AnsiLowerCase(TTesseractLanguages[i].DisplayName) = AnsiLowerCase(DisplayName) then
    begin
      Result := TTesseractLanguages[i].CultureName;
      Break;
    end;
end;

// -----------------------------------------------------------------------------

function CultureNameFromIndex(const Index: Integer): AnsiString;
begin
  Result := '';

  if (Index < Low(TTesseractLanguages)) or (Index > High(TTesseractLanguages)) then
    Exit;

  Result := TTesseractLanguages[Index].CultureName;
end;

// -----------------------------------------------------------------------------

procedure FillTStringsWithLanguages(const Items: TStrings);
var
  i: Integer;
begin
  Items.BeginUpdate;
  try
    Items.Clear;
    for i := 0 to High(TTesseractLanguages)-1 do
      Items.Add(TTesseractLanguages[i].DisplayName);
  finally
    Items.EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

procedure FillComboWithAvailableLanguages(DataFolder: String; const Combo: TComboBox);
var
  TessFiles : TStringList;
  i : Integer;
  s : String;
begin
  DataFolder := IncludeTrailingPathDelimiter(DataFolder);
  TessFiles := TStringList.Create;
  try
    FindAllFiles(TessFiles, DataFolder, '*.traineddata', True);

    Combo.Items.BeginUpdate;
    try
      Combo.Sorted := True;

      for i := 0 to TessFiles.Count-1 do
      begin
        s := ExtractFileName(TessFiles[i]);
        Combo.Items.Add(
          CultureNameToDisplayName(Copy(s, 1, Pos('.', s)-1))
        );
      end;
      if Combo.Items.Count > 0 then
      begin
        i := Combo.Items.IndexOf(CultureNameToDisplayName('eng'));
        if i >= 0 then
          Combo.ItemIndex := i
        else
          Combo.ItemIndex := 0;
      end;
    finally
      Combo.Items.EndUpdate;
    end;
  finally
    TessFiles.Free;
  end;
end;

// -----------------------------------------------------------------------------

end.
