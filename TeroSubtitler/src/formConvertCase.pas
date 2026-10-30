{*
 *  URUWorks Tero Subtitler
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
 *  Copyright (C) 2023-2026 URUWorks, uruworks@gmail.com.
 *}

unit formConvertCase;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  UWRadioButton, UWLayout, LCLTranslator;

type

  { TfrmConvertCase }

  TfrmConvertCase = class(TForm)
    btnApply: TButton;
    btnClose: TButton;
    lblScope: TLabel;
    rbnAllTheSubtitles: TUWRadioButton;
    rbnFromTheSelectedSubtitle: TUWRadioButton;
    rbnOnlySelectedSubtitles: TUWRadioButton;
    rdoInverseType: TUWRadioButton;
    rdoLowercase: TUWRadioButton;
    rdoSentenceType: TUWRadioButton;
    rdoTitleType: TUWRadioButton;
    rdoUppercase: TUWRadioButton;
    rdoSmartCase: TUWRadioButton;
    btnNames: TButton;
    lyoScope: TUWLayout;
    procedure btnApplyClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnNamesClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  frmConvertCase: TfrmConvertCase;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, procVST, procVST_Loops, UWSystem.StrUtils,
  UWSubtitleAPI, formMain, LazUTF8, procWorkspace, procConfig, LCLIntf,
  UWSpellcheck.Hunspell;

{$R *.lfm}

var
  FNames: TStringList = NIL; // user names list
  FTextStart, FTransStart: Boolean; // next word starts a sentence
  FDictionary: Boolean; // hunspell dictionary loaded

// -----------------------------------------------------------------------------

{ TfrmConvertCase }

// -----------------------------------------------------------------------------

procedure TfrmConvertCase.FormCreate(Sender: TObject);
begin
  if (frmMain.VST.SelectedCount = 1) then
    rbnFromTheSelectedSubtitle.Checked := True
  else if (frmMain.VST.SelectedCount > 1) then
    rbnOnlySelectedSubtitles.Checked := True
  else
    rbnAllTheSubtitles.Checked := True;

  {$IFNDEF WINDOWS}
  PrepareCustomControls(Self);
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

procedure TfrmConvertCase.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  frmConvertCase := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmConvertCase.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmConvertCase.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

// Smart case: all caps text to normal casing. Sentence starts are capitalized
// (also following the previous entry), names and acronyms are kept using the
// names list and the hunspell dictionary loaded for the spell check.

// -----------------------------------------------------------------------------

function UCFirst(const AWord: String): String;
begin
  Result := UTF8UpperCase(UTF8Copy(AWord, 1, 1)) + UTF8Copy(AWord, 2, MaxInt);
end;

// -----------------------------------------------------------------------------

function IsCasedLetter(const AChar: String): Boolean;
begin
  Result := UTF8LowerCase(AChar) <> UTF8UpperCase(AChar);
end;

// -----------------------------------------------------------------------------

function IsDigitChar(const AChar: String): Boolean;
begin
  Result := (AChar <> '') and (AChar[1] in ['0'..'9']);
end;

// -----------------------------------------------------------------------------

function IsAllCaps(const AText: String): Boolean;
var
  s: String;
begin
  s := RemoveTagsFromText(RemoveTagsFromText(AText), '{', '}');
  Result := (UTF8LowerCase(s) <> s) and (UTF8UpperCase(s) = s);
end;

// -----------------------------------------------------------------------------

function IsRomanNumeral(const AWord: String): Boolean;
var
  s: String;
  i: Integer;
begin
  // II..XXXIX (not single letters, I is a word)
  Result := False;
  s := LowerCase(AWord);
  if (Length(s) < 2) or (Length(s) > 7) then Exit;
  i := 1;
  while (i <= Length(s)) and (s[i] = 'x') do Inc(i);
  if i > 4 then Exit;
  s := Copy(s, i, MaxInt);
  Result := (s = '') or (s = 'i') or (s = 'ii') or (s = 'iii') or (s = 'iv') or
    (s = 'v') or (s = 'vi') or (s = 'vii') or (s = 'viii') or (s = 'ix');
end;

// -----------------------------------------------------------------------------

function SmartCaseWord(const AWord: String; const ASentenceStart: Boolean): String;
const
  Vowels = 'aeiouyáéíóúàèìòùâêîôûäëïöüãõåæøœ';
var
  lw, s: String;
  i: Integer;
  Suggests: TStrings;
  HasVowel: Boolean;
begin
  lw := UTF8LowerCase(AWord);
  if IsRomanNumeral(AWord) then Exit(AWord); // WORLD WAR II

  // names list, as written there
  if Assigned(FNames) then
  begin
    s := FNames.Values[lw];
    if s <> '' then Exit(s);
  end;

  if HunspellInstance.Spell(lw) then
    Result := lw // a common word
  else if HunspellInstance.Spell(UCFirst(lw)) then
    Result := UCFirst(lw) // a proper noun (London, María, I'm)
  else if HunspellInstance.Spell(AWord) then
  begin
    // the dictionary has it in another case: NASA, McDonald, iPhone
    Result := AWord;
    Suggests := TStringList.Create;
    try
      if HunspellInstance.Suggest(lw, Suggests) then
        for i := 0 to Suggests.Count-1 do
          if UTF8UpperCase(Suggests[i]) = UTF8UpperCase(AWord) then
          begin
            Result := Suggests[i];
            Break;
          end;
    finally
      Suggests.Free;
    end;
  end
  else
  begin
    // unknown or no dictionary
    HasVowel := False;
    for i := 1 to UTF8Length(lw) do
    begin
      s := UTF8Copy(lw, i, 1);
      if UTF8Pos(s, Vowels) > 0 then HasVowel := True;
      if IsDigitChar(s) then Exit(AWord); // 3D, 1080P
    end;

    if (UTF8Length(lw) > 1) and not HasVowel then
      Result := AWord // FBI, TV
    else if lw.StartsWith('i''') or lw.StartsWith('i’') then
      Result := UCFirst(lw) // english I'm, I'll
    else if FDictionary and (UTF8Length(lw) > 1) then
      Result := UCFirst(lw) // not in the dictionary, most likely a name
    else
      Result := lw;
  end;

  if ASentenceStart and (UTF8Copy(Result, 1, 1) <> UTF8UpperCase(UTF8Copy(Result, 1, 1))) then
    Result := UCFirst(Result);
end;

// -----------------------------------------------------------------------------

function SmartCase(const AText: String; var ASentenceStart: Boolean; const AChange: Boolean): String;
var
  i, l, x: Integer;
  c, w: String;
  LineStart: Boolean;

  function Ch(const AIndex: Integer): String;
  begin
    if (AIndex >= 1) and (AIndex <= l) then
      Result := UTF8Copy(AText, AIndex, 1)
    else
      Result := '';
  end;

begin
  // AChange = False only follows the sentences (text not in all caps)
  Result := '';
  l := UTF8Length(AText);
  LineStart := True;
  i := 1;
  while i <= l do
  begin
    c := Ch(i);
    if (c = '<') or (c = '{') then
    begin
      // tags, as they are
      x := i;
      if c = '<' then w := '>' else w := '}';
      while (x < l) and (Ch(x) <> w) do Inc(x);
      Result += UTF8Copy(AText, i, x-i+1);
      i := x+1;
      Continue;
    end;

    if IsCasedLetter(c) or IsDigitChar(c) then
    begin
      // a word, with its apostrophes (I'M, DON'T)
      x := i;
      while (x < l) and (IsCasedLetter(Ch(x+1)) or IsDigitChar(Ch(x+1)) or
        (((Ch(x+1) = '''') or (Ch(x+1) = '’')) and IsCasedLetter(Ch(x+2)))) do Inc(x);
      w := UTF8Copy(AText, i, x-i+1);
      // U.S.A., single letters between dots stay
      if AChange and IsCasedLetter(c) and not ((x = i) and
        (((Ch(x+1) = '.') and IsCasedLetter(Ch(x+2))) or ((Ch(i-1) = '.') and IsCasedLetter(Ch(i-2))))) then
        w := SmartCaseWord(w, ASentenceStart);
      Result += w;
      if IsCasedLetter(c) then ASentenceStart := False;
      LineStart := False;
      i := x+1;
      Continue;
    end;

    if (c = #13) or (c = #10) then
      LineStart := True
    else if (c = '-') and LineStart then
      ASentenceStart := True // dialog
    else if (c = '!') or (c = '?') or (c = '¿') or (c = '¡') then
      ASentenceStart := True
    else if c = '.' then
      // an ellipsis continues the sentence (in the next entry too)
      ASentenceStart := (Ch(i-1) <> '.') and (Ch(i+1) <> '.') and not IsCasedLetter(Ch(i+1))
    else if c = '…' then
      ASentenceStart := False;

    if (c <> ' ') and (c <> #13) and (c <> #10) then LineStart := False;
    Result += c;
    Inc(i);
  end;
end;

// -----------------------------------------------------------------------------

function ApplyNamePhrases(const AText: String): String;
var
  i, p: Integer;
  n, lt: String;
begin
  // names with more than one word (Buenos Aires), word by word they are not found
  Result := AText;
  if not Assigned(FNames) then Exit;
  for i := 0 to FNames.Count-1 do
  begin
    n := FNames.ValueFromIndex[i];
    if Pos(' ', n) = 0 then Continue;

    lt := UTF8LowerCase(Result);
    if Length(lt) <> Length(Result) then Exit; // byte positions would not match

    p := Pos(FNames.Names[i], lt);
    while p > 0 do
    begin
      if ((p = 1) or not (lt[p-1] in ['a'..'z', '0'..'9', #128..#255])) and
        ((p+Length(n) > Length(lt)) or not (lt[p+Length(n)] in ['a'..'z', '0'..'9', #128..#255])) then
        Result := Copy(Result, 1, p-1) + n + Copy(Result, p+Length(n), MaxInt);
      p := Pos(FNames.Names[i], lt, p+Length(n));
    end;
  end;
end;

// -----------------------------------------------------------------------------

function ApplySmartCase(const AText: String; var ASentenceStart: Boolean): String;
begin
  if IsAllCaps(AText) then
    Result := ApplyNamePhrases(SmartCase(AText, ASentenceStart, True))
  else
    Result := SmartCase(AText, ASentenceStart, False);
end;

// -----------------------------------------------------------------------------

procedure LoadNames;
var
  sl: TStringList;
  s: String;
  i: Integer;
begin
  FNames := TStringList.Create;
  // without a dictionary unknown words are not taken as names
  FDictionary := HunspellInstance.Spell('the') or HunspellInstance.Spell('de') or
    HunspellInstance.Spell('la') or HunspellInstance.Spell('und') or HunspellInstance.Spell('a');

  if not FileExists(NamesFileName) then Exit;

  sl := TStringList.Create;
  try
    sl.LoadFromFile(NamesFileName);
    for i := 0 to sl.Count-1 do
    begin
      s := Trim(sl[i]);
      if (s <> '') and not s.StartsWith('#') then
        FNames.Add(UTF8LowerCase(s) + '=' + s);
    end;
  finally
    sl.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure ApplyConvertCase(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with frmConvertCase, Item^ do
  begin
    if rdoSmartCase.Checked then
      ApplyText(Item, Index, ApplySmartCase(Text, FTextStart), ApplySmartCase(Translation, FTransStart), False)
    else if rdoSentenceType.Checked then
      ApplyText(Item, Index, SentenceCase(Text), SentenceCase(Translation), False)
    else if rdoLowercase.Checked then
      ApplyText(Item, Index, UTF8LowerCase(Text), UTF8LowerCase(Translation), False)
    else if rdoUppercase.Checked then
      ApplyText(Item, Index, UTF8UpperCase(Text), UTF8UpperCase(Translation), False)
    else if rdoTitleType.Checked then
      ApplyText(Item, Index, TitleCase(Text), TitleCase(Translation), False)
    else
      ApplyText(Item, Index, InvertCase(Text), InvertCase(Translation), False);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmConvertCase.btnApplyClick(Sender: TObject);
var
  SelLoop: TVSTDoLoopSelection;
begin
  if rbnAllTheSubtitles.Checked then
    SelLoop := dlAll
  else if rbnFromTheSelectedSubtitle.Checked then
    SelLoop := dlCurrentToLast
  else
    SelLoop := dlSelected;

  FTextStart  := True;
  FTransStart := True;
  if rdoSmartCase.Checked then LoadNames;
  try
    VSTDoLoop(frmMain.VST, @ApplyConvertCase, SelLoop, True, True);
  finally
    FreeAndNil(FNames);
  end;
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmConvertCase.btnNamesClick(Sender: TObject);
var
  sl: TStringList;
begin
  // names list for smart case, edited with the system text editor
  if not FileExists(NamesFileName) then
  begin
    sl := TStringList.Create;
    try
      sl.Add('# Names, places, acronyms... written as they must be kept, one per line');
      sl.Add('# e.g. Frodo, McDonald, NASA');
      sl.SaveToFile(NamesFileName);
    finally
      sl.Free;
    end;
  end;
  OpenDocument(NamesFileName);
end;

// -----------------------------------------------------------------------------

end.

