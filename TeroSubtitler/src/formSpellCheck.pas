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
 *  Copyright (C) 2023-2026 URUWorks, uruworks@gmail.com.
 *}

unit formSpellCheck;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Dialogs, fgl, LazUTF8,
  UWSubtitleAPI, LCLTranslator, procLocalize;

type

  { TFPGMaps }

  TWordMap        = specialize TFPGMap<Integer, String>;
  TWordReplaceMap = specialize TFPGMap<String, String>;

  { TfrmSpellCheck }

  TfrmSpellCheck = class(TForm)
    btnAdd: TButton;
    btnChange: TButton;
    btnChangeAll: TButton;
    btnClose: TButton;
    btnSkip: TButton;
    btnSkipAll: TButton;
    cboDictionary: TComboBox;
    cboSource: TComboBox;
    edtCustom: TEdit;
    lblDictionary: TLabel;
    lblCurrentLine: TLabel;
    lblSource: TLabel;
    lblNotFound: TLabel;
    lblSuggestions: TLabel;
    lstSuggestions: TListBox;
    mmoNotFound: TMemo;
    procedure btnAddClick(Sender: TObject);
    procedure btnChangeAllClick(Sender: TObject);
    procedure btnChangeClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnSkipAllClick(Sender: TObject);
    procedure btnSkipClick(Sender: TObject);
    procedure cboDictionaryChange(Sender: TObject);
    procedure cboSourceChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lstSuggestionsSelectionChange(Sender: TObject; User: boolean);
  private
    FWordMap    : TWordMap;
    FChangeList : TWordReplaceMap;
    FLastPos    : Integer;
    FLastLine   : Integer;
    FSkipList   : TStrings;
    FTempList   : TStrings;
    FSource     : TSubtitleMode;
    FStarted    : Boolean;

    procedure SelectWord(const AStart, ALength: Integer);
    procedure PopulateWordMap(const AText: String);
    procedure InitializeSpellAtLine(const ALine: Integer = 0);
    procedure DoSpell(const FirstLoad: Boolean = False);
  public

  end;

var
  frmSpellCheck: TfrmSpellCheck;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, procWorkspace, procColorTheme, procSubtitle, procConfig,
  procDialogs, UWSystem.StrUtils, UWSpellcheck.Hunspell, formMain, Math, procMacTextView, Character;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ Words to check }

// Words are runs of letters, marks and digits (any script), joined by an
// apostrophe (' ’) or a middle dot (col·lecció) only between two of them.
// Everything else separates words: spaces (no-break too), punctuation, curly
// quotes (“ ” ‘ « »), ellipsis (…), dashes, symbols (♪), etc. Before, only a
// list of ASCII separators was used, so “Hello”, Wait… or ♪ were checked as
// they are and reported as misspelled. Runs without letters (♪, 1990) aren't
// checked. ASS override blocks ({\i1}, {\an8}) and HTML format tags (<i>,
// <font color=...>) aren't text.

type
  TSpellCharKind = (sckSeparator, sckWord, sckJoiner);

  TSpellWord = record
    Start, Len: Integer; // bytes, Start is 1-based
  end;
  TSpellWords = array of TSpellWord;

// -----------------------------------------------------------------------------

function SpellCodepointSize(const AText: String; const APos: Integer): Integer;
var
  c: Byte;
begin
  c := Ord(AText[APos]);
  if c < $C0 then
    Result := 1
  else if c < $E0 then
    Result := 2
  else if c < $F0 then
    Result := 3
  else
    Result := 4;

  if APos + Result - 1 > Length(AText) then
    Result := Length(AText) - APos + 1;
end;

// -----------------------------------------------------------------------------

function SpellCharKind(const AText: String; const APos, ALen: Integer; out AIsLetter: Boolean): TSpellCharKind;
var
  s: String;
  u: UnicodeString;
begin
  AIsLetter := False;
  if ALen = 1 then
  begin
    case AText[APos] of
      'a'..'z', 'A'..'Z' : begin
                             AIsLetter := True;
                             Result := sckWord;
                           end;
      '0'..'9'           : Result := sckWord;
      ''''               : Result := sckJoiner;
    else
      Result := sckSeparator;
    end;
    Exit;
  end;

  s := Copy(AText, APos, ALen);
  // ’ (U+2019), · (U+00B7), ZWNJ and ZWJ (U+200C, U+200D)
  if (s = #$E2#$80#$99) or (s = #$C2#$B7) or (s = #$E2#$80#$8C) or (s = #$E2#$80#$8D) then
    Exit(sckJoiner);

  Result := sckSeparator;
  u := UTF8Decode(s);
  if u = '' then Exit;
  case TCharacter.GetUnicodeCategory(u, 1) of
    TUnicodeCategory.ucUppercaseLetter,
    TUnicodeCategory.ucLowercaseLetter,
    TUnicodeCategory.ucTitlecaseLetter,
    TUnicodeCategory.ucModifierLetter,
    TUnicodeCategory.ucOtherLetter     : begin
                                           AIsLetter := True;
                                           Result := sckWord;
                                         end;
    TUnicodeCategory.ucNonSpacingMark,
    TUnicodeCategory.ucCombiningMark,
    TUnicodeCategory.ucEnclosingMark,
    TUnicodeCategory.ucDecimalNumber,
    TUnicodeCategory.ucLetterNumber,
    TUnicodeCategory.ucOtherNumber     : Result := sckWord;
  end;
end;

// -----------------------------------------------------------------------------

// length in bytes of the tag at APos ({\...}, <i>, </font>...), 0 if none
function SpellTagLength(const AText: String; const APos: Integer): Integer;
var
  p: Integer;
  t: String;
begin
  Result := 0;
  case AText[APos] of
    '{': if (APos < Length(AText)) and (AText[APos+1] = '\') then
         begin
           // same rule as RemoveTSTags: only override blocks are tags
           p := Pos('}', AText, APos+2);
           if p > 0 then Result := p - APos + 1;
         end;
    '<': begin
           p := Pos('>', AText, APos+1);
           if p > 0 then
           begin
             t := LowerCase(Copy(AText, APos, p - APos + 1));
             if (t = '<i>') or (t = '</i>') or (t = '<b>') or (t = '</b>') or
               (t = '<u>') or (t = '</u>') or (t = '<s>') or (t = '</s>') or
               (t = '</font>') or t.StartsWith('<font ') then
               Result := p - APos + 1;
           end;
         end;
  end;
end;

// -----------------------------------------------------------------------------

// the text as shown in the window, without tags
function SpellVisibleText(const AText: String): String;
var
  i, l: Integer;
begin
  Result := '';
  i := 1;
  while i <= Length(AText) do
  begin
    l := SpellTagLength(AText, i);
    if l > 0 then
      Inc(i, l)
    else
    begin
      Result := Result + AText[i];
      Inc(i);
    end;
  end;
end;

// -----------------------------------------------------------------------------

function GetSpellWords(const AText: String): TSpellWords;
var
  i, l, ws, we, n : Integer;
  k               : TSpellCharKind;
  isL, hasL, join : Boolean;

  procedure EndWord;
  begin
    if (ws > 0) and hasL then
    begin
      if n = Length(Result) then SetLength(Result, n + 16);
      Result[n].Start := ws;
      Result[n].Len   := we - ws;
      Inc(n);
    end;
    ws   := 0;
    hasL := False;
    join := False;
  end;

begin
  Result := NIL;
  n    := 0;
  ws   := 0;
  we   := 0;
  hasL := False;
  join := False;
  i    := 1;
  while i <= Length(AText) do
  begin
    l := SpellTagLength(AText, i);
    if l > 0 then
    begin
      EndWord;
      Inc(i, l);
      Continue;
    end;

    l := SpellCodepointSize(AText, i);
    k := SpellCharKind(AText, i, l, isL);
    case k of
      sckWord     : begin
                      if (ws > 0) and (we <> i) and not join then EndWord;
                      if ws = 0 then ws := i;
                      we   := i + l;
                      hasL := hasL or isL;
                      join := False;
                    end;
      sckJoiner   : if (ws > 0) and (we = i) then
                      join := True // inside a word if a letter follows
                    else
                      EndWord;
      sckSeparator: EndWord;
    end;
    Inc(i, l);
  end;
  EndWord;
  SetLength(Result, n);
end;

// -----------------------------------------------------------------------------

// AOld as a whole word (out of the tags) by ANew, in all the text
function ReplaceSpellWord(const AText, AOld, ANew: String): String;
var
  w : TSpellWords;
  i : Integer;
begin
  Result := AText;
  if AOld = '' then Exit;
  w := GetSpellWords(AText);
  for i := High(w) downto 0 do
    if Copy(Result, w[i].Start, w[i].Len) = AOld then
    begin
      Delete(Result, w[i].Start, w[i].Len);
      Insert(ANew, Result, w[i].Start);
    end;
end;

// -----------------------------------------------------------------------------

{ TfrmSpellCheck }

// -----------------------------------------------------------------------------

procedure TfrmSpellCheck.FormCreate(Sender: TObject);
begin
  FWordMap    := TWordMap.Create;
  FChangeList := TWordReplaceMap.Create;
  FSkipList   := TStringList.Create;
  FTempList   := TStringList.Create;

  FillWithDictionaries(NIL, cboDictionary);

  cboSource.AddItem(lngasText, NIL);
  cboSource.AddItem(lngasTranslation, NIL);

  if Workspace.TranslatorMode then
    cboSource.ItemIndex := 1
  else
    cboSource.ItemIndex := 0;
end;

// -----------------------------------------------------------------------------

procedure TfrmSpellCheck.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);

var
  i: Integer;
begin
  FWordMap.Free;
  FChangeList.Free;
  FSkipList.Free;
  FTempList.Free;

  with frmMain do
    for i := 0 to popDictionaries.Items.Count-1 do
      if popDictionaries.Items[i].Caption = cboDictionary.Text then
        popDictionaries.Items[i].Checked := True
      else
        popDictionaries.Items[i].Checked := False;

  CloseAction := caFree;
  frmSpellCheck := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmSpellCheck.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
  if not FStarted then
  begin
    FStarted := True;
    // no Font panel updates on macOS (an access violation in AppKit on
    // some Macs when the window turns key with a word selected)
    MacDisableFontPanel(mmoNotFound);
    cboSourceChange(NIL);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSpellCheck.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

// a selection out of the text raises an NSRangeException on macOS
procedure TfrmSpellCheck.SelectWord(const AStart, ALength: Integer);
var
  len, s: Integer;
begin
  len := UTF8Length(mmoNotFound.Text);
  s   := EnsureRange(AStart, 0, len);
  mmoNotFound.SelStart  := s;
  mmoNotFound.SelLength := EnsureRange(ALength, 0, len - s);
end;

// -----------------------------------------------------------------------------

procedure TfrmSpellCheck.PopulateWordMap(const AText: String);
var
  w : TSpellWords;
  i : Integer;
begin
  FWordMap.Clear;
  if AText = '' then Exit;

  // the keys are positions in characters (1-based), like the memo
  w := GetSpellWords(AText);
  for i := 0 to High(w) do
    FWordMap.Add(UTF8Length(Copy(AText, 1, w[i].Start-1)) + 1, Copy(AText, w[i].Start, w[i].Len));
end;

// -----------------------------------------------------------------------------

procedure TfrmSpellCheck.InitializeSpellAtLine(const ALine: Integer = 0);
begin
  btnChange.Enabled    := False;
  btnChangeAll.Enabled := False;

  FTempList.Clear;
  FLastPos  := 0;
  FLastLine := ALine;

  if ALine < Subtitles.Count then
  begin
    lblCurrentLine.Caption := Format(lngspCurrentLine, [ALine+1]);
    // the memo uses only LF on macOS/Linux, so a CRLF would shift the positions.
    // Without tags: they aren't checked and only made the text harder to read
    mmoNotFound.Text := SpellVisibleText(AdjustLineBreaks(GetSubtitleText(ALine, FSource)));
    PopulateWordMap(mmoNotFound.Text);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSpellCheck.DoSpell(const FirstLoad: Boolean = False);
var
  Suggests: TStrings;
  i, x: Integer;
begin
  while FLastLine < Subtitles.Count do
  begin
    lstSuggestions.Clear;
    for i := 0 to FWordMap.Count-1 do
    begin
      // as written (I'm, London) or in lowercase (a sentence start)
      if not HunspellInstance.Spell(FWordMap.Data[i]) and not HunspellInstance.Spell(UTF8LowerCase(FWordMap.Data[i])) and
        (FSkipList.IndexOf(FWordMap.Data[i]) < 0) and (FTempList.IndexOf(FWordMap.Data[i]) < 0) then
      begin
        x := FChangeList.IndexOf(FWordMap.Data[i]);
        if x >= 0 then
        begin
          SetSubtitleText(FLastLine, ReplaceSpellWord(GetSubtitleText(FLastLine, FSource), FChangeList.Keys[x], FChangeList.Data[x]), FSource, False, False, False);
        end
        else
        begin
          FLastPos := FWordMap.Keys[i];
          SelectWord(FLastPos-1, UTF8Length(FWordMap.Data[i]));

          Suggests := NIL;
          try
            if HunspellInstance.Suggest(FWordMap.Data[i], Suggests) then
            begin
              lstSuggestions.Items.Assign(Suggests);
              lstSuggestions.ItemIndex := 0;
            end;
          finally
            Suggests.Free;
          end;

          btnChange.Enabled    := lstSuggestions.Count > 0;
          btnChangeAll.Enabled := btnChange.Enabled;

          Exit;
        end;
      end;
    end;
    InitializeSpellAtLine(FLastLine+1);
  end;

  edtCustom.Text := '';
  lblCurrentLine.Caption := '';

  if not FirstLoad then
    ShowMessageDialog(lngSpellCheckFinished);
end;

// -----------------------------------------------------------------------------

procedure TfrmSpellCheck.btnSkipClick(Sender: TObject);
begin
  FTempList.Add(mmoNotFound.SelText);
  DoSpell;
end;

// -----------------------------------------------------------------------------

procedure TfrmSpellCheck.btnSkipAllClick(Sender: TObject);
begin
  if (mmoNotFound.SelText <> '') and (FSkipList.IndexOf(mmoNotFound.SelText) < 0) then
    FSkipList.Add(mmoNotFound.SelText);

  DoSpell;
end;

// -----------------------------------------------------------------------------

procedure TfrmSpellCheck.btnAddClick(Sender: TObject);
begin
  if (mmoNotFound.SelText <> '') then
    HunspellInstance.Add(mmoNotFound.SelText);

  DoSpell;
end;

// -----------------------------------------------------------------------------

// the suggestions have the plain apostrophe, as it was written: Kun’s -> Ken’s
function ApostropheAsWritten(const AWord, AReplacement: String): String;
begin
  if Pos(#$E2#$80#$99, AWord) > 0 then
    Result := StringReplace(AReplacement, '''', #$E2#$80#$99, [rfReplaceAll])
  else
    Result := AReplacement;
end;

// -----------------------------------------------------------------------------

procedure TfrmSpellCheck.btnChangeClick(Sender: TObject);
begin
  if (lstSuggestions.Count > 0) and (mmoNotFound.SelText <> '') then
  begin
    SetSubtitleText(FLastLine, ReplaceSpellWord(GetSubtitleText(FLastLine, FSource), mmoNotFound.SelText, ApostropheAsWritten(mmoNotFound.SelText, edtCustom.Text)), FSource, False, False, False);
    FTempList.Add(mmoNotFound.SelText);
  end;

  DoSpell;
end;

// -----------------------------------------------------------------------------

procedure TfrmSpellCheck.btnChangeAllClick(Sender: TObject);
begin
  if (lstSuggestions.Count > 0) and (mmoNotFound.SelText <> '') then
  begin
    SetSubtitleText(FLastLine, ReplaceSpellWord(GetSubtitleText(FLastLine, FSource), mmoNotFound.SelText, ApostropheAsWritten(mmoNotFound.SelText, edtCustom.Text)), FSource, False, False, False);
    FChangeList.Add(mmoNotFound.SelText, ApostropheAsWritten(mmoNotFound.SelText, edtCustom.Text));
  end;

  DoSpell;
end;

// -----------------------------------------------------------------------------

procedure TfrmSpellCheck.cboDictionaryChange(Sender: TObject);
begin
  if (cboDictionary.Items.Count > 0) then
  begin
    AppOptions.HunspellLanguage := GetDictionaryNameFromCaption(cboDictionary.Text);
    HunspellInstance.LoadDictionary(DictionariesFolder+AppOptions.HunspellLanguage+'.aff', DictionariesFolder+AppOptions.HunspellLanguage+'.dic');
    UpdateStatusBar(True);

    InitializeSpellAtLine;
    DoSpell;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSpellCheck.cboSourceChange(Sender: TObject);
begin
  if cboSource.ItemIndex = 0 then
    FSource := smText
  else
    FSource := smTranslation;

  InitializeSpellAtLine;
  DoSpell(Sender = NIL);
end;

// -----------------------------------------------------------------------------

procedure TfrmSpellCheck.lstSuggestionsSelectionChange(Sender: TObject;
  User: boolean);
begin
  if (lstSuggestions.ItemIndex >= 0) and (lstSuggestions.ItemIndex < lstSuggestions.Count) then
    edtCustom.Text := lstSuggestions.Items[lstSuggestions.ItemIndex];
end;

// -----------------------------------------------------------------------------

end.

