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
 *  Copyright (C) 2023 URUWorks, uruworks@gmail.com.
 *}

unit formSpellCheck;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Dialogs, fgl, LazUTF8,
  UWSubtitleAPI;

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
    FWordMap     : TWordMap;
    FChangeList  : TWordReplaceMap;
    FLastPos     : Integer;
    FLastLine    : Integer;
    FSkipList    : TStrings;
    FTempList    : TStrings;
    FSource      : TSubtitleMode;
    FCurrentLine : String;

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
  procTypes, procWorkspace, procColorTheme, procSubtitle, procCommon,
  UWSystem.XMLLang, UWSystem.StrUtils, UWSpellcheck.Hunspell,
  formMain;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmSpellCheck }

// -----------------------------------------------------------------------------

procedure TfrmSpellCheck.FormCreate(Sender: TObject);
begin
  LoadLanguage(Self);

  FWordMap    := TWordMap.Create;
  FChangeList := TWordReplaceMap.Create;
  FSkipList   := TStringList.Create;
  FTempList   := TStringList.Create;

  FCurrentLine := lblCurrentLine.Caption;
  FillWithDictionaries(NIL, cboDictionary);

  cboSource.AddItem(GetLangString('Text'), NIL);
  cboSource.AddItem(GetLangString('Translation'), NIL);

  if Workspace.TranslatorMode then
    cboSource.ItemIndex := 1
  else
    cboSource.ItemIndex := 0;

  cboSourceChange(NIL);
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
    for i := 0 to mnuDictionary.Count-1 do
      if mnuDictionary.Items[i].Caption = cboDictionary.Text then
        mnuDictionary.Items[i].Checked := True
      else
        mnuDictionary.Items[i].Checked := False;

  CloseAction := caFree;
  frmSpellCheck := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmSpellCheck.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmSpellCheck.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmSpellCheck.PopulateWordMap(const AText: String);
const
  WordSplit: array [0..16] of String = (' ',',','.',';','/',':','"','`','-','(',')','[',']','{','}','!',sLineBreak);

var
  sArr : TStringArray;
  s : String;
  i, p, l : Integer;
begin
  FWordMap.Clear;
  if AText = '' then Exit;

  sArr := AText.Split(WordSplit);
  p    := 0;
  l    := 1;
  for i := 0 to High(sArr) do
  begin
    if sArr[i].Trim <> '' then
    begin
      p := UTF8Pos(sArr[i], AText, p + l);
      l := UTF8Length(sArr[i]);
      s := UTF8Copy(AText, p-1, l+2);
      if not s.StartsWith(swt_StartTag) and not s.EndsWith(swt_EndTag) then // skip possible Tero tags
      begin
        FWordMap.Add(p, sArr[i]);
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSpellCheck.InitializeSpellAtLine(const ALine: Integer = 0);
begin
  btnChange.Enabled    := False;
  btnChangeAll.Enabled := False;

  FTempList.Clear;
  FLastPos  := 0;
  FLastLine := ALine;

  if ALine < (Subtitles.Count-1) then
  begin
    lblCurrentLine.Caption := Format(FCurrentLine, [ALine+1]);
    mmoNotFound.Text := GetSubtitleText(ALine, FSource);
    PopulateWordMap(mmoNotFound.Text);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmSpellCheck.DoSpell(const FirstLoad: Boolean = False);
var
  Suggests: TStrings;
  i, x: Integer;
begin
  if FLastLine < (Subtitles.Count-1) then
  begin
    Suggests := NIL;
    lstSuggestions.Clear;
    try
      for i := 0 to FWordMap.Count-1 do
      begin
        if not HunspellInstance.Spell(FWordMap.Data[i]) and
          (FSkipList.IndexOf(FWordMap.Data[i]) < 0) and (FTempList.IndexOf(FWordMap.Data[i]) < 0) then
        begin
          x := FChangeList.IndexOf(FWordMap.Data[i]);
          if x >= 0 then
          begin
            SetSubtitleText(FLastLine, ReplaceString(GetSubtitleText(FLastLine, FSource), FChangeList.Keys[x], FChangeList.Data[x], True, False), FSource, False, False, False);
          end
          else
          begin
            FLastPos := FWordMap.Keys[i];
            mmoNotFound.SelStart  := FLastPos-1;
            mmoNotFound.SelLength := UTF8Length(FWordMap.Data[i]);

            if HunspellInstance.Suggest(FWordMap.Data[i], Suggests) then
            begin
              lstSuggestions.Items.Assign(Suggests);
              lstSuggestions.ItemIndex := 0;
            end;

            btnChange.Enabled    := lstSuggestions.Count > 0;
            btnChangeAll.Enabled := btnChange.Enabled;

            Exit;
          end;
        end;
      end;
      InitializeSpellAtLine(FLastLine+1);
      DoSpell;
    finally
      if Suggests <> NIL then
        Suggests.Free;
    end;
  end
  else
  begin
    edtCustom.Text := '';
    lblCurrentLine.Caption := '';

    if not FirstLoad then
    begin
      with LanguageManager do
        ShowMessageDialog(GetCommonString('SpellCheckFinished'));

      //Close;
    end;
  end;
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

procedure TfrmSpellCheck.btnChangeClick(Sender: TObject);
begin
  if (lstSuggestions.Count > 0) and (mmoNotFound.SelText <> '') then
  begin
    SetSubtitleText(FLastLine, ReplaceString(GetSubtitleText(FLastLine, FSource), mmoNotFound.SelText, edtCustom.Text, True, False), FSource, False, False, False);
    FTempList.Add(mmoNotFound.SelText);
  end;

  DoSpell;
end;

// -----------------------------------------------------------------------------

procedure TfrmSpellCheck.btnChangeAllClick(Sender: TObject);
begin
  if (lstSuggestions.Count > 0) and (mmoNotFound.SelText <> '') then
  begin
    SetSubtitleText(FLastLine, ReplaceString(GetSubtitleText(FLastLine, FSource), mmoNotFound.SelText, edtCustom.Text, True, False), FSource, False, False, False);
    FChangeList.Add(mmoNotFound.SelText, edtCustom.Text);
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
  edtCustom.Text := lstSuggestions.Items[lstSuggestions.ItemIndex];
end;

// -----------------------------------------------------------------------------

end.

