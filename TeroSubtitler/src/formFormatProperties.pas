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

unit formFormatProperties;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  UWLayout, UWCheckBox, UWTimeEdit;

type

  { TfrmFormatProperties }

  TfrmFormatProperties = class(TForm)
    btnApply: TButton;
    btnClose: TButton;
    cbnASFontColor: TColorButton;
    cboDVDLanguage: TComboBox;
    cboEBUDFC: TComboBox;
    cboEBUDSC: TComboBox;
    cboEBUCCT: TComboBox;
    cboFormats: TComboBox;
    cboASAlignment: TComboBox;
    cboASLanguage: TComboBox;
    cboCavenaLanguage: TComboBox;
    chkVTTUseXTIMESTAMP: TUWCheckBox;
    edtASFontName: TEdit;
    edtDVDWeb: TEdit;
    edtDVDAuthor: TEdit;
    edtDVDTitle: TEdit;
    edtDVDInfo: TEdit;
    edtEBUCPN: TEdit;
    edtCavenaComment: TEdit;
    edtCavenaTranslatedTitle: TEdit;
    edtCavenaOriginalTitle: TEdit;
    edtCavenaTranslator: TEdit;
    edtEBULC: TEdit;
    edtEBUCO: TEdit;
    lblASFontColor: TLabel;
    lblVTTMPEGTS: TLabel;
    lblVTTLOCAL: TLabel;
    lblDVDWeb: TLabel;
    lblDVDLanguage: TLabel;
    lblDVDAuthor: TLabel;
    lblDVDTitle: TLabel;
    lblCavenaTranslator: TLabel;
    lblDVDInfo: TLabel;
    lblEBUCPN: TLabel;
    lblCavenaComment: TLabel;
    lblCavenaTranslatedTitle: TLabel;
    lblEBUDSC: TLabel;
    lblEBUCCT: TLabel;
    lblCavenaOriginalTitle: TLabel;
    lblCavenaLanguage: TLabel;
    lblCavenaTCStart: TLabel;
    lblEBUNDR: TLabel;
    lblEBUDFC: TLabel;
    lblASPositionX: TLabel;
    lblEBULC: TLabel;
    lblASPositionY: TLabel;
    lblEBUCO: TLabel;
    lblASWidth: TLabel;
    lblASHeight: TLabel;
    lblASAlignment: TLabel;
    lblASLanguage: TLabel;
    lblASFontName: TLabel;
    lblASFontSize: TLabel;
    lblEBUNDC: TLabel;
    lyoDVDSubtitle: TUWLayout;
    lyoEBU: TUWLayout;
    lyoCavena890: TUWLayout;
    lyoWebVTT: TUWLayout;
    chkVTTWriteCueIdentifiers: TUWCheckBox;
    lyoAdvancedSubtitles: TUWLayout;
    spnASFontSize: TSpinEdit;
    spnVTTMPEGTS: TSpinEdit;
    spnASPositionX: TSpinEdit;
    spnASPositionY: TSpinEdit;
    spnEBUNDC: TSpinEdit;
    spnEBUNDR: TSpinEdit;
    spnASWidth: TSpinEdit;
    spnASHeight: TSpinEdit;
    tedCavenaTCStart: TUWTimeEdit;
    tedVTTLOCAL: TUWTimeEdit;
    procedure btnApplyClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure cboFormatsSelect(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure FillComboWithFormatProperties;
    procedure SetLayoutPage(const APage: TUWLayout);
  public

  end;

var
  frmFormatProperties: TfrmFormatProperties;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, LazUTF8, procWorkspace, procCommon, UWSubtitleAPI.Formats,
  UWSystem.TimeUtils, UWTranslateAPI.Google, UWSystem.XMLLang,
  UWSubtitleAPI.Formats.Cavena890.Types;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ Helpers }

// -----------------------------------------------------------------------------

procedure FillComboWithCavenaLangs(const ACombo: TCombobox);
var
  i: Integer;
begin
  with ACombo do
  begin
    Items.BeginUpdate;
    Clear;
    for i := 0 to Length(LanguageId)-1 do
      Items.Add(GoogleTranslateName[GoogleGetLanguageIndex(LanguageId[i].ShortName)]);
    Items.EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

{ TfrmFormatProperties }

// -----------------------------------------------------------------------------

procedure TfrmFormatProperties.FormCreate(Sender: TObject);
var
  FAppStringList: TAppStringList = NIL;
  i: Integer;
begin
  LoadLanguage(Self);
  FillComboWithFormatProperties;
  FillComboWithGoogleLanguages(cboASLanguage);
  FillComboWithCavenaLangs(cboCavenaLanguage);
  cboASLanguage.Items.Delete(0); // delete "auto" item
  cboDVDLanguage.Items.Assign(cboASLanguage.Items);

  LanguageManager.GetAppStringList('FormatPropertiesStrings', FAppStringList);
  try
    // Advanced Subtitles
    with Subtitles.FormatProperties^.AdvancedSubtitles do
    begin
      cboASLanguage.ItemIndex := GoogleGetLanguageIndex(Language)-1;
      edtASFontName.Text := FontName;
      spnASFontSize.Value := FontSize;
      cbnASFontColor.ButtonColor := TColor(FontColor);
      spnASPositionX.Value := X;
      spnASPositionY.Value := Y;
      spnASWidth.Value := W;
      spnASHeight.Value := H;
      with cboASAlignment.Items do
      begin
        Add(GetString(FAppStringList, 'Center'));
        Add(GetString(FAppStringList, 'Left'));
        Add(GetString(FAppStringList, 'Right'));
      end;
      cboASAlignment.ItemIndex := Alignment;
    end;
    // Cavena 890
    with Subtitles.FormatProperties^.Cavena890 do
    begin
      edtCavenaTranslatedTitle.Text := TranslatedTitle;
      edtCavenaOriginalTitle.Text := OriginalTitle;
      edtCavenaTranslator.Text := Translator;
      edtCavenaComment.Text := Comments;
      cboCavenaLanguage.ItemIndex := GetCavena890LangIndex(PrimaryLanguage);
      tedCavenaTCStart.Value := StringToTime(StartTime, False, GetFPS);
    end;
    // DVD Subtitle
    with Subtitles.FormatProperties^.DVDSubtitle do
    begin
      edtDVDTitle.Text := DVDTitle;
      edtDVDAuthor.Text := Author;
      edtDVDInfo.Text := Info;
      edtDVDWeb.Text := Web;
      cboDVDLanguage.ItemIndex := GoogleGetLanguageIndex(Language)-1;
    end;
    // EBU
    with Subtitles.FormatProperties^.EBU do
    begin
      with cboEBUDFC.Items do
      begin
        Add('STL25.01');
        Add('STL30.01');
      end;

      with cboEBUDSC.Items do
      begin
        Add(GetString(FAppStringList, 'Undefined'));
        Add(GetString(FAppStringList, 'OpenSubtitling'));
        Add(GetString(FAppStringList, 'Level1Teletext'));
        Add(GetString(FAppStringList, 'Level2Teletext'));
      end;

      case DisplayStandardCode of
        $30: i:= 1;
        $31: i:= 2;
        $32: i:= 3;
      else
        i := 0;
      end;

      with cboEBUCCT.Items do
      begin
        Add(GetString(FAppStringList, 'Latin'));
        Add(GetString(FAppStringList, 'LatinCyrillic'));
        Add(GetString(FAppStringList, 'LatinArabic'));
        Add(GetString(FAppStringList, 'LatinGreek'));
        Add(GetString(FAppStringList, 'LatinHebrew'));
      end;

      cboEBUDFC.ItemIndex := DiskFormatCode;
      cboEBUDSC.ItemIndex := i;
      cboEBUCCT.ItemIndex := StrToIntDef(CharCodeTableNumber, 0);
      edtEBUCPN.Text := CodePageNumber;
      edtEBULC.Text := LanguageCode;
      edtEBUCO.Text := CountryOrigin;
      spnEBUNDC.Value := MaxNumberDisplayableChars.ToInteger;
      spnEBUNDR.Value := MaxNumberDisplayableRows.ToInteger;
    end;
    // WebVTT
    with Subtitles.FormatProperties^.WebVTT do
    begin
      chkVTTWriteCueIdentifiers.Checked := WriteCueIdentifiers;
      chkVTTUseXTIMESTAMP.Checked := UseXTIMESTAMP;
      spnVTTMPEGTS.Value := MPEGTS;
      tedVTTLOCAL.Value := LOCAL;
    end;
  finally
    FAppStringList.Free;
  end;

  cboFormatsSelect(NIL);
end;

// -----------------------------------------------------------------------------

procedure TfrmFormatProperties.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  frmFormatProperties := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmFormatProperties.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmFormatProperties.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmFormatProperties.btnApplyClick(Sender: TObject);
begin
  // Advanced Subtitles
  with Subtitles.FormatProperties^.AdvancedSubtitles do
  begin
    Language := GoogleTranslateLocale[cboASLanguage.ItemIndex+1];
    FontName := edtASFontName.Text;
    FontSize := spnASFontSize.Value;
    FontColor := Integer(cbnASFontColor.ButtonColor);
    X := spnASPositionX.Value;
    Y := spnASPositionY.Value;
    W := spnASWidth.Value;
    H := spnASHeight.Value;
    Alignment := cboASAlignment.ItemIndex;
  end;
  // Cavena 890
  with Subtitles.FormatProperties^.Cavena890 do
  begin
    TranslatedTitle := edtCavenaTranslatedTitle.Text;
    OriginalTitle := edtCavenaOriginalTitle.Text;
    Translator := edtCavenaTranslator.Text;
    Comments := edtCavenaComment.Text;
    PrimaryLanguage := LanguageId[cboCavenaLanguage.ItemIndex].Hex;
    StartTime := TimeToString(tedCavenaTCStart.Value, 'hh:mm:ss:ff', GetFPS);
  end;
  // DVD Subtitle
  with Subtitles.FormatProperties^.DVDSubtitle do
  begin
    DVDTitle := edtDVDTitle.Text;
    Author := edtDVDAuthor.Text;
    Info := edtDVDInfo.Text;
    Web := edtDVDWeb.Text;
    Language := GoogleTranslateLocale[cboDVDLanguage.ItemIndex+1];
  end;
  // EBU
  with Subtitles.FormatProperties^.EBU do
  begin
    case cboEBUDSC.ItemIndex of
      1: DisplayStandardCode := $30;
      2: DisplayStandardCode := $31;
      3: DisplayStandardCode := $32;
    else
      DisplayStandardCode := 0;
    end;
    DiskFormatCode := cboEBUDFC.ItemIndex;
    CharCodeTableNumber := cboEBUCCT.ItemIndex.ToString;
    CodePageNumber := edtEBUCPN.Text;
    LanguageCode := edtEBULC.Text;
    CountryOrigin := edtEBUCO.Text;
    MaxNumberDisplayableChars := spnEBUNDC.Value.ToString;
    MaxNumberDisplayableRows := spnEBUNDR.Value.ToString;
  end;
  // WebVTT
  with Subtitles.FormatProperties^.WebVTT do
  begin
    WriteCueIdentifiers := chkVTTWriteCueIdentifiers.Checked;
    UseXTIMESTAMP := chkVTTUseXTIMESTAMP.Checked;
    MPEGTS := spnVTTMPEGTS.Value;
    LOCAL := tedVTTLOCAL.Value;
  end;

  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmFormatProperties.FillComboWithFormatProperties;
begin
  with cboFormats.Items do
  begin
    BeginUpdate;
    Clear;
    Add(FormatToName(sfAdvancedSubtitles));
    Add(FormatToName(sfCavena890));
    Add(FormatToName(sfDVDSubtitle));
    Add(FormatToName(sfEBU));
    Add(FormatToName(sfWebVTT));
    EndUpdate;
  end;
  cboFormats.ItemIndex := 0;
end;

// -----------------------------------------------------------------------------

procedure TfrmFormatProperties.SetLayoutPage(const APage: TUWLayout);
var
  C: TComponent;
begin
  for C in Self do
    if (C is TUWLayout) then
      if TUWLayout(C) = APage then
      begin
        TUWLayout(C).Left := 8;
        TUWLayout(C).Top  := 40;
        TUWLayout(C).Show;
      end
      else
        TUWLayout(C).Hide;
end;

// -----------------------------------------------------------------------------

procedure TfrmFormatProperties.cboFormatsSelect(Sender: TObject);
begin
  case cboFormats.ItemIndex of
    0: SetLayoutPage(lyoAdvancedSubtitles);
    1: SetLayoutPage(lyoCavena890);
    2: SetLayoutPage(lyoDVDSubtitle);
    3: SetLayoutPage(lyoEBU);
    4: SetLayoutPage(lyoWebVTT);
  end;
end;

// -----------------------------------------------------------------------------

end.

