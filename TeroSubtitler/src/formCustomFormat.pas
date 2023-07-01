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

unit formCustomFormat;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, UWLayout,
  UWSubtitleAPI.CustomFormat, UWSubtitleAPI;

type

  { TfrmCustomFormat }

  TfrmCustomFormat = class(TForm)
    btnSave: TButton;
    btnClose: TButton;
    cboEncoding: TComboBox;
    cboTimeCode: TComboBox;
    cboScript: TComboBox;
    cboFPS: TComboBox;
    edtExtension: TEdit;
    edtTitle: TEdit;
    edtNewLineChar: TEdit;
    edtTimeFormat: TEdit;
    edtSeparatorChar: TEdit;
    lblEncoding: TLabel;
    lblTimeCode: TLabel;
    lblExtension: TLabel;
    lblNewLineChar: TLabel;
    lblTitle: TLabel;
    lblTimeFormat: TLabel;
    lblSeparatorChar: TLabel;
    lblScript: TLabel;
    lblFPS: TLabel;
    mmoSource: TMemo;
    lyoEditor: TUWLayout;
    lyoSettings: TUWLayout;
    procedure btnCloseClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure cboScriptSelect(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    CustomFormat: TUWSubtitleCustomTextFormat;
    function ReplaceHeaderTags(const S, TimeFormat: String): String;
    function ReplaceBodyTags(const S, TimeFormat: String; const Item: TUWSubtitleItem; const InFrames: Boolean; const NewChar: String; const Index: Integer): String;
  public

  end;

var
  frmCustomFormat: TfrmCustomFormat;

// -----------------------------------------------------------------------------

implementation

uses
  procWorkspace, procTypes, procCommon, UWSystem.XMLLang, UWSystem.Encoding,
  UWSystem.SysUtils, UWSystem.StrUtils, UWSystem.TimeUtils;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmCustomFormat }

// -----------------------------------------------------------------------------

procedure TfrmCustomFormat.FormCreate(Sender: TObject);
var
  FAppStringList: TAppStringList = NIL;
begin
  LoadLanguage(Self);
  CustomFormat := TUWSubtitleCustomTextFormat.Create('');

  FillComboWithFPS(cboFPS, Workspace.FPS.DefFPS);
  FillComboWithEncodings(cboEncoding);
  FillComboWithCustomFormat(cboScript);

  LanguageManager.GetAppStringList('CustomFormatStrings', FAppStringList);
  with cboTimeCode.Items do
  begin
    Add(GetString(FAppStringList, 'Time'));
    Add(GetString(FAppStringList, 'Frames'));
  end;
  FAppStringList.Free;
  cboTimeCode.ItemIndex := 0;

  cboScriptSelect(NIL);

  btnSave.Enabled := cboScript.Items.Count > 0;
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomFormat.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CustomFormat.Free;

  CloseAction := caFree;
  frmCustomFormat := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomFormat.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomFormat.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomFormat.btnSaveClick(Sender: TObject);
var
  SD  : TSaveDialog;
  TXT : TStringList;
  i   : Integer;
  s   : String;
begin
  SD := TSaveDialog.Create(NIL);
  try
    SD.Title  := GetCommonString('SaveFile');
    SD.Filter := Format('%s (%s)|%s', [edtTitle.Text, edtExtension.Text, edtExtension.Text]);
    SD.FilterIndex := 0;
    SD.FileName := ChangeFileExt(ExtractFileName(SubtitleInfo.Text.FileName), '');
    SD.Options := [ofOverwritePrompt, ofEnableSizing];
    if SD.Execute then
    begin
      s := SD.FileName;
      CustomFormat.Clear;
      with CustomFormat do
      begin
        Name         := edtTitle.Text;
        Extension    := Copy(edtExtension.Text, 2);
        NewLineChar  := edtNewLineChar.Text;
        TimeFormat   := edtTimeFormat.Text;
        DecSeparator := iff(edtSeparatorChar.Text <> '', edtSeparatorChar.Text[1], ',');
        FPS          := StrToSingle(cboFPS.Text, Workspace.FPS.DefFPS, AppOptions.FormatSettings);
        Time         := cboTimeCode.ItemIndex = 0;
        ReadFormattingFromStrings(mmoSource.Lines);

        TXT := TStringList.Create;
        with TXT do
        try
          BeginUpdate;
          try
            Clear;
            if Header <> '' then
              Add(ReplaceHeaderTags(Header, TimeFormat));

            for i := 0 to Subtitles.Count-1 do
              Add(ReplaceBodyTags(Body, TimeFormat, Subtitles[i], not Time, NewLineChar, i+1));

            if Footer <> '' then
              Add(ReplaceHeaderTags(Footer, TimeFormat));
          finally
            EndUpdate;
          end;
          SaveToFile(ChangeFileExt(s, Extension), TEncoding.GetEncoding(Encodings[cboEncoding.ItemIndex].CPID));
        finally
          Free;
        end;
      end;
      Close;
    end;
  finally
    SD.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmCustomFormat.cboScriptSelect(Sender: TObject);
var
  FS: TFormatSettings;
begin
  with CustomFormat do
  begin
    LoadFromFile(CustomFormatFolder + cboScript.Items[cboScript.ItemIndex] + '.cfp');
    if Success then
    begin
      FS := AppOptions.FormatSettings;
      FS.DecimalSeparator := DecSeparator;

      edtTitle.Text           := Name;
      edtExtension.Text       := Extension;
      edtNewLineChar.Text     := NewLineChar;
      edtTimeFormat.Text      := TimeFormat;
      edtSeparatorChar.Text   := DecSeparator;
      cboFPS.Text             := SingleToStr(FPS, FS);
      if Time then
        cboTimeCode.ItemIndex := 0
      else
        cboTimeCode.ItemIndex := 1;
      mmoSource.Text          := Text;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function TfrmCustomFormat.ReplaceHeaderTags(const S, TimeFormat: String): String;
begin
  Result := ReplaceString(S,   '{$Company}', 'URUWorks');
  Result := ReplaceString(Result, '{$Product}', ProgramName);
  Result := ReplaceString(Result, '{$WebSite}', ProgramWebsite);
  Result := ReplaceString(Result, '{TotalCount}', IntToStr(Subtitles.Count));
  Result := ReplaceString(Result, '{FPS}', cboFPS.Text);
  with Subtitles[0] do // First node
  begin
    Result := ReplaceString(Result, '{FirstStart}', Iff(TimeFormat <> '', TimeToString(InitialTime, TimeFormat), IntToStr(InitialTime)));
    Result := ReplaceString(Result, '{FirstEnd}', Iff(TimeFormat <> '', TimeToString(FinalTime, TimeFormat), IntToStr(FinalTime)));
  end;
  with Subtitles[Subtitles.Count-1] do // Last node
  begin
    Result := ReplaceString(Result, '{LastStart}', Iff(TimeFormat <> '', TimeToString(InitialTime, TimeFormat), IntToStr(InitialTime)));
    Result := ReplaceString(Result, '{LastEnd}', Iff(TimeFormat <> '', TimeToString(FinalTime, TimeFormat), IntToStr(FinalTime)));
  end;
end;

// -----------------------------------------------------------------------------

function TfrmCustomFormat.ReplaceBodyTags(const S, TimeFormat: String; const Item: TUWSubtitleItem; const InFrames: Boolean; const NewChar: String; const Index: Integer): String;

  function GetTime(const Time: Cardinal): String;
  begin
    if InFrames then
      Result := IntToStr(TimeToFrames(Time, StrToSingle(cboFPS.Text, Workspace.FPS.DefFPS, AppOptions.FormatSettings)))
    else
      Result := Iff(TimeFormat <> '', TimeToString(Time, TimeFormat), IntToStr(Time));
  end;

begin
  with Item do
  begin
    Result := ReplaceHeaderTags(S, TimeFormat);
    Result := ReplaceString(Result, '{tsIndex}', IntToStr(Index));
    Result := ReplaceString(Result, '{tsStart}', GetTime(InitialTime));
    Result := ReplaceString(Result, '{tsEnd}', GetTime(FinalTime));
    Result := ReplaceString(Result, '{tsDuration}', GetTime(FinalTime-InitialTime));
    Result := ReplaceString(Result, '{tsText}', ReplaceEnters(Text, sLineBreak, iff(LowerCase(NewChar) = '[enter]', sLineBreak, NewChar)));
    Result := ReplaceString(Result, '{tsTranslation}', ReplaceEnters(Translation, sLineBreak, iff(LowerCase(NewChar) = '[enter]', sLineBreak, NewChar)));
    Result := ReplaceString(Result, '{tsStyle}', Style);
    Result := ReplaceString(Result, '{tsActor}', Actor);
    Result := ReplaceString(Result, '{tsX1}', IntToStr(R.Left));
    Result := ReplaceString(Result, '{tsX2}', IntToStr(R.Right));
    Result := ReplaceString(Result, '{tsY1}', IntToStr(R.Top));
    Result := ReplaceString(Result, '{tsY2}', IntToStr(R.Bottom));
  end;
end;

// -----------------------------------------------------------------------------

end.

