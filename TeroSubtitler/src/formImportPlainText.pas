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

unit formImportPlainText;

// -----------------------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LCLTranslator, UWCheckBox, procTextSync;

type

  { TfrmImportPlainText }

  TfrmImportPlainText = class(TForm)
    btnImport: TButton;
    btnClose: TButton;
    cboMode: TComboBox;
    chkReplace: TUWCheckBox;
    chkSyncWithAudio: TUWCheckBox;
    lblMode: TLabel;
    lblPreview: TLabel;
    lblCount: TLabel;
    mmoPreview: TMemo;
    procedure btnCloseClick(Sender: TObject);
    procedure btnImportClick(Sender: TObject);
    procedure cboModeSelect(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FLines    : TStringList;
    FSegments : TSegments;
    function GetStartTime: Integer;
    procedure DoPreview;
  public
    function LoadFile(const AFileName: String): Boolean;
  end;

var
  frmImportPlainText: TfrmImportPlainText;
  // set when the entries were imported and the user wants to synchronize them
  ImportPlainTextSyncRequested: Boolean = False;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, procWorkspace, procSubtitle, procUndo, procLocalize, procVST,
  formMain, UWSystem.Encoding, UWSubtitleAPI.Tags, Math;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmImportPlainText }

// -----------------------------------------------------------------------------

procedure TfrmImportPlainText.FormCreate(Sender: TObject);
begin
  FLines := TStringList.Create;
  SetLength(FSegments, 0);
  ImportPlainTextSyncRequested := False;

  with cboMode.Items do
  begin
    BeginUpdate;
    Clear;
    Add(lngPlainTextEachLine);
    Add(lngPlainTextEachParagraph);
    Add(lngPlainTextAutomatic);
    EndUpdate;
  end;
  cboMode.ItemIndex := 2;

  chkReplace.Enabled := Subtitles.Count > 0;
  chkReplace.Checked := False;
  chkSyncWithAudio.Enabled := frmMain.MPV.IsMediaLoaded;
  chkSyncWithAudio.Checked := chkSyncWithAudio.Enabled;

  {$IFNDEF WINDOWS}
  PrepareCustomControls(Self);
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

procedure TfrmImportPlainText.FormDestroy(Sender: TObject);
begin
  FLines.Free;
  SetLength(FSegments, 0);
end;

// -----------------------------------------------------------------------------

procedure TfrmImportPlainText.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  frmImportPlainText := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmImportPlainText.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

function TfrmImportPlainText.LoadFile(const AFileName: String): Boolean;
var
  Enc: TEncoding;
  i: Integer;
begin
  Result := False;
  FLines.Clear;
  try
    Enc := GetEncodingFromFile(AFileName);
    if Enc = NIL then Enc := TEncoding.UTF8;
    FLines.LoadFromFile(AFileName, Enc);
    // <i>...</i> (html) as our tags, {\i1}...{\i0}
    for i := 0 to FLines.Count-1 do
      if Pos('<', FLines[i]) > 0 then
        FLines[i] := HTMLTagsToTS(FLines[i]);
    Result := True;
  except
    FLines.Clear;
  end;
  Caption := Format('%s - %s', [Caption, ExtractFileName(AFileName)]);
  DoPreview;
end;

// -----------------------------------------------------------------------------

function TfrmImportPlainText.GetStartTime: Integer;
var
  i: Integer;
begin
  // appended after the last entry
  Result := 0;
  if not chkReplace.Checked then
    for i := 0 to Subtitles.Count-1 do
      Result := Max(Result, Subtitles[i].FinalTime + GetCorrectTime(AppOptions.Conventions.MinPause, AppOptions.Conventions.PauseInFrames));
end;

// -----------------------------------------------------------------------------

procedure TfrmImportPlainText.DoPreview;
var
  Options: TSegmentOptions;
  sl: TStringList;
  i: Integer;
begin
  with AppOptions.Conventions do
  begin
    Options.CPL         := CPL;
    Options.MaxLines    := MaxLines;
    Options.MaxDuration := MaxDuration;
    Options.MinDuration := MinDuration;
    Options.MinPause    := GetCorrectTime(MinPause, PauseInFrames);
    Options.PauseSplit  := 0;
    Options.MaxCPS      := MaxCPS;
    Options.Language    := '';
  end;

  FSegments := PlainTextToSegments(FLines, TPlainTextMode(Max(0, cboMode.ItemIndex)), Options, GetStartTime);

  sl := TStringList.Create;
  try
    for i := 0 to High(FSegments) do
    begin
      if i > 0 then sl.Add('');
      sl.Add(Format('%d', [i+1]));
      sl.Add(TSTagsToHTML(FSegments[i].Text));
    end;
    mmoPreview.Lines.Assign(sl);
  finally
    sl.Free;
  end;
  mmoPreview.SelStart := 0;

  lblCount.Caption := Format(lngPlainTextEntries, [Length(FSegments)]);
  btnImport.Enabled := Length(FSegments) > 0;
end;

// -----------------------------------------------------------------------------

procedure TfrmImportPlainText.cboModeSelect(Sender: TObject);
begin
  DoPreview;
end;

// -----------------------------------------------------------------------------

procedure TfrmImportPlainText.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmImportPlainText.btnImportClick(Sender: TObject);
var
  i, First: Integer;
begin
  if Length(FSegments) = 0 then Exit;

  // the start time depends on the entries kept
  DoPreview;

  with frmMain do
  begin
    if not LayoutVST.Visible then SetViewMode(vmList);
    if not VST.Enabled then EnableWorkArea;

    VST.BeginUpdate;
    try
      if chkReplace.Checked then
        ClearSubtitles(False);

      First := Subtitles.Count;
      for i := 0 to High(FSegments) do
        InsertSubtitle(Subtitles.Count, FSegments[i].InitialTime,
          FSegments[i].FinalTime, FSegments[i].Text, '', False, False);
    finally
      VST.EndUpdate;
    end;
  end;

  UndoInstance.IncrementUndoGroup;
  DoAutoCheckErrors;
  VSTSelectNode(frmMain.VST, First, True, True);
  UpdateValues(True);

  ImportPlainTextSyncRequested := chkSyncWithAudio.Enabled and chkSyncWithAudio.Checked;
  Close;
end;

// -----------------------------------------------------------------------------

end.
