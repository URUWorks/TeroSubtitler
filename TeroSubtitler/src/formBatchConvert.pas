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

unit formBatchConvert;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  laz.VirtualTrees, fgl, LCLIntf, LCLType;

type

  { TQueueItem }

  PQueueItem = ^TQueueItem;
  TQueueItem = record
    FileName : String;
    Size     : String;
    Format   : String;
    Success  : Boolean;
  end;

  TQueueList = specialize TFPGList<PQueueItem>;

  { TfrmBatchConvert }

  TfrmBatchConvert = class(TForm)
    btnApply: TButton;
    btnClose: TButton;
    btnSelectAll: TButton;
    btnInvert: TButton;
    btnScanFolder: TButton;
    btnAdd: TButton;
    btnRemove: TButton;
    btnOpenFolder: TButton;
    cboFromFPS: TComboBox;
    cboToFPS: TComboBox;
    cboFormat: TComboBox;
    cboEncoding: TComboBox;
    edtFolder: TEdit;
    lblFromFPS: TLabel;
    lblToFPS: TLabel;
    lblFormat: TLabel;
    lblEncoding: TLabel;
    lblOutputFolder: TLabel;
    lblOutput: TLabel;
    prbProgress: TProgressBar;
    VST: TLazVirtualStringTree;
    procedure btnAddClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnInvertClick(Sender: TObject);
    procedure btnOpenFolderClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnScanFolderClick(Sender: TObject);
    procedure btnSelectAllClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure VSTAdvancedHeaderDraw(Sender: TVTHeader;
      var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
    procedure VSTDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const CellText: String;
      const CellRect: TRect; var DefaultDraw: Boolean);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure VSTHeaderDrawQueryElements(Sender: TVTHeader;
      var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
    procedure VSTResize(Sender: TObject);
    procedure VSTStructureChange(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Reason: TChangeReason);
  private
    FList: TQueueList;
    procedure AddFile(const AFileName: String);
    procedure SetControlsEnabled(const AValue: Boolean);
  public

  end;

var
  frmBatchConvert: TfrmBatchConvert;

// -----------------------------------------------------------------------------

implementation

uses
  UWSystem.SysUtils, UWSubtitleAPI, UWSubtitleAPI.Formats, procVST,
  procTypes, RegExpr, procWorkspace, procColorTheme, procCommon,
  UWSystem.XMLLang, UWSystem.FileUtils, UWSystem.Encoding;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmBatchConvert }

// -----------------------------------------------------------------------------

procedure TfrmBatchConvert.FormCreate(Sender: TObject);
var
  FAppStringList: TAppStringList = NIL;
begin
  LoadLanguage(Self);

  FList := TQueueList.Create;
  FillComboWithFormats(cboFormat);
  FillComboWithEncodings(cboEncoding);
  FillComboWithFPS(cboFromFPS);
  cboToFPS.Items.Assign(cboFromFPS.Items);

  cboFormat.ItemIndex := Integer(sfSubRip)-1;
  cboFromFPS.ItemIndex  := cboFromFPS.Items.IndexOf(SingleToStr(GetInputFPS, AppOptions.FormatSettings));
  cboToFPS.ItemIndex    := cboToFPS.Items.IndexOf(SingleToStr(GetFPS, AppOptions.FormatSettings));
  cboEncoding.ItemIndex := Workspace.DefEncoding;

  LanguageManager.GetAppStringList('BatchConvertStrings', FAppStringList);
  VSTAddColumn(VST, GetString(FAppStringList, 'FileName'), 150);
  VSTAddColumn(VST, GetString(FAppStringList, 'FileFormat'), 150);
  VSTAddColumn(VST, GetString(FAppStringList, 'FileState'), 80);
  FAppStringList.Free;
end;

// -----------------------------------------------------------------------------

procedure TfrmBatchConvert.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
var
  i: Integer;
begin
  if FList.Count > 0 then
  begin
    for i := FList.Count-1 downto 0 do
    begin
      Dispose(PQueueItem(FList.Items[i]));
      FList.Items[i] := NIL;
      FList.Delete(i);
    end;
  end;

  FList.Free;
  CloseAction := caFree;
  frmBatchConvert := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmBatchConvert.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmBatchConvert.VSTAdvancedHeaderDraw(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
begin
  if (hpeBackground in Elements) then
  begin
    PaintInfo.TargetCanvas.Brush.Color := ColorThemeInstance.Colors.Window;

    if Assigned(PaintInfo.Column) then
      DrawFrameControl(PaintInfo.TargetCanvas.Handle, PaintInfo.PaintRectangle, DFC_BUTTON, DFCS_FLAT or DFCS_ADJUSTRECT);

    PaintInfo.TargetCanvas.FillRect(PaintInfo.PaintRectangle);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmBatchConvert.VSTHeaderDrawQueryElements(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
begin
  if ColorThemeInstance.GetRealColorMode = cmDark then
    Elements := [hpeBackground];
end;

// -----------------------------------------------------------------------------

procedure TfrmBatchConvert.VSTDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
begin
  DefaultDraw := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmBatchConvert.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  case Column of
    0: CellText := ChangeFileExt(ExtractFileName(FList.Items[Node^.Index]^.FileName), '');
    1: CellText := FList.Items[Node^.Index]^.Format;
    2: if FList.Items[Node^.Index]^.Success then CellText := '✓';
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmBatchConvert.VSTResize(Sender: TObject);
var
  c, wCols: Integer;
begin
  wCols := (VST.Width - VST.Header.Columns[2].Width - (GetSystemMetrics(SM_CXVSCROLL)+VST.Header.Columns.Count)) div 2;
  for c := 0 to 1 do
    VST.Header.Columns[c].Width := wCols;
end;

// -----------------------------------------------------------------------------

procedure TfrmBatchConvert.VSTStructureChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Reason: TChangeReason);
begin
  btnRemove.Enabled := (VST.RootNodeCount > 0);
end;

// -----------------------------------------------------------------------------

procedure TfrmBatchConvert.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmBatchConvert.btnRemoveClick(Sender: TObject);
var
  I: Integer;
  Nodes: TNodeArray;
begin
  Nodes := NIL;
  if (VST.SelectedCount > 0) and (FList.Count > 0) then
  begin
    Nodes := VST.GetSortedSelection(True);
    VST.BeginUpdate;
    try
      for I := High(Nodes) downto 0 do
      begin
        Dispose(PQueueItem(FList.Items[Nodes[I]^.Index]));
        FList.Items[Nodes[I]^.Index] := NIL;
        FList.Delete(Nodes[I]^.Index);
      end;
    finally
      VST.ClearSelection;
      VST.EndUpdate;
      VST.RootNodeCount := FList.Count;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmBatchConvert.btnSelectAllClick(Sender: TObject);
begin
  VST.SelectAll(False);
end;

// -----------------------------------------------------------------------------

procedure TfrmBatchConvert.btnInvertClick(Sender: TObject);
begin
  VST.InvertSelection(False);
end;

// -----------------------------------------------------------------------------

procedure TfrmBatchConvert.btnScanFolderClick(Sender: TObject);
var
  SearchRec: TSearchRec;
begin
  with TSelectDirectoryDialog.Create(Self) do
  try
    Options := Options + [ofPathMustExist, ofOldStyleDialog];

    if Execute then
    begin
      if SysUtils.FindFirst(ConcatPaths([FileName, '*.*']), faAnyFile, SearchRec) = 0 then
      try
        repeat
          if (SearchRec.Attr and faDirectory) = 0 then
            AddFile(ConcatPaths([FileName, SearchRec.Name]));
        until FindNext(SearchRec) <> 0;
      finally
        FindClose(SearchRec);
      end;
    end;
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmBatchConvert.btnAddClick(Sender: TObject);
var
  i: Integer;
begin
  with TOpenDialog.Create(Self) do
  try
    Title   := GetCommonString('OpenFile');
    Filter  := Subtitles.FillDialogFilter(GetCommonString('AllSupportedFiles'));
    Options := Options + [ofFileMustExist, ofAllowMultiSelect];

    if Execute then
    begin
      for i := 0 to Files.Count-1 do
        AddFile(Files[i]);
    end;
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmBatchConvert.btnOpenFolderClick(Sender: TObject);
begin
  with TSelectDirectoryDialog.Create(Self) do
  try
    Options := Options + [ofOldStyleDialog, ofCreatePrompt];

    if Execute then
      edtFolder.Text := FileName;
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmBatchConvert.AddFile(const AFileName: String);
var
  Item: PQueueItem;
  sf: TUWSubtitleFormats;
begin
  sf := GetSubtitleFileFormat(AFileName);
  if sf = sfInvalid then Exit;

  New(Item);
  with Item^ do
  begin
    FileName := AFileName;
    Size     := GetFileSize(AFileName).ToString;
    Format   := IndexToName(ShortInt(sf));
    Success  := False;
  end;
  FList.Add(Item);

  VST.RootNodeCount := FList.Count;
end;

// -----------------------------------------------------------------------------

procedure TfrmBatchConvert.btnApplyClick(Sender: TObject);
var
  i: Integer;
  SubtitleAPI: TUWSubtitles;
begin
  if edtFolder.Text = '' then
  begin
    edtFolder.SetFocus;
    Exit;
  end;

  if not DirectoryExists(edtFolder.Text) then
    if not ForceDirectories(edtFolder.Text) then
    begin
      edtFolder.SetFocus;
      Exit;
    end;

  if not IsEmptyFolder(edtFolder.Text) then
    if MsgFolderNotEmpty = mrNo then
      Exit;

  prbProgress.Position := 0;
  prbProgress.Max := FList.Count-1;

  SetControlsEnabled(False);
  prbProgress.Visible := True;

  SubtitleAPI := TUWSubtitles.Create;
  try
    for i := 0 to FList.Count-1 do
    begin
      if  SubtitleAPI.LoadFromFile(FList[i]^.FileName, NIL, StrToFloatDef(cboFromFPS.Text, Workspace.FPS.OutputFPS, FormatSettings)) then
      begin
        if SubtitleAPI.SaveToFile(ConcatPaths([edtFolder.Text, ExtractFileName(FList[i]^.FileName)]), StrToFloatDef(cboToFPS.Text, Workspace.FPS.OutputFPS, FormatSettings),
          TEncoding.GetEncoding(Encodings[cboEncoding.ItemIndex-1].CPID), TUWSubtitleFormats(cboFormat.ItemIndex+1), smText, -1, -1, True) then
        begin
          FList[i]^.Success := True;

          VST.Invalidate;
        end;
      end;
      prbProgress.Position := i;
      Application.ProcessMessages;
      Sleep(100);
    end;
  finally
    SubtitleAPI.Free;
  end;

  SetControlsEnabled(True);
  prbProgress.Visible := False;
end;

// -----------------------------------------------------------------------------

procedure TfrmBatchConvert.SetControlsEnabled(const AValue: Boolean);
var
  i: Integer;
begin
  for i := 0 to ControlCount-1 do
    if (Controls[i] is TWinControl) and not (Controls[i] is TLazVirtualStringTree) then
      (Controls[i] as TWinControl).Enabled := AValue;
end;

// -----------------------------------------------------------------------------

end.

