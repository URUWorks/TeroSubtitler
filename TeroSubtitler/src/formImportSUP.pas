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

unit formImportSUP;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLIntf,
  LCLType, laz.VirtualTrees, LCLTranslator, ExtCtrls, ComCtrls, procLocalize,
  BlurayPGSParser;

type

  { TfrmImportSUP }

  TfrmImportSUP = class(TForm)
    btnOCR: TButton;
    btnLanguage: TButton;
    btnImport: TButton;
    btnClose: TButton;
    cboLanguage: TComboBox;
    imgSUP: TImage;
    lblLanguage: TLabel;
    pnlSUP: TPanel;
    prbProgress: TProgressBar;
    VST: TLazVirtualStringTree;
    procedure btnCloseClick(Sender: TObject);
    procedure btnImportClick(Sender: TObject);
    procedure btnLanguageClick(Sender: TObject);
    procedure btnOCRClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure VSTAdvancedHeaderDraw(Sender: TVTHeader;
      var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
    procedure VSTDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const CellText: String;
      const CellRect: TRect; var DefaultDraw: Boolean);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure VSTHeaderDrawQueryElements(Sender: TVTHeader;
      var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
    procedure VSTResize(Sender: TObject);
  private
    FList: TBlurayPGSParser;
    procedure CheckButtons;
    procedure SetControlsEnabled(const AValue: Boolean);
  public
    CurrentJob: Integer;
  end;

var
  frmImportSUP: TfrmImportSUP;
  CancelProcess: Boolean;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, procWorkspace, procVST, procColorTheme, FileUtil, procSubtitle,
  UWSubtitleAPI.Formats, UWSubtitles.Utils, BGRABitmap, procTesseract,
  procConfig, procForms, UWSystem.Process, StrUtils;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmImportSUP }

// -----------------------------------------------------------------------------

procedure TfrmImportSUP.FormCreate(Sender: TObject);
begin
  VSTAddColumn(VST, lngIndex, 50);
  VSTAddColumn(VST, lnghTimes, 90);
  VSTAddColumn(VST, lnghDuration, 70);
  VSTAddColumn(VST, lnghText, 100, taLeftJustify);

  VST.DefaultNodeHeight := VST.Canvas.Font.GetTextHeight('W')*3;
  VST.UpdateRanges;

  FList := TBlurayPGSParser.Create;

  with TOpenDialog.Create(Self) do
  try
    Title   := lngOpenFile;
    Filter  := lngAllSupportedFiles + ' (*.sup)|*.sup';
    Options := [ofPathMustExist, ofFileMustExist];

    if Execute then
      FList.Parse(FileName);
  finally
    Free;
  end;

  VST.RootNodeCount := FList.DisplaySets.Count;
  prbProgress.Max   := VST.RootNodeCount;
  FillComboWithAvailableLanguages(TessDataFolder, cboLanguage);

  CheckButtons;

  if btnImport.Enabled then
    VSTSelectNode(VST, 0, True, True);

  CancelProcess := False;
end;

// -----------------------------------------------------------------------------

procedure TfrmImportSUP.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  FList.Free;
  CloseAction := caFree;
  frmImportSUP := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmImportSUP.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmImportSUP.btnCloseClick(Sender: TObject);
begin
  if btnClose.Tag = 0 then
    Close
  else
    CancelProcess := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmImportSUP.btnImportClick(Sender: TObject);
var
  i: Integer;
begin
  if (VST.RootNodeCount > 0) then
  begin
    ClearSubtitles(False);
    for i := FList.DisplaySets.Count-1 downto 0 do
      InsertSubtitle(0, FList.DisplaySets[i]^.InCue, FList.DisplaySets[i]^.OutCue, FList.DisplaySets[i]^.Text, '', i = 0, i = 0);

    Close;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmImportSUP.btnLanguageClick(Sender: TObject);
begin
  ShowImportSUPLanguages;
  FillComboWithAvailableLanguages(TessDataFolder, cboLanguage);
  CheckButtons;
end;

// -----------------------------------------------------------------------------

procedure TessCB(Output: String; var Cancel: Boolean);
begin
  //writeln(Output);
  Cancel := CancelProcess;
  if Output.IsEmpty then Exit;

  with frmImportSUP do
  begin
    FList.DisplaySets[CurrentJob]^.Text := Trim(Output);
    VSTSelectNode(VST, CurrentJob, True);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmImportSUP.btnOCRClick(Sender: TObject);
var
  i, c: Integer;
  params: TStringArray;
  s, lang: String;
begin
  CancelProcess := False;
  if FList.DisplaySets.Count > 0 then
  begin
    SetControlsEnabled(False);
    try
      lang := DisplayNameToCultureName(cboLanguage.Text);
      if lang.IsEmpty then lang := 'eng';

      for c := 0 to FList.DisplaySets.Count-1 do
      begin
        CurrentJob := c;

        prbProgress.Position := c;
        if CancelProcess then Exit;
        Application.ProcessMessages;

        s := ChangeFileExt(GetTempFileName, '.png');
        if not FList.SaveBitmapToFile(c, s, False) then
          Continue;

        params := Tesseract_Params.Split(' ');
        for i := 0 to High(params) do
          params[i] := StringsReplace(params[i], ['%input', '%lang'], [s, lang], []);

        ExecuteAppEx(Tools.Tesseract, params, NIL, @TessCB);
      end;

      if FileExists(s) then
        DeleteFile(s);
    finally
      SetControlsEnabled(True);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmImportSUP.VSTAdvancedHeaderDraw(Sender: TVTHeader;
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

procedure TfrmImportSUP.VSTHeaderDrawQueryElements(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
begin
  if ColorThemeInstance.GetRealColorMode = cmDark then
    Elements := [hpeBackground];
end;

// -----------------------------------------------------------------------------

procedure TfrmImportSUP.VSTDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
var
  TS : TTextStyle;
  it, ft: Integer;
begin
  DefaultDraw := False;
  TargetCanvas.Brush.Style := bsClear;

  if vsSelected in Node^.States then
    TargetCanvas.Font.Color := ColorThemeInstance.Colors.HighlightText
  else
    TargetCanvas.Font.Color := ColorThemeInstance.Colors.Text;

  FillByte(TS, SizeOf(TTextStyle), 0);
  TS.EndEllipsis := True;
  TS.RightToLeft := Application.IsRightToLeft or Subtitles.IsRightToLeft; // is correct?
  TS.Layout := tlTop;

  if FList.DisplaySets.Count > 0 then
  begin
    case Column of
      0: begin
           TS.Alignment := taCenter;
           TargetCanvas.TextRect(CellRect, CellRect.Left, CellRect.Top, '#' + (Node^.Index+1).ToString, TS);
         end;
      1: begin
           TS.Alignment := taCenter;
           TargetCanvas.TextRect(CellRect, CellRect.Left, CellRect.Top, GetTimeStr(FList.DisplaySets[Node^.Index]^.InCue) + sLineBreak + GetTimeStr(FList.DisplaySets[Node^.Index]^.OutCue), TS);
         end;
      2: begin
           TS.Alignment := taCenter;
           RoundFramesValue(FList.DisplaySets[Node^.Index]^.InCue, FList.DisplaySets[Node^.Index]^.OutCue, Workspace.FPS.OutputFPS, it, ft);
           TargetCanvas.TextRect(CellRect, CellRect.Left, CellRect.Top, GetTimeStr(ft-it, True), TS);
         end;
      3: begin
           TS.Alignment := taLeftJustify;
           TargetCanvas.TextRect(CellRect, CellRect.Left, CellRect.Top, FList.DisplaySets[Node^.Index]^.Text, TS);
         end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmImportSUP.VSTFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  bmp: TBGRABitmap;
begin
  if VST.FocusedNode = NIL then Exit;

  bmp := FList.GetBitmap(VSTFocusedNode(VST));
  if bmp = NIL then
    Exit;

  //Label2.Caption := inttostr(bmp.Width) + 'x' + inttostr(bmp.Height);
  imgSUP.Picture.Assign(bmp.Bitmap);
end;

// -----------------------------------------------------------------------------

procedure TfrmImportSUP.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  CellText := ' ';
end;

// -----------------------------------------------------------------------------

procedure TfrmImportSUP.VSTResize(Sender: TObject);
var
  i, NewWidth: Integer;
begin
  if VST.Header.Columns.Count > 0 then
  begin
    NewWidth := 0;
    for i := 0 to 2 do
      NewWidth := NewWidth + VST.Header.Columns[i].Width;

    NewWidth := (VST.Width-NewWidth) - (GetSystemMetrics(SM_CXVSCROLL)+5);
    VST.Header.Columns[3].Width := NewWidth;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmImportSUP.CheckButtons;
begin
  btnImport.Enabled := VST.RootNodeCount > 0;
  btnOCR.Enabled := FileExists(Tools.Tesseract) and (cboLanguage.Items.Count > 0);
end;

// -----------------------------------------------------------------------------

procedure TfrmImportSUP.SetControlsEnabled(const AValue: Boolean);
begin
  prbProgress.Position := 0;

  VST.Enabled         := AValue;
  btnOCR.Enabled      := False;
  btnImport.Enabled   := False;
  cboLanguage.Enabled := AValue;
  btnLanguage.Enabled := AValue;
  prbProgress.Visible := not AValue;

  if AValue then
  begin
    btnClose.Caption := lngbtnClose;
    btnClose.Tag := 0;
    CheckButtons;
  end
  else
  begin
    btnClose.Caption := lngbtnCancel;
    btnClose.Tag := 1;
  end;
end;

// -----------------------------------------------------------------------------

end.

