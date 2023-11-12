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

unit formGenerateVideo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  laz.VirtualTrees, LCLIntf, LCLType, Spin, Menus, ComCtrls, UWCheckBox,
  UWLayout, UWRadioButton, UWTimeEdit;

type

  { TfrmGenerateVideo }

  TfrmGenerateVideo = class(TForm)
    btnClose: TButton;
    btnGenerate: TButton;
    btnRes: TButton;
    cbnSub: TColorButton;
    cbnBox: TColorButton;
    cboFont: TComboBox;
    cboAudioCodec: TComboBox;
    cboFormat: TComboBox;
    cboSampleRate: TComboBox;
    cboBitRate: TComboBox;
    cboAudioChannels: TComboBox;
    cboVideoCodec: TComboBox;
    cboVideoSubtype: TComboBox;
    chkDeinterlace: TUWCheckBox;
    chkCrop: TUWCheckBox;
    lblCropLeft: TLabel;
    lblCropTop: TLabel;
    lblCropWidth: TLabel;
    lblCropHeight: TLabel;
    lblCutFrom: TLabel;
    lblCutTo: TLabel;
    lblFont: TLabel;
    lblAudioEncoding: TLabel;
    lblFormat: TLabel;
    lblSampleRate: TLabel;
    lblBitrate: TLabel;
    lblAudioChannels: TLabel;
    lblTimeElapsed: TLabel;
    lblVideoCodec: TLabel;
    lblFontSize: TLabel;
    lblFontColor: TLabel;
    lblVideoSubtype: TLabel;
    lblVideoRes: TLabel;
    lblX: TLabel;
    popRes: TPopupMenu;
    prbProgress: TProgressBar;
    rdoOthers: TUWRadioButton;
    spnCropLeft: TSpinEdit;
    spnCropTop: TSpinEdit;
    spnCropWidth: TSpinEdit;
    spnCropHeight: TSpinEdit;
    spnFontSize: TSpinEdit;
    spnHeight: TSpinEdit;
    spnWidth: TSpinEdit;
    chkBox: TUWCheckBox;
    chkReEncodeAudio: TUWCheckBox;
    lyoSubtitles: TUWLayout;
    lyoVideo: TUWLayout;
    lyoAudio: TUWLayout;
    rdoSubtitle: TUWRadioButton;
    rdoVideo: TUWRadioButton;
    rdoAudio: TUWRadioButton;
    chkCut: TUWCheckBox;
    tedCutFrom: TUWTimeEdit;
    tedCutTo: TUWTimeEdit;
    lyoOthers: TUWLayout;
    VST: TLazVirtualStringTree;
    procedure btnGenerateClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnResClick(Sender: TObject);
    procedure cboFormatSelect(Sender: TObject);
    procedure cboVideoCodecSelect(Sender: TObject);
    procedure chkCropClick(Sender: TObject);
    procedure chkCutClick(Sender: TObject);
    procedure chkReEncodeAudioClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rdoAudioChange(Sender: TObject);
    procedure rdoOthersChange(Sender: TObject);
    procedure rdoSubtitleChange(Sender: TObject);
    procedure rdoVideoChange(Sender: TObject);
    procedure tedCutFromTimeChange(Sender: TObject; const NewTime: Cardinal);
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
  private
    FOutputFileName: String;
    procedure ResItemClick(Sender: TObject);
    procedure SetLayoutPage(const APage: TUWLayout);
    procedure SetControlsEnabled(const AValue: Boolean);
    procedure OpenFolderClick(Sender: TObject);
    function SuggestNewVideoFileName: String;
  public

  end;

var
  frmGenerateVideo: TfrmGenerateVideo;
  CancelGeneration: Boolean;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, procWorkspace, procColorTheme, procConfig, UWSystem.XMLLang,
  procCustomFormat, procGenerateVideo, formMain, UWSubtitleAPI,
  UWSubtitleAPI.Formats, UWSystem.Encoding, UWSystem.TimeUtils,
  UWSystem.SysUtils, procDialogs;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmGenerateVideo }

// -----------------------------------------------------------------------------

procedure TfrmGenerateVideo.FormCreate(Sender: TObject);
var
  i: Integer;
  m: TMenuItem;
  //  FAppStringList: TAppStringList = NIL;
begin
  LoadLanguage(Self);

  //LanguageManager.GetAppStringList('GenerateVideoStrings', FAppStringList);
  //VSTAddColumn(VST, GetString(FAppStringList, 'Subtitle'), 150);
  //FAppStringList.Free;

  tedCutFrom.FPS := GetFPS;
  tedCutTo.FPS := tedCutFrom.FPS;
  tedCutFrom.TimeMode := GetTimeEditMode;
  tedCutTo.TimeMode := tedCutFrom.TimeMode;

  cboFont.Items.Assign(Screen.Fonts);
  i := cboFont.Items.IndexOf('Verdana');
  if i >= 0 then
    cboFont.ItemIndex := i
  else
    cboFont.ItemIndex := 0;

  popRes.Items.Clear;
  for i := 0 to High(TResolutionList) do
  begin
    m := TMenuItem.Create(popRes);
    m.Caption := TResolutionList[i].Name;
    m.Tag := i;
    m.OnClick := @ResItemClick;
    popRes.Items.Add(m);
  end;

  spnWidth.Value  := frmMain.MPV.GetVideoWidth;
  spnHeight.Value := frmMain.MPV.GetVideoHeight;

  FillComboWithFormats(cboFormat);
  FillComboWithAudioChannels(cboAudioChannels);
  FillComboWithAudioSampleRate(cboSampleRate);
  FillComboWithAudioBitRate(cboBitRate);

  CancelGeneration := False;

  rdoSubtitle.Checked := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmGenerateVideo.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  SaveFormSettings(Self, Format('%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d',
    [cboFormat.ItemIndex, cboFont.ItemIndex, spnFontSize.Value, cbnSub.ButtonColor, cbnBox.ButtonColor,
    chkBox.Checked.ToInteger, cboVideoCodec.ItemIndex, cboVideoSubtype.ItemIndex,
    chkReEncodeAudio.Checked.ToInteger, cboAudioCodec.ItemIndex, cboAudioChannels.ItemIndex, cboSampleRate.ItemIndex, cboBitRate.ItemIndex,
    chkDeinterlace.Checked.ToInteger]));
  CloseAction := caFree;
  frmGenerateVideo := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmGenerateVideo.FormShow(Sender: TObject);
var
  s: String;
  AParamArray: TStringArray;
begin
  CheckColorTheme(Self);
  s := LoadFormSettings(Self);
  if not s.IsEmpty then
  begin
    AParamArray := s.Split(',');
    if Length(AParamArray) = 14 then
    begin
      cboFormat.ItemIndex := AParamArray[0].ToInteger;
      cboFormatSelect(NIL);
      cboFont.ItemIndex := AParamArray[1].ToInteger;
      spnFontSize.Value := AParamArray[2].ToInteger;
      cbnSub.ButtonColor := AParamArray[3].ToInteger;
      cbnBox.ButtonColor := AParamArray[4].ToInteger;
      chkBox.Checked := AParamArray[5].ToBoolean;
      cboVideoCodec.ItemIndex := AParamArray[6].ToInteger;
      cboVideoSubtype.ItemIndex := AParamArray[7].ToInteger;
      chkReEncodeAudio.Checked := AParamArray[8].ToBoolean;
      chkReEncodeAudioClick(NIL);
      cboAudioCodec.ItemIndex := AParamArray[9].ToInteger;
      cboAudioChannels.ItemIndex := AParamArray[10].ToInteger;
      cboSampleRate.ItemIndex := AParamArray[11].ToInteger;
      cboBitRate.ItemIndex := AParamArray[12].ToInteger;
      chkDeinterlace.Checked := AParamArray[13].ToBoolean;
      //chkCut.Checked := AParamArray[14].ToBoolean;
      chkCutClick(NIL);
      chkCropClick(NIL);
    end;
  end
  else
  begin
    cboFormatSelect(NIL);
    chkCutClick(NIL);
    chkReEncodeAudioClick(NIL);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmGenerateVideo.rdoAudioChange(Sender: TObject);
begin
  SetLayoutPage(lyoAudio);
end;

// -----------------------------------------------------------------------------

procedure TfrmGenerateVideo.rdoOthersChange(Sender: TObject);
begin
  SetLayoutPage(lyoOthers);
end;

// -----------------------------------------------------------------------------

procedure TfrmGenerateVideo.rdoSubtitleChange(Sender: TObject);
begin
  SetLayoutPage(lyoSubtitles);
end;

// -----------------------------------------------------------------------------

procedure TfrmGenerateVideo.rdoVideoChange(Sender: TObject);
begin
  SetLayoutPage(lyoVideo);
end;

// -----------------------------------------------------------------------------

procedure TfrmGenerateVideo.tedCutFromTimeChange(Sender: TObject;
  const NewTime: Cardinal);
begin
  if chkCut.Checked then
    frmMain.MPV.SetMediaPosInMs(NewTime);
end;

// -----------------------------------------------------------------------------

procedure TfrmGenerateVideo.btnCloseClick(Sender: TObject);
begin
  if btnGenerate.Enabled then
    Close
  else
    CancelGeneration := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmGenerateVideo.btnResClick(Sender: TObject);
begin
  popRes.PopUp;
end;

// -----------------------------------------------------------------------------

procedure TfrmGenerateVideo.cboFormatSelect(Sender: TObject);
var
  ProRes: Boolean;
begin
  FillComboWithVideoEncoders(cboVideoCodec, cboFormat.ItemIndex);
  cboVideoCodecSelect(NIL);
  ProRes := cboFormat.ItemIndex = High(TFFFormats);
  FillComboWithAudioEncoders(cboAudioCodec, ProRes);
  //lblBitrate.Visible := not ProRes;
  //cboBitRate.Visible := lblBitrate.Visible;
end;

// -----------------------------------------------------------------------------

procedure TfrmGenerateVideo.chkReEncodeAudioClick(Sender: TObject);
begin
  cboAudioCodec.Enabled := chkReEncodeAudio.Checked;
  cboAudioChannels.Enabled := chkReEncodeAudio.Checked;
  cboSampleRate.Enabled    := chkReEncodeAudio.Checked;
  cboBitRate.Enabled       := chkReEncodeAudio.Checked;
  lblAudioEncoding.Enabled := chkReEncodeAudio.Checked;
  lblAudioChannels.Enabled := chkReEncodeAudio.Checked;
  lblSampleRate.Enabled    := chkReEncodeAudio.Checked;
  lblBitrate.Enabled       := chkReEncodeAudio.Checked;
end;

// -----------------------------------------------------------------------------

procedure TfrmGenerateVideo.VSTAdvancedHeaderDraw(Sender: TVTHeader;
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

procedure TfrmGenerateVideo.VSTHeaderDrawQueryElements(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
begin
  if ColorThemeInstance.GetRealColorMode = cmDark then
    Elements := [hpeBackground];
end;

// -----------------------------------------------------------------------------

procedure TfrmGenerateVideo.VSTDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
begin
  DefaultDraw := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmGenerateVideo.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
//  case Column of
//  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmGenerateVideo.VSTResize(Sender: TObject);
var
  c, wCols: Integer;
begin
  wCols := (VST.Width - (GetSystemMetrics(SM_CXVSCROLL)+VST.Header.Columns.Count)) div VST.Header.Columns.Count;
  for c := 0 to VST.Header.Columns.Count-1 do
    VST.Header.Columns[c].Width := wCols;
end;

// -----------------------------------------------------------------------------

procedure TfrmGenerateVideo.SetLayoutPage(const APage: TUWLayout);
var
  C: TComponent;
begin
  for C in Self do
    if (C is TUWLayout) then
      if TUWLayout(C) = APage then
      begin
        TUWLayout(C).Left := 130;
        TUWLayout(C).Top  := 64;
        TUWLayout(C).Show;
      end
      else
        TUWLayout(C).Hide;
end;

// -----------------------------------------------------------------------------

procedure TfrmGenerateVideo.ResItemClick(Sender: TObject);
begin
  with TResolutionList[TMenuItem(Sender).Tag] do
  begin
    spnWidth.Value  := Width;
    spnHeight.Value := Height;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmGenerateVideo.cboVideoCodecSelect(Sender: TObject);
var
  idx: Integer;
begin
  idx := cboVideoCodec.ItemIndex;

  if cboFormat.ItemIndex > 1 then
    idx += cboFormat.ItemIndex;

  FillComboWithVideoSubtypes(cboVideoSubtype, idx);
end;

// -----------------------------------------------------------------------------

procedure TfrmGenerateVideo.chkCropClick(Sender: TObject);
begin
  lblCropLeft.Enabled   := chkCrop.Checked;
  lblCropTop.Enabled    := chkCrop.Checked;
  lblCropWidth.Enabled  := chkCrop.Checked;
  lblCropHeight.Enabled := chkCrop.Checked;
  spnCropLeft.Enabled   := chkCrop.Checked;
  spnCropTop.Enabled    := chkCrop.Checked;
  spnCropWidth.Enabled  := chkCrop.Checked;
  spnCropHeight.Enabled := chkCrop.Checked;
end;

// -----------------------------------------------------------------------------

procedure TfrmGenerateVideo.chkCutClick(Sender: TObject);
begin
  tedCutFrom.Enabled := chkCut.Checked;
  tedCutTo.Enabled   := chkCut.Checked;
  lblCutFrom.Enabled := chkCut.Checked;
  lblCutTo.Enabled   := chkCut.Checked;
end;

// -----------------------------------------------------------------------------

procedure TfrmGenerateVideo.SetControlsEnabled(const AValue: Boolean);
begin
  lblTimeElapsed.Caption := '';
  lblTimeElapsed.Visible := not AValue;
  prbProgress.Visible := not AValue;
  btnGenerate.Enabled := AValue;

  cboFormat.Enabled := AValue;
  rdoSubtitle.Enabled := AValue;
  rdoVideo.Enabled := AValue;
  rdoAudio.Enabled := AValue;
  cboFont.Enabled := AValue;
  spnFontSize.Enabled := AValue;
  chkBox.Enabled := AValue;
  cbnSub.Enabled := AValue;
  cbnBox.Enabled := AValue;
  spnWidth.Enabled := AValue;
  spnHeight.Enabled := AValue;
  btnRes.Enabled := AValue;
  cboVideoCodec.Enabled := AValue;
  cboVideoSubtype.Enabled := AValue;
  chkReEncodeAudio.Enabled := AValue;
  cboAudioCodec.Enabled := False;
  cboAudioChannels.Enabled := False;
  cboSampleRate.Enabled := False;
  cboBitRate.Enabled := False;
  chkDeinterlace.Enabled := AValue;
  chkCut.Enabled := AValue;
  tedCutFrom.Enabled := False;
  tedCutTo.Enabled := False;
  chkCrop.Enabled := AValue;
  spnCropLeft.Enabled   := False;
  spnCropTop.Enabled    := False;
  spnCropWidth.Enabled  := False;
  spnCropHeight.Enabled := False;

  if AValue then
  begin
    chkCutClick(NIL);
    chkCropClick(NIL);
    chkReEncodeAudioClick(NIL);
    cboVideoCodecSelect(NIL);
  end;

  if AValue then
    btnClose.Caption := GetCommonString('btnClose', 'CommonControls')
  else
    btnClose.Caption := GetCommonString('btnCancel', 'CommonControls');
end;

// -----------------------------------------------------------------------------

procedure ProcessCB(Output: String; var ATerminate: Boolean);
var
  x, time : Integer;
begin
  ATerminate := CancelGeneration;
  Application.ProcessMessages;
  // process output received
  x := Pos('time=', Output);
  if x > 0 then
  begin
    time := StringToTime(Copy(Output, x+5, 8));
    if (time >= 0) and (frmMain.sbrSeek.Max > 0) then
    begin
      x := Round((time / frmMain.sbrSeek.Max) * 100);
      frmGenerateVideo.prbProgress.Position := x;
      frmGenerateVideo.lblTimeElapsed.Caption := x.ToString + '%';
    end;
  end;
end;

// -----------------------------------------------------------------------------
procedure TfrmGenerateVideo.OpenFolderClick(Sender: TObject);
begin
  OpenDocument(ExtractFileDir(FOutputFileName));
end;

// -----------------------------------------------------------------------------

function TfrmGenerateVideo.SuggestNewVideoFileName: String;
begin
  Result := ChangeFileExt(ExtractFileName(frmMain.MPV.FileName), '');

  case cboFormat.ItemIndex of
    0, 1 :  case cboVideoCodec.ItemIndex of
              1 : Result += '_x265';
              2 : Result += '_vp9';
              else
                Result += '_x264';
              end;
    2: Result += '_vp9';
    3: Result += '_ProRes';
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmGenerateVideo.btnGenerateClick(Sender: TObject);

  function GetVideoCodec: String;
  begin
    case cboFormat.ItemIndex of
      0, 1 :  case cboVideoCodec.ItemIndex of
                1 : Result := TFFVideoH265Subtype[cboVideoSubtype.ItemIndex].Value;
                2 : Result := TFFVideoVP9Subtype[cboVideoSubtype.ItemIndex].Value;
                else
                  Result := TFFVideoH264Subtype[cboVideoSubtype.ItemIndex].Value;
                end;
      2: Result := TFFVideoVP9Subtype[cboVideoSubtype.ItemIndex].Value;
      3: Result := TFFVideoProResSubtype[cboVideoSubtype.ItemIndex].Value;
    end;
  end;

  function GetCutValue(const TED: TUWTimeEdit): Integer;
  begin
    if chkCut.Checked then
      Result := TED.Value
    else
      Result := -1;
  end;

var
  sub, aEnc, style, extra, ext: String;
begin
  prbProgress.Position := 0;
  lblTimeElapsed.Caption := '';
  FOutputFileName := '';

  if not FileExists(Tools.FFmpeg) then
  begin
    ShowErrorMessageDialog(Format(GetCommonString('ExtractAppError'), [ExtractFileName(Tools.FFmpeg)]));
    Exit;
  end;

  CancelGeneration := False;

  with TSaveDialog.Create(NIL) do
  try
    Title := GetCommonString('SaveFile');

    Filter := Format('%s (*%s)|*%s', [TFFFormats[cboFormat.ItemIndex].Name, TFFFormats[cboFormat.ItemIndex].Value, TFFFormats[cboFormat.ItemIndex].Value]);
    ext := TFFFormats[cboFormat.ItemIndex].Value;

    Options  := [ofOverwritePrompt, ofEnableSizing];
    FileName := ChangeFileExt(SuggestNewVideoFileName, ext);

    if Execute then
    begin
      if ExtractFileExt(FileName) <> ext then
        FOutputFileName := ChangeFileExt(FileName, ext)
      else
        FOutputFileName := FileName;
    end
    else
      Exit;
  finally
    Free;
  end;

  SetControlsEnabled(False);

  sub := ChangeFileExt(GetTempFileName, '.ass');
  if Subtitles.SaveToFile(sub, Workspace.FPS.OutputFPS, TEncoding.GetEncoding(Encodings[Workspace.DefEncoding].CPID), sfAdvancedSubStationAlpha, smText) then
  begin
    if chkReEncodeAudio.Checked then
    begin
      if cboFormat.ItemIndex = High(TFFFormats) then
        aEnc := TFFAudioProResEncoders[cboAudioCodec.ItemIndex].Value
      else
        aEnc := TFFAudioEncoders[cboAudioCodec.ItemIndex].Value;
    end
    else
      aEnc := '';

    if chkDeinterlace.Checked then
      extra := 'yadif, '
    else
      extra := '';

    if chkCrop.Checked then
      extra += Format('crop=%d:%d:%d:%d, ', [spnCropWidth.Value, spnCropHeight.Value, spnCropLeft.Value, spnCropTop.Value]);

    style := Format('Fontname=%s,Fontsize=%d,Alignment=2,PrimaryColour=%s,Outline=0,Shadow=0,MarginV=20',
      [StringReplace(cboFont.Text, ' ', '%%', [rfReplaceAll]), spnFontSize.Value, IntToHexStr(cbnSub.ButtonColor, True, '&H00')]);

    if chkBox.Checked then
      style += ',BorderStyle=4,BackColour=' + IntToHexStr(cbnBox.ButtonColor, True, '&H00');

    if GenerateVideoWithSubtitle(frmMain.MPV.FileName, sub, FOutputFileName,
      spnWidth.Value, spnHeight.Value,
      GetVideoCodec, cboVideoSubtype.ItemIndex, extra,
      GetCutValue(tedCutFrom), GetCutValue(tedCutTo),
      style,
      aEnc, TFFAudioChannels[cboAudioChannels.ItemIndex].Value,
      TFFAudioSampleRate[cboSampleRate.ItemIndex].Value,
      TFFAudioBitRate[cboBitRate.ItemIndex],
      @ProcessCB) then
        ShowMessageDialog(GetCommonString('FileSavedSuccessfully'), '', GetCommonString('OpenContainingFolder'), @OpenFolderClick)
      else
        ShowErrorMessageDialog(GetCommonString('VideoGenerationFailed'));

    SetControlsEnabled(True);
    //Close;
  end;
end;

// -----------------------------------------------------------------------------

end.

