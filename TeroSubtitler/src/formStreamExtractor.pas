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

unit formStreamExtractor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLIntf,
  LCLType, laz.VirtualTrees, LCLTranslator, ComCtrls, procLocalize, FGL,
  StrUtils;

type

  { TStreamInfo }

  PStreamInfo = ^TStreamInfo;
  TStreamInfo = record
    Checked: Boolean;
    Id: Integer;
    Kind: String;
    Language: String;
    Codec: String;
    Output: String;
    Convert: Boolean;
  end;

  TStreamInfoList = specialize TFPGList<PStreamInfo>;

  { TfrmStreamExtractor }

  TfrmStreamExtractor = class(TForm)
    btnExtract: TButton;
    btnClose: TButton;
    btnFolder: TButton;
    btnFile: TButton;
    edtFolder: TEdit;
    edtFile: TEdit;
    lblState: TLabel;
    lblFolder: TLabel;
    lblFile: TLabel;
    prbProgress: TProgressBar;
    VST: TLazVirtualStringTree;
    procedure btnCloseClick(Sender: TObject);
    procedure btnExtractClick(Sender: TObject);
    procedure btnFileClick(Sender: TObject);
    procedure btnFolderClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure VSTAdvancedHeaderDraw(Sender: TVTHeader;
      var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
    procedure VSTChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const CellText: String;
      const CellRect: TRect; var DefaultDraw: Boolean);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure VSTHeaderDrawQueryElements(Sender: TVTHeader;
      var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
    procedure VSTInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure VSTResize(Sender: TObject);
  private
    FList: TStreamInfoList;
    procedure AllowToExtract;
    procedure SetStateText(const AText: String);
    procedure SetControlsEnabled(const AValue: Boolean);
    procedure ScanFile(const AFileName: String);
  public

  end;

var
  frmStreamExtractor: TfrmStreamExtractor;
  CancelProcess: Boolean;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, procWorkspace, procVST, procColorTheme, FileUtil, procDialogs,
  UWSubtitleAPI.Formats, UWSystem.Process, UWSystem.SysUtils,
  UWSystem.TimeUtils;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmStreamExtractor }

// -----------------------------------------------------------------------------

procedure TfrmStreamExtractor.FormCreate(Sender: TObject);
begin
  VSTAddColumn(VST, lngseID, 60);
  VSTAddColumn(VST, lngseKind, 100);
  VSTAddColumn(VST, lngseLanguage, 100);
  VSTAddColumn(VST, lngseCodec, 100);
  VSTAddColumn(VST, lngseOutput, 100);

  FList := TStreamInfoList.Create;
  CancelProcess := False;
end;

// -----------------------------------------------------------------------------

procedure TfrmStreamExtractor.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  FList.Free;
  CloseAction := caFree;
  frmStreamExtractor := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmStreamExtractor.FormShow(Sender: TObject);
begin
  CheckColorTheme(Self);
  VSTResize(NIL);
end;

// -----------------------------------------------------------------------------

procedure TfrmStreamExtractor.VSTAdvancedHeaderDraw(Sender: TVTHeader;
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

procedure TfrmStreamExtractor.VSTChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  if Assigned(Node) and (FList.Count > 0) then
    FList[Node^.Index]^.Checked := Node^.CheckState = csCheckedNormal;
end;

// -----------------------------------------------------------------------------

procedure TfrmStreamExtractor.VSTHeaderDrawQueryElements(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
begin
  if ColorThemeInstance.GetRealColorMode = cmDark then
    Elements := [hpeBackground];
end;

// -----------------------------------------------------------------------------

procedure TfrmStreamExtractor.VSTInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  if FList[Node^.Index]^.Output <> '' then
  begin
    Node^.CheckType  := ctTriStateCheckBox;
    Node^.CheckState := csUncheckedNormal;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmStreamExtractor.VSTDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
begin
  DefaultDraw := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmStreamExtractor.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  if FList.Count > 0 then
  begin
    case Column of
      0: CellText := IntToStr(FList[Node^.Index]^.Id);
      1: CellText := FList[Node^.Index]^.Kind;
      2: CellText := FList[Node^.Index]^.Language;
      3: CellText := FList[Node^.Index]^.Codec;
      4: CellText := FList[Node^.Index]^.Output;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmStreamExtractor.VSTResize(Sender: TObject);
var
  c, wCols: Integer;
begin
  wCols := (VST.Width - VST.Header.Columns[0].Width - GetSystemMetrics(SM_CXVSCROLL)) div (VST.Header.Columns.Count-1);
  for c := 1 to VST.Header.Columns.Count-1 do
    VST.Header.Columns[c].Width := wCols;
end;

// -----------------------------------------------------------------------------

procedure TfrmStreamExtractor.btnFileClick(Sender: TObject);
begin
  edtFile.Text := GetFileFromOpenDialog(edtFile.Text);
  AllowToExtract;
  ScanFile(edtFile.Text);
end;

// -----------------------------------------------------------------------------

procedure TfrmStreamExtractor.btnFolderClick(Sender: TObject);
begin
  with TSelectDirectoryDialog.Create(Self) do
  try
    Options := Options + [ofOldStyleDialog, ofCreatePrompt];

    if Execute then
      edtFolder.Text := FileName;

    btnExtract.Enabled := FileExists(edtFile.Text);
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmStreamExtractor.btnCloseClick(Sender: TObject);
begin
  if (FList.Count = 0) or VST.Enabled then
    Close
  else
    CancelProcess := True;
end;

// -----------------------------------------------------------------------------

procedure ThreadProcessCB(Output: String; var Cancel: Boolean);
var
  sl : TStringList;
  i, x : Integer;
  s : String;
begin
  Cancel := CancelProcess;
  //writeln(output);
  Application.ProcessMessages;
  // process output received
  sl := TStringList.Create;
  try
    sl.Text := Output;

    for i := 0 to sl.Count-1 do
    begin
      // Progress
      x := Pos('time=', sl[i]);
      if x > 0 then
      begin
        s := Copy(sl[i], x+5, Pos(' ', sl[i], x)-x-5).Trim;
        if s.StartsWith('-') then
          x := 0
        else
          x := StringToTime(s);

        //writeln(s);
        //writeln(x);
        frmStreamExtractor.prbProgress.Position := x;
      end;
    end;
  finally
    sl.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmStreamExtractor.btnExtractClick(Sender: TObject);
var
  i, x : Integer;
  AParamArray : TStringArray;
  s : String;
  r : Boolean;
begin
  if (edtFile.Text = '') or (edtFolder.Text = '') or
    not FileExists(edtFile.Text) or (not DirectoryExists(edtFolder.Text) and not CreateDir(edtFolder.Text)) then
    Exit
  else if not FileExists(Tools.FFmpeg) then
  begin
    ShowErrorMessageDialog(Format(lngExtractAppError, [FFMPEG_EXE]));
    Exit;
  end;

  if (FList.Count > 0) then
  begin
    SetControlsEnabled(False);
    try
      r := False;
      for i := 0 to FList.Count-1 do
        if FList[i]^.Checked then
        begin
          s := Format('%s_%d_%s.%s', [ChangeFileExt(ExtractFileName(edtFile.Text), ''), FList[i]^.Id, FList[i]^.Kind, FList[i]^.Output]);
          s := ConcatPaths([edtFolder.Text, s]);

          if FList[i]^.Convert then
            AParamArray := StringReplace(FFMPEG_ExtractStream, '-c copy', '', []).Split(' ')
          else
            AParamArray := FFMPEG_ExtractStream.Split(' ');

          for x := 0 to High(AParamArray) do
            AParamArray[x] := StringsReplace(AParamArray[x],
              ['%input', '%output', '%trackid'],
              [edtFile.Text, s, IntToStr(FList[i]^.Id)], []);

          r := ExecuteAppEx(Tools.FFmpeg, AParamArray, @ThreadProcessCB);
        end;

      if r then
        ShowMessageDialog(lngFileSavedSuccessfully);
    finally
      SetControlsEnabled(True);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmStreamExtractor.AllowToExtract;
begin
  btnExtract.Enabled := FileExists(edtFile.Text) and DirectoryExists(edtFolder.Text) and (FList.Count > 0);
end;

// -----------------------------------------------------------------------------

procedure TfrmStreamExtractor.SetStateText(const AText: String);
begin
  lblState.Caption := AText;
  Application.ProcessMessages;
end;

// -----------------------------------------------------------------------------

procedure TfrmStreamExtractor.SetControlsEnabled(const AValue: Boolean);
begin
  edtFile.Enabled    := AValue;
  btnFile.Enabled    := AValue;
  edtFolder.Enabled  := AValue;
  btnFolder.Enabled  := AValue;
  VST.Enabled        := AValue;
  btnExtract.Enabled := AValue;

  prbProgress.Visible := not AValue;

  if AValue then
  begin
    btnClose.Caption := lngbtnClose;
    btnClose.Tag := 0;
  end
  else
  begin
    btnClose.Caption := lngbtnCancel;
    btnClose.Tag := 1;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmStreamExtractor.ScanFile(const AFileName: String);
var
  i : Integer;
  AParamArray : TStringArray;
  Output : TStringList = NIL;
  s : String;
  Info : PStreamInfo;
begin
  if AFileName.IsEmpty then Exit;

  if not FileExists(Tools.FFmpeg) then
  begin
    ShowErrorMessageDialog(Format(lngExtractAppError, [FFMPEG_EXE]));
    Exit;
  end;

  SetStateText(lngseAnalyzingFile);
  VST.RootNodeCount := 0;
  FList.Clear;

  AParamArray := FFMPEG_Info.Split(' ');
  for i := 0 to High(AParamArray) do
    AParamArray[i] := StringReplace(AParamArray[i], '%input', AFileName, []);

  ExecuteAppLoop(Tools.FFmpeg, AParamArray, Output);
  writeln(output.Text);

  try
    prbProgress.Max := 0;
    s := '';
    for i := 0 to Output.Count-1 do
    begin
      Output[i] := Trim(Output[i]);

      if Output[i].StartsWith('Duration: ') and s.IsEmpty then
        s := Trim(Copy(Output[i], 11, Pos(',', Output[i])-11))
      else if Output[i].StartsWith('Stream #') then
      begin
        AParamArray := Trim(Output[i]).Split(' ');
        if Length(AParamArray) >= 4 then
        begin
          New(Info);
          Info^.Checked := False;
          Info^.Convert := False;

          if Pos('[', AParamArray[1]) > 0 then
            Info^.Id := StrToIntDef(Copy(AParamArray[1], Pos(':', AParamArray[1])+1, Pos('[', AParamArray[1])-Pos(':', AParamArray[1])-1), -1)
          else if Pos('(', AParamArray[1]) > 0 then
            Info^.Id := StrToIntDef(Copy(AParamArray[1], Pos(':', AParamArray[1])+1, Pos('(', AParamArray[1])-Pos(':', AParamArray[1])-1), -1)
          else
            Info^.Id := StrToIntDef(Copy(AParamArray[1], Pos(':', AParamArray[1])+1, Length(AParamArray[1])-Pos(':', AParamArray[1])-1), -1);

          Info^.Kind     := Copy(AParamArray[2], 1, Pos(':', AParamArray[2])-1);
          Info^.Codec    := iff(AParamArray[3].EndsWith(','), Copy(AParamArray[3], 1, Length(AParamArray[3])-1), AParamArray[3]);
          Info^.Language := Copy(AParamArray[1], Pos('(', AParamArray[1])+1, Pos(')', AParamArray[1])-Pos('(', AParamArray[1])-1);

          if Info^.Kind.ToLower = 'video' then
          begin
            if Info^.Codec.Contains('mpeg1') then
              Info^.Output := 'mpg'
            else if Info^.Codec.Contains('prores') then
                Info^.Output := 'mov'
            else
              Info^.Output := 'mp4';
          end
          else if Info^.Kind.ToLower = 'audio' then
          begin
            if Info^.Codec.Contains('mp3') then
              Info^.Output := 'mp3'
            else if Info^.Codec.Contains('mp2') then
              Info^.Output := 'mp2'
            else if Info^.Codec.Contains('pcm') then
              Info^.Output := 'wav'
            else
              Info^.Output := 'aac';
          end
          else if Info^.Kind.ToLower = 'subtitle' then
          begin
            if Info^.Codec.Contains('pgs') then
              Info^.Output := 'sup'
            else if Info^.Codec.Contains('vtt') then
              Info^.Output := 'vtt'
            else if Info^.Codec.Contains('subrip') then
              Info^.Output := 'srt'
            else
            begin
              Info^.Output  := 'srt';
              Info^.Convert := True;
            end;
          end
          else
            Info^.Output := '';

          FList.Add(Info);
        end;
      end;
    end;
    if not s.IsEmpty then
      prbProgress.Max := StringToTime(s);
  finally
    Output.Free;
    VST.RootNodeCount := FList.Count;
    SetStateText('');
  end;
end;

{procedure TfrmStreamExtractor.ScanFile(const AFileName: String);
var
  i : Integer;
  AParamArray : TStringArray;
  Output : TStringList = NIL;
  Parser : TJSONParser;
  Data : TJSONData;
  joData,
  joItem,
  joItem2 : TJSONObject;
  jaData,
  ZeroArray : TJSONArray;
  s, ff : String;
  Info : PStreamInfo;
begin
  if AFileName.IsEmpty then Exit;

  ff := ConcatPaths([ExtractFileDir(Tools.FFmpeg), FFPROBE_EXE]);
  if not FileExists(ff) then
  begin
    ShowErrorMessageDialog(Format(lngExtractAppError, [ff]));
    Exit;
  end;

  VST.RootNodeCount := 0;
  FList.Clear;

  AParamArray := FFPROBE_Params.Split(' ');
  for i := 0 to High(AParamArray) do
    AParamArray[i] := StringReplace(AParamArray[i], '%input', AFileName, [rfReplaceAll]);

  ExecuteAppLoop(ff, AParamArray, Output);
  writeln(output.Text);

  Parser := TJSONParser.Create(Output.Text);
  ZeroArray := TJSONArray.Create;
  try
    try
      Data := Parser.Parse;
    except
      Exit;
    end;

    ZeroArray.Clear;
    joData := TJSONObject(Data);
    if not Assigned(joData) then Exit;

    jaData := joData.Get('streams', ZeroArray);
    for i := 0 to jaData.Count-1 do
    begin
      joItem := jaData.Objects[i];

      New(Info);
      Info^.Checked  := False;
      Info^.Id       := joItem.Get('index', -1);
      Info^.Kind     := joItem.Get('codec_type', '');
      Info^.Codec    := joItem.Get('codec_name', '');
      Info^.Duration := joItem.Get('duration', '');

      if Info^.Duration <> '' then
        Info^.Duration := TimeToString(Round(StrToSingle(Info^.Duration.Replace('.', FormatSettings.DecimalSeparator) , 0, FormatSettings)*1000));

      joItem2 := joitem.Objects['tags'];
      if Assigned(joItem2) then
      begin
        if Info^.Duration = '' then
        begin
          s := joItem2.Get('DURATION-eng', '');
          Info^.Duration := Copy(s, 1, Pos('.', s)-1);
        end;
        Info^.Language := joItem2.Get('language', '');
      end;

      FList.Add(Info);
    end;
  finally
    Output.Free;
    ZeroArray.Free;
    FreeAndNil(Parser);
  end;

  VST.RootNodeCount := FList.Count;
end;}

// -----------------------------------------------------------------------------

end.

