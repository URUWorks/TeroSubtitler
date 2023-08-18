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

unit formShotChanges;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Spin, UWTimeEdit, Types, process, StrUtils;

type

  { TfrmShotChanges }

  TfrmShotChanges = class(TForm)
    btnApply: TButton;
    btnClose: TButton;
    btnImport: TButton;
    btnDetect: TButton;
    btnExport: TButton;
    cboOffset: TComboBox;
    cboTimeCodeImport: TComboBox;
    cboTimeCodeExport: TComboBox;
    cboDetectApp: TComboBox;
    lblOffset: TLabel;
    lblDetectApp: TLabel;
    prbExtracting: TProgressBar;
    spnSensitivity: TFloatSpinEdit;
    lblSensitivity: TLabel;
    lblIdle: TLabel;
    lblTimeCodeIn: TLabel;
    lblTimeCodeOut: TLabel;
    mmoTimes: TMemo;
    tedOffset: TUWTimeEdit;
    procedure btnApplyClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
    procedure btnDetectClick(Sender: TObject);
    procedure btnImportClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mmoTimesChange(Sender: TObject);
  private
    FTimeElapsed: Double;
    FCancel: Boolean;
    FSMPTE: Boolean;
    procedure SetControlsEnabled(const AValue: Boolean);
    procedure RunApp_CB(Sender, Context: TObject; Status: TRunCommandEventCode; const Message: String);
    function IsTimeCode(const ACode: Integer): Boolean;
    function IsTimeCodeOut(const ACode: Integer): Boolean;
    function FillDialogFilter(const AAllSupportedFiles: Boolean): String;
  public

  end;

var
  frmShotChanges: TfrmShotChanges;

implementation

uses
  procTypes, procWorkspace, procCommon, UWSystem.TimeUtils, UWSystem.SysUtils,
  UWSystem.Process, formMain, UWSubtitleAPI.EDL, UWSystem.XMLLang,
  formCustomSelectDlg;

const
  tcFrames       = 0;
  tcSeconds      = 1;
  tcMilliseconds = 2;
  tcHHMMSSZZZ    = 3;
  tcHHMMSSFF     = 4;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmShotChanges }

// -----------------------------------------------------------------------------

procedure TfrmShotChanges.FormCreate(Sender: TObject);
var
  FAppStringList: TAppStringList = NIL;
  i: Integer;
begin
  LoadLanguage(Self);

  if LanguageManager.GetAppStringList('ShotChangesStrings', FAppStringList) then
  try
    with cboDetectApp.Items do
    begin
      BeginUpdate;
      Clear;
      Add('FFmpeg');
      if FileExists(SceneDetectFileName) then Add('PySceneDetect');
      EndUpdate;
    end;
    with cboTimeCodeImport.Items do
    begin
      BeginUpdate;
      Clear;
      Add(GetString(FAppStringList, 'Frames'));
      Add(GetString(FAppStringList, 'Seconds'));
      Add(GetString(FAppStringList, 'Milliseconds'));
      Add(GetString(FAppStringList, 'HHMMSSZZZ'));
      Add(GetString(FAppStringList, 'HHMMSSFF'));
      EndUpdate;
    end;
    cboTimeCodeExport.Items.Assign(cboTimeCodeImport.Items);
    cboTimeCodeImport.ItemIndex := 2;
    cboTimeCodeExport.ItemIndex := 4;
    cboDetectApp.ItemIndex := 0;
  finally
    FAppStringList.Free;
  end;

  FCancel := False;
  FSMPTE  := False;

  with frmMain.WAVE do
    if Length(GetSceneChangeList) > 0 then
      for i := 0 to High(GetSceneChangeList) do
        mmoTimes.Lines.Add(GetSceneChangeList[i].ToString);
end;

// -----------------------------------------------------------------------------

procedure TfrmShotChanges.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  SaveFormSettings(Self, Format('%d,%d,%d', [cboDetectApp.ItemIndex, cboTimeCodeImport.ItemIndex, cboTimeCodeExport.ItemIndex]));
  CloseAction := caFree;
  frmShotChanges := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmShotChanges.FormShow(Sender: TObject);
var
  s: String;
  AParamArray: TStringArray;
begin
  CheckColorTheme(Self);
  s := LoadFormSettings(Self);
  if not s.IsEmpty then
  begin
    AParamArray := s.Split(',');
    if Length(AParamArray) = 3 then
    begin
      cboDetectApp.ItemIndex := AParamArray[0].ToInteger;
      cboTimeCodeImport.ItemIndex := AParamArray[1].ToInteger;
      cboTimeCodeExport.ItemIndex := AParamArray[2].ToInteger;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmShotChanges.mmoTimesChange(Sender: TObject);
begin
  btnExport.Enabled := btnDetect.Enabled and (mmoTimes.Lines.Count > 0);
end;

// -----------------------------------------------------------------------------

procedure TfrmShotChanges.btnCloseClick(Sender: TObject);
begin
  if btnClose.Tag = 0 then
    Close
  else
    FCancel := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmShotChanges.btnApplyClick(Sender: TObject);
var
  i, v : Integer;
  s : String;
  SC : TIntegerDynArray;
begin
  if mmoTimes.Lines.Count > 0 then
  begin
    for i := mmoTimes.Lines.Count-1 downto 0 do
    begin
      if (IsTimeCode(tcHHMMSSZZZ) and not TimeInFormat(mmoTimes.Lines[i], 'hh:mm:ss.zzz'))
        or (not IsTimeCode(tcHHMMSSZZZ) and not IsInteger(mmoTimes.Lines[i])) then
        mmoTimes.Lines.Delete(i);
    end;

    SC := NIL;
    SetLength(SC, mmoTimes.Lines.Count);
    try
      for i := 0 to mmoTimes.Lines.Count-1 do
      begin
        if IsTimeCode(tcFrames) then
          v := FramesToTime(StrToIntDef(mmoTimes.Lines[i], 0), GetFPS)
        else if IsTimeCode(tcSeconds) then
          v := StrToIntDef(mmoTimes.Lines[i], 0) * 1000
        else if IsTimeCode(tcMilliseconds) then
          v := StrToIntDef(mmoTimes.Lines[i], 0)
        else if IsTimeCode(tcHHMMSSFF) then
          v := StringToTime(mmoTimes.Lines[i], False, GetFPS)
        else
          v := StringToTime(mmoTimes.Lines[i]);

        if tedOffset.Value > 0 then
          v := v + iff(cboOffset.ItemIndex = 0, tedOffset.Value, -tedOffset.Value);

        if FSMPTE and (Workspace.WorkMode = wmTime) then
          v := Round(v * 1.001);

        SC[i] := v;
      end;

      s := ConcatPaths([ShotChangesFolder, ChangeFileExt(ExtractFileName(frmMain.MPV.FileName), '.shotchanges')]);
      //if not FileExists(s) then
        mmoTimes.Lines.SaveToFile(s);

      frmMain.WAVE.SetSceneChangeList(SC);
    finally
      SetLength(SC, 0);
    end;

    Close;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmShotChanges.btnImportClick(Sender: TObject);
var
  i   : Integer;
  edl : TEDL;
  ATracks: TStrings = NIL;
begin
  with TOpenDialog.Create(Self) do
  try
    Title   := GetCommonString('OpenFile');
    Filter  := FillDialogFilter(True);
    Options := Options + [ofFileMustExist];

    if Execute then
    begin
      if FileName.ToLower.EndsWith('.edl') or FileName.ToLower.EndsWith('.xml') then
      begin
        edl := TEDL.Create('', '', GetFPS, AppOptions.FormatSettings);
        try
          if edl.GetXMLTrackList(FileName, ATracks) and (ATracks.Count > 1) then
            edl.ID := formCustomSelectDlg.ExecuteDialog(GetCommonString('MultipleTracksDetected'), GetCommonString('SelectTrackToUse'), ATracks);

          edl.LoadFromFile(FileName);
          mmoTimes.Lines.BeginUpdate;
          try
            mmoTimes.Clear;
            for i := 0 to edl.Items.Count-1 do
              with edl.Items[i]^ do
                if TypeOfTrack <> totAA then
                  mmoTimes.Lines.Add(edl.Items[i]^.ClipOut.ToString);
          finally
            mmoTimes.Lines.EndUpdate;
          end;
          cboTimeCodeImport.ItemIndex := tcMilliseconds;
        finally
          edl.Free;
          if Assigned(ATracks) then ATracks.Free;
          FSMPTE := True;
        end;
      end
      else
      begin
        mmoTimes.Lines.LoadFromFile(FileName);
        if mmoTimes.Lines.Count > 0 then
        begin
          FSMPTE := False;

          if StrToIntDef(mmoTimes.Lines[0], 0) > 0 then
            cboTimeCodeImport.ItemIndex := tcMilliseconds
          else if TimeInFormat(mmoTimes.Lines[0], 'hh:mm:ss.zzz') then
            cboTimeCodeImport.ItemIndex := tcHHMMSSZZZ;
        end;
      end;
    end;
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmShotChanges.btnExportClick(Sender: TObject);
var
  i, x : Integer;
  s    : String;
  edl  : TEDL;
  sl   : TStringList;
begin
  if mmoTimes.Lines.Count > 0 then
    with TSaveDialog.Create(frmMain) do
    try
      Title    := GetCommonString('SaveFile');
      Filter   := FillDialogFilter(False);
      FileName := '';
      Options  := [ofOverwritePrompt, ofEnableSizing];
      if Execute then
      begin
        if FileName.ToLower.EndsWith('.edl') or FileName.ToLower.EndsWith('.xml') then
        begin
          edl := TEDL.Create(FileName, frmMain.MPV.FileName, GetFPS, AppOptions.FormatSettings);
          try
            x := 0;
            for i := 0 to mmoTimes.Lines.Count-1 do
            begin
              edl.AddItem(i+1, '',  totV, tocC, x, mmoTimes.Lines[i].ToInteger, 0, 0, '', '');
              x := mmoTimes.Lines[i].ToInteger;
            end;
            edl.SaveToFile(FileName);
          finally
            edl.Free;
          end;
        end
        else
        begin
          //mmoTimes.Lines.SaveToFile(FileName);
          sl := TStringList.Create;
          try
            for i := 0 to mmoTimes.Lines.Count-1 do
            begin
              if IsTimeCode(tcFrames) then
                x := FramesToTime(StrToIntDef(mmoTimes.Lines[i], 0), GetFPS)
              else if IsTimeCode(tcSeconds) then
                x := StrToIntDef(mmoTimes.Lines[i], 0) * 1000
              else if IsTimeCode(tcMilliseconds) then
                x := StrToIntDef(mmoTimes.Lines[i], 0)
              else if IsTimeCode(tcHHMMSSFF) then
                x := StringToTime(mmoTimes.Lines[i], False, GetFPS)
              else
                x := StringToTime(mmoTimes.Lines[i]);

              if IsTimeCodeOut(tcFrames) then
                s := TimeToFrames(x, GetFPS).ToString
              else if IsTimeCodeOut(tcSeconds) then
                s := (x div 1000).ToString
              else if IsTimeCodeOut(tcMilliseconds) then
                s := x.ToString
              else if IsTimeCodeOut(tcHHMMSSFF) then
                s := TimeToString(x, 'hh:mm:ss:ff', GetFPS)
              else
                s := TimeToString(x, 'hh:mm:ss.zzz');

              sl.Add(s);
            end;
            sl.SaveToFile(FileName);
          finally
            sl.Free;
          end;
        end;
      end;
    finally
      Free;
    end;
end;

// -----------------------------------------------------------------------------

procedure TfrmShotChanges.SetControlsEnabled(const AValue: Boolean);
begin
  lblIdle.Caption := '';
  prbExtracting.Visible := not AValue;
  lblIdle.Visible := not AValue;
  mmoTimes.Enabled := AValue;
  btnImport.Enabled := AValue;
  btnExport.Enabled := AValue;
  btnDetect.Enabled := AValue;
  cboDetectApp.Enabled := AValue;
  cboTimeCodeImport.Enabled := AValue;
  cboTimeCodeExport.Enabled := AValue;
  btnApply.Enabled := AValue;
  spnSensitivity.Enabled := AValue;
  cboOffset.Enabled := AValue;
  tedOffset.Enabled := AValue;

  if AValue then
  begin
    btnClose.Caption := GetCommonString('btnClose', 'CommonControls');
    btnClose.Tag := 0;
  end
  else
  begin
    btnClose.Caption := GetCommonString('btnCancel', 'CommonControls');
    btnClose.Tag := 1;
  end;
end;

// -----------------------------------------------------------------------------

procedure RunAppTime_CB(const TimeElapsed: Double; var Cancel: Boolean);
begin
  frmShotChanges.lblIdle.Caption := TimeToString(Trunc(TimeElapsed)*1000, 'mm:ss');
  Cancel := frmShotChanges.FCancel;
end;

// -----------------------------------------------------------------------------

procedure TfrmShotChanges.btnDetectClick(Sender: TObject);
const
  sc_timeid = 'pts_time:';

var
  AParamArray: TStringArray;
  i, x, sc: Integer;
  sl: TStringList = NIL;
  outfile: String;
begin
  if frmMain.MPV.FileName.StartsWith('http') then
  begin
    ShowErrorMessageDialog(GetCommonString('FeatureNotAvailableFromURL'));
    Exit;
  end;

  FCancel := False;
  SetControlsEnabled(False);
  try
    if cboDetectApp.ItemIndex = 0 then // FFmpeg
    begin
      AParamArray := FFMPEG_SCParams.Split(' ');
      for i := 0 to High(AParamArray) do
        AParamArray[i] := StringsReplace(AParamArray[i], ['%input', '%value'], [frmMain.MPV.FileName, FloatToStr(spnSensitivity.Value, AppOptions.FormatSettings)], []);

      if not FileExists(GetExtractAppFile) then
      begin
        ShowErrorMessageDialog(Format(GetCommonString('ExtractAppError'), [FFMPEG_EXE]))
      end
      else
      begin
        FTimeElapsed := 0;

        if ExecuteAppLoop(GetExtractAppFile, AParamArray, sl, @RunApp_CB) and (sl.Count > 0) then
        try
          mmoTimes.Lines.BeginUpdate;
          mmoTimes.Clear;
          for i := 0 to sl.Count-1 do
          begin
            x := Pos(sc_timeid, sl[i]);
            if x > 0 then
            begin
              sc := Round(StrToSingle(Copy(sl[i], x+sc_timeid.Length, Pos(' ', sl[i], x+1)-x-sc_timeid.Length).Replace(',', '.'), 0, AppOptions.FormatSettings)*1000);
              if sc > 0 then
                mmoTimes.Lines.Add(IntToStr(sc));
            end;
          end;
        finally
          mmoTimes.Lines.EndUpdate;
          if Assigned(sl) then sl.Free;
        end;
      end;
    end
    else // PySceneDetect
    begin
      outfile := ChangeFileExt(GetTempFileName, '.csv');
      AParamArray := SCENEDETECT_SCParams.Split(' ');
      for i := 0 to High(AParamArray) do
        AParamArray[i] := StringsReplace(AParamArray[i], ['%input', '%output', '%value'], [frmMain.MPV.FileName, outfile, FloatToStr(spnSensitivity.Value, AppOptions.FormatSettings)], []);

      if not FileExists(GetExtractAppFile(False)) then
      begin
        ShowErrorMessageDialog(Format(GetCommonString('ExtractAppError'), [SCENEDETECT_EXE]))
      end
      else
      begin
        FTimeElapsed := 0;

        if ExecuteApp(GetExtractAppFile(False), AParamArray, True, True, @RunAppTime_CB) then
        begin
          if FileExists(outfile) then
          begin
            sl := TStringList.Create;
            try
              sl.LoadFromFile(outfile);
              if sl.Count > 2 then
                for i := 0 to 1 do
                  sl.Delete(i);

              mmoTimes.Lines.BeginUpdate;
              mmoTimes.Clear;
              for i := 0 to sl.Count-1 do
              begin
                AParamArray := sl[i].Split(',');
                sc := StringToTime(AParamArray[2]);
                if sc > 0 then
                  mmoTimes.Lines.Add(sc.ToString);
              end;
              DeleteFile(outfile);
            finally
              mmoTimes.Lines.EndUpdate;
              if Assigned(sl) then sl.Free;
            end;
          end;
        end;
      end;
    end;
  finally
    SetControlsEnabled(True);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmShotChanges.RunApp_CB(Sender, Context: TObject; Status: TRunCommandEventCode; const Message: String);
begin
  lblIdle.Caption := TimeToString(Trunc(FTimeElapsed)*1000, 'mm:ss');
  Sleep(100);
  FTimeElapsed += 0.1;

  if FCancel then
    TProcess(Sender).Terminate(-1);

  Application.ProcessMessages;
end;

// -----------------------------------------------------------------------------

function TfrmShotChanges.IsTimeCode(const ACode: Integer): Boolean;
begin
  Result := cboTimeCodeImport.ItemIndex = ACode;
end;

// -----------------------------------------------------------------------------

function TfrmShotChanges.IsTimeCodeOut(const ACode: Integer): Boolean;
begin
  Result := cboTimeCodeExport.ItemIndex = ACode;
end;

// -----------------------------------------------------------------------------

function TfrmShotChanges.FillDialogFilter(const AAllSupportedFiles: Boolean): String;
var
  i: Integer;
  s: String;
begin
  Result := '';
  s := '';

  for i := 0 to Length(TShotChangeExts)-1 do
  begin
    Result := Result + GetCommonString(Copy(TShotChangeExts[i], 2), 'ShotChangesStrings') + ' (*' + TShotChangeExts[i] + ')|*' + TShotChangeExts[i] + '|';
    s := s + '*' + TShotChangeExts[i] + ';';
  end;

  if AAllSupportedFiles then
  begin
    System.Delete(s, Length(s), 1);
    Result := GetCommonString('AllSupportedFiles') + ' (' + s + ')|' + s + '|' + Result;
  end;
end;

// -----------------------------------------------------------------------------

end.

