{*
 *  URUWorks Lazarus TextToSpeech
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
 *  Copyright (C) 2001-2024 URUWorks, uruworks@gmail.com.
 *}

unit UWSystem.TextToSpeech;

// -----------------------------------------------------------------------------

interface

uses
  Classes, SysUtils, StrUtils, FGL, fphttpclient, opensslsockets;

type

  { TJobInfo }

  PJobInfo = ^TJobInfo;
  TJobInfo = record
    Text,
    VoiceID,
    FileName : String;
    VoiceIndex : Integer;
    Stability,
    Similarity,
    Style : Single;
    Boost : Boolean;
  end;

  TJobList = specialize TFPGList<PJobInfo>;

  { TVoiceInfo }

  PVoiceInfo = ^TVoiceInfo;
  TVoiceInfo = record
    // Basic
    ID,
    Name,
    Preview_URL,
    // Extra
    Accent,
    Description,
    Age,
    Gender,
    Use_case : String;
  end;

  TVoiceList = specialize TFPGList<PVoiceInfo>;

  { TModel }

  TModel = (emMultilingual_v1, emMultilingual_v2);

  { TOutput_format }

  TOutput_format =
  (
    mp3_22050_32,
    mp3_44100_32,
    mp3_44100_64,
    mp3_44100_96,
    mp3_44100_128,
    mp3_44100_192
  );

  { TTextToSpeech }

  TOnJobsError = procedure(Sender: TObject; const ErrorDesc: String; const ErrorID, CurrentJob: Integer; var Cancel: Boolean) of object;
  TOnJobsProgress = procedure(Sender: TObject; const Index, Total: Integer) of object;

  TTextToSpeech = class(TThread)
  private
    { Private declarations }
    FFPHTTPClient : TFPHTTPClient;
    FJobs : TJobList;
    FVoices : TVoiceList;
    Fxi_api_key : String;
    FModel : TModel;
    FCurrentJob : Integer;
    FError : Integer;
    FErrorDesc : String;
    FCancel : Boolean;
    FOnJobsError : TOnJobsError;
    FOnJobsProgress : TOnJobsProgress;
    function ParseVoices(const JSON: String): Boolean;
    function GetVoices(out Voices: TStringList): Boolean;
    procedure DoOnJobsError;
    procedure DoOnJobsProgress;
  protected
    { Protected declarations }
    procedure Execute; override;
  public
    { Public declarations }
    constructor Create(const xi_api_key: String = '');
    destructor Destroy; override;
    function GetVoices: Boolean; overload;
    function Make(const Text, FileName, voice_id: String; const similarity_boost: Single = 0.75; const stability: Single = 0.5; const style: Single = 0; const boost: Boolean = True; const output_format: TOutput_format = mp3_44100_128): Boolean;
    function Make(const Text, FileName: String; const voice_info: TVoiceInfo; const similarity_boost: Single = 0.75; const stability: Single = 0.5; const style: Single = 0; const boost: Boolean = True; const output_format: TOutput_format = mp3_44100_128): Boolean; overload;
    function Make(const Text, FileName: String; const voice_index: Integer; const similarity_boost: Single = 0.75; const stability: Single = 0.5; const style: Single = 0; const boost: Boolean = True; const output_format: TOutput_format = mp3_44100_128): Boolean; overload;
    procedure CancelJobs;
    procedure DoJobs;
    property api_key : String read Fxi_api_key write Fxi_api_key;
    property Model : TModel read FModel write FModel;
    property Jobs : TJobList read FJobs write FJobs;
    property Voices : TVoiceList read FVoices;
  public
    { Public declarations }
    property OnJobsError: TOnJobsError read FOnJobsError write FOnJobsError;
    property OnJobsProgress: TOnJobsProgress read FOnJobsProgress write FOnJobsProgress;
  end;

const
  JOB_ERROR_InvalidDir = -1;
  JOB_ERROR_TTS = -2;
  JOB_ERROR_QuotaExceeded = -3;
  JOB_ERROR_Exception = -4;
  JOB_ERROR_Uknown = -5;

// -----------------------------------------------------------------------------

implementation

uses
  fpjson, jsonparser, Math, UWSystem.SysUtils, UWSystem.StrUtils;

const
  elevenlabs_url_Voices = 'https://api.elevenlabs.io/v1/voices';
  elevenlabs_url_TextToSpeech = 'https://api.elevenlabs.io/v1/text-to-speech/';

  TModel_str : array[0..1] of String =
  (
    'eleven_multilingual_v1', // recommended.
    'eleven_multilingual_v2'
  );

  TOutput_format_str : array[0..5] of String =
  (
    'mp3_22050_32',  // mp3 with 22.05kHz sample rate at 32kbps
    'mp3_44100_32',  // mp3 with 44.1kHz sample rate at 32kbps
    'mp3_44100_64',  // mp3 with 44.1kHz sample rate at 64kbps.
    'mp3_44100_96',  // mp3 with 44.1kHz sample rate at 96kbps.
    'mp3_44100_128', // mp3 with 44.1kHz sample rate at 128kbps. Default.
    'mp3_44100_192'  // mp3 with 44.1kHz sample rate at 192kbps.
  );

//------------------------------------------------------------------------------

{ Helpers}

// -----------------------------------------------------------------------------

function ListCompare(const Item1, Item2: PVoiceInfo): Integer;
begin
  if Item1^.Name < Item2^.Name then
    Result := -1
  else if Item1^.Name > Item2^.Name then
    Result := 1
  else
    Result := 0;
end;

// -----------------------------------------------------------------------------

{ TTextToSpeech }

// -----------------------------------------------------------------------------

constructor TTextToSpeech.Create(const xi_api_key: String = '');
begin
  inherited Create(True);
  FreeOnTerminate := False;

  FFPHTTPClient := TFPHTTPClient.Create(NIL);
  FJobs := TJobList.Create;
  FVoices := TVoiceList.Create;
  Fxi_api_key := xi_api_key;
  FModel := emMultilingual_v2;
  FError := 0;
  FErrorDesc := '';
  FCancel := False;
end;

// -----------------------------------------------------------------------------

destructor TTextToSpeech.Destroy;
begin
  FFPHTTPClient.Free;
  FJobs.Free;
  FVoices.Free;

  inherited Destroy;
end;

// -----------------------------------------------------------------------------

function TTextToSpeech.ParseVoices(const JSON: String): Boolean;
var
  i : Integer;
  Parser : TJSONParser;
  Data : TJSONData;
  joData,
  joItem,
  joItem2 : TJSONObject;
  jaData,
  ZeroArray : TJSONArray;
  Info : PVoiceInfo;
begin
  Result := False;
  FVoices.Clear;

  if JSON.IsEmpty then Exit;

  Parser := TJSONParser.Create(JSON);
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

    jaData := joData.Get('voices', ZeroArray);
    for i := 0 to jaData.Count-1 do
    begin
      joItem := jaData.Objects[i];

      New(Info);
      // Basic
      Info^.ID          := joItem.Get('voice_id', '');    //21m00Tcm4TlvDq8ikWAM
      Info^.Name        := joItem.Get('name', '');        //Rachel
      Info^.Preview_URL := joItem.Get('preview_url', ''); //https://storage.googleapis.com/eleven-public-prod/premade/voices/21m00Tcm4TlvDq8ikWAM/df6788f9-5c96-470d-8312-aab3b3d8f50a.mp3
      // Extra
      joItem2 := joitem.Objects['labels'];
      if Assigned(joItem2) then
      begin
        Info^.Accent      := joItem2.Get('accent', '');      //american
        Info^.Description := joItem2.Get('description', ''); //calm
        Info^.Age         := joItem2.Get('age', '');         //young
        Info^.Gender      := joItem2.Get('gender', '');      //female
        Info^.Use_case    := joItem2.Get('use case', '');    //narration
      end;
      FVoices.Add(Info);
    end;

    Result := FVoices.Count > 0;
    if Result then FVoices.Sort(@ListCompare);
  finally
    ZeroArray.Free;
    FreeAndNil(Parser);
  end;
end;

// -----------------------------------------------------------------------------

function TTextToSpeech.GetVoices(out Voices: TStringList): Boolean;
begin
  Result := False;
  if not Assigned(Voices) then
    Voices := TStringList.Create;

  Voices.Clear;
  if not Fxi_api_key.IsEmpty then
    with FFPHTTPClient do
    begin
      RequestHeaders.Clear;
      AddHeader('Accept', 'application/json');
      AddHeader('Content-Type', 'application/json');
      AddHeader('xi-api-key', Fxi_api_key);
      AllowRedirect := True;
      try
        Get(elevenlabs_url_Voices, Voices);
        Result := Voices.Count > 0;
      except
      end;
    end;
end;

// -----------------------------------------------------------------------------

function TTextToSpeech.GetVoices: Boolean;
var
  sl: TStringList = NIL;
begin
  Result := GetVoices(sl) and ParseVoices(sl.Text);
  if Assigned(sl) then sl.Free;
end;

// -----------------------------------------------------------------------------

function TTextToSpeech.Make(const Text, FileName, voice_id: String; const similarity_boost: Single = 0.75; const stability: Single = 0.5; const style: Single = 0; const boost: Boolean = True; const output_format: TOutput_format = mp3_44100_128): Boolean;
var
  url, sim, sta, sty, boo : String;
  PostData : TStringStream;
  GetData : TMemoryStream;
begin
  Result := False;

  with FFPHTTPClient do
  begin
    RequestHeaders.Clear;
    AddHeader('Accept', 'audio/mpeg');
    AddHeader('Content-Type', 'application/json');
    AddHeader('xi-api-key', Fxi_api_key);
    AllowRedirect := True;

    sim := SingleToStr(similarity_boost, '.', '0.#');
    sta := SingleToStr(stability, '.', '0.#');
    sty := SingleToStr(style, '.', '0.#');
    boo := iff(boost, 'true', 'false');

    PostData := TStringStream.Create(
      '{"text":"' + StringToJSONString(StringReplace(Text.Trim, sLineBreak, '. ', [rfReplaceAll])) +
      '","model_id":"' + TModel_str[Integer(FModel)] +
      '","voice_settings":{"stability":' + sta +
      ',"similarity_boost":' + sim +
      ',"style":' + sty +
      ',"use_speaker_boost":' + boo + '}}'
      , TEncoding.UTF8);
    try
      RequestBody := PostData;
      GetData := TMemoryStream.Create;
      try
        url := elevenlabs_url_TextToSpeech + voice_id;
        if output_format <> mp3_44100_128 then
          url := url + '?output_format=' + TOutput_format_str[Integer(output_format)];

        Post(url, GetData);
        if (ResponseStatusCode = 200) and (GetData.Size > 0) then
        begin
          if FileExists(FileName) then
            DeleteFile(FileName);

          GetData.SaveToFile(FileName);
          Result := True;
        end
        else
        begin
          with TStringStream.Create do
          try
            LoadFromStream(GetData);
            FErrorDesc := DataString;
          finally
            Free;
          end;

          if FErrorDesc.Contains('quota_exceeded') then
            FError := JOB_ERROR_QuotaExceeded
          else if (ResponseStatusCode = 422) then
            FError := JOB_ERROR_TTS
          else
            FError := JOB_ERROR_Uknown;

          GetData.SaveToFile(ChangeFileExt(FileName, '.log'));
        end;
      except
        on E: Exception do
        begin
          FError := JOB_ERROR_Exception;
          FErrorDesc := E.Message;
        end;
      end;
    finally
      PostData.Free;
      GetData.Free;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function TTextToSpeech.Make(const Text, FileName: String; const voice_info: TVoiceInfo; const similarity_boost: Single = 0.75; const stability: Single = 0.5; const style: Single = 0; const boost: Boolean = True; const output_format: TOutput_format = mp3_44100_128): Boolean;
begin
  Result := Make(Text, FileName, voice_info.ID, similarity_boost, stability, style, boost, output_format);
end;

// -----------------------------------------------------------------------------

function TTextToSpeech.Make(const Text, FileName: String; const voice_index: Integer; const similarity_boost: Single = 0.75; const stability: Single = 0.5; const style: Single = 0; const boost: Boolean = True; const output_format: TOutput_format = mp3_44100_128): Boolean; overload;
begin
  Result := False;
  if (FVoices.Count > 0) and InRange(voice_index, 0, FVoices.Count-1) then
    Result := Make(Text, FileName, FVoices[voice_index]^.ID, similarity_boost, stability, style, boost, output_format);
end;

// -----------------------------------------------------------------------------

procedure TTextToSpeech.CancelJobs;
begin
  if Assigned(FFPHTTPClient) then
    FFPHTTPClient.Terminate;
end;

// -----------------------------------------------------------------------------

procedure TTextToSpeech.Execute;
var
  Dir : String;
  i : Integer;
begin
  for i := 0 to FJobs.Count-1 do
  begin
    FCurrentJob := i;
    Dir := ExtractFileDir(FJobs[i]^.FileName);
    if not DirectoryExists(Dir) and not CreateDir(Dir) then
    begin
      FError := JOB_ERROR_InvalidDir;
      Synchronize(@DoOnJobsError)
    end
    else
    begin
      try
        if not Make(FJobs[i]^.Text, FJobs[i]^.FileName, FJobs[i]^.VoiceID, FJobs[i]^.Similarity, FJobs[i]^.Stability) then
          Synchronize(@DoOnJobsError);

        Synchronize(@DoOnJobsProgress);
      except
        on E: Exception do
        begin
          FError := JOB_ERROR_Exception;
          FErrorDesc := E.Message;
          Synchronize(@DoOnJobsError);
        end;
      end;
    end;
    if FCancel then Exit;
  end;
end;

// -----------------------------------------------------------------------------

procedure TTextToSpeech.DoOnJobsError;
begin
  if Assigned(FOnJobsError) then
    FOnJobsError(Self, FErrorDesc, FError, FCurrentJob+1, FCancel);
end;

// -----------------------------------------------------------------------------

procedure TTextToSpeech.DoOnJobsProgress;
begin
  if Assigned(FOnJobsProgress) then
    FOnJobsProgress(Self, FCurrentJob+1, FJobs.Count);
end;

// -----------------------------------------------------------------------------

procedure TTextToSpeech.DoJobs;
begin
  if FJobs.Count > 0 then
    Start;
end;

// -----------------------------------------------------------------------------

end.
