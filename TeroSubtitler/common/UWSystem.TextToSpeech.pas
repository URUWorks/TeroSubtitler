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

  TOnJobsError = procedure(Sender: TObject; const Error, CurrentJob: Integer) of object;
  TOnJobsProgress = procedure(Sender: TObject; const Index, Total: Integer) of object;

  TTextToSpeech = class(TThread)
  private
    { Private declarations }
    FFPHTTPClient: TFPHTTPClient;
    FJobs : TJobList;
    FVoices : TVoiceList;
    Fxi_api_key : String;
    FCurrentJob : Integer;
    FError      : Integer;
    FOnJobsError: TOnJobsError;
    FOnJobsProgress: TOnJobsProgress;
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
    function Make(const Text, FileName, voice_id: String; const output_format: TOutput_format = mp3_44100_128): Boolean;
    function Make(const Text, FileName: String; const voice_info: TVoiceInfo; const output_format: TOutput_format = mp3_44100_128): Boolean; overload;
    function Make(const Text, FileName: String; const voice_index: Integer; const output_format: TOutput_format = mp3_44100_128): Boolean; overload;
    procedure CancelJobs;
    procedure DoJobs;
    property api_key : String read Fxi_api_key write Fxi_api_key;
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
  JOB_ERROR_Exception = -3;

// -----------------------------------------------------------------------------

implementation

uses
  fpjson, jsonparser, Math;

const
  elevenlabs_url_Voices = 'https://api.elevenlabs.io/v1/voices';
  elevenlabs_url_TextToSpeech = 'https://api.elevenlabs.io/v1/text-to-speech/';

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
  FError := 0;
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
  if JSON.IsEmpty then Exit;
  FVoices.Clear;

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

function TTextToSpeech.Make(const Text, FileName, voice_id: String; const output_format: TOutput_format = mp3_44100_128): Boolean;
var
  url : String;
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

    PostData := TStringStream.Create(
      '{"text":"' + StringReplace(Text.Trim, sLineBreak, '. ', [rfReplaceAll]) +
      '","model_id":"eleven_multilingual_v1"}' //'","model_id":"eleven_multilingual_v1","voice_settings":{"stability":0.5,"similarity_boost":0.5}}'
      , TEncoding.UTF8);
    try
      RequestBody := PostData;
      GetData := TMemoryStream.Create;
      try
        url := elevenlabs_url_TextToSpeech + voice_id;
        if output_format <> mp3_44100_128 then
          url := url + '?output_format=' + TOutput_format_str[Integer(output_format)];

        Post(url, GetData);
        if (ResponseStatusCode = 200) and (GetData.Size > 0)then
        begin
          GetData.SaveToFile(FileName);
          Result := True;
        end;
      except
      end;
    finally
      PostData.Free;
      GetData.Free;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function TTextToSpeech.Make(const Text, FileName: String; const voice_info: TVoiceInfo; const output_format: TOutput_format = mp3_44100_128): Boolean;
begin
  Result := Make(Text, FileName, voice_info.ID, output_format);
end;

// -----------------------------------------------------------------------------

function TTextToSpeech.Make(const Text, FileName: String; const voice_index: Integer; const output_format: TOutput_format = mp3_44100_128): Boolean; overload;
begin
  Result := False;
  if (FVoices.Count > 0) and InRange(voice_index, 0, FVoices.Count-1) then
    Result := Make(Text, FileName, FVoices[voice_index]^.ID, output_format);
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
        if not Make(FJobs[i]^.Text, FJobs[i]^.FileName, FJobs[i]^.VoiceID) then
        begin
          FError := JOB_ERROR_TTS;
          Synchronize(@DoOnJobsError);
        end;

        Synchronize(@DoOnJobsProgress);
      except
        on E: Exception do
        begin
          FError := JOB_ERROR_Exception;
          Synchronize(@DoOnJobsError);
        end;
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TTextToSpeech.DoOnJobsError;
begin
  if Assigned(FOnJobsError) then
    FOnJobsError(Self, FError, FCurrentJob);
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
