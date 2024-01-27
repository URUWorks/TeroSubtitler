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

unit procGenerateVideo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, StdCtrls, SysUtils, StrUtils, FileUtil, UWSystem.ThreadProcess;

type

  TFFmpegEncoderInfoS = record
    Name,
    Value: String;
  end;

  TFFmpegEncoderInfoI = record
    Name: String;
    Value: Integer;
  end;

const

  TFFVideoEncoders: array[0..3] of TFFmpegEncoderInfoS =
    (
      (Name: 'H.264'; Value: '.mp4;.mkv'),
      (Name: 'H.265/HEVC'; Value: '.mp4;.mkv'),
      (Name: 'VP9'; Value: '.webm;.mp4;.mkv'),
      (Name: 'Apple ProRes'; Value: '.mov')
    );

  TFFVideoH264Subtype: array[0..2] of TFFmpegEncoderInfoS =
    (
      (Name: 'H.264'; Value: 'libx264'),
      (Name: 'NVIDIA NVENC'; Value: 'h264_nvenc'),
      (Name: 'AMD AMF'; Value: 'h264_amf')
    );

  TFFVideoH265Subtype: array[0..2] of TFFmpegEncoderInfoS =
    (
      (Name: 'H.265/HEVC'; Value: 'libx265'),
      (Name: 'NVIDIA NVENC'; Value: 'hevc_nvenc'),
      (Name: 'AMD AMF'; Value: 'hevc_amf')
    );

  TFFVideoVP9Subtype: array[0..0] of TFFmpegEncoderInfoS =
    (
      (Name: 'VP9'; Value: 'libvpx-vp9')
    );

  TFFVideoProResSubtype: array[0..0] of TFFmpegEncoderInfoS =
    (
      (Name: 'Apple ProRes'; Value: 'prores_ks')
    );

  TFFAudioEncoders: array[0..3] of TFFmpegEncoderInfoS =
    (
      (Name: 'AAC'; Value: 'aac'),
      (Name: 'FLAC'; Value: 'flac'),
      (Name: 'ALAC'; Value: 'alac'),
      (Name: 'Opus'; Value: 'libopus')
    );

  TFFAudioProResEncoders: array[0..3] of TFFmpegEncoderInfoS =
    (
      (Name: 'PCM 16-bit'; Value: 'pcm_s16le'),
      (Name: 'PCM 24-bit'; Value: 'pcm_s24le'),
      (Name: 'PCM 32-bit'; Value: 'pcm_s32le'),
      (Name: 'AAC'; Value: 'aac')
    );

  TFFAudioSampleRate: array[0..9] of TFFmpegEncoderInfoI =
    (
      (Name: '8000 Hz'; Value: 8000),
      (Name: '12000 Hz'; Value: 12000),
      (Name: '16000 Hz'; Value: 16000),
      (Name: '22050 Hz'; Value: 22050),
      (Name: '24000 Hz'; Value: 24000),
      (Name: '32000 Hz'; Value: 32000),
      (Name: '44100 Hz'; Value: 44100),
      (Name: '48000 Hz'; Value: 48000),
      (Name: '88200 Hz'; Value: 88200),
      (Name: '96000 Hz'; Value: 96000)
//      (Name: '192000 Hz'; Value: 192000)
    );

  TFFAudioBitRate: array[0..4] of String =
    (
      '64k', '128k', '160k', '196k', '320k'
    );

  TFFAudioChannels: array[0..4] of TFFmpegEncoderInfoI =
    (
      (Name: 'Mono'; Value: 1),
      (Name: 'Stereo'; Value: 2),
      (Name: '2.1'; Value: 3),
      (Name: '5.1'; Value: 6),
      (Name: '7.1'; Value: 8)
    );

  TFFFormats: array[0..3] of TFFmpegEncoderInfoS =
    (
      (Name: 'MP4'; Value: '.mp4'),
      (Name: 'Matroska'; Value: '.mkv'),
      (Name: 'WebM'; Value: '.webm'),
      (Name: 'MOV'; Value: '.mov')
    );

  TFFProResProfile: array[0..5] of String =
    (
      '422 Proxy', '422 LT', '422', '422 HQ', '4444', '4444 XQ'
    );

  TFFRotate: array[0..2] of TFFmpegEncoderInfoS =
    (
      (Name: '90'; Value: 'transpose=1'),
      (Name: '-90'; Value: 'transpose=2'),
      (Name: '180'; Value: 'transpose=1,transpose=1')
    );

procedure FillComboWithVideoEncoders(const Combo: TComboBox; const AFormat: Integer);
procedure FillComboWithVideoSubtypes(const Combo: TComboBox; const AEncoderIndex: Integer);
procedure FillComboWithVideoProfileProRes(const Combo: TComboBox);
procedure FillComboWithAudioEncoders(const Combo: TComboBox; const AProRes: Boolean = False);
procedure FillComboWithAudioChannels(const Combo: TComboBox);
procedure FillComboWithAudioSampleRate(const Combo: TComboBox);
procedure FillComboWithAudioBitRate(const Combo: TComboBox);
procedure FillComboWithFormats(const Combo: TComboBox);
procedure FillComboWithRotate(const Combo: TComboBox);

function GenerateVideoWithSubtitle(AVideoFileName, ASubtitleFileName, AOutputVideoFileName: String; AWidth, AHeight: Integer; AVideoCodec: String; AVideoProfile: Integer = -1; AExtra: String = ''; ACutFrom: Integer = -1; ACutTo: Integer = -1; AStyle: String = ''; AAudioCodec: String = ''; AAudioChannels: Integer = 2; AAudioSampleRate: Integer = 44100; AAudioBitRate: String = ''; const ACB: TOnDataReceived = NIL): Boolean;
function GenerateBlankVideo(const AOutputVideoFileName: String; const AWidth, AHeight: Integer; const AFPS: Single; const ADuration: Integer; const AColor: Integer; const AImageFile: String; const ASMPTEBars: Boolean; const ASolidColor: Boolean; const AImage: Boolean; const AGenerateTC: Boolean; const AGenerateTone: Boolean; const ACB: TOnDataReceived = NIL): Boolean;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, procWorkspace, UWSystem.TimeUtils, UWSystem.SysUtils;

// -----------------------------------------------------------------------------

procedure FillComboWithArrayType(const Combo: TComboBox; const AType: array of TFFmpegEncoderInfoS; const AIndex: Integer = 0);
var
  i: Byte;
begin
  with Combo do
  begin
    Items.BeginUpdate;
    Clear;
    for i := 0 to High(AType) do
      Items.Add(AType[i].Name);

    ItemIndex := AIndex;
    Items.EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

procedure FillComboWithArrayType(const Combo: TComboBox; const AType: array of TFFmpegEncoderInfoI; const AIndex: Integer = 0);
var
  i: Byte;
begin
  with Combo do
  begin
    Items.BeginUpdate;
    Clear;
    for i := 0 to High(AType) do
      Items.Add(AType[i].Name);

    ItemIndex := AIndex;
    Items.EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

procedure FillComboWithArrayString(const Combo: TComboBox; const AArray: array of String; const AIndex: Integer = 0);
var
  i: Byte;
begin
  with Combo do
  begin
    Items.BeginUpdate;
    Clear;
    for i := 0 to High(AArray) do
      Items.Add(AArray[i]);

    ItemIndex := AIndex;
    Items.EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

procedure FillComboWithVideoEncoders(const Combo: TComboBox; const AFormat: Integer);
var
  i: Byte;
begin
  with Combo do
  begin
    Items.BeginUpdate;
    Clear;
    for i := 0 to High(TFFVideoEncoders) do
      if TFFVideoEncoders[i].Value.Contains(TFFFormats[AFormat].Value) then
        Items.Add(TFFVideoEncoders[i].Name);

    ItemIndex := 0;
    Items.EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

procedure FillComboWithVideoSubtypes(const Combo: TComboBox; const AEncoderIndex: Integer);
begin
  case AEncoderIndex of
    1: FillComboWithArrayType(Combo, TFFVideoH265Subtype);
    2: FillComboWithArrayType(Combo, TFFVideoVP9Subtype);
    3: FillComboWithArrayString(Combo, TFFProResProfile);
  else
    FillComboWithArrayType(Combo, TFFVideoH264Subtype);
  end;
end;

// -----------------------------------------------------------------------------

procedure FillComboWithVideoProfileProRes(const Combo: TComboBox);
begin
  FillComboWithArrayString(Combo, TFFProResProfile);
end;

// -----------------------------------------------------------------------------

procedure FillComboWithAudioEncoders(const Combo: TComboBox; const AProRes: Boolean = False);
begin
  if AProRes then
    FillComboWithArrayType(Combo, TFFAudioProResEncoders)
  else
    FillComboWithArrayType(Combo, TFFAudioEncoders);
end;

// -----------------------------------------------------------------------------

procedure FillComboWithAudioChannels(const Combo: TComboBox);
begin
  FillComboWithArrayType(Combo, TFFAudioChannels, 1);
end;

// -----------------------------------------------------------------------------

procedure FillComboWithAudioSampleRate(const Combo: TComboBox);
begin
  FillComboWithArrayType(Combo, TFFAudioSampleRate, 6);
end;

// -----------------------------------------------------------------------------

procedure FillComboWithAudioBitRate(const Combo: TComboBox);
begin
  FillComboWithArrayString(Combo, TFFAudioBitRate, 1);
end;

// -----------------------------------------------------------------------------

procedure FillComboWithFormats(const Combo: TComboBox);
begin
  FillComboWithArrayType(Combo, TFFFormats);
end;

// -----------------------------------------------------------------------------

procedure FillComboWithRotate(const Combo: TComboBox);
begin
  FillComboWithArrayType(Combo, TFFRotate);
end;

// -----------------------------------------------------------------------------

function GenerateVideoWithSubtitle(AVideoFileName, ASubtitleFileName, AOutputVideoFileName: String; AWidth, AHeight: Integer; AVideoCodec: String; AVideoProfile: Integer = -1; AExtra: String = ''; ACutFrom: Integer = -1; ACutTo: Integer = -1; AStyle: String = ''; AAudioCodec: String = ''; AAudioChannels: Integer = 2; AAudioSampleRate: Integer = 44100; AAudioBitRate: String = ''; const ACB: TOnDataReceived = NIL): Boolean;
var
  VideoSettings,
  AudioSettings, s : String;
  AParamArray : TStringArray;
  i : Integer;
begin
  Result := False;
  if AVideoFileName.IsEmpty or ASubtitleFileName.IsEmpty or AOutputVideoFileName.IsEmpty then Exit;

  if AVideoCodec.IsEmpty then
    AVideoCodec := 'libx264';

  VideoSettings := '-c:v ' + AVideoCodec;
  if AVideoCodec = 'libx265' then
    VideoSettings += ' -tag:v hvc1'
  else if (AVideoCodec = 'prores_ks') and (AVideoProfile <> -1) then
    VideoSettings += ' -profile:v ' + AVideoProfile.ToString;

  if (ACutFrom > -1) and (ACutTo > -1) then
    VideoSettings += Format(' -ss %s -to %s', [TimeToString(ACutFrom, DefTimeFormat, GetFPS), TimeToString(ACutTo, DefTimeFormat, GetFPS)]);

  if AAudioCodec.IsEmpty then
    AudioSettings := ''
  else
  begin
    AudioSettings := '-c:a ' + AAudioCodec + ' -ac ' + AAudioChannels.ToString + ' -ar ' + AAudioSampleRate.ToString;
    if not AAudioBitRate.IsEmpty then
      AudioSettings += ' -b:a ' + AAudioBitRate;
  end;

  if AStyle.IsEmpty then
    AStyle := 'Fontname=Verdana,Fontsize=20,Alignment=2,PrimaryColour=&H00FFFFFF,BackColour=&H00000000,Outline=0,Shadow=0,MarginV=20';

  s := StringsReplace(FFMPEG_VideoEncoding,
    ['%width', '%height', '%videosettings', '%audiosettings', '%style'],
    [AWidth.ToString, AHeight.ToString, VideoSettings, AudioSettings, AStyle], [rfReplaceAll]);

  ASubtitleFileName := StringsReplace(ASubtitleFileName, ['\', ':'], ['\\', '\:'], [rfReplaceAll]);

  AParamArray := s.Split(' ', TStringSplitOptions.ExcludeEmpty);
  try
    if FileExists(AOutputVideoFileName) then
      DeleteFile(AOutputVideoFileName);

    for i := 0 to High(AParamArray) do
      AParamArray[i] := StringsReplace(AParamArray[i],
        ['%input', '%extra', '%subtitle', '%output', '%%'],
        [AVideoFileName, AExtra, ASubtitleFileName, AOutputVideoFileName, ' '], [rfReplaceAll]);

    ExecuteThreadProcess(Tools.FFmpeg, AParamArray, ACB);

    Result := FileExists(AOutputVideoFileName) and (FileSize(AOutputVideoFileName) > 0);
  finally
    SetLength(AParamArray, 0);
  end;
end;

// -----------------------------------------------------------------------------

function GenerateBlankVideo(const AOutputVideoFileName: String; const AWidth, AHeight: Integer; const AFPS: Single; const ADuration: Integer; const AColor: Integer; const AImageFile: String; const ASMPTEBars: Boolean; const ASolidColor: Boolean; const AImage: Boolean; const AGenerateTC: Boolean; const AGenerateTone: Boolean; const ACB: TOnDataReceived = NIL): Boolean;
var
  s, DrawText, GenTone, fps, HexColor : String;
  AParamArray : TStringArray;
  i : Integer;
begin
  Result := False;
  if AOutputVideoFileName.IsEmpty then Exit;

  fps := SingleToStr(AFPS, '.');
  HexColor := IntToHexStr(AColor, False, '#');

  if AGenerateTC then
    DrawText := ' -vf drawtext="timecode=''00\:00\:00\:00'':r=' + fps + ':x=(w-text_w)/2:y=(h-text_h)/2:fontsize=48:fontcolor=white"'
  else
    DrawText := '';

  if AGenerateTone then
    GenTone := ' -f lavfi -i "sine=frequency=1000:sample_rate=48000"'
  else
    GenTone := '';

  if ASMPTEBars then
    s := '-t %Duration -f lavfi -i "smptehdbars=rate=%fps:size=%VideoWidthx%VideoHeight"%GenTone -c:v libx264 -tune stillimage -shortest -s %VideoWidthx%VideoHeight%DrawText "%OutputFileName"'
  else if AImage and not AImageFile.IsEmpty then
    s := '-t %Duration -loop 1 -r %fps -i "%ImageFile"%GenTone -c:v libx264 -tune stillimage -shortest -s %VideoWidthx%VideoHeight%DrawText "%OutputFileName"'
  else
    s := '-t %Duration -f lavfi -i "color=c=%Color:r=%fps:s=%VideoWidthx%VideoHeight"%GenTone -c:v libx264 -tune stillimage -shortest -s %VideoWidthx%VideoHeight%DrawText "%OutputFileName"';

  s := StringsReplace(s, ['%DrawText', '%GenTone'], [DrawText, GenTone], [rfReplaceAll]);

  {$IFNDEF WINDOWS}
  s := s.Replace('"', '', [rfReplaceAll]);
  {$ENDIF}

  AParamArray := s.Split(' ', TStringSplitOptions.ExcludeEmpty);
  try
    if FileExists(AOutputVideoFileName) then
      DeleteFile(AOutputVideoFileName);

    for i := 0 to High(AParamArray) do
      AParamArray[i] := StringsReplace(AParamArray[i],
        ['%Duration', '%VideoWidth', '%VideoHeight', '%fps', '%ImageFile', '%Color', '%OutputFileName'],
        [IntToStr(ADuration div 1000), AWidth.ToString, AHeight.ToString, fps, AImageFile, HexColor, AOutputVideoFileName], [rfReplaceAll]);

    ExecuteThreadProcess(Tools.FFmpeg, AParamArray, ACB);

    Result := FileExists(AOutputVideoFileName) and (FileSize(AOutputVideoFileName) > 0);
  finally
    SetLength(AParamArray, 0);
  end;
end;

// -----------------------------------------------------------------------------

end.

