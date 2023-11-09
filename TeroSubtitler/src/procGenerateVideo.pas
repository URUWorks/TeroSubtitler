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
  Classes, StdCtrls, SysUtils, StrUtils, FileUtil, UWSystem.Process;

type

  TFFmpegEncoderInfoS = record
    Name,
    Codec: String;
  end;

  TFFmpegEncoderInfoI = record
    Name: String;
    Value: Integer;
  end;

const

  TFFVideoEncoders: array[0..7] of TFFmpegEncoderInfoS =
    (
      (Name: 'H.264'; Codec: 'libx264'),
      (Name: 'H.264 (NVIDIA NVENC)'; Codec: 'h264_nvenc'),
      (Name: 'H.264 (AMD AMF)'; Codec: 'h264_amf'),
      (Name: 'H.265/HEVC'; Codec: 'libx265'),
      (Name: 'H.265/HEVC (NVIDIA NVENC)'; Codec: 'hevc_nvenc'),
      (Name: 'H.265/HEVC (AMD AMF)'; Codec: 'hevc_amf'),
      (Name: 'VP9'; Codec: 'libvpx-vp9'),
      (Name: 'Apple ProRes'; Codec: 'prores_ks')
    );

  TFFAudioEncoders: array[0..1] of TFFmpegEncoderInfoS =
    (
      (Name: 'AAC'; Codec: 'aac'),
      (Name: 'FLAC'; Codec: 'flac')
    );

  TFFAudioSampleRate: array[0..4] of TFFmpegEncoderInfoI =
    (
      (Name: '44100 Hz'; Value: 44100),
      (Name: '48000 Hz'; Value: 48000),
      (Name: '88200 Hz'; Value: 88200),
      (Name: '96000 Hz'; Value: 96000),
      (Name: '192000 Hz'; Value: 192000)
    );

  TFFAudioBitRate: array[0..4] of TFFmpegEncoderInfoI =
    (
      (Name: '64k'; Value: 64),
      (Name: '128k'; Value: 128),
      (Name: '160k'; Value: 160),
      (Name: '196k'; Value: 196),
      (Name: '320k'; Value: 320)
    );

  TFFProResProfile: array[0..5] of String =
    (
      'Proxy', 'LT', 'Standard', 'HQ', '4444', '4444 XQ'
    );

procedure FillComboWithVideoEncoders(const Combo: TComboBox);
procedure FillComboWithVideoProfileProRes(const Combo: TComboBox);
procedure FillComboWithAudioEncoders(const Combo: TComboBox);
procedure FillComboWithAudioSampleRate(const Combo: TComboBox);
procedure FillComboWithAudioBitRate(const Combo: TComboBox);

function GenerateVideoWithSubtitle(AVideoFileName, ASubtitleFileName, AOutputVideoFileName: String; AWidth, AHeight: Integer; AVideoCodec: String; AVideoProfile: Integer = -1; AStyle: String = ''; AAudioCodec: String = ''; AAudioSampleRate: Integer = 44100; AAudioBitRate: Integer = 128; const ACB: TUWProcessCB = NIL): Boolean;

// -----------------------------------------------------------------------------

implementation

uses procTypes;

// -----------------------------------------------------------------------------

procedure FillComboWithVideoEncoders(const Combo: TComboBox);
var
  i: Byte;
begin
  with Combo do
  begin
    Items.BeginUpdate;
    Clear;
    for i := 0 to High(TFFVideoEncoders) do
      Items.Add(TFFVideoEncoders[i].Name);

    ItemIndex := 0;
    Items.EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

procedure FillComboWithVideoProfileProRes(const Combo: TComboBox);
var
  i: Byte;
begin
  with Combo do
  begin
    Items.BeginUpdate;
    Clear;
    for i := 0 to High(TFFProResProfile) do
      Items.Add(TFFProResProfile[i]);

    ItemIndex := 0;
    Items.EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

procedure FillComboWithAudioEncoders(const Combo: TComboBox);
var
  i: Byte;
begin
  with Combo do
  begin
    Items.BeginUpdate;
    Clear;
    for i := 0 to High(TFFAudioEncoders) do
      Items.Add(TFFAudioEncoders[i].Name);

    ItemIndex := 0;
    Items.EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

procedure FillComboWithAudioSampleRate(const Combo: TComboBox);
var
  i: Byte;
begin
  with Combo do
  begin
    Items.BeginUpdate;
    Clear;
    for i := 0 to High(TFFAudioSampleRate) do
      Items.Add(TFFAudioSampleRate[i].Name);

    ItemIndex := 0;
    Items.EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

procedure FillComboWithAudioBitRate(const Combo: TComboBox);
var
  i: Byte;
begin
  with Combo do
  begin
    Items.BeginUpdate;
    Clear;
    for i := 0 to High(TFFAudioBitRate) do
      Items.Add(TFFAudioBitRate[i].Name);

    ItemIndex := 0;
    Items.EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

function GenerateVideoWithSubtitle(AVideoFileName, ASubtitleFileName, AOutputVideoFileName: String; AWidth, AHeight: Integer; AVideoCodec: String; AVideoProfile: Integer = -1; AStyle: String = ''; AAudioCodec: String = ''; AAudioSampleRate: Integer = 44100; AAudioBitRate: Integer = 128; const ACB: TUWProcessCB = NIL): Boolean;
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

  if AAudioCodec.IsEmpty then
    AudioSettings := ''
  else
    AudioSettings := '-c:a ' + AAudioCodec + ' -ar ' + AAudioSampleRate.ToString + ' -b:a ' + AAudioBitRate.ToString;

  if AStyle.IsEmpty then
    AStyle := 'Fontname=Verdana,Fontsize=20,Alignment=2,PrimaryColour=&H00FFFFFF,BackColour=&H00000000,Outline=0,Shadow=0,MarginV=20';

  s := StringsReplace(FFMPEG_VideoEncoding,
    ['%width', '%height', '%videosettings', '%audiosettings', '%style'],
    [AWidth.ToString, AHeight.ToString, VideoSettings, AudioSettings, AStyle], [rfReplaceAll]);

  ASubtitleFileName := StringsReplace(ASubtitleFileName, ['\', ':'], ['\\', '\:'], [rfReplaceAll]);

  AParamArray := s.Split(' ', TStringSplitOptions.ExcludeEmpty);
  try
    for i := 0 to High(AParamArray) do
      AParamArray[i] := StringsReplace(AParamArray[i],
        ['%input', '%subtitle', '%output', '%%'],
        [AVideoFileName, ASubtitleFileName, AOutputVideoFileName, ' '], [rfReplaceAll]);

    ExecuteApp(Tools.FFmpeg, AParamArray, True, True, ACB);

    Result := FileExists(AOutputVideoFileName) and (FileSize(AOutputVideoFileName) > 0);
  finally
    SetLength(AParamArray, 0);
  end;
end;

// -----------------------------------------------------------------------------

end.

