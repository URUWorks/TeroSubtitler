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
  Classes, SysUtils, StrUtils, UWSystem.Process;

function GenerateVideoWithSubtitle(AVideoFileName, ASubtitleFileName, AOutputVideoFileName: String; AWidth, AHeight: Integer; AVideoCodec: String; AStyle: String = ''; AAudioCodec: String = ''; AAudioSampleRate: Integer = 44100; AAudioBitRate: Integer = 128; const ACB: TUWProcessCB = NIL): Boolean;

// -----------------------------------------------------------------------------

implementation

uses procTypes;

// -----------------------------------------------------------------------------

function GenerateVideoWithSubtitle(AVideoFileName, ASubtitleFileName, AOutputVideoFileName: String; AWidth, AHeight: Integer; AVideoCodec: String; AStyle: String = ''; AAudioCodec: String = ''; AAudioSampleRate: Integer = 44100; AAudioBitRate: Integer = 128; const ACB: TUWProcessCB = NIL): Boolean;
var
  VideoSettings,
  AudioSettings, s : String;
  AParamArray : TStringArray;
  i : Integer;
begin
  Result := False;
  if AVideoFileName.IsEmpty or ASubtitleFileName.IsEmpty or AOutputVideoFileName.IsEmpty then Exit;

  if AVideoCodec.IsEmpty then
    AVideoCodec := 'libx265';

  VideoSettings := '-c:v ' + AVideoCodec;
  if AVideoCodec = 'libx265' then
    VideoSettings += ' -tag:v hvc1';

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
        ['%input', '%subtitle', '%output'],
        [AVideoFileName, ASubtitleFileName, AOutputVideoFileName], [rfReplaceAll]);

    Result := ExecuteApp(Tools.FFmpeg, AParamArray, True, True, ACB);
  finally
    SetLength(AParamArray, 0);
  end;
end;

// -----------------------------------------------------------------------------

end.

