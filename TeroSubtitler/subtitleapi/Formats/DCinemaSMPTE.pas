{*
 *  URUWorks Subtitle API
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
 *  Copyright (C) 2001-2026 URUWorks, uruworks@gmail.com.
 *}

unit DCinemaSMPTE;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, UWSubtitleAPI, UWSubtitleAPI.Formats;

type

  { TUWDCinemaSMPTE }

  TUWDCinemaSMPTE = class(TUWSubtitleCustomFormat)
  public
    function Name: String; override;
    function Format: TUWSubtitleFormats; override;
    function Extension: String; override;
    function IsTimeBased: Boolean; override;
    function HasStyleSupport: Boolean; override;
    function IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean; override;
    function LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean; override;
    function SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean; override;
  end;

// -----------------------------------------------------------------------------

implementation

uses DCinema.Utils;

// -----------------------------------------------------------------------------

function TUWDCinemaSMPTE.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWDCinemaSMPTE.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfDCinemaSMPTE;
end;

// -----------------------------------------------------------------------------

function TUWDCinemaSMPTE.Extension: String;
begin
  Result := '*.xml';
end;

// -----------------------------------------------------------------------------

function TUWDCinemaSMPTE.IsTimeBased: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWDCinemaSMPTE.HasStyleSupport: Boolean;
begin
  Result := True; // italic, bold and underline
end;

// -----------------------------------------------------------------------------

function TUWDCinemaSMPTE.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  Result := (Pos('SubtitleReel', SubtitleFile[Row]) > 0) and (Pos('<', SubtitleFile[Row]) > 0) and (Pos('</', SubtitleFile[Row]) = 0);
end;

// -----------------------------------------------------------------------------

function TUWDCinemaSMPTE.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean;
begin
  Result := DCinemaLoadSubtitle(SubtitleFile, FPS, Subtitles, dcfSMPTE);
end;

// -----------------------------------------------------------------------------

function TUWDCinemaSMPTE.SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
begin
  Result := False;
  if not DCinemaSaveSubtitle(StringList, FPS, Subtitles, SubtitleMode, FromItem, ToItem, dcfSMPTE) then Exit;
  try
    if not FileName.IsEmpty then
      StringList.SaveToFile(FileName, TEncoding.UTF8); // must be encoded using UTF-8
    Result := True;
  except
  end;
end;

// -----------------------------------------------------------------------------

end.
