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
 *  Copyright (C) 2001-2023 URUWorks, uruworks@gmail.com.
 *}

unit UWSubtitleAPI.Formats;

// -----------------------------------------------------------------------------

interface

uses SysUtils, Classes;

const

  { TUWSubtitleFormatsName }

  TUWSubtitleFormatsName : array[1..39] of String =
  (
    'ABC iView',
    'Adobe Encore DVD',
    'Advanced SubStation Alpha',
    'Advanced Subtitles',
    'AQTitle',
    'Avid Caption',
    'Captions 32',
    'Captions Inc.',
    'Cavena 890',
    'Cheetah',
    'Cheetah Caption',
    'CPC-600',
    'DKS Subtitle Format',
    'DRTIC',
    'DVD Junior',
    'DVD Subtitle System',
    'DVDSubtitle',
    'EBU Subtitling Format',
    'FAB Subtitler',
    'GPAC TTXT',
    'I-Author Script',
    'Inscriber CG',
    'iTunes Timed Text',
    'JACOSub 2.7+',
    'Karaoke Lyrics LRC',
    'Karaoke Lyrics VKT',
    'MAC DVD Studio Pro',
    'MacSUB',
    'MicroDVD',
    'MPlayer',
    'MPlayer2',
    'Netflix Timed Text',
    'SBV',
    'Sofni',
    'STL',
    'SubRip',
    'Tero',
    'Timed-Text',
    'WebVTT'
  );

type

  { TUWSubtitleFormats }

  TUWSubtitleFormats =
  (
    sfInvalid,
    sfABCiView,
    sfAdobeEncoreDVD,
    sfAdvancedSubStationAlpha,
    sfAdvancedSubtitles,
    sfAQTitle,
    sfAvidCaption,
    sfCaptions32,
    sfCaptionsInc,
    sfCavena890,
    sfCheetah,
    sfCheetahCaption,
    sfCPC600,
    sfDKS,
    sfDRTIC,
    sfDVDJunior,
    sfDVDSubtitleSystem,
    sfDVDSubtitle,
    sfEBU,
    sfFABSubtitler,
    sfGPAC,
    sfIAuthorScript,
    sfInscriberCG,
    sfITunesTimedText,
    sfJACOSub,
    sfKaraokeLyricsLRC,
    sfKaraokeLyricsVKT,
    sfMACDVDStudioPro,
    sfMacSUB,
    sfMicroDVD,
    sfMPlayer,
    sfMPlayer2,
    sfNetflixTimedText,
    sfSBV,
    sfSofni,
    sfSTL,
    sfSubRip,
    sfTero,
    sfTimedText,
    sfWebVTT
  );

{ Helpers }

function IndexToName(const FormatIndex: ShortInt): String;
function NameToIndex(const FormatName: String): ShortInt;
function SubtitleEncoding(const Encoding: Integer): TEncoding;
procedure AddFormatsToStrings(var AStrings: TStrings);

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

{ Helpers }

// -----------------------------------------------------------------------------

function IndexToName(const FormatIndex: ShortInt): String;
begin
  Result := '';
  if (FormatIndex >= Low(TUWSubtitleFormatsName)) and (FormatIndex <= High(TUWSubtitleFormatsName)) then
    Result := TUWSubtitleFormatsName[FormatIndex];
end;

//------------------------------------------------------------------------------

function NameToIndex(const FormatName: String): ShortInt;
var
  i: ShortInt;
begin
  Result := 0;
  for i := Low(TUWSubtitleFormatsName) to High(TUWSubtitleFormatsName) do
    if LowerCase(FormatName) = AnsiLowerCase(TUWSubtitleFormatsName[i]) then
      Result := i;
end;

//------------------------------------------------------------------------------

function SubtitleEncoding(const Encoding: Integer): TEncoding;
begin
  case Encoding of
    -1    : Result := NIL;
    950   : Result := TEncoding.BigEndianUnicode;
    1201  : Result := TEncoding.Unicode;
    1252  : Result := TEncoding.ANSI;
    20127 : Result := TEncoding.ASCII;
    65000 : Result := TEncoding.UTF7;
    65001 : Result := TEncoding.UTF8;
    else
      Result := TEncoding.GetEncoding(Encoding);
  end;
end;

//------------------------------------------------------------------------------

procedure AddFormatsToStrings(var AStrings: TStrings);
var
  i: ShortInt;
begin
  if not Assigned(AStrings) then AStrings := TStringList.Create;

  AStrings.Clear;
  for i := Low(TUWSubtitleFormatsName) to High(TUWSubtitleFormatsName) do
    AStrings.Add(TUWSubtitleFormatsName[i]);
end;

//------------------------------------------------------------------------------

end.
