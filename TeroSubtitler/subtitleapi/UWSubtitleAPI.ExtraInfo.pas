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

unit UWSubtitleAPI.ExtraInfo;

// -----------------------------------------------------------------------------

interface

uses
  UWSubtitleAPI.Formats.Cavena890.Types;

// -----------------------------------------------------------------------------

type

  { TExtraInfoType }

  TUWSubtitleExtraInfoType =
  (
    eiNone,
    eiCavena890,
    eiEBU,
    eiMicroDVD,
    eiSSA,
    eiSubRip,
    eiWebVTT
  );

  { Advanced Subtitles }

  PXAS_Info = ^TXAS_Info;
  TXAS_Info = record
    Language  : AnsiString;
    FontName  : AnsiString;
    FontSize  : Byte;
    FontColor : Cardinal;
    X         : Byte;
    Y         : Byte;
    W         : Byte;
    H         : Byte;
    Alignment : Byte;
  end;

  { Cavena 890 }

  PCavena890_Info = ^TCavena890_Info;
  TCavena890_Info = THeaderBlock;

  { DVDSubtitle Info }

  PDVDSubtitle_Info = ^TDVDSubtitle_Info;
  TDVDSubtitle_Info = record
    Assigned : Boolean;
    DiskId   : String;
    DVDTitle : String;
    Language : String;
    Author   : String;
    Web      : String;
    Info     : String;
    License  : String;
  end;

  { EBU }

  PEBU_Info = ^TEBU_Info;
  TEBU_Info = record
    DiskFormatCode            : Byte;       // DFC (0= STL25.01 or  1= STL30.01)
    CodePageNumber            : AnsiString; // CPN (i.e. 850)
    DisplayStandardCode       : Byte;       // DSC
    CharCodeTableNumber       : AnsiString; // CCT
    LanguageCode              : AnsiString; // LC
    CountryOrigin             : AnsiString; // CO
    MaxNumberDisplayableChars : AnsiString; // MNC
    MaxNumberDisplayableRows  : AnsiString; // MNR
  end;

  PEBU_ExtraInfo = ^TEBU_ExtraInfo;
  TEBU_ExtraInfo = record
    CumulativeStatus  : Byte; // CS_Not, CS_First, CS_Intermediate, CS_Last
    VerticalPosition  : Byte; //
    JustificationCode : Byte; // JC_Unchanged, JC_Left, JC_Center, JC_Center, JC_Right
    CommentFlag       : Byte; // CF_TFHaveData, CF_TFHaveComments
  end;

  { MicroDVD }

  PMicroDVD_ExtraInfo = ^TMicroDVD_ExtraInfo;
  TMicroDVD_ExtraInfo = record
    X: Integer;
    Y: Integer;
  end;

  { SSA }

  TSSAEventType  = (etDialogue, etComment, etPicture, etSound, etCommand);
  PSSA_ExtraInfo = ^TSSA_ExtraInfo;
  TSSA_ExtraInfo = record
    EventType : TSSAEventType; // Event (most times "Dialogue)         (Default = "Dialogue")
    Marked    : Boolean;       // 0 = False; 1 = True                  (Default = "0")
    Style     : AnsiString;    // Style name                           (Default = "Default")
    Name      : AnsiString;    // Name of the person who is speaking   (Default = "NTP")
    MarginL   : Integer;       // Left margin (4 numbers)              (Default = "0000")
    MarginR   : Integer;       // Right margin (4 numbers)             (Default = "0000")
    MarginV   : Integer;       // Vertical margin (4 numbers)          (Default = "0000")
    Effect    : AnsiString;    // Effect                               (Default = "!Effect")
  end;

  { SubRip }

  PSubRip_ExtraInfo = ^TSubRip_ExtraInfo;
  TSubRip_ExtraInfo = record
    X1: Integer; // Left
    X2: Integer; // Right
    Y1: Integer; // Top
    Y2: Integer; // Bottom
  end;

  { WebVTT }

  PWebVTT_Info = ^TWebVTT_Info;
  TWebVTT_Info = record
    WriteCueIdentifiers : Boolean;
    UseXTIMESTAMP : Boolean;
    MPEGTS: Integer;
    LOCAL: Integer;
  end;

  PWebVTT_ExtraInfo = ^TWebVTT_ExtraInfo;
  TWebVTT_ExtraInfo = record
    //VerticalText : Integer;    // D:vertical (vertical growing left)
                               // D:vertical-lr (vertical growing right)
    LinePos      : Integer;    // line:[a number]%, where [a number] is a positive integer.
                               // line:[a number], where [a number] is a positive or negative integer.
    //TextPos      : Integer;    // T:[a number]%, where [a number] is a positive integer.
    //TextSize     : Integer;    // S:[a number]%, where [a number] is a positive integer.
    TextAlign    : AnsiString; // align:left or align:center or align:right
  end;

  { Format properties }

  PFormatProperties = ^TFormatProperties;
  TFormatProperties = record
    AdvancedSubtitles : TXAS_Info;
    Cavena890         : TCavena890_Info;
    DVDSubtitle       : TDVDSubtitle_Info;
    EBU               : TEBU_Info;
    WebVTT            : TWebVTT_Info;
  end;

procedure DefaultFormatPropertiesSettings(AFormatProperties: PFormatProperties);

// -----------------------------------------------------------------------------

implementation

uses UWSystem.StrUtils;

// -----------------------------------------------------------------------------

procedure DefaultFormatPropertiesSettings(AFormatProperties: PFormatProperties);
begin
  FillByte(AFormatProperties^, SizeOf(TFormatProperties), 0);
  with AFormatProperties^ do
  begin
    with AdvancedSubtitles do
    begin
      Language  := 'en';
      FontName  := 'arialbd.ttf';
      FontSize  := 60;
      FontColor := $FFFFFF;
      X         := 10;
      Y         := 90;
      W         := 80;
      H         := 10;
      Alignment := 0;
    end;

    with Cavena890 do
    begin
      //AnsiStringToAnsiChar(TapeNumber, '');
      //AnsiStringToAnsiChar(TranslatedTitle, '');
      //AnsiStringToAnsiChar(Translator, '');
      //AnsiStringToAnsiChar(TranslatedEpisode, '');
      //AnsiStringToAnsiChar(Comments, '');
      //AnsiStringToAnsiChar(PrimaryFont, '');
      //AnsiStringToAnsiChar(OriginalTitle, '');
      //AnsiStringToAnsiChar(SecondaryFont, '');
      AnsiStringToAnsiChar(StartTime, '00:00:00:00');
      //AnsiStringToAnsiChar(Producer, '');
      //AnsiStringToAnsiChar(EpisodeTitle, '');
      PrimaryLanguage := LangIdEnglish;
    end;

    with DVDSubtitle do
    begin
      Assigned := False;
      DiskId   := '';
      DVDTitle := '';
      Language := 'en';
      Author   := '';
      Web      := 'uruworks.net';
      Info     := '';
      License  := '';
    end;

    with EBU do
    begin
      DiskFormatCode            := 0;
      CodePageNumber            := '850';
      DisplayStandardCode       := 0;
      CharCodeTableNumber       := '';
      LanguageCode              := '0A';
      CountryOrigin             := 'URY';
      MaxNumberDisplayableChars := '40';
      MaxNumberDisplayableRows  := '23';
    end;

    with WebVTT do
    begin
      WriteCueIdentifiers := False;
      UseXTIMESTAMP       := False;
      MPEGTS              := 0;
      LOCAL               := 0;
    end;
  end;
end;

// -----------------------------------------------------------------------------

end.
