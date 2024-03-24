{*
 *  URUWorks MPEGAudio
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

unit UWFiles.MPEGAudio;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, Classes;

type

  { TUWID3v1Tag }

  TID3v1Tag = record
    Header  : array [1..3]  of Char;
    Title   : array [1..30] of Char;
    Artist  : array [1..30] of Char;
    Album   : array [1..30] of Char;
    Year    : array [1..4]  of Char;
    Comment : array [1..30] of Char;
    Genre   : Byte;
  end;

  TUWID3v1Tag = class
  private
    { Private declarations }
    FFileName : String;
    FTitle    : String;
    FArtist   : String;
    FAlbum    : String;
    FYear     : String;
    FComment  : String;
    FTrack    : Byte;
    FGenre    : Byte;
    FTrackStr : String;
    FGenreStr : String;
  public
    { Public declarations }
    constructor Create(const FileName: String);
    function ReadTag: Boolean;
    function WriteTag: Boolean;
    function RemoveTag: Boolean;
    procedure ClearTag;
    function TagAvailable: Boolean;
    function GenreToString(Genre: Byte): String;
    function StringToGenre(Genre: String): Byte;
    property Title    : String read FTitle    write FTitle;
    property Artist   : String read FArtist   write FArtist;
    property Album    : String read FAlbum    write FAlbum;
    property Year     : String read FYear     write FYear;
    property Comment  : String read FComment  write FComment;
    property Track    : Byte   read FTrack    write FTrack;
    property Genre    : Byte   read FGenre    write FGenre;
    property TrackStr : String read FTrackStr;
    property GenreStr : String read FGenreStr;
  end;

  { TUWID3v2Tag }

  TID3v2xFrameHeader = record // ID3v2.3.x & ID3v2.4.x
    ID    : array [1..4] of AnsiChar;
    Size  : Integer;
    Flags : Word;
  end;

  TID3v22FrameHeader = record // ID3v2.2.x
    ID   : array [1..3] of AnsiChar;
    Size : array [1..3] of Byte;
  end;

  TID3v2Tag = record
    ID          : array [1..3] of AnsiChar;
    Version     : Byte;
    Revision    : Byte;
    Flags       : Byte;
    Size        : array [1..4] of Byte;
    { Extended data }
    FileSize    : Integer;
    Frame       : array [1..16] of AnsiString;
    NeedRewrite : Boolean;
    PaddingSize : Integer;
  end;

  TUWID3v2Tag = class
    private
      { Private declarations }
      FFileName   : String;
      FSize       : Integer;
      FTitle      : String;
      FArtist     : String;
      FAlbum      : String;
      FTrack      : Word;
      FTrackStr   : String;
      FYear       : String;
      FGenre      : String;
      FComment    : String;
      FComposer   : String;
      FEncoder    : String;
      FCopyright  : String;
      FOrigArtist : String;
      FLink       : String;
      function ReadHeader(var Tag: TID3v2Tag): Boolean;
      function GetSize(const Tag: TID3v2Tag): Integer;
      procedure SetTagItem(const ID, Data: String; var Tag: TID3v2Tag);
      function Swap32(const Figure: Integer): Integer;
      procedure ReadFrames_v2x(var Tag: TID3v2Tag);
      procedure ReadFrames_v22(var Tag: TID3v2Tag);
      function RebuildFile(TagData: TStream): Boolean;
    public
      { Public declarations }
      constructor Create(const FileName: String);
      function ReadTag: Boolean;
      function WriteTag: Boolean;
      function RemoveTag: Boolean;
      procedure ClearTag;
      function TagAvailable: Boolean;
      property Size       : Integer read FSize;
      property Title      : String  read FTitle      write FTitle;
      property Artist     : String  read FArtist     write FArtist;
      property Album      : String  read FAlbum      write FAlbum;
      property Track      : Word    read FTrack      write FTrack;
      property TrackStr   : String  read FTrackStr;
      property Year       : String  read FYear       write FYear;
      property Genre      : String  read FGenre      write FGenre;
      property Comment    : String  read FComment    write FComment;
      property Composer   : String  read FComposer   write FComposer;
      property Encoder    : String  read FEncoder    write FEncoder;
      property Copyright  : String  read FCopyright  write FCopyright;
      property Link       : String  read FLink       write FLink;
      property OrigArtist : String  read FOrigArtist write FOrigArtist;
  end;

  { TUWMPEGAudioTag }

  TMPEGVBRData = record
    Found    : Boolean;
    ID       : array[1..4] of AnsiChar;
    Frames   : Integer;
    Bytes    : Integer;
    Scale    : Byte;
    VendorID : String;
  end;

  TMPEGFrameData = record
    Found           : Boolean;
    Position        : Integer;
    Size            : Word;
    Data            : array[1..4] of Byte;
    VersionID       : Byte;
    LayerID         : Byte;
    ProtectionBit   : Boolean;
    BitRateID       : Word;
    SampleRateID    : Word;
    PaddingBit      : Boolean;
    PrivateBit      : Boolean;
    ModeID          : Byte;
    ModeExtensionID : Byte;
    CopyrightBit    : Boolean;
    OriginalBit     : Boolean;
    EmphasisID      : Byte;
  end;

  TUWMPEGAudioTag = class
  private
    { Private declarations }
    FFileName    : String;
    FFileLength  : Integer;
    FVBR         : TMPEGVBRData;
    FFrame       : TMPEGFrameData;
    FID3v1       : TUWID3v1Tag;
    FID3v2       : TUWID3v2Tag;
    FVersion     : String;
    FLayer       : String;
    FBitRate     : Word;
    FSampleRate  : Word;
    FChannelMode : String;
    FEmphasis    : String;
    FFrames      : Integer;
    FDuration    : Integer;
    function GetCoefficient(const Frame: TMPEGFrameData): Byte;
    function GetBitRate(const Frame: TMPEGFrameData): Word;
    function GetSampleRate(const Frame: TMPEGFrameData): Word;
    function GetPadding(const Frame: TMPEGFrameData): Byte;
    function FindVBR(const Index: Word; Data: array of Byte): TMPEGVBRData;
  public
    { Public declarations }
    constructor Create(const FileName: String);
    destructor Destroy; override;
    function Read: Boolean;
    procedure Clear;
    property FileLength  : Integer        read FFileLength;
    property VBR         : TMPEGVBRData   read FVBR;
    property Frame       : TMPEGFrameData read FFrame;
    property Version     : String         read FVersion;
    property Layer       : String         read FLayer;
    property BitRate     : Word           read FBitRate;
    property SampleRate  : Word           read FSampleRate;
    property ChannelMode : String         read FChannelMode;
    property Emphasis    : String         read FEmphasis;
    property Frames      : Integer        read FFrames;
    property Duration    : Integer        read FDuration;
  end;

// -----------------------------------------------------------------------------

implementation

uses
  UWSystem.FileUtils, Math;

// -----------------------------------------------------------------------------

{ TUWID3v1Tag }

// -----------------------------------------------------------------------------

constructor TUWID3v1Tag.Create(const FileName: String);
begin
  FFileName := FileName;
  ReadTag;
end;

// -----------------------------------------------------------------------------

function TUWID3v1Tag.ReadTag: Boolean;
var
  UWFile : TUWFile;
  Tag    : TID3v1Tag;
begin
  Result := False;
  ClearTag;

  UWFile := TUWFile.Create(FFileName, fmOpenRead or fmShareDenyWrite);
  try
    if UWFile.Ready then
      if UWFile.Seek(UWFile.Size - 128) and UWFile.Read(Tag, 128) then
        if Tag.Header = 'TAG' then
        begin
          FTitle    := TrimRight(Tag.Title);
          FArtist   := TrimRight(Tag.Artist);
          FAlbum    := TrimRight(Tag.Album);
          FYear     := TrimRight(Tag.Year);
          FGenre    := Min(148, Tag.Genre);
          FGenreStr := GenreToString(FGenre);

          if ((Tag.Comment[29] = #0)  and (Tag.Comment[30] <> #0))  or
             ((Tag.Comment[29] = #32) and (Tag.Comment[30] <> #32)) then
          begin
            FComment  := TrimRight(Copy(Tag.Comment, 1, 28));
            FTrack    := Ord(Tag.Comment[30]);
            FTrackStr := IntToStr(FTrack);
          end
          else
            FComment := TrimRight(Tag.Comment);

          Result := True;
        end;
  finally
    UWFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function TUWID3v1Tag.WriteTag: Boolean;
var
  UWFile : TUWFile;
  Tag    : TID3v1Tag;
begin
  Result := False;

  FillByte(Tag, SizeOf(TID3v1Tag), 0);
  Tag.Header := 'TAG';
  Move(FTitle[1], Tag.Title[1], Min(30, Length(FTitle)));
  Move(FArtist[1], Tag.Artist[1], Min(30, Length(FArtist)));
  Move(FAlbum[1], Tag.Album[1], Min(30, Length(FAlbum)));
  Move(FYear[1], Tag.Year[1], Min(4, Length(FYear)));
  Move(FComment[1], Tag.Comment[1], Min(30, Length(FComment)));
  Tag.Genre := Byte(FGenre);

  if FTrack > 0 then
  begin
    Tag.Comment[29] := #0;
    Tag.Comment[30] := Chr(FTrack);
  end;

  UWFile := TUWFile.Create(FFileName, fmOpenReadWrite or fmShareExclusive);
  try
    if UWFile.Ready then
      if UWFile.Seek(UWFile.Size) and UWFile.Write(Tag, SizeOf(TID3v1Tag)) then
        Result := True;
  finally
    UWFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function TUWID3v1Tag.RemoveTag: Boolean;
var
  UWFile : TUWFile;
  Tag    : TID3v1Tag;
begin
  Result := False;

  UWFile := TUWFile.Create(FFileName, fmOpenReadWrite or fmShareExclusive);
  try
    if UWFile.Ready then
      if UWFile.Seek(UWFile.Size - 128) and UWFile.Read(Tag, 128) then
        if (Tag.Header = 'TAG') and UWFile.Truncate then
          Result := True;
  finally
    UWFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWID3v1Tag.ClearTag;
begin
  FTitle    := '';
  FArtist   := '';
  FAlbum    := '';
  FYear     := '';
  FComment  := '';
  FTrack    := 0;
  FGenre    := 148;
  FTrackStr := '';
  FGenreStr := '';
end;

// -----------------------------------------------------------------------------

function TUWID3v1Tag.TagAvailable: Boolean;
begin
  if (FTitle <> '') or (FArtist <> '') or (FAlbum <> '') or (FYear <> '') or
     (FComment <> '') then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWID3v1Tag.GenreToString(Genre: Byte): String;
const
  GenreStrings: array[0..148] of AnsiString =
   ('Blues', 'Classic Rock', 'Country', 'Dance', 'Disco', 'Funk', 'Grunge',
    'Hip-Hop', 'Jazz', 'Metal', 'New Age', 'Oldies', 'Other', 'Pop',
    'R&B', 'Rap', 'Reggae', 'Rock', 'Techno', 'Industrial', 'Alternative',
    'Ska', 'Death Metal', 'Pranks', 'Soundtrack', 'Euro-Techno', 'Ambient',
    'Trip-Hop', 'Vocal', 'Jazz+Funk', 'Fusion', 'Trance', 'Classical',
    'Instrumental', 'Acid', 'House', 'Game', 'Sound Clip', 'Gospel',
    'Noise', 'AlternRock', 'Bass', 'Soul', 'Punk', 'Space', 'Meditative',
    'Instrumental Pop', 'Instrumental Rock', 'Ethnic', 'Gothic', 'Darkwave',
    'Techno-Industrial', 'Electronic', 'Pop-Folk', 'Eurodance', 'Dream',
    'Southern Rock', 'Comedy', 'Cult', 'Gangsta', 'Top 40', 'Christian Rap',
    'Pop/Funk', 'Jungle', 'Native American', 'Cabaret', 'New Wave', 'Psychadelic',
    'Rave', 'Showtunes', 'Trailer', 'Lo-Fi', 'Tribal', 'Acid Punk', 'Acid Jazz',
    'Polka', 'Retro', 'Musical', 'Rock & Roll', 'Hard Rock', 'Folk', 'Folk/Rock',
    'National Folk', 'Swing', 'Fast Fusion', 'Bebob', 'Latin', 'Revival',
    'Celtic', 'Bluegrass', 'Avantgarde', 'Gothic Rock', 'Progressive Rock',
    'Psychedelic Rock', 'Symphonic Rock', 'Slow Rock', 'Big Band', 'Chorus',
    'Easy Listening', 'Acoustic', 'Humour', 'Speech', 'Chanson', 'Opera',
    'Chamber Music', 'Sonata', 'Symphony', 'Booty Bass', 'Primus',
    'Porn Groove', 'Satire', 'Slow Jam', 'Club', 'Tango', 'Samba', 'Folklore',
    'Ballad', 'Power Ballad', 'Rhythmic Soul', 'Freestyle', 'Duet', 'Punk Rock',
    'Drum Solo', 'Acapella', 'Euro-House', 'Dance Hall', 'Goa', 'Drum & Bass',
    'Club-House', 'Hardcore', 'Terror', 'Indie', 'BritPop', 'Negerpunk',
    'Polsk Punk', 'Beat', 'Christian Gangsta Rap', 'Heavy Metal', 'Black Metal',
    'Crossover', 'Contemporary Christian', 'Christian Rock', 'Merengue', 'Salsa',
    'Trash Metal', 'Anime', 'JPop', 'Synthpop', '');
begin
  if Genre <= High(GenreStrings) then
    Result := GenreStrings[Genre]
  else
    Result := '';
end;

// -----------------------------------------------------------------------------

function TUWID3v1Tag.StringToGenre(Genre: String): Byte;
var
  i: Byte;
begin
  Result := 148;

  if Genre <> '' then
    for i := 0 to 147 do
      if Genre = GenreToString(i) then
      begin
        Result := i;
        Break;
      end;
end;

// -----------------------------------------------------------------------------

{ TUWID3v2Tag }

// -----------------------------------------------------------------------------

constructor TUWID3v2Tag.Create(const FileName: String);
begin
  FFileName := FileName;
  ReadTag;
end;

// -----------------------------------------------------------------------------

function TUWID3v2Tag.ReadTag: Boolean;

  function GetANSI(const Source: String): String;
  var
    Index       : Integer;
    FirstByte,
    SecondByte  : Byte;
    UnicodeChar : WideChar;
  begin
    if (Length(Source) > 0) and (Source[1] = #1) then
    begin
      Result := '';

      for Index := 1 to ((Length(Source)-1) div 2) do
      begin
        FirstByte   := Ord(Source[Index * 2]);
        SecondByte  := Ord(Source[Index * 2 + 1]);
        UnicodeChar := WideChar(FirstByte or (SecondByte shl 8));
        if UnicodeChar = #0 then Break;
        if FirstByte < $FF then
          Result := Result + UnicodeChar;
      end;
      Result := Trim(Result);
    end
    else
      Result := Trim(Source);
  end;

  {function GetContent(const Content1, Content2: String): String;
  begin
    Result := GetANSI(Content1);
    if Result = '' then
      Result := GetANSI(Content2);
  end;}

  function ExtractTrack(TrackString: String): Word;
  var
    Track       : String;
    c           : Char;
    NumberFound : Boolean;
    i, Value,
    Code        : Integer;
  begin
    TrackString := GetANSI(TrackString);
    Track       := '';
    NumberFound := False;

    for i := 1 to Length(TrackString) do
    begin
      c := TrackString[i];
      if (c >= '0') and (c <= '9') then
      begin
        NumberFound := True;
        Track       := Track + c;
      end
      else if (NumberFound) then
        Break;
    end;

    if (NumberFound) then
    begin
      try
        Val(Track, Value, Code);
        if (Code = 0) then
          Result := Value
        else
          Result := 0;
      except
        Result := 0;
      end
    end
    else
       Result := 0;
  end;

  {function ExtractYear(const YearString, DateString: String): String;
  begin
    Result := GetANSI(YearString);
    if Result = '' then
      Result := Copy(GetANSI(DateString), 1, 4);
  end;}

  function ExtractGenre(const GenreString: String): String;
  begin
    Result := GetANSI(GenreString);
    if Pos(')', Result) > 0 then
      Delete(Result, 1, LastDelimiter(')', Result));
  end;

  function ExtractText(const SourceString: String; LanguageID: Boolean): String;
  var
    Source, Separator : String;
    EncodingID        : Char;
  begin
    Source := SourceString;
    Result := '';
    if Length(Source) > 0 then
    begin
      EncodingID := Source[1];
      if EncodingID = #1 then
        Separator := #0#0
      else
        Separator := #0;

      if LanguageID then
        Delete(Source, 1, 4)
      else
        Delete(Source, 1, 1);

      Delete(Source, 1, Pos(Separator, Source) + Length(Separator) - 1);
      Result := GetANSI(EncodingID + Source);
    end;
  end;

var
  Tag: TID3v2Tag;
begin
  Result := False;
  ClearTag;
  if (FFileName = '') or not FileExists(FFileName) then Exit;

  Result := ReadHeader(Tag);
  if (Result) and (Tag.ID = 'ID3') then
  begin
    FSize := GetSize(Tag);
    if (Tag.Version in [2..4]) and (FSize > 0) then
    begin
      if Tag.Version > 2 then
        ReadFrames_v2x(Tag)
      else
        ReadFrames_v22(Tag);

      FTitle      := GetANSI(Tag.Frame[1]); //GetContent(Tag.Frame[1], Tag.Frame[15]);
      FArtist     := GetANSI(Tag.Frame[2]);
      FAlbum      := GetANSI(Tag.Frame[3]); //GetContent(Tag.Frame[3], Tag.Frame[16]);
      FTrack      := ExtractTrack(Tag.Frame[4]);
      FTrackStr   := GetANSI(Tag.Frame[4]);
      FYear       := GetANSI(Tag.Frame[5]); //ExtractYear(Tag.Frame[5], Tag.Frame[13]);
      FGenre      := ExtractGenre(Tag.Frame[6]);
      FComment    := ExtractText(Tag.Frame[7], True);
      FComposer   := GetANSI(Tag.Frame[8]);
      FEncoder    := GetANSI(Tag.Frame[9]);
      FCopyright  := GetANSI(Tag.Frame[10]);
      FLink       := ExtractText(Tag.Frame[12], False);
      FOrigArtist := GetANSI(Tag.Frame[14]);
    end;
  end;
end;

// -----------------------------------------------------------------------------

function TUWID3v2Tag.WriteTag: Boolean;
  procedure BuildHeader(var Tag: TID3v2Tag);
  var
    i, TagSize: Integer;
  begin
    TagSize := 10;

    for i := 1 to 16 do
      if Tag.Frame[i] <> '' then
        Inc(TagSize, Length(Tag.Frame[i]) + 11);

    Tag.NeedRewrite := (Tag.ID <> 'ID3') or (GetSize(Tag) < TagSize) or (GetSize(Tag) > 4096);

    if Tag.NeedRewrite then
      Tag.PaddingSize := 4096 - TagSize
    else
      Tag.PaddingSize := GetSize(Tag) - TagSize;

    if Tag.PaddingSize > 0 then
      Inc(TagSize, Tag.PaddingSize);

    Tag.ID       := 'ID3';
    Tag.Version  := 3;
    Tag.Revision := 0;
    Tag.Flags    := 0;

    for i := 1 to 4 do
      Tag.Size[i] := ((TagSize - 10) shr ((4 - i) * 7)) and $7F;
  end;

  function ReplaceTag(TagData: TStream): Boolean;
  var
    Destination: TFileStream;
  begin
    Result := False;
    if (not FileExists(FFileName)) or (FileSetAttr(FFileName, 0) <> 0) then Exit;

    TagData.Position := 0;
    Destination      := TFileStream.Create(FFileName, fmOpenReadWrite);
    try
      Destination.CopyFrom(TagData, TagData.Size);
      Result := True;
    finally
      Destination.Free;
    end;
  end;

const
  // ID3v2.3.x & ID3v2.4.x
  ID3v2xFrame: array[1..16] of AnsiString =
    ('TIT2', 'TPE1', 'TALB', 'TRCK', 'TYER', 'TCON', 'COMM', 'TCOM', 'TENC',
     'TCOP', 'TLAN', 'WXXX', 'TDRC', 'TOPE', 'TIT1', 'TOAL');

var
  Tag          : TID3v2Tag;
  TagData      : TStringStream;
  i, FrameSize : Integer;
  Padding      : array [1..4096] of Byte;
begin
  Result := False;
  if (FFileName = '') or not FileExists(FFileName) then Exit;

  FillByte(Tag, SizeOf(TID3v2Tag), 0);
  ReadHeader(Tag);
  Tag.Frame[1]  := FTitle;
  Tag.Frame[2]  := FArtist;
  Tag.Frame[3]  := FAlbum;
  if FTrack > 0 then
    Tag.Frame[4] := IntToStr(FTrack);
  Tag.Frame[5]  := FYear;
  Tag.Frame[6]  := FGenre;
  if FComment <> '' then
    Tag.Frame[7] := 'eng' + #0 + FComment;
  Tag.Frame[8]  := FComposer;
  Tag.Frame[9]  := FEncoder;
  Tag.Frame[10] := FCopyright;
  if FLink <> '' then
    Tag.Frame[12] := #0 + FLink;
  Tag.Frame[14] := FOrigArtist;

  TagData := TStringStream.Create('');
  try
    BuildHeader(Tag);
    TagData.Write(Tag, 10);

    for i := 1 to 16 do
      if Tag.Frame[i] <> '' then
      begin
        TagData.WriteString(ID3v2xFrame[i]);
        FrameSize := Swap32(Length(Tag.Frame[i]) + 1);
        TagData.Write(FrameSize, SizeOf(FrameSize));
        TagData.WriteString(#0#0#0 + Tag.Frame[i]);
      end;

    FillChar(Padding, SizeOf(Padding), 0);
    if Tag.PaddingSize > 0 then
      TagData.Write(Padding, Tag.PaddingSize);

    if Tag.NeedRewrite then
      Result := RebuildFile(TagData)
    else
      Result := ReplaceTag(TagData);
  finally
    TagData.Free;
  end;
end;

// -----------------------------------------------------------------------------

function TUWID3v2Tag.RemoveTag: Boolean;
begin
  Result := RebuildFile(NIL);
end;

// -----------------------------------------------------------------------------

procedure TUWID3v2Tag.ClearTag;
begin
  FSize       := 0;
  FTitle      := '';
  FArtist     := '';
  FAlbum      := '';
  FTrack      := 0;
  FTrackStr   := '';
  FYear       := '';
  FGenre      := '';
  FComment    := '';
  FComposer   := '';
  FEncoder    := '';
  FCopyright  := '';
  FLink       := '';
  FOrigArtist := '';
end;

// -----------------------------------------------------------------------------

function TUWID3v2Tag.TagAvailable: Boolean;
begin
  if (FTitle <> '') or (FArtist <> '') or (FAlbum <> '') or (FYear <> '') or
     (FGenre <> '') or (FComment <> '') or (FComposer <> '') or (FEncoder <> '') or
     (FCopyright <> '') or (FOrigArtist <> '') or (FLink <> '') then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWID3v2Tag.ReadHeader(var Tag: TID3v2Tag): Boolean;
var
  UWFile: TUWFile;
begin
  Result := False;

  UWFile := TUWFile.Create(FFileName, fmOpenRead or fmShareDenyWrite);
  try
    if UWFile.Ready then
      if UWFile.Read(Tag, 10) and (UWFile.Transferred = 10) then
      begin
        Tag.FileSize := UWFile.Size;
        Result := True;
      end;
  finally
    UWFile.Free;
  end;
end;

{ --------------------------------------------------------------------------- }

function TUWID3v2Tag.GetSize(const Tag: TID3v2Tag): Integer;
begin
  Result := Tag.Size[1] * $200000 + Tag.Size[2] * $4000 +
            Tag.Size[3] * $80     + Tag.Size[4] + 10;

  if Tag.Flags and $10 = $10 then Inc(Result, 10);
  if Result > Tag.FileSize then Result := 0;
end;

{ --------------------------------------------------------------------------- }

procedure TUWID3v2Tag.SetTagItem(const ID, Data: String; var Tag: TID3v2Tag);
const
  // ID3v2.3.x & ID3v2.4.x
  ID3v2xFrame: array[1..16] of AnsiString =
    ('TIT2', 'TPE1', 'TALB', 'TRCK', 'TYER', 'TCON', 'COMM', 'TCOM', 'TENC',
     'TCOP', 'TLAN', 'WXXX', 'TDRC', 'TOPE', 'TIT1', 'TOAL');

  // ID3v2.2.x
  ID3v22Frame: array[1..16] of AnsiString =
    ('TT2', 'TP1', 'TAL', 'TRK', 'TYE', 'TCO', 'COM', 'TCM', 'TEN',
     'TCR', 'TLA', 'WXX', 'TOR', 'TOA', 'TT1', 'TOT');

var
  i       : Byte;
  FrameID : String;
begin
  for i := 1 to 16 do
  begin
    if Tag.Version > 2 then
      FrameID := ID3v2xFrame[i]
    else
      FrameID := ID3v22Frame[i];

    if (FrameID = ID) and (Data[1] <= #1) then
      Tag.Frame[i] := Data;
  end;
end;

{ --------------------------------------------------------------------------- }

function TUWID3v2Tag.Swap32(const Figure: Integer): Integer;
var
  ByteArray: array[1..4] of Byte absolute Figure;
begin
  Result := ByteArray[1] * $1000000 + ByteArray[2] * $10000 +
            ByteArray[3] * $100     + ByteArray[4];
end;

{ --------------------------------------------------------------------------- }

procedure TUWID3v2Tag.ReadFrames_v2x(var Tag: TID3v2Tag);
var
  UWFile      : TUWFile;
  Frame       : TID3v2xFrameHeader;
  Data        : array[1..500] of AnsiChar;
  DataPosition,
  DataSize    : Integer;
begin
  UWFile := TUWFile.Create(FFileName, fmOpenRead or fmShareDenyWrite);
  try
    if UWFile.Ready then
      if UWFile.Seek(10) then
        while (UWFile.Position < GetSize(Tag)) and (not UWFile.EOF) do
        begin
          FillChar(Data, SizeOf(Data), 0);
          UWFile.Read(Frame, 10);
          if not (Frame.ID[1] in ['A'..'Z']) then Break;

          DataPosition := UWFile.Position;
          if Swap32(Frame.Size) > SizeOf(Data) then
            DataSize := SizeOf(Data)
          else
            DataSize := Swap32(Frame.Size);

          UWFile.Read(Data, DataSize);
          if Frame.Flags and $8000 <> $8000 then
            SetTagItem(Frame.ID, Data, Tag);

          UWFile.Seek(DataPosition + Swap32(Frame.Size));
        end;
  finally
    UWFile.Free;
  end;
end;

{ --------------------------------------------------------------------------- }

procedure TUWID3v2Tag.ReadFrames_v22(var Tag: TID3v2Tag);
var
  UWFile      : TUWFile;
  Frame       : TID3v22FrameHeader;
  Data        : array[1..500] of AnsiChar;
  DataPosition,
  FrameSize,
  DataSize    : Integer;
begin
  UWFile := TUWFile.Create(FFileName, fmOpenRead or fmShareDenyWrite);
  try
    if UWFile.Ready then
      if UWFile.Seek(10) then
        while (UWFile.Position < GetSize(Tag)) and (not UWFile.EOF) do
        begin
          FillChar(Data, SizeOf(Data), 0);
          UWFile.Read(Frame, 6);
          if not (Frame.ID[1] in ['A'..'Z']) then Break;

          DataPosition := UWFile.Position;
          FrameSize    := Frame.Size[1] shl 16 + Frame.Size[2] shl 8 + Frame.Size[3];
          if FrameSize > SizeOf(Data) then
            DataSize := SizeOf(Data)
          else
            DataSize := FrameSize;

          UWFile.Read(Data, DataSize);
          SetTagItem(Frame.ID, Data, Tag);
          UWFile.Seek(DataPosition + FrameSize);
        end;
  finally
    UWFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function TUWID3v2Tag.RebuildFile(TagData: TStream): Boolean;
var
  Tag         : TID3v2Tag;
  Source,
  Destination : TFileStream;
  BufferName  : String;
begin
  Result := False;
  if (not FileExists(FFileName)) or (FileSetAttr(FFileName, 0) <> 0) then Exit;

  if not ReadHeader(Tag) or ((TagData = NIL) and (Tag.ID <> 'ID3')) then Exit;
  try
    BufferName  := FFileName + '~';
    Source      := TFileStream.Create(FFileName, fmOpenRead);
    Destination := TFileStream.Create(BufferName, fmCreate);
    try
      if Tag.ID = 'ID3' then Source.Seek(GetSize(Tag), soFromBeginning);
      if TagData <> NIL then Destination.CopyFrom(TagData, 0);
      Destination.CopyFrom(Source, Source.Size - Source.Position);
    finally
      Source.Free;
      Destination.Free;
    end;

    if (DeleteFile(FFileName)) and (RenameFile(BufferName, FFileName)) then
      Result := True;
  finally
    if (BufferName <> '') and FileExists(BufferName) then
      DeleteFile(BufferName);
  end;
end;

// -----------------------------------------------------------------------------

{ TUWMPEGAudioTag }

// -----------------------------------------------------------------------------

constructor TUWMPEGAudioTag.Create(const FileName: String);
begin
  FFileName := FileName;
  FID3v1    := TUWID3v1Tag.Create(FFileName);
  FID3v2    := TUWID3v2Tag.Create(FFileName);
  Read;
end;

// -----------------------------------------------------------------------------

destructor TUWMPEGAudioTag.Destroy;
begin
  FID3v1.Free;
  FID3v2.Free;
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

function TUWMPEGAudioTag.Read: Boolean;
  function IsFrameHeader(const HeaderData: array of Byte): Boolean;
  begin
    if ((HeaderData[0] and $FF) <> $FF) or ((HeaderData[1] and $E0) <> $E0) or
       (((HeaderData[1] shr 3) and 3) = 1) or (((HeaderData[1] shr 1) and 3) = 0) or
       ((HeaderData[2] and $F0) = $F0) or ((HeaderData[2] and $F0) = 0) or
       (((HeaderData[2] shr 2) and 3) = 3) or ((HeaderData[3] and 3) = 2) then
      Result := False
    else
      Result := True;
  end;

  procedure DecodeHeader(const HeaderData: array of Byte; var Frame: TMPEGFrameData);
  begin
    Move(HeaderData, Frame.Data, SizeOf(Frame.Data));
    Frame.VersionID       := (HeaderData[1] shr 3) and 3;
    Frame.LayerID         := (HeaderData[1] shr 1) and 3;
    Frame.ProtectionBit   := (HeaderData[1] and 1) <> 1;
    Frame.BitRateID       := HeaderData[2] shr 4;
    Frame.SampleRateID    := (HeaderData[2] shr 2) and 3;
    Frame.PaddingBit      := ((HeaderData[2] shr 1) and 1) = 1;
    Frame.PrivateBit      := (HeaderData[2] and 1) = 1;
    Frame.ModeID          := (HeaderData[3] shr 6) and 3;
    Frame.ModeExtensionID := (HeaderData[3] shr 4) and 3;
    Frame.CopyrightBit    := ((HeaderData[3] shr 3) and 1) = 1;
    Frame.OriginalBit     := ((HeaderData[3] shr 2) and 1) = 1;
    Frame.EmphasisID      := HeaderData[3] and 3;
  end;

  function ValidFrameAt(const Index: Word; Data: array of Byte): Boolean;
  var
    HeaderData: array[1..4] of Byte;
  begin
    HeaderData[1] := Data[Index];
    HeaderData[2] := Data[Index + 1];
    HeaderData[3] := Data[Index + 2];
    HeaderData[4] := Data[Index + 3];
    if IsFrameHeader(HeaderData) then
      Result := True
    else
      Result := False;
  end;

  function GetFrameLength(const Frame: TMPEGFrameData): Word;
  var
    Coefficient, BitRate, SampleRate, Padding: Word;
  begin
    Coefficient := GetCoefficient(Frame);
    BitRate     := GetBitRate(Frame);
    SampleRate  := GetSampleRate(Frame);
    Padding     := GetPadding(Frame);
    Result      := Trunc(Coefficient * BitRate * 1000 / SampleRate) + Padding;
  end;

  function GetVBRDeviation(const Frame: TMPEGFrameData): Byte;
  begin
    if Frame.VersionID = 3 then
    begin
      if Frame.ModeID <> 3 then
        Result := 36
      else
        Result := 21;
    end
    else
      if Frame.ModeID <> 3 then
        Result := 21
      else
        Result := 13;
  end;

  function FindFrame(const Data: array of Byte; var VBR: TMPEGVBRData): TMPEGFrameData;
  var
    HeaderData : array [1..4] of Byte;
    i          : Integer;
  begin
    FillChar(Result, SizeOf(Result), 0);
    Move(Data, HeaderData, SizeOf(HeaderData));
    for i := 0 to SizeOf(Data) - 1729 do
    begin
      if IsFrameHeader(HeaderData) then
      begin
        DecodeHeader(HeaderData, Result);

        if ValidFrameAt(i + GetFrameLength(Result), Data) then
        begin
          Result.Found    := True;
          Result.Position := i;
          Result.Size     := GetFrameLength(Result);
          VBR             := FindVBR(i + GetVBRDeviation(Result), Data);
          Break;
        end;
      end;

      HeaderData[1] := HeaderData[2];
      HeaderData[2] := HeaderData[3];
      HeaderData[3] := HeaderData[4];
      HeaderData[4] := Data[i + SizeOf(HeaderData)];
    end;
  end;

  function GetBRate: Word;
  begin
    if (FVBR.Found) and (FVBR.Frames > 0) then
      Result := Round((FVBR.Bytes / FVBR.Frames - GetPadding(FFrame)) *
                GetSampleRate(FFrame) / GetCoefficient(FFrame) / 1000)
    else
      Result := GetBitRate(FFrame);
  end;

  function GetFrames: Integer;
  var
    MPEGSize: Integer;
  begin
    if FVBR.Found then
      Result := FVBR.Frames
    else
    begin
      if FID3v1.TagAvailable then
        MPEGSize := FFileLength - FID3v2.Size - 128
      else
        MPEGSize := FFileLength - FID3v2.Size;

      Result := (MPEGSize - FFrame.Position) div GetFrameLength(FFrame);
    end;
  end;

  function GetDuration: Double;
  var
    MPEGSize: Integer;
  begin
    Result := 0;
    if FFrame.Found then
      if (FVBR.Found) and (FVBR.Frames > 0) then
        Result := FVBR.Frames * GetCoefficient(FFrame) * 8 / GetSampleRate(FFrame)
      else
      begin
        if FID3v1.TagAvailable then
          MPEGSize := FFileLength - FID3v2.Size - 128
        else
          MPEGSize := FFileLength - FID3v2.Size;

        Result := (MPEGSize - FFrame.Position) / GetBitRate(FFrame) / 1000 * 8;
      end;
  end;

const
  MPEG_Version  : array[0..3] of AnsiString = ('MPEG 2.5', 'MPEG ?', 'MPEG 2', 'MPEG 1');
  MPEG_Layer    : array[0..3] of AnsiString = ('Layer ?', 'Layer III', 'Layer II', 'Layer I');
  MPEG_CMode    : array[0..4] of AnsiString = ('Stereo', 'Joint Stereo', 'Dual Channel', 'Mono', 'Unknown');
  MPEG_Emphasis : array[0..3] of AnsiString = ('None', '50/15 ms', 'Unknown', 'CCIT J.17');

var
  UWFile : TUWFile;
  Data   : array[1..3458] of Byte;
begin
  Result := False;
  Clear;

  UWFile := TUWFile.Create(FFileName, fmOpenRead or fmShareDenyWrite);
  try
    if UWFile.Ready then
    begin
      FFileLength := UWFile.Size;

      UWFile.Seek(FID3v2.Size);
      UWFile.Read(Data, SizeOf(Data));
      FFrame := FindFrame(Data, FVBR);

      if (not FFrame.Found) and (UWFile.Transferred = SizeOf(Data)) then
      begin
        UWFile.Seek((FFileLength - FID3v2.Size) div 2);
        UWFile.Read(Data, SizeOf(Data));
        FFrame := FindFrame(Data, FVBR);
      end;

      if not FFrame.Found then
      begin
        Clear;
        Exit;
      end;

      FVersion     := MPEG_Version[FFrame.VersionID];
      FLayer       := MPEG_Layer[FFrame.LayerID];
      FBitRate     := GetBRate;
      FSampleRate  := GetSampleRate(FFrame);
      FChannelMode := MPEG_CMode[FFrame.ModeID];
      FEmphasis    := MPEG_Emphasis[FFrame.EmphasisID];
      FFrames      := GetFrames;
      FDuration    := Round(GetDuration*1000);
      Result       := True;
    end;
  finally
    UWFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWMPEGAudioTag.Clear;
begin
  FillByte(FVBR, SizeOf(TMPEGVBRData), 0);
  FillByte(FFrame, SizeOf(TMPEGFrameData), 0);
  FFileLength            := 0;
  FFrame.VersionID       := 1;
  FFrame.SampleRateID    := 3;
  FFrame.ModeID          := 4;
  FFrame.ModeExtensionID := 4;
  FFrame.EmphasisID      := 2;
  FID3v1.ClearTag;
  FID3v2.ClearTag;
end;

// -----------------------------------------------------------------------------

function TUWMPEGAudioTag.GetCoefficient(const Frame: TMPEGFrameData): Byte;
begin
  if Frame.VersionID = 3 then
  begin
    if Frame.LayerID = 3 then
      Result := 48
    else
      Result := 144;
  end
  else
    if Frame.LayerID = 3 then
      Result := 24
    else if Frame.LayerID = 2 then
      Result := 144
    else
      Result := 72;
end;

// -----------------------------------------------------------------------------

function TUWMPEGAudioTag.GetBitRate(const Frame: TMPEGFrameData): Word;
const
  MPEG_BitRate: array[0..3, 0..3, 0..15] of Word =
    (
    { MPEG 2.5 }
    ((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 8, 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 144, 160, 0),
    (0, 8, 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 144, 160, 0),
    (0, 32, 48, 56, 64, 80, 96, 112, 128, 144, 160, 176, 192, 224, 256, 0)),
    { Reserved }
    ((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)),
    { MPEG 2 }
    ((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 8, 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 144, 160, 0),
    (0, 8, 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 144, 160, 0),
    (0, 32, 48, 56, 64, 80, 96, 112, 128, 144, 160, 176, 192, 224, 256, 0)),
    { MPEG 1 }
    ((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256, 320, 0),
    (0, 32, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256, 320, 384, 0),
    (0, 32, 64, 96, 128, 160, 192, 224, 256, 288, 320, 352, 384, 416, 448, 0))
    );

begin
  Result := MPEG_BitRate[Frame.VersionID, Frame.LayerID, Frame.BitRateID];
end;

// -----------------------------------------------------------------------------

function TUWMPEGAudioTag.GetSampleRate(const Frame: TMPEGFrameData): Word;
const
  MPEG_SampleRate: array[0..3, 0..3] of Word =
    (
    (11025, 12000, 8000, 0),  { MPEG 2.5 }
    (0, 0, 0, 0),             { Reserved }
    (22050, 24000, 16000, 0), { MPEG 2 }
    (44100, 48000, 32000, 0)  { MPEG 1 }
    );

begin
  Result := MPEG_SampleRate[Frame.VersionID, Frame.SampleRateID];
end;

// -----------------------------------------------------------------------------

function TUWMPEGAudioTag.GetPadding(const Frame: TMPEGFrameData): Byte;
begin
  if Frame.PaddingBit then
  begin
    if Frame.LayerID = 3 then
      Result := 4
    else
      Result := 1;
  end
  else
    Result := 0;
end;

// -----------------------------------------------------------------------------

function TUWMPEGAudioTag.FindVBR(const Index: Word; Data: array of Byte): TMPEGVBRData;
const
  ID_Xing = 'Xing';
  ID_FhG  = 'VBRI';

  function GetXingInfo(const Index: Word; Data: array of Byte): TMPEGVBRData;
  begin
    FillChar(Result, SizeOf(Result), 0);
    Result.Found    := True;
    Result.ID       := ID_Xing;
    Result.Frames   := Data[Index + 8]  * $1000000 + Data[Index + 9] * $10000 +
                       Data[Index + 10] * $100     + Data[Index + 11];

    Result.Bytes    := Data[Index + 12] * $1000000 + Data[Index + 13] * $10000 +
                       Data[Index + 14] * $100     + Data[Index + 15];

    Result.Scale    := Data[Index + 119];

    Result.VendorID := Chr(Data[Index + 120]) + Chr(Data[Index + 121]) +
                       Chr(Data[Index + 122]) + Chr(Data[Index + 123]) +
                       Chr(Data[Index + 124]) + Chr(Data[Index + 125]) +
                       Chr(Data[Index + 126]) + Chr(Data[Index + 127]);
  end;

  function GetFhGInfo(const Index: Word; Data: array of Byte): TMPEGVBRData;
  begin
    FillChar(Result, SizeOf(Result), 0);
    Result.Found  := True;
    Result.ID     := ID_FhG;
    Result.Scale  := Data[Index + 9];
    Result.Bytes  := Data[Index + 10] * $1000000 + Data[Index + 11] * $10000 +
                     Data[Index + 12] * $100     + Data[Index + 13];

    Result.Frames := Data[Index + 14] * $1000000 + Data[Index + 15] * $10000 +
                     Data[Index + 16] * $100     + Data[Index + 17];
  end;

begin
  FillChar(Result, SizeOf(Result), 0);
  if Chr(Data[Index]) + Chr(Data[Index + 1]) +
     Chr(Data[Index + 2]) + Chr(Data[Index + 3]) = ID_Xing then
    Result := GetXingInfo(Index, Data);

  if Chr(Data[Index]) + Chr(Data[Index + 1]) +
     Chr(Data[Index + 2]) + Chr(Data[Index + 3]) = ID_FhG then
    Result := GetFhGInfo(Index, Data);
end;

// -----------------------------------------------------------------------------

end.
