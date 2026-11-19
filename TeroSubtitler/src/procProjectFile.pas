{*
 *  URUWorks Tero Subtitler
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
 *  Copyright (C) 2023-2026 URUWorks, uruworks@gmail.com.
 *}

unit procProjectFile;

{$mode ObjFPC}{$H+}

// Project files:
//  - .tero1: a zip with "project.ini" (the same keys of the old projects) and
//    the thumbnails of the timeline ("thumbnails/<ms>d.jpg"), so they are not
//    generated again with ffmpeg when the project is opened, and the shot
//    changes ("shotchanges.txt", the format of the cache of Tero: video time
//    in ms, "-1" in the first line if they are in SMPTE time).
//  - .stp: the old projects (only the ini), still opened.
// The format is detected by the content, not by the extension.

interface

uses
  SysUtils, Classes, IniFiles, Zipper;

const
  TProjectExt       = '.tero1';
  TProjectLegacyExt = '.stp';

type

  { TProjectFile }

  TProjectFile = class
  private
    { Private declarations }
    FFileName    : String;
    FOriginal    : String;
    FTranslation : String;
    FMovieFile   : String;
    FMoviePos    : Integer;
    FFocusedNode : Integer;
    FMovieSize   : Int64;   // size of the video of the thumbnails/shot changes
    FShotChanges : TStringList;
    FThumbsDir   : String;  // to save: the cache folder of the thumbnails
    FThumbCount  : Integer;
    FReady       : Boolean;
    FIsContainer : Boolean; // .tero1 (zip), else an old ini project
    // restore
    FRestoreDir  : String;
    FRestored    : Integer;
    procedure ReadValues(const AIni: TCustomIniFile);
    procedure WriteValues(const AIni: TCustomIniFile);
    procedure DoCreateStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
    procedure DoDoneIniStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
    procedure DoDoneThumbStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
  public
    { Public declarations }
    constructor Create(const FileName: String; const ARead: Boolean = True);
    procedure ReadProject;
    procedure SaveProject;
    // copies the thumbnails saved in the project to the cache folder of the
    // video (only the ones that are not there), if they are of AVideoFile
    function RestoreThumbnails(const ACacheDir, AVideoFile: String): Integer;
    // writes the shot changes saved in the project to AFileName (the cache
    // file of the video), if they are of AVideoFile
    function RestoreShotChanges(const AFileName, AVideoFile: String): Boolean;
    destructor Destroy; override;
    property FileName      : String  read FFileName;
    property Original      : String  read FOriginal      write FOriginal;
    property Translation   : String  read FTranslation   write FTranslation;
    property Movie         : String  read FMovieFile     write FMovieFile;
    property MoviePos      : Integer read FMoviePos      write FMoviePos;
    property FocusedNode   : Integer read FFocusedNode   write FFocusedNode;
    property ThumbnailsDir : String  read FThumbsDir     write FThumbsDir;
    property ThumbnailCount: Integer read FThumbCount;
    // to save, and read: the cache format (see above)
    property ShotChanges   : TStringList read FShotChanges;
    property Ready         : Boolean read FReady;
    property IsContainer   : Boolean read FIsContainer;
  end;

// .tero1 or .stp
function IsProjectFileName(const AFileName: String): Boolean;

// -----------------------------------------------------------------------------

implementation

uses
  ZStream, FPImage, FPReadBMP, FPWriteBMP, FPReadJPEG, FPWriteJPEG;

// -----------------------------------------------------------------------------

const
  HeaderFiles  = 'Subtitle files';
  HeaderMovie  = 'Movie file';
  HeaderOther  = 'Other';
  HeaderProj   = 'Project';
  HeaderThumbs = 'Thumbnails';

  ProjectVersion  = 1;
  ProjectIniName  = 'project.ini';
  ShotChangesName = 'shotchanges.txt';
  ThumbsFolder    = 'thumbnails/';
  ThumbJPEGQuality = 85;

// -----------------------------------------------------------------------------

function IsProjectFileName(const AFileName: String): Boolean;
var
  ext: String;
begin
  ext := LowerCase(ExtractFileExt(AFileName));
  Result := (ext = TProjectExt) or (ext = TProjectLegacyExt);
end;

// -----------------------------------------------------------------------------

// "<ms>d" (the name the thumbnails have in the cache), the ms may be negative.
// Also so a name from the zip can't be a path ("../")
function IsValidThumbName(const AName: String): Boolean;
var
  i, f: Integer;
begin
  Result := False;
  if (Length(AName) < 2) or (AName[Length(AName)] <> 'd') then Exit;
  f := 1;
  if AName[1] = '-' then f := 2;
  if f > Length(AName)-1 then Exit;
  for i := f to Length(AName)-1 do
    if not (AName[i] in ['0'..'9']) then Exit;
  Result := True;
end;

// -----------------------------------------------------------------------------

function IsZipFile(const AFileName: String): Boolean;
var
  fs : TFileStream;
  sig: array[0..1] of Char;
begin
  Result := False;
  try
    fs := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
    try
      Result := (fs.Read(sig, 2) = 2) and (sig[0] = 'P') and (sig[1] = 'K');
    finally
      fs.Free;
    end;
  except
  end;
end;

// -----------------------------------------------------------------------------

function GetFileSize64(const AFileName: String): Int64;
var
  sr: TSearchRec;
begin
  Result := -1;
  if FindFirst(AFileName, faAnyFile, sr) = 0 then
  try
    Result := sr.Size;
  finally
    FindClose(sr);
  end;
end;

// -----------------------------------------------------------------------------

{ TProjectFile }

// -----------------------------------------------------------------------------

constructor TProjectFile.Create(const FileName: String; const ARead: Boolean = True);
begin
  FFileName    := FileName;
  FOriginal    := '';
  FTranslation := '';
  FMovieFile   := '';
  FMoviePos    := 0;
  FFocusedNode := -1;
  FMovieSize   := -1;
  FShotChanges := TStringList.Create;
  FThumbsDir   := '';
  FThumbCount  := 0;
  FReady       := False;
  FIsContainer := False;
  if ARead then ReadProject;
end;

// -----------------------------------------------------------------------------

procedure TProjectFile.ReadValues(const AIni: TCustomIniFile);
begin
  with AIni do
  begin
    FOriginal    := ReadString(HeaderFiles, 'Original', '');
    FTranslation := ReadString(HeaderFiles, 'Translated', '');
    FMovieFile   := ReadString(HeaderMovie, 'Movie', '');
    FMoviePos    := ReadInteger(HeaderMovie, 'Position', 0);
    FFocusedNode := ReadInteger(HeaderOther, 'Focused node', -1);
    FMovieSize   := StrToInt64Def(ReadString(HeaderMovie, 'Size', ''), -1);
    FThumbCount  := ReadInteger(HeaderThumbs, 'Count', 0);
  end;
end;

// -----------------------------------------------------------------------------

procedure TProjectFile.WriteValues(const AIni: TCustomIniFile);
begin
  with AIni do
  begin
    WriteInteger(HeaderProj, 'Version', ProjectVersion);
    WriteString(HeaderFiles, 'Original', FOriginal);
    WriteString(HeaderFiles, 'Translated', FTranslation);
    WriteString(HeaderMovie, 'Movie', FMovieFile);
    WriteInteger(HeaderMovie, 'Position', FMoviePos);
    WriteInteger(HeaderOther, 'Focused node', FFocusedNode);
    if FMovieFile <> '' then
      WriteString(HeaderMovie, 'Size', IntToStr(FMovieSize));
    if FThumbCount > 0 then
      WriteInteger(HeaderThumbs, 'Count', FThumbCount);
  end;
end;

// -----------------------------------------------------------------------------

procedure TProjectFile.DoCreateStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
begin
  AStream := TMemoryStream.Create;
end;

// -----------------------------------------------------------------------------

procedure TProjectFile.DoDoneIniStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
var
  sl  : TStringList;
  ini : TMemIniFile;
begin
  try
    if SameText(AItem.ArchiveFileName, ShotChangesName) then
    begin
      AStream.Position := 0;
      FShotChanges.LoadFromStream(AStream);
    end
    else if SameText(AItem.ArchiveFileName, ProjectIniName) then
    begin
      sl := TStringList.Create;
      ini := TMemIniFile.Create('');
      try
        AStream.Position := 0;
        sl.LoadFromStream(AStream);
        ini.SetStrings(sl);
        ReadValues(ini);
        FReady := True;
      finally
        ini.Free;
        sl.Free;
      end;
    end;
  finally
    FreeAndNil(AStream);
  end;
end;

// -----------------------------------------------------------------------------

// jpg (in the project) -> bmp (in the cache, the format ffmpeg makes and the
// one the timeline loads)
procedure TProjectFile.DoDoneThumbStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
var
  n, dst, tmp : String;
  img : TFPMemoryImage;
  rd  : TFPReaderJPEG;
  wr  : TFPWriterBMP;
begin
  try
    n := AItem.ArchiveFileName;
    if not n.StartsWith(ThumbsFolder) or not SameText(ExtractFileExt(n), '.jpg') then Exit;
    n := ChangeFileExt(Copy(n, Length(ThumbsFolder)+1, MaxInt), '');
    if not IsValidThumbName(n) then Exit;

    dst := IncludeTrailingPathDelimiter(FRestoreDir) + n + '.bmp';
    if FileExists(dst) then Exit;
    tmp := IncludeTrailingPathDelimiter(FRestoreDir) + n + '_part.bmp';

    img := TFPMemoryImage.Create(0, 0);
    rd  := TFPReaderJPEG.Create;
    wr  := TFPWriterBMP.Create;
    try
      try
        AStream.Position := 0;
        img.LoadFromStream(AStream, rd);
        wr.BitsPerPixel := 24;
        img.SaveToFile(tmp, wr);
        // the same as the thumbnails thread: written with another name and
        // renamed when complete, so a cut file is never a valid thumbnail
        if RenameFile(tmp, dst) then
          Inc(FRestored)
        else
          DeleteFile(tmp);
      except
        if FileExists(tmp) then DeleteFile(tmp);
      end;
    finally
      wr.Free;
      rd.Free;
      img.Free;
    end;
  finally
    FreeAndNil(AStream);
  end;
end;

// -----------------------------------------------------------------------------

procedure TProjectFile.ReadProject;
var
  uz  : TUnZipper;
  sl  : TStringList;
  ini : TIniFile;
begin
  FReady := False;
  FIsContainer := False;
  if (FFileName = '') or not FileExists(FFileName) then Exit;

  if IsZipFile(FFileName) then
  begin
    FIsContainer := True;
    uz := TUnZipper.Create;
    sl := TStringList.Create;
    try
      try
        uz.FileName := FFileName;
        uz.OnCreateStream := @DoCreateStream;
        uz.OnDoneStream := @DoDoneIniStream;
        sl.Add(ProjectIniName);
        sl.Add(ShotChangesName);
        uz.UnZipFiles(sl); // not the thumbnails
      except
        FReady := False;
      end;
    finally
      sl.Free;
      uz.Free;
    end;
  end
  else // old project (.stp), only the ini
  begin
    ini := TIniFile.Create(FFileName);
    try
      ReadValues(ini);
      FReady := True;
    finally
      ini.Free;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TProjectFile.SaveProject;
var
  z       : TZipper;
  sl, Names : TStringList;
  ini     : TMemIniFile;
  Streams : TList;
  ms      : TMemoryStream;
  sr      : TSearchRec;
  n, tmp  : String;
  img     : TFPMemoryImage;
  rd      : TFPReaderBMP;
  wr      : TFPWriterJPEG;
  e       : TZipFileEntry;
  i       : Integer;
begin
  if (FFileName = '') then Exit;

  Streams := TList.Create;
  Names := TStringList.Create;
  sl := TStringList.Create;
  try
    // the thumbnails of the cache (jpg, ~10 times smaller than the bmp)
    FThumbCount := 0;
    if FMovieFile <> '' then
      FMovieSize := GetFileSize64(FMovieFile);
    if (FThumbsDir <> '') and DirectoryExists(FThumbsDir) and (FMovieFile <> '') then
    begin
      if FindFirst(IncludeTrailingPathDelimiter(FThumbsDir) + '*d.bmp', faAnyFile, sr) = 0 then
      try
        repeat
          n := ChangeFileExt(sr.Name, '');
          if ((sr.Attr and faDirectory) = 0) and IsValidThumbName(n) then
          begin
            img := TFPMemoryImage.Create(0, 0);
            rd  := TFPReaderBMP.Create;
            wr  := TFPWriterJPEG.Create;
            ms  := TMemoryStream.Create;
            try
              try
                img.LoadFromFile(IncludeTrailingPathDelimiter(FThumbsDir) + sr.Name, rd);
                wr.CompressionQuality := ThumbJPEGQuality;
                img.SaveToStream(ms, wr);
                ms.Position := 0;
                Streams.Add(ms);
                Names.Add(ThumbsFolder + n + '.jpg');
                ms := NIL;
              except
                // a damaged thumbnail is not saved (it will be generated)
              end;
            finally
              ms.Free;
              wr.Free;
              rd.Free;
              img.Free;
            end;
          end;
        until FindNext(sr) <> 0;
      finally
        FindClose(sr);
      end;
      FThumbCount := Streams.Count;
    end;

    // the ini
    ini := TMemIniFile.Create('');
    try
      WriteValues(ini);
      sl.Clear;
      ini.GetStrings(sl);
    finally
      ini.Free;
    end;
    ms := TMemoryStream.Create;
    Streams.Insert(0, ms);
    sl.SaveToStream(ms);
    ms.Position := 0;

    if FShotChanges.Count > 0 then
    begin
      ms := TMemoryStream.Create;
      Streams.Insert(1, ms);
      FShotChanges.SaveToStream(ms);
      ms.Position := 0;
    end;

    // written to a temporary file and renamed at the end: a failed save
    // doesn't destroy the project that was there
    tmp := FFileName + '.tmp';
    z := TZipper.Create;
    try
      z.FileName := tmp;
      z.Entries.AddFileEntry(TStream(Streams[0]), ProjectIniName);
      if FShotChanges.Count > 0 then
        z.Entries.AddFileEntry(TStream(Streams[1]), ShotChangesName);
      for i := Streams.Count-Names.Count to Streams.Count-1 do
      begin
        e := z.Entries.AddFileEntry(TStream(Streams[i]), Names[i-(Streams.Count-Names.Count)]);
        e.CompressionLevel := clnone; // jpg, already compressed
      end;
      try
        z.ZipAllFiles;
      except
        if FileExists(tmp) then DeleteFile(tmp);
        raise;
      end;
    finally
      z.Free;
    end;

    if FileExists(tmp) then
    begin
      if FileExists(FFileName) then DeleteFile(FFileName);
      if not RenameFile(tmp, FFileName) then
        DeleteFile(tmp);
    end;
  finally
    for i := 0 to Streams.Count-1 do
      TStream(Streams[i]).Free;
    Streams.Free;
    Names.Free;
    sl.Free;
  end;
end;

// -----------------------------------------------------------------------------

function TProjectFile.RestoreThumbnails(const ACacheDir, AVideoFile: String): Integer;
var
  uz : TUnZipper;
  sl : TStringList;
  i  : Integer;
begin
  Result := 0;
  if not FIsContainer or (FThumbCount <= 0) or (ACacheDir = '') or
    not FileExists(FFileName) or not FileExists(AVideoFile) then Exit;

  // the thumbnails are of the video of the project: another video (or the
  // same one replaced) would show the frames of the other
  if (FMovieSize < 0) or (GetFileSize64(AVideoFile) <> FMovieSize) or
    not SameFileName(ExtractFileName(AVideoFile), ExtractFileName(FMovieFile)) then Exit;

  if not DirectoryExists(ACacheDir) and not ForceDirectories(ACacheDir) then Exit;

  FRestoreDir := ACacheDir;
  FRestored := 0;
  uz := TUnZipper.Create;
  sl := TStringList.Create;
  try
    try
      uz.FileName := FFileName;
      uz.Examine;
      for i := 0 to uz.Entries.Count-1 do
        if uz.Entries[i].ArchiveFileName.StartsWith(ThumbsFolder) then
          sl.Add(uz.Entries[i].ArchiveFileName);

      if sl.Count > 0 then
      begin
        uz.OnCreateStream := @DoCreateStream;
        uz.OnDoneStream := @DoDoneThumbStream;
        uz.UnZipFiles(sl);
      end;
    except
    end;
  finally
    sl.Free;
    uz.Free;
  end;
  Result := FRestored;
end;

// -----------------------------------------------------------------------------

function TProjectFile.RestoreShotChanges(const AFileName, AVideoFile: String): Boolean;
begin
  Result := False;
  if not FIsContainer or (FShotChanges.Count = 0) or (AFileName = '') or
    not FileExists(AVideoFile) then Exit;

  // the shot changes are of the video of the project
  if (FMovieSize < 0) or (GetFileSize64(AVideoFile) <> FMovieSize) or
    not SameFileName(ExtractFileName(AVideoFile), ExtractFileName(FMovieFile)) then Exit;

  try
    ForceDirectories(ExtractFileDir(AFileName));
    FShotChanges.SaveToFile(AFileName);
    Result := True;
  except
  end;
end;

// -----------------------------------------------------------------------------

destructor TProjectFile.Destroy;
begin
  FShotChanges.Free;
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

end.
