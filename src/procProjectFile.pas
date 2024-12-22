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

unit procProjectFile;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, IniFiles;

type
  TProjectFile = class
  private
    { Private declarations }
    FFileName    : String;
    FOriginal    : String;
    FTranslation : String;
    FMovieFile   : String;
    FMoviePos    : Integer;
    FFocusedNode : Integer;
    FReady       : Boolean;
  public
    { Public declarations }
    constructor Create(const FileName: String; const ARead: Boolean = True);
    procedure ReadProject;
    procedure SaveProject;
    property FileName    : String  read FFileName;
    property Original    : String  read FOriginal    write FOriginal;
    property Translation : String  read FTranslation write FTranslation;
    property Movie       : String  read FMovieFile   write FMovieFile;
    property MoviePos    : Integer read FMoviePos    write FMoviePos;
    property FocusedNode : Integer read FFocusedNode write FFocusedNode;
    property Ready       : Boolean read FReady;
  end;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

const
  HeaderFiles = 'Subtitle files';
  HeaderMovie = 'Movie file';
  HeaderOther = 'Other';

// -----------------------------------------------------------------------------

constructor TProjectFile.Create(const FileName: String; const ARead: Boolean = True);
begin
  FFileName    := FileName;
  FOriginal    := '';
  FTranslation := '';
  FMovieFile   := '';
  FMoviePos    := 0;
  FFocusedNode := -1;
  FReady       := False;
  if ARead then ReadProject;
end;

// -----------------------------------------------------------------------------

procedure TProjectFile.ReadProject;
begin
  FReady := False;
  if (FFileName <> '') and FileExists(FFileName) then
    with TIniFile.Create(FFileName) do
    try
      FOriginal    := ReadString(HeaderFiles, 'Original', '');
      FTranslation := ReadString(HeaderFiles, 'Translated', '');
      FMovieFile   := ReadString(HeaderMovie, 'Movie', '');
      FMoviePos    := ReadInteger(HeaderMovie, 'Position', 0);
      FFocusedNode := ReadInteger(HeaderOther, 'Focused node', -1);

      FReady := True;
    finally
      Free;
    end;
end;

// -----------------------------------------------------------------------------

procedure TProjectFile.SaveProject;
begin
  if (FFileName <> '') then
    with TIniFile.Create(FFileName) do
    try
      WriteString(HeaderFiles, 'Original', FOriginal);
      WriteString(HeaderFiles, 'Translated', FTranslation);
      WriteString(HeaderMovie, 'Movie', FMovieFile);
      WriteInteger(HeaderMovie, 'Position', FMoviePos);
      WriteInteger(HeaderOther, 'Focused node', FFocusedNode);
    finally
      Free;
    end;
end;

// -----------------------------------------------------------------------------

end.

