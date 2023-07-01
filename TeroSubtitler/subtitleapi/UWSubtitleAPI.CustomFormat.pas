{*
 *  URUWorks Custom format
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

unit UWSubtitleAPI.CustomFormat;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, UWSystem.FileUtils, UWSystem.StrUtils, UWSystem.SysUtils,
  IniFiles;

type

  { TUWSubtitleTemplate }

  TUWSubtitleTemplate = class
  private
    { Private declarations }
    FFileName     : String;
    FName         : String;
    FExtension    : ShortString;
    FTimeFormat   : ShortString;
    FTime         : Boolean;
    FDecSeparator : Char;
    FSubPadding   : Byte;
    FFPSPadding   : Byte;
    FHeader       : String;
    FBody         : String;
    FFooter       : String;
    FText         : String;
    FSuccess      : Boolean;
    procedure ReadTemplate(const ReadOnlyHeader: Boolean = False); virtual;
    procedure ReadFormatting;
  public
    { Public declarations }
    constructor Create(const AFileName: String; const ReadOnlyHeader: Boolean = False);
    procedure LoadFromFile(const AFileName: String; const ReadOnlyHeader: Boolean = False);
    procedure SaveToFile(const AFileName: String); virtual;
    procedure Clear;
    procedure ReadFormattingFromStrings(const Buffer: TStrings);
    property FileName     : String      read FFileName;
    property Success      : Boolean     read FSuccess;
    property Name         : String      read FName         write FName;
    property Extension    : ShortString read FExtension    write FExtension;
    property TimeFormat   : ShortString read FTimeFormat   write FTimeFormat;
    property Time         : Boolean     read FTime         write FTime;
    property DecSeparator : Char        read FDecSeparator write FDecSeparator;
    property SubPadding   : Byte        read FSubPadding   write FSubPadding;
    property FPSPadding   : Byte        read FFPSPadding   write FFPSPadding;
    property Header       : String      read FHeader       write FHeader;
    property Body         : String      read FBody         write FBody;
    property Footer       : String      read FFooter       write FFooter;
    property Text         : String      read FText         write FText;
  end;

  { TUWSubtitleCustomTextFormat }

  TUWSubtitleCustomTextFormat = class(TUWSubtitleTemplate)
  private
    { Private declarations }
    FFPS     : Single;
    FNewLine : String;
    procedure ReadTemplate(const ReadOnlyHeader: Boolean = False); override;
  public
    { Public declarations }
    procedure SaveToFile(const AFileName: String); override;
    property FPS         : Single read FFPS     write FFPS;
    property NewLineChar : String read FNewLine write FNewLine;
  end;

  { TUWSubtitleExportFormat }

  TUWSubtitleExportFormat = class(TUWSubtitleTemplate)
  private
    { Private declarations }
    FImageFormats : String;
    FTransparent  : Integer;
    procedure ReadTemplate(const ReadOnlyHeader: Boolean = False); override;
  public
    { Public declarations }
    property ImageFormats     : String  read FImageFormats write FImageFormats;
    property TransparentColor : Integer read FTransparent  write FTransparent;
  end;

// -----------------------------------------------------------------------------

implementation

type
  TSection = (scHeader, scBody, scFooter);

const
  InfoHeader = 'Information';

// -----------------------------------------------------------------------------

{ TUWSubtitleTemplate }

// -----------------------------------------------------------------------------

constructor TUWSubtitleTemplate.Create(const AFileName: String; const ReadOnlyHeader: Boolean = False);
begin
  LoadFromFile(AFileName, ReadOnlyHeader);
end;

// -----------------------------------------------------------------------------

procedure TUWSubtitleTemplate.LoadFromFile(const AFileName: String; const ReadOnlyHeader: Boolean = False);
begin
  FFileName := AFileName;
  Clear;
  if FFileName <> '' then
    ReadTemplate(ReadOnlyHeader);
end;

// -----------------------------------------------------------------------------

procedure TUWSubtitleTemplate.SaveToFile(const AFileName: String);
begin
  // Override this!
end;

// -----------------------------------------------------------------------------

procedure TUWSubtitleTemplate.Clear;
begin
  FSuccess    := False;
  FHeader     := '';
  FBody       := '';
  FFooter     := '';
  FText       := '';
  FSubPadding := 0;
  FFPSPadding := 0;
end;

// -----------------------------------------------------------------------------

procedure TUWSubtitleTemplate.ReadTemplate(const ReadOnlyHeader: Boolean = False);
begin
  if not ReadOnlyHeader then
    ReadFormatting
  else
    FSuccess := True;
end;

// -----------------------------------------------------------------------------

procedure TUWSubtitleTemplate.ReadFormatting;
var
  S, Tmp  : String;
  Found   : Boolean;
  Section : TSection;
begin
  with TUWTextFile.Create(FFileName, fmOpenRead) do
  try
    if Ready then
    begin
      Found   := False;
      Section := scHeader;

      while not EOF do
      begin
        ReadLn(S);
        Tmp := Trim(LowerCase(S));

        if not Found then
        begin
          if Tmp = '[format text]' then
          begin
            Found := True;
            Continue;
          end;
        end
        else
        begin
          FText := iff(FText = '', FText + S, FText + sLineBreak + S);

          if Tmp = '{repeatsub}' then
          begin
            Section := scBody;
            Continue;
          end
          else if Tmp = '{endrepeat}' then
          begin
            Section := scFooter;
            Continue;
          end;

          case Section of
            scHeader : FHeader := iff(FHeader = '', S, FHeader + sLineBreak + S);
            scBody   : FBody   := iff(FBody = '', S, FBody + sLineBreak + S);
            scFooter : FFooter := iff(FFooter = '', S, FFooter + sLineBreak + S);
          end;
        end;
      end;

      if Found and ((FHeader <> '') or (FBody <> '') or (FFooter <> '')) then
        FSuccess := True;
    end;
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWSubtitleTemplate.ReadFormattingFromStrings(const Buffer: TStrings);
var
  i       : Integer;
  S, Tmp  : String;
  Section : TSection;
begin
  if (Buffer = NIL) or (Buffer.Count = 0) then Exit;

  with Buffer do
  begin
    Section := scHeader;
    for i := 0 to Count - 1 do
    begin
      S   := Buffer.Strings[i];
      Tmp := Trim(LowerCase(S));

      if Tmp = '{repeatsub}' then
      begin
        Section := scBody;
        Continue;
      end
      else if Tmp = '{endrepeat}' then
      begin
        Section := scFooter;
        Continue;
      end;

      case Section of
        scHeader : FHeader := iff(FHeader = '', S, FHeader + sLineBreak + S);
        scBody   : FBody   := iff(FBody = '', S, FBody + sLineBreak + S);
        scFooter : FFooter := iff(FFooter = '', S, FFooter + sLineBreak + S);
      end;
    end;
    if ((FHeader <> '') or (FBody <> '') or (FFooter <> '')) then FSuccess := True;
  end;
end;

// -----------------------------------------------------------------------------

{ TUWSubtitleCustomTextFormat }

// -----------------------------------------------------------------------------

procedure TUWSubtitleCustomTextFormat.ReadTemplate(const ReadOnlyHeader: Boolean = False);
begin
  if (FFileName <> '') and FileExists(FFileName) then
    with TIniFile.Create(FFileName) do
    try
      FName         := ReadString(InfoHeader, 'Name', '');
      FExtension    := ReadString(InfoHeader, 'Extension', '');
      FTimeFormat   := ReadString(InfoHeader, 'Time Structure', 'hh:mm:ss,zzz');
      FTime         := ReadBool(InfoHeader, 'Time', True);
      FFPS          := ReadInteger(InfoHeader, 'FPS', 25);
      FDecSeparator := ReadString(InfoHeader, 'Decimal separator', ',')[1];
      FNewLine      := ReadString(InfoHeader, 'New line char', '|');
      FSubPadding   := ReadInteger(InfoHeader, 'SubPadding', 0);
      FFPSPadding   := ReadInteger(InfoHeader, 'FPSPadding', 0);

      if (FName <> '') then
        inherited ReadTemplate(ReadOnlyHeader);
    finally
      Free;
    end;
end;

// -----------------------------------------------------------------------------

procedure TUWSubtitleCustomTextFormat.SaveToFile(const AFileName: String);
begin
  with TUWTextFile.Create(AFileName, fmOpenWrite or fmShareDenyWrite, True) do
  try
    if Ready then
    begin
      WriteLn(';');
      WriteLn('; URUWorks Tero Subtitler - Subtitle Template File');
      WriteLn(';');
      WriteLn('');
      WriteLn('[' + InfoHeader + ']');
      WriteLn('Name=' + FName);
      WriteLn('Extension=' + FExtension);
      WriteLn('Time Structure=' + FTimeFormat);
      WriteLn('Time=' + IntToStr(Integer(FTime)));
      WriteLn('FPS=' + Format('%.3f', [FFPS]));
      WriteLn('New line char=' + FNewLine);
      WriteLn('SubPadding=' + IntToStr(Integer(FSubPadding)));
      WriteLn('FPSPadding=' + IntToStr(Integer(FFPSPadding)));
      WriteLn('');
      WriteLn('[Format text]');
      if FText <> '' then
        WriteLn(FText)
      else
      begin
        if FHeader <> '' then WriteLn(FHeader);
        WriteLn(FBody);
        if FFooter <> '' then WriteLn(FFooter);
      end;
    end;
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

{ TUWSubtitleExportFormat }

// -----------------------------------------------------------------------------

procedure TUWSubtitleExportFormat.ReadTemplate(const ReadOnlyHeader: Boolean = False);
begin
  if (FFileName <> '') and FileExists(FFileName) then
    with TIniFile.Create(FFileName) do
    try
      FName         := ReadString(InfoHeader, 'Name', '');
      FExtension    := ReadString(InfoHeader, 'Extension', '');
      FTimeFormat   := ReadString(InfoHeader, 'Time Structure', 'hh:mm:ss:zz');
      FTime         := ReadBool(InfoHeader, 'Time', True);
      FDecSeparator := ReadString(InfoHeader, 'Decimal separator', '.')[1];
      FImageFormats := ReadString(InfoHeader, 'Image format', 'bmp8');
      FTransparent  := HexStrToInt(ReadString(InfoHeader, 'Transparent color', 'FF00FF'));
      FSubPadding   := ReadInteger(InfoHeader, 'SubPadding', 0);
      FFPSPadding   := ReadInteger(InfoHeader, 'FPSPadding', 0);

      if (FName <> '') then
        inherited ReadTemplate(ReadOnlyHeader);
    finally
      Free;
    end;
end;

// -----------------------------------------------------------------------------

end.

