{*
 *  URUWorks Hunspell API
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

unit UWSpellcheck.Hunspell;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, Classes, dynlibs, LConvEncoding;

type

  { TUWHunspell }

  TUWHunspell = class
  private
    hunspell_create    : function(aff_file: PChar; dict_file: PChar): Pointer; cdecl;
    hunspell_destroy   : procedure(spell: Pointer); cdecl;
    // int Hunspell_spell(...): a C int, not a (one byte) Boolean
    hunspell_spell     : function(spell: Pointer; word: PChar): Integer; cdecl;
    hunspell_suggest   : function(spell: Pointer; var suggestions: PPChar; word: PChar): Integer; cdecl;
    hunspell_free_list : procedure(spell: Pointer; var suggestions: PPChar; suggestLen: Integer); cdecl;
    hunspell_add       : function(spell: Pointer; word: PChar): Integer; cdecl;
    hunspell_get_dic_encoding : function(spell: Pointer): PChar; cdecl;
    FHandle   : TLibHandle;
    FSpell    : Pointer;
    FEncoding : String; // dictionary encoding ('' = UTF-8, like our strings)
    function ToDic(const AWord: String): String;
    function FromDic(const AWord: String): String;
  public
    constructor Create(const LibraryName: String = {$IFDEF MSWINDOWS}{$IFDEF WIN32}'libhunspellx86.dll'{$ELSE}'libhunspellx64.dll'{$ENDIF}{$ELSE}
      {$IFDEF UNIX}{$IFDEF DARWIN}{$IFDEF CPU32}'libhunspellx86.dylib'{$ELSE}'libhunspellx64.dylib'{$ENDIF}{$ELSE}
      {$IFDEF CPU32}'libhunspellx86.so'{$ELSE}'libhunspellx64.so'{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF});
    destructor Destroy; override;
    function LoadHunspell(const LibraryName: String): Boolean;
    function Ready: Boolean;
    function LoadDictionary(const aff, dict: String): Boolean;
    procedure UnloadDictionary;
    function Spell(const AWord: String): Boolean;
    function Suggest(const AWord: String; var ASuggests: TStrings): Boolean;
    procedure Add(const AWord: String);
    function GetCorrectLibrayFileName: String;
  end;

var
  HunspellInstance: TUWHunspell = NIL;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

constructor TUWHunspell.Create(const LibraryName: String = {$IFDEF MSWINDOWS}{$IFDEF WIN32}'libhunspellx86.dll'{$ELSE}'libhunspellx64.dll'{$ENDIF}{$ELSE}
  {$IFDEF UNIX}{$IFDEF DARWIN}{$IFDEF CPU32}'libhunspellx86.dylib'{$ELSE}'libhunspellx64.dylib'{$ENDIF}{$ELSE}
  {$IFDEF CPU32}'libhunspellx86.so'{$ELSE}'libhunspellx64.so'{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF});
begin
  inherited Create;

  FHandle := 0;
  FSpell  := NIL;
  LoadHunspell(LibraryName);
end;

// -----------------------------------------------------------------------------

destructor TUWHunspell.Destroy;
begin
  UnloadDictionary;

  if FSpell <> NIL then FSpell := NIL;
  if FHandle <> 0 then FreeLibrary(FHandle);

  inherited Destroy;
end;

// -----------------------------------------------------------------------------

function TUWHunspell.LoadHunspell(const LibraryName: String): Boolean;
begin
  Result := False;
  if (LibraryName = '') or (FHandle <> 0) or not FileExists(LibraryName) then Exit;

  FHandle := LoadLibrary(LibraryName);
  if FHandle <> 0 then
  begin
    Result := True;

    Pointer(hunspell_create) := GetProcAddress(FHandle, 'Hunspell_create');
    Pointer(hunspell_destroy) := GetProcAddress(FHandle, 'Hunspell_destroy');
    Pointer(hunspell_spell) := GetProcAddress(FHandle, 'Hunspell_spell');
    Pointer(hunspell_suggest) := GetProcAddress(FHandle, 'Hunspell_suggest');
    Pointer(hunspell_free_list) := GetProcAddress(FHandle, 'Hunspell_free_list');
    Pointer(hunspell_add) := GetProcAddress(FHandle, 'Hunspell_add');
    // optional
    Pointer(hunspell_get_dic_encoding) := GetProcAddress(FHandle, 'Hunspell_get_dic_encoding');

    if not Assigned(hunspell_create) or not Assigned(hunspell_destroy) or
      not Assigned(hunspell_spell) or not Assigned(hunspell_suggest) or
      not Assigned(hunspell_free_list) or not Assigned(hunspell_add) then
    begin
      FreeLibrary(FHandle);
      Result := False;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function TUWHunspell.Ready: Boolean;
begin
  Result := FHandle <> 0;
end;

// -----------------------------------------------------------------------------

function TUWHunspell.LoadDictionary(const aff, dict: String): Boolean;
var
  e: String;
begin
  Result := False;
  if not Ready or not FileExists(aff) or not FileExists(dict) then Exit;

  // free the previous one (before, every change of dictionary left the
  // previous instance loaded)
  UnloadDictionary;

  if Assigned(hunspell_create) then
    FSpell := hunspell_create(PChar(aff), PChar(dict));

  // words must be passed in the dictionary encoding (e.g. ISO8859-1): UTF-8
  // bytes in an 8 bit dictionary are invalid input for hunspell
  FEncoding := '';
  if Assigned(FSpell) and Assigned(hunspell_get_dic_encoding) then
  begin
    e := StrPas(hunspell_get_dic_encoding(FSpell));
    if not SameText(StringReplace(e, '-', '', [rfReplaceAll]), 'UTF8') then
      FEncoding := e;
  end;

  Result := FSpell <> NIL;
end;

// -----------------------------------------------------------------------------

function TUWHunspell.ToDic(const AWord: String): String;
begin
  // typographic apostrophes (’ ‘ ʼ) as ', the dictionaries use it, and some
  // hunspell versions crash with dictionaries converting them (ICONV ’ ')
  Result := StringReplace(AWord, #$E2#$80#$99, '''', [rfReplaceAll]);
  Result := StringReplace(Result, #$E2#$80#$98, '''', [rfReplaceAll]);
  Result := StringReplace(Result, #$CA#$BC, '''', [rfReplaceAll]);

  if FEncoding <> '' then
    Result := ConvertEncoding(Result, EncodingUTF8, FEncoding);
end;

// -----------------------------------------------------------------------------

function TUWHunspell.FromDic(const AWord: String): String;
begin
  if FEncoding = '' then
    Result := AWord
  else
    Result := ConvertEncoding(AWord, FEncoding, EncodingUTF8);
end;

// -----------------------------------------------------------------------------

procedure TUWHunspell.UnloadDictionary;
begin
  if Assigned(FSpell) and Assigned(hunspell_destroy) then
  begin
    hunspell_destroy(FSpell);
    FSpell := NIL;
    FEncoding := '';
  end;
end;

// -----------------------------------------------------------------------------

function TUWHunspell.Spell(const AWord: String): Boolean;
var
  w: String;
begin
  Result := False;
  if (AWord = '') or not Assigned(FSpell) or not Assigned(hunspell_spell) then Exit;
  w := ToDic(AWord);
  Result := hunspell_spell(FSpell, PChar(w)) <> 0;
end;

// -----------------------------------------------------------------------------

function TUWHunspell.Suggest(const AWord: String; var ASuggests: TStrings): Boolean;
var
  l, i : Integer;
  s, w : PPChar;
  d    : String;
begin
  if (ASuggests = NIL) then ASuggests := TStringList.Create;
  ASuggests.Clear;

  if (AWord <> '') and Assigned(FSpell) and Assigned(hunspell_suggest) then
  begin
    s := NIL;
    d := ToDic(AWord);
    l := hunspell_suggest(FSpell, s, PChar(d));
    if (l > 0) and (s <> NIL) then
    begin
      w := s;
      for i := 1 to l do
      begin
        if w^ <> NIL then
          if Pos(#$E2#$80#$99, AWord) > 0 then
            // as it was written, Kun’s -> Ken’s
            ASuggests.Add(StringReplace(FromDic(StrPas(w^)), '''', #$E2#$80#$99, [rfReplaceAll]))
          else
            ASuggests.Add(FromDic(StrPas(w^)));
        Inc(w);
      end;
    end;

    if (s <> NIL) and Assigned(hunspell_free_list) then hunspell_free_list(FSpell, s, l);
  end;

  Result := ASuggests.Count > 0;
end;

// -----------------------------------------------------------------------------

procedure TUWHunspell.Add(const AWord: String);
begin
  if Assigned(FSpell) and Assigned(hunspell_add) then
    if AWord <> '' then hunspell_add(FSpell, PChar(ToDic(AWord)));
end;

// -----------------------------------------------------------------------------

function TUWHunspell.GetCorrectLibrayFileName: String;
begin
  Result := '';
  {$IFDEF MSWINDOWS} // windows
    {$IFDEF WIN32}
      Result := 'libhunspellx86.dll';
    {$ELSE}
      Result := 'libhunspellx64.dll';
    {$ENDIF}
  {$ELSE}
    {$IFDEF UNIX}
      {$IFDEF DARWIN} // mac
        {$IFDEF CPU32}
          Result := 'libhunspellx86.dylib';
        {$ELSE}
          Result := 'libhunspellx64.dylib';
        {$ENDIF}
      {$ELSE} // linux
        {$IFDEF CPU32}
          Result := 'libhunspellx86.so';
        {$ELSE}
          Result := 'libhunspellx64.so';
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

initialization
  HunspellInstance := TUWHunspell.Create;

// -----------------------------------------------------------------------------

finalization
  FreeAndNil(HunspellInstance);

// -----------------------------------------------------------------------------

end.
