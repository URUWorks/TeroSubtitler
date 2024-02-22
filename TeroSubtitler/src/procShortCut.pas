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

unit procShortCut;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ActnList;

procedure LoadShortCutsFromFile(const AFileName: String; const AActionList: TActionList);
procedure SaveShortCutsToFile(const FileName: String; const AActionList: TActionList);

// -----------------------------------------------------------------------------

implementation

uses
  fpjson, jsonparser, LCLProc, procDialogs, procLocalize;

// -----------------------------------------------------------------------------

procedure LoadShortCutsFromFile(const AFileName: String; const AActionList: TActionList);
var
  FileStream : TFileStream;
  Parser     : TJSONParser;
  Data       : TJSONData;
  joData,
  joItem     : TJSONObject;
  jaData     : TJSONArray;
  Comp       : TAction;
  i          : Integer;
  s          : String;
  ZeroArray  : TJSONArray;
begin
  if not FileExists(AFileName) then Exit;

  FileStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    Parser := TJSONParser.Create(FileStream);
    ZeroArray := TJSONArray.Create;
    try
      try
        Data := Parser.Parse;
      except
        Exit;
      end;

      for i := 0 to AActionList.ActionCount-1 do
        TAction(AActionList.Actions[i]).ShortCut := 0;

      ZeroArray.Clear;
      joData := TJSONObject(Data);
      if not Assigned(joData) then Exit; //Access violation #266

      jaData := joData.Get('Shortcuts', ZeroArray);
      for i := 0 to jaData.Count-1 do
      begin
        joItem := jaData.Objects[i];
        Comp   := TAction(AActionList.ActionByName(joItem.Get('Action', '')));
        if Comp <> NIL then
        begin
          s := joItem.Get('Key', '');
          Comp.ShortCut := TextToShortCutRaw(s);
        end;
      end;
    finally
      ZeroArray.Free;
      FreeAndNil(Parser);
    end;
  finally
    FileStream.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure SaveShortCutsToFile(const FileName: String; const AActionList: TActionList);
var
  FileStream : TFileStream;
  joData     : TJSONObject;
  jaData     : TJSONArray;
  s          : TJSONStringType;
  j          : TJSONData;
  i          : Integer;
  path       : String;
begin
  if not Assigned(AActionList) and (AActionList.ActionCount = 0) then Exit;

  path := ExtractFilePath(FileName);
  if not DirectoryExists(path) then
    if not CreateDir(path) then
    begin
      ShowErrorMessageDialog(lngSaveSubtitleError);
      Exit;
    end;

  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    joData := TJSONObject.Create;
    jaData := TJSONArray.Create;
    try
      for i := 0 to AActionList.ActionCount-1 do
        with TAction(AActionList.Actions[i]) do
          if ShortCut <> 0 then
          begin
            s := '{"Action" : "' + Name +
                 '", "Key" : "'  + ShortCutToTextRaw(ShortCut) + '"}';
            j := GetJSON(s);
            jaData.Add(j);
          end;

      joData.Add('Shortcuts', jaData);
      s := joData.AsJSON;
      try
        FileStream.WriteBuffer(s[1], Length(s));
      except
        ShowErrorMessageDialog(lngSaveSubtitleError);
      end;
    finally
      joData.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

// -----------------------------------------------------------------------------

end.

