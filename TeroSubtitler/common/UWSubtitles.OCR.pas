{*
 *  URUWorks OCR Script
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

unit UWSubtitles.OCR;

// -----------------------------------------------------------------------------

interface

uses Classes, SysUtils, StrUtils, FGL, fpjson, jsonparser;

type

  { TUWOCRMethod }

  TUWOCRMethod = (rmNone, rmWord, rmLine, rmBegin, rmEnd);

  { TUWOCRItem }

  PUWOCRItem = ^TUWOCRItem;
  TUWOCRItem = packed record
    Find      : String;
    ReplaceBy : String;
    UseRE     : Boolean;
    Method    : TUWOCRMethod;
  end;

  { TUWOCRList }

  TUWOCRList = specialize TFPGList<PUWOCRItem>;

  { TUWOCRScript }

  TUWOCRScript = class
  private
    FOCRList: TUWOCRList;
  public
    constructor Create(const FileName: String);
    destructor Destroy; override;
    procedure Parse(const FileName: String);
    function Fix(const Text: String): String;
    function HasErrors(const Text: String): Boolean;
    property OCR: TUWOCRList read FOCRList;
  end;

// -----------------------------------------------------------------------------

implementation

uses UWSystem.StrUtils, RegExpr;

// -----------------------------------------------------------------------------

constructor TUWOCRScript.Create(const FileName: String);
begin
  FOCRList := TUWOCRList.Create;
  if FileName <> '' then Parse(FileName);
end;

// -----------------------------------------------------------------------------

destructor TUWOCRScript.Destroy;
var
  i: Integer;
begin
  if FOCRList.Count > 0 then
  begin
    for i := FOCRList.Count-1 downto 0 do
    begin
      Dispose(PUWOCRItem(FOCRList.Items[i]));
      FOCRList.Items[i] := NIL;
      FOCRList.Delete(i);
    end;
  end;

  FOCRList.Free;
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TUWOCRScript.Parse(const FileName: String);

  function GetMethod(S: String): TUWOCRMethod;
  begin
    S := UpperCase(S);
    if S = 'WORD' then
      Result := rmWord
    else if S = 'LINE' then
      Result := rmLine
    else if S = 'BEGIN' then
      Result := rmBegin
    else if S = 'END' then
      Result := rmEnd
    else
      Result := rmNone;
  end;

var
  FileStream : TFileStream;
  Parser     : TJSONParser;
  Data       : TJSONData;
  joData,
  joItem     : TJSONObject;
  jaData     : TJSONArray;
  OCRItem    : PUWOCRItem;
  i          : Integer;
  ZeroArray  : TJSONArray;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    Parser := TJSONParser.Create(FileStream);
    ZeroArray := TJSONArray.Create;
    try
      FOCRList.Clear;
      try
        Data := Parser.Parse;
      except
        Exit;
      end;
      ZeroArray.Clear;
      joData := TJSONObject(Data);
      jaData := joData.Get('OCR', ZeroArray);
      for i := 0 to jaData.Count-1 do
      begin
        joItem := jaData.Objects[i];
        New(OCRItem);
        with OCRItem^ do
        begin
          Find      := joItem.Get('Find', '');
          ReplaceBy := joItem.Get('ReplaceBy', '');
          Method    := GetMethod(joItem.Get('Method', ''));
          //UseRE     := joItem.Get('UseRE', True); // this FAIL??
          if joItem.Find('UseRE', Data) and (Data <> NIL) then
            UseRE := Data.AsBoolean
          else
            UseRE := True;
        end;

        FOCRList.Add(OCRItem);
      end;
    finally
      ZeroArray.Free;
      FreeAndNil(Parser);
    end;
  finally
    FileStream.Destroy;
  end;
end;

// -----------------------------------------------------------------------------

function TUWOCRScript.Fix(const Text: String): String;
var
  i: Integer;
begin
  Result := Text;
  if FOCRList.Count = 0 then Exit;

  for i := 0 to FOCRList.Count-1 do
    if (FOCRList[i]^.ReplaceBy <> #0) then
    begin
      if FOCRList[i]^.UseRE and (FOCRList[i]^.Method = rmNone) then
      begin
        try
          Result := ReplaceRegExpr(FOCRList[i]^.Find, Result, FOCRList[i]^.ReplaceBy, True);
        except
          Result := Text;
        end;
      end
      else
        case FOCRList[i]^.Method of
          rmWord : Result := ReplaceWordString(Result, FOCRList[i]^.Find, FOCRList[i]^.ReplaceBy, False);
          rmLine : if Result = FOCRList[i]^.Find then Result := FOCRList[i]^.ReplaceBy;
          rmBegin: if StartsText(FOCRList[i]^.Find, Result) then
                   begin
                     Delete(Result, 1, Length(FOCRList[i]^.Find));
                     Result := FOCRList[i]^.ReplaceBy + Result;
                   end;
          rmEnd  : if EndsText(FOCRList[i]^.Find, Result) then
                     Result := Copy(Result, 1, Length(Result)-Length(FOCRList[i]^.Find)) + FOCRList[i]^.ReplaceBy;
          else
            Result := ReplaceString(Result, FOCRList[i]^.Find, FOCRList[i]^.ReplaceBy);
        end;
    end;
end;

// -----------------------------------------------------------------------------

function TUWOCRScript.HasErrors(const Text: String): Boolean;
begin
  Result := Fix(Text) <> Text;
end;

// -----------------------------------------------------------------------------

end.
