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

unit UWSubtitleAPI.Tags;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils;

{ Tags Helpers }

function RemoveTSTags(const Text: String): String;
function TSTagsToHTML(const Text: String): String;
function HTMLTagsToTS(const Text: String): String;
function TSTagsToMicroDVD(const Text: String): String;
function MicroDVDTagsToTS(const Text: String): String;
function TSTagsToXML(const Text: String): String;
function XMLTagsToTS(const Text: String): String;
function MacDVDTagsToTS(const Text: String): String;
function TSToMacDVDTags(const Text: String): String;

// -----------------------------------------------------------------------------

implementation

uses UWSystem.StrUtils, RegExpr;

// -----------------------------------------------------------------------------

{ Tags Helpers }

//------------------------------------------------------------------------------

function RemoveTSTags(const Text: String): String;
begin
  Result := ReplaceRegExpr('\{.*?\}', Text, '', True);
end;

//------------------------------------------------------------------------------

function ReplaceHTMLTagColor(const Text: String): String;
var
  a, b, x : Integer;
  c, s : String;
begin
  Result := Text;
  x := 1;
  repeat
    a := Pos('{\c&', Result, x);
    if a > 0 then
    begin
      x := a+1;
      b := Pos('&}', Result, x);
      if b > a then
      begin
        c := Copy(Result, a+4, 6);
        if c.Length = 6 then
        begin
          s := Format('%s%s%s', [Copy(c, 5, 2), Copy(c, 3, 2), Copy(c, 1, 2)]);
          Result := Result.Remove(a+3, 6);
          Result := Result.Insert(a+3, s);
        end;
      end;
    end;
  until a = 0;
end;

//------------------------------------------------------------------------------

function TSTagsToHTML(const Text: String): String;
begin
  Result := Text;
  if Result.IsEmpty then Exit;

  Result := ReplaceHTMLTagColor(Result);
  Result := ReplaceRegExpr('\{c&(.*?|var)\&}', Result, '<font color="#$1">', True);
  Result := ReplaceString(Result, '{\c}', '</font>');
  Result := ReplaceString(Result, '{\b1}', '<b>');
  Result := ReplaceString(Result, '{\i1}', '<i>');
  Result := ReplaceString(Result, '{\u1}', '<u>');
  Result := ReplaceString(Result, '{\s1}', '<s>');
  Result := ReplaceString(Result, '{\b0}', '</b>');
  Result := ReplaceString(Result, '{\i0}', '</i>');
  Result := ReplaceString(Result, '{\u0}', '</u>');
  Result := ReplaceString(Result, '{\s0}', '</s>');
end;

// -----------------------------------------------------------------------------

function HTMLTagsToTS(const Text: String): String;
begin
  Result := Text;
  if Result.IsEmpty then Exit;

  Result := ReplaceString(Result, 'color="#', 'color="');
  Result := ReplaceRegExpr('<font color="(.*?|var)">', Result, '{\\c&$1&}', True);
  Result := ReplaceHTMLTagColor(Result);
  Result := ReplaceString(Result, '</font>', '{\c}');
  Result := ReplaceString(Result, '<b>', '{\b1}');
  Result := ReplaceString(Result, '<i>', '{\i1}');
  Result := ReplaceString(Result, '<u>', '{\u1}');
  Result := ReplaceString(Result, '<s>', '{\s1}');
  Result := ReplaceString(Result, '</b>', '{\b0}');
  Result := ReplaceString(Result, '</i>', '{\i0}');
  Result := ReplaceString(Result, '</u>', '{\u0}');
  Result := ReplaceString(Result, '</s>', '{\s0}');
end;

// -----------------------------------------------------------------------------

function TSTagsToMicroDVD(const Text: String): String;
begin
  Result := Text;
  if Result.IsEmpty then Exit;

  Result := ReplaceString(Result, '{\i1}', '{y:i}');
  Result := ReplaceString(Result, '{\i0}', '');
  Result := ReplaceString(Result, '{\b1}', '{y:b}');
  Result := ReplaceString(Result, '{\b0}', '');
  Result := ReplaceString(Result, '{\u1}', '{y:u}');
  Result := ReplaceString(Result, '{\u0}', '');
  Result := ReplaceString(Result, '{\s1}', '{y:s}');
  Result := ReplaceString(Result, '{\s0}', '');
end;

// -----------------------------------------------------------------------------

function MicroDVDTagsToTS(const Text: String): String;
begin
  Result := Text;
  if Result.IsEmpty then Exit;

  Result := ReplaceString(Result, '{y:i}', '{\i1}');
  Result := ReplaceString(Result, '{y:b}', '{\b1}');
  Result := ReplaceString(Result, '{y:u}', '{\u1}');
  Result := ReplaceString(Result, '{y:s}', '{\s1}');
  Result := ReplaceRegExpr('{c:$(.*?|var)}', Result, '{\c&$1&}', True);
  Result := ReplaceRegExpr('{(.*?)}', Result, '', True);
end;

// -----------------------------------------------------------------------------

function TSTagsToXML(const Text: String): String;
begin
  Result := Text;
  if Result.IsEmpty then Exit;

  Result := ReplaceRegExpr('\{\\b1}(.*?)\{\\b0}', Result, '<span style="bold">$1</span>', True);
  Result := ReplaceRegExpr('\{\\i1}(.*?)\{\\i0}', Result, '<span style="italic">$1</span>', True);
  Result := ReplaceRegExpr('\{\\u1}(.*?)\{\\u0}', Result, '<span style="underline">$1</span>', True);
  Result := ReplaceRegExpr('\{\\s1}(.*?)\{\\s0}', Result, '<span style="strikeout">$1</span>', True);
  Result := ReplaceRegExpr('{(.*?)}', Result, '', True);
end;

// -----------------------------------------------------------------------------

function XMLTagsToTS(const Text: String): String;
begin
  Result := Text;
  if Result.IsEmpty then Exit;

  Result := ReplaceRegExpr('<span (s|.*S)tyle="bold">(.*?)<\/span>', Result, '{\\b1}$2{\\b0}', True);
  Result := ReplaceRegExpr('<span (s|.*S)tyle="italic">(.*?)<\/span>', Result, '{\\i1}$2{\\i0}', True);
  Result := ReplaceRegExpr('<span (s|.*S)tyle="underline">(.*?)<\/span>', Result, '{\\u1}$2{\\u0}', True);
  Result := ReplaceRegExpr('<span (s|.*S)tyle="strikeout">(.*?)<\/span>', Result, '{\\s1}$2{\\s0}', True);
  // unsupported tags for now
  Result := ReplaceRegExpr('<span (s|.*S)tyle=".*">(.*?)<\/span>', Result, '$2', True);
end;

// -----------------------------------------------------------------------------

function MacDVDTagsToTS(const Text: String): String;
begin
  Result := Text;
  if Result.IsEmpty then Exit;

  Result := ReplaceRegExpr('\^B(.*?)\^B', Result, '{\\b1}$1{\\b0}', True);
  Result := ReplaceRegExpr('\^I(.*?)\^I', Result, '{\\i1}$1{\\i0}', True);
  Result := ReplaceRegExpr('\^U(.*?)\^U', Result, '{\\u1}$1{\\u0}', True);
  Result := ReplaceRegExpr('\^S(.*?)\^S', Result, '{\\s1}$1{\\s0}', True);
end;

// -----------------------------------------------------------------------------

function TSToMacDVDTags(const Text: String): String;
begin
  Result := Text;
  if Result.IsEmpty then Exit;

  Result := ReplaceRegExpr('\{\\b1\}(.*?)\{\\b0\}', Result, '\^B$1\^B', True);
  Result := ReplaceRegExpr('\{\\i1\}(.*?)\{\\i0\}', Result, '\^I$1\^I', True);
  Result := ReplaceRegExpr('\{\\u1\}(.*?)\{\\u0\}', Result, '\^U$1\^U', True);
  Result := ReplaceRegExpr('\{\\s1\}(.*?)\{\\s0\}', Result, '\^S$1\^S', True);
end;

// -----------------------------------------------------------------------------

end.
