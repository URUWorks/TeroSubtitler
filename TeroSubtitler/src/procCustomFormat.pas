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

unit procCustomFormat;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, UWSubtitleAPI;

function CFReplaceHeaderTags(const S, TimeFormat, FPS: String; const W, H: Integer): String;
function CFReplaceBodyTags(const S, TimeFormat, FPS: String; const Item: TUWSubtitleItem; const InFrames: Boolean; const NewChar: String; const Index: Integer; const ImageFile: String = ''; const W: Integer = 0; const H: Integer = 0): String;
function CFFixExtension(const AFileName, AExtension: String): String;

// -----------------------------------------------------------------------------

implementation

uses
  procTypes, UWSystem.SysUtils, UWSystem.StrUtils, UWSystem.TimeUtils;

// -----------------------------------------------------------------------------

function CFReplaceHeaderTags(const S, TimeFormat, FPS: String; const W, H: Integer): String;
begin
  Result := ReplaceString(S,   '{Company}', 'URUWorks');
  Result := ReplaceString(Result, '{Software}', ProgramName);
  Result := ReplaceString(Result, '{WebSite}', ProgramWebsite);
  Result := ReplaceString(Result, '{TotalCount}', Subtitles.Count.ToString);
  Result := ReplaceString(Result, '{FPS}', FPS);
  Result := ReplaceString(Result, '{ImageWidth}', W.ToString);
  Result := ReplaceString(Result, '{ImageHeight}', H.ToString);
  with Subtitles[0] do // First sub
  begin
    Result := ReplaceString(Result, '{FirstStart}', Iff(TimeFormat <> '', TimeToString(InitialTime, TimeFormat), IntToStr(InitialTime)));
    Result := ReplaceString(Result, '{FirstEnd}', Iff(TimeFormat <> '', TimeToString(FinalTime, TimeFormat), IntToStr(FinalTime)));
  end;
  with Subtitles[Subtitles.Count-1] do // Last sub
  begin
    Result := ReplaceString(Result, '{LastStart}', Iff(TimeFormat <> '', TimeToString(InitialTime, TimeFormat), InitialTime.ToString));
    Result := ReplaceString(Result, '{LastEnd}', Iff(TimeFormat <> '', TimeToString(FinalTime, TimeFormat), FinalTime.ToString));
  end;
end;

// -----------------------------------------------------------------------------

function CFReplaceBodyTags(const S, TimeFormat, FPS: String; const Item: TUWSubtitleItem; const InFrames: Boolean; const NewChar: String; const Index: Integer; const ImageFile: String = ''; const W: Integer = 0; const H: Integer = 0): String;

  function GetTime(const Time: Cardinal): String;
  begin
    if InFrames then
      Result := IntToStr(TimeToFrames(Time, StrToSingle(FPS, Workspace.FPS.DefFPS, AppOptions.FormatSettings)))
    else
      Result := iff(TimeFormat <> '', TimeToString(Time, TimeFormat), Time.ToString);
  end;

begin
  with Item do
  begin
    Result := CFReplaceHeaderTags(S, TimeFormat, FPS, W, H);
    Result := ReplaceString(Result, '{tsIndex}', IntToStr(Index));
    Result := ReplaceString(Result, '{tsStart}', GetTime(InitialTime));
    Result := ReplaceString(Result, '{tsEnd}', GetTime(FinalTime));
    Result := ReplaceString(Result, '{tsDuration}', GetTime(FinalTime-InitialTime));
    Result := ReplaceString(Result, '{tsText}', ReplaceEnters(Text, sLineBreak, iff(LowerCase(NewChar) = '[enter]', sLineBreak, NewChar)));
    Result := ReplaceString(Result, '{tsTranslation}', ReplaceEnters(Translation, sLineBreak, iff(LowerCase(NewChar) = '[enter]', sLineBreak, NewChar)));
    Result := ReplaceString(Result, '{tsStyle}', Style);
    Result := ReplaceString(Result, '{tsActor}', Actor);
    Result := ReplaceString(Result, '{tsX1}', R.Left.ToString);
    Result := ReplaceString(Result, '{tsX2}', R.Right.ToString);
    Result := ReplaceString(Result, '{tsY1}', R.Top.ToString);
    Result := ReplaceString(Result, '{tsY2}', R.Bottom.ToString);
    Result := ReplaceString(Result, '{tsImage}', ImageFile);
  end;
end;

// -----------------------------------------------------------------------------

function CFFixExtension(const AFileName, AExtension: String): String;
begin
  Result := ChangeFileExt(AFileName, AExtension.Replace('*', ''));
end;

// -----------------------------------------------------------------------------

end.

