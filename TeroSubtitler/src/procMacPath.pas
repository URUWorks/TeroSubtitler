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

unit procMacPath;

{$mode ObjFPC}{$H+}

interface

// -----------------------------------------------------------------------------

implementation

// A GUI app launched from Finder/Dock gets the minimal launchd PATH
// (/usr/bin:/bin:/usr/sbin:/sbin), not the one of the Terminal. yt-dlp, run
// by the mpv ytdl_hook, then can't find deno (JS challenges of YouTube) or
// ffmpeg installed with Homebrew/MacPorts, and YouTube urls don't load.
// Add those folders to the PATH of the process at startup (unit
// initialization, before mpv is created) so mpv and the processes started
// by Tero inherit it.

{$IFDEF DARWIN}
uses
  SysUtils, ctypes;

function setenv(name, value: PChar; overwrite: cint): cint; cdecl; external 'c';
function NSGetEnviron: PPPChar; cdecl; external 'c' name '_NSGetEnviron';

// -----------------------------------------------------------------------------

procedure AddFoldersToPath(const AFolders: array of String);
var
  Path, Folder: String;
  Changed: Boolean;
begin
  Path    := GetEnvironmentVariable('PATH');
  Changed := False;

  for Folder in AFolders do
    if DirectoryExists(Folder) and
      (Pos(':' + Folder + ':', ':' + Path + ':') = 0) then
    begin
      if Path = '' then
        Path := Folder
      else
        Path := Path + ':' + Folder; // after the system ones
      Changed := True;
    end;

  if Changed and (setenv('PATH', PChar(Path), 1) = 0) then
    envp := NSGetEnviron^; // the RTL (GetEnvironmentVariable, TProcess) reads envp
end;

// -----------------------------------------------------------------------------

initialization
  AddFoldersToPath(['/opt/homebrew/bin', '/usr/local/bin', '/opt/local/bin']);
{$ENDIF}

end.
