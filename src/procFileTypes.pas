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

unit procFileTypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, procTypes
  {$IFDEF WINDOWS}
  , Registry, ShlObj
  {$ENDIF};

{$IFDEF WINDOWS}
procedure RegisterFileType(const AFileExtension, AIconFileName: String; const ARegister: Boolean = True; const AChangeNotify: Boolean = True);
function IsRegisteredFileType(const AFileExtension: String): Boolean;
{$ENDIF}

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

{$IFDEF WINDOWS}
// AFileExtension = .ext
procedure RegisterFileType(const AFileExtension, AIconFileName: String; const ARegister: Boolean = True; const AChangeNotify: Boolean = True);
const
  ExplorerFileExtsKey = '\Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\';
  SoftwareClassesKey  = '\Software\Classes\';
  ShellOpenCommandKey = '\Shell\Open\Command';
  DefaultIconKey      = '\DefaultIcon';

var
  Registry: TRegistry;
  FileType: String;
  Description: String;
  IsAssociated: Boolean;
  BackupKey: String;

  procedure MoveKeyEx(const OldName, NewName: String);
  var
    s: String;
  begin
    with Registry do
      try
        if OpenKey(OldName, False) then
        begin
          s := ReadString('');
          CloseKey;
          DeleteKey(OldName);

          if OpenKey(NewName, True) then
          begin
            WriteString('', s);
            CloseKey;
          end;
        end;
      except
      end;
  end;

begin
  Registry := TRegistry.Create;
  with Registry do
    try
      RootKey := HKEY_CLASSES_ROOT;
      try
        FileType := ProgramName.Replace(' ', '') + AFileExtension;
        Description := ProgramName + ' file';
        BackupKey := ProgramName + ' backup';

        if OpenKey(AFileExtension, False) then
        begin
          IsAssociated := ReadString('').Equals(ProgramName.Replace(' ', '') + AFileExtension);
          CloseKey;
        end;

        RootKey := HKEY_CURRENT_USER;

        if ARegister then
        begin
          if not IsAssociated then
          begin
            if KeyExists(ExplorerFileExtsKey + AFileExtension) then
              MoveKeyEx(ExplorerFileExtsKey + AFileExtension, ExplorerFileExtsKey + AFileExtension + '_Backup');

            OpenKey(SoftwareClassesKey + FileType, True);
            WriteString('', Description);
            CloseKey;

            OpenKey(SoftwareClassesKey + FileType + DefaultIconKey, True);
            WriteString('', AIconFileName);
            CloseKey;

            OpenKey(SoftwareClassesKey + FileType + ShellOpenCommandKey, True);
            WriteString('', '"' + Application.ExeName + '" "%1"');
            CloseKey;

            OpenKey(SoftwareClassesKey + AFileExtension, True);
            WriteString(BackupKey, ReadString(''));
            WriteString('', FileType);
            CloseKey;
          end;
        end
        else
        begin
          if IsAssociated then
          begin
            if KeyExists(ExplorerFileExtsKey + AFileExtension + '_Backup') then
            begin
              if KeyExists(ExplorerFileExtsKey + AFileExtension) then
                DeleteKey(ExplorerFileExtsKey + AFileExtension);

              MoveKeyEx(ExplorerFileExtsKey + AFileExtension + '_Backup', ExplorerFileExtsKey + AFileExtension);
            end;

            OpenKey(SoftwareClassesKey + AFileExtension, True);
            if ValueExists(BackupKey) then
            begin
              WriteString('', ReadString(BackupKey));
              DeleteValue(BackupKey);
            end;
            CloseKey;

            if KeyExists(SoftwareClassesKey + FileType) then
            begin
              DeleteKey(SoftwareClassesKey + FileType + ShellOpenCommandKey);
              DeleteKey(SoftwareClassesKey + FileType + '\Shell\Open');
              DeleteKey(SoftwareClassesKey + FileType + '\Shell');
              DeleteKey(SoftwareClassesKey + FileType + DefaultIconKey);
              DeleteKey(SoftwareClassesKey + FileType);
            end;
          end;
        end;
      except
      end;
    finally
      Registry.Free;
    end;

  if AChangeNotify then
    SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, NIL, NIL);
end;

// -----------------------------------------------------------------------------

function IsRegisteredFileType(const AFileExtension: String): Boolean;
var
  s: String;
begin
  with TRegistry.Create do
    try
      RootKey := HKEY_CLASSES_ROOT;
      if OpenKey(AFileExtension, False) then
      begin
        s := ReadString('');
        Result := s.Equals(ProgramName.Replace(' ', '') + AFileExtension);
        CloseKey;
      end;
    finally
      Free;
    end;
end;
{$ENDIF}

// -----------------------------------------------------------------------------

end.

