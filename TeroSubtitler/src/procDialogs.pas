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

unit procDialogs;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, procTypes, UWSubtitleAPI, formCustomQuestionDlg,
  formCustomMessageDlg;

{ Custom inputbox dialog }

function InputDialog(const ACaption, APrompt, ADefault: String; const AHelp: String = ''; const AURL: String = ''; const AHeight: Integer = 93): String;

{ Custom message dialogs }

procedure ShowErrorMessageDialog(const AMessage: String; const ACaption: String = '');
procedure ShowMessageDialog(const AMessage: String; const ACaption: String = '');

{ Custom question dialogs }

function CustomQuestionDialog(const ASectionName, Title, Caption: String; const AButtons: TCustomDlgButtons = []): Integer;
function MsgSaveSubtitle(FileName: String; const ASubtitleMode: TSubtitleMode = smText): Integer;
function MsgSaveWithErrors: Integer;
function MsgDeleteFiles: Integer;
function MsgFolderNotEmpty: Integer;
function MsgExportTextOnlyFormat: Integer;
function MsgExitTranscriptionMode: Integer;

{ Open dialog helper }

function GetFileFromOpenDialog(const ADefault: String = ''): String;

// -----------------------------------------------------------------------------

implementation

uses
  procConfig, Dialogs, UWSystem.XMLLang, formCustomInputDlg;

// -----------------------------------------------------------------------------

{ Custom inputbox dialog }

// -----------------------------------------------------------------------------

function InputDialog(const ACaption, APrompt, ADefault: String; const AHelp: String = ''; const AURL: String = ''; const AHeight: Integer = 93): String;
begin
  with TfrmCustomInputDlg.Create(NIL) do
  try
    FURL := AURL;
    lblHelp.Caption := AHelp;
    Result := Execute(ACaption, APrompt, ADefault, AHeight);
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

{ Custom Message dialog }

// -----------------------------------------------------------------------------

procedure ShowErrorMessageDialog(const AMessage: String; const ACaption: String = '');
begin
  with TfrmCustomMessageDlg.Create(NIL) do
  try
    Execute(AMessage, ACaption);
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure ShowMessageDialog(const AMessage: String; const ACaption: String = '');
begin
  with TfrmCustomMessageDlg.Create(NIL) do
  try
    Execute(AMessage, ACaption, imInformation);
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

{ Custom question dialogs }

// -----------------------------------------------------------------------------

function CustomQuestionDialog(const ASectionName, Title, Caption: String; const AButtons: TCustomDlgButtons = []): Integer;
var
  sl: TAppStringList = NIL;
begin
  Result := 0;
  if LanguageManager.GetAppStringList(ASectionName, sl) then
  try
    Result := formCustomQuestionDlg.ShowQuestionDialog(GetString(sl, Title), GetString(sl, Caption), ProgramName, AButtons);
  finally
    FreeAndNil(sl);
  end;
end;

// -----------------------------------------------------------------------------

function MsgSaveSubtitle(FileName: String; const ASubtitleMode: TSubtitleMode = smText): Integer;
var
  sl : TAppStringList = NIL;
  sm : String;
begin
  Result := 0;
  if LanguageManager.GetAppStringList('CommonStrings', sl) then
  try
    if ASubtitleMode = smText then
      sm := GetString(sl, 'FileChanged')
    else
      sm := GetString(sl, 'TranslationFileChanged');

    if FileName.IsEmpty then FileName := GetString(sl, 'NoName');
    Result := formCustomQuestionDlg.ShowQuestionDialog(sm, Format(GetString(sl, 'AskToSave'), [FileName]), ProgramName);
  finally
    FreeAndNil(sl);
  end;
end;

// -----------------------------------------------------------------------------

function MsgSaveWithErrors: Integer;
begin
  Result := CustomQuestionDialog('CommonStrings', 'SubtitleHasErrorsToFix', 'ContinueAnyway', [dbYes, dbNo]);
end;

// -----------------------------------------------------------------------------

function MsgDeleteFiles: Integer;
begin
  Result := CustomQuestionDialog('CommonStrings', 'PromptForDeleteLines', 'ContinueAnyway', [dbYes, dbNo]);
end;

// -----------------------------------------------------------------------------

function MsgFolderNotEmpty: Integer;
begin
  Result := CustomQuestionDialog('CommonStrings', 'FolderNotEmpty', 'ContinueAnyway', [dbYes, dbNo]);
end;

// -----------------------------------------------------------------------------

function MsgExportTextOnlyFormat: Integer;
begin
  Result := CustomQuestionDialog('CommonStrings', 'TextFormatted', '', [dbYes, dbNo]);
end;

// -----------------------------------------------------------------------------

function MsgExitTranscriptionMode: Integer;
begin
  Result := CustomQuestionDialog('CommonStrings', 'TranscriptionModeExit', 'ContinueAnyway', [dbYes, dbNo])
end;

// -----------------------------------------------------------------------------

{ Open dialog helper }

// -----------------------------------------------------------------------------

function GetFileFromOpenDialog(const ADefault: String = ''): String;
begin
  Result := ADefault;

  with TOpenDialog.Create(NIL) do
  try
    Title   := GetCommonString('OpenFile');
    Filter  := GetCommonString('AllSupportedFiles') + ' (*.*)|*.*';
    Options := Options + [ofPathMustExist, ofFileMustExist];

    if Execute then
      Result := FileName;
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

end.

