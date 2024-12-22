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
  formCustomMessageDlg, procLocalize;

{ Custom inputbox dialog }

function InputDialog(const ACaption, APrompt, ADefault: String; const AHelp: String = ''; const AURL: String = ''; const AHeight: Integer = 93; const AOnlyNumbers: Boolean = False): String;

{ Custom message dialogs }

procedure ShowErrorMessageDialog(const AMessage: String; const ACaption: String = ''; const ABold: Boolean = True; const ACenter: Boolean = True);
procedure ShowMessageDialog(const AMessage: String; const ACaption: String = ''; const ACustomAction: String = ''; const ACustomActionClick: TNotifyEvent = NIL);

{ Custom question dialogs }

function CustomQuestionDialog(Title, Caption: String; const AButtons: TCustomDlgButtons = []): Integer;
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
  Dialogs, formCustomInputDlg;

// -----------------------------------------------------------------------------

{ Custom inputbox dialog }

// -----------------------------------------------------------------------------

function InputDialog(const ACaption, APrompt, ADefault: String; const AHelp: String = ''; const AURL: String = ''; const AHeight: Integer = 93; const AOnlyNumbers: Boolean = False): String;
begin
  with TfrmCustomInputDlg.Create(NIL) do
  try
    FURL := AURL;
    lblHelp.Caption := AHelp;
    Result := Execute(ACaption, APrompt, ADefault, AHeight, AOnlyNumbers);
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

{ Custom Message dialog }

// -----------------------------------------------------------------------------

procedure ShowErrorMessageDialog(const AMessage: String; const ACaption: String = ''; const ABold: Boolean = True; const ACenter: Boolean = True);
begin
  with TfrmCustomMessageDlg.Create(NIL) do
  try
    Execute(AMessage, ACaption, imQuestion, ABold, ACenter);
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure ShowMessageDialog(const AMessage: String; const ACaption: String = ''; const ACustomAction: String = ''; const ACustomActionClick: TNotifyEvent = NIL);
begin
  with TfrmCustomMessageDlg.Create(NIL) do
  try
    Execute(AMessage, ACaption, imInformation, True, True, ACustomAction, ACustomActionClick);
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

{ Custom question dialogs }

// -----------------------------------------------------------------------------

function CustomQuestionDialog(Title, Caption: String; const AButtons: TCustomDlgButtons = []): Integer;
begin
    Result := formCustomQuestionDlg.ShowQuestionDialog(Title, Caption, ProgramName, AButtons);
end;

// -----------------------------------------------------------------------------

function MsgSaveSubtitle(FileName: String; const ASubtitleMode: TSubtitleMode = smText): Integer;
var
  sm : String;
begin
  Result := 0;
  try
    if ASubtitleMode = smText then
      sm := lngFileChanged
    else
      sm := lngTranslationFileChanged;

    if FileName.IsEmpty then FileName := lngNoName;
    Result := formCustomQuestionDlg.ShowQuestionDialog(sm, Format(lngAskToSave, [FileName]), ProgramName);
  finally
  end;
end;

// -----------------------------------------------------------------------------

function MsgSaveWithErrors: Integer;
begin
  Result := CustomQuestionDialog(lngSubtitleHasErrorsToFix, lngContinueAnyway, [dbYes, dbNo]);
end;

// -----------------------------------------------------------------------------

function MsgDeleteFiles: Integer;
begin
  Result := CustomQuestionDialog(lngPromptForDeleteLines, lngContinueAnyway, [dbYes, dbNo]);
end;

// -----------------------------------------------------------------------------

function MsgFolderNotEmpty: Integer;
begin
  Result := CustomQuestionDialog(lngFolderNotEmpty, lngContinueAnyway, [dbYes, dbNo]);
end;

// -----------------------------------------------------------------------------

function MsgExportTextOnlyFormat: Integer;
begin
  Result := CustomQuestionDialog(lngTextFormatted, '', [dbYes, dbNo, dbCancel]);
end;

// -----------------------------------------------------------------------------

function MsgExitTranscriptionMode: Integer;
begin
  Result := CustomQuestionDialog(lngTranscriptionModeExit, lngContinueAnyway, [dbYes, dbNo])
end;

// -----------------------------------------------------------------------------

{ Open dialog helper }

// -----------------------------------------------------------------------------

function GetFileFromOpenDialog(const ADefault: String = ''): String;
begin
  Result := ADefault;

  with TOpenDialog.Create(NIL) do
  try
    Title   := lngOpenFile;
    Filter  := lngAllSupportedFiles + ' (*.*)|*.*';
    Options := Options + [ofPathMustExist, ofFileMustExist];

    if Execute then
      Result := FileName;
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

end.

