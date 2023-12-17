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

unit formVideo;

// -----------------------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Forms, Controls;

type

  { TfrmVideo }

  TfrmVideo = class(TForm)
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private

  public

  end;

var
  frmVideo: TfrmVideo;

// -----------------------------------------------------------------------------

implementation

uses
  procUnDockVideoControls, procConfig;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmVideo }

// -----------------------------------------------------------------------------

procedure TfrmVideo.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  DockVideoControls(True);
  SaveFormSettings(Self);

  CloseAction := caFree;
  frmVideo    := NIL;
end;

// -----------------------------------------------------------------------------

end.

