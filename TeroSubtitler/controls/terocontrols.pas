{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit TeroControls;

{$warn 5023 off : no warning about unused units}
interface

uses
  UWLayout, UWTimeEdit, WAVDisplayer, UWSeekBar, UWCPSBar, UWMemo, 
  UWFlatButton, UWStatusBar, UWCheckBox, UWRadioButton, UWGroupBox, UWHotKey, 
  UWTextBox, UWEditAction, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('UWLayout', @UWLayout.Register);
  RegisterUnit('UWTimeEdit', @UWTimeEdit.Register);
  RegisterUnit('WAVDisplayer', @WAVDisplayer.Register);
  RegisterUnit('UWSeekBar', @UWSeekBar.Register);
  RegisterUnit('UWCPSBar', @UWCPSBar.Register);
  RegisterUnit('UWMemo', @UWMemo.Register);
  RegisterUnit('UWFlatButton', @UWFlatButton.Register);
  RegisterUnit('UWStatusBar', @UWStatusBar.Register);
  RegisterUnit('UWCheckBox', @UWCheckBox.Register);
  RegisterUnit('UWRadioButton', @UWRadioButton.Register);
  RegisterUnit('UWGroupBox', @UWGroupBox.Register);
  RegisterUnit('UWHotKey', @UWHotKey.Register);
  RegisterUnit('UWTextBox', @UWTextBox.Register);
  RegisterUnit('UWEditAction', @UWEditAction.Register);
end;

initialization
  RegisterPackage('TeroControls', @Register);
end.
