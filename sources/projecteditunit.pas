unit projecteditunit;
//***************************************************************************************
//  Description: Project editor dialog.
//    File Name: projecteditnit.pas
//
//---------------------------------------------------------------------------------------
//                          C O P Y R I G H T
//---------------------------------------------------------------------------------------
//              Copyright 2022 (c) by Frank Voorburg   All rights reserved.
//
//   This software has been carefully tested, but is not guaranteed for any particular
// purpose. The author does not offer any warranties and does not guarantee the accuracy,
//   adequacy, or completeness of the software and is not responsible for any errors or
//              omissions or the results obtained from use of the software.
//
//---------------------------------------------------------------------------------------
//                            L I C E N S E
//---------------------------------------------------------------------------------------
// This file is part of TheWhiteSheet. TheWhiteSheet is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// TheWhiteSheet is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
// PARTICULAR PURPOSE. See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with this
// program.  If not, see <http://www.gnu.org/licenses/>.
//
//***************************************************************************************
{$mode objfpc}{$H+}

interface
//***************************************************************************************
// Global includes
//***************************************************************************************
uses
  Classes, SysUtils, FileUtil, DateTimePicker, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, projectdata, DateUtils;

//***************************************************************************************
// Type Definitions
//***************************************************************************************
//---------------------------------- TProjectEditDialog ---------------------------------
type

  { TProjectEditDialog }

  TProjectEditDialog = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    ckbCompleted: TCheckBox;
    cmbType: TComboBox;
    edtPoNumber: TEdit;
    edtClient: TEdit;
    edtScope: TEdit;
    grpBoxBackground: TGroupBox;
    lblComment: TLabel;
    lblCompleted: TLabel;
    lblDeadline: TLabel;
    lblScope: TLabel;
    lblType: TLabel;
    lblPoNumber: TLabel;
    lblClient: TLabel;
    mmoComment: TMemo;
    dtpDeadline: TDateTimePicker;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
  private
    { private declarations }
    FProjectData: TProjectData;
    function  GetProjectData: TProjectData;
    procedure SetProjectData(value: TProjectData);
  public
    { public declarations }
    property  ProjectData: TProjectData read GetProjectData write SetProjectData;
  end;

implementation

{$R *.lfm}

{ TProjectEditDialog }

//***************************************************************************************
// NAME:           FormCreate
// PARAMETER:      Sender Signal source.
// RETURN VALUE:   None.
// DESCRIPTION:    Called when the form is created
//
//***************************************************************************************
procedure TProjectEditDialog.FormCreate(Sender: TObject);
var
  idx: integer;
begin
  // initialize the project types in the combobox
  cmbType.Items.Clear;
  for idx := 0 to Length(ProjectTypes)-1 do
  begin
    cmbType.Items.Add(ProjectTypes[idx]);
  end;
  // set default values
  edtClient.Text := '';
  edtPoNumber.Text := '';
  cmbType.ItemIndex := 0;
  edtScope.Text := '';
  dtpDeadline.DateTime := Now;
  ckbCompleted.Checked := false;
  mmoComment.Clear;
end; //*** end of FormCreate ***

//***************************************************************************************
// NAME:           btnOKClick
// PARAMETER:      Sender Signal source.
// RETURN VALUE:   None.
// DESCRIPTION:    Called when the OK button is clicked.
//
//***************************************************************************************
procedure TProjectEditDialog.btnOKClick(Sender: TObject);
var
  dtHour, dtMin, dtSec, dtMilli: Word;
begin
  // make sure that at least a client name was entered
  if  Trim(edtClient.Text) = '' then
  begin
    MessageDlg('Please enter at least the client information.', mtInformation, [mbOK], 0);
    Exit;
  end;
  // deadline needs a bit of extra work, because we don't want to use seconds so this
  // part in TDateTime should be set to zero.
  DecodeTime(dtpDeadline.DateTime, dtHour, dtMin, dtSec, dtMilli);
  RecodeTime(dtpDeadline.DateTime, dtHour, dtMin, 0, 0);
  // update the members based on what was entered in the user interface
  FProjectData.FClient := edtClient.Text;
  FProjectData.FPoNumber := edtPoNumber.Text;
  FProjectData.FType := cmbType.ItemIndex;
  FProjectData.FScope := edtScope.Text;
  FProjectData.FDeadline := dtpDeadline.DateTime;
  FProjectData.FCompleted := ckbCompleted.Checked;
  FProjectData.FComment := mmoComment.Text;
  // set the modal result value
  ModalResult := mrOK;
end; //*** end of btnOKClick ***

//***************************************************************************************
// NAME:           btnCancelClick
// PARAMETER:      Sender Signal source.
// RETURN VALUE:   None.
// DESCRIPTION:    Called when the Cancel button is clicked.
//
//***************************************************************************************
procedure TProjectEditDialog.btnCancelClick(Sender: TObject);
begin
  // set the modal result value
  ModalResult := mrCancel;
end; //*** end of btnCancelClick ***

//***************************************************************************************
// NAME:           FormKeyPress
// PARAMETER:      Sender Signal source.
//                 Key The key's character code that was pressed.
// RETURN VALUE:   None.
// DESCRIPTION:    Called when a key is pressed.
//
//***************************************************************************************
procedure TProjectEditDialog.FormKeyPress(Sender: TObject; var Key: char);
begin
  // was the escape key pressed?
  if Key = Char(27) then
  begin
    // simulate button cancel click
    btnCancelClick(Sender);
  end
  // was the enter key pressed?
  else if Key = Char(13) then
  begin
    // do not process the enter key when the comment memo is active because then the
    // user can't go to a new line
    if not (ActiveControl.Name = 'mmoComment') then
    begin
      if ActiveControl.Name = 'btnCancel' then
        // simulate button cancel click
        btnCancelClick(Sender)
      else
        // simulate button ok click
        btnOKClick(Sender);
    end;
  end;
end; //*** end of FormKeyPress ***

//***************************************************************************************
// NAME:           GetProjectData
// PARAMETER:      None.
// RETURN VALUE:   Value of ProjectData member.
// DESCRIPTION:    Getter for the ProjectData member.
//
//***************************************************************************************
function TProjectEditDialog.GetProjectData: TProjectData;
begin
  Result := FProjectData;
end; //*** end of GetProjectData ****

//***************************************************************************************
// NAME:           SetProjectData
// PARAMETER:      value New value for the ProjectData member.
// RETURN VALUE:   None.
// DESCRIPTION:    Setter for the ProjectData member.
//
//***************************************************************************************
procedure TProjectEditDialog.SetProjectData(value: TProjectData);
begin
  FProjectData.FClient := value.FClient;
  FProjectData.FPoNumber := value.FPoNumber;
  FProjectData.FScope := value.FScope;
  FProjectData.FType := value.FType;
  FProjectData.FDeadline := value.FDeadline;
  FProjectData.FCompleted := value.FCompleted;
  FProjectData.FComment := value.FComment;
  // update the user interface
  edtClient.Text := value.FClient;
  edtPoNumber.Text := value.FPoNumber;
  edtScope.Text := value.FScope;
  cmbType.ItemIndex := value.FType;
  dtpDeadline.DateTime:= value.FDeadline;
  ckbCompleted.Checked := value.FCompleted;
  mmoComment.Text:= value.FComment;
end; //*** end of SetProjectData ***


end.
//********************************** end of projecteditunit.pas *************************

