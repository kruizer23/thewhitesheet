unit aboutunit;
//***************************************************************************************
//  Description: About information dialog.
//    File Name: aboutunit.pas
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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, LCLIntf;

//***************************************************************************************
// Type definitions
//***************************************************************************************
//---------------------------------- TAboutDialog ---------------------------------------
type
  { TAboutDialog }
  TAboutDialog = class(TForm)
    btnClose: TButton;
    grpBackground: TGroupBox;
    imgLogo: TImage;
    lblWebsite: TLabel;
    lblCopyright: TLabel;
    lblBuildInfo1: TLabel;
    lblBuildInfo2: TLabel;
    lblVersion: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure lblWebsiteClick(Sender: TObject);
    procedure lblWebsiteMouseEnter(Sender: TObject);
    procedure lblWebsiteMouseLeave(Sender: TObject);
  private
    { private declarations }
    procedure InitializeGui;
  public
    { public declarations }
  end;


implementation

{$R *.lfm}

{ TAboutDialog }


//***************************************************************************************
// NAME:           InitializeGui
// PARAMETER:      None.
// RETURN VALUE:   None.
// DESCRIPTION:    Initializes the user interface components of the dialog.
//
//***************************************************************************************
procedure TAboutDialog.InitializeGui;
begin
  // set the version label
  lblVersion.Caption := 'TheWhiteSheet v1.00';
  // set the copyright info
  lblCopyright.Caption := 'Copyright ' + '©' + ' 2022 by Frank Voorburg';
  // set the website
  lblWebsite.Caption := 'View on GitHub';
end; //*** end of InitializeGui ***


//***************************************************************************************
// NAME:           FormKeyPress
// PARAMETER:      Sender Signal source.
//                 Key The key's character code that went was pressed.
// RETURN VALUE:   None.
// DESCRIPTION:    Called when a key is pressed.
//
//***************************************************************************************
procedure TAboutDialog.FormKeyPress(Sender: TObject; var Key: char);
begin
  // was the escape key pressed?
  if Key = Char(27) then
  begin
    // set the modal result value
    ModalResult := mrCancel;
  end
  // was the enter key pressed?
  else if Key = Char(13) then
  begin
    // set the modal result value
    ModalResult := mrOk;
  end;
end; //*** end of FormKeyPress ***


//***************************************************************************************
// NAME:           lblWebsiteClick
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the component was clicked.
//
//***************************************************************************************
procedure TAboutDialog.lblWebsiteClick(Sender: TObject);
begin
  // Open the browser and visit TheWhiteSheet on GitHub.
  OpenURL('https://github.com/kruizer23/thewhitesheet');
end; //*** end of lblWebsiteClick ***


//***************************************************************************************
// NAME:           LblWebLinkMouseEnter
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the mouse entered the rect of the
//                 component.
//
//***************************************************************************************
procedure TAboutDialog.lblWebsiteMouseEnter(Sender: TObject);
begin
  // Switch mouse cursor to hand to indicate that it is a hyperlink.
  lblWebsite.Cursor := crHandPoint;
end; //*** end of lblWebsiteMouseEnter ***


//***************************************************************************************
// NAME:           lblWebsiteMouseLeave
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the mouse left the rect of the
//                 component.
//
//***************************************************************************************
procedure TAboutDialog.lblWebsiteMouseLeave(Sender: TObject);
begin
  // Switch back to the default mouse cursor.
  lblWebsite.Cursor := crDefault;
end; //*** end of lblWebsiteMouseLeave ***


//***************************************************************************************
// NAME:           FormCreate
// PARAMETER:      Sender Signal source.
// RETURN VALUE:   None.
// DESCRIPTION:    Event handler for when the form is created.
//
//***************************************************************************************
procedure TAboutDialog.FormCreate(Sender: TObject);
begin
  // initialize the user interface components
  InitializeGui;
end; //*** end of FormCreate ***


end.
//********************************** end of aboutunit.pas *******************************

