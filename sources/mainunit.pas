unit mainunit;
//***************************************************************************************
//  Description: Program's main window.
//    File Name: mainunit.pas
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
  Classes, SysUtils, LazFileUtils, UniqueInstance, Forms, Controls, Graphics,
  Dialogs, ComCtrls, DateUtils, ActnList, Menus, ExtCtrls, projectviewunit,
  projecteditunit, projectdata, XMLConf, aboutunit;

//***************************************************************************************
// Type Definitions
//***************************************************************************************
//---------------------------------- TMainForm ------------------------------------------
type

  { TMainForm }

  TMainForm = class(TForm)
    ActFileExit: TAction;
    ActHelpAbout: TAction;
    ActFileExport: TAction;
    ActProjectPaste: TAction;
    ActProjectCopy: TAction;
    ActViewCompleted: TAction;
    ActViewActive: TAction;
    ActViewAll: TAction;
    ActProjectDelete: TAction;
    ActProjectEdit: TAction;
    ActProjectAdd: TAction;
    ActionList: TActionList;
    ImageList: TImageList;
    MainMenu: TMainMenu;
    MnuItemExport: TMenuItem;
    MnuItemSep2: TMenuItem;
    MnuItemPaste: TMenuItem;
    MnuItemCopy: TMenuItem;
    MnuItemSep1: TMenuItem;
    MnuItemAbout: TMenuItem;
    MnuItemHelp: TMenuItem;
    MnuItemCompleted: TMenuItem;
    MnuItemActive: TMenuItem;
    MnuItemAll: TMenuItem;
    MnuItemView: TMenuItem;
    MnuItemDelete: TMenuItem;
    MnuItemEdit: TMenuItem;
    MnuItemAdd: TMenuItem;
    MnuItemProject: TMenuItem;
    MnuItemExit: TMenuItem;
    MnuItemFile: TMenuItem;
    MainPanel: TPanel;
    SaveDialog: TSaveDialog;
    statusBar: TStatusBar;
    TmrAutosave: TTimer;
    ToolBar1: TToolBar;
    tbProjectAdd: TToolButton;
    tbProjectEdit: TToolButton;
    tbProjectDelete: TToolButton;
    ToolButton1: TToolButton;
    tbProjectCopy: TToolButton;
    tbProjectPaste: TToolButton;
    ToolButton2: TToolButton;
    tbViewActive: TToolButton;
    tbViewCompleted: TToolButton;
    tbViewAll: TToolButton;
    ToolButton3: TToolButton;
    tbExport: TToolButton;
    UniqueInstance: TUniqueInstance;
    procedure ActFileExitExecute(Sender: TObject);
    procedure ActFileExportExecute(Sender: TObject);
    procedure ActHelpAboutExecute(Sender: TObject);
    procedure ActProjectAddExecute(Sender: TObject);
    procedure ActProjectCopyExecute(Sender: TObject);
    procedure ActProjectDeleteExecute(Sender: TObject);
    procedure ActProjectEditExecute(Sender: TObject);
    procedure ActProjectPasteExecute(Sender: TObject);
    procedure ActViewActiveExecute(Sender: TObject);
    procedure ActViewAllExecute(Sender: TObject);
    procedure ActViewCompletedExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure DisplayHint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MnuItemFileClick(Sender: TObject);
    procedure MnuItemProjectClick(Sender: TObject);
    procedure TmrAutosaveTimer(Sender: TObject);
    procedure UniqueInstanceOtherInstance(Sender: TObject; ParamCount: Integer;
      Parameters: array of String);
  private
    { private declarations }
    FUserDataFile: string;
    FClipboardProject: TProjectData;
    FClipboardEmpty: Boolean;
    ProjectView: TProjectViewForm;
    ProjectList: TProjectList;
    procedure UserInterfaceInit;
    procedure StatusbarInit;
    procedure StatusbarResize;
    procedure UserDataFileInit;
    procedure UserDataFileSave;
    procedure UserDataFileLoad;
    procedure ProjectViewUpdateFilterMode;
    procedure ProjectViewRefreshed(Sender: TObject);
    procedure ProjectViewEditRequest(Sender: TObject);
  public
    { public declarations }
  end;

//***************************************************************************************
// Global data declarations
//***************************************************************************************
var
  MainForm: TMainForm;

implementation
//***************************************************************************************
// Local constant declarations
//***************************************************************************************
const
  statusBarPnlProjectsSize = 200;

{$R *.lfm}

//---------------------------------------------------------------------------------------
//-------------------------------- TMainForm --------------------------------------------
//---------------------------------------------------------------------------------------
//***************************************************************************************
// NAME:           FormResize
// PARAMETER:      Sender Signal source.
// RETURN VALUE:   None.
// DESCRIPTION:    Called when the main form is resized.
//
//***************************************************************************************
procedure TMainForm.FormResize(Sender: TObject);
begin
  // resize the panels on the statusbar
  StatusbarResize;
end; //*** end of FormResize ***

//***************************************************************************************
// NAME:           FormCreate
// PARAMETER:      Sender Signal source.
// RETURN VALUE:   None.
// DESCRIPTION:    Called when the form is created
//
//***************************************************************************************
procedure TMainForm.FormCreate(Sender: TObject);
begin
  // construct the project list
  ProjectList := TProjectList.Create;
  // construct the project view form and set the members
  ProjectView := TProjectViewForm.Create(Self);
  ProjectView.ProjectList := ProjectList;
  ProjectView.OnRefresh := @ProjectViewRefreshed;
  ProjectView.OnEdit := @ProjectViewEditRequest;
  ProjectView.OnInsert := @ActProjectAddExecute;
  // re-route hint handler
  Application.OnHint := @DisplayHint;
  // initialize and load the user data file
  UserDataFileInit;
  UserDataFileLoad;
  // initialize the status bar
  StatusbarInit;
  // initialize the user interface
  UserInterfaceInit;
  // initialize the clipboard
  FClipboardEmpty := True;
  FClipboardProject.FId := 0;
  FClipboardProject.FClient := '';
  FClipboardProject.FPoNumber := '';
  FClipboardProject.FScope := '';
  FClipboardProject.FType := 0;
  FClipboardProject.FDeadline := Now;
  FClipboardProject.FCompleted := false;
  FClipboardProject.FComment := '';
end; //*** end of FormCreate ***

//***************************************************************************************
// NAME:           FormClose
// PARAMETER:      Sender Signal source.
//                 CloseAction Action for closing.
// RETURN VALUE:   None.
// DESCRIPTION:    Called when the form is closed.
//
//***************************************************************************************
procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // store configuration and data in the user's data file
  UserDataFileSave;
end; //*** end of FormClose ***

//***************************************************************************************
// NAME:           FormDestroy
// PARAMETER:      Sender Signal source.
// RETURN VALUE:   None.
// DESCRIPTION:    Called when the form is destroyed
//
//***************************************************************************************
procedure TMainForm.FormDestroy(Sender: TObject);
begin
  // release the project view form
  ProjectView.Free;
  // release the project list
  ProjectList.Free;
end; //*** end of FormDestroy ***

//***************************************************************************************
// NAME:           actFileExitExecute
// PARAMETER:      Sender Signal source.
// RETURN VALUE:   None.
// DESCRIPTION:    Exit's the program.
//
//***************************************************************************************
procedure TMainForm.ActFileExitExecute(Sender: TObject);
begin
  Close;
end; //*** end of ActFileExitExecute ***

//***************************************************************************************
// NAME:           ActFileExportExecute
// PARAMETER:      Sender Signal source.
// RETURN VALUE:   None.
// DESCRIPTION:    Exports the currently shown projects to a CSV file.
//
//***************************************************************************************
procedure TMainForm.ActFileExportExecute(Sender: TObject);
begin
  // make sure there is actually something to export
  if ProjectView.Count <= 0 then
    Exit;

  // prompt user to select the configuration file to write to
  if saveDialog.Execute then
  begin
    // perform the exporting
    ProjectView.ExportToCsv(SaveDialog.FileName, FUserDataFile);
    // inform user of result
    MessageDlg('Export successfully completed.', mtInformation, [mbOK], 0);
  end;
end; //*** end of ActFileExportExecute ***

//***************************************************************************************
// NAME:           ActHelpAboutExecute
// PARAMETER:      Sender Signal source.
// RETURN VALUE:   None.
// DESCRIPTION:    Display program's about dialog.
//
//***************************************************************************************
procedure TMainForm.ActHelpAboutExecute(Sender: TObject);
var
  AboutDialog: TAboutDialog;
begin
  // create the dialog
  AboutDialog := TAboutDialog.Create(MainForm);
  // make sure it is centered
  AboutDialog.Position := poScreenCenter;
  // show it in the modal state. we are not interested in the result at this point
  AboutDialog.ShowModal;
  // release the dialog
  AboutDialog.Free;
end; //*** end of ActHelpAboutExecute ***

//***************************************************************************************
// NAME:           ActProjectAddExecute
// PARAMETER:      Sender Signal source.
// RETURN VALUE:   None.
// DESCRIPTION:    Adds a new project.
//
//***************************************************************************************
procedure TMainForm.ActProjectAddExecute(Sender: TObject);
var
  ProjectEditDialog: TProjectEditDialog;
  ProjectData: TProjectData;
  dialogResult: Integer;
begin
  // create the dialog
  ProjectEditDialog := TProjectEditDialog.Create(Self);
  // make sure it is centered
  ProjectEditDialog.Position := poScreenCenter;
  // show it in the modal state
  dialogResult := ProjectEditDialog.ShowModal;
  // process the result
  if dialogResult = mrOK then
  begin
    // read out the project data from the dialog. note that this does not yet have a
    // valid project ID. The project ID is assigned when it is added to the list.
    ProjectData := ProjectEditDialog.ProjectData;
    // add the data to the project list and read out its assigned project ID.
    ProjectData.FId := ProjectList.Add(ProjectData);
    // refresh the project view
    ProjectView.Refresh;
    // select the newly added project
    ProjectView.Focused := ProjectData.FId;
  end;
  // release the dialog
  ProjectEditDialog.Free;
end; //*** end of ActProjectAddExecute ***

//***************************************************************************************
// NAME:           ActProjectCopyExecute
// PARAMETER:      Sender Signal source.
// RETURN VALUE:   None.
// DESCRIPTION:    Action handler for copying a project to the clipboard.
//
//***************************************************************************************
procedure TMainForm.ActProjectCopyExecute(Sender: TObject);
begin
  // only copy if a project is selected in the project view
  if ProjectView.Focused >= 0 then
  begin
    // copy the project to the clipboard
    FClipboardProject := ProjectList.Items[ProjectView.Focused]^;
    // update clipboard state
    FClipboardEmpty := False;
  end;
end; //*** end of ActProjectCopyExecute ***

//***************************************************************************************
// NAME:           ActProjectPasteExecute
// PARAMETER:      Sender Signal source.
// RETURN VALUE:   None.
// DESCRIPTION:    Action handler for pasting a project from the clipboard.
//
//***************************************************************************************
procedure TMainForm.ActProjectPasteExecute(Sender: TObject);
begin
  // only continue of the clipboard contains data
  if not FClipboardEmpty then
  begin
    // add the data to the project list and read out its assigned project ID.
    FClipboardProject.FId := ProjectList.Add(FClipboardProject);
    // refresh the project view
    ProjectView.Refresh;
    // select the newly added project
    ProjectView.Focused := FClipboardProject.FId;
  end;
end; //*** end of ActProjectPasteExecute ***

//***************************************************************************************
// NAME:           ActProjectDeleteExecute
// PARAMETER:      Sender Signal source.
// RETURN VALUE:   None.
// DESCRIPTION:    Delete the currently selected project.
//
//***************************************************************************************
procedure TMainForm.ActProjectDeleteExecute(Sender: TObject);
var
  dialogMessage: string;
  projectId: integer;
begin
  // can only delete if a project is selected
  if ProjectView.Focused = -1 then
    Exit;
  // display confirmation dialog
  dialogMessage := 'Are you sure you want to delete the selected project(s)?';
  if MessageDlg(dialogMessage, mtConfirmation, [mbYes,mbNo], 0) = mrYes then
  begin
    // delete the selected projects
    projectId := ProjectView.GetFirstSelected;
    while projectId <> -1 do
    begin
      ProjectList.Delete(projectId);
      projectId := ProjectView.GetNextSelected(projectId);
    end;
    // now clear the selection because these entries no longer exists
    ProjectView.ClearSelection;
    // refresh the view
    ProjectView.Refresh;
  end;
end; //*** end of ActProjectDeleteExecute ***

//***************************************************************************************
// NAME:           ActProjectEditExecute
// PARAMETER:      Sender Signal source.
// RETURN VALUE:   None.
// DESCRIPTION:    Edits the currently selected project
//
//***************************************************************************************
procedure TMainForm.ActProjectEditExecute(Sender: TObject);
var
  ProjectEditDialog: TProjectEditDialog;
  dialogResult: Integer;
begin
  // nothing to do if no project is selected in the list
  if ProjectView.Focused < 0 then
    Exit;
  // create the dialog
  ProjectEditDialog := TProjectEditDialog.Create(Self);
  // make sure it is centered
  ProjectEditDialog.Position := poScreenCenter;
  // set the initial project data to that of the selected project in the view
  ProjectEditDialog.ProjectData := ProjectList.Items[ProjectView.Focused]^;
  // show it in the modal state
  dialogResult := ProjectEditDialog.ShowModal;
  // process the result
  if dialogResult = mrOK then
  begin
    // update with the newly edited values
    ProjectList.Items[ProjectView.Focused]^ := ProjectEditDialog.ProjectData;
    // refresh the project view
    ProjectView.Refresh;
  end;
  // release the dialog
  ProjectEditDialog.Free;
end; //*** end of ActProjectEditExecute ***

//***************************************************************************************
// NAME:           ActViewActiveExecute
// PARAMETER:      Sender Signal source.
// RETURN VALUE:   None.
// DESCRIPTION:    Processes the request to only show the active projects in the view.
//
//***************************************************************************************
procedure TMainForm.ActViewActiveExecute(Sender: TObject);
begin
  // update checked state
  if not ActViewActive.Checked then
  begin
    ActViewActive.Checked := true;
    ActViewAll.Checked := false;
    ActViewCompleted.Checked := false;
  end
  else
  begin
    // button goes up when clicked even if already checked. undo this unwanted behavior
    tbViewActive.Down := true;
  end;
  // update project view based on the newly selected filter mode
  ProjectViewUpdateFilterMode;
end; //*** end of ActViewActiveExecute ***

//***************************************************************************************
// NAME:           ActViewAllExecute
// PARAMETER:      Sender Signal source.
// RETURN VALUE:   None.
// DESCRIPTION:    Processes the request to show all the projects in the view.
//
//***************************************************************************************
procedure TMainForm.ActViewAllExecute(Sender: TObject);
begin
  // update checked state
  if not ActViewAll.Checked then
  begin
    ActViewActive.Checked := false;
    ActViewAll.Checked := true;
    ActViewCompleted.Checked := false;
  end
  else
  begin
    // button goes up when clicked even if already checked. undo this unwanted behavior
    tbViewAll.Down := true;
  end;

  // update project view based on the newly selected filter mode
  ProjectViewUpdateFilterMode;
end; //*** end of ActViewAllExecute ***

//***************************************************************************************
// NAME:           ActViewCompletedExecute
// PARAMETER:      Sender Signal source.
// RETURN VALUE:   None.
// DESCRIPTION:    Processes the request to only show the completed projects in the view.
//
//***************************************************************************************
procedure TMainForm.ActViewCompletedExecute(Sender: TObject);
begin
  // update checked state
  if not ActViewCompleted.Checked then
  begin
    ActViewActive.Checked := false;
    ActViewAll.Checked := false;
    ActViewCompleted.Checked := true;
  end
  else
  begin
    // button goes up when clicked even if already checked. undo this unwanted behavior
    tbViewCompleted.Down := true;
  end;
  // update project view based on the newly selected filter mode
  ProjectViewUpdateFilterMode;
end; //*** end of ActViewCompletedExecute ***

//***************************************************************************************
// NAME:           UserInterfaceInit
// PARAMETER:      None.
// RETURN VALUE:   None.
// DESCRIPTION:    Initializes the user interface
//
//***************************************************************************************
procedure TMainForm.UserInterfaceInit;
begin
  // configure and show the project view on the main panel
  ProjectView.Parent := MainPanel;
  ProjectView.BorderStyle := bsNone;
  ProjectView.Align := alClient;
  ProjectView.Show;
  // set the default filter option for the view to only show active projects
  ProjectView.ViewFilter := pfActive;
  ActViewActive.Checked := true;
end; //*** end of UserInterfaceInit ***

//***************************************************************************************
// NAME:           StatusbarInit
// PARAMETER:      None.
// RETURN VALUE:   None.
// DESCRIPTION:    Initializes the statusbar
//
//***************************************************************************************
procedure TMainForm.StatusbarInit;
begin
  // initialize the panels on the status bar
  StatusBar.Panels[1].Width := statusBarPnlProjectsSize;
  StatusBar.Panels[0].Text := '';
  StatusBar.Panels[1].Text := 'Project count: 0';
end; //*** end of StatusbarInit ***

//***************************************************************************************
// NAME:           StatusbarResize
// PARAMETER:      None.
// RETURN VALUE:   None.
// DESCRIPTION:    Resizes the statusbar
//
//***************************************************************************************
procedure TMainForm.StatusbarResize;
begin
  // update layout of panels on the statusbar
  StatusBar.Panels[0].Width := StatusBar.Width - StatusBar.Panels[1].Width;
end; //*** end of StatusbarResize

//***************************************************************************************
// NAME:           StatusbarResize
// PARAMETER:      None.
// RETURN VALUE:   None.
// DESCRIPTION:    Initializes the filename to be use for reading and writing the user
//                 data.
//
//***************************************************************************************
procedure TMainForm.UserDataFileInit;
var
 userDataFileDir: String;
begin
  // set the filename for the user data
  FUserDataFile := GetAppConfigFile(False, False);
  userDataFileDir := ExtractFilePath(FUserDataFile);
  // double check that the directory is actually there
  if not DirectoryExists(userDataFileDir) then
  begin
    // force the directory creation
    ForceDirectories(userDataFileDir);
  end;
  // now double-check that the directory is there and is writable
  if (not DirectoryExists(userDataFileDir)) or
     (not DirectoryIsWritable(userDataFileDir)) then
  begin
    // set the filename to an invalid value to indicate that we cannot use it
    FUserDataFile := '';
  end;
end; //*** end of UserDataFileInit ***

//***************************************************************************************
// NAME:           UserDataFileSave
// PARAMETER:      None.
// RETURN VALUE:   None.
// DESCRIPTION:    Saves the program configuration and data to the user data file.
//
//***************************************************************************************
procedure TMainForm.UserDataFileSave;
var
  xmlConfig: TXMLConfig;
  entryIdx: integer;
  entryData: PProjectData;
  dtYear, dtMonth, dtDay: Word;
  dtHour, dtMin, dtSec, dtMilli: Word;
begin
  // make sure user data file is valid before proceeding
  if FUserDataFile = '' then
    Exit;

  // create the XML config and prepare the file
  xmlConfig := TXMLConfig.Create(nil);
  xmlConfig.StartEmpty := True;
  xmlConfig.Filename := FUserDataFile;
  // --------------- user interface configuration ---------------------------------------
  xmlConfig.OpenKey('Configuration');
  // ***** main window *****
  xmlConfig.OpenKey('MainWindow');
  if Self.WindowState = wsMaximized then
  begin
    xmlConfig.SetValue('Dimensions/Width', 1048);
    xmlConfig.SetValue('Dimensions/Height', 516);
    xmlConfig.SetValue('Dimensions/Left', 100);
    xmlConfig.SetValue('Dimensions/Top', 100);
  end
  else
  begin
    xmlConfig.SetValue('Dimensions/Width', Self.Width);
    xmlConfig.SetValue('Dimensions/Height', Self.Height);
    xmlConfig.SetValue('Dimensions/Left', Self.Left);
    xmlConfig.SetValue('Dimensions/Top', Self.Top);
  end;
  xmlConfig.CloseKey; // MainWindow
  xmlConfig.CloseKey; // Configuration
  // --------------- user data  ---------------------------------------------------------
  xmlConfig.OpenKey('Data');
  // ***** projects *****
  xmlConfig.OpenKey('Projects');
  // store the total number of projects
  xmlConfig.SetValue('Total', ProjectList.Count);
  // store each project
  for entryIdx := 0 to ProjectList.Count-1 do
  begin
    // get the entry
    entryData := ProjectList.Items[entryIdx];
    xmlConfig.OpenKey('Project' + WideString(IntToStr(entryIdx+1)));
    // store the client
    xmlConfig.SetValue('Client', WideString(entryData^.FClient));
    // store the po number
    xmlConfig.SetValue('PoNumber', WideString(entryData^.FPoNumber));
    // store the scope
    xmlConfig.SetValue('Scope', WideString(entryData^.FScope));
    // store the type
    xmlConfig.SetValue('Type', entryData^.FType);
    // store the deadline as decoded entities
    DecodeDateTime(entryData^.FDeadline, dtYear, dtMonth, dtDay, dtHour, dtMin, dtSec, dtMilli);
    xmlConfig.SetValue('Year', dtYear);
    xmlConfig.SetValue('Month', dtMonth);
    xmlConfig.SetValue('Day', dtDay);
    xmlConfig.SetValue('Hour', dtHour);
    xmlConfig.SetValue('Min', dtMin);
    // store the completion flag
    xmlConfig.SetValue('Completed', entryData^.FCompleted);
    // store the comment
    xmlConfig.SetValue('Comment', WideString(entryData^.FComment));
    xmlConfig.CloseKey; // Project#
  end;
  xmlConfig.CloseKey; // Projects
  xmlConfig.CloseKey; // Data
  // call children SaveConfiguration methods
  ProjectView.SaveConfiguration(xmlConfig);
  // write the contents and release the object
  xmlConfig.Flush;
  xmlConfig.Free;
end; //*** end of UserDataFileSave ***

//***************************************************************************************
// NAME:           UserDataFileLoad
// PARAMETER:      None.
// RETURN VALUE:   None.
// DESCRIPTION:    Loads the program configuration and data from the user data file.
//
//***************************************************************************************
procedure TMainForm.UserDataFileLoad;
var
  xmlConfig: TXMLConfig;
  entryIdx: integer;
  entryTotal: integer;
  entryData: TProjectData;
  dtYear, dtMonth, dtDay: Word;
  dtHour, dtMin, dtSec, dtMilli: Word;
begin
  // make sure user data file is valid before proceeding
  if FUserDataFile = '' then
    Exit;
  if not FileExists(FUserDataFile) then
    Exit;

  // create the XML config and prepare the file
  xmlConfig := TXMLConfig.Create(nil);
  xmlConfig.StartEmpty := False;
  xmlConfig.Filename := FUserDataFile;
  // --------------- user interface configuration ---------------------------------------
  xmlConfig.OpenKey('Configuration');
  // ***** main window *****
  xmlConfig.OpenKey('MainWindow');
  Self.Width := xmlConfig.GetValue('Dimensions/Width', 1048);
  Self.Height := xmlConfig.GetValue('Dimensions/Height', 516);
  Self.Left := xmlConfig.GetValue('Dimensions/Left', 100);
  Self.Top := xmlConfig.GetValue('Dimensions/Top', 100);
  xmlConfig.CloseKey; // MainWindow
  xmlConfig.CloseKey; // Configuration
  // --------------- project data  ------------------------------------------------------
  xmlConfig.OpenKey('Data');
  // ***** projects *****
  xmlConfig.OpenKey('Projects');
  // first read the total numberof project entries
  entryTotal := xmlConfig.GetValue('Total', 0);
  // clear the project list
  ProjectList.Clear;
  for entryIdx := 0 to entryTotal-1 do
  begin
    xmlConfig.OpenKey('Project' + WideString(IntToStr(entryIdx+1)));
    // read the client
    entryData.FClient := String(xmlConfig.GetValue('Client', 'Client for project' + WideString(IntToStr(entryIdx+1))));
    // read the po number
    entryData.FPoNumber := String(xmlConfig.GetValue('PoNumber', ''));
    // read the scope
    entryData.FScope := String(xmlConfig.GetValue('Scope', ''));
    // read the type
    entryData.FType := xmlConfig.GetValue('Type', 0);
    // read the deadline as encoded entities
    DecodeDateTime(Now, dtYear, dtMonth, dtDay, dtHour, dtMin, dtSec, dtMilli);
    dtYear := xmlConfig.GetValue('Year', dtYear);
    dtMonth := xmlConfig.GetValue('Month', dtMonth);
    dtDay := xmlConfig.GetValue('Day', dtDay);
    dtHour := xmlConfig.GetValue('Hour', dtHour);
    dtMin := xmlConfig.GetValue('Min', dtMin);
    entryData.FDeadline := EncodeDateTime(dtYear, dtMonth, dtDay, dtHour, dtMin, 0, 0);
    // read the completion flag
    entryData.FCompleted := xmlConfig.GetValue('Completed', false);
    // read the comment
    entryData.FComment := String(xmlConfig.GetValue('Comment', ''));
    // now add the project
    ProjectList.Add(entryData);
    xmlConfig.CloseKey; // Project#
  end;
  xmlConfig.CloseKey; // Projects
  xmlConfig.CloseKey; // Data
  // call children LoadConfiguration methods
  ProjectView.LoadConfiguration(xmlConfig);
  // release the object
  xmlConfig.Free;
end; //*** end of UserDataFileLoad ***

//***************************************************************************************
// NAME:           ProjectViewUpdateFilterMode
// PARAMETER:      None.
// RETURN VALUE:   None.
// DESCRIPTION:    Updates the filter mode in the project view based on what the user
//                 selected.
//
//***************************************************************************************
procedure TMainForm.ProjectViewUpdateFilterMode;
begin
  if ActViewAll.Checked then
    ProjectView.ViewFilter := pfAll
  else if ActViewActive.Checked then
    ProjectView.ViewFilter := pfActive
  else
    ProjectView.ViewFilter := pfCompleted;
end; //*** end of ProjectViewUpdateFilterMode ***

//***************************************************************************************
// NAME:           ProjectViewRefreshed
// PARAMETER:      Sender Signal source.
// RETURN VALUE:   None.
// DESCRIPTION:    Event handler that gets called when the contents of the project view
//                 got refreshed.
//
//***************************************************************************************
procedure TMainForm.ProjectViewRefreshed(Sender: TObject);
begin
  // update the project count on the status bar
  StatusBar.Panels[1].Text := Format('Project count: %d', [ProjectView.Count]);
end; //*** end of ProjectViewRefreshed ***

//***************************************************************************************
// NAME:           ProjectViewRefreshed
// PARAMETER:      Sender Signal source.
// RETURN VALUE:   None.
// DESCRIPTION:    Event handler that gets called when the user requested an edit
//                 operation in the project view got refreshed.
//
//***************************************************************************************
procedure TMainForm.ProjectViewEditRequest(Sender: TObject);
begin
  // perform project edit operation
  ActProjectEditExecute(Sender);
end; //*** end of ProjectViewEditRequest ***

//***************************************************************************************
// NAME:           DisplayHint
// PARAMETER:      Sender Signal source.
// RETURN VALUE:   None.
// DESCRIPTION:    Application OnHint event handler.
//
//***************************************************************************************
procedure TMainForm.DisplayHint(Sender: TObject);
begin
  statusBar.Panels[0].Text := GetLongHint(Application.Hint);
end; //*** end of DisplayHint

//***************************************************************************************
// NAME:           FormShow
// PARAMETER:      Sender Signal source.
// RETURN VALUE:   None.
// DESCRIPTION:    Application OnShow event handler.
//
//***************************************************************************************
procedure TMainForm.FormShow(Sender: TObject);
begin
  // refresh the project view upon show event
  ProjectView.Refresh;
end; //*** end of FormShow ***

//***************************************************************************************
// NAME:           MnuItemFileClick
// PARAMETER:      Sender Signal source.
// RETURN VALUE:   None.
// DESCRIPTION:    OnClick event handler for the file menu.
//
//***************************************************************************************
procedure TMainForm.MnuItemFileClick(Sender: TObject);
begin
  // control en/disabling based on the project view
  if ProjectView.Count > 0 then
  begin
    MnuItemExport.Enabled := True;
  end
  else
  begin
    MnuItemExport.Enabled := False;
  end;
end; //*** end of MnuItemFileClick ***

//***************************************************************************************
// NAME:           MnuItemProjectClick
// PARAMETER:      Sender Signal source.
// RETURN VALUE:   None.
// DESCRIPTION:    OnClick event handler for the project menu.
//
//***************************************************************************************
procedure TMainForm.MnuItemProjectClick(Sender: TObject);
begin
  // control en/disabling based on the selection in the project view
  if ProjectView.Focused = -1 then
  begin
    MnuItemEdit.Enabled := false;
    MnuItemDelete.Enabled := false;
    MnuItemCopy.Enabled := false;
  end
  else
  begin
    MnuItemEdit.Enabled := true;
    MnuItemDelete.Enabled := true;
    MnuItemCopy.Enabled := true;
  end;

  // control en/disabling based on the contents of the clipboard
  if FClipboardEmpty then
  begin
    MnuItemPaste.Enabled := false;
  end
  else
  begin
    MnuItemPaste.Enabled := true;
  end;
end; //*** end of MnuItemProjectClick ***

//***************************************************************************************
// NAME:           TmrAutosaveTimer
// PARAMETER:      Sender Signal source.
// RETURN VALUE:   None.
// DESCRIPTION:    Called periodically to perform an autosave of the user's
//                 configuration.
//
//***************************************************************************************
procedure TMainForm.TmrAutosaveTimer(Sender: TObject);
begin
  // perform save operation of the user configuration
  UserDataFileSave;
end; //*** end of TmrAutosaveTimer ***

//***************************************************************************************
// NAME:           UniqueInstanceOtherInstance
// PARAMETER:      Sender Signal source.
//                 ParamCount number of command line parameters
//                 Parameters string array with the command line parameters
// RETURN VALUE:   None.
// DESCRIPTION:    Called if another instance is tried to be started. This is ignored and
//                 instead its command line parameters are passed back to us.
//
//***************************************************************************************
procedure TMainForm.UniqueInstanceOtherInstance(Sender: TObject;
  ParamCount: Integer; Parameters: array of String);
begin
  // make sure the window is not minimized because then it won't show
  if WindowState = wsMinimized then
  begin
    Application.Restore;
  end;
end; //*** end of UniqueInstanceOtherInstance ***

end.
//********************************** end of mainunit.pas ********************************


