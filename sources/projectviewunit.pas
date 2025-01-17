unit projectviewunit;
//***************************************************************************************
//  Description: Project view form.
//    File Name: projectviewunit.pas
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
  Classes, SysUtils, FileUtil, laz.VirtualTrees, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, projectdata, DateUtils, XMLConf;

//***************************************************************************************
// Type Definitions
//***************************************************************************************
//---------------------------------- TProjectViewFilter ---------------------------------
type
  TProjectViewFilter = ( pfAll=0,         // show all projects
                         pfActive,        // show only active projects
                         pfCompleted      // show only completed projects
                       );

//---------------------------------- TProjectViewCRefreshEvent ---------------------------
type
  TProjectViewRefreshEvent = procedure(Sender: TObject) of object;

//---------------------------------- TProjectViewEditEvent ------------------------------
type
  TProjectViewEditEvent = procedure(Sender: TObject) of object;

//---------------------------------- TProjectViewInsertEvent ----------------------------
type
    TProjectViewInsertEvent = procedure(Sender: TObject) of object;

//---------------------------------- TProjectViewForm -----------------------------------
type

  { TProjectViewForm }

  TProjectViewForm = class(TForm)
    DeadlineTimer: TTimer;
    vstProjectInfo: TLazVirtualStringTree;
    procedure DeadlineTimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure vstProjectInfoBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure vstProjectInfoClick(Sender: TObject);
    procedure vstProjectInfoColumnDblClick(Sender: TBaseVirtualTree;
      Column: TColumnIndex; Shift: TShiftState);
    procedure vstProjectInfoCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstProjectInfoDrawText(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const TextInfo: String; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure vstProjectInfoFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure vstProjectInfoGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vstProjectInfoGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure vstProjectInfoGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure vstProjectInfoHeaderClick(Sender: TVTHeader;
      HitInfo: TVTHeaderHitInfo);
    procedure vstProjectInfoKeyPress(Sender: TObject; var Key: char);
  private
    { private declarations }
    FProjectList: TProjectList;
    FViewFilter: TProjectViewFilter;
    FOnRefresh: TProjectViewRefreshEvent;
    FOnEdit: TProjectViewEditEvent;
    FOnInsert: TProjectViewInsertEvent;
    function  GetCount: integer;
    function  GetFocused: integer;
    procedure SetFocused(value: integer);
    procedure SetProjectList(value: TProjectList);
    procedure SetViewFilter(value: TProjectViewFilter);
    function  GetDeadlineFlagIconIndex(Node: PVirtualNode): integer;
    procedure Add(ProjectData: PProjectData);
    function  ConvertProjectIdToNode(projectId: integer): PVirtualNode;
  public
    { public declarations }
    procedure ClearSelection;
    procedure Refresh;
    function  GetFirstSelected: integer;
    function  GetNextSelected(previousId: integer): integer;
    procedure ExportToCsv(exportFile: string; userDataFile: string);
    procedure SaveConfiguration(xmlConfig: TXMLConfig);
    procedure LoadConfiguration(xmlConfig: TXMLConfig);
    { public properties }
    property  ProjectList: TProjectList write SetProjectList;
    property  Count: integer read GetCount;
    property  Focused: integer read GetFocused write SetFocused;
    property  ViewFilter: TProjectViewFilter read FViewFilter write SetViewFilter;
    property  OnRefresh: TProjectViewRefreshEvent read FOnRefresh write FOnRefresh;
    property  OnEdit: TProjectViewEditEvent read FOnEdit write FOnEdit;
    property  OnInsert: TProjectViewInsertEvent read FOnInsert write FOnInsert;
  end;

implementation

{$R *.lfm}

//***************************************************************************************
// Local constant declarations
//***************************************************************************************
const
  WeekDayNameShort: array  [1..7] of string = ('Mon','Tue','Wed', 'Thu', 'Fri', 'Sat', 'Sun');

//---------------------------------------------------------------------------------------
//-------------------------------- TProjectViewForm -------------------------------------
//---------------------------------------------------------------------------------------
//***************************************************************************************
// NAME:           Add
// PARAMETER:      ProjectData Pointer to the project data.
// RETURN VALUE:   None.
// DESCRIPTION:    Adds a project to the view.
//
//***************************************************************************************
procedure TProjectViewForm.Add(ProjectData: PProjectData);
var
  NewNode: PVirtualNode;
  NewNodeData: PProjectData;
begin
  // add a new root node
  NewNode := vstProjectInfo.AddChild(nil);
  // access the node's data
  NewNodeData := vstProjectInfo.GetNodeData(NewNode);
  // make sure the node is properly initialized
  vstProjectInfo.ValidateNode(NewNode, False);
  // copy the data to the node
  NewNodeData^.FId := ProjectData^.FId;
  NewNodeData^.FClient := ProjectData^.FClient;
  NewNodeData^.FPoNumber := ProjectData^.FPoNumber;
  NewNodeData^.FType := ProjectData^.FType;
  NewNodeData^.FScope := ProjectData^.FScope;
  NewNodeData^.FDeadline := ProjectData^.FDeadline;
  NewNodeData^.FCompleted := ProjectData^.FCompleted;
  NewNodeData^.FComment := ProjectData^.FComment;
end; //*** end of Add ****

//***************************************************************************************
// NAME:           ConvertProjectIdToNode
// PARAMETER:      projectId Project ID of the node to search for.
// RETURN VALUE:   Node pointer if found, nil otherwise.
// DESCRIPTION:    Finds the node that belongs to the specified projectId.
//
//***************************************************************************************
function TProjectViewForm.ConvertProjectIdToNode(projectId: integer): PVirtualNode;
var
  Node: PVirtualNode;
  NodeData: PProjectData;
begin
  // init result
  Result := nil;

  // access the first node
  Node := vstProjectInfo.GetFirst;
  // loop through all projects currently shown in the view
  while Assigned(Node) do
  begin
    // access the node data
    NodeData := vstProjectInfo.GetNodeData(Node);
    // check if this is the one
    if NodeData^.FId = projectId then
    begin
      // found a match
      Result := Node;
      // no need to continue
      Break;
    end;
    // continue with the next one
    Node := vstProjectInfo.GetNext(Node);
  end;
end; //*** end of ConvertProjectIdToNode ***

//***************************************************************************************
// NAME:           ClearSelection
// PARAMETER:      None.
// RETURN VALUE:   None.
// DESCRIPTION:    Clears the selection in the view if anything is selected.
//
//***************************************************************************************
procedure TProjectViewForm.ClearSelection;
begin
  // clear the selection
  vstProjectInfo.ClearSelection;
  // clear the focused node
  if vstProjectInfo.FocusedNode <> nil then
  begin
    vstProjectInfo.FocusedNode := nil;
  end;
end; //*** end of ClearSelection ***

//***************************************************************************************
// NAME:           Refresh
// PARAMETER:      None.
// RETURN VALUE:   None.
// DESCRIPTION:    Refreshed the view to process possibly changed project data.
//
//***************************************************************************************
procedure TProjectViewForm.Refresh;
var
  idx: integer;
  focusedProjectId: integer;
begin
  // first make a backup of the focused project ID
  focusedProjectId := -1;
  if vstProjectInfo.FocusedNode <> nil then
    focusedProjectId := Focused;
  // start update
  vstProjectInfo.BeginUpdate;
  // clear all entries
  vstProjectInfo.Clear;
  // add the projects one-by-one
  for idx := 0 to (FProjectList.Count-1) do
  begin
    // process view type (all, active, completed)
    case FViewFilter of
      pfActive: // only show active projects
        begin
          if not FProjectList.Items[idx]^.FCompleted then
            Add(FProjectList.Items[idx]);
        end;
      pfCompleted: // only show completed project
        begin
          if FProjectList.Items[idx]^.FCompleted then
            Add(FProjectList.Items[idx]);
        end;
      else
        // show all projects
        Add(FProjectList.Items[idx]);
    end;
  end;
  // end the update
  vstProjectInfo.EndUpdate;
  // perform sorting
  vstProjectInfo.SortTree(vstProjectInfo.Header.SortColumn, vstProjectInfo.Header.SortDirection);
  // in case all projects are shown, do an extra sorting on project completion if needed
  if (vstProjectInfo.Header.SortColumn <> 5) and (ViewFilter = pfAll) then
    vstProjectInfo.SortTree(5, sdAscending);
  // restore the selection
  if focusedProjectId >= 0 then
    Focused := focusedProjectId;
  // trigger the OnRefresh event
  if Assigned(FOnRefresh) then
  begin
    FOnRefresh(Self);
  end;
end; //*** end of Refresh ***

//***************************************************************************************
// NAME:           ExportToCsv
// PARAMETER:      exportFile Filename with full path to export to.
// RETURN VALUE:   None.
// DESCRIPTION:    Exports the projects currently show to a CSV formatted file.
//
//***************************************************************************************
procedure TProjectViewForm.ExportToCsv(exportFile: string; userDataFile: string);
var
  Node: PVirtualNode;
  NodeData: PProjectData;
  Line: string;
begin
  // construct a stringlist
  with TStringList.Create do
  try
    // write filename of the user data file to the export
    Add('"User datafile";"' + userDataFile + '"' + sLineBreak);
    // add header
    Add('"Client";"PO Number";"Type";"Scope";"Deadline";"Deadline";"Completed";"Comment"');
    // check if there are lines selected. in this case only the selected lines should
    // be included in the export.
    if vstProjectInfo.SelectedCount > 0 then
    begin
      Node := vstProjectInfo.GetFirstSelected();
      while Assigned(Node) do
      begin
        // access the node data
        NodeData := vstProjectInfo.GetNodeData(Node);
        // place the project info on a line
        Line := '"' + NodeData^.FClient + '";"' + NodeData^.FPoNumber + '";"';
        Line := Line + ProjectTypes[NodeData^.FType] + '";"' + NodeData^.FScope + '";"';
        Line := Line + Format('%s', [DateToStr(NodeData^.FDeadline)]) + '";"';
        Line := Line + Format('%s', [FormatDateTime('hh:nn', NodeData^.FDeadline)]) + '";"';
        if NodeData^.FCompleted then
          Line := Line + 'Yes";"'
        else
          Line := Line + 'No";"';
        Line := Line + NodeData^.FComment + '";';
        // add the line
        Add(Line);
        // continue with the next one
        Node := vstProjectInfo.GetNextSelected(Node);
      end;
    end
    // no lines selected, so export all projects shown in the view
    else
    begin
      // access the first node
      Node := vstProjectInfo.GetFirst;
      // loop through all projects currently shown in the view
      while Assigned(Node) do
      begin
        // access the node data
        NodeData := vstProjectInfo.GetNodeData(Node);
        // place the project info on a line
        Line := '"' + NodeData^.FClient + '";"' + NodeData^.FPoNumber + '";"';
        Line := Line + ProjectTypes[NodeData^.FType] + '";"' + NodeData^.FScope + '";"';
        Line := Line + Format('%s', [DateToStr(NodeData^.FDeadline)]) + '";"';
        Line := Line + Format('%s', [FormatDateTime('hh:nn', NodeData^.FDeadline)]) + '";"';
        if NodeData^.FCompleted then
          Line := Line + 'Yes";"'
        else
          Line := Line + 'No";"';
        Line := Line + NodeData^.FComment + '";';
        // add the line
        Add(Line);
        // continue with the next one
        Node := vstProjectInfo.GetNext(Node);
      end;
    end;
    // save stringlist contents to the file
    SaveToFile(exportFile);
  finally
    // release the stringlist
    Free;
  end;
end; //*** end of ExportToCsv ***

//***************************************************************************************
// NAME:           SaveConfiguration
// PARAMETER:      xmlConfig Configuration file object
// RETURN VALUE:   None.
// DESCRIPTION:    Saves the configuration to the user data file.
//
//***************************************************************************************
procedure TProjectViewForm.SaveConfiguration(xmlConfig: TXMLConfig);
var
  columnIdx: Integer;
begin
  // ------ project view window -------------
  xmlConfig.OpenKey('ProjectView');
  for columnIdx := 0 to vstProjectInfo.Header.Columns.Count - 1 do
  begin
    xmlConfig.SetValue('Columns/Width/C' + WideString(IntToStr(columnIdx)), vstProjectInfo.Header.Columns[columnIdx].Width);
  end;
  xmlConfig.CloseKey; // ProjectView
end; //*** end of SaveConfiguration ***/

//***************************************************************************************
// NAME:           LoadConfiguration
// PARAMETER:      xmlConfig Configuration file object
// RETURN VALUE:   None.
// DESCRIPTION:    Loads the configuration from the user data file.
//
//***************************************************************************************
procedure TProjectViewForm.LoadConfiguration(xmlConfig: TXMLConfig);
begin
  // ------ project view window -------------
  xmlConfig.OpenKey('ProjectView');
  vstProjectInfo.Header.Columns[0].Width := xmlConfig.GetValue('Columns/Width/C0', 140);
  vstProjectInfo.Header.Columns[1].Width := xmlConfig.GetValue('Columns/Width/C1', 120);
  vstProjectInfo.Header.Columns[2].Width := xmlConfig.GetValue('Columns/Width/C2', 100);
  vstProjectInfo.Header.Columns[3].Width := xmlConfig.GetValue('Columns/Width/C3', 140);
  vstProjectInfo.Header.Columns[4].Width := xmlConfig.GetValue('Columns/Width/C4', 180);
  vstProjectInfo.Header.Columns[5].Width := xmlConfig.GetValue('Columns/Width/C5', 100);
  vstProjectInfo.Header.Columns[6].Width := xmlConfig.GetValue('Columns/Width/C6', 260);
  xmlConfig.CloseKey; // ProjectView
end; //*** end of LoadConfiguration ***

//***************************************************************************************
// NAME:           GetCount
// PARAMETER:      None.
// RETURN VALUE:   Number of projects shown.
// DESCRIPTION:    Reads out the number of projects that are currently shown in the
//                 project view.
//
//***************************************************************************************
function TProjectViewForm.GetCount: integer;
begin
  Result := vstProjectInfo.RootNodeCount;
end; //*** end of GetCount ***

//***************************************************************************************
// NAME:           GetFocused
// PARAMETER:      None.
// RETURN VALUE:   Project ID of the focused project or -1 if nothing focused.
// DESCRIPTION:    Reads out the project ID of the currently focused project.
//
//***************************************************************************************
function TProjectViewForm.GetFocused: integer;
var
  Node: PVirtualNode;
  NodeData: PProjectData;
begin
  // initialize the result to not found
  Result := -1;
  // access the node
  Node := vstProjectInfo.FocusedNode;
  // access its data
  if Assigned(Node) then
  begin
    NodeData := vstProjectInfo.GetNodeData(Node);
    // read out its project ID
    if Assigned(NodeData) then
    begin
      Result := NodeData^.FId;
    end;
  end;
end; //*** end of GetFocused ***

//***************************************************************************************
// NAME:           SetFocused
// PARAMETER:      value Project ID of the project that should be focused in the view.
// RETURN VALUE:   None.
// DESCRIPTION:    Searches for the project with the specified project ID in the view and
//                 makes it the focused one.
//
//***************************************************************************************
procedure TProjectViewForm.SetFocused(value: integer);
var
  Node: PVirtualNode;
  NodeData: PProjectData;
begin
  // access the first node
  Node := vstProjectInfo.GetFirst;
  while Assigned(Node) do
  begin
    // access the node data
    NodeData := vstProjectInfo.GetNodeData(Node);
    // is this the one that needs to be selected?
    if Assigned(NodeData) then
    begin
      if NodeData^.FId = value then
      begin
        // clear current selection
        vstProjectInfo.ClearSelection;
        // select it
        vstProjectInfo.Selected[Node] := True;
        vstProjectInfo.FocusedNode := Node;
        vstProjectInfo.ScrollIntoView(Node, False);
        // done so no need to continue loop
        Break;
      end;
    end;
    // continue with the next one
    Node := vstProjectInfo.GetNext(Node);
  end;
end; //*** end of SetFocused ***

//***************************************************************************************
// NAME:           GetFirstSelected
// PARAMETER:      None.
// RETURN VALUE:   Project ID of the first selected project or -1 if not available.
// DESCRIPTION:    Reads out the project ID of the first selected project.
//
//***************************************************************************************
function TProjectViewForm.GetFirstSelected: integer;
var
  Node: PVirtualNode;
  NodeData: PProjectData;
begin
  // initialize the result to not found
  Result := -1;
  // access the first selected node
  Node := vstProjectInfo.GetFirstSelected();
  // access its data
  if Assigned(Node) then
  begin
    NodeData := vstProjectInfo.GetNodeData(Node);
    // read out its project ID
    if Assigned(NodeData) then
    begin
      Result := NodeData^.FId;
    end;
  end;
end; //*** end of GetFirstSelected ***

//***************************************************************************************
// NAME:           GetNextSelected
// PARAMETER:      previousId Project ID of the previous selected one.
// RETURN VALUE:   Project ID of the next selected project or -1 if not available.
// DESCRIPTION:    Reads out the project ID of the next selected project.
//
//***************************************************************************************
function TProjectViewForm.GetNextSelected(previousId: integer): integer;
var
  PreviousNode: PVirtualNode;
  Node: PVirtualNode;
  NodeData: PProjectData;
begin
  // initialize the result to not found
  Result := -1;
  // find the Node that the id belongs to
  PreviousNode := ConvertProjectIdToNode(previousId);
  if Assigned(PreviousNode) then
  begin
    Node := vstProjectInfo.GetNextSelected(PreviousNode);
    // access its data
    if Assigned(Node) then
    begin
      NodeData := vstProjectInfo.GetNodeData(Node);
      // read out its project ID
      if Assigned(NodeData) then
      begin
        Result := NodeData^.FId;
      end;
    end;
  end;
end; //*** end of GetNextSelected ***

//***************************************************************************************
// NAME:           SetProjectList
// PARAMETER:      value New value for member ProjectList
// RETURN VALUE:   None.
// DESCRIPTION:    Setter for member ProjectList.
//
//***************************************************************************************
procedure TProjectViewForm.SetProjectList(value: TProjectList);
begin
  // store the member
  FProjectList := value;
end; //*** end of SetProjectList ***

//***************************************************************************************
// NAME:           SetViewFilter
// PARAMETER:      value New value for member ViewFilter.
// RETURN VALUE:   None.
// DESCRIPTION:    Setter for member ViewFilter.
//
//***************************************************************************************
procedure TProjectViewForm.SetViewFilter(value: TProjectViewFilter);
begin
  FViewFilter := value;
  // refresh to process the filter
  Refresh;
end; //*** end of SetViewFilter ***

//***************************************************************************************
// NAME:           GetDeadlineFlagIconIndex
// PARAMETER:      Node Node to check.
// RETURN VALUE:   None.
// DESCRIPTION:    Checks the deadline for the node and determines the flag icon index
//                 from the image list.
//
//***************************************************************************************
function TProjectViewForm.GetDeadlineFlagIconIndex(Node: PVirtualNode): integer;
var
  dtCurrent, dtDeadline: TDateTime;
  NodeData: PProjectData;
  minutesToDeadline: integer;
begin
  // init result to no flag icon
  Result := -1;

  // access the project data
  NodeData := vstProjectInfo.GetNodeData(Node);
  if not Assigned(NodeData) then
    Exit;

  // no need to check on a completed project
  if NodeData^.FCompleted then
    Exit;

  // read date time values
  dtCurrent := Now;
  dtDeadline := NodeData^.FDeadline;

  // deadline expired always gets a red flag
  if CompareDateTime(dtCurrent, dtDeadline) >= 0 then
  begin
    // set red flag
    Result := 15;
  end
  // deadline is not yet expired
  else
  begin
    // determine how many hours until the deadline
    minutesToDeadline := MinutesBetween(dtCurrent, dtDeadline);
    if minutesToDeadline < 180 then
      // set white flag
      Result := 14;
    if minutesToDeadline < 120 then
      // set yellow flag
      Result := 16;
    if minutesToDeadline < 60 then
      // set red flag
      Result := 15;
  end;
end; //*** end of GetDeadlineFlagIconIndex ***

//***************************************************************************************
// NAME:           vstProjectInfoGetNodeDataSize
// PARAMETER:      Sender Signal source.
//                 NodeDataSize Size of the node.
// RETURN VALUE:   None.
// DESCRIPTION:    Event handler for when the node's data size should be specified.
//
//***************************************************************************************
procedure TProjectViewForm.vstProjectInfoGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TProjectData);
end; //*** end of vstProjectInfoGetNodeDataSize ***

//***************************************************************************************
// NAME:           vstProjectInfoGetText
// PARAMETER:      Sender Signal source.
//                 Node Node that changed.
//                 Column Column index.
//                 TextType The type of text.
//                 CellText Destination string of the celltext.
// RETURN VALUE:   None.
// DESCRIPTION:    Event handler for when a cell's string should be specified.
//
//***************************************************************************************
procedure TProjectViewForm.vstProjectInfoGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  NodeData: PProjectData;
begin
  // prevent compiler hint for unused parameter
  TextType := TextType;
  // get pointer to the node's data
  NodeData := vstProjectInfo.GetNodeData(Node);
  if Assigned(NodeData) then
  begin
    // determine for which column the cell text is requested
    case Column of
      0: // client
        begin
          CellText := NodeData^.FClient;
        end;
      1: // po number
        begin
          CellText := NodeData^.FPoNumber;
        end;
      2: // type
        begin
          CellText := ProjectTypes[NodeData^.FType];
        end;
      3: // scope
        begin
          CellText := NodeData^.FScope;
        end;
      4: // deadline
        begin
          CellText := WeekDayNameShort[DayOfTheWeek(NodeData^.FDeadline)] + ' ';
          CellText := CellText + Format('%s %s', [DateToStr(NodeData^.FDeadline), FormatDateTime('hh:nn', NodeData^.FDeadline)]);
        end;
      5: // completed
        begin
          if NodeData^.FCompleted then
            CellText := 'Yes'
          else
            CellText := 'No';
        end;
      6: // comment
        begin
          CellText := NodeData^.FComment;
        end;
      else
        CellText := '';
    end;
  end;
end; //*** end of vstProjectInfoGetText ***

//***************************************************************************************
// NAME:           vstProjectInfoHeaderClick
// PARAMETER:      Sender Signal source.
//                 HitInfo Details about where and how the click occurred.
// RETURN VALUE:   None.
// DESCRIPTION:    Event handler for when the header is clicked.
//
//***************************************************************************************
procedure TProjectViewForm.vstProjectInfoHeaderClick(Sender: TVTHeader;
  HitInfo: TVTHeaderHitInfo);
begin
  // don't call sorting procedure on right click
  // some list-headers have a contextmenu which should popup then.
  if HitInfo.Button = mbRight then
    Exit;
  // update sorting details. actual sorting will be done at the end of the timer event
  Sender.SortColumn := HitInfo.Column;
  // toggle sort direction
  if Sender.SortDirection = sdAscending then
    Sender.SortDirection := sdDescending
  else
    Sender.SortDirection := sdAscending;
  // perform sorting
  vstProjectInfo.SortTree(HitInfo.Column, Sender.SortDirection);
end; //*** end of vstProjectInfoHeaderClick ***

//***************************************************************************************
// NAME:           vstProjectInfoKeyPress
// PARAMETER:      Sender Signal source.
//                 Key Key that caused the event.
// RETURN VALUE:   None.
// DESCRIPTION:    Event handler for the key-press event of the treeview.
//
//***************************************************************************************
procedure TProjectViewForm.vstProjectInfoKeyPress(Sender: TObject; var Key: char);
begin
  // <enter> key?
  if Key = Char(13) then
  begin
    // check if a row is selected and the click was on a row
    if Focused <> -1 then
    begin
      // quick way to select just the focused one
      Focused := Focused;
      // trigger the OnEdit event
      if Assigned(FOnEdit) then
      begin
        FOnEdit(Sender);
      end;
    end;
  end;
end; //*** end of vstProjectInfoKeyPress ***

//***************************************************************************************
// NAME:           vstProjectInfoFreeNode
// PARAMETER:      Sender Signal source.
//                 Node Node that should be freed.
// RETURN VALUE:   None.
// DESCRIPTION:    Event handler for when a node is being released.
//
//***************************************************************************************
procedure TProjectViewForm.vstProjectInfoFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  NodeData: PProjectData;
begin
  NodeData := vstProjectInfo.GetNodeData(Node);
  if Assigned(NodeData) then
  begin
    // reset members
    NodeData^.FId := 0;
    NodeData^.FClient := '';
    NodeData^.FPoNumber := '';
    NodeData^.FType := 0;
    NodeData^.FScope := '';
    NodeData^.FDeadline := 0;
    NodeData^.FCompleted := false;
    NodeData^.FComment := '';
  end;
end; //*** end of vstProjectInfoFreeNode ***

//***************************************************************************************
// NAME:           vstProjectInfoGetImageIndex
// PARAMETER:      Sender Signal source.
//                 Node The node for which an image index is requested.
//                 Kind Allows different image to be used for different states.
//                 Column The column of the node for which the image index is requested.
//                 Ghosted True to show the image in a disbled state, False otherwise.
//                 ImageIndex Image index in the linked image list.
// RETURN VALUE:   None.
// DESCRIPTION:    Determines the image index in the image list that should be used.
//
//***************************************************************************************
procedure TProjectViewForm.vstProjectInfoGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
begin
  // prevent compiler hint due to unused parameters
  Node := Node;
  Kind := Kind;
  // show normal icon
  Ghosted:= False;
  // show icon only in the deadline column
  if Column = 4 then
    ImageIndex := GetDeadlineFlagIconIndex(Node)
  else
    ImageIndex := -1;
end; //*** end of vstProjectInfoGetImageIndex ***

//***************************************************************************************
// NAME:           vstProjectInfoColumnDblClick
// PARAMETER:      Sender Signal source.
//                 Column Zero based index of the column that was double-clicked.
//                 Shift State of the shift type keys such as shift, alt, ctrl etc.
// RETURN VALUE:   None.
// DESCRIPTION:    Event handler for when when a column was double-clicked.
//
//***************************************************************************************
procedure TProjectViewForm.vstProjectInfoColumnDblClick(
  Sender: TBaseVirtualTree; Column: TColumnIndex; Shift: TShiftState);
begin
  // prevent compiler hint due to unused parameters
  Shift := Shift;

  // check if a row is selected and the click was on a row
  if (Focused <> -1) and (Column >= 0) then
  begin
    // trigger the OnEdit event
    if Assigned(FOnEdit) then
    begin
      FOnEdit(Sender);
    end;
  end
  // check if a double click occurred in the empty white space underneath
  else if (Focused = -1) and (Column = -1) then
  begin
    // trigger the OnInsert event
    if Assigned(FOnInsert) then
    begin
      FOnInsert(Sender);
    end;
  end;
end; //*** end of vstProjectInfoColumnDblClick ***

//***************************************************************************************
// NAME:           vstProjectInfoCompareNodes
// PARAMETER:      Sender Signal source.
//                 Node1 First node for comparison.
//                 Node2 Second node for comparison.
//                 Column The column the nodes are in
//                 Result 0 if both node are equal, > 0 if node1 > node2, < 0 otherwise.
// RETURN VALUE:   None.
// DESCRIPTION:    Event callback for comparing nodes for sorting purposes.
//
//***************************************************************************************
procedure TProjectViewForm.vstProjectInfoCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1: PProjectData;
  Data2: PProjectData;
begin
  // initialize the result code
  Result := 0;
  // access node data
  Data1 := vstProjectInfo.GetNodeData(Node1);
  Data2 := vstProjectInfo.GetNodeData(Node2);
  // make sure data is valid
  if (not Assigned(Data1)) or (not Assigned(Data2)) then
    Exit;
  // handle sorting per column
  case Column of
    0: // client
      begin
        Result := CompareText(Data1^.FClient, Data2^.FClient);
      end;
    1: // po number
      begin
        Result := CompareText(Data1^.FPoNumber, Data2^.FPoNumber);
      end;
    2: // type
      begin
        Result := CompareText(ProjectTypes[Data1^.FType], ProjectTypes[Data2^.FType]);
      end;
    3: // scope
      begin
        Result := CompareText(Data1^.FScope, Data2^.FScope);
      end;
    4: // deadline
      begin
        Result := CompareDateTime(Data1^.FDeadline, Data2^.FDeadline);
      end;
    5: // completed
      begin
        if Data1^.FCompleted > Data2^.FCompleted then
          Result := 1
        else if Data1^.FCompleted < Data2^.FCompleted then
          Result := -1;
      end;
    6: // comment
      begin
        Result := CompareText(Data1^.FComment, Data2^.FComment);
      end;
  end;
end; //*** end of vstProjectInfoCompareNodes ***

//***************************************************************************************
// NAME:           vstProjectInfoDrawText
// PARAMETER:      Sender Signal source.
//                 TargetCanvas Canvas for drawing
//                 Node Node that is about the be text drawn.
//                 Column Column index.
//                 TextInfo The text.
//                 CellRect Rect of the cell.
//                 DefaultDraw Control for if the cell should be text drawn.
// RETURN VALUE:   None.
// DESCRIPTION:    Event handler for when when text is about the be text drawn for a
//                 node.
//
//***************************************************************************************
procedure TProjectViewForm.vstProjectInfoDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const TextInfo: String; const CellRect: TRect; var DefaultDraw: Boolean);
var
  NodeData: PProjectData;
begin
  // prevent hints due to unused parameters
  Column := Column;
  DefaultDraw := DefaultDraw;
  // access the node data
  NodeData := Sender.GetNodeData(Node);
  if Assigned(NodeData) then
  begin
    // make font of completed projects grey if we are not selected
    if (NodeData^.FCompleted) and (vstProjectInfo.Selected[Node] = False) then
    begin
      TargetCanvas.Font.Color:= clGray;
    end;
  end;
end; //*** end of vstProjectInfoDrawText ***

//***************************************************************************************
// NAME:           vstProjectInfoClick
// PARAMETER:      Sender Signal source.
// RETURN VALUE:   None.
// DESCRIPTION:    Event handler for when the left mouse click was detected in the
//                 tree.
//
//***************************************************************************************
procedure TProjectViewForm.vstProjectInfoClick(Sender: TObject);
var
  Node: PVirtualNode;
begin
  // deselect row if clicked in the white space outside of the projects
  if vstProjectInfo.HotNode = nil then
  begin
    Node := vstProjectInfo.FocusedNode;
    if Assigned(Node) then
    begin
      vstProjectInfo.Selected[Node] := False;
    end;
    vstProjectInfo.FocusedNode := nil
  end;
end; //*** end of vstProjectInfoClick ***

//***************************************************************************************
// NAME:           vstProjectInfoBeforeCellPaint
// PARAMETER:      Sender Signal source.
//                 TargetCanvas Canvas for painting.
//                 Node Node that is about the be painted.
//                 Column Column index.
//                 CellPaintMode Paintmode for the cell.
//                 CellRect Rect of the cell.
//                 ContentRect Rect of the content.
// RETURN VALUE:   None.
// DESCRIPTION:    Event handler for when the row is about the be painted.
//
//***************************************************************************************
procedure TProjectViewForm.vstProjectInfoBeforeCellPaint(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
begin
  // give every other row a slightly different background color for ease of reading
  // TODO: evaluate if users would like this
  {if (Node^.Index mod 2 <> 0) and (Sender.FocusedNode <> Node)then
  begin
    TargetCanvas.Brush.Color := RGBToColor(255, 215, 215);
    TargetCanvas.FillRect(CellRect);
  end;}
end; //*** end of vstProjectInfoBeforeCellPaint ***

//***************************************************************************************
// NAME:           FormCreate
// PARAMETER:      Sender Signal source.
// RETURN VALUE:   None.
// DESCRIPTION:    Called when the form is created
//
//***************************************************************************************
procedure TProjectViewForm.FormCreate(Sender: TObject);
begin
  // init member defaults
  FViewFilter := pfActive;
  FOnRefresh := nil;
  FOnEdit := nil;
  // set selection color
  with vstProjectInfo.Colors do
  begin
    DropMarkColor := clGray;
    DropTargetBorderColor := clGray;
    DropTargetColor := clGray;
    FocusedSelectionBorderColor := clGray;
    FocusedSelectionColor := clGray;
    SelectionRectangleBlendColor := clGray;
    SelectionRectangleBorderColor := clGray;
  end;
end; //*** end of FormCreate ***

//***************************************************************************************
// NAME:           DeadlineTimerTimer
// PARAMETER:      Sender Signal source.
// RETURN VALUE:   None.
// DESCRIPTION:    Called when the deadline timer period expired.
//
//***************************************************************************************
procedure TProjectViewForm.DeadlineTimerTimer(Sender: TObject);
begin
  // refresh the project view to update the deadline dependent flag icons
  vstProjectInfo.Refresh;
end; //*** end of DeadlineTimerTimer ***

end.
//********************************** end of projectviewunit.pas *************************

