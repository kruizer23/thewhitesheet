unit projectdata;
//***************************************************************************************
//  Description: Type definitions regarding the project data model.
//    File Name: projectdata.pas
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
  Classes, SysUtils, DateUtils;

//***************************************************************************************
// Type Definitions
//***************************************************************************************
//---------------------------------- TProjectData ---------------------------------------
type
  PProjectData = ^TProjectData;
  TProjectData = record
    FId: integer;
    FClient: string;
    FPoNumber: string;
    FScope: string;
    FType: byte;
    FDeadline: TDateTime;
    FCompleted: boolean;
    FComment: string;
  end;

//---------------------------------- TProjectList ---------------------------------------
type
  PProjectList = ^TProjectList;
  TProjectList=class(TList)
  private
    { Private declarations }
    function Get(Index: Integer): PProjectData;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; override;
    function Add(Project: TProjectData): Integer;
    procedure Delete(ProjectId: Integer);
    property Items[Index: Integer]: PProjectData read Get; default;
  end;

//***************************************************************************************
// Global constant declarations
//***************************************************************************************
const
  ProjectTypes: array  [0..3] of string = ('Translation','Screening','Testing', 'Other');

implementation
//---------------------------------------------------------------------------------------
//-------------------------------- TProjectList -----------------------------------------
//---------------------------------------------------------------------------------------
//***************************************************************************************
// NAME:           Create
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Object constructor. Calls TObject's constructor and initializes
//                 the private property variables to their default values.
//
//***************************************************************************************
constructor TProjectList.Create;
begin
  // call inherited constructor
  inherited Create;
end; //*** end of Create ***

//***************************************************************************************
// NAME:           Destroy
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Component destructor.
//
//***************************************************************************************
destructor TProjectList.Destroy;
var
  idx: Integer;
begin
  // release allocated heap memory
  for idx := 0 to Count - 1 do
    Dispose(Items[idx]);
  inherited;
end; //*** end of Destroy ***

//***************************************************************************************
// NAME:           Get
// PARAMETER:      Index Index in the list
// RETURN VALUE:   Pointer to the list item.
// DESCRIPTION:    Obtains an element from the list.
//
//***************************************************************************************
function TProjectList.Get(Index: Integer): PProjectData;
begin
  // update the unique project ID, since it could have changed when entries got removed.
  PProjectData(inherited Get(Index))^.FId := Index;
  Result := PProjectData(inherited Get(Index));
end; //*** end of Get ***

//***************************************************************************************
// NAME:           Delete
// PARAMETER:      ProjectId ProjectId of the project to delete.
// RETURN VALUE:   None.
// DESCRIPTION:    Deletes the item from the list with the specified project ID.
//
//***************************************************************************************
procedure TProjectList.Delete(ProjectId: Integer);
var
  idx: Integer;
begin
  // search for the index with this ProjectId
  for idx := 0 to Count - 1 do
  begin
    if PProjectData(inherited Get(idx))^.FId = ProjectId then
    begin
     Dispose(Items[idx]);
     inherited Delete(idx);
     Break;
    end;
  end;
end; //*** end of Delete ***

//***************************************************************************************
// NAME:           Add
// PARAMETER:      Project Project to add.
// RETURN VALUE:   Index of the newly added project in the list if successful, -1
//                 otherwise.
// DESCRIPTION:    Adds an element to the list. Space is allocated on the heap.
//
//***************************************************************************************
function TProjectList.Add(Project: TProjectData): Integer;
var
  entry: PProjectData;
begin
  // init result
  Result := -1;
  // allocate memory for the entry
  New(entry);
  // copy project info
  entry^.FClient := Project.FClient;
  entry^.FPoNumber := Project.FPoNumber;
  entry^.FScope := Project.FScope;
  entry^.FType := Project.FType;
  entry^.FDeadline := Project.FDeadline;
  entry^.FCompleted := Project.FCompleted;
  entry^.FComment := Project.FComment;
  // add the entry to the list
  Result := inherited Add(entry);
  // set project ID to the index in the list
  if Result >= 0 then
    entry^.FId := Result;
end; //*** end of Add ***

end.
//********************************** end of projectdata.pas *****************************

