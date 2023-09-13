(* Delphi-Unit
   Resource strings for VssUtils - English
   =======================================

   © Dr. J. Rathlev, D-24222 Schwentinental (info(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   Version 1.0: February 2015
   *)

unit WinApi.Vss.Consts;

interface

function rsVssNotAvail : string;
function rsInitBackupComps : string;
function rsInitMetaData : string;
function rsGatherMetaData : string;
function rsGatherWriterStatus : string;
function rsDscvDirExclComps : string;
function rsDscvNsExclComps : string;
function rsDscvAllExclComps : string;
function rsDscvExclWriters : string;
function rsDscvExpInclComps : string;
function rsSelExpInclComps : string;
function rsSaveBackCompsDoc : string;
function rsAddVolSnapSet : string;
function rsPrepBackup : string;
function rsCreateShadowCopy : string;
function rsCreateCopySuccess : string;
function rsQueryShadowCopies : string;
function rsQuerySnapshotSetID : string;
function rsCreateShadowSet : string;
function rsCompleteBackup : string;

implementation

function rsVssNotAvail : string;
begin
  Result:='Volume Shadow Copy could not be initialized on this system!';
  end;

function rsInitBackupComps : string;
begin
  Result:='Initializing IVssBackupComponents Interface ...';
  end;

function rsInitMetaData : string;
begin
  Result:='Initialize writer metadata ...';
  end;

function rsGatherMetaData : string;
begin
  Result:='Gathering writer metadata ...';
  end;

function rsGatherWriterStatus : string;
begin
  Result:='Gathering writer status ...';
  end;

function rsDscvDirExclComps : string;
begin
  Result:='Discover directly excluded components ...';
  end;

function rsDscvNsExclComps : string;
begin
  Result:='Discover components that reside outside the shadow set ...';
  end;

function rsDscvAllExclComps : string;
begin
  Result:='Discover all excluded components ...';
  end;

function rsDscvExclWriters : string;
begin
  Result:='Discover excluded writers ...';
  end;

function rsDscvExpInclComps : string;
begin
  Result:='Discover explicitly included components ...';
  end;

function rsSelExpInclComps : string;
begin
  Result:='Select explicitly included components ...';
  end;

function rsSaveBackCompsDoc : string;
begin
  Result:='Saving the backup components document ...';
  end;

function rsAddVolSnapSet : string;
begin
  Result:='Add volumes to snapshot set ...';
  end;

function rsPrepBackup : string;
begin
  Result:='Preparing for backup ...';
  end;

function rsCreateShadowCopy : string;
begin
  Result:='Creating the shadow copy ...';
  end;

function rsCreateCopySuccess : string;
begin
  Result:='Shadow copy set successfully created';
  end;

function rsQueryShadowCopies : string;
begin
  Result:='Querying all shadow copies in the system ...';
  end;

function rsQuerySnapshotSetID : string;
begin
  Result:='Querying all shadow copies with the SnapshotSetID %s ...';
  end;

function rsCreateShadowSet : string;
begin
  Result:='Creating shadow set {%s} ...';
  end;

function rsCompleteBackup : string;
begin
  Result:='Completing the backup ...';
  end;

end.
