unit WinApi.Vss.Copy;

interface

uses
  Lazy.Types, VCL.Lazy.Types.Windows, VCL.Lazy.Utils.Windows,
  WinApi.Windows, WinApi.Messages, System.SysUtils, System.Variants,
  System.Classes, WinApi.Vss.API, WinApi.Vss.Utils, System.Win.ComObj,
  WinApi.ShlObj, WinApi.KnownFolders;

type
  TLZVSSCopy = class(TLZObject)
  private
    FOnProgress: TOnProgress;
    FLog: TStringList;
    FCancel: Boolean;
    FOnLog: TOnLog;
    procedure SetOnProgress(const Value: TOnProgress);
    procedure FileSearch(const PathName, FileName: string;
      const Recurse: Boolean; FileList: TStrings);
  protected
    procedure Log(AMessage: string); override;
    procedure Progress(AProgress: integer; AMessage: string = '');
    function Validate(ASourceFolder, ADestinationFolder: string): Boolean;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function Backup(ASourceFolder: string; AFileMask: string;
      ARecursive: Boolean; ADestinationFolder: string): Boolean;
    procedure Cancel;
    property OnProgress: TOnProgress read FOnProgress write SetOnProgress;
    property OnLog: TOnLog read FOnLog write FOnLog;
  end;

implementation

{ TVSSCopy }

procedure TLZVSSCopy.FileSearch(const PathName, FileName: string;
  const Recurse: Boolean; FileList: TStrings);
begin
  TLZFile.QuickFileSearch(PathName, FileName, Recurse, FileList);
end;

function TLZVSSCopy.Backup(ASourceFolder, AFileMask: string;
  ARecursive: Boolean; ADestinationFolder: string): Boolean;
var
  VolumeShadowCopy: TVolumeShadowCopy;
  SnapShot: TVssSnapshotProp;
  VolName: string;
  SourceIdx: integer;
  SnapshotGUID: TGUID;
  Drive, SourceFile, SourcePath, DestPath, ShadowPath, LogFileName: string;
  SourceFiles: TStringList;
  VolumeList: TStringList;
  SuccessCount, FailCount: integer;
begin
  FLog.Clear;
  FCancel := false;
  Progress(0);
  VolumeShadowCopy := TVolumeShadowCopy.Create;
  SourceFiles := TStringList.Create;
  VolumeList := TStringList.Create;
  FailCount := 0;
  SuccessCount := 0;
  try
    if Validate(ASourceFolder, ADestinationFolder) then
    begin
      Log(Format('Searching %s for %s...', [ASourceFolder, AFileMask]));
      Progress(0, Format('Searching %s for %s...', [ASourceFolder, AFileMask]));
      FileSearch(ASourceFolder, AFileMask, ARecursive, SourceFiles);
      Progress(0, Format('%d file(s) found.', [SourceFiles.Count]));
      Log(Format('%d file(s) found.', [SourceFiles.Count]));
      if (SourceFiles.Count > 0) and (not FCancel) then
      begin
        Log('Starting backup operation.');
        Drive := IncludeTrailingPathDelimiter(ExtractFileDrive(ASourceFolder));
        Log('Drive to snapshot = ' + Drive + '.');
        GetDisplayNameForVolume(GetVolumeUniqueName(Drive), VolName);
        Log('Volume Name to snapshot = ' + VolName + '.');
        LogFileName := TLZFile.GetUserAppDataFolder + 'vss.log';
        if FileExists(LogFileName) then
        begin
          Log('Removing old log file ' + LogFileName);
          DeleteFile(LogFileName);
        end;
        VolumeShadowCopy.WriteLog := True;
        VolumeShadowCopy.LogFileName := LogFileName;
        Log('Log= ' + LogFileName + '.');
        VolumeShadowCopy.Initialize(VSS_CTX_BACKUP);
        VolumeList.Add(VolName);
        VolumeShadowCopy.CreateSnapshotSet(VolumeList, nil, nil);
        if VolumeShadowCopy.VssContext and
          VSS_VOLSNAP_ATTR_DELAYED_POSTSNAPSHOT = 0 then
        begin
          try
            Log(IntToStr(Length(VolumeShadowCopy.LatestSnapshotIdList)));

            SnapshotGUID := VolumeShadowCopy.LatestSnapshotIdList
              [High(VolumeShadowCopy.LatestSnapshotIdList)];

            Log(GUIDToString(SnapshotGUID));

            VolumeShadowCopy.GetSnapshotProperties(SnapshotGUID, SnapShot);
            try
              SourceIdx := 0;
              While (SourceIdx < SourceFiles.Count) and (not FCancel) do
              begin
                try
                  SourceFile := SourceFiles[SourceIdx];
                  SourcePath := System.Copy(SourceFile, 3,
                    Length(SourceFile) - 2);
                  ShadowPath :=
                    WideCharToString(SnapShot.m_pwszSnapshotDeviceObject);
                  SourcePath := ShadowPath + SourcePath;

                  DestPath := IncludeTrailingPathDelimiter(ADestinationFolder) +
                    System.Copy(SourceFile, 3, Length(SourceFile) - 2);

                  Log(SourcePath + ' => ' + DestPath);

                  Progress(TLZMath.CalculatePercentage(SourceIdx,
                    SourceFiles.Count), SourceFile);

                  if FileExists(DestPath) then
                  begin
                    Log('Removing: ' + DestPath);
                    if not DeleteFile(DestPath) then
                    begin
                      Log('Failed to remove: ' + DestPath);
                    end;
                  end;

                  if TLZFile.CheckDirectoryExists(ExtractFilePath(DestPath),
                    True) then
                  begin
                    If (CopyFile(PWideChar(SourcePath), PWideChar(DestPath),
                      True)) then
                    begin
                      inc(SuccessCount);
                    end
                    else
                    begin
                      Log('Failed to backup: ' + SourcePath + ', Error: ' +
                        SysErrorMessage(GetLastError()));
                      inc(FailCount);
                    end;
                  end
                  else
                  begin
                    Log('Failed to access destination folder.');
                    inc(FailCount);
                  end;
                finally
                  inc(SourceIdx);
                end;
              end;
              try
                Log('Removing Snapshot: ' + GUIDToString(SnapshotGUID));
                VolumeShadowCopy.DeleteSnapshotSet(SnapshotGUID);
              except
                on E: Exception do
                begin
                  Error(E, 'Failed to delete snapshot.');
                end;
              end;
            finally
              VolumeShadowCopy.FreeSnapshotProperties(SnapShot);
            end;

          except
            if VolumeShadowCopy.VssContext and VSS_VOLSNAP_ATTR_NO_WRITERS = 0
            then
            begin
              VolumeShadowCopy.BackupComplete(false);
            end;
          end;
          if VolumeShadowCopy.VssContext and VSS_VOLSNAP_ATTR_NO_WRITERS = 0
          then
            VolumeShadowCopy.BackupComplete(True);
        end;

      end;
      Log(Format('%d file(s) copied.', [SuccessCount]));

    end;
  finally
    Progress(100);
    Result := (SuccessCount > 0) and (FailCount = 0);

    if FailCount > 0 then
    begin
      Log(Format('Finished, with %d error(s).', [FailCount]));
    end
    else
    begin
      Log('Finished.');
    end;

    FreeAndNil(VolumeShadowCopy);
    FreeAndNil(SourceFiles);
    FreeAndNil(VolumeList);
  End;
end;

procedure TLZVSSCopy.Cancel;
begin
  FCancel := True;
end;

constructor TLZVSSCopy.Create;
begin
  inherited;
  FLog := TStringList.Create;
  VssCoInitializeSecurity;
end;

destructor TLZVSSCopy.Destroy;
begin
  try
    FreeAndNil(FLog);
  finally
    inherited;
  end;
end;

procedure TLZVSSCopy.Log(AMessage: string);
begin
  inherited Log(AMessage);
  FLog.Add(AMessage);
  if Assigned(FOnLog) then
    FOnLog(Self, AMessage);
end;

procedure TLZVSSCopy.Progress(AProgress: integer; AMessage: string);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, AProgress, AMessage, FCancel);
end;

procedure TLZVSSCopy.SetOnProgress(const Value: TOnProgress);
begin
  FOnProgress := Value;
end;

function TLZVSSCopy.Validate(ASourceFolder, ADestinationFolder: string)
  : Boolean;
begin
  Result := True;
  if not TLZFile.CheckDirectoryExists(ASourceFolder, false) then
  begin
    Log(Format('Source folder "%s" does not exist.', [ASourceFolder]));
    Result := false;
  end;

  if not TLZFile.CheckDirectoryExists(ADestinationFolder, True) then
  begin
    Log(Format('Destination folder "%s" does not exist.',
      [ADestinationFolder]));
    Result := false;
  end;

  if SameText(ASourceFolder, ADestinationFolder) then
  begin
    Log('Source and destination folder are the same.');
    Result := false;
  end;

end;

end.
