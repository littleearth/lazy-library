unit VCL.Lazy.Utils;

interface

uses Windows, Messages, SysUtils, Classes, Lazy.Utils, Lazy.Types;

procedure QuickFileSearch(const PathName, FileName: string;
  const Recurse: Boolean; FileList: TStrings);

implementation

procedure QuickFileSearch(const PathName, FileName: string;
  const Recurse: Boolean; FileList: TStrings);
var
  Rec: TSearchRec;
  path: string;
  Cancel: Boolean;
begin
  path := IncludeTrailingPathDelimiter(PathName);
  Cancel := False;
  if FindFirst(path + FileName, faAnyFile, Rec) = 0 then
    try
      repeat
        if (Rec.Name <> '.') and (Rec.Name <> '..') then
        begin
          FileList.Add(path + Rec.Name);
        end;
      until (FindNext(Rec) <> 0) or (Cancel = True);
    finally
      FindClose(Rec);
    end;

  if (Recurse) and (Cancel = False) then
  begin
    if FindFirst(path + '*', faDirectory, Rec) = 0 then
      try
        repeat
          if ((Rec.Attr and faDirectory) = faDirectory) and (Rec.Name <> '.')
            and (Rec.Name <> '..') then
          begin
            QuickFileSearch(path + Rec.Name, FileName, True, FileList);
          end;
        until (FindNext(Rec) <> 0) or (Cancel = True);
      finally
        FindClose(Rec);
      end;
  end;
end;

end.
