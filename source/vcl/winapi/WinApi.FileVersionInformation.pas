{ -----------------------------------------------------------------------------
  Unit Name: WinApi.FileVersionInformation
  Author: Tristan Marlow
  Purpose: Little Earth Solutions File Version Information

  ----------------------------------------------------------------------------
  Copyright (c) 2016 Tristan David Marlow
  Copyright (c) 2016 Little Earth Solutions
  All Rights Reserved

  This product is protected by copyright and distributed under
  licenses restricting copying, distribution and decompilation

  ----------------------------------------------------------------------------

  History: 03/05/2007 - First Release.
  18/06/2014 - Improvements for Locale

  ----------------------------------------------------------------------------- }
unit WinApi.FileVersionInformation;

interface

uses WinApi.Windows, WinApi.ShellAPI, WinApi.CommCtrl,
  System.SysUtils, System.Classes, System.Variants,
  VCL.Graphics,
  WinApi.FileVersionInformation.Base;

type
  TLZFileVersionInformation = class(TLZFileVersionInformationBase)
  private
    FIcon: TIcon;
    function GetApplicationIcon(AFileName: TFileName): TIcon;
  protected
    procedure SetFilename(const AFileName: TFileName); override;
    procedure ClearValues; override;
  public
    property Icon: TIcon read FIcon;
  end;

implementation

procedure TLZFileVersionInformation.ClearValues;
begin
  inherited ClearValues;
  if Assigned(FIcon) then
    FreeAndNil(FIcon);
end;

function TLZFileVersionInformation.GetApplicationIcon
  (AFileName: TFileName): TIcon;

const
  IID_IImageList: TGUID = '{46EB5926-582E-4017-9FDF-E8998DAA0950}';

  function GetImageListSH(SHIL_FLAG: Cardinal): HIMAGELIST;
  type
    _SHGetImageList = function(iImageList: Integer; const riid: TGUID;
      var ppv: Pointer): hResult; stdcall;
  var
    Handle: THandle;
    SHGetImageList: _SHGetImageList;
  begin
    Result := 0;
    Handle := LoadLibrary('Shell32.dll');
    if Handle <> S_OK then
      try
        SHGetImageList := GetProcAddress(Handle, PChar(727));
        if Assigned(SHGetImageList) and (Win32Platform = VER_PLATFORM_WIN32_NT)
        then
          SHGetImageList(SHIL_FLAG, IID_IImageList, Pointer(Result));
      finally
        FreeLibrary(Handle);
      end;
  end;

var
  aImgList: HIMAGELIST;
  SFI: TSHFileInfo;
  aIndex: Integer;
begin
  SHGetFileInfo(PChar(AFileName), FILE_ATTRIBUTE_NORMAL, SFI,
    SizeOf(TSHFileInfo), SHGFI_ICON or SHGFI_LARGEICON or SHGFI_SHELLICONSIZE or
    SHGFI_SYSICONINDEX or SHGFI_TYPENAME or SHGFI_DISPLAYNAME);
  Result := TIcon.Create;
  aImgList := GetImageListSH(0);
  aIndex := SFI.iIcon;
  Result.Handle := ImageList_GetIcon(aImgList, aIndex, ILD_NORMAL);
end;

procedure TLZFileVersionInformation.SetFilename(const AFileName: TFileName);
begin
  inherited SetFilename(AFileName);
  if FileExists(FileName) then
  begin
    FIcon := GetApplicationIcon(AFileName);
  end;
end;

end.
