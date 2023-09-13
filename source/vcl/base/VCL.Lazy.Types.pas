unit VCL.Lazy.Types;

interface

type
  TLazyOSVersion = (osUnknown, osWin95, osWin98, osWin98SE, osWinNT, osWinME,
    osWin2000, osWinXP, osWinVista, osWinSeven, osWin8, osWin81,
    osWin10, osWin11);
  TLazyOSStartupState = (ssUnknown, ssNormal, ssSafeMode, ssSafeModeNetwork);
  TLazyOSArchitecture = (oaUnkown, oaX86, oaX64);
  TLazyOSElevationLevel = (elUnknown, elDefault, elFull, elLimited);

implementation

end.
